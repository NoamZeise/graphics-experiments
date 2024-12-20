#version 460

layout (local_size_x = 10, local_size_y = 10, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D imgOut;

layout(std430, binding = 1) buffer Radiance_Intervals {
  vec4 interval[];
};

uniform sampler2D colour_buff;
uniform sampler2D light_buff;
uniform sampler2D depth_buff;

const int width = 100;
const int height = 100;
const int depth = 100;
const int samples = 6;

vec4 getIntervalCol(int x, int y, int z) {
  return interval[z * width * height * samples + y * width * samples + z * samples];
}

vec3 getIntervalPos(int x, int y, int z) {
  return vec3(x / float(width), y / float(height), z / float(depth));
}

struct Interval {
  vec4 colour;
  vec3 pos;
};

Interval getInterval(int x, int y, int z) {
  Interval i;
  i.colour = getIntervalCol(x, y, z);
  i.pos = getIntervalPos(x, y, z);
  return i;
}

Interval fgetInterval(float x, float y, float z) {
  return getInterval(int(x), int(y), int(z));
}

Interval intervalFromId(vec3 id, int flag) {


  return fgetInterval(floor(id.x), floor(id.y), floor(id.z));
}

void main() {
    vec4 col = vec4(1);    
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    vec2 uv = vec2((0.5 + float(coord.x))/float(gl_NumWorkGroups.x),
		   (0.5 + float(coord.y))/float(gl_NumWorkGroups.y));
    //col = vec4(pow(texture(depth_buff, uv).r, 100));
    col = texture(depth_buff, uv);
    col.a = 1;
    col.rgb += vec3(1);
    col.rgb /=2;
    vec3 pos = texture(depth_buff, uv).rgb;

    vec3 id = vec3(pos.x * width, pos.y * height, pos.z * depth);

    Interval bbb = fgetInterval(floor(id.x), floor(id.y), floor(id.z));
    Interval ubb = fgetInterval(ceil(id.x), floor(id.y), floor(id.z));
    Interval uub = fgetInterval(ceil(id.x), ceil(id.y), floor(id.z));
    
    //col = vec4(depth);
    //col.a = 1;
    imageStore(imgOut, coord, col);
}
