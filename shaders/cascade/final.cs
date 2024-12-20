#version 460

layout (local_size_x = 10, local_size_y = 10, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D imgOut;

layout(std430, binding = 1) buffer Radiance_Intervals {
  vec4 interval[];
};

uniform sampler2D colour_buff;
uniform sampler2D light_buff;
uniform sampler2D depth_buff;

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
    //col = vec4(depth);
    //col.a = 1;
    imageStore(imgOut, coord, col);
}
