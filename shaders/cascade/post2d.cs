#version 460
layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D imgOut;

layout(std430, binding = 1) buffer Radiance_Intervals {
  vec4 interval[];
};

uniform sampler2D colour_buff;
uniform sampler2D light_buff;
uniform sampler2D depth_buff;
uniform sampler2D normal_buff;

uniform ivec4 dim;

vec4 getInterval(int x, int y) {
    vec4 iv =  interval[y * dim.x * dim.z
			+ x * dim.z];
    if(iv.a > 0) iv.a = 1;
    return iv;
}

vec4 mixIntervals(vec4 int1, vec4 int2, float t) {
  if(int1.w == int2.w) {
    return mix(int1, int2, t);
  }
  if(int1.w == 0) {
    return int2;
  }
  if(int2.w == 0) {
    return int1;
  }
  return mix(int1, int2, t);
}

void main() {
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    vec2 uv = vec2((0.5 + float(coord.x))/float(gl_NumWorkGroups.x),
		   (0.5 + float(coord.y))/float(gl_NumWorkGroups.y));

    vec3 fpos = texture(depth_buff, uv).xyz;
    //
    //TODO: adjust based on position and
    //      probe sample point positions

    vec3 pndc = (fpos+vec3(1))/2;
    vec2 cascade_space =
      vec2(pndc.x * dim.x - 1/(dim.x*2),
	   pndc.y * dim.y - 1/(dim.y*2));
    vec2 cascade_fract =
	vec2(fract(cascade_space.x),
	     fract(cascade_space.y));
    
    int left = int(floor(cascade_space.x));
    int right = left + int(left < dim.x - 1 && cascade_space.x >= 0);
    int down = int(floor(cascade_space.y));
    int up = down + int(down < dim.y - 1 && cascade_space.y >= 0);
    
    vec4 l_sample = mixIntervals(
	getInterval(left, down),
	getInterval(left, up),
	cascade_fract.y);

    vec4 r_sample = mixIntervals(
	getInterval(right, down),
	getInterval(right, up),
	cascade_fract.y);

    vec4 final_sample = mixIntervals(
	l_sample,
	r_sample,
	cascade_fract.x);

        if(pndc.z == 1)
	final_sample = vec4(1);
    
    vec4 frag_col = texture(colour_buff, uv);
    //final_sample = vec4(pndc.x, pndc.y, pndc.z, 1);
    //vec4 col = final_sample * frag_col;
    vec4 col = final_sample;
    //col = texture(normal_buff, uv);
    imageStore(imgOut, coord, col);
}  
