#version 460

layout (local_size_x = 10, local_size_y = 10, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D imgOut;

layout(std430, binding = 1) buffer Radiance_Intervals {
  vec4 interval[];
};

uniform sampler2D colour_buff;
uniform sampler2D light_buff;
uniform sampler2D depth_buff;

uniform ivec4 dim;

vec4 getInterval(int x, int y, int z) {
    return interval[z * dim.x * dim.y * dim.w
		  + y * dim.x         * dim.w
		  + x                 * dim.w];
}

vec4 mixIntervals(vec4 int1, vec4 int2, float t) {
  //return mix(int1, int2, t);
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
    vec3 fpos = texture(depth_buff, uv).rgb;

    // interpolate down to a single point
    //   X --- X
    //  /     /|       X --- X
    // X --- X |  -->  |     |  -->  X --- X  -->  X
    // |     | X       X --- X
    // X --- X`


    vec3 pndc = (fpos+vec3(1))/2;
    vec3 cascade_space =
	vec3(pndc.x * dim.x,
	     pndc.y * dim.y,
	     pndc.z * dim.z);

    vec3 cascade_fract =
	vec3(fract(cascade_space.x),
	     fract(cascade_space.y),
	     fract(cascade_space.z));
    
    int left = int(floor(cascade_space.x));
    int right = left + int(left < dim.x - 1);
    int down = int(floor(cascade_space.y));
    int up = down + int(down < dim.y - 1);
    int front = int(floor(cascade_space.z));
    int back = front + int(front < dim.z - 1);
    
    vec4 ul_sample = mixIntervals(
	getInterval(left, up, front),
	getInterval(left, up, back),
	cascade_fract.z);

    vec4 ur_sample = mixIntervals(
	getInterval(right, up, front),
	getInterval(right, up, back),
	cascade_fract.z);

    vec4 dl_sample = mixIntervals(
	getInterval(left, down, front),
	getInterval(left, down, back),
	cascade_fract.z);

    vec4 dr_sample = mixIntervals(
	getInterval(right, down, front),
	getInterval(right, down, back),
	cascade_fract.z);

    vec4 l_sample = mixIntervals(
	dl_sample,
	ul_sample,
	cascade_fract.y);

    vec4 r_sample = mixIntervals(
	dr_sample,
	ur_sample,
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
    //col = texture(colour_buff, uv);
    imageStore(imgOut, coord, col);
}  
