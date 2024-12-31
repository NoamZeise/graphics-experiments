#version 460

layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(std430, binding = 0) buffer Radiance_Intervals {
  vec4 interval[];
};

uniform sampler2D colour_buff;
uniform sampler2D light_buff;
uniform sampler2D depth_buff;

uniform int intervals_per_probe;
uniform vec2 interval_range;

uniform ivec4 dim;

void main() {
  uvec3 id = gl_GlobalInvocationID.xyz;
  vec3 pos = vec3(id.x / float(dim.x*dim.w),
		  id.y / float(dim.y),
		  id.z / float(dim.z));
  vec3 prepos = pos;
  pos /= 2;
  pos += vec3(1);
  
  interval[id.z * dim.x * dim.y * dim.w
	 + id.y * dim.x         * dim.w
	 + id.x]
      = vec4(prepos.x, prepos.y, prepos.z, 1);
}
