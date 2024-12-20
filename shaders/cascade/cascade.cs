#version 460

layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(std430, binding = 0) buffer Radiance_Intervals {
  vec4 interval[];
};

uniform sampler2D colour_buff;
uniform sampler2D light_buff;
uniform sampler2D depth_buff;

void main() {
  uvec3 id = gl_GlobalInvocationID.xyz;

  int width = 100;
  int height = 100;
  int depth = 100;
  int samples = 6;

  vec3 pos = vec3(id.x / width, id.y / height, id.z / depth);
  
  interval[id.z * 100 * 100 * 6 + id.y * 100 * 6 + id.x] = vec4(0.5);
}
