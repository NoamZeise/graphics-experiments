#version 460

layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(std430, binding = 1) buffer Radiance_Intervals {
  vec4 interval[];
};

uniform sampler2D light_buff;
uniform sampler2D depth_buff;

uniform int intervals_per_probe;
uniform vec2 interval_range;

void main() {
}
