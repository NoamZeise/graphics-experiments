#version 460

layout (location = 0) in vec3 pos;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

uniform mat4 model;
uniform mat4 norm_mat;
uniform mat4 viewproj;
uniform mat4 shadow_vp;

out vec3 fpos;
out vec3 fnorm;
out vec2 fuv;
out vec4 sm_pos;

void main() {
  vec4 world = model * vec4(pos, 1);
  fpos = vec3(world);
  fuv = uv;
  fnorm = vec3(norm_mat * vec4(normal, 1));
  sm_pos = shadow_vp * world;
  gl_Position = viewproj * world;  
}
