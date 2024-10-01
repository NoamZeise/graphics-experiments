#version 460

layout (location = 0) in vec3 pos;
layout (location = 1) in vec3 normal;

uniform mat4 model;
uniform mat4 norm_mat;
uniform mat4 viewproj;

out vec3 fpos;
out vec3 fnorm;

void main() {
  vec4 world = model * vec4(pos, 1);
  fpos = vec3(world);
  fnorm = vec3(norm_mat * vec4(normal, 1));
  gl_Position = viewproj * world;
}