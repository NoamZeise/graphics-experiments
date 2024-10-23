#version 460

layout (location = 0) in vec3 pos;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

uniform mat4 model;
uniform mat4 norm_mat;
uniform mat4 viewproj;

out vec3 fpos;
out vec3 fnorm;
out vec2 fuv;

void main() {
  vec4 world = model * vec4(pos, 1);
  fpos = vec3(world);
  fuv = uv;
  fnorm = vec3(norm_mat * vec4(normal, 0));
  vec4 ss_norm = viewproj * vec4(fnorm, 0);
  gl_Position = (viewproj * world) + vec4(ss_norm.xy, 0, 0)/80;  
}
