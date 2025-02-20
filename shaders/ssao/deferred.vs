#version 460

layout (location = 0) in vec3 pos;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform mat4 norm_mat;
uniform mat4 norm_view;

out vec4 fpos;
out vec3 fnorm;
out vec2 fuv;

void main() {
  fpos = view * model * vec4(pos, 1);
  fuv = uv;
  fnorm = vec3(norm_view * norm_mat * vec4(normal, 0));
  gl_Position = proj * fpos;
}
