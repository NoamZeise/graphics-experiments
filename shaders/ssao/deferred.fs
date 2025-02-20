#version 460

in vec4 fpos;
in vec3 fnorm;
in vec2 fuv;

layout(location = 0) out vec4 position;
layout(location = 1) out vec4 normal;

void main() {
  position = fpos/fpos.w;
  normal = vec4(normalize(fnorm), 1);
}
