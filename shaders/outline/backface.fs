#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;

out vec4 colour;

uniform vec4 outline;

void main() {
  colour = outline;
}
