#version 460

in vec4 fpos;
in vec3 fnorm;
out vec4 colour;

void main() {
  colour = vec4((fnorm+vec3(1))/2, 1);
}
