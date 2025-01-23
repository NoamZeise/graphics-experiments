#version 460

in vec4 fpos;
in vec4 ival;

layout(location = 0) out vec4 colour;

void main() {
  if(ival.a == 0)
    colour = vec4(1, 0, 0, 1);
  else
    colour = ival;
}
