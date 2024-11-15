#version 460

layout (location = 0) in vec3 pos;

uniform mat4 model;
uniform mat4 viewproj;

out vec4 fpos;

void main() {
  vec4 world = model * vec4(pos, 1);
  fpos = viewproj * world;
  gl_Position = fpos;
}
