#version 460

in vec4 fpos;
out vec4 colour;

void main() {
  float depth = fpos.z/fpos.w;
  depth += 1;
  depth /= 2;
  colour = vec4(depth, depth*depth, 0, 1);
}
