#version 460

in vec2 uv;
out vec4 colour;

uniform sampler2D position;
uniform sampler2D normal;
uniform sampler2D noise;

void main() { 
  //if(uv.x > 1 || uv.y > 1 || uv.x < 0 || uv.y < 0) discard;
  //colour = texture(position, uv);
  colour = vec4(1, 0, 0, 0);
}
