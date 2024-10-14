#version 460

in vec2 uv;
out vec4 colour;

// metatexture pass output
uniform sampler2D mt;
// colour pass output
uniform sampler2D col;

void main() { 
  if(uv.x > 1 || uv.y > 1 || uv.x < 0 || uv.y < 0) discard;
  vec2 offset = texture(mt, uv).rg/80;
  //colour = texture(mt, uv)*0.5 + texture(col, uv)*0.5;
  colour = texture(col, uv+offset);
}
