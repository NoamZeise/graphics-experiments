#version 460

in vec2 uv;
out vec4 colour;

// metatexture pass output
uniform sampler2D mt;
// colour pass output
uniform sampler2D col;

uniform float offset_intensity;

void main() { 
  if(uv.x > 1 || uv.y > 1 || uv.x < 0 || uv.y < 0) discard;
  vec4 mtc = texture(mt, uv);
  vec2 offset = (mtc.rg - vec2(0.5, 0.5))*offset_intensity;
  //colour = texture(mt, uv)*0.5 + texture(col, uv+offset)*0.5;
  colour = texture(col, uv+offset);
}
