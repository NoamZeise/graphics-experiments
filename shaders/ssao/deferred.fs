#version 460

in vec4 fpos;
in vec3 fnorm;
in vec2 fuv;

layout(location = 0) out vec4 position;
layout(location = 1) out vec4 normal;
layout(location = 2) out vec4 colour;

uniform vec4 obj_colour;
uniform int use_texture;

uniform sampler2D tex;

void main() {
  position = fpos/fpos.w;
  normal = vec4(normalize(fnorm), 1);
  
  vec4 c_obj = obj_colour;    
  if(use_texture == 1)
    c_obj = texture(tex, fuv);
  
  colour = c_obj;
}
