#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;

layout(location = 0) out vec4 colour;
layout(location = 1) out vec4 light;

uniform vec3 cam;
uniform vec3 light_dir;
uniform vec4 obj_colour;
uniform int use_texture;
uniform int is_light;

uniform sampler2D tex;

void main() {  
  vec4 c_obj = obj_colour;
  if(use_texture == 1)
    c_obj = texture(tex, fuv);
  colour = c_obj;
  
  if(is_light == 1) {
    light = vec4(1);//c_obj;
  } else {
    light = vec4(0, 0, 0, 1);
  }
}
