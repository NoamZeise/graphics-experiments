#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;
out vec4 colour;

uniform vec3 cam;
uniform vec3 light_dir;
uniform vec4 obj_colour;
uniform int use_texture;
uniform int is_light;

uniform sampler2D tex;

void main() {
  vec3 normal = normalize(fnorm);
  vec3 view = normalize(fpos - cam);
  vec4 c_light = vec4(1);
  vec4 c_obj = vec4(0, 0, 0, 1);
  if(is_light == 1) {
    c_obj = obj_colour;
    if(use_texture == 1)
      c_obj = texture(tex, fuv);
  }
  colour = c_obj;
}
