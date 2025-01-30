#version 460

in vec4 fpos;
in vec3 fnorm;
in vec2 fuv;
in vec4 world_pos;
in vec3 world_norm;
in vec4 shadow_pos;

layout(location = 0) out vec4 colour;
layout(location = 1) out vec4 light;
layout(location = 2) out vec4 position;
layout(location = 3) out vec4 normal;

uniform vec3 cam;
uniform vec3 light_dir;
uniform vec4 obj_colour;
uniform int use_texture;
uniform int is_light;

uniform sampler2D tex;
uniform sampler2D shadow_map;

vec3 correct_light_pos() {
  vec4 p = shadow_pos;
  p /= p.w;
  p += vec4(1);
  return (p/2).xyz;
}

float vsm_shadow(vec3 n, vec3 l) {
  vec3 pos = correct_light_pos();
  float depth = pos.z;
  vec2 float_vec = texture(shadow_map, pos.xy).xy;
  float M1 = float_vec.x;
  float M2 = float_vec.y;
  if(depth <= M1) return 1.0;
  // clamp variance to avoid numerical artifacts
  float s2 = max(abs(M2 - M1*M1), 0.000001);
  float diff = depth - M1;
  float pmax = s2 / (s2 + diff*diff);
  return pmax;
}

void main() {
  vec3 wn = normalize(world_norm);
  
  float in_shadow = vsm_shadow(wn, light_dir);

  float lambertian = min(dot(light_dir, wn), in_shadow);
  
  vec4 c_obj = obj_colour;    
  if(use_texture == 1)
    c_obj = texture(tex, fuv);
  
  //colour = c_obj * lambertian; 
  
  if(is_light == 1) {
    light = c_obj;
  } else {
    light = vec4(0, 0, 0, 1);
  }
  position = fpos/fpos.w;
  normal = vec4(normalize(fnorm), 1);
}
