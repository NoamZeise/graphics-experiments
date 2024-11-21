#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;
in vec4 sm_pos;
out vec4 colour;

uniform vec3 cam;
uniform vec3 light_dir;
uniform sampler2D tex;
uniform sampler2D shadow_map;

vec3 correct_light_pos() {
  vec4 p = sm_pos;
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

const float ROOT2 = 1.414213562;

vec2 getGridPos() {
  vec2 grid = vec2(fpos.x, fpos.y);
  vec3 nabs = vec3(abs(fnorm.x), abs(fnorm.y), abs(fnorm.z));  
  if(nabs.x >= max(nabs.y, nabs.z))
    grid = vec2(fpos.y, fpos.z);
  else if (nabs.y >= max(nabs.y, nabs.z))
    grid = vec2(fpos.x, fpos.z);
  float rot = ROOT2/2;
  grid = vec2(rot*grid.x - rot*grid.y, rot*grid.x + rot*grid.y);
  return grid;
}

void main() {
  vec3 normal = normalize(fnorm);
  vec3 view = normalize(fpos - cam);
  
  vec3 light = light_dir;
  vec4 c_light = vec4(1);

  vec4 c_obj = texture(tex, fuv);

  float in_shadow = vsm_shadow(normal, light_dir);
  
  float lambertian = min(dot(light, normal), in_shadow);
  //lambertian*lambertian;
  
  vec2 grid = getGridPos();
  float freq = 20; // higher -> smaller dots + dot spacing  
  grid = vec2(fract(grid.x*freq), fract(grid.y*freq));
  // lines instead of dots
  //grid.y = 0.1;

  float d = length(grid - vec2(0.5, 0.5));
  
  float sum_pos = fpos.x+fpos.y+fpos.z;
  float d_x = dFdx(sum_pos) + dFdy(sum_pos);

  float cd = length(fpos - cam);
  
  float range = 0.15 * (abs(d_x)*500);
  float darkest = 0.9; // 1 -> dots go to black
  
  float size = 0.5*((lambertian-0.7)/-1.5); // larger spots when darker
  size = clamp(size, 0+cd/15, 1);
  
  float tone = smoothstep(size - range, size + range, d);
  tone = clamp(tone, 1 - abs(clamp(lambertian-0.3, -darkest, 0.0)), 1.0);
  
  lambertian = clamp(lambertian, tone, 1.0);

  colour = c_obj * c_light * lambertian;
  colour = mix(colour * 1.8,
	       mix(vec4(0.5, 0.7, 0.8, 1), colour, 1.2), 0.5);
  //colour = vec4(vec3(lambertian), 1);
  //colour = vec4(vec3(tone), 1);

  //if(in_shadow > 0.99999) colour = vec4(1, 0, 0, 1);
  //colour.r = in_shadow;
}
