#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;
in vec3 modelpos;
out vec4 colour;

uniform vec3 cam;
uniform vec3 light_dir;
uniform sampler2D tex;

void main() {
  vec3 normal = normalize(fnorm);
  vec3 view = normalize(fpos - cam);
  
  vec3 light = light_dir;
  vec4 c_light = vec4(1);

  vec4 c_obj = texture(tex, fuv);

  float lambertian = dot(light, normal);
  
  vec2 grid = vec2(fpos.x, fpos.y);
  vec3 nabs = vec3(abs(fnorm.x), abs(fnorm.y), abs(fnorm.z));
  if(nabs.x >= max(nabs.y, nabs.z))
    grid = vec2(fpos.y, fpos.z);
  else if (nabs.y >= max(nabs.y, nabs.z))
    grid = vec2(fpos.x, fpos.z);

  float freq = 20;
  grid = vec2(fract(grid.x*freq), fract(grid.y*freq));

  float d = length(grid - vec2(0.5, 0.5));
 
  float tone = 0;
  if(d > 0.4)
    tone = 1;
  
  lambertian = smoothstep(0.15, 0.4, lambertian + 0.05);
  lambertian = clamp(lambertian, tone, 1.0);

  colour = c_obj * c_light * lambertian;
  //colour = vec4(vec3(tone), 1);
}
