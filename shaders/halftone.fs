#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;
in vec3 modelpos;
out vec4 colour;

uniform vec3 cam;
uniform vec3 light_dir;
uniform sampler2D tex;

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

  float lambertian = dot(light, normal);
  
  vec2 grid = getGridPos();
  
  float sum_pos = fpos.x+fpos.y+fpos.z;
  float d_x = dFdx(sum_pos) + dFdy(sum_pos);
		  
  float size = 0.5*((lambertian-0.7)/-1.4);

  float freq = 20;
  grid = vec2(fract(grid.x*freq), fract(grid.y*freq));
  // lines instead of dots
  //grid.y = 0.1;

  float d = length(grid - vec2(0.5, 0.5));
  float range = 0.05 * (abs(d_x)*500);
  float darkest = 0.3;
  
  float tone = smoothstep(size - range, size + range, d);

  float tl = lambertian;
  lambertian = smoothstep(0.2, darkest, lambertian);
  lambertian = clamp(lambertian, tone + 0.8*abs(tl+0.9), 1.0);

  colour = c_obj * c_light * lambertian;
  //colour = vec4(vec3(lambertian), 1);
  //colour = vec4(vec3(tone), 1);
}
