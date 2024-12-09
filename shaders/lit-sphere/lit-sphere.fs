#version 460

const float PI = 3.14159;

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;
out vec4 colour;

uniform vec3 cam;
uniform vec3 light_dir;
uniform sampler2D tex;
uniform sampler2D matcap;
uniform mat4 viewmat;
uniform int enableMC;


void main() {
  vec3 normal = normalize(fnorm);
  vec3 view = normalize(fpos - cam);
  
  vec4 c_light = vec4(1);

  vec4 c_obj = texture(tex, fuv);

  float lambertian = dot(normal, light_dir);
  lambertian = smoothstep(0.2, 0.3, lambertian);
  lambertian = clamp(lambertian, 0.4, 1.0);
  
  colour = c_obj * c_light * lambertian;

  if(enableMC == 1) {
    vec2 mcuv = vec2(viewmat * vec4(normal, 0)) * 0.5 + vec2(0.5,0.5);
    vec4 mccol = texture(matcap, vec2(mcuv.x, 1.0 - mcuv.y));  
    colour = (colour * mccol)*0.25 + mccol*0.8;
  }
}
