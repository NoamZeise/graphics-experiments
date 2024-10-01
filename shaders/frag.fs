#version 460

in vec3 fpos;
in vec3 fnorm;
out vec4 colour;

uniform vec3 cam;

#define PI 3.14159265358979

vec4 lambertian() {
 vec4 c_obj = vec4(1, 0, 0, 1);
 return c_obj * (1/PI);
}

vec4 brdf(vec3 light, vec3 view) {
   return lambertian();
}

void main() {
  vec3 normal = normalize(fnorm);
  vec3 view = normalize(fpos - cam);
  
  vec3 light = normalize(vec3(2, 3, 1));
  vec4 c_light = vec4(1);

  colour = PI * brdf(light, view) * c_light * clamp(dot(normal,light), 0, 1);
}