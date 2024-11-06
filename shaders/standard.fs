#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;
out vec4 colour;

uniform vec3 cam;
uniform vec3 light_dir;

uniform sampler2D tex;

#define PI 3.14159265358979

float chr(vec3 v, vec3 u) {
  // positive characteristic of dot product
  return (sign(dot(v, u))+1)/2;
}

vec4 lambertian() {
 vec4 c_obj = texture(tex, fuv);
 return c_obj * (1/PI);
}
/*
// schlick approximation
float fresnel(vec3 n, vec3 l) {
  float mat_ior = 0.045; // plastic
  float F0 = (mat_ior - 1) / (mat_ior + 1);
  F0 *= F0;  
  return F0 + (1 - F0)*pow(1-(max(dot(n,l), 0)), 5);
}

float ggx_lambda(vec3 n, vec3 s) {
  float ag = 0.25 * 0.25;
  float a = dot(n, s) / (ag * pow(1 - pow(dot(n, s), 2), 1/2));
  return (-1 + pow(1 + 1/(a*a), 1/2))/2;
}

float mask_factor(float x) {
  return (4.41*x)/(4.41*x + 1);
}

float joint_masking_shadowing(vec3 l, vec3 v, vec3 h, vec3 n) {
  float numerator = chr(h, v) * chr(h, l);
  float cv = ggx_lambda(n, v);
  float cl = ggx_lambda(n, l);
  float denominator = 1 + max(cv, cl) + mask_factor(dot(v, l))*min(cv, cl);
  return numerator / denominator;
}

// GGX
float distribution(vec3 n, vec3 h) {
  float roughness = 0.25;
  float ag = roughness * roughness;
  float ag2 = ag * ag;
  float num = chr(n, h) * ag2;
  float den = PI * pow(1 + dot(n, h)*dot(n, h)*(ag2 - 1), 2);
  return num/den;
}

// Cook-Torrance
float specular(vec3 normal, vec3 light, vec3 view) {
  vec3 half_vec = normalize(light + view);
  float F = fresnel(normal, light);
  float G2 = joint_masking_shadowing(light, view, half_vec, normal);
  float D = distribution(normal, half_vec);
  float factor = 4 * dot(normal, light) * dot(normal, view);
  return (F * G2 * D) / factor;
}
*/
vec4 brdf(vec3 normal, vec3 light, vec3 view) {
   return lambertian();
}

void main() {
  vec3 normal = normalize(fnorm);
  vec3 view = normalize(fpos - cam);
  vec4 c_light = vec4(1);
  
  colour = PI * brdf(normal, light_dir, view) * c_light * clamp(dot(normal,light_dir), 0.05, 1.0);
}
