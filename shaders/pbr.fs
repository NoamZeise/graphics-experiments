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

float lambertian() {
 return (1/PI);
}

// schlick approximation
float fresnel(float mat_ior, float ndl) {
  float F0 = (mat_ior - 1) / (mat_ior + 1);
  F0 *= F0;
  return F0 + (1 - F0)*pow(1-ndl, 5);
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
float distribution(vec3 n, vec3 h, float roughness) {
  float ag = roughness * roughness;
  float ag2 = ag * ag;
  float num = chr(n, h) * ag2;
  float ndh2 = pow(max(dot(n, h), 0), 2);
  float den = PI * pow(1 + ndh2*(ag2 - 1), 2);
  return num/den;
}

// Cook-Torrance
float specular(vec3 normal, vec3 light, vec3 view, float F, float roughness) {
  vec3 half_vec = normalize(light + view);
  float nl = max(dot(normal, light), 0);
  float nh = max(dot(normal, half_vec), 0);
  float G2 = joint_masking_shadowing(light, view, half_vec, normal);
  float D = distribution(normal, half_vec, roughness);
  float factor = 4 * nl * nh;
  return (F * G2 * D) / max(factor, 0.000001);
}

vec4 brdf(vec3 normal, vec3 light, vec3 view) {
   vec4 c_obj = texture(tex, fuv);
   float ndl = max(dot(normal, light), 0.0);
   float fresnel = fresnel(0.045 /* plastic IOR */, ndl);
   float specular = specular(normal, light, view, fresnel, 0.6);
   return lambertian() * c_obj * fresnel;/* + specular*vec4(1));*/
}

void main() {
  vec3 normal = normalize(fnorm);
  vec3 view = normalize(fpos - cam);
  vec4 c_light = vec4(1);
  
  colour = PI * brdf(normal, light_dir, view) * c_light *
    //clamp(dot(normal,light_dir), 0.05, 1.0);
    clamp(dot(normal,light_dir), 0.1, 1.0);
  colour *= 1.5f;
}
