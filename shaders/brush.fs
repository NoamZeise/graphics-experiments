#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;
in vec3 modelpos;
out vec4 colour;

uniform vec3 cam;
uniform vec3 light_dir;
uniform sampler2D tex;
uniform sampler2D brushtex;

const float PI = 3.14159;

void main() {
  vec3 normal = normalize(fnorm);
  vec3 view = normalize(fpos - cam);
  
  vec3 light = light_dir;
  vec4 c_light = vec4(1);

  float min_lambertian = 0.15;

  vec4 c_obj = texture(tex, fuv);
  
  vec3 c1 = normalize(cross(light, vec3(light.y, light.z, light.x)));
  vec3 c2 = normalize(cross(c1, light));
  
  float cd = dot(normal, c1);
  float cd2 = dot(normal, c2);

  cd =  -2*acos(cd )/PI + 1;
  cd2 = -2*acos(cd2)/PI + 1;
  if (abs(cd - 0.2) > abs(cd2 - 0.2))
    cd = cd2;

  float lambertian = dot(normal,light);
  
  float filterbrush = 1 - (smoothstep(abs(lambertian - min_lambertian - 0.1), 0.0, 0.05));
  
  float buvx = abs(cd)*4;
  vec4 brush = texture(brushtex, vec2(buvx, abs(lambertian) + 0.2));
  
  lambertian = smoothstep(0.15, 0.4, lambertian + 0.05);
  lambertian = clamp(lambertian, min_lambertian, 1.0)
               + (brush.r - 1)*0.4*filterbrush;
  
  colour = c_obj * c_light * lambertian;

  // brush effect only 
  //colour = vec4(0.5, 0.5, 0.5, 1) + vec4(0.1)*(brush.r - 1)*2*filterbrush;
}
