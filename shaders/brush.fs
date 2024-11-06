#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;
in vec3 modelpos;
out vec4 colour;

uniform vec3 cam;
uniform sampler2D tex;
uniform sampler2D brushtex;

const float PI = 3.14159;

void main() {
  vec3 normal = normalize(fnorm);
  vec3 view = normalize(fpos - cam);
  
  vec3 light = normalize(vec3(2, 3, 1));
  vec4 c_light = vec4(1);

  vec4 c_obj = texture(tex, fuv);

  float lambertian = dot(normal,light);
  lambertian = smoothstep(0.0, 0.4, lambertian + 0.05);
  vec3 dir = normalize(cross(normal, light));
  vec3 sdir = cross(normal, dir);
  vec3 cardinal = normalize(cross(light, vec3(0, 1, 0)));
  vec3 c2 = normalize(cross(cardinal, light));
  
  float cd = dot(normal, cardinal);
  float cd2 = dot(normal, c2);

  cd =  -2*acos(cd )/PI + 1;
  cd2 = -2*acos(cd2)/PI + 1;
  if (abs(cd - 0.2) > abs(cd2 - 0.2))
    cd = cd2;
  
  float buvx = abs(cd)*10;
  vec4 brush = texture(brushtex, vec2(buvx, clamp(lambertian, 0.0, 0.99)));
  if(lambertian < 1.0)
    lambertian += brush.r/5;
  lambertian = clamp(lambertian, 0.2, 1.0);
  
  colour = c_obj * c_light * lambertian;
  //colour += vec4(5, 0, 0, 0)*(0.5*cd+0.5);
}
