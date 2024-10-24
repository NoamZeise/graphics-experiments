#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;
out vec4 colour;

uniform vec3 cam;
uniform sampler2D tex;

void main() {
  vec3 normal = normalize(fnorm);
  vec3 view = normalize(fpos - cam);
  
  vec3 light = normalize(vec3(2, 3, 1));
  vec4 c_light = vec4(1);

  vec4 c_obj = texture(tex, fuv);

  float lambertian = dot(normal,light);
  lambertian = smoothstep(0.3, 0.46, lambertian);
  lambertian = clamp(lambertian, 0.1, 1.0);
  
  colour = c_obj * c_light * lambertian;
}
