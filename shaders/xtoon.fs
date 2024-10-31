#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;
out vec4 colour;

uniform vec3 cam;
uniform sampler2D toontex;

void main() {
  vec3 normal = normalize(fnorm);
  vec3 view = normalize(fpos - cam);
  
  vec3 light = normalize(vec3(2, 3, 1));
  vec4 c_light = vec4(1);

  float dist = length(cam - fpos);
  dist = smoothstep(clamp((dist/10), 0.0, 0.999),
		    0.3, 0.35);

  float lambertian = clamp(dot(normal,light), 0.0, 1.0);
  lambertian = smoothstep(0.0, 1.0, lambertian + 0.3);
  lambertian = clamp(lambertian, 0.0, 0.99);
  vec4 toon = texture(toontex, vec2(lambertian, dist));

   colour = toon * c_light;
}
