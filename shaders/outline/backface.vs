#version 460

layout (location = 0) in vec3 pos;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

uniform mat4 model;
uniform mat4 norm_mat;
uniform mat4 viewproj;

uniform float normal_divisor;

uniform vec3 cam;

out vec3 fpos;
out vec3 fnorm;
out vec2 fuv;

void main() {
  fnorm = vec3(norm_mat * vec4(normal, 1));
  fuv = uv;
  
  vec4 world = model * vec4(pos, 1);  
  vec4 screen = viewproj * world;
  fpos = screen.xyz;
  
  vec4 ss_norm = viewproj * vec4(fnorm, 0);
  
  gl_Position = screen
    + vec4(ss_norm.xy, 0, 0)/normal_divisor;
}
