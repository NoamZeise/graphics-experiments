#version 460

layout (location = 0) in vec3 pos;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform mat4 norm_mat;
uniform mat4 norm_view;
uniform mat4 light_vp;

out vec4 fpos;
out vec3 fnorm;
out vec2 fuv;
out vec4 world_pos;
out vec3 world_norm;
out vec4 shadow_pos;

void main() {
  vec4 world = model * vec4(pos, 1);
  vec3 world_normal = vec3(norm_mat * vec4(normal, 0));
  
  world_pos = world;
  world_norm = world_normal;
  shadow_pos = light_vp * world;

  fpos = view * world;
  fuv = uv;
  fnorm = vec3(norm_view * vec4(world_normal, 0));
  gl_Position = proj * fpos;
}
