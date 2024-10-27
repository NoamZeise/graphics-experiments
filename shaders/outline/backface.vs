#version 460

layout (location = 0) in vec3 pos;
layout (location = 1) in vec3 normal;

uniform mat4 model;
uniform mat4 norm_mat;
uniform mat4 viewproj;

uniform vec3 cam;

out vec4 fpos;
out vec3 fnorm;

void main() {
  vec4 world = model * vec4(pos, 1);
  vec4 cw = vec4((cam - world.xyz), 0);
  float cwd = length(cw);
  fpos = viewproj * (world + (normalize(cw)/(cwd*10)));
  fnorm = vec3(norm_mat * vec4(normal, 1));
  vec4 ss_norm = viewproj * vec4(fnorm, 0);		     
  
  gl_Position = fpos; //+ vec4(ss_norm.xy, 0, 0)/50
    //+ vec4(0, 0, cw.z, 0)/100;
}
