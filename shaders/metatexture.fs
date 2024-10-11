#version 460

in vec3 fpos;
in vec3 fnorm;
in vec2 fuv;
out vec4 colour;

uniform sampler2D tex;
uniform vec3 cam;
uniform vec2 tex_dim;

void calc_uvs(inout vec2[4] uvs, vec2 E, vec2 uv) {
  uvs[0] = vec2(exp2(E.x)*uv.x,exp2(E.y)*uv.y);  
  uvs[1] = vec2(uvs[0].x*2, uvs[0].y);
  uvs[2] = vec2(uvs[0].x, uvs[0].y*2);
  uvs[3] = vec2(uvs[0].x*2, uvs[0].y*2);
}

float blend_factor(float x) {
  return -2*x*x*x + 3*x*x;
}

vec4 lerp(float t, vec4 v1, vec4 v2) {
  t = blend_factor(t);
  return t*v1 + (1-t)*v2;
}

vec4 metatexture(vec2 blend, vec2[4] u) {
  vec4 T[4];
  for(int i = 0; i < 4; i++) {
    T[i] = texture(tex, u[i]);
  }
  return lerp(blend.x,
	      lerp(blend.y, T[3], T[1]),
	      lerp(blend.y, T[2], T[0]));
}

// return skew0 and skew1 with blending factor
vec3 skew_phi(vec2 delu, vec2 delv, vec2 delM) {
  float epsilon = delM.x < delM.y ? (delM.x / delM.y) : (delM.y / delM.x);
  float skew = atan(epsilon)*dot(delu/delM.x, delv/delM.y);
  int n = 3;
  float ns = n * skew;
  float s = floor(ns);
  float skew0 = s/n;
  float skew1 = (s+1)/n;
  return vec3(atan(skew0/2), atan(skew1/2), ns - s);
}

void skew_uvs(inout vec2[4] uvs, float phi) {
  float cp = cos(phi);
  float sp = sin(phi);
  for(int i = 0; i < 4; i++)
    uvs[i] = vec2(cp*uvs[i].x + sp*uvs[i].y, sp*uvs[i].x + cp*uvs[i].y);
}

void main() {
  float scale = 1 - dot(normalize(fnorm), normalize(cam - fpos));
  scale *= scale * scale*scale*scale;
  
  int w = int(tex_dim.x);
  int h = int(tex_dim.y);

  float u = fuv.x;
  float v = fuv.y;

  vec2 delu = vec2(dFdx(u), dFdy(u));
  vec2 delv = vec2(dFdx(v), dFdy(v));
  vec2 delM = vec2(length(delu), length(delv));
  vec2 E = 1 / vec2(w*delM.x, h*delM.y);
  E = vec2(log2(E.x), log2(E.y));  
  vec2 Ef = vec2(floor(E.x), floor(E.y));
  vec2 blend = vec2(E.x - Ef.x, E.y - Ef.y);

  vec2[4] uvs0 = vec2[4](vec2(0, 0), vec2(0, 0), vec2(0, 0), vec2(0, 0));
  calc_uvs(uvs0, Ef, fuv);
  // copy
  vec2[4] uvs1;
  for(int i = 0; i < 4; i++) uvs1[i] = uvs0[i];
  
  vec3 phi = skew_phi(delu, delv, delM);  
  skew_uvs(uvs0, phi.x);
  skew_uvs(uvs1, phi.y);  
  
  vec4 T0 = metatexture(blend, uvs0);
  vec4 T1 = metatexture(blend, uvs1);

  colour = lerp(phi.z, T1, T0);
  colour += vec4(scale*0.006, 0, 0, 1);
}
