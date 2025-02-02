#version 460

layout (location = 0) in vec3 pos;

uniform ivec4 dim;
uniform mat4 projection;
uniform sampler2D depth_buff;

layout(std430, binding = 0) buffer Radiance_Intervals {
  vec4 interval[];
};

out vec4 fpos;
out vec4 ival;

vec4 getInterval(int x, int y) {
  vec4 iv =  interval[y * dim.x * dim.z
		      + x * dim.z];
  if(iv.a > 0) iv.a = 1;
  return iv;
}

void main() {
  int id = gl_InstanceID;
  ivec2 ids = ivec2(0);
  ids.y = id / dim.x;
  ids.x = id - (ids.y*dim.x);

  vec4 sspos = vec4(ids.x / float(dim.x) + 1/float(dim.x*2),
		    ids.y / float(dim.y) + 1/float(dim.y*2),
		    0,
		    1);
  vec4 buff_pos = projection *texture(depth_buff, sspos.xy);
  buff_pos /= buff_pos.w;
  sspos *= 2;
  sspos -= vec4(1);
  sspos.z = buff_pos.z;

  vec4 np = vec4(pos.x, pos.y, pos.z, 1);

  float near = 0.005;
  float far = 100;
  float d = (2 * near * far) / (far + near - sspos.z * (far - near));

  float dist = d;

  float sz = dim.x * dim.y;
  float modif = 2*sz;
  float scale = mix(0.03, 0.2, dist);  
  
  fpos = sspos + np/(1400*scale * log(dim.x+dim.y));
  fpos.w = 1;
  ival = getInterval(ids.x, ids.y);
  
  gl_Position = fpos;
}
