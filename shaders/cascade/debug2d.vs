#version 460

layout (location = 0) in vec3 pos;

uniform ivec4 dim;

uniform sampler2D depth_buff;

out vec4 fpos;

void main() {
  int id = gl_InstanceID;
  ivec2 ids = ivec2(0);
  ids.y = id / dim.x;
  ids.x = id - (ids.y*dim.x);

  vec4 sspos = vec4(ids.x / float(dim.x),
		    ids.y / float(dim.y),
		    0,
		    1);
  vec3 buff_pos = texture(depth_buff, sspos.xy).xyz;
  sspos *= 2;
  sspos -= vec4(1);
  sspos.z = buff_pos.z;

  vec4 np = vec4(pos.x, pos.y, pos.z, 1);

  float near = 0.01;
  float far = 50;
  float d = (2 * near) / (far + near - sspos.z * (far - near);     

  float dist = d;
  dist = pow(dist, 1);
  float scale = mix(0.02, 0.1, dist);
  
  fpos = sspos + np/(dim.x*dim.y*scale);
  fpos.w = 1;
  
  gl_Position = fpos;
}
