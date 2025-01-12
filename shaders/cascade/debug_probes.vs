#version 460

layout (location = 0) in vec3 pos;

uniform ivec4 dim;

out vec4 fpos;

void main() {
  int id = gl_InstanceID;
  ivec3 ids = ivec3(0);
  ids.z = id / (dim.x * dim.y);
  ids.y = (id - (ids.z*dim.x*dim.y))/dim.x;
  ids.x = (id - (ids.z*dim.x*dim.y) - (ids.y * dim.x));

  vec4 sspos = vec4(ids.x / float(dim.x),
		    ids.y / float(dim.y),
		    ids.z / float(dim.z),
		    1);
  sspos *= 2;
  sspos -= vec4(1);

  vec4 np = vec4(pos.x, pos.y, pos.z, 1);
  
  fpos = sspos + np/(dim.x*10);
  fpos.w = 1;
  
  gl_Position = fpos;
}
