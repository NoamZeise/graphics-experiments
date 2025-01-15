#version 460

layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(std430, binding = 0) buffer Radiance_Intervals {
  vec4 interval[];
};

uniform sampler2D colour_buff;
uniform sampler2D light_buff;
uniform sampler2D depth_buff;

uniform ivec4 dim;
uniform int cascade_level;
uniform int write_other_buff;

vec4 sample_tex(vec3 pos, sampler2D tex) {
    vec2 uv = vec2(pos.x, pos.y);
    uv += vec2(1);
    uv /= 2;
    return texture(tex, uv);
}

vec4 read_interval(uint x, uint y, uint s, uvec3 size) {
  return interval[((write_other_buff == 0) ?
		   dim.x * dim.y * dim.z : 0)
		  + y * size.x * size.z +
		    x * size.z + s];
}

void write_interval(uint x, uint y, uint s, uvec3 size, vec4 data) {
  interval[((write_other_buff == 0) ?
	    0 : dim.x * dim.y * dim.z)
	   + y * size.x * size.z +
	     x * size.z + s] = data;
}

vec3 ray_dir(uint id, uint total) {
  vec3 dir = vec3(0);
    switch(id) {
    case 0:
      dir = vec3(0, -1, 0);
      break;
    case 1:
      dir = vec3(0, 1, 0);
      break;
    case 2:
      dir = vec3(1, 0, 0);
      break;
    case 3:
      dir = vec3(-1, 0, 0);
      break;
    case 4:
      dir = vec3(0, 0, 1);
      break;
    case 5:
       dir = vec3(0, 0, -1);
      break;
    case 6:
      dir = vec3(1/1.414, 0, -1/1.414);
      break;
    case 7:
      dir = vec3(-1/1.414, 0, 1/1.414);
      break;      
    }
    return dir;
}

vec4 trace(vec3 pos, vec3 dir) {
    vec4 ray_col = vec4(0);
    const int STEPS = 15;
    float step_size = 0.002;
    float offset = 0;

    pos += dir * offset;
    for(int i = 0; i < STEPS; i++) {
	vec3 new_pos = pos + dir * step_size;	
	vec3 frag_pos = (sample_tex(new_pos, depth_buff)).xyz;
	// might be slow
	if(frag_pos.z < new_pos.z && frag_pos.z + 0.1 > new_pos.z && ray_col.w == 0) {
	    ray_col = sample_tex(new_pos, light_buff);
	    ray_col.w = 1;
	}
	pos = new_pos;
    }
    
    return  
      ray_col;
    //vec4(poss.x, poss.y, poss.z, 1);
}

vec4 cascade_ray(uvec3 id) {
  
  vec3 probe_pos = vec3(id.x / float(gl_NumWorkGroups.x),
			id.y / float(gl_NumWorkGroups.y),
			0);
  probe_pos *= 2;
  probe_pos -= vec3(1);

  vec3 frag_pos = sample_tex(probe_pos, depth_buff).xyz;
  probe_pos.z = frag_pos.z;
  
  int factor = int(exp2(cascade_level));
  int samples = dim.z*factor;

  vec3 dir = ray_dir(id.z, gl_NumWorkGroups.z);
  
  return trace(probe_pos, dir);
}

vec4 avg_dirs(uvec3 id) {
  vec4 total = vec4(0);
  for(uint i = 0; i < dim.z; i++) {
    vec4 ray = read_interval(id.x, id.y, i, uvec3(dim));
    total += ray * ray.a;
  }
  return total / dim.z;
}

void main() {
  uvec3 id = gl_GlobalInvocationID.xyz;
  if(cascade_level >= 0) {
    write_interval(id.x, id.y, id.z, gl_NumWorkGroups.xyz, cascade_ray(id));
  } else {
    write_interval(id.x, id.y, id.z, uvec3(dim), avg_dirs(id));
  }
}
