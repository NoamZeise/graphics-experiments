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

vec4 sample_tex(vec3 pos, sampler2D tex) {
    vec2 uv = vec2(pos.x, pos.y);
    uv += vec2(1);
    uv /= 2;
    return texture(tex, uv);
}

vec4 trace(vec3 pos, int id, int total) {
    vec4 ray_col = vec4(0);
    const int STEPS = 15;
    float step_size = 0.002;
    float offset = 0;

    vec3 dir = vec3(0, 0, 0);

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
  
  vec3 probe_pos = vec3(id.x / float(dim.x),
			id.y / float(dim.y),
			0);
  probe_pos *= 2;
  probe_pos -= vec3(1);

  vec3 frag_pos = sample_tex(probe_pos, depth_buff).xyz;
  probe_pos.z = frag_pos.z;
  
  int factor = int(exp2(cascade_level));
  int samples = dim.z*factor;
  
  return trace(probe_pos, int(id.z), samples);
}

vec4 avg_dirs(uvec3 id) {
  vec4 total = vec4(0);
  for(int i = 0; i < dim.z; i++) {
    vec4 ray = interval[id.y * dim.x * dim.z +
			id.x * dim.z + i];
    total += ray * ray.a;
  }
  return total / dim.z;
}

void main() {
  uvec3 id = gl_GlobalInvocationID.xyz;

  vec4 val = vec4(0);
  if(cascade_level >= 0) {
    val = cascade_ray(id);
  } else {
    val = avg_dirs(id);
  }

  interval[id.y * dim.x * dim.z +
	   id.x * dim.z +
	   id.z] = val;
}
