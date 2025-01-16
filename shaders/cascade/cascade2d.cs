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

struct CascadeParams {
  int steps;
  float step_size;
  int merge_rays;
};
uniform CascadeParams params;

const float PI = 3.14159;

vec4 sample_tex(vec3 pos, sampler2D tex) {
    vec2 uv = vec2(pos.x, pos.y);
    uv += vec2(1);
    uv /= 2;
    return texture(tex, uv);
}

vec3 id_to_pos(uvec2 id, uvec2 sz) {
  vec2 uv = vec2((id.x / float(sz.x)) + 1/(sz.x*2.0),
		 (id.y / float(sz.y)) + 1/(sz.y*2.0));
  return texture(depth_buff, uv).xyz;
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

vec3 ray_dir(uint id) {
  int total = int(gl_NumWorkGroups.z);
  float offset = float(id)/total + 1.0/(2*total);
  offset *= 2 * PI;

  mat3 rot = mat3(1);
  float co = cos(offset);
  float so = sin(offset);

  rot[0][0] = co;
  rot[0][1] = so;
  rot[1][0] = -so;
  rot[1][1] = co;
  
  vec3 dir = rot * vec3(0, 1, 0);
    
  return dir;
}

vec4 trace(vec3 pos, vec3 dir) {
    vec4 ray_col = vec4(0);
    int factor = int(exp2(cascade_level));
    int steps = params.steps * factor;
    float step_size = params.step_size * factor;
    float offset = step_size - params.step_size;

    pos += dir * offset;
    for(int i = 0; i < steps; i++) {
	vec3 new_pos = pos + dir * step_size;	
	vec3 frag_pos = (sample_tex(new_pos, depth_buff)).xyz;
	if(frag_pos.z < new_pos.z && frag_pos.z + step_size > new_pos.z && ray_col.w == 0) {
	    ray_col = sample_tex(new_pos, light_buff);
	    ray_col.w = 1;
	}
	pos = new_pos;
    }
    
    return ray_col;
}

vec4 avg_prev(vec4 ray1, vec4 ray2) {
  vec4 m = (ray1 + ray2)/2;
  m.a = m.a == 0 ? 0.0 : 1.0;
  return m;
}

vec4 cascade_ray(uvec3 id) {

  uvec3 cd = gl_NumWorkGroups.xyz;
  
  vec3 probe_pos = id_to_pos(id.xy, cd.xy);

  vec3 dir = ray_dir(id.z);

  vec4 ray_hit = trace(probe_pos, dir);

  if(cascade_level < dim.w - 1) {
    uvec3 pcd = uvec3(cd.x/2, cd.y/2, cd.z*2);
    uvec3 pid = uvec3(id.x/2, id.y/2, id.z*2);
    uvec3 offset = uvec3(id.x % 2, id.y % 2, 0);
    uvec3 inv_offset = uvec3((id.x + 1) % 2, (id.y + 1) % 2, 0);

    uint left = pid.x;
    uint right = left + uint(left < pcd.x - 1 && left >= 0);
    uint down = pid.y;
    uint up = down + uint(down < pcd.y - 1 && down >= 0);

    /*vec3 ulpos = id_to_pos(uvec2(left,  up),   pcd.xy);
    vec3 urpos = id_to_pos(uvec2(right, up),   pcd.xy);
    vec3 dlpos = id_to_pos(uvec2(left,  down), pcd.xy);
    vec3 drpos = id_to_pos(uvec2(right, down), pcd.xy);
    float ulD = distance(ulpos, probe_pos);
      /*abs(ulpos.z - probe_pos.z)
      + 1/(cd.x*2) + inv_offset.x*(1/cd.x)
      + 1/(cd.y*2) + inv_offset.y*(1/cd.y);*/
    /*float urD = distance(urpos, probe_pos);
      /*abs(urpos.z - probe_pos.z)
      + 1/(cd.x*2) + offset.x*(1/cd.x)
      + 1/(cd.y*2) + inv_offset.y*(1/cd.y);*/
    /*float dlD = distance(dlpos, probe_pos);
      /*abs(dlpos.z - probe_pos.z)
      + 1/(cd.x*2) + inv_offset.x*(1/cd.x)
      + 1/(cd.y*2) + offset.y*(1/cd.y);*/
    /*float drD = distance(drpos, probe_pos);
      /*abs(drpos.z - probe_pos.z)
      + 1/(cd.x*2) + offset.x*(1/cd.x)
      + 1/(cd.y*2) + offset.y*(1/cd.y);*/
    //float normalizer = 1/ulD + 1/urD + 1/dlD + 1/drD;
    
    vec4 ul1 = read_interval(left, up, pid.z, pcd);
    vec4 ur1 = read_interval(right, up, pid.z, pcd);
    vec4 dl1 = read_interval(left, down, pid.z, pcd);
    vec4 dr1 = read_interval(right, down, pid.z, pcd);
    vec4 int1 = //(ul1/ulD + ur1/urD + dl1/dlD + dr1/drD)/normalizer;
    (ul1 + ur1 + dl1 + dr1) / 4;
    int1.a = int1.a == 0 ? 0.0 : 1.0;

    uint r2 = pid.z + uint(pid.z < pcd.z - 1);
    vec4 ul2 = read_interval(left, up, r2, pcd);
    vec4 ur2 = read_interval(right, up, r2, pcd);
    vec4 dl2 = read_interval(left, down, r2, pcd);
    vec4 dr2 = read_interval(right, down, r2, pcd);
    vec4 int2 = //(ul2/ulD + ur2/urD + dl2/dlD + dr2/drD)/normalizer;
      (ul2 + ur2 + dl2 + dr2) / 4;
    int2.a = int2.a == 0 ? 0.0 : 1.0;
    
    vec4 prev = avg_prev(int1, int2);

    if(params.merge_rays != 0)
      ray_hit += int(!bool(int(ray_hit.a))) * prev;
  }
  
  return ray_hit;
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
    write_interval(id.x, id.y, id.z, uvec3(dim),           avg_dirs(id));
  }
}
