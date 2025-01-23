#version 460

layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(std430, binding = 0) buffer Radiance_Intervals {
  vec4 interval[];
};

uniform sampler2D colour_buff;
uniform sampler2D light_buff;
uniform sampler2D depth_buff;
uniform sampler2D normal_buff;

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

vec2 id_to_uv(uvec2 id, uvec2 sz) {
  return vec2((id.x / float(sz.x)) + 1/(sz.x*2.0),
	      (id.y / float(sz.y)) + 1/(sz.y*2.0));
}

vec3 id_to_pos(uvec2 id, uvec2 sz) {
  vec2 uv = id_to_uv(id, sz);
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
    int factor = int(exp2(cascade_level));
    float step_scale = (1 + 1 * cascade_level);
    float sz = params.steps * params.step_size * factor;
    int steps = params.steps * factor;
    float step_size = params.step_size;
    float offset = sz - (params.step_size * params.steps);

    pos += dir * offset;
    vec4 ray_col = vec4(0);
    float surface_depth = pos.z;
    for(int i = 0; i < steps; i++) {
	vec3 frag_pos = (sample_tex(pos, depth_buff)).xyz;
	vec4 sample_colour = sample_tex(pos, light_buff);
	
	if(frag_pos.z <= surface_depth && frag_pos.z < 1) {
	  surface_depth = frag_pos.z;	    
	  ray_col = sample_colour;
	  ray_col.a = surface_depth;
	}
	pos += dir * step_size;
    }
    
    return ray_col;
}

/*vec4 add_ray(vec4 ray, vec4 add, float depth) {
    if(add.a >= 0 && add.a <= depth){
	ray.rgb += add.rgb;
	if(add.a < ray.a)
	    ray.a = add.a;
    }
    return ray;
}*/

vec4 mix_intervals(vec4 int1, vec4 int2, float t, float depth) {
    if(int1.a > depth)
	int1 = vec4(0);
    if(int2.a > depth)
	int2 = vec4(0);
  if(int1.w == int2.w) {
    return mix(int1, int2, t);
  }
  if(int1.w == 0) {
    return int2;
  }
  if(int2.w == 0) {
    return int1;
  }
  return mix(int1, int2, t);
}

vec4 interpolate_ray(uvec3 pcd, float depth,
		     uint left, uint right, uint up, uint down,
		     uint rayid, vec2 fract) {
  vec4 ul = read_interval(left, up, rayid, pcd);
  vec4 ur = read_interval(right, up, rayid, pcd);
  vec4 dl = read_interval(left, down, rayid, pcd);
  vec4 dr = read_interval(right, down, rayid, pcd);  

  vec4 l = mix_intervals(dl, ul, fract.y, depth);
  vec4 r = mix_intervals(dr, ur, fract.y, depth);
  vec4 s = mix_intervals(l, r, fract.x, depth);
  return s;
  //ray = add_ray(ray, ul, depth);
  //ray = add_ray(ray, ur, depth);
  //ray = add_ray(ray, dl, depth);
  //ray = add_ray(ray, dr, depth);  
  //ray.rgb /= 4;
  //return ray;
}

vec4 avg_prev(vec4 ray1, vec4 ray2) {
  vec4 m = vec4(0);
  m.rgb = (ray1.rgb + ray2.rgb)/2;
  if(ray1.a > 0 && ray2.a > 0)
      m.a = min(ray1.a, ray2.a);
  else
      m.a = max(ray1.a, ray2.a);
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

    vec2 uvpos = id_to_uv(id.xy, cd.xy);
    vec2 ulpos = id_to_uv(uvec2(left,  up),   pcd.xy);
    vec2 urpos = id_to_uv(uvec2(right, up),   pcd.xy);
    vec2 dlpos = id_to_uv(uvec2(left,  down), pcd.xy);
    vec2 drpos = id_to_uv(uvec2(right, down), pcd.xy);

    vec2 pndc = vec2((probe_pos+vec3(1))/2);
    vec2 cascade_space = vec2(pndc.x * pcd.x - 1/(pcd.x*2),
			      pndc.y * pcd.y - 1/(pcd.y*2));
    vec2 fract = vec2(fract(cascade_space.x),
		      fract(cascade_space.y));    

    float surface_depth = (ray_hit.a == 0) ? probe_pos.z : ray_hit.a;
    
    vec4 int1 = interpolate_ray(pcd, surface_depth,
				left, right, up, down, pid.z,
                                fract);
    uint r2 = pid.z + 1;
    if(r2 >= pcd.z) r2 = 0;
    vec4 int2 = interpolate_ray(pcd, surface_depth,
				left, right, up, down, r2,
                                fract);

    vec4 prev = avg_prev(int1, int2);

    if(params.merge_rays != 0) {
      ray_hit.rgb += /*int(bool(ray_hit.a == 0)) */ prev.rgb;
      ray_hit.a += int(bool(ray_hit.a == 0)) * prev.a;
    }
  }
  
  return ray_hit;
}

vec4 avg_dirs(uvec3 id) {
  vec4 total = vec4(0);
  for(uint i = 0; i < dim.z; i++) {
    vec4 ray = read_interval(id.x, id.y, i, uvec3(dim));
    total += ray;// * ray.a;
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
