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
uniform mat4 projection;

struct CascadeParams {
  int steps;
  float step_size;
  int merge_rays;
};
uniform CascadeParams params;

const float PI = 3.14159;

vec2 id_to_uv(uvec2 id, uvec2 sz) {
  return vec2((id.x / float(sz.x)) + 1.0/(sz.x*2.0),
	      (id.y / float(sz.y)) + 1.0/(sz.y*2.0));
}

vec4 id_to_pos(uvec2 id, uvec2 sz) {
  vec2 uv = id_to_uv(id, sz);
  return texture(depth_buff, uv);
}

vec2 pos_to_uv(vec3 pos) {
  vec4 ndc = projection * vec4(pos, 1);
  ndc /= ndc.w;
  ndc += vec4(1);
  ndc /= 2;
  return vec2(ndc.x, ndc.y);
}

bool uv_in_range(vec2 uv) {
  return uv.x >= 0 && uv.x <= 1 && uv.y >= 0 && uv.y <= 1;
}

vec4 sample_tex(vec3 pos, sampler2D tex) {  
  return texture(tex, pos_to_uv(pos));
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
  float offset = float(id)/total + 1.0/(2.0*total);
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
  //return vec4(pos, 1);
  //return sample_tex(pos, depth_buff);
    int factor = int(exp2(cascade_level));
    float sz = params.steps * params.step_size * factor;
    int steps = params.steps * factor;
    float step_size = params.step_size;// * factor;
    float offset = sz - (params.step_size * params.steps// * (factor - 1));
			 * (cascade_level < 1 ? 1 : cascade_level));

    float range = offset + sz;

    vec3 start = pos + vec3(0, 0, -0.01);
    //pos += dir * offset;   
    
    vec4 ray_col = vec4(0);
    ray_col.a = 0;
    vec4 prev_col = vec4(0);
    float angle_interval = 0;
    vec3 cam = normalize(-start.xyz);
    
    for(int i = 0; i < steps; i++) {
      	vec2 uv = pos_to_uv(pos);
	vec4 frag_pos = texture(depth_buff, uv);
	vec3 start_to_pos = frag_pos.xyz - start;
	float dist = length(start_to_pos);	
	start_to_pos /= dist;
	float angle = 1 - acos(dot(start_to_pos, cam))/PI; //towrds cam
	// equiv :
	//float angle = start_to_pos.z*-1;	
	if(dist > offset && dist < range &&
	   angle > angle_interval
	   && frag_pos.a != 0
	   && uv_in_range(uv)) {
	  float angle_diff = angle - angle_interval;
	  vec4 sample_colour = texture(light_buff, uv);
	  /*if(prev_col.r == -1)	   
	    ray_col += sample_colour*angle_diff;
	  else
	  ray_col += (sample_colour + prev_col)*0.5*angle_diff;*/
	  ray_col += sample_colour
	    *angle_diff;//pow(angle_diff, 0.3);
	  //ray_col += vec4(0.1);
	  //if(ray_col.r < angle_diff)
	  //  ray_col = vec4(angle_diff);
	  prev_col = sample_colour;
	  angle_interval = angle;
	  ray_col.a = angle;
	}
	pos += dir * step_size;
    }
    
    return ray_col;
}

vec4 mix_intervals(vec4 int1, vec4 int2, float t) {
  return mix(int1, int2, t);
}

vec4 interpolate_ray(uvec3 pcd,
		     uint left, uint right, uint up, uint down,
		     uint rayid, vec2 frac) {
  // bilinearly interpolate using frac
  vec4 ul = read_interval(left, up, rayid, pcd);
  vec4 ur = read_interval(right, up, rayid, pcd);
  vec4 dl = read_interval(left, down, rayid, pcd);
  vec4 dr = read_interval(right, down, rayid, pcd);
  vec4 l = mix_intervals(dl, ul, frac.y);
  vec4 r = mix_intervals(dr, ur, frac.y);
  return mix_intervals(l, r, frac.x);
}

vec4 avg_prev_cascade(vec4 ray1, vec4 ray2) {
  vec4 m = vec4(0);
  m.rgb = ray1.rgb+ray2.rgb;
  m.a = max(ray1.a, ray2.a);
  return m;
}

vec4 cascade_ray(uvec3 id) {
  uvec3 cd = gl_NumWorkGroups.xyz;

  vec3 dir = ray_dir(id.z);
  vec4 probe_pos = id_to_pos(id.xy, cd.xy);
  vec4 ray_hit =
    probe_pos.a > 0 ? trace(probe_pos.xyz, dir) : vec4(0);
 
  //if(cascade_level < dim.w - 1)
  //  ray_hit = vec4(0);
  
  if(cascade_level < dim.w - 1) {
    uvec3 pcd = uvec3(cd.x/2, cd.y/2, cd.z*2);
    uvec3 pid = uvec3(id.x/2, id.y/2, id.z*2);
    uvec3 offset = uvec3(id.x % 2, id.y % 2, 0);
    uvec3 inv_offset = uvec3((id.x + 1) % 2, (id.y + 1) % 2, 0);

    uint left = pid.x;
    uint right = left + uint(left < pcd.x - 1 && left >= 0);
    uint down = pid.y;
    uint up = down + uint(down < pcd.y - 1 && down >= 0);

    vec2 pndc = vec2(id_to_uv(id.xy, cd.xy));
    vec2 cascade_space = vec2(pndc.x * pcd.x,// + 1/(pcd.x*2.0),
			      pndc.y * pcd.y);// + 1/(pcd.y*2.0));
    vec2 frac = vec2(fract(cascade_space.x),
		     fract(cascade_space.y));
    
    vec4 int1 = interpolate_ray(pcd,
				left, right, up, down, pid.z,
                                frac);
    uint r2 = pid.z + 1;
    if(r2 >= pcd.z) r2 = 0;
    vec4 int2 = interpolate_ray(pcd,
				left, right, up, down, r2,
                                frac);

    vec4 merged_cascade = avg_prev_cascade(int1, int2);
    
    float angle_interval = ray_hit.a;
    if(params.merge_rays != 0 && merged_cascade.a > angle_interval) {
      float diff = merged_cascade.a - angle_interval;
      float ratio = 1 - (angle_interval / merged_cascade.a);
      ray_hit.rgb += merged_cascade.rgb*ratio;//*(cascade_level + 1);
      ray_hit.a = merged_cascade.a;
    }
  }
  return ray_hit;
}

vec4 avg_dirs(uvec3 id) {
  vec4 total = vec4(0);
  for(uint i = 0; i < dim.z; i++) {
    vec4 ray = read_interval(id.x, id.y, i, uvec3(dim));
    total += ray * ray.a;
  }
  return total /dim.z;
}

void main() {
  uvec3 id = gl_GlobalInvocationID.xyz;
  if(cascade_level >= 0) {
    write_interval(id.x, id.y, id.z, gl_NumWorkGroups.xyz, cascade_ray(id));
  } else {
    write_interval(id.x, id.y, id.z, uvec3(dim), avg_dirs(id));
  }
}
