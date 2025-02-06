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
  float step_size;
  float thickness;
  int steps;
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
    int factor = int(exp2(cascade_level));
    float sz = (cascade_level < 1 ? 1 : 0.5) *
	params.steps * params.step_size * factor;
    int steps = params.steps * factor;
    float step_size = params.step_size;
    float offset = sz - (params.step_size * params.steps
			 * (cascade_level < 1 ? 1 : cascade_level));
    
    float range = offset + sz;

    vec3 start = pos;// + vec3(0, 0, -0.01);
    vec2 probe_uv = pos_to_uv(pos);
    vec3 probe_normal = normalize(texture(normal_buff, probe_uv).xyz);
    vec3 cam = normalize(-start.xyz);
    float ndv = dot(probe_normal, cam);
    
    // begin with emitted light for cascade 0
    vec4 ray_col =
	cascade_level == 0 ? texture(light_buff, probe_uv)
	: vec4(0);
    float prev_cos2 = 0; // = cos(PI/2)^2
    float max_pdv = -1;
    
    for(int i = 0; i < steps; i++) {
      	vec2 uv = pos_to_uv(pos);
	vec4 frag_pos = texture(depth_buff, uv);
	
	float zdiff = start.z - frag_pos.z;
	vec3 start_to_pos = frag_pos.xyz - start;
	float dist = length(start_to_pos);
	start_to_pos /= dist;
	float min_dist = dist;

	if(zdiff > 0) { // sample frag infront of start
	    float max_thickness = params.thickness * factor;
	    float thickness = min(max_thickness, zdiff);
	    min_dist = length((frag_pos.xyz + vec3(0, 0, thickness))
			      - start);
	}

	float pdv = dot(start_to_pos, cam);
	if(pdv > ndv) {
	  // above normal
	  // TODO: handle this case
	  //continue;
	}
	if(pdv > max_pdv) {
	   max_pdv = pdv;
	}
	
	float cos_a2 = dot(start_to_pos, probe_normal);
	cos_a2 *= cos_a2;

	bool inrange =
	    (dist > offset && dist < range) ||
	    (min_dist > offset && min_dist < range) ||
	    (min_dist < offset && dist > range);
	if(inrange &&
	   cos_a2 > prev_cos2 &&
	   frag_pos.a != 0 &&
	   uv_in_range(uv)) {

	  float d_phi = (2*PI) / dim.z;
	  float d_theta = cos_a2 - prev_cos2;
	  float brdf = 2*PI;
	  vec4 incoming_radiance = texture(light_buff, uv);
	  ray_col += (1.0/2) * d_phi * d_theta
	      * brdf * incoming_radiance;
	  prev_cos2 = cos_a2;
	}
	pos += dir * step_size;
    }
    ray_col.a = max_pdv;
    
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
      ray_hit.rgb += merged_cascade.rgb*pow(ratio, 0.5)*(cascade_level+1);
      ray_hit.a = merged_cascade.a;
    }
  }
  return ray_hit;
}

vec4 avg_dirs(uvec3 id) {
  vec4 total = vec4(0);
  for(uint i = 0; i < dim.z; i++) {
    vec4 ray = read_interval(id.x, id.y, i, uvec3(dim));
    total += ray;
  }
  return total;
}

void main() {
  uvec3 id = gl_GlobalInvocationID.xyz;
  if(cascade_level >= 0) {
    write_interval(id.x, id.y, id.z, gl_NumWorkGroups.xyz, cascade_ray(id));
  } else {
    write_interval(id.x, id.y, id.z, uvec3(dim), avg_dirs(id));
  }
}
