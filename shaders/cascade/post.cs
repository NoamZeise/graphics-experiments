#version 460
layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D imgOut;

layout(std430, binding = 1) buffer Radiance_Intervals {
  vec4 interval[];
};

uniform sampler2D colour_buff;
uniform sampler2D light_buff;
uniform sampler2D depth_buff;
uniform sampler2D normal_buff;

uniform ivec4 dim;
uniform mat4 projection;

vec4 getInterval(int x, int y) {
    vec4 iv =  interval[y * dim.x * dim.z
			+ x * dim.z];
    return iv;
}

vec3 intervalPos(int x, int y) {
  vec2 uv = vec2((x + 0.5) / float(dim.x),
		 (y + 0.5) / float(dim.y));
  return texture(depth_buff, uv).xyz;
}

float project_to_fract(vec3 p1, vec3 p2, vec3 x) {
  vec3 to_x = x - p1;
  vec3 to_p2 = p2 - p1;
  return dot(to_x, to_p2)/dot(to_p2, to_p2);
}

vec3 project_to_line(vec3 p1, vec3 p2, vec3 x) {
  float t = project_to_fract(p1, p2, x);
  return p1 + t*(p2 - p1);
}

vec4 mixIntervals(vec4 int1, vec4 int2, float t) {
  return mix(int1, int2, t);
}

float d2(vec3 p1, vec3 p2) {
  vec3 diff = p1 - p2;
  return dot(diff, diff);
}
void main() {
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    vec2 pixel_pos = vec2(float(coord.x)/float(gl_NumWorkGroups.x),
			  float(coord.y)/float(gl_NumWorkGroups.y));
    vec2 uv = pixel_pos + vec2(0.5)/gl_NumWorkGroups.xy;		       

    vec4 fpos = texture(depth_buff, uv);
    
    vec2 cascade_space =
      vec2(pixel_pos.x * dim.x - 0.5,
	   pixel_pos.y * dim.y - 0.5);
    vec2 cascade_fract =
      vec2(fract(cascade_space.x),
	   fract(cascade_space.y));
    
    int left = int(trunc(cascade_space.x));
    int right = left + int((left < dim.x - 1) && cascade_space.x >= 0);
    int down = int(trunc(cascade_space.y));
    int up = down + int((down < dim.y - 1) && cascade_space.y >= 0);

    /*vec3 ld = intervalPos(left, down);
    vec3 lu = intervalPos(left, up);
    vec3 rd = intervalPos(right, down);
    vec3 ru = intervalPos(right, up);

    vec3 l = project_to_line(ld, lu, fpos.xyz);
    vec3 r = project_to_line(rd, ru, fpos.xyz);
    vec3 mid = project_to_line(l, r, fpos.xyz);

    float ld_dist = d2(ld, fpos.xyz);
    float lu_dist = d2(lu, fpos.xyz);
    float rd_dist = d2(rd, fpos.xyz);
    float ru_dist = d2(ru, fpos.xyz);
    float total = ld_dist +
      lu_dist +
      rd_dist +
      ru_dist;
    ld_dist = 1 - (ld_dist / 3*total);
    lu_dist = 1 - (lu_dist / 3*total);
    rd_dist = 1 - (rd_dist / 3*total);
    ru_dist = 1 - (ru_dist / 3*total);
      
    vec4 final_sample =
      getInterval(left, down) * ld_dist +
      getInterval(left, up) * lu_dist +
      getInterval(right, down) * rd_dist +
      getInterval(right, up) * ru_dist;*/      
    
    //float du_frac = project_to_fract(ld, lu, mid);
    //float lr_frac = project_to_fract(ld, rd, mid);
    
    //vec4 mid_p = (projection * vec4(mid, 1));
    //mid_p /= mid_p.w;
    //vec2 mid_uv = (mid_p.xy + vec2(1))/2;
    //float l = clamp(project_to_fract(ld, lu, fpos.xyz), 0, 1);
    //float r = clamp(project_to_fract(rd, ru, fpos.xyz), 0, 1);
    //float u = clamp(project_to_fract(lu, ru, fpos.xyz), 0, 1);
    //float d = clamp(project_to_fract(ld, rd, fpos.xyz), 0, 1);

    //cascade_fract = vec2(1, 1);
    
    vec4 l_sample = mixIntervals(
	getInterval(left, down),
	getInterval(left, up),
	cascade_fract.y);

    vec4 r_sample = mixIntervals(
	getInterval(right, down),
	getInterval(right, up),
	cascade_fract.y);

    vec4 final_sample = mixIntervals(
	l_sample,
	r_sample,
	cascade_fract.x);

    //  if(fpos.z == 1)
    //  final_sample = vec4(1);o
    
    vec4 frag_col = texture(colour_buff, uv);
    vec4 light_col = texture(light_buff, uv);
    //final_sample = vec4(pndc.x, pndc.y, pndc.z, 1);
    vec4 col = vec4(1 - final_sample.a);//getInterval(left, down);//final_sample;//final_sample;// * frag_col;
    //vec4 col = final_sample;
    //col = (vec4(1) + texture(normal_buff, uv))/2;
    //col = vec4(texture(depth_buff, uv).xyz, 1);
    //col = texture(depth_buff, uv) - col + vec4(0.5);
    //col = vec4(cascade_fract.x, cascade_fract.y, 0, 1);
    //col = vec4(mid, 1);
    /*col = vec4(
	       mid.xyz//project_to_line(ru, lu, fpos.xyz)
	       - texture(depth_buff, uv).xyz, 1)*3;
    col.r = abs(col.r);
    col.g = abs(col.g);
    col.b = abs(col.b);*/
    imageStore(imgOut, coord, col);
}  
