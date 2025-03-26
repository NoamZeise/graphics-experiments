#version 460
layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D imgOut;

layout(std430, binding = 1) buffer Radiance_Intervals {
  vec4 interval[];
};

uniform sampler2D colour_buff;
uniform sampler2D light_buff;
uniform sampler2D depth_buff;

uniform ivec4 dim;
uniform mat4 projection;

vec4 read_interval(int x, int y) {
    vec4 iv =  interval[y * dim.x * dim.z
			+ x * dim.z];
    return iv;
}

vec3 id_to_pos(int x, int y) {
  vec2 uv = vec2((x + 0.5) / float(dim.x),
		 (y + 0.5) / float(dim.y));
  return texture(depth_buff, uv).xyz;
}

float interp(vec3 pos1, vec3 pos2, vec3 mid) {
  float p1 = length(pos1-mid);
  float total = p1 + length(pos2-mid);
  return p1/total;
}

void main() {
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    vec2 pixel_pos = vec2(float(coord.x)/float(gl_NumWorkGroups.x),
			  float(coord.y)/float(gl_NumWorkGroups.y));
    vec2 uv = pixel_pos + vec2(0.5)/gl_NumWorkGroups.xy;		       

    vec3 fpos = texture(depth_buff, uv).xyz;
    
    vec2 cascade_space =
      vec2(pixel_pos.x * dim.x - 0.5,
	   pixel_pos.y * dim.y - 0.5);
    
    int left = int(trunc(cascade_space.x));
    int right = left + int((left < dim.x - 1) && cascade_space.x >= 0);
    int down = int(trunc(cascade_space.y));
    int up = down + int((down < dim.y - 1) && cascade_space.y >= 0);

    vec3 ld = id_to_pos(left, down);
    vec3 lu = id_to_pos(left, up);
    vec3 rd = id_to_pos(right, down);
    vec3 ru = id_to_pos(right, up);

    float l_y = interp(ld, lu, fpos);
    float r_y = interp(rd, ru, fpos);
    float x = interp((lu + ld)/2, (ru + rd)/2, fpos);
    
    vec4 l_sample = mix(
        read_interval(left, down),
	read_interval(left, up),
	l_y);

    vec4 r_sample = mix(
        read_interval(right, down),
	read_interval(right, up),
	r_y);

    vec4 final_sample = mix(
    	l_sample,
    	r_sample,
	x);
    
    vec4 frag_col = texture(colour_buff, uv);
    vec4 light_col = texture(light_buff, uv);
    vec3 ambient = light_col.a > 0 ? max((1 - final_sample.a), 0.1) * frag_col.rgb : vec3(0);
    
    vec4 final_colour =
        0.5*light_col // direct lighting
      + 0.25*vec4(final_sample.rgb*frag_col.rgb, 1) // indirect lighting
      + vec4(ambient, 0); // ambient lighting
    //final_colour = vec4(texture(depth_buff, uv).z > 0);

    final_colour *= 1;
    
    if(light_col.a == 0)
      final_colour = frag_col;
    imageStore(imgOut, coord, final_colour);
}  
