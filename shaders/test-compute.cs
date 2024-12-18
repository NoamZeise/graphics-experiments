#version 460

layout (local_size_x = 10, local_size_y = 10, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D imgOut;

layout(std430, binding = 2) buffer data {
  float ssbo[];
};

uniform sampler2D colour_buff;
uniform sampler2D depth_buff;

void main() {
    vec4 col = vec4(1);    
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    col.x = float(coord.x)/(gl_NumWorkGroups.x);
    col.y = float(coord.y)/(gl_NumWorkGroups.y);
    col.z = ssbo[coord.x*gl_NumWorkGroups.y + coord.y];
    vec2 uv = vec2((0.5 + float(coord.x))/float(gl_NumWorkGroups.x),
		   (0.5 + float(coord.y))/float(gl_NumWorkGroups.y));
    //col = vec4(pow(texture(depth_buff, uv).r, 500));
    col = texture(colour_buff, uv);
    imageStore(imgOut, coord, col);
}
