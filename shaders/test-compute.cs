#version 460

layout (local_size_x = 10, local_size_y = 10, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D imgOut;

layout(std430, binding = 2) buffer data {
  float ssbo[];
};

void main() {
    vec4 col = vec4(1);    
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    col.x = float(coord.x)/(gl_NumWorkGroups.x);
    col.y = float(coord.y)/(gl_NumWorkGroups.y);
    col.z = ssbo[coord.x*gl_NumWorkGroups.y + coord.y];
    imageStore(imgOut, coord, col);
}
