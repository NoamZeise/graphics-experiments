#version 460
layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D img_result;

uniform sampler2D blight;
uniform sampler2D bcolour;

void main() {
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    vec2 uv = vec2(float(coord.x + 0.5)/float(gl_NumWorkGroups.x),
		   float(coord.y + 0.5)/float(gl_NumWorkGroups.y));
    vec4 light = texture(blight, uv)*0.8;
    vec4 col = texture(bcolour, uv);
    light += (light.a == 0) ? col : col * 0.4; // ambient light
    
    imageStore(img_result, coord, light);
}  
