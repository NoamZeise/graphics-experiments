#version 460
layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D img_result;

uniform sampler2D blight;
uniform sampler2D bcolour;

void main() {
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    vec2 uv = vec2(float(coord.x + 0.5)/float(gl_NumWorkGroups.x),
		   float(coord.y + 0.5)/float(gl_NumWorkGroups.y));
    vec3 light = texture(blight, uv).xyz;

    light += texture(bcolour, uv).xyz * 0.3;
    
    imageStore(img_result, coord, vec4(light, 1));
}  
