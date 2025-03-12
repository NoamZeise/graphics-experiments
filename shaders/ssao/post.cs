#version 460
layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D img_result;

uniform sampler2D bcolour;
uniform sampler2D blight;
uniform sampler2D bssao;

uniform vec3 light_dir;

#define BLUR 2

void main() {
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    vec2 uv = vec2(float(coord.x + 0.5)/float(gl_NumWorkGroups.x),
		   float(coord.y + 0.5)/float(gl_NumWorkGroups.y));

    vec3 col = texture(bcolour, uv).xyz;
    vec3 light = texture(blight, uv).xyz;
    float ao = texture(bssao, uv).r;

    vec3 ambient = 0.3 * ao * col;
    
    imageStore(img_result, coord, vec4(light + ambient, 1));
}  
