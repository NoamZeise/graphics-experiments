#version 460
layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D img_result;

uniform sampler2D bcolour;
uniform sampler2D blight;
uniform sampler2D bssao;

void main() {
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    vec2 uv = vec2(float(coord.x + 0.5)/float(gl_NumWorkGroups.x),
		   float(coord.y + 0.5)/float(gl_NumWorkGroups.y));
    
    vec4 col = texture(bcolour, uv);
    vec4 light = texture(blight, uv);
    float ao = texture(bssao, uv).r;
    
    vec3 ambient = light.a > 0 ? (ao * col.xyz) : col.xyz;
    vec3 final_col = light.rgb*0.8 + ambient*0.8;
    imageStore(img_result, coord, vec4(final_col, 1));
}  
