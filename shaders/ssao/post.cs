#version 460
layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D img_result;

uniform sampler2D bposition;
uniform sampler2D bnormal;
uniform sampler2D bcolour;
uniform sampler2D bssao;

uniform vec3 light_dir;

#define BLUR 2

void main() {
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    vec2 uv = vec2(float(coord.x + 0.5)/float(gl_NumWorkGroups.x),
		   float(coord.y + 0.5)/float(gl_NumWorkGroups.y));    

    vec3 pos = texture(bposition, uv).xyz;
    vec3 norm = texture(bnormal, uv).xyz;
    vec3 col = texture(bcolour, uv).xyz;
    float ao = texture(bssao, uv).r;

    vec3 ambient = vec3(0.3 * col * ao);
    vec3 lighting = ambient;
    vec3 view = normalize(-pos);

    vec3 diffuse = vec3(max(dot(norm, light_dir), 0.0));
    vec3 halfvec = normalize(light_dir + view);
    vec3 spec = vec3(0.7*pow(max(dot(norm, halfvec), 0.0), 8.0));

    lighting += diffuse + spec;    
    
    imageStore(img_result, coord, vec4(lighting, 1));
}  
