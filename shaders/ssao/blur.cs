#version 460
layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D img_result;

uniform sampler2D ssao_buff;

#define BLUR 2

void main() {
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    vec2 uv = vec2(float(coord.x + 0.5)/float(gl_NumWorkGroups.x),
		   float(coord.y + 0.5)/float(gl_NumWorkGroups.y));    

    vec2 texelS = 1.0 / vec2(textureSize(ssao_buff, 0));
    float r = 0;
    for(int x = -BLUR; x < BLUR; x++) {
      for(int y = -BLUR; y < BLUR; y++) {
	r += texture(ssao_buff, uv + vec2(x, y) * texelS).r;
      }
    }
    
    imageStore(img_result, coord, vec4(r / float(BLUR*2*BLUR*2)));
}  
