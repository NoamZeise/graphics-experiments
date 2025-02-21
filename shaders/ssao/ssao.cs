#version 460
layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(rgba32f, binding = 0) uniform image2D img_result;

uniform sampler2D bposition;
uniform sampler2D bnormal;
uniform sampler2D bnoise;

uniform float radius;
uniform mat4 proj;
uniform vec2 screen_res;
#define KERNEL_SIZE 64
uniform vec3 samples[KERNEL_SIZE];

void main() {
    ivec2 coord = ivec2(gl_GlobalInvocationID.xy);
    vec2 uv = vec2(float(coord.x + 0.5)/float(gl_NumWorkGroups.x),
		   float(coord.y + 0.5)/float(gl_NumWorkGroups.y));

  vec3 pos = texture(bposition, uv).xyz;
  vec3 normal = texture(bnormal, uv).xyz;
  vec3 rand = texture(bnoise, uv * (screen_res / 4.0)).xyz;
  vec3 tangent = normalize(rand - normal * dot(rand, normal));
  vec3 bitangent = cross(normal, tangent);
  mat3 tbn = mat3(tangent, bitangent, normal);

  float occlusion = 0;

  for(int i = 0; i < KERNEL_SIZE; i++) {
    vec3 smpl = tbn * samples[i];
    smpl = pos + smpl*radius;

    vec4 offset = proj * vec4(smpl, 1);
    offset /= offset.w;
    offset = offset*0.5 + vec4(0.5);

    float smpl_depth = texture(bposition, offset.xy).z;
    float range = smoothstep(0.0, 1.0, radius / abs(pos.z - smpl_depth));
    occlusion += (smpl_depth <= smpl.z ? 1.0 : 0.0)
      * range;
  }
  occlusion = 1.0 - (occlusion / KERNEL_SIZE);
  imageStore(img_result, coord, vec4(vec3(occlusion), 1));
}
