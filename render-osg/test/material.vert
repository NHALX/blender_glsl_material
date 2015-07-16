#version 130
#extension GL_ARB_texture_query_lod : enable
#extension GL_EXT_gpu_shader4: enable
#extension GL_ARB_draw_instanced: enable
//#define GPU_ATI
//#define CLIP_WORKAROUND

#define BUMP_BICUBIC 1

uniform mat4 my_ModelViewMatrix;
uniform mat4 my_ModelViewMatrixInverse;
uniform mat4 my_ProjectionMatrix;
uniform mat4 my_ProjectionMatrixInverse;
uniform mat3 my_NormalMatrix;
attribute vec2 att0;
varying vec2 var0;


varying vec3 varposition;
varying vec3 varnormal;

#ifdef CLIP_WORKAROUND
varying float gl_ClipDistance[6];
#endif

void main()
{
	vec4 co = gl_ModelViewMatrix * gl_Vertex;

	varposition = co.xyz;
	varnormal = normalize(gl_NormalMatrix * gl_Normal);
	gl_Position = gl_ProjectionMatrix * co;

#ifdef CLIP_WORKAROUND
	int i;
	for(i = 0; i < 6; i++)
		gl_ClipDistance[i] = dot(co, gl_ClipPlane[i]);
#elif !defined(GPU_ATI)
	// Setting gl_ClipVertex is necessary to get glClipPlane working on NVIDIA
	// graphic cards, while on ATI it can cause a software fallback.
	gl_ClipVertex = co; 
#endif 

	var0 = att0;
}

