#include "OpenSceneGraph.hh"
#include "BlenderObject.hh"
#include "Matrix.hh"
/*
#include "BlenderLamp.hh"
#include "GlobalState.hh"
#include "RenderTexture.hh"
#include "Shadow.hh"
*/


//#define trace printf
#define trace

extern "C"
{
	static int base_channel = 6; // 1 6 7    - TODO: deal with this better

	int attribute_generic(
		void *environment,
		const char *name,
		const int channel,
		const char *type,
		bind_target *target)
	{
		int i = base_channel + channel;
		target->program->addBindAttribLocation(name, i); // TODO: will this overwrite each other?

		if (strcmp(type,"vertex-texture-coordinates") == 0)
			target->geometry->setVertexAttribArray(i, target->geometry->getTexCoordArray(0));

		else if (strcmp(type, "vertex-color") == 0)
			target->geometry->setVertexAttribArray(i, target->geometry->getColorArray());
		
        // TODO: this isnt original coords? maybe it is
		else if (strcmp(type, "vertex-original-coordinates") == 0)
			target->geometry->setVertexAttribArray(i, target->geometry->getVertexArray());
    
		else if (strcmp(type, "vertex-tangent-vectors") == 0)
		{
			trace("attribute:generic:tangent-vectors: TODO: implement me\n");
			return -1;
		}
		else
			return -1;
 
		target->geometry->setVertexAttribNormalize(i, false);
		// g->setVertexAttribBinding(i, osg::Geometry::BIND_PER_VERTEX);
		return -1;   
	}
	
#define ATTRIBUTE_SF(XXX)									   \
	int attribute_##XXX(									   \
		void *x,											   \
		const char *n,										   \
		const int i,										   \
		const char *t,										   \
		const void *g)										   \
	{ return attribute_generic(x, n, i, t, (bind_target*) g); } 
	
	ATTRIBUTE_SF(2fv)
	ATTRIBUTE_SF(3fv)
	ATTRIBUTE_SF(4fv)
	ATTRIBUTE_SF(4ubv)

#define BARRIER_TYPECHECK(UT,N)                                         \
    do {                                                                \
        if (!v){                                                        \
            printf("uniform-%s: %s: warning typecheck failed.\n", UT, N); \
            return -1;                                                  \
        }                                                               \
    } while (0)
        
    
#define BARRIER_NONE(UT,N) \
    do {} while (0)
    
#define UNIFORM_SF(XXX,T,T2,VAL,BARRIER)                                \
	int uniform_##XXX(void *env, const char *n, T v, const void *obj)	\
	{																	\
        BARRIER(#XXX,n);                                                \
                                                                        \
		((osg::StateSet*)obj)->getOrCreateUniform(						\
			std::string(n),												\
			osg::Uniform::T2)->set(VAL);								\
		trace("uniform-%s: %s = %p\n", #XXX, n, v);						\
        return 0;                                                       \
	}																	

	UNIFORM_SF(1i,int,INT,v,)
	UNIFORM_SF(1fv,real,FLOAT,v,)
	UNIFORM_SF(3fv,v3_t,FLOAT_VEC3,*v, BARRIER_TYPECHECK)
	UNIFORM_SF(4fv,v4_t,FLOAT_VEC4,*v, BARRIER_TYPECHECK)
	UNIFORM_SF(Matrix4fv,m44_t,FLOAT_MAT4, *v, BARRIER_TYPECHECK)
			   //osg::Matrix(MATRIX_FIELDS_TRANSPOSED(v)))


	int sampler(
		void *ctx,
		const char *uniform_name,
		const int channel,
		const void *tex,
		const void *ss)
	{
        if (!tex || !ss)
            return -1;
        
		((osg::StateSet*)ss)->setTextureAttributeAndModes(
			channel,
			(osg::Texture2D*)tex,
			osg::StateAttribute::ON);

		((osg::StateSet*)ss)->setTextureMode(channel,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
		((osg::StateSet*)ss)->setTextureMode(channel,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
		((osg::StateSet*)ss)->setTextureMode(channel,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
		((osg::StateSet*)ss)->setTextureMode(channel,GL_TEXTURE_GEN_Q,osg::StateAttribute::ON);
	
		return 0;
	}

	
	const void * const
	preload_image(
		void *ctx,
		const int size_x,
		const int size_y,
		const char * filename,
		const void *db)
	{
		//BlenderObject *object = (BlenderObject*) obj;
		osg::Texture2D* tex   = new osg::Texture2D; // TODO: record this in blender object
		osg::Image* img       = osgDB::readImageFile(filename);
   
		// protect from being optimized away as static state:
		tex->setDataVariance(osg::Object::DYNAMIC); 

		if (!img)
		{
			trace("warning: image not found: %s.\n", filename);
			return NULL;
		}
		else
			trace("load-image: %s\n", filename);
    
		tex->setImage(img);
		tex->setWrap(osg::Texture::WRAP_S, osg::Texture::REPEAT);
		tex->setWrap(osg::Texture::WRAP_T, osg::Texture::REPEAT);
		tex->setWrap(osg::Texture::WRAP_R, osg::Texture::REPEAT);
	
		return (void*) tex;         
	}


	const void * const
	preload_buffer(
		void *ctx,
		const char *buffer_id,
		const int size_x,
		const void *db)
	{
		trace("sampler_buffer: TODO: implement me\n");
		return NULL;
	}

	const void * const
	preload_shadow_buffer(
		void *ctx,
		const char *lamp_id,
		const int width,
		const void *db) // TODO: this db shouldnt be const
	{
		SpotLamp *lamp = ((PreloadEnv*)db)->sg->reserveLamp(lamp_id, width);
		osg::Texture2D *tex = lamp->getShadowBuffer();
		return (void*)tex;        
	}

}



	/*
	int uniform_Matrix4fv(void *ctx, const char *n, matrix v, const void *obj)
	{
		BlenderObject *object = (BlenderObject*) obj;
		osg::Matrixf m = osg::Matrixf(MATRIX_FIELDS_TRANSPOSED(*v));
		object->getStateSet()->getOrCreateUniform(std::string(n),osg::Uniform::FLOAT_MAT4)->set(m);
		//printf("matrix44\n");
*

  printf("BIND: %f %f %f %f \n"
  "     %f %f %f %f \n"
  "     %f %f %f %f \n"
  "     %f %f %f %f \n",
  (real)m(0,0),(real)m(0,1),(real)m(0,2),(real)m(0,3),
  (real)m(1,0),(real)m(1,1),(real)m(1,2),(real)m(1,3),
  (real)m(2,0),(real)m(2,1),(real)m(2,2),(real)m(2,3),
  (real)m(3,0),(real)m(3,1),(real)m(3,2),(real)m(3,3));            
  printf("BIND-CMP: %f %f %f %f \n"
  "     %f %f %f %f \n"
  "     %f %f %f %f \n"
  "     %f %f %f %f \n",MATRIX_FIELDS(*v));
*
}*/



	/*
	int uniform_1i(void *ctx, const char *n, int v, const void *obj)
	{
		BlenderObject *object = (BlenderObject*) obj;
		object->getStateSet()->getOrCreateUniform(std::string(n), osg::Uniform::INT)->set(v);
		trace("uniform-1i: %s = %d\n", n, v);
	}

	int uniform_1fv(void *ctx, const char *n, real v, const void *obj)
	{
		BlenderObject *object = (BlenderObject*) obj;
		object->getStateSet()->getOrCreateUniform(std::string(n),osg::Uniform::FLOAT)->set(v);
		trace("uniform-1fv: %s = %f\n", n, v);
	}

	int uniform_3fv(void *ctx, const char *n, vector v, const void *obj)
	{
		BlenderObject *object = (BlenderObject*) obj;
		osg::Vec3 ov = osg::Vec3((*v)[0],
								 (*v)[1],
								 (*v)[2]);
    
		object->getStateSet()->getOrCreateUniform(std::string(n),osg::Uniform::FLOAT_VEC3)->set(ov);
  
		trace("uniform-3fv: %s = %f %f %f\n", n,
			  ov[0],
			  ov[1],
			  ov[2]);
	}

	int uniform_4fv(void *ctx, const char *n, vector v, const void *obj)
	{
		BlenderObject *object = (BlenderObject*) obj;
		osg::Vec4 ov = osg::Vec4((*v)[0],
								 (*v)[1],
								 (*v)[2],
								 (*v)[3]);

		object->getStateSet()->getOrCreateUniform(std::string(n),osg::Uniform::FLOAT_VEC4)->set(ov);
  
		trace("uniform-4fv: %s = %f %f %f %f\n", n,
			  ov[0],
			  ov[1],
			  ov[2],
			  ov[3]);
	}
	*/
