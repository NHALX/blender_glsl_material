#lang reader "../../misc/CSPL15.rkt"
| head #include "OpenSceneGraph.hh" |
| import
  "Matrix"
  #include "generated/scheme.h"
  "BlenderObject" |


  //#define trace printf
  #define trace

  @(define (attribute type xs)
    (define (attach symbol source)
      @C{

        if (strcmp(attribute_name, "@|symbol|") == 0){
            // TODO: will this overwrite each other?
            target->program->addBindAttribLocation(name, i);
            target->geometry->setVertexAttribArray(i,
                                                   target->geometry->@|source|);
            target->geometry->setVertexAttribNormalize(i, false);
            return 0;
        }

      })

    @C{

    integer
    attribute_@|type|(
        void         *environment,
        string        name,
        const integer channel,
        string        attribute_name,
        const void   *user_data)
    {
        bind_target *target = (bind_target*) user_data;
        int i = base_channel + channel;

        @(map (⤶ apply attach) xs)

        // g->setVertexAttribBinding(i, osg::Geometry::BIND_PER_VERTEX);
        return -1;
    }

    })


  @(define (uniform suffix type-in type-out src check-ptr)
    @C{

      integer uniform_@|suffix|(
          void *env,
          string n,
          @|type-in| v,
          const void *obj)
      {
          @(if check-ptr @C{

              if (!v){
                  printf("uniform-@|suffix|: %s: "
                         "warning typecheck failed.\n", n);
                  return -1;
              }

            }
            "// <no typecheck>")

          ((osg::StateSet*)obj)->getOrCreateUniform(
              std::string(n),
              osg::Uniform::@|type-out|)->set(@|src|);

          trace("uniform-@|suffix|: %s = %p\n", n, v);
          return 0;
      }

    })



  extern "C"
  {
      static int base_channel = 6; // 1 6 7    - TODO: deal with this better

      @(map (⤶ apply attribute)
        `((2fv  ((vertex-texture-coordinates   "getTexCoordArray(0)")))
          (3fv  ((vertex-color                 "getColorArray()")
                 (vertex-original-coordinates  "getVertexArray()")
                 ;TODO: (vertex-tangent-vectors undefined)
                 ))
          (4fv  ())
          (4ubv ())))

      @(map (⤶ apply uniform)
        `((1i         integer INT         "(int) v"            #f)
          (1fv        real    FLOAT       "v"                  #f)
          (3fv        v3_t    FLOAT_VEC3  "*v"                 #t)
          (4fv        v4_t    FLOAT_VEC4  "*v"                 #t)
          (Matrix4fv  m44_t   FLOAT_MAT4  "sync_matrix(*v)"    #t) ))

      integer sampler(
          void *        ctx,
          string        uniform_name,
          const integer channel,
          const void *  tex,
          const void *  ss)
      {
          if (!tex || !ss)
              return -1;

          ((osg::StateSet*)ss)->setTextureAttributeAndModes(
              channel,
              (osg::Texture2D*)tex,
              osg::StateAttribute::ON);

          @(map (⤶ format 
            "((osg::StateSet*)ss)->setTextureMode(channel, 
                                             ~a,
                                             osg::StateAttribute::ON |
                                             osg::StateAttribute::OVERRIDE);\n")
            `(GL_TEXTURE_GEN_S
              GL_TEXTURE_GEN_T
              GL_TEXTURE_GEN_R
              GL_TEXTURE_GEN_Q))

          return 0;
      }


      const void * const
      preload_image(
          void *         ctx,
          const integer  size_x,
          const integer  size_y,
          string         filename,
          const void *   db)
      {
          //BlenderObject *object = (BlenderObject*) obj;
          // TODO: record this in blender object
          osg::Texture2D* tex   = new osg::Texture2D; 
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
          void *         ctx,
          string         buffer_id,
          const integer  size_x,
          const void *   db)
      {
          trace("sampler_buffer: TODO: implement me\n");
          return NULL;
      }

      const void * const
      preload_shadow_buffer(
          void *         ctx,
          string         lamp_id,
          const integer  width,
          const void *   db) // TODO: this db shouldnt be const
      {
          SpotLamp *lamp = ((PreloadEnv*)db)->sg->reserveLamp(lamp_id, width);
          osg::Texture2D *tex = lamp->getShadowBuffer();
          return (void*)tex;        
      }

  }

