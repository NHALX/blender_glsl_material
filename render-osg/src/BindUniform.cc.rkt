#lang reader "../../CSPL15/CSPL15.rkt"
(include: "OpenSceneGraph.hh")
(import: "Matrix")
(include: "generated/scheme.h")
(import: "BlenderObject")

(𝑣: base_channel = 6 ∷ (C-link) => "int") ; 1 6 7 - TODO: deal with this better


(define (attach symbol source)
{
    if (strcmp(attribute_name, "@|symbol|") == 0)
    {
     // TODO: will this overwrite each other?
        target->program->addBindAttribLocation(name, i);
        target->geometry->setVertexAttribArray(i, target->geometry->@|source|);
        target->geometry->setVertexAttribNormalize(i, false);
        return 0;
    }
})
            
(define (attribute type xs)

  (ƒ: (format "/attribute_~a" type) (environment name channel attribute_name user_data)
    :: (C-link)
    => "void *"
    -> "string "
    -> "const integer"
    -> "string"
    -> "const void *"
    -> "integer"  
    {
        bind_target *target = (bind_target*) user_data;
        int i               = base_channel + channel;

        @(map (⤶ apply attach) xs)

        // g->setVertexAttribBinding(i, osg::Geometry::BIND_PER_VERTEX);
        return -1;
    }))



(define (uniform suffix type-in type-out src check-ptr)

  (define type-verify
  {
      if (!v)
      {
          printf("uniform-@|suffix|: %s: "
                 "warning typecheck failed.\n", n);
          return -1;
      }
  })
  
  (define create-uniform
  {
      ((osg::StateSet*)obj)->getOrCreateUniform(
              std::string(n),
              osg::Uniform::@|type-out|)->set(@|src|);
  })
      
  (ƒ: (format "/uniform_~a" suffix) (env n v obj)
    :: (C-link)
    => "void *"
    -> "string"
    -> (symbol->string type-in)
    -> "const void *"
    -> "integer"
    
    (if check-ptr type-verify "// <no typecheck>")
    
    create-uniform
    
    { trace("uniform-@|suffix|: %s = %p\n", n, v);
      return 0; }))




(for-each
 (⤶ apply attribute)
 `((2fv  ((vertex-texture-coordinates   "getTexCoordArray(0)")))
   (3fv  ((vertex-color                 "getColorArray()")
          (vertex-original-coordinates  "getVertexArray()")
          ;TODO: (vertex-tangent-vectors undefined)
          ))
   (4fv  ())
   (4ubv ())))


(for-each
 (⤶ apply uniform)
 ; gl-type     input   uniform-ty  access/conversion    check-access
 `((1i         integer INT         "(int) v"            #f)
   (1fv        real    FLOAT       "v"                  #f)
   (3fv        v3_t    FLOAT_VEC3  "*v"                 #t)
   (4fv        v4_t    FLOAT_VEC4  "*v"                 #t)
   (Matrix4fv  m44_t   FLOAT_MAT4  "sync_matrix(*v)"    #t) ))


(ƒ: /sampler (ctx uniform_name channel tex ss)
    :: "void *"
    -> "string"
    -> "const integer"
    -> "const void *"
    -> "const void *"
    -> "integer"
    
    (define (set-texture-mode obj x)
      { ((osg::StateSet*) @|obj|)->setTextureMode(
           channel, 
           |x|,
           osg::StateAttribute::ON |
           osg::StateAttribute::OVERRIDE); })
        
    {
        if (!tex || !ss)
            return -1;

        ((osg::StateSet*)ss)->setTextureAttributeAndModes(
            channel,
            (osg::Texture2D*)tex,
            osg::StateAttribute::ON);

        @(map (⤶ set-texture-mode `ss)

          `(GL_TEXTURE_GEN_S
            GL_TEXTURE_GEN_T
            GL_TEXTURE_GEN_R
            GL_TEXTURE_GEN_Q))

        return 0;
    })


(ƒ: /preload_image (ctx size_x size_y filename db)
    :: "void *"
    -> "const integer"
    -> "const integer"
    -> "string"
    -> "const void *"
    -> "const void * const"

    (define (set-wrap obj x)
      { @|obj|->setWrap(osg::Texture::@|x|, 
                        osg::Texture::REPEAT); })

    {
        // TODO: record this in blender object
        osg::Texture2D* tex   = new osg::Texture2D; 
        osg::Image* img       = osgDB::readImageFile(filename);

        // protect from being optimized away as static state:
        tex->setDataVariance(osg::Object::DYNAMIC); 

        if (!img)
        {
            trace("warning: image not found: %s.\n", filename);
            delete tex;
            return NULL;
        } 
        else
            trace("load-image: %s\n", filename);

        tex->setImage(img);

        @(map (⤶ set-wrap "tex") `(WRAP_S WRAP_T WRAP_R))

        return (void*) tex;         
    })

(ƒ: /preload_buffer (ctx buffer_id size_x db)
    :: "void *"
    -> "string"
    -> "const integer"
    -> "const void *"
    -> "const void * const"
    
    { trace("sampler_buffer: TODO: implement me\n");
      return NULL; })
      
(ƒ: /preload_shadow_buffer (ctx lamp_id width db)
    :: "void *"
    -> "string"
    -> "const integer"
    -> "const void *" ; TODO: this db shouldnt be const
    -> "const void * const"

    { SpotLamp *lamp      = ((PreloadEnv*)db)->sg->reserveLamp(lamp_id, width);
      osg::Texture2D *tex = lamp->getShadowBuffer();
      return (void*) tex; })
      

