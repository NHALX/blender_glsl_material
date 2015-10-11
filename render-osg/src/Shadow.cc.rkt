#lang reader "../../CSPL15/CSPL15.rkt"
(require "language-ext.rkt")
(include-head: "OpenSceneGraph.hh")

(import: "Matrix")
(import: "RenderTexture") 
(include: "generated/scheme.h")
(include: <stdexcept>)

(𝑣: _scheme ∷ (extern) ⇒ "scheme")


;;;;;;;;;;;;;
; TODO: these should be elsewhere

(define (matrix-fields X)
  { (*(X))(0,0),(*(X))(0,1),(*(X))(0,2),(*(X))(0,3), 
    (*(X))(1,0),(*(X))(1,1),(*(X))(1,2),(*(X))(1,3), 
    (*(X))(2,0),(*(X))(2,1),(*(X))(2,2),(*(X))(2,3), 
    (*(X))(3,0),(*(X))(3,1),(*(X))(3,2),(*(X))(3,3)	})

(define (matrix-fields-transposed X)
  { (*(X))(0,0),(*(X))(1,0),(*(X))(2,0),(*(X))(3,0), 
    (*(X))(0,1),(*(X))(1,1),(*(X))(2,1),(*(X))(3,1), 
    (*(X))(0,2),(*(X))(1,2),(*(X))(2,2),(*(X))(3,2), 
    (*(X))(0,3),(*(X))(1,3),(*(X))(2,3),(*(X))(3,3) })

;  const int ReceivesShadowTraversalMask = 0x1;
;  const int CastsShadowTraversalMask = 0x2;


(typedef: "double" "degrees")
(typedef: "double" "radians")
(typedef: "std::string" "ID")

(class: SpotLamp
; TODO: (friend: "ShadowGroup") ; friend class ShadowGroup
  (private: (𝑟: shadow ∷ "RenderTexture"))
  (public:  (𝑣: id     ∷ "const char *")))

(constructor: SpotLamp/.protected (name width scene)
  ∷ "const char *" → "size_t" → "osg::Node *" → "void"
  { shadow = new RenderTexture(width, width, scene, RenderTexture::DEPTH);
    id     = (const char*) strdup(name); })
 

(destructor: SpotLamp/.protected ∷ "void"
  { free((void*) id); })


(ƒ: SpotLamp/.public/getShadowBuffer () ∷ "osg::Texture2D *"
  { return shadow->getTexture(); })


(define (get type result name)
 {
   @|type| @|result|;
   {
        char fmt_buf[1024];
        snprintf(fmt_buf, sizeof fmt_buf - 1, "%s-%s", id, "@|name|");

        if (ss_get_@|type|(_scheme, fmt_buf, &@|result|) != 0)
            std::runtime_error(@(format "error: ~a not found." name));
    }
 })


(ƒ: SpotLamp/.public/syncTransform () ∷ "void"
{
    ss_env env = ss_env_enter(_scheme, "global");
    @(get "real" "spotsize" "spot-size")
    @(get "real" "znear" "shadow-buffer-clip-start")
    @(get "real" "zfar" "shadow-buffer-clip-end")
    @(get "m44_t" "pos_" "<world=>lamp>")
    ss_env_exit(_scheme, env);

    // TODO: mark pointer with gc during copy
    osg::Matrix pos = osg::Matrix(*pos_); 

    shadow->setViewMatrix(sync_matrix(pos));     

    if (spotsize < M_PI) // 180 degrees
    {
        double w = znear * tan(spotsize / 2);
        shadow->setProjectionMatrixAsFrustum(-w,w,-w,w,
                                             znear,
                                             zfar);
    } 
    else 
        shadow->setProjectionMatrixAsOrtho(-40, 40, -40, 40,
                                           znear,
                                           zfar); // TODO: what are the
                                                  // bounds? 40 isnt
                                                  // right
 })



(class: ShadowGroup ∈ "public osg::Node"
  (private:
   (𝑣: _lamps ∷ "std::map<ID, SpotLamp*>")
   (𝑟: _scene ∷ "osg::Node")))


(constructor: ShadowGroup/.public (scene)
  ∷ "osg::ref_ptr<osg::Node>" → "void"
  { setName("ShadowNode");
    _scene = scene; }) 

(destructor: ShadowGroup/.protected ∷ "void"
  { return; })


(ƒ: ShadowGroup/.public/getScene () ∷ "osg::ref_ptr<osg::Node>"
  { return _scene; }) 


(ƒ: ShadowGroup/.public/get (i) ∷ "ID" → "SpotLamp *"
  { return _lamps[i]; }) 


(ƒ: ShadowGroup/.public/reserveLamp (name width)
  ∷ "const char *" → "size_t" → "SpotLamp *"
  {
      std::map<ID,SpotLamp*>::iterator it;
      std::string id = std::string(name);

      it = _lamps.find(id);
      if (it != _lamps.end())
      {
          if ((*it).second->getShadowBuffer()->getTextureWidth() != width)
              printf("warning: shadow buffer size mismatch");

          return (*it).second;

      } else
      {
          printf("allocating lamp\n");

          SpotLamp *lp = new SpotLamp(name, width, _scene);
          _lamps[id] = lp; // TODO: make sure this key gets
                           // free'd eventually
          return lp;
      }
  })





(ƒ: ShadowGroup/.public/traverse (nv)
  ∷ (virtual) ⇒ "osg::NodeVisitor &" → "void"
    
  (define (process-lamp lamp)
   { @|lamp|.second->syncTransform();
     @|lamp|.second->shadow->accept(nv); })

  (for-each: process-lamp _lamps))



(module+ test
  (ƒ: /main (argc argv) ∷ "int" → "char **" → "int"
    { printf("test\n");
      return 0; }))
