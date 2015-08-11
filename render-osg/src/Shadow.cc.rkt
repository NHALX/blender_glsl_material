#lang reader "../../misc/CSPL15.rkt"
| head #include "OpenSceneGraph.hh" |
| import
  "Matrix"
  "RenderTexture" |

#include "generated/scheme.h"
#include <stdexcept>

extern scheme _scheme;


  // TODO: these should be elsewhere
  #define MATRIX_FIELDS(X)                   \
    (*(X))(0,0),(*(X))(0,1),(*(X))(0,2),(*(X))(0,3), \
    (*(X))(1,0),(*(X))(1,1),(*(X))(1,2),(*(X))(1,3), \
    (*(X))(2,0),(*(X))(2,1),(*(X))(2,2),(*(X))(2,3), \
    (*(X))(3,0),(*(X))(3,1),(*(X))(3,2),(*(X))(3,3)					

  #define MATRIX_FIELDS_TRANSPOSED(X)\
    (*(X))(0,0),(*(X))(1,0),(*(X))(2,0),(*(X))(3,0), \
    (*(X))(0,1),(*(X))(1,1),(*(X))(2,1),(*(X))(3,1), \
    (*(X))(0,2),(*(X))(1,2),(*(X))(2,2),(*(X))(3,2), \
    (*(X))(0,3),(*(X))(1,3),(*(X))(2,3),(*(X))(3,3)

  const int ReceivesShadowTraversalMask = 0x1;
  const int CastsShadowTraversalMask = 0x2;


@(c:typedef "double" "degrees")
@(c:typedef "double" "radians")
@(c:typedef "std::string" "ID")

@(c:class SpotLamp
  (shadow : private "osg::ref_ptr<RenderTexture>")
  (id     : public "const char *"))

@(c:namespace++ `SpotLamp "friend class ShadowGroup;")


@constructor[SpotLamp protected
    (("const char *" name)
     ("size_t"       width)
     ("osg::Node *"  scene))
]{
    shadow = new RenderTexture(width, width, scene, RenderTexture::DEPTH);
    id     = (const char*) strdup(name);
 }


@destructor[SpotLamp protected]{
    free((void*)id);
}


@ƒ[
 SpotLamp getShadowBuffer (public "osg::Texture2D*")
]{
    return shadow->getTexture();
 }


@(define (get type result name)

    @C{
    @|type| @|result|;
    {
        char fmt_buf[1024];
        snprintf(fmt_buf, sizeof fmt_buf - 1, "%s-%s", id, "@|name|");

        if (ss_get_@|type|(_scheme, fmt_buf, &@|result|) != 0)
            std::runtime_error(@C{"error: @|name| not found."});
    }
    })


@ƒ[
 SpotLamp syncTransform (public void)
]{
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
 }




@(c:class ShadowGroup || "public osg::Node"
  (_lamps : private "std::map<ID, SpotLamp*>")
  (_scene : private "osg::ref_ptr<osg::Node>"))


@constructor[
 ShadowGroup public (("osg::ref_ptr<osg::Node>" scene))
]{
    setName("ShadowNode");
    _scene = scene;
 }

@destructor[ShadowGroup protected]{
    return;
}


@ƒ[
 ShadowGroup getScene (public "osg::ref_ptr<osg::Node>")
]{
    return _scene;
 }


@ƒ[
 ShadowGroup get (public "SpotLamp *" (ID i))
]{
    return _lamps[i];
 }



@ƒ[
  ShadowGroup reserveLamp
  (public "SpotLamp*" ("const char *" name) ("size_t" width))
]{
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
  }



@ƒ[
 ShadowGroup traverse (public virtual void (osg::NodeVisitor &nv))
]{
      @Λ[(process-lamp lamp)]{
          @|lamp|.second->syncTransform();
          @|lamp|.second->shadow->accept(nv);
      }

      @(c:for-each process-lamp
         (c:type-info "_lamps" "std::map<ID,SpotLamp*>"))
 }


