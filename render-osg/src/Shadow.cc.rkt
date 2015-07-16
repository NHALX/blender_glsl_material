#lang at-exp s-exp "../../misc/c-pre.rkt"

(export
 (begin (import "RenderTexture"))
 @begin/text{
  //////////////////////////////////////////////////
  // ╻ ╻
  // ┣━┫
  // ╹ ╹
  ///////// auto-generated by: @|__FILE__| /////////
  @RenderTexture:header-source

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

  typedef double degrees;
  typedef double radians;

  class SpotLamp
  {
      friend class ShadowGroup;
  private:
      osg::ref_ptr<RenderTexture> shadow;

  public:
      const char *id;

      //degrees fov;
      //double znear;
      //double zfar;
      //radians spotsize;

      //osg::ref_ptr<osg::MatrixTransform> transform;


      void syncTransform();

      osg::Texture2D* getShadowBuffer()
      {
          return shadow->getTexture();
      }
      /*
      void dirty()
      {
          _dirty = true;
      }*/
  protected:
      //bool _dirty;


      SpotLamp(const char *name, size_t width, osg::Node *scene)
      {
          shadow = new RenderTexture(width, width, scene, RenderTexture::DEPTH);
          // _dirty = false;
          id     = (const char*) strdup(name);
      }

      ~SpotLamp()
      {
          free((void*)id);
      }
  };



  class ShadowGroup : public osg::Node // Group
  {    
  private:
  protected:
      ~ShadowGroup(){}
  public:

      //typedef unsigned long long ID;
      typedef std::string ID;

      std::map<ID, SpotLamp*>  _lamps;
      osg::ref_ptr<osg::Group> _scene;

      osg::ref_ptr<osg::Group> getScene()
      {
          return _scene;
      }

      SpotLamp *
      get(ID i)
      {
          return _lamps[i];
      }

      //META_Node(osg, ShadowGroup); // TODO: is this needed?
      ShadowGroup();
      virtual void traverse(osg::NodeVisitor &nv);

      SpotLamp* reserveLamp(const char *id, size_t width);
  };


 })


(source (begin
         (import "Matrix")
         (import* ".."))
 @begin/text{
  
  //////////////////////////////////////////////////
  // ┏━╸
  // ┃    
  // ┗━╸
  ///////// auto-generated by: @|__FILE__| /////////
  
  #include "OpenSceneGraph.hh"
  @..:header-source
  @Matrix:header-source
  #include "generated/scheme.h"
  #include <stdexcept>

  extern scheme _scheme;


  ShadowGroup::ShadowGroup()
  {
      _scene = new osg::Group;
      //addChild(scene);
  }

  SpotLamp*
  ShadowGroup::reserveLamp(const char *name, size_t width)
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
  }


  @(define (= type result name)
    @C{
    @|type| @|result|;
    {
        char fmt_buf[1024];
        snprintf(fmt_buf, sizeof fmt_buf - 1, "%s-%s", id, "@|name|");
        if (ss_get_@|type|(_scheme, fmt_buf, &@|result|) != 0)
            std::runtime_error(@C{"error: @|name| not found."});
    }
    })



  void
  applyLampCamera(const char *id, osg::Camera *shadow)
  {   
      @(= "real" "spotsize" "spot-size")
      @(= "real" "znear" "shadow-buffer-clip-start")
      @(= "real" "zfar" "shadow-buffer-clip-end")
      @(= "m44_t" "pos_" "<world=>lamp>")

      // TODO: mark pointer with gc during copy
      osg::Matrix pos = osg::Matrix(*pos_); 
      /*
      printf("shadow(%s): \n"
             "            %f %f %f %f\n"
             "            %f %f %f %f\n"
             "            %f %f %f %f\n"
             "            %f %f %f %f\n", id, MATRIX_FIELDS((&pos))); 
      printf("spotsize=%f,znear=%f,zfar=%f\n", spotsize, znear, zfar);
  */

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



  void
  SpotLamp::syncTransform()
  {
      applyLampCamera(id, shadow);
  }



  void
  ShadowGroup::traverse(osg::NodeVisitor &nv)
  {
      for (std::map<ID,SpotLamp*>::iterator x = _lamps.begin(); 
           x != _lamps.end();
           ++x)
      {
          //if ((*x).second->_dirty)
          //{
          //    (*x).second->_dirty = false;            
              (*x).second->syncTransform();
          //}

          (*x).second->shadow->accept(nv);
      }
      _scene->accept(nv);
  }


})