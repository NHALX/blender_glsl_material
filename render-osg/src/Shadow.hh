#include "RenderTexture.hh"

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

