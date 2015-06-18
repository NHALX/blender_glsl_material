#include "OpenSceneGraph.hh"
#include "Shadow.hh"
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
    
scheme_result
get(const char *id, const char *prop)
{
    /*
      va_list args;
      va_start(args, fmt);
      vsnprintf(fmt_buf, sizeof fmt_buf - 1, fmt, args);
      va_end(args);
    */
    char fmt_buf[1024];
    snprintf(fmt_buf, sizeof fmt_buf - 1, "%s-%s", id, prop);
    return scheme_get(_scheme, fmt_buf);
}

//double get(const char *fmt, ...)
double get_d(const char *id, const char *prop)
{
    scheme_result ret = get(id, prop);
    
    if (ret.type != SCHEME_REAL)
        std::runtime_error("scheme_get: unexpected return type.");
    
    return ret.real;
}

void* get_p(const char *id, const char *prop)
{
    scheme_result ret = get(id, prop);
    
    if (ret.type != SCHEME_POINTER)
        std::runtime_error("scheme_get: unexpected return type.");
    
    return ret.pointer;
}



void
SpotLamp::syncTransform()
{   
    double spotsize = get_d(id, "spot-size");
    double znear    = get_d(id, "shadow-buffer-clip-start");
    double zfar     = get_d(id, "shadow-buffer-clip-end");
    matrix pos      = (matrix) get_p(id, "<world=>lamp>");
    
    shadow->setViewMatrix(osg::Matrix(MATRIX_FIELDS_TRANSPOSED(pos)));
                                   
    //printf("spotsize=%f,znear=%f,zfar=%f\n", spotsize, znear, zfar);
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
