#include "Shadow.hh"

extern scheme _scheme;



extern bool loadShaderSource(osg::Shader* obj, const std::string& fileName );




/*
struct transform_set
{
    osg::Matrix camera_to_world; // InverseViewMatrix
    osg::Matrix world_to_camera; // ViewMatrix
    osg::Matrix lamp_to_world;   // matrix_world
    osg::Matrix world_to_lamp;   // matrix_world^-1
    osg::Matrix obj_to_world;    // matrix_world
    osg::Matrix world_to_obj;    // matrix_world^-1
};
*/

struct PreloadEnv
{
    ShadowGroup *sg;
};
    
class BlenderMaterial : public osg::Group //, public osg::NodeCallback
{
protected:
    ~BlenderMaterial()
    {

        // TODO: free other stuff
    }
	
	osg::ref_ptr<osg::Program> _program;
	osg::ref_ptr<osg::Shader>  _vert;
	osg::ref_ptr<osg::Shader>  _frag;
	
public:
	BlenderMaterial(const char *file_name);
	
    void         bind_uniforms(osgUtil::CullVisitor* cv);
    virtual void traverse(osg::NodeVisitor &nv);
	
	osg::ref_ptr<osg::Program> getProgram()
	{
		return _program;
	}

	void preload_samplers(PreloadEnv *);
};



class BlenderObject : public osg::Node //, public osg::NodeCallback
{
protected:
    ~BlenderObject()
    {

        // TODO: free other stuff
    }
    
public:
	BlenderObject(osg::ref_ptr<BlenderMaterial>, osg::ref_ptr<osg::Node>);
	
	osg::ref_ptr<osg::Node>    _node;
	osg::ref_ptr<osg::Program> _program;
	    
    static void *bind_attributes(void * const ctx, osg::Geode &geode, osg::Geometry* g);
    void         bind_uniforms(osgUtil::CullVisitor* cv);	
    virtual void traverse(osg::NodeVisitor &nv);
    
    void set_m(const char *name, osg::Matrix m) 
    {
        getStateSet()->getOrCreateUniform(std::string(name),
                                      osg::Uniform::FLOAT_MAT4)->set(m);
    }

    void set_m(const char *name, osg::Matrix3 m) 
    {
        getStateSet()->getOrCreateUniform(std::string(name),
                                      osg::Uniform::FLOAT_MAT3)->set(m);    
    }

};


struct bind_target {
	osg::Program  *program;
	BlenderObject *object;
	osg::Geometry *geometry;
};
