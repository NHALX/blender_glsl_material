#lang reader "../../misc/CSPL15.rkt"
| head #include "OpenSceneGraph.hh" |
| import
  "Matrix"
  "Shadow" |

#include "generated/scheme.h"
#include <stdexcept>

extern scheme _scheme;
extern bool loadShaderSource(osg::Shader* obj, const std::string& fileName);

@(c:struct PreloadEnv (sg : public "ShadowGroup*"))

@(c:class BlenderMaterial || "public osg::Group"
          (_program : private "osg::ref_ptr<osg::Program>")
          (_vert    : private "osg::ref_ptr<osg::Shader>")
          (_frag    : private "osg::ref_ptr<osg::Shader>"))



@(c:namespace material)



static 
osg::Material* find_material(osg::Geode *geode)
{
    @(c:guard "return NULL" 
      !NULL : osg::Drawable* drawable = "geode->getDrawable(0)"
      !NULL : osg::Geometry* geom     = "drawable->asGeometry()"
      !NULL : osg::StateSet* ss       = "geom->getStateSet()"
      !NULL : osg::Material* attr     =
                           "ss->getAttribute(osg::StateAttribute::MATERIAL)")

    return attr;
}


// NOTE: this relies strictly on the layout of the input scene graph

@ƒ[
 material group_meshes ("std::multimap<std::string, osg::Node *>"
                        ("std::string"  root) 
                        ("osg::Group *" node))
]{        
    std::multimap<std::string, osg::Node *> output;

    for (int i = 0; i < node->getNumChildren(); ++i)
    {
        @(c:guard "continue"
          !NULL : osg::Group*    child    = "node->getChild(i)->asGroup()"
          !NULL : osg::Geode*    geode    = "child->getChild(0)->asGeode()"
          !NULL : osg::Material* material = "find_material(geode)")

        std::string name = material->getName();
        std::cout << name << "\n";

        output.insert(std::pair<std::string, osg::Node *>
                      (root + name, child));
    }

    return output;
}



@ƒ[
 material directory_load_all ("std::map<std::string, BlenderMaterial*>"
                              ("std::string" root))
]{
    @(c:var "osgDB::DirectoryContents" 
       files = @S{osgDB::expandWildcardsInFilename(root + "*.scm")})

    @(c:map (c:dictionary "std::string" "BlenderMaterial*" "materials")
       files
       @c:λ[(x return)]{

          std::string name = @|x|.substr(0, @|x|.length() - 4);
          osg::notify(osg::WARN) 
            << "loading material: "
            << name
            << std::endl;

          @|return| = std::pair<std::string, BlenderMaterial*>
              (name, new BlenderMaterial(name));
      })

    return materials;
}


@destructor[BlenderMaterial protected]{
     // TODO: free stuff
     return;
 }


@ƒ[
 BlenderMaterial getProgram (public "osg::ref_ptr<osg::Program>")
]{
     return _program;
 }



@constructor[
 BlenderMaterial public ((std::string filebase))
]{
    setName(filebase);

    _program = new osg::Program;
    _vert    = new osg::Shader(osg::Shader::VERTEX);
    _frag    = new osg::Shader(osg::Shader::FRAGMENT);

    if (!loadShaderSource(_vert, filebase + ".vert"))
        std::runtime_error("BlenderMaterial.LoadFile: Vertex Shader");

    if (!loadShaderSource(_frag, filebase + ".frag"))
        std::runtime_error("BlenderMaterial.LoadFile: Fragment Shader");

    ss_env_new(_scheme, filebase.c_str());
    ss_load(_scheme, (filebase + ".scm").c_str(), 
                      filebase.c_str()); // TODO: error check

    _program->addShader(_frag);
    _program->addShader(_vert);

    getOrCreateStateSet()->setAttributeAndModes(_program,
                                                osg::StateAttribute::ON);
}



@ƒ[
 BlenderMaterial traverse (public virtual void ("osg::NodeVisitor &" nv))
]{
    osgUtil::CullVisitor* cv = dynamic_cast<osgUtil::CullVisitor*>(&nv);

    if (cv) // && cv->getTraversalMask() == -1) // TODO: whats the correct mask?
        bind_uniforms(cv);

    osg::Group::traverse(nv); 
 }


@ƒ[
 BlenderMaterial preload_samplers (public void ("PreloadEnv *" sg))
]{
    ss_env env = ss_env_enter(_scheme, getName().c_str());
    ss_call1p(_scheme, "preload", sg);
    ss_env_exit(_scheme, env);
 }


@ƒ[
 BlenderMaterial bind_uniforms (public void ("osgUtil::CullVisitor *" cv))
]{
#ifdef UNIFORM_INPUT_MATRIX
    const osg::Matrix& projection = *(cv->getProjectionMatrix());
    osg::Matrix projection_inv;
    projection_inv.invert(projection);

    set_m("my_ProjectionMatrix", projection);
    set_m("my_ProjectionMatrixInverse", projection_inv);
#endif       
    ss_env env = ss_env_enter(_scheme, getName().c_str());
    ss_call1p(_scheme, "bind-samplers", this->getStateSet());
    ss_call1p(_scheme, "bind-uniforms-all", this->getStateSet());
    ss_env_exit(_scheme, env);
 }

