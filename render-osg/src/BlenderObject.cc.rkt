#lang reader "../../misc/CSPL15.rkt"
| head #include "OpenSceneGraph.hh" |
| import
  "Matrix"
  "BlenderMaterial" |

#include "generated/scheme.h"
#include "Traversal.hh"
#include <stdexcept>

extern scheme _scheme;


@(c:class BlenderObject || "public osg::Group"
  (_material : public "osg::ref_ptr<BlenderMaterial>"))

@(c:struct bind_target 
  (program  : public "osg::Program *")
  (object   : public "BlenderObject *")
  (geometry : public "osg::Geometry *"))

  
@destructor[BlenderObject protected]{
  // TODO: free other stuff
  return;
}

@constructor[BlenderObject public 
  (("BlenderMaterial *" material)
   ("osg::Node *"       node))
]{
    //getOrCreateStateSet();
    setName("BlenderObject");
    _material = material;
    addChild(node);
    FoldGeodeGeometry fold = FoldGeodeGeometry(bind_attributes, this);
    node->accept(fold);
 }

    
@ƒ[
 BlenderObject set_m (public "void" ("const char *" name) 
                                    ("osg::Matrix"  m))
]{
    getStateSet()->getOrCreateUniform(std::string(name), 
                                      osg::Uniform::FLOAT_MAT4)->set(m);
 }


@ƒ[
 BlenderObject set_m (public "void" ("const char *" name) 
                                    ("osg::Matrix3" m))
]{          
    getStateSet()->getOrCreateUniform(std::string(name),
                                      osg::Uniform::FLOAT_MAT3)->set(m);
 }


@ƒ[
 BlenderObject traverse (public virtual "void" ("osg::NodeVisitor &" nv))
]{
    osg::Group::traverse(nv);
 }


@ƒ[
 BlenderObject bind_uniforms (public "void" ("osgUtil::CullVisitor*" cv))
]{    
    return;
 }


@ƒ[
  BlenderObject bind_attributes (public static "void *" 
   ("void * const"    ctx)
   ("osg::Geode &"    geode)
   ("osg::Geometry *" g))
]{                
    BlenderObject *self = (BlenderObject*) ctx;
    //geode.setNodeMask(CastsShadowTraversalMask|ReceivesShadowTraversalMask);
    bind_target target = {self->_material->getProgram(), self, g};

    ss_env env = ss_env_enter(_scheme, self->_material->getName().c_str());
    ss_call1p(_scheme, "bind-attributes", &target);
    ss_env_exit(_scheme, env);

    return ctx;
 }

