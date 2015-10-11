#lang reader "../../CSPL15/CSPL15.rkt"
(include: "OpenSceneGraph.hh")
(import: "Matrix")
(import: "BlenderMaterial")
(include: "generated/scheme.h")
(include: "Traversal.hh")
(include: <stdexcept>)

(𝑣: _scheme ∷ "extern scheme")

(class: BlenderObject ∊ "public osg::Group"
  (public:
   (𝑣: _material ∷ "osg::ref_ptr<BlenderMaterial>")))

(struct: bind_target 
  (𝑣: program  ∷ "osg::Program *")
  (𝑣: object   ∷ "BlenderObject *")
  (𝑣: geometry ∷ "osg::Geometry *"))

(destructor: /BlenderObject/.protected ∷ "void"
  { // TODO: free other stuff
    return; })

(constructor: /BlenderObject/.public (material node)
  :: "BlenderMaterial *"
  -> "osg::Node *"
  -> "void"
  ""
  { //getOrCreateStateSet();
    setName("BlenderObject");
    _material = material;
    addChild(node);
    FoldGeodeGeometry fold = FoldGeodeGeometry(bind_attributes, this);
    node->accept(fold); })


(ƒ: /BlenderObject/.public/set_m (name m)
    :: "const char *"
    -> "osg::Matrix"
    -> "void"
  { getStateSet()->getOrCreateUniform(std::string(name), 
                                      osg::Uniform::FLOAT_MAT4)->set(m); })

(ƒ: /BlenderObject/.public/set_m (name m)
    :: "const char *"
    -> "osg::Matrix3"
    -> "void"
  { getStateSet()->getOrCreateUniform(std::string(name),
                                      osg::Uniform::FLOAT_MAT3)->set(m); })


(ƒ: /BlenderObject/.public/traverse (nv) 
  :: (virtual) => "osg::NodeVisitor &" -> "void"
  { osg::Group::traverse(nv); })

(ƒ: /BlenderObject/.public/bind_uniforms (cv)
  :: (r) => "osgUtil::CullVisitor *" -> "void"
  { return; })
    
(ƒ: /BlenderObject/.public/bind_attributes (ctx geode g)
    :: "void * const"
    -> "osg::Geode &"
    -> "osg::Geometry *"
    -> "void *"
       
 { BlenderObject *self = (BlenderObject*) ctx;
   bind_target target  = {self->_material->getProgram(), self, g};

   ss_env env = ss_env_enter(_scheme, self->_material->getName().c_str());
   ss_call1p(_scheme, "bind-attributes", &target);
   ss_env_exit(_scheme, env);
   
   return ctx; })

