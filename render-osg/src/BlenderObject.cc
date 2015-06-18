#include "OpenSceneGraph.hh"
#include "BlenderObject.hh"
#include "Traversal.hh"
#include <stdexcept>

static const matrix scheme_matrix(osg::Matrixd &m);

BlenderMaterial::BlenderMaterial(const char *filebase)
{
	#define SUFFIX_LEN     6
	#define SUFFIX_VERT    ".vert"
	#define SUFFIX_FRAG    ".frag"
	#define SUFFIX_UNIFORM ".scm\0"
	
	size_t len = SUFFIX_LEN + strlen(filebase);
	char buf[len];
	memcpy(buf, filebase, len - SUFFIX_LEN);
  
    _program = new osg::Program;
    _vert    = new osg::Shader(osg::Shader::VERTEX);
    _frag    = new osg::Shader(osg::Shader::FRAGMENT);

	memcpy(&buf[len - SUFFIX_LEN], SUFFIX_VERT, SUFFIX_LEN);
    if (!loadShaderSource(_vert, buf))
		std::runtime_error("BlenderMaterial.LoadFile: Vertex Shader");
	
	memcpy(&buf[len - SUFFIX_LEN], SUFFIX_FRAG, SUFFIX_LEN);
	if (!loadShaderSource(_frag, buf))
		std::runtime_error("BlenderMaterial.LoadFile: Fragment Shader");
	
	memcpy(&buf[len - SUFFIX_LEN], SUFFIX_UNIFORM, SUFFIX_LEN);
	scheme_load(_scheme, buf); // TODO: error check
	
    _program->addShader(_frag);
    _program->addShader(_vert);

    getOrCreateStateSet()->setAttributeAndModes(_program, osg::StateAttribute::ON);
}

void
BlenderMaterial::traverse(osg::NodeVisitor &nv)
{
    osgUtil::CullVisitor* cv = dynamic_cast<osgUtil::CullVisitor*>(&nv);
    
    if (cv && cv->getTraversalMask() == -1) // TODO: whats the correct mask?
        bind_uniforms(cv);
    
	osg::Group::traverse(nv); 
}


void
BlenderMaterial::preload_samplers(PreloadEnv *sg)
{
    //const real * const mark = vs_mark(_scheme.vs);
	scheme_call1p(_scheme, "preload", sg);
    //vs_reset_to(_scheme.vs, mark);
}


void
BlenderMaterial::bind_uniforms(osgUtil::CullVisitor* cv)
{
    osg::Matrix view;
    osg::Matrix view_inv;
    
    view.orthoNormalize(*(cv->getRenderStage()->getInitialViewMatrix()));
    view_inv.invert(view);
        
#ifdef UNIFORM_INPUT_MATRIX
	const osg::Matrix& projection = *(cv->getProjectionMatrix());
    osg::Matrix projection_inv;
	projection_inv.invert(projection);
	
    set_m("my_ProjectionMatrix", projection);
    set_m("my_ProjectionMatrixInverse", projection_inv);
#endif       

    //const real * const mark = vs_mark(_scheme.vs);
    scheme_set_ptr(_scheme, "<world=>camera>", scheme_matrix(view));
    scheme_set_ptr(_scheme, "<camera=>world>", scheme_matrix(view_inv));
    //scheme_set_ptr(_scheme, "lamp-0-<lamp=>world>", scheme_matrix(lamp_to_world));
    //scheme_set_ptr(_scheme, "lamp-0-<world=>lamp>", scheme_matrix(world_to_lamp)); 
    //scheme_eval(_scheme, "(bind-uniforms)");
	scheme_call1p(_scheme, "bind-samplers", this->getStateSet());
	scheme_call1p(_scheme, "bind-uniforms", this->getStateSet());
    // vs_reset_to(_scheme.vs, mark); 
}


///////////////////////////


BlenderObject::BlenderObject(osg::ref_ptr<BlenderMaterial> material, osg::ref_ptr<osg::Node> node)
{
	//getOrCreateStateSet();
	
	_program = material->getProgram();
	_node    = node;
	FoldGeodeGeometry fold = FoldGeodeGeometry(bind_attributes, this);
    _node->accept(fold);
}



void
BlenderObject::traverse(osg::NodeVisitor &nv)
{
    osgUtil::CullVisitor* cv = dynamic_cast<osgUtil::CullVisitor*>(&nv);
    
    //UNCOMMENT: if (cv && cv->getTraversalMask() == -1) // TODO: whats the correct mask?
        //    bind_uniforms(cv);
    /* UNCOMMENT:
#ifdef UNIFORM_INPUT_MATRIX
	osg::Matrix model_view_inv;
    osg::Matrix model_view;

    model_view.orthoNormalize(*(cv->getModelViewMatrix()));
    model_view_inv.invert(model_view);
	
    set_m("my_ModelViewMatrix", model_view);
    set_m("my_ModelViewMatrixInverse", model_view_inv);
    set_m("my_NormalMatrix", normal_matrix(model_view_inv));
#else
    osg::Matrix model_view;
	model_view.orthoNormalize(*(cv->getModelViewMatrix()));
    cv->getState()->applyModelViewMatrix(model_view);
#endif
    */
    
	_node->accept(nv); 
}


void
BlenderObject::bind_uniforms(osgUtil::CullVisitor* cv)
{    
 	
}

void *
BlenderObject::bind_attributes(void * const ctx, osg::Geode &geode, osg::Geometry* g)
{                
    BlenderObject *self = (BlenderObject*) ctx;
    geode.setNodeMask(CastsShadowTraversalMask|ReceivesShadowTraversalMask);
	bind_target target = {self->_program,self,g};
    //const real * const mark = vs_mark(_scheme.vs);
    scheme_call1p(_scheme, "bind-attributes", &target);
    //vs_reset_to(_scheme.vs, mark);
    return ctx;
}
        



static const matrix
scheme_matrix(osg::Matrixd &m)
{
    return m44(NULL, //UNCOMMENT: _scheme.vs,
               (real)m(0,0),(real)m(1,0),(real)m(2,0),(real)m(3,0),
               (real)m(0,1),(real)m(1,1),(real)m(2,1),(real)m(3,1),
               (real)m(0,2),(real)m(1,2),(real)m(2,2),(real)m(3,2),
               (real)m(0,3),(real)m(1,3),(real)m(2,3),(real)m(3,3)); 
}
