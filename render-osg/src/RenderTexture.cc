#include "OpenSceneGraph.hh"
#include "RenderTexture.hh"

// cull front faces so that only backfaces contribute to depth map
void
RenderTexture::applyDepthCullSettings()
{
	osg::StateSet* ss                               = getOrCreateStateSet();
	osg::ref_ptr<osg::CullFace> cull_face           = new osg::CullFace;
	osg::ref_ptr<osg::PolygonOffset> polygon_offset = new osg::PolygonOffset;
	
	cull_face->setMode(osg::CullFace::FRONT);
	ss->setAttribute(cull_face.get(), osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
	ss->setMode(GL_CULL_FACE, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);

    // negative polygonoffset - move the backface nearer to the eye point so that backfaces
    // shadow themselves
	float factor = -_polyOffset[0];
	float units =  -_polyOffset[1];
	
	polygon_offset->setFactor(factor);
	polygon_offset->setUnits(units);
	
	ss->setAttribute(polygon_offset.get(), osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
	ss->setMode(GL_POLYGON_OFFSET_FILL, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
}


RenderTexture::RenderTexture(size_t size_x, size_t size_y, osg::Node *scene, BufferTarget type) :
	_polyOffset(1.0,1.0)
{
	_texture = new osg::Texture2D;
	_texture->setTextureSize(size_x, size_y);
	_texture->setFilter(osg::Texture2D::MIN_FILTER,osg::Texture2D::LINEAR);
	_texture->setFilter(osg::Texture2D::MAG_FILTER,osg::Texture2D::LINEAR);
	_texture->setWrap(osg::Texture2D::WRAP_S,osg::Texture2D::CLAMP_TO_BORDER);
	_texture->setWrap(osg::Texture2D::WRAP_T,osg::Texture2D::CLAMP_TO_BORDER);
	_texture->setBorderColor(osg::Vec4(1.0f,1.0f,1.0f,1.0f));
	_texture->setDataVariance(osg::Object::DYNAMIC);
	
	
	setClearColor(osg::Vec4(1.0f,1.0f,1.0f,1.0f));
	setReferenceFrame(osg::Camera::ABSOLUTE_RF_INHERIT_VIEWPOINT);
	setComputeNearFarMode(osg::Camera::DO_NOT_COMPUTE_NEAR_FAR);
	setRenderOrder(osg::Camera::PRE_RENDER);
	setViewport(0,0,_texture->getTextureWidth(),_texture->getTextureHeight());
	setProjectionMatrix(osg::Matrixd::identity());
	setViewMatrix(osg::Matrixd::identity());
	setRenderTargetImplementation(osg::Camera::FRAME_BUFFER_OBJECT);
	
	switch (type){
	case DEPTH:
		_texture->setInternalFormat(GL_DEPTH_COMPONENT);
        _texture->setShadowComparison(true);
		_texture->setShadowTextureMode(osg::Texture2D::LUMINANCE);
		setClearMask(GL_DEPTH_BUFFER_BIT);
		attach(osg::Camera::DEPTH_BUFFER, _texture);
		applyDepthCullSettings();
		break;
		
    case COLOR:
    default:
		_texture->setInternalFormat(GL_RGBA);
		setClearMask(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT);
		attach(osg::Camera::COLOR_BUFFER, _texture);
		break;
    }

	addChild(scene);
}


		
