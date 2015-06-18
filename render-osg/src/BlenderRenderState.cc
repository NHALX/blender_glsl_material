#include "OpenSceneGraph.hh"

osg::Group*
blenderRenderState()
{
    osg::Group *group = new osg::Group();
    osg::StateSet *ss = group->getOrCreateStateSet();
    //ss->setAttribute(new osg::Depth(osg::Depth::Function(GL_LEQUAL), 0.0, 1.0));
    ss->setAttribute(new osg::ShadeModel(osg::ShadeModel::Mode(GL_FLAT)));
    ss->setAttribute(new osg::FrontFace(osg::FrontFace::Mode(GL_CW)));
    ss->setAttribute(new osg::CullFace(osg::CullFace::Mode(GL_BACK)));
    
    osg::LightModel *lm = new osg::LightModel();
    lm->setTwoSided(GL_FALSE);
    ss->setAttribute(lm);
 
    // ss->setMode(GL_RESCALE_NORMAL, osg::StateAttribute::ON); // TODO: do i need this?
    
#define Disable(mode)                                                   \
    ss->setMode(mode, osg::StateAttribute::OFF|osg::StateAttribute::OVERRIDE);
#define Enable(mode)                                                    \
    ss->setMode(mode, osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
        
    Disable(GL_ALPHA_TEST);
    Disable(GL_BLEND);
    Disable(GL_COLOR_LOGIC_OP);
    
    for (int i = GL_CLIP_PLANE0; i < GL_MAX_CLIP_PLANES; ++i)
        Disable(GL_CLIP_PLANE0 + i);
    
    //Disable(GL_COLOR_MATERIAL);
    Disable(GL_CULL_FACE);
    Disable(GL_DEPTH_TEST);
    Disable(GL_DITHER);
    Disable(GL_FOG);

    for (int i = GL_LIGHT0; i < GL_MAX_LIGHTS; ++i)
        Disable(GL_LIGHT0 + i);
    
    Disable(GL_LIGHTING );
    Disable(GL_LINE_SMOOTH);
    Disable(GL_MULTISAMPLE);
    Disable(GL_NORMALIZE);
    Disable(GL_POINT_SMOOTH);
    // Disable(GL_POINT_SPRITE_OES);
    Disable(GL_POLYGON_OFFSET_FILL);
    Disable(GL_RESCALE_NORMAL);
    Disable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    Disable(GL_SAMPLE_ALPHA_TO_ONE);
    Disable(GL_SAMPLE_COVERAGE);
    Disable(GL_SCISSOR_TEST);
    Disable(GL_STENCIL_TEST);
    // Disable(GL_TEXTURE_2D);
    // Disable(GL_TEXTURE_1D);
    
    Enable(GL_BLEND);
    Enable(GL_DEPTH_TEST);
    Enable(GL_CULL_FACE);
    //Enable(GL_RESCALE_NORMAL);
/*
  TODO:

  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, mat_ambient);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, mat_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular);
  glMateriali(GL_FRONT_AND_BACK, GL_SHININESS, 35);

  GPU_default_lights();

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);

  glPixelTransferi(GL_MAP_COLOR, GL_FALSE);
  glPixelTransferi(GL_RED_SCALE, 1);
  glPixelTransferi(GL_RED_BIAS, 0);
  glPixelTransferi(GL_GREEN_SCALE, 1);
  glPixelTransferi(GL_GREEN_BIAS, 0);
  glPixelTransferi(GL_BLUE_SCALE, 1);
  glPixelTransferi(GL_BLUE_BIAS, 0);
  glPixelTransferi(GL_ALPHA_SCALE, 1);
  glPixelTransferi(GL_ALPHA_BIAS, 0);
	
  glPixelTransferi(GL_DEPTH_BIAS, 0);
  glPixelTransferi(GL_DEPTH_SCALE, 1);

  glPolygonStipple(patc);

  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glMatrixMode(GL_MODELVIEW);

  gpu_multisample(false);
*/
    return group;
}
