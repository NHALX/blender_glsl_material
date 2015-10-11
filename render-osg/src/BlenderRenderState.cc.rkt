#lang reader "../../CSPL15/CSPL15.rkt"
(include: "OpenSceneGraph.hh") 

(define ctx:disable
  '(
   ;; GL_COLOR_MATERIAL
   ;; GL_POINT_SPRITE_OES
   ;; GL_TEXTURE_2D
   ;; GL_TEXTURE_1D
   GL_ALPHA_TEST
   GL_BLEND
   GL_COLOR_LOGIC_OP
   GL_CULL_FACE
   GL_DEPTH_TEST
   GL_DITHER
   GL_FOG
   GL_LIGHTING 
   GL_LINE_SMOOTH
   GL_MULTISAMPLE
   GL_NORMALIZE
   GL_POINT_SMOOTH
   GL_POLYGON_OFFSET_FILL
   GL_RESCALE_NORMAL
   GL_SAMPLE_ALPHA_TO_COVERAGE
   GL_SAMPLE_ALPHA_TO_ONE
   GL_SAMPLE_COVERAGE
   GL_SCISSOR_TEST
   GL_STENCIL_TEST))

(define ctx:enable
  '(GL_BLEND
    GL_DEPTH_TEST
    GL_CULL_FACE
    GL_RESCALE_NORMAL))


(define (disable ss mode)
  { @|ss|->setMode(@|mode|, osg::StateAttribute::OFF |
                            osg::StateAttribute::OVERRIDE); })

(define (enable ss mode)
  { @|ss|->setMode(@|mode|, osg::StateAttribute::ON |
                            osg::StateAttribute::OVERRIDE); })



(ƒ: /blenderRenderState () ∷ "osg::Group*"
{
   osg::Group   * group = new osg::Group();
   osg::LightModel * lm = new osg::LightModel();
   osg::StateSet   * ss = group->getOrCreateStateSet();
    
   group->setName("BlenderRenderState");
    
   //ss->setAttribute(new osg::Depth(osg::Depth::Function(GL_LEQUAL), 0.0, 1.0));
   ss->setAttribute(new osg::ShadeModel(osg::ShadeModel::Mode(GL_FLAT)));
   ss->setAttribute(new osg::FrontFace(osg::FrontFace::Mode(GL_CCW)));
   ss->setAttribute(new osg::CullFace(osg::CullFace::Mode(GL_BACK)));
   lm->setTwoSided(GL_FALSE);
   ss->setAttribute(lm);
   ss->setMode(GL_RESCALE_NORMAL, osg::StateAttribute::ON); // TODO: do i need this?
 
   @(map (⤶ disable "ss") ctx:disable)
   @(map (⤶ enable "ss") ctx:enable)
    
   for (int i = GL_LIGHT0; i < GL_MAX_LIGHTS; ++i){
       @(disable "ss" "GL_LIGHT0 + i")
   }
    
   for (int i = GL_CLIP_PLANE0; i < GL_MAX_CLIP_PLANES; ++i){
       @(disable "ss" "GL_CLIP_PLANE0 + i")
   }

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
})


