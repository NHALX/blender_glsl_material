#include "OpenSceneGraph.hh"

#include "BlenderObject.hh"
#include "BlenderRenderState.hh"
#include <cstdarg>



bool loadShaderSource(osg::Shader* obj, const std::string& fileName )
{
    std::string fqFileName = osgDB::findDataFile(fileName);
    if( fqFileName.length() == 0 )
    {
        std::cout << "File \"" << fileName << "\" not found." << std::endl;
        return false;
    }
    bool success = obj->loadShaderSourceFromFile( fqFileName.c_str());
    if ( !success  )
    {
        std::cout << "Couldn't load file: " << fileName << std::endl;
        return false;
    }
    else
    {
        return true;
    }
}


class Core {
public:
    Core(){
        //root   = new osg::Group();
        viewer = new osgViewer::Viewer;
    }
    /*
    osg::ref_ptr<osg::Group>                  root;
    osg::ref_ptr<osg::Group>                  scene;*/
    osg::ref_ptr<osgViewer::Viewer>           viewer;
    osg::ref_ptr<osgGA::TrackballManipulator> manip;
    
    
    void db_init();
    void manipulator_init();
    void camera_init();
    void mainloop();
    
    osg::Matrixd perspective,view_matrix,view_matrix_inv,perspective_inv;
    osg::Matrix3 normal_matrix;
};

void Core::db_init()
{
    osgDB::FilePathList pathList = osgDB::getDataFilePathList();
    pathList.push_back("/home/nha/data/osg/OpenSceneGraph-Data-3.0.0/");
    pathList.push_back("/home/nha/data/blender_glsl_material/render-osg/test/");
    osgDB::setDataFilePathList(pathList);
}



void Core::manipulator_init()
{
    //viewer.run();
    manip = new osgGA::TrackballManipulator();

    double fovy;
    double aspectRatio;
    double zNear;
    double zFar;
    //cam->setViewMatrix(light_m);

    manip->setHomePosition(osg::Vec3(9,16,20), osg::Vec3(9,16,0), osg::Vec3(0,0,-1), false);
    viewer->setCameraManipulator(manip);
}

void Core::camera_init()
{
    viewer->getCamera()->setProjectionResizePolicy(osg::Camera::FIXED);
    viewer->setLightingMode( osg::View::NO_LIGHT );

    viewer->getCamera()->setComputeNearFarMode(osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR); 
    viewer->getCamera()->setClearMask(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        osg::Matrix m = osg::Matrix(1, 0, 0, 0,
                                    0, 1, 0, 0,
                                    0, 0, 1, 0,
                                    -9, -16, -20, 1);
    viewer->getCamera()->setViewMatrix(m);
    viewer->getCamera()->setProjectionMatrixAsPerspective(123.673,1.5,0.1,1000.0);
    viewer->getCamera()->setReferenceFrame(osg::Camera::ABSOLUTE_RF_INHERIT_VIEWPOINT);
//    viewer->getCamera()->setReferenceFrame(osg::Camera::ABSOLUTE_RF);
    
    
}


//osg::ref_ptr<osg::Group>
osg::Group *
test_scene()
{
    
        /*
    osg::ref_ptr<osg::Group> scene   = new osg::Group;

    osg::ref_ptr<osg::Group> cessna1 = (osg::Group*) osgDB::readNodeFile("cessna.osg");
    osg::ref_ptr<osg::Group> cessna2 = (osg::Group*) osgDB::readNodeFile("cessna.osg");
    osg::ref_ptr<osg::MatrixTransform> positioned = new osg::MatrixTransform;
    
    cessna1->getChild(0)->setNodeMask(CastsShadowTraversalMask|ReceivesShadowTraversalMask);
    cessna2->getChild(0)->setNodeMask(CastsShadowTraversalMask|ReceivesShadowTraversalMask);
    
    positioned->setMatrix(osg::Matrix::translate(10,10,25));
    positioned->addChild(cessna1);
    scene->addChild(positioned);
    scene->addChild(cessna2); 
    return scene;
    */
    return new osg::Group;
    
}

/*
void whatever(osg::ref_ptr<ShadowGroup> sg,
              osg::ref_ptr<BlenderMaterial> obj,
              int index,
              int shadowUnit)
{
    obj->getStateSet()->setTextureAttributeAndModes(
        shadowUnit,
        sg->get(index)->getTexture(),
        osg::StateAttribute::ON |
        osg::StateAttribute::OVERRIDE);
    
	obj->getStateSet()->setTextureMode(shadowUnit,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
	obj->getStateSet()->setTextureMode(shadowUnit,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
	obj->getStateSet()->setTextureMode(shadowUnit,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
	obj->getStateSet()->setTextureMode(shadowUnit,GL_TEXTURE_GEN_Q,osg::StateAttribute::ON);
}
*/


osg::ref_ptr<osg::Group>
compose3(osg::ref_ptr<osg::Group> a,
         osg::ref_ptr<osg::Group> b,
         osg::ref_ptr<osg::Group> c)
{
    a->addChild(b);
    b->addChild(c);
    return a;
}

osg::ref_ptr<osg::Group>
compose2(osg::ref_ptr<osg::Group> a,
         osg::ref_ptr<osg::Group> b)
{
    a->addChild(b);
    return a;
}


void Core::mainloop()
{    
//    osg::ref_ptr<osg::Group> scene = test_scene();
//    printf("ref:%d\n",scene->referenceCount());
    //abort();
    
    osg::ref_ptr<BlenderMaterial> material = new BlenderMaterial("test/material");
    osg::ref_ptr<BlenderObject> BLEND_OBJ = new BlenderObject(material, osgDB::readNodeFile("mattest.fbx"));


    osg::ref_ptr<osg::Group> blender = blenderRenderState();   
	osg::ref_ptr<ShadowGroup> sg     = new ShadowGroup();
    PreloadEnv p_env = {sg};
    
    material->preload_samplers(&p_env);
    /*
    osg::ref_ptr<SpotLamp> lamp0 = sg->get("lamp-0");
    osg::ref_ptr<SpotLamp> lamp1 = sg->get("lamp-1");
    */

    
    //  whatever(sg, material, 0x1234, 2);
    //whatever(sg, material, 0x1235, 1);
    
    material->addChild(BLEND_OBJ);
    sg->getScene()->addChild(material);
    viewer->setSceneData(sg);
    viewer->stopThreading();
    
    
    // root->setCullingActive(false);  // TODO: remove this
    
    //viewer->setSceneData(scene); //compose3(sg,blender,scene));//compose3(sg, blender, scene));

    viewer->realize();

    while( !viewer->done() )
    {
        // obj->getStateSet()->setTextureAttributeAndModes(1, sm->getTexture(), osg::StateAttribute::ON); // TODO: disable this?
        viewer->advance();
        viewer->eventTraversal();
        viewer->updateTraversal();
        viewer->renderingTraversals();
    }

}


scheme _scheme;

int main( int argc, char **argv )
{
    _scheme = scheme_init(NULL);
    scheme_eval(_scheme, "(apply varlet (curlet) linear-algebra)");
    scheme_eval(_scheme, "(apply varlet (curlet) uniform)");
    scheme_load(_scheme, "test3.rkt"); // TODO: error check
    
    Core *core = new Core;
    core->db_init();
    core->manipulator_init();
    core->camera_init();
    core->mainloop();

    scheme_free(_scheme);
}
        
