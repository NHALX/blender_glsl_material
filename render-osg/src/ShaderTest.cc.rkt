#lang reader "../../misc/CSPL15.rkt"
| top-level (require "language-ext.rkt") |
| head #include "OpenSceneGraph.hh" |
| import
  "BlenderRenderState"
  "RenderState" |

//#include "generated/scheme.h"
#include "generated/linear-algebra.h"
#include <cstdarg>
#include <zmq.h>

scheme _scheme;


bool loadShaderSource(osg::Shader* obj, const std::string& fileName)
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

osg::Geode* createAxis()
{
    osg::Geode*     geode    = new osg::Geode();
    osg::Geometry*  geometry = new osg::Geometry();
    osg::Vec3Array* vertices = new osg::Vec3Array();
    osg::Vec4Array* colors   = new osg::Vec4Array();

    vertices->push_back(osg::Vec3(0.0f, 0.0f, 0.0f));
    vertices->push_back(osg::Vec3(10.0f, 0.0f, 0.0f));
    vertices->push_back(osg::Vec3(0.0f, 0.0f, 0.0f));
    vertices->push_back(osg::Vec3(0.0f, 10.0f, 0.0f));
    vertices->push_back(osg::Vec3(0.0f, 0.0f, 0.0f));
    vertices->push_back(osg::Vec3(0.0f, 0.0f, 10.0f));

    colors->push_back(osg::Vec4(1.0f, 0.0f, 0.0f, 1.0f));
    colors->push_back(osg::Vec4(1.0f, 0.0f, 0.0f, 1.0f));
    colors->push_back(osg::Vec4(0.0f, 1.0f, 0.0f, 1.0f));
    colors->push_back(osg::Vec4(0.0f, 1.0f, 0.0f, 1.0f));
    colors->push_back(osg::Vec4(0.0f, 0.0f, 1.0f, 1.0f));
    colors->push_back(osg::Vec4(0.0f, 0.0f, 1.0f, 1.0f));

    geometry->setVertexArray(vertices);
    geometry->setColorArray(colors, osg::Array::BIND_PER_VERTEX);
    geometry->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES, 0, 6));
    geometry->getOrCreateStateSet()->setMode(GL_LIGHTING, false);

    geode->addDrawable(geometry);
    geode->setName("Axis");
    return geode;
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
    pathList.push_back(
                     "/home/nha/data/blender_glsl_material/render-osg/test/");

    osgDB::setDataFilePathList(pathList);
}


class CameraManipulator : public osgGA::TrackballManipulator
{
public:
    CameraManipulator()
    {
    }

    osg::Matrixd getMatrix() const
    {
        osg::Matrixd result;
        osg::Matrixd m = osg::Matrixd::translate( 0., 0., _distance ) *
          osg::Matrixd::rotate( _rotation ) *
          osg::Matrixd::translate( _center );
        result.orthoNormalize(m);
        return result;
    }

    osg::Matrixd getInverseMatrix() const
    {     
        osg::Matrixd result;
        osg::Matrixd m = osg::Matrixd::translate( -_center ) *
          osg::Matrixd::rotate( _rotation.inverse() ) *
          osg::Matrixd::translate( 0.0, 0.0, -_distance );

        result.orthoNormalize(m);
        return result;
    }

    void zoomModel(const float dy, bool pushForwardIfNeeded)
    {
        float scale = 1.0f + dy;
        _distance *= scale;
    }

protected:
    ~CameraManipulator(){}
};


void Core::manipulator_init()
{
    //viewer.run();
    manip = new CameraManipulator();

    double fovy;
    double aspectRatio;
    double zNear;
    double zFar;
    //cam->setViewMatrix(light_m);

    manip->setHomePosition(osg::Vec3(9,16,20),
                           osg::Vec3(9,16,0),
                           osg::Vec3(0,0,-1), false);

    viewer->setCameraManipulator(manip);
}


void Core::camera_init()
{
    viewer->getCamera()->setProjectionResizePolicy(osg::Camera::FIXED);
    viewer->setLightingMode( osg::View::NO_LIGHT );

    viewer->getCamera()->setComputeNearFarMode(
                                  osg::CullSettings::DO_NOT_COMPUTE_NEAR_FAR);

    viewer->getCamera()->setClearMask( GL_COLOR_BUFFER_BIT
                                      |GL_DEPTH_BUFFER_BIT);

    osg::Matrix m = osg::Matrix(1, 0, 0, 0,
                                0, 1, 0, 0,
                                0, 0, 1, 0,
                                -9, -16, -20, 1);

    viewer->getCamera()->setViewMatrix(m);
    viewer->getCamera()->setProjectionMatrixAsPerspective(123.673,
                                                          1.5,
                                                          0.1,
                                                          1000.0);

    viewer->getCamera()->setReferenceFrame(
                                  osg::Camera::ABSOLUTE_RF_INHERIT_VIEWPOINT);
//    viewer->getCamera()->setReferenceFrame(osg::Camera::ABSOLUTE_RF);

}


//osg::ref_ptr<osg::Group>
osg::Group *
test_scene()
{
  /*
    osg::ref_ptr<osg::Group> scene   = new osg::Group;

    osg::ref_ptr<osg::Group> cessna1 = (osg::Group*) 
                                            osgDB::readNodeFile("cessna.osg");
    osg::ref_ptr<osg::Group> cessna2 = (osg::Group*) 
                                            osgDB::readNodeFile("cessna.osg");
    osg::ref_ptr<osg::MatrixTransform> positioned = new osg::MatrixTransform;

    cessna1->getChild(0)->setNodeMask(CastsShadowTraversalMask | 
                                      ReceivesShadowTraversalMask);
    cessna2->getChild(0)->setNodeMask(CastsShadowTraversalMask |
                                      ReceivesShadowTraversalMask);

    positioned->setMatrix(osg::Matrix::translate(10,10,25));
    positioned->addChild(cessna1);
    scene->addChild(positioned);
    scene->addChild(cessna2); 
    return scene;
  */
    return new osg::Group;
}



void material_update_camera(osg::Camera *camera)
{
    osg::Matrix view;
    osg::Matrix view_inv;

    // Warning: dont double transpose by accident
    view.orthoNormalize(sync_matrix(camera->getViewMatrix()));
    view_inv.orthoNormalize(sync_matrix(camera->getInverseViewMatrix()));

    // fresh allocation is required for pointer ownership reasons
    ss_env env = ss_env_enter(_scheme, "global");
    ss_set_m44_t(_scheme, "camera-<world=>camera>", new osg::Matrix(view));
    ss_set_m44_t(_scheme, "camera-<camera=>world>", 
                          new osg::Matrix(view_inv));
    ss_env_exit(_scheme, env);
}



void set_animation(osg::ref_ptr<osg::Node> obj)
{
    @Λ[(name-equal? str x result)]{
        @|result| = @|x|->getName() == "@|str|";
    }

    @Λ[(find-animation-manager state x)]{

        if (@|state|.valid())
            return;

        if (@|x|.getUpdateCallback()) 
        {
            osgAnimation::AnimationManagerBase* b = dynamic_cast
                <osgAnimation::AnimationManagerBase*>
                (@|x|.getUpdateCallback());

            if (b){
                @|state| = new osgAnimation::BasicAnimationManager(*b);
                return;
            }
        }
    }

    /////////////////

    @(c:var "osg::ref_ptr<osgAnimation::BasicAnimationManager>" 
      manager)

    manager = NULL;

    @(node:fold manager "obj" find-animation-manager)


    if (manager.valid())
    {
        obj->setUpdateCallback(manager);

        @(c:for-each 
          @c:λ[(x)]{
                  std::cout << "animation: " 
                            << @|x|->getName()
                            << std::endl;
              }

          (c:type-info "manager->getAnimationList()" 
                       "osgAnimation::AnimationList"))

        osg::ref_ptr<osgAnimation::Animation> animation;

        @(c:find "osgAnimation::AnimationList" 
          "animation"
          (curry name-equal? "Action")
          "manager->getAnimationList()")

        if (animation)
        {
            manager->playAnimation(animation);
        }

    } else 
    {
        osg::notify(osg::WARN) << 
            "no osgAnimation::AnimationManagerBase found in the subgraph,"
            "no animations available" << std::endl;
    }
}


void print_path(osg::Node &x)
{
    osg::NodePathList paths = x.getParentalNodePaths();
    osg::NodePath path = paths[0];

    @(c:for-each
        @c:λ[(p)]{
            const std::string& name = @|p|->getName();
            osg::notify(osg::WARN) 
              << "/"
              //      << static_cast<void*>(@|p|)
              //      << ":"
              << (name.empty() ? "???" : name)
              << "("
              << @|p|->className()
              << ")";
          }
        (c:type-info "path" "osg::NodePath"))
             

    osg::notify(osg::WARN) 
          << std::endl;    
}


std::string debug_node(osg::Node *node)
{
    const std::string& name = node->getName();
    std::ostringstream os;
    os
        << (name.empty() ? "???" : name)
        << ":"
        << node->className();

    return os.str();
}

void debug_tree(std::string parent, osg::Node *node)
{ 
    if (!node)
        return;

    parent += "/";
    parent += debug_node(node);
    osg::Group *g = node->asGroup();

    if (!g)
        std::cout << parent << std::endl;
      
    else
        for (int i = 0; i < g->getNumChildren(); ++i)
            debug_tree(parent, g->getChild(i));
      
}
 



@Λ[(print-key-val kv)]{
           osg::notify(osg::WARN) 
             << @|kv|.first
             << ' ' 
             << @|kv|.second 
             << std::endl;
}



void Core::mainloop()
{    
    RS env = RS("test/");

    env.material_merge_dir("test/");
    env.load_object("object:0", "export/Plane.001.fbx");


    @(c:for-each 
        @c:λ[(material)]{
            @|material|.second->preload_samplers(&env.preload_env);
            env.scene->addChild(@|material|.second);
        } 
        (c:type-info "env.materials" 
                     "std::map<std::string,BlenderMaterial*>"))


    //set_animation(BLEND_OBJ); 
      
    ///////////////////////
    osg::ref_ptr<osg::Group> blender = blenderRenderState();   
    env.scene->setName("scene");
    blender->addChild(env.scene);
    blender->addChild(env.sg);
    blender->addChild(createAxis());
    
    /*
      @(node:for-each "fbx_scene"
        @c:λ[(x)]{ print_path(@|x|); })
    */
    const char* port = "tcp://127.0.0.1:5555";

    @(define bind-failure @S{
          std::cerr 
            << "error: failed to bind: " << port
            << " -- errno: "             << errno
            << std::endl;

          return;
        })

    @(c:guard bind-failure
        !NULL : void* zmq = "zmq_ctx_new()"
        !NULL : void*  sd = "zmq_socket(zmq, ZMQ_REP)"
                        0 = "zmq_bind(sd, port)")
          

    viewer->setSceneData(blender);
    viewer->stopThreading();
    viewer->realize(); 

    while( !viewer->done() )
    {
        char buf[4096];
        const char *result;
        int n;

        if ((n = zmq_recv(sd, buf, sizeof buf, ZMQ_DONTWAIT)) != -1)
        {
            // TODO: sanitize
            result = ss_seval(_scheme, (const char*) buf, NULL);
            n      = zmq_send(sd, result, strlen(result), ZMQ_DONTWAIT);

            if (n == -1)
                std::cerr << "warning: dropped reply" << std::endl;
              
            free((void*) result);
        }

        material_update_camera(viewer->getCamera());

        viewer->advance();
        viewer->eventTraversal();
        viewer->updateTraversal();
        viewer->renderingTraversals();
    }
}  





int main( int argc, char **argv )
{
    _scheme = ss_init();
    ss_import_linear_algebra(_scheme, NULL);
    ss_import_uniform(_scheme, NULL);
    // TODO: error check
    ss_eval(_scheme, "(apply varlet (curlet) linear-algebra)", NULL);
    ss_eval(_scheme, "(apply varlet (curlet) uniform)", NULL);
    ss_load(_scheme, "../shader-link/shader-link.scm", NULL); 
    ss_eval(_scheme, "(unit-test:shader-link)", NULL);      
      
    Core *core = new Core;
    core->db_init();
    core->manipulator_init();
    core->camera_init();
    core->mainloop();
      
    ss_free(_scheme);
}


 
