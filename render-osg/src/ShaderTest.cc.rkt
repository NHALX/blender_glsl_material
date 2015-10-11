#lang reader "../../CSPL15/CSPL15.rkt"
(require "language-ext.rkt")
(include: "OpenSceneGraph.hh") 
(import: "BlenderRenderState")
(import: "RenderState")

;;(include: "generated/scheme.h")
(include: "generated/linear-algebra.h")
(include: <cstdarg>)
(include: <zmq.h>)

(𝑣: _scheme ∷ "scheme")

(ƒ: loadShaderSource (obj fileName)
  ∷ "osg::Shader *" → "const std::string &" → "bool"
  { std::string fqFileName = osgDB::findDataFile(fileName);
    
    if (fqFileName.length() == 0)
      { std::cout << "File \""
                  << fileName
                  << "\" not found."
                  << std::endl;
        return false; }
    
    bool success = obj->loadShaderSourceFromFile( fqFileName.c_str());
    if (!success)
      { std::cout << "Couldn't load file: "
                  << fileName
                  << std::endl;
        return false; }
    else
      { return true; }})


(ƒ: createAxis () ∷ "osg::Geode *"
  { osg::Geode*     geode    = new osg::Geode();
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
    return geode; })


;; CAMERA 

(class CameraManipulator ∊ "public osgGA::TrackballManipulator")

(constructor: CameraManipulator/.public () ∷ "void"
  { return; })

(destructor: CameraManipulator/.protected ∷ "void"
  { return; })

(ƒ: CameraManipulator/.public/getMatrix () ∷ (r) => "osg::Matrixd" 
  { osg::Matrixd result;
    osg::Matrixd m = osg::Matrixd::translate(0., 0., _distance)
                   * osg::Matrixd::rotate(_rotation)
                   * osg::Matrixd::translate(_center);
    result.orthoNormalize(m);
    return result; })

(ƒ: CameraManipulator/.public/getInverseMatrix () ∷ (r) => "osg::Matrixd"
  { osg::Matrixd result;
    osg::Matrixd m = osg::Matrixd::translate(-_center)
                   * osg::Matrixd::rotate(_rotation.inverse())
                   * osg::Matrixd::translate(0.0, 0.0, -_distance);
    
    result.orthoNormalize(m);
    return result; })


(ƒ: CameraManipulator/.public/zoomModel (dy pushForwardIfNeeded)
  ∷ "const float" → "bool" → "void"
  { float scale = 1.0f + dy;
    _distance  *= scale; })


;; CORE

(class: Core
  (public:
   (𝑟: root   ∷ "osg::Group")
   (𝑟: scene  ∷ "osg::Group")
   (𝑟: viewer ∷ "osgViewer::Viewer")
   (𝑟: manip  ∷ "osgGA::TrackballManipulator")))

(constructor: Core/.public () ∷ "void"
  { //root = new osg::Group();
    viewer = new osgViewer::Viewer; })

(ƒ: Core/.public/db_init () ∷ "void"
  { osgDB::FilePathList pathList = osgDB::getDataFilePathList();
    pathList.push_back("/home/nha/data/osg/OpenSceneGraph-Data-3.0.0/");
    pathList.push_back("/home/nha/data/blender_glsl_material/render-osg/test/");
    osgDB::setDataFilePathList(pathList); })


(ƒ: Core/.public/manipulator_init () ∷ "void"
  { //viewer.run();
    manip = new CameraManipulator();

    double fovy;
    double aspectRatio;
    double zNear;
    double zFar;
    //cam->setViewMatrix(light_m);

    manip->setHomePosition(osg::Vec3(9,16,20),
                           osg::Vec3(9,16,0),
                           osg::Vec3(0,0,-1), false);

    viewer->setCameraManipulator(manip); })



(ƒ: Core/.public/camera_init () ∷ "void"
  { viewer->getCamera()->setProjectionResizePolicy(osg::Camera::FIXED);
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
    // viewer->getCamera()->setReferenceFrame(osg::Camera::ABSOLUTE_RF);
  })




(ƒ: material_update_camera (camera)
  ∷ "osg::Camera *" → "void"
  { osg::Matrix view;
    osg::Matrix view_inv;

    // Warning: dont double transpose by accident
    view.orthoNormalize(sync_matrix(camera->getViewMatrix()));
    view_inv.orthoNormalize(sync_matrix(camera->getInverseViewMatrix()));

    // fresh allocation is required for ownership reasons
    ss_env env = ss_env_enter(_scheme, "global");
    ss_set_m44_t(_scheme, "camera-<world=>camera>", new osg::Matrix(view));
    ss_set_m44_t(_scheme, "camera-<camera=>world>", 
                          new osg::Matrix(view_inv));
    ss_env_exit(_scheme, env); })



; TODO: stuff
(ƒ: set_animation (obj) ∷ "osg::ref_ptr<osg::Node>" → "void"
    
  (define (name-equal? str x result)
    { @|result| = @|x|->getName() == "@|str|"; } ) 

  (define (display-anim-name x)
    { std::cout << "animation: " 
                << @|x|->getName()
                << std::endl; })
    
  (define (find-animation-manager state x))
  {
      if (@|state|.valid())
        return;

      if (@|x|.getUpdateCallback()) 
      {
          osgAnimation::AnimationManagerBase* b =
             dynamic_cast<osgAnimation::AnimationManagerBase*>
             (@|x|.getUpdateCallback());
 
          if (b)
          {
              @|state| = new osgAnimation::BasicAnimationManager(*b);
              return;
          }
      }
  }
  
  (guard:
   (λ (x)
     { osg::notify(osg::WARN)
         << "no osgAnimation::AnimationManagerBase found in the subgraph,"
            "no animations available: "
         << @|x|
         << std::endl; })
   
   (𝑟: manager = (node:fold find-animation-manager obj)
       ?  "manager.valid()" 
       :: "osgAnimation::BasicAnimationManager")

   (𝑟: animation_list = "manager->getAnimationList()"
       != "NULL"
       :: "osgAnimation::AnimationList")

   (for-each: display-anim-name animation_list)
  
   (𝑟: animation = (find: (⤶ name-equal? "Action") animation_list)
       != "NULL"
       :: "osgAnimation::Animation")
      
   { obj->setUpdateCallback(manager);
     manager->playAnimation(animation); }))
  



(ƒ: print_path (x) ∷ "osg::Node &" → "void"
    (𝑟: paths = "x.getParentalNodePaths()" ∷ "osg::NodePathList")
    (𝑟: path  = "paths[0]"                 ∷ "osg::NodePath")

    (define (print-node p)
      { const std::string& name = @|p|->getName();
        osg::notify(osg::WARN) 
          << "/"
          //      << static_cast<void*>(@|p|)
          //      << ":"
          << (name.empty() ? "???" : name)
          << "("
          << @|p|->className()
          << ")"; })
    
    (for-each: print-node path)

    { osg::notify(osg::WARN) << std::endl; })


(ƒ: debug_node (node) ∷ "osg::Node *" → "std::string"
  { const std::string& name = node->getName();
    std::ostringstream os;
    os << (name.empty() ? "???" : name)
       << ":"
       << node->className();

    return os.str(); })


(ƒ: debug_tree * ∷ "const std::string &" → "osg::Node *" → "void"    
    [(parent "NULL") { return; }]
    [(parent node)
     { parent += "/";
       parent += debug_node(node);
       osg::Group *g = node->asGroup();

       if (!g)
         std::cout << parent << std::endl;
      
       else
         for (int i = 0; i < g->getNumChildren(); ++i)
            debug_tree(parent, g->getChild(i)); }])
 




(define (print-key-val kv)
  
  { osg::notify(osg::WARN) 
      << @|kv|.first
      << ' ' 
      << @|kv|.second 
      << std::endl; })

(ƒ: Core/.public/mainloop () ∷ "void"
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
})






(ƒ: main (argc argv) ∷ "int" → "char **"
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
})




 
