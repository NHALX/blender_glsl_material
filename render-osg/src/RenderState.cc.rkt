#lang reader "../../CSPL15/CSPL15.rkt"
(include: "OpenSceneGraph.hh") 
(import: "Matrix")
(include: <stdio.h>)
(import: "BlenderRenderState")
(import: "BindUniform") 


(class: RS
  
  (public:
   (𝑣: materials   ∷ "std::map<std::string,BlenderMaterial*>")
   (𝑣: objects     ∷ "std::map<std::string,BlenderObject*>")
   (𝑣: preload_env ∷ "PreloadEnv")
   (𝑣: scene       ∷ "osg::ref_ptr<osg::Group>")
   (𝑣: sg          ∷ "osg::ref_ptr<ShadowGroup>"))
  
  (private:
   (𝑣: _matdir     ∷ "std::string")))


(constructor: RS/.public (root)
 ∷ "std::string" → "void"
 { _matdir         = root;
   scene           = new osg::Group();
   sg              = new ShadowGroup(scene);
   // TODO: kill seperate sg reference
   preload_env.sg  = sg; })


(ƒ: RS/.public/material_merge_dir (material_root)
 ∷ "std::string" → "void"
 { std::map<std::string, BlenderMaterial*> ms = 
      material::directory_load_all(material_root);

   materials.insert(ms.begin(), ms.end()); })



(ƒ: RS/.public/load_object (id file)
 ∷ "std::string" → "std::string" → "bool" 
 {                                                      
    osg::ref_ptr<osg::Node> object = osgDB::readNodeFile(file);

    if (!object)
        return false; 

    //osgDB::writeNodeFile(*fbx_scene, "mattest.osg");
    std::multimap<std::string, osg::Node *> meshes = 
        material::group_meshes(_matdir, object->asGroup());

    attach_meshes(meshes, materials);
    return true;
 })



(ƒ: attach_meshes (meshes materials)
 ∷ (static)
 => "std::multimap<std::string, osg::Node *>&"
 -> "std::map<std::string, BlenderMaterial*>&"
 -> "void"
 {
    for (std::map<std::string, BlenderMaterial*>::const_iterator
         i0  = materials.begin();
         i0 != materials.end();
         i0++)
    {
         std::pair <std::multimap<std::string, osg::Node *>::iterator,
                    std::multimap<std::string, osg::Node *>::iterator> ret;

         ret = meshes.equal_range((*i0).first);

         for (std::multimap<std::string, osg::Node *>::iterator
              i1  = ret.first;
              i1 != ret.second;
              i1++)
         {
              std::cout << "Allocating blend obj\n";
              osg::ref_ptr<BlenderObject> obj = 
                  new BlenderObject((*i0).second,
                                    (*i1).second);

              // TODO: set_animation(obj);
              (*i0).second->addChild(obj);
         }
    }
 })
