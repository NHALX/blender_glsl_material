#lang reader "../../misc/CSPL15.rkt"
| top-level (require "../../misc/c-pre-stdlib.rkt") |
| import "Matrix"
         #include <stdio.h>
         "BlenderMaterial" 
         "BlenderObject"
         "BlenderRenderState"
         "BindUniform" |

@(c:class RS (materials    : public  "std::map<std::string,BlenderMaterial*>")
             (objects      : public  "std::map<std::string,BlenderObject*>")
             (_scene       : private "osg::ref_ptr<osg::Group>")
             (_sg          : private "osg::ref_ptr<ShadowGroup>")
             (_matdir      : private "std::string")
             (_preload_env : private "PreloadEnv"))


@ƒ[
 RS RS (public "" (std::string root))
]{
    _matdir      = root;
    _scene       = new osg::Group();
    _sg          = new ShadowGroup(scene);
    _preload_env = {sg}; 
 }


@ƒ[
 RS material_merge_dir (public void)
]{
    std::map<std::string, BlenderMaterial*> ms = 
      material::directory_load_all(material_root);

    materials.insert(ms.begin(), ms.end())
 }


@ƒ[
 RS load_object (public void (std::string id) (std::string file))
]{                                                      
    osg::ref_ptr<osg::Node> object = osgDB::readNodeFile(file);

    if (!object)
        return false; 

    //osgDB::writeNodeFile(*fbx_scene, "mattest.osg");
    std::multimap<std::string, osg::Node *> meshes = 
        material::group_meshes(_matdir, object->asGroup());

    attach_meshes(meshes, materials);
    return true;
 }


@ƒ[
 attach_meshes
 (static void
   ("std::multimap<std::string, osg::Node *>&"       meshes)
   ("const std::map<std::string, BlenderMaterial*>&" materials))
]{
    for (const std::map<std::string, BlenderMaterial*>::const_iterator
         i0  = materials.begin();
         i0 != materials.end();
         i0++)
    {
         std::pair <std::multimap<std::string, osg::Node *>::iterator,
                    std::multimap<std::string, osg::Node *>::iterator> ret;

         ret = meshes.equal_range((*i0).first);

         for (std::multimap<std::string, osg::Node *>::iterator
              i1  = ret.second;
              i1 != ret.first;
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
 }


