import bpy
 
export_root = "/home/nha/data/blender_glsl_material/render-osg/test/export/"
scene = bpy.context.scene 
bpy.ops.object.select_all(action='DESELECT')    
 
for ob in scene.objects: 
 
    if ob.type == 'MESH':
        scene.objects.active = ob
        ob.select = True
    
        #print("hi")
        # export to FBX
        pre = set(scene.objects)
        
        bpy.ops.object.duplicate(linked=False, mode='DUMMY')
        if bpy.ops.object.mode_set.poll():
            
            bpy.ops.mesh.separate(type='MATERIAL')
            if bpy.ops.object.mode_set.poll():
                new = set(scene.objects) - pre;
                for o in new:
                    o.select = True;
                    
                bpy.ops.export_scene.fbx(
                    filepath=export_root + ob.name + '.fbx', 
                    use_selection=True,
                    version='ASCII6100',
                    axis_forward='Y',
                    axis_up='Z',
                    object_types={'MESH','ARMATURE'},
                    use_anim=True,
                    use_anim_action_all=True,
                    use_anim_optimize=True,
                    use_default_take=False
                    )
                    
                for o in new:
                    bpy.ops.object.delete(use_global=False)
                    o.select = False;
                    
            else:
                new = set(scene.objects) - pre;
                for o in new:
                    o.select = True;
                    bpy.ops.object.delete(use_global=False)
                                                
        ob.select = False
