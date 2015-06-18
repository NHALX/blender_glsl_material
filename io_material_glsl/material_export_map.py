import bpy
import gpu
import os
import math
import types
from operator import add
from functools import reduce
from mathutils import Matrix, Vector, Color
from types import FunctionType
from io_material_glsl.dispatch import match
import io_material_glsl.shadelink
import io_material_glsl.dnalink
import io_material_glsl.format.internal
import io_material_glsl.format.s_expression
import io_material_glsl.test_export

def map_atr(mat, attribute, lang):
   
    mesh = bpy.data.meshes['Cube']
    
    if attribute['type'] == gpu.CD_ORCO:
        for x in mesh.vertices:
            print(x.undeformed_co)
            
    elif attribute['type'] == gpu.CD_MTFACE:
        for layer in mesh.uv_layers:
            print("layer:", layer.name)
            for x in layer.data:
                print(x.uv)
            
    elif attribute['type'] == gpu.CD_MCOL:
        pass #mesh.vertex_colors[attribute["name"]]
    
    elif attribute['type'] == gpu.CD_TANGENT:
        pass
    
    elif attribute['type'] == gpu.CD_TANGENT:
        pass
    

    print(attribute)
    xs = io_material_glsl.dnalink.convert_attribute(attribute)
    return lang.attribute(*xs)
             
                
def map_u(mat, uniform, seen, lang):

    data, name, expr = io_material_glsl.dnalink.convert_uniform(mat, uniform)
    import test_export
    test_export.test_conversion(uniform, expr)

    return (lang.default(expr,seen), lang.uniform(data, name, expr))
    



def test():
    scene = bpy.context.scene
    materials = bpy.data.materials
    mat =  materials[0]
    
    shader = gpu.export_shader(scene, mat)
    
    us1 = []
    us2 = []
    ats = []

    texvars = []
    preload = []
    bindtex = []

    
    for x in mat.texture_slots:
        if x:
            print("huh",x.name,x.texture)

    print("DYNCO!!", gpu.GPU_DYNAMIC_LAMP_DYNCO);
    
    for u in shader["uniforms"]:
        if ((u["type"] != gpu.GPU_DYNAMIC_SAMPLER_2DBUFFER) and
            (u["type"] != gpu.GPU_DYNAMIC_SAMPLER_2DIMAGE) and
            (u["type"] != gpu.GPU_DYNAMIC_SAMPLER_2DSHADOW)):
            continue
        
        identifier = "sampler-" + str(u['texnumber']) + "-texture"
        gvar       = identifier
        env        = "usr"
        
        # TODO: move this to s_expr
            
        if u["type"] == gpu.GPU_DYNAMIC_SAMPLER_2DBUFFER:
            line = "(preload-buffer \"%s\" %d %d %s)" % (identifier, u['texsize'], env)
    
        elif u["type"] == gpu.GPU_DYNAMIC_SAMPLER_2DIMAGE:
            line = "(preload-image %d %d \"%s\" %s)" % (u['image'].size[0],
                                                        u['image'].size[1],
                   "/home/nha/data/blender_glsl_material/test" + u['image'].filepath,
                                                               env)
    
        elif u["type"] == gpu.GPU_DYNAMIC_SAMPLER_2DSHADOW:
            line = "(preload-shadow-buffer \"lamp-%s\" %d %s)" % (
                io_material_glsl.format.s_expression.valid_id(u['lamp'].name),
                u['lamp'].data.shadow_buffer_size,
                env)

            
        texvars.append("(define %s #<unspecified>)" % (gvar))
        bindtex.append("(sampler \"%s\" sampler-%d-bind-index %s %s)" % (u['varname'], u['texnumber'], gvar, env))
        preload.append("(set! %s %s)" % (gvar,line))

        
    seen = set()
    for uniform in shader["uniforms"]:        
        d, u = map_u(mat, uniform, seen, io_material_glsl.format.s_expression)
        if d:
            us1 += [d]
        us2 += [u]
    
    for attribute in shader["attributes"]:
        ats += [map_atr(mat, attribute, io_material_glsl.format.s_expression)]

    #us1.sort()
    #us2.sort()
    
    #print('\n'.join(us1))
   # print('\n'.join(us2))
    #print(ats)
    header = """
#version 130
#extension GL_ARB_texture_query_lod : enable
#extension GL_EXT_gpu_shader4: enable
#extension GL_ARB_draw_instanced: enable
#define GPU_ATI
#define CLIP_WORKAROUND

#define BUMP_BICUBIC 1
"""
    file = "/home/nha/data/blender_glsl_material/render-osg/test/material"
    try:
        os.remove(file + ".vert")
        os.remove(file + ".frag")
        os.remove(file + ".scm")
        
    except:
        pass
    
    with open(file + ".vert.in", "w") as f:
#        f.write(header)
        f.write(shader["vertex"])

    with open(file + ".frag.in", "w") as f:
 #       f.write(header)
        f.write(shader["fragment"])

    with open(file + ".scm", "w") as f:
        preload_txt = "(define (preload usr)\n  (begin\n    %s))\n\n" % ('\n    '.join(preload))
        bind_txt = "(define (bind-samplers usr)\n  (begin\n    %s))\n\n" % ('\n    '.join(bindtex))
        texvars_txt = '\n'.join(texvars) + '\n\n'

        f.write(io_material_glsl.format.s_expression.group_defines(us1))
        f.write(texvars_txt)
        f.write(preload_txt)
        f.write(bind_txt)
        
        f.write(io_material_glsl.format.s_expression.group_uniforms(us2))
        f.write(io_material_glsl.format.s_expression.group_attributes(ats))
    

    
