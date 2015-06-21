import gpu
from mathutils import Matrix, Vector, Color
from io_material_glsl.format.internal import Expr, Val
import io_material_glsl.shadelink as shade

# morph: GPUDynamicType -> (BlenderDNA -> ExpressionTree)
unf_to_expr = {
    
    # SAMPLER        
    gpu.GPU_DYNAMIC_SAMPLER_2DBUFFER : lambda u:
             Val(("sampler-{}-bind-index", u['texnumber']),
                 u['texnumber']),
    
    gpu.GPU_DYNAMIC_SAMPLER_2DIMAGE  : lambda u:
             Val(("sampler-{}-bind-index", u['texnumber']),
                 u['texnumber']),
    
    gpu.GPU_DYNAMIC_SAMPLER_2DSHADOW : lambda u:
             Val(("sampler-{}-bind-index", u['texnumber']),
                 u['texnumber']),

                
    # MATERIAL
    gpu.GPU_DYNAMIC_MAT_DIFFRGB : lambda m:
             Val("material-diffuse-color", m.diffuse_color),
    
    gpu.GPU_DYNAMIC_MAT_SPECRGB : lambda m:
             Val("material-specular-color", m.specular_color),
    
    gpu.GPU_DYNAMIC_MAT_EMIT    : lambda m:
        Val("material-emission", m.emit),
    
    gpu.GPU_DYNAMIC_MAT_AMB     : lambda m:
        Val("material-ambient", m.ambient),
    
    gpu.GPU_DYNAMIC_MAT_ALPHA   : lambda m:
        Val("material-alpha", m.alpha),
    
    gpu.GPU_DYNAMIC_MAT_SPEC    : lambda m:
        Val("material-specular-intensity", m.specular_intensity),
    
    gpu.GPU_DYNAMIC_MAT_HARD    : lambda m:
        Val("material-specular-hardness", float(m.specular_hardness)), #FIXME: float() HACK
    
    gpu.GPU_DYNAMIC_MAT_REF     : lambda m:
        Val("material-diffuse-intensity", m.diffuse_intensity),

    
    # OBJECT
    # see gpu_material.c:GPU_material_bind_uniforms() for these
    #gpu.GPU_DYNAMIC_NONE                 : lambda: Val("", ()),
    gpu.GPU_DYNAMIC_OBJECT_IMAT          : lambda o, cam2w, w2c:
        Val("object-<obj=>world>", o.matrix_world),
    
    gpu.GPU_DYNAMIC_OBJECT_MAT           : lambda o, cam2w, w2c:
        Val("object-<world=>obj>", o.matrix_world.inverted()),
    
    gpu.GPU_DYNAMIC_OBJECT_VIEWMAT       : lambda o, cam2w, w2c:
        Val("<world=>camera>", w2c),

    gpu.GPU_DYNAMIC_OBJECT_VIEWIMAT      : lambda o, cam2w, w2c:
        Val("<camera=>world>", cam2w),
    
    gpu.GPU_DYNAMIC_OBJECT_COLOR         : lambda o, cam2w, w2c:
        #HACK: convert o.color: bpy_prop_array -> tuple
        Val("object-color", tuple(o.color)),
    
    gpu.GPU_DYNAMIC_OBJECT_AUTOBUMPSCALE : lambda o, cam2w, w2c:
        Val("object-bump-scale", 0.12345678), # FIXME: find this!

    
    # WORLD
    gpu.GPU_DYNAMIC_HORIZON_COLOR  : lambda w:
        Expr(shade.v4_v3,
             Val("world-horizon-color", w.horizon_color)),
    
    gpu.GPU_DYNAMIC_AMBIENT_COLOR  : lambda w:
        Expr(shade.v4_v3,
             Val("world-ambient-color", w.ambient_color)),

    
    # MIST
    gpu.GPU_DYNAMIC_MIST_START     : lambda mt:
        Val("mist-start", mt.start),
    
    gpu.GPU_DYNAMIC_MIST_DISTANCE  : lambda mt:
        Val("mist-depth", mt.depth),
    
    gpu.GPU_DYNAMIC_MIST_INTENSITY : lambda mt:
        Val("mist-min-intensity", mt.intensity),
    
    gpu.GPU_DYNAMIC_MIST_COLOR     : lambda mt:
        Val("mist-color", mt.color),
    
    gpu.GPU_DYNAMIC_MIST_ENABLE    : lambda mt:
        Expr(shade.bool_to_float,
             Val("mist-enable", mt.use_mist)),
    
    gpu.GPU_DYNAMIC_MIST_TYPE      : lambda mt:
        Expr(shade.mist_type,
             Val("mist-falloff-type", mt.falloff)),

    
    # LAMP
    gpu.GPU_DYNAMIC_LAMP_DISTANCE   : lambda l, cam2w, w2c:
        Val(("lamp-{}-falloff-distance", l.name),
            l.data.distance),
    
    gpu.GPU_DYNAMIC_LAMP_DYNENERGY  : lambda l, cam2w, w2c:
        Expr(shade.lamp_dyn_energy,
             Val(("lamp-{}-negative", l.name), l.data.use_negative),
             Val(("lamp-{}-energy", l.name), l.data.energy)),
        
    gpu.GPU_DYNAMIC_LAMP_ATT1       : lambda l, cam2w, w2c:
        Val(("lamp-{}-linear-attenuation", l.name),
            l.data.linear_attenuation),
    
    gpu.GPU_DYNAMIC_LAMP_ATT2       : lambda l, cam2w, w2c:
        Val(("lamp-{}-quadratic-attenuation", l.name),
            l.data.quadratic_attenuation),

    gpu.GPU_DYNAMIC_LAMP_DYNCOL     : lambda l, cam2w, w2c:
        Val(("lamp-{}-color", l.name), l.data.color),
    
    gpu.GPU_DYNAMIC_LAMP_SPOTBLEND  : lambda l, cam2w, w2c:
        Expr(shade.lamp_spotblend,
             Val(("lamp-{}-spot-size", l.name), l.data.spot_size),
             Val(("lamp-{}-spot-blend", l.name), l.data.spot_blend)),

    gpu.GPU_DYNAMIC_LAMP_SPOTSIZE   : lambda l, cam2w, w2c:
        Expr(shade.lamp_spotsize,
             Val(("lamp-{}-spot-size", l.name), l.data.spot_size)),

    gpu.GPU_DYNAMIC_LAMP_DYNPERSMAT : lambda l, cam2w, w2c:
        Expr(shade.lamp_perspective_matrix,
             Val(("lamp-{}-spot-size", l.name),
                 l.data.spot_size),
             Val(("lamp-{}-shadow-buffer-clip-start", l.name),
                 l.data.shadow_buffer_clip_start),
             Val(("lamp-{}-shadow-buffer-clip-end", l.name),
                 l.data.shadow_buffer_clip_end),
             Val(("lamp-{}-<world=>lamp>", l.name),
                 l.matrix_world.inverted()),
             Val("<camera=>world>",
                 cam2w)),
    
    gpu.GPU_DYNAMIC_LAMP_DYNIMAT    : lambda l, cam2w, w2c:
        Expr(shade.lamp_imat,
             Val(("lamp-{}-<world=>lamp>", l.name),
                 l.matrix_world.inverted()),
             Val("<camera=>world>",
                 cam2w)),

    gpu.GPU_DYNAMIC_LAMP_DYNVEC     : lambda l, cam2w, w2c:
        Expr(shade.lamp_dynvec,
             Val(("lamp-{}-<lamp=>world>", l.name),
                 l.matrix_world),
             Val("<world=>camera>",
                 w2c)),

    gpu.GPU_DYNAMIC_LAMP_DYNCO      : lambda l, cam2w, w2c:
        Expr(shade.lamp_dynco,
             Val(("lamp-{}-<lamp=>world>", l.name),
                 l.matrix_world),
             Val("<world=>camera>",
                 w2c)),

}


glt_to_str = { gpu.GPU_DATA_1I  : "1i",
               gpu.GPU_DATA_1F  : "1fv",
               gpu.GPU_DATA_2F  : "2fv",
               gpu.GPU_DATA_3F  : "3fv",
               gpu.GPU_DATA_4F  : "4fv",
               gpu.GPU_DATA_4UB : "4ubv",
               gpu.GPU_DATA_9F  : "Matrix3fv",
               gpu.GPU_DATA_16F : "Matrix4fv" }


def convert_uniform(mat, uniform):
    import bpy
    cam_to_world  = Matrix(bpy.context.scene.camera.matrix_world)
    world_to_cam  = cam_to_world.inverted()
    target_obj    = bpy.data.objects['Cube']
    
    # FIXME: add to gpu.c
    uniform["type-group"] = uniform["type"] & 0xFFFF0000
    # FIXME: use horizon color for mist?
    class MistSettings(bpy.types.WorldMistSettings): pass
    
    mist         = MistSettings(bpy.context.scene.world.mist_settings)
    mist.color   = bpy.context.scene.world.horizon_color
    group_to_env = {
        gpu.GPU_DYNAMIC_GROUP_MISC    : lambda: [],
        gpu.GPU_DYNAMIC_GROUP_SAMPLER : lambda: [uniform],
        gpu.GPU_DYNAMIC_GROUP_MIST    : lambda: [mist],
        gpu.GPU_DYNAMIC_GROUP_WORLD   : lambda: [bpy.context.scene.world],
        gpu.GPU_DYNAMIC_GROUP_MAT     : lambda: [mat],
        gpu.GPU_DYNAMIC_GROUP_LAMP    : lambda: [uniform["lamp"],cam_to_world,world_to_cam],
        gpu.GPU_DYNAMIC_GROUP_OBJECT  : lambda: [target_obj,cam_to_world,world_to_cam],
    }

    to_expr     = unf_to_expr[uniform["type"]]
    environment = group_to_env[uniform["type-group"]]()

    return (glt_to_str[uniform["datatype"]], uniform["varname"], to_expr(*environment))



attr_to_name = {
    gpu.CD_MTFACE  : "vertex_texture_coordinates",
    gpu.CD_MCOL    : "vertex_color",
    gpu.CD_ORCO    : "vertex_original_coordinates",
    gpu.CD_TANGENT : "vertex_tangent_vectors"
    }
    
def convert_attribute(attr):
    return (glt_to_str[attr['datatype']],
            attr['varname'],
            attr['number'],
            attr_to_name[attr['type']])
