from __future__ import division # division (/) defaulting to int breaks things
import math

if __name__ == '__main__':
    from io_material_glsl.test_linalg import Matrix
else:
    from mathutils import Matrix

Degree = math.pi/180


def frustum(left, right, bottom, top, zNear, zFar):

    A = (right + left) / float(right - left)
    B = (top + bottom) / (top - bottom)
    C = - (zFar + zNear) / (zFar - zNear)
    D = - (2*zFar*zNear) / (zFar - zNear)
    u = (2*zNear) / (right-left)
    v = (2*zNear) / (top - bottom)

    return Matrix([[u,0,A,0],
                   [0,v,B,0],
                   [0,0,C,D],
                   [0,0,-1,0]])

##### value conversion from native blender settings to those expected by the GLSL uniforms
# from rna_world.c:394,
# FIXME: hard coded values...
MIST_TYPE_QUADRATIC         = 0
MIST_TYPE_LINEAR            = 1
MIST_TYPE_INVERSE_QUADRATIC = 2

def vnegate(v):
    u = v.copy()
    u.negate()
    return u

def bool_to_float(v):
    return float(v)

#def color4(x):
#    return (tuple(x)+(1,1,1,1))[:4]

#def color3(x):
#    return (tuple(x)+(1,1,1))[:3]

def m44_invert(m):
    return m.inverted()

def mist_type(x):
    return {
        'QUADRATIC'         : MIST_TYPE_QUADRATIC,
        'LINEAR'            : MIST_TYPE_LINEAR,
        'INVERSE_QUADRATIC' : MIST_TYPE_INVERSE_QUADRATIC
    }[x]


# see blender/gpu/gpu_material.c:gpu_lamp_from_blender() and:
#void GPU_lamp_update_spot(GPULamp *lamp, float spotsize, float spotblend)
#{
#	lamp->spotsi = cosf(spotsize * 0.5f);
#	lamp->spotbl = (1.0f - lamp->spotsi) * spotblend;
#
#	gpu_lamp_calc_winmat(lamp);
#}
#
# gpu_lamp_from_blender...:
#lamp->spotsi = la->spotsize;
#if (lamp->mode & LA_HALO)
#	if (lamp->spotsi > DEG2RADF(170.0f))
#		lamp->spotsi = DEG2RADF(170.0f);

def lamp_dyn_energy(neg,x):
    if neg:
        return -x;
    else:
        return x;
    
def lamp_spotsize(size): #FIXME: this clamps all lamp modes to 170deg, not just LA_HALO
    return math.cos(0.5 * min(170.0 * Degree, size))

def lamp_spotblend(size, blend):
    return (1.0 - lamp_spotsize(size)) * blend


def lamp_imat(world_to_lamp, cam_to_world):
    return world_to_lamp * cam_to_world

def lamp_dynco(lamp_to_world, world_to_cam):
    location = lamp_to_world.col[3]
    return tuple(world_to_cam * location)[:3]

def lamp_dynvec(lamp_to_world, world_to_cam):
    v = lamp_to_world.col[2].normalized()
    return tuple(world_to_cam * vnegate(v))[:3]

def lamp_to_perspective(spotsize,clip0,clip1):
    w = clip0 * math.tan(spotsize/2.0)
    # TODO: handle sun
    #	if (lamp->type == LA_SUN) {
    #	wsize = lamp->la->shadow_frustum_size;
    #	orthographic_m4(lamp->winmat, -wsize, wsize, -wsize, wsize, lamp->d, lamp->clipend);

    return frustum(-w, w, -w, w, clip0, clip1)



# see blender/gpu/gpu_material.c:GPU_lamp_update_buffer_mats() and GPU_material_bind()
def lamp_perspective_matrix(spot_size, clip_start, clip_end, world_to_lamp, cam_to_world):
    perspective_to_depth = Matrix([[0.5, 0.0, 0.0, 0.5],
                                   [0.0, 0.5, 0.0, 0.5],
                                   [0.0, 0.0, 0.5, 0.5],
                                   [0.0, 0.0, 0.0, 1.0]])

    lp = lamp_to_perspective(spot_size, clip_start, clip_end)

    return perspective_to_depth * lp * world_to_lamp * cam_to_world


if __name__ == '__main__':
    from fractions import Fraction
    f = Fraction

    flatten = lambda x: sum(x,[])

    def compare_rows(a,b,epsilon = 0.000001):
        return all(map(lambda x: abs(x[0]-x[1]) < epsilon,
                       zip(flatten(a),
                           flatten(b))))
    
    m = Matrix([[f(1), f(2), f(3), f(4)],
                [f(5), f(6), f(7), f(8)],
                [f(9), f(1), f(2), f(3)],
                [f(4), f(5), f(0), f(7)]])

    
    ### test frustum
    frust = frustum(-1, 1, -1, 1, -1, 1)
    print("> frustum: test:\n> \t%r" % frust.rows)
    assert frust.rows == [[-1, 0, 0, 0],
                          [0, -1, 0, 0],
                          [0, 0, 0, 1],
                          [0, 0, -1, 0]]



    ### lamp_to_perspective
    lp = lamp_to_perspective(20,5,5000)
    print("> lamp_to_perspective: test:\n> \t%r" % lp.rows)
    assert compare_rows(lp.rows, [[1.5423510453569202, 0, 0.0, 0],
                                    [0, 1.5423510453569202, 0.0, 0],
                                    [0, 0, -1.002002002002002, -10.01001001001001],
                                    [0, 0, -1, 0]])

    lp = lamp_to_perspective(15,1,100)
    print("> lamp_to_perspective2: test:\n> \t%r" % lp.rows)
    assert compare_rows(lp.rows, [[0.36954725630901636, 0, 0.0, 0],
                                  [0, 0.36954725630901636, 0.0, 0],
                                  [0, 0, -1.02020202020202, -2.0202020202020203],
                                  [0, 0, -1, 0]])

    
    ### test lamp_perspective_matrix ###
    lpm = lamp_perspective_matrix(15, 1, 100, m, m)
    print("> lamp_perspective_matrix: test:\n> \t%r" % lpm.rows)    
    lpm_ref = [[0.18477362815450818, 0.0, -0.5, 0.0],
               [-1.1102230246251565e-16, 0.18477362815450799, -0.5000000000000002,
                                                              -3.3306690738754696e-16],
               [-2.220446049250313e-16, -5.551115123125783e-17, -1.01010101010101,
                                                                -1.0101010101010102],
               [0.0, -1.1102230246251565e-16, -1.0, -2.220446049250313e-16]]

    assert compare_rows(lpm.rows,lpm_ref)
