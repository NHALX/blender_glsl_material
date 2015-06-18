#include <stdlib.h>
#include <stdio.h>
#include <string.h> 
#include <math.h>
#include <stdbool.h>
#include "generated/shader-link.h" // TODO: rename to shader prelude
#include "generated/linear-algebra.h"

#define MATRIX_MUL(SL,N,...) m44mul(SL, (const void*[N]){ __VA_ARGS__ }, N)

real bool_to_float(void *sl, const bool v)
{
    return v ? 1.0 : 0.0;
}

float mist_type(void *sl, const char *name)
{
    if (strcmp(name, "QUADRATIC") == 0)
        return 0.0;
    else if (strcmp(name, "LINEAR") == 0)
        return 1.0;
    else if (strcmp(name, "INVERSE_QUADRATIC") == 0)
        return 2.0;
    else
        return NAN;
}

real lamp_spotsize(void *sl, real x)
{
#define PI_180 0.017453292519943295769236907684886127134428718885417
    return cos(0.5 * fmin(170*PI_180,x));
}

real lamp_spotblend(void *sl, real size, real blend)
{
    return (1.0 - lamp_spotsize(sl,size)) * blend;
}


matrix lamp_imat(void *sl, matrix world_to_lamp, matrix cam_to_world)
{    
    return MATRIX_MUL(sl, 2, world_to_lamp, cam_to_world);
}

vector lamp_dynco(void *sl, matrix lamp_to_world, matrix world_to_cam)
{
    return
        m44mul_v3(sl,
                   world_to_cam,
                   m44_col(sl, lamp_to_world, 3)); // position
}

vector lamp_dynvec(void *sl, matrix lamp_to_world, matrix world_to_cam)
{
    return
        m44mul_v3(sl,
                   world_to_cam, v3_negate(sl,
                                 v3_normalize(sl,
                                 m44_col(sl, lamp_to_world, 2)))); // orientation
}


matrix lamp_to_perspective(void *sl, real bl_spotsize, real clip0, real clip1)
{
    const real w = clip0 * tan(bl_spotsize / 2.0);
    /*
     * TODO: handle sun
     *	if (lamp->type == LA_SUN) {
     *	wsize = lamp->la->shadow_frustum_size;
     *	orthographic_m4(lamp->winmat, -wsize, wsize, -wsize, wsize, lamp->d, lamp->clipend);
     */
        
    return m44_frustum(sl, -w, w, -w, w, clip0, clip1);
}


matrix lamp_perspective_matrix(
    void *sl, 
    real bl_spotsize,
    real clip_start,
    real clip_end,
    matrix world_to_lamp,
    matrix cam_to_world)
{
    matrix perspective_to_depth = m44(sl,
                                      0.5, 0.0, 0.0, 0.5,
                                      0.0, 0.5, 0.0, 0.5,
                                      0.0, 0.0, 0.5, 0.5,
                                      0.0, 0.0, 0.0, 1.0);
    
    matrix lp = lamp_to_perspective(sl, bl_spotsize, clip_start, clip_end);

    return MATRIX_MUL(sl, 4, perspective_to_depth, lp, world_to_lamp, cam_to_world);
}
        

