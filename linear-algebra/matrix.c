#include <string.h>
#include <stdio.h>
#include "impl_sse.c"
#include "generated/linear-algebra.h"

#define MATRIX_MUL(SL,N,...) m44mul(SL, (const void*[N]){ __VA_ARGS__ }, N)

extern vector alloc_vector(struct vector_stack *vs_ctx);

matrix alloc_matrix(struct vector_stack *vs_ctx)
{
    void *m = vs_alloc(vs_ctx, 4*4);
    return m;
}

const matrix
m44mul(void *sl, const void *xs[], const unsigned int argc)
{
    matrix result = alloc_matrix(sl);
    size_t i = 0;

    if (argc < 2)
        return NULL; // TODO: raise error
    
    M4x4_SSE((real*)xs[0], (real*)xs[1], (real*)result);

    for (i = 2; i < argc; ++i)
        M4x4_SSE((real*)result, (real*)xs[i], (real*)result);

    return result;
}

const matrix
m44_invert(void *sl, const matrix src)
{
    matrix result = alloc_matrix(sl);
    
    memcpy(result, src, sizeof *result);
    PIII_Inverse_4x4((real*)result);
    return result;
}


const matrix
m44(
    void *sl, 
    const real x00, const real x01, const real x02, const real x03,
    const real x10, const real x11, const real x12, const real x13,
    const real x20, const real x21, const real x22, const real x23,
    const real x30, const real x31, const real x32, const real x33)
{
    matrix m = alloc_matrix(sl);
    
    (*m)[0][0] = x00;
    (*m)[0][1] = x01;
    (*m)[0][2] = x02;
    (*m)[0][3] = x03;
    (*m)[1][0] = x10;
    (*m)[1][1] = x11;
    (*m)[1][2] = x12;
    (*m)[1][3] = x13;
    (*m)[2][0] = x20;
    (*m)[2][1] = x21;
    (*m)[2][2] = x22;
    (*m)[2][3] = x23;
    (*m)[3][0] = x30;
    (*m)[3][1] = x31;
    (*m)[3][2] = x32;
    (*m)[3][3] = x33;
    return m;
}

const matrix
m44_transpose(void *sl, const matrix m0)
{
    matrix m = alloc_matrix(sl);
    
    (*m)[0][0] = (*m0)[0][0];
    (*m)[0][1] = (*m0)[1][0];
    (*m)[0][2] = (*m0)[2][0];
    (*m)[0][3] = (*m0)[3][0];
    (*m)[1][0] = (*m0)[0][1];
    (*m)[1][1] = (*m0)[1][1];
    (*m)[1][2] = (*m0)[2][1];
    (*m)[1][3] = (*m0)[3][1];
    (*m)[2][0] = (*m0)[0][2];
    (*m)[2][1] = (*m0)[1][2];
    (*m)[2][2] = (*m0)[2][2];
    (*m)[2][3] = (*m0)[3][2];
    (*m)[3][0] = (*m0)[0][3];
    (*m)[3][1] = (*m0)[1][3];
    (*m)[3][2] = (*m0)[2][3];
    (*m)[3][3] = (*m0)[3][3];
    return m;
}

const matrix
m44_frustum(
    void *sl, 
    const real left,
    const real right,
    const real bottom,
    const real top,
    const real zNear,
    const real zFar)
{
    real A = (right + left) / (right - left);
    real B = (top + bottom) / (top - bottom);
    real C = - (zFar + zNear) / (zFar - zNear);
    real D = - (2*zFar*zNear) / (zFar - zNear);
    real u = (2*zNear) / (right-left);
    real v = (2*zNear) / (top - bottom);

    return m44(sl,
               u,0,A,0,
               0,v,B,0,
               0,0,C,D,
               0,0,-1,0);
}


const vector
m44_col(void *sl, const matrix m, const unsigned int i)
{
    vector v = alloc_vector(sl);
    
    (*v)[0] = (*m)[0][i];
    (*v)[1] = (*m)[1][i];
    (*v)[2] = (*m)[2][i];
    (*v)[3] = (*m)[3][i];
    return v;
}

const vector
m44mul_v3(void *sl, const matrix m_, const vector v_)
{
        real v3 = 1.0;
#define v (*v_)
#define m (*m_)
        return v4(sl,                                    
              v[0]*m[0][0] + v[1]*m[0][1] + v[2]*m[0][2] + v3*m[0][3], 
              v[0]*m[1][0] + v[1]*m[1][1] + v[2]*m[1][2] + v3*m[1][3], 
              v[0]*m[2][0] + v[1]*m[2][1] + v[2]*m[2][2] + v3*m[2][3], 
              v[0]*m[3][0] + v[1]*m[3][1] + v[2]*m[3][2] + v3*m[3][3]);
#undef v
#undef m
}

const vector
m33mul_v3(void *sl, const matrix m_, const vector v_)
{
#define v (*v_)
#define m (*m_)
        return v4_from3(sl,
                  v[0]*m[0][0] + v[1]*m[0][1] + v[2]*m[0][2],
                  v[0]*m[1][0] + v[1]*m[1][1] + v[2]*m[1][2],
                  v[0]*m[2][0] + v[1]*m[2][1] + v[2]*m[2][2]);
#undef v
#undef m
}


const int
m44_print(void *ctx, const matrix v)
{
    printf("m44 (%f %f %f %f\n"
           "     %f %f %f %f\n"
           "     %f %f %f %f\n"
           "     %f %f %f %f)\n", 
           (*v)[0][0],(*v)[0][1],(*v)[0][2],(*v)[0][3],
           (*v)[1][0],(*v)[1][1],(*v)[1][2],(*v)[1][3],
           (*v)[2][0],(*v)[2][1],(*v)[2][2],(*v)[2][3],
           (*v)[3][0],(*v)[3][1],(*v)[3][2],(*v)[3][3]);
    
    return 0;
}
