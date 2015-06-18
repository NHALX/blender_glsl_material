#include <math.h>
#include <stdio.h>
#include "generated/linear-algebra.h"


const vector
alloc_vector(struct vector_stack *vs_ctx){
    void *v = vs_alloc(vs_ctx, 4);
    return v;
}

const vector
v4(void *sl, const real x00, const real x01, const real x02, const real x03)
{
    vector v = alloc_vector(sl);
    
    (*v)[0] = x00;
    (*v)[1] = x01;
    (*v)[2] = x02;
    (*v)[3] = x03;
    return v;
}

const vector
v4_from3(void *sl, const real x00, const real x01, const real x02)
{
    return v4(sl,x00,x01,x02,1.0);
}

const vector
v3_negate(void *sl, const vector v1)
{
    vector v2 = alloc_vector(sl);
    
    (*v2)[0] = -(*v1)[0];
    (*v2)[1] = -(*v1)[1];
    (*v2)[2] = -(*v1)[2];
    (*v2)[3] = 1.0;
    return v2;
}

const vector
v3_normalize(void *sl, const vector v)
{
    vector v2 = alloc_vector(sl);
    
    real mag = sqrtf(pow(fabs((*v)[0]),2) +
                     pow(fabs((*v)[1]),2) +
                     pow(fabs((*v)[2]),2));
    
    (*v2)[0] = (*v)[0] / mag;
    (*v2)[1] = (*v)[1] / mag;
    (*v2)[2] = (*v)[2] / mag; 
    (*v2)[3] = 1.0;
    return v2;
}


const int
v4_print(void *ctx, const vector v)
{
    printf("vec4 (%f %f %f %f)\n", 
           (*v)[0],(*v)[1],(*v)[2],(*v)[3]);
    
    return 0;
}
