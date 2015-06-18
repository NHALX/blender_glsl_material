#include <stdio.h>
#include <stdlib.h>
#include "generated/linear-algebra.h"

#define VS_ALLOC_SIZE 1024

struct vector_stack {
    real vs_base[VS_ALLOC_SIZE];
    const real *vs_stack_pointer;
};


int vs_reset_full(struct vector_stack * const vs)
{
    return vs_reset_to(vs, vs->vs_base);
}

int vs_reset_to(struct vector_stack * const vs, const real * const ptr)
{
    vs->vs_stack_pointer = ptr;
    return 0;
}

const real * const vs_mark(struct vector_stack * const vs)
{
    return vs->vs_stack_pointer;
}

struct vector_stack *
vs_init()
{
    struct vector_stack *vs = malloc(sizeof (struct vector_stack));
    vs_reset_full(vs);
    return vs;
}

int vs_free(struct vector_stack * const vs)
{
    free(vs);
    return 0;
}

real * const
vs_alloc(struct vector_stack * const vs, const unsigned int n)
{
    const real *result = vs->vs_stack_pointer;
    vs->vs_stack_pointer += n;
    
    if (result > &vs->vs_base[VS_ALLOC_SIZE - 1])
    {
         printf("vs_alloc: stack overflow\n");
         abort();
    }
    else
         return (real*)result;
}



