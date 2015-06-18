#include "embed/s7/s7.h"
#include "generated/linear-algebra.h"
#include "generated/shader-link.h"
#include "generated/scheme-s7.h"

const char *code =
"(begin"
"  (lamp-perspective-matrix 25 1.0 100.0 "
"  (m44 1 2 3 4"
"       5 6 7 8"
"       1 1 2 3"
"       4 5 1 7)"
"  (m44 1 2 3 4"
"       4 3 2 1"
"       1 2 3 4"
"       4 3 2 1) ))"
;

int
__attribute__ ((noinline)) native(struct vector_stack *vs)
{
    matrix m1 = m44(vs,
                    1,2,3,4,
                    5,6,7,8,
                    1,1,2,3,
                    4,5,1,7);
    matrix m2 = m44(vs,
                    1,2,3,4,
                    4,3,2,1,
                    1,2,3,4,
                    4,3,2,1);

    matrix p = lamp_perspective_matrix(vs, 25,1.0,100.0,m1,m2);
    return (int) (*p)[0][0];
}

int __attribute__ ((noinline)) interp(scheme s)
{
    return scheme_eval(s,code).type;
}

int __attribute__ ((noinline)) interp_pure(scheme s)
{
    return scheme_eval(s,code).type;
}


int main(int argc, char**argv)
{
    int i,x;
    scheme s = scheme_init(NULL);
    scheme s2 = scheme_init(NULL);
    struct vector_stack *vs = vs_init();

    for (x = 0, i = 0; i < 1000000; ++i)
    {
        // TODO: this module is out of sync with test3.rkt
        vs_reset_full(vs);
        x += native(vs);        
    }
    printf("x=%p\n", x);

    scheme_eval(s2, "(apply varlet (curlet) linear-algebra)");
    scheme_load(s2, "test3.rkt");
    for (x = 0, i = 0; i < 1000000; ++i)
        x += interp_pure(s2);

    printf("x=%p\n", x);
        
    scheme_eval(s, "(apply varlet (curlet) (append linear-algebra shader-link))");
    for (x = 0, i = 0; i < 1000000; ++i)
        x += interp(s);

    printf("x=%p\n", x);
}

