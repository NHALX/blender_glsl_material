#include "embed/s7/s7.h"
#include "generated/linear-algebra.h"
#include "generated/shader-link.h"
#include "generated/scheme-s7.h"


int main(int argc, char**argv)
{
    if (argc == 2)
    {
        scheme s2 = scheme_init(NULL);
        scheme_eval(s2, "(apply varlet (curlet) linear-algebra)");
        scheme_load(s2, argv[1]);
        scheme_free(s2);
        return 0;
    }
}
