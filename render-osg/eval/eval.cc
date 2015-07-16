#include "OpenSceneGraph.hh"
#include "Matrix.hh"
#include "generated/scheme.h"


int main(int argc, char**argv)
{
    if (argc == 2)
    {
        scheme _scheme = ss_init();
        ss_import_linear_algebra(_scheme, NULL);
        //ss_import_uniform(_scheme, NULL);
        ss_eval(_scheme, "(apply varlet (curlet) linear-algebra)");
        //ss_eval(_scheme, "(apply varlet (curlet) uniform)");
        ss_load(_scheme, argv[1]); 
        ss_free(_scheme);
    }
}
