#ifdef __cplusplus
extern "C" {
#endif

#include "s7.h"

typedef float real;
//typedef real (*matrix)[4][4]; 
//typedef real (*vector)[4];
//struct vector_stack;

typedef bool          boolean;
typedef const char*   string;
typedef s7_Int        integer;
typedef char          character;
typedef unsigned long ulong;

enum scheme_type { SCHEME_POINTER, SCHEME_REAL, SCHEME_INTEGER, SCHEME_UNKNOWN, SCHEME_NOT_FOUND };

typedef struct {
    enum scheme_type type;
    union {
        void  *pointer;
        double real;
        int    integer;
    };
} scheme_result;


typedef struct {
    s7_scheme *s7;
#ifdef WITH_LINEAR_ALGEBRA
    struct vector_stack *vs;
#endif
} scheme;
    
extern int           scheme_set_ptr(scheme, const char *, void *);
extern scheme_result scheme_load(scheme, const char*);
extern scheme_result scheme_eval(scheme, const char*);
extern scheme_result scheme_get(scheme s, const char *name);
extern void          scheme_call1p(scheme, const char *, void *);
extern scheme        scheme_init(void*,void*);
extern void          scheme_free(scheme);

#ifdef __cplusplus
}
#endif
