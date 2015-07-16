#ifdef __cplusplus
extern "C" {
#endif
#include "embed/s7/s7.h"

typedef float         real;
typedef bool          boolean;
typedef const char*   string;
typedef s7_Int        integer;
typedef char          character;
typedef unsigned long ulong;

enum ss_type { SS_POINTER, SS_REAL, SS_INTEGER, SS_UNKNOWN, SS_NOT_FOUND };

typedef struct {
    enum ss_type type;
    union {
        void  *pointer;
        double real;
        int    integer;
    };
} ss_result;


typedef struct {
    s7_scheme *s7;
#ifdef WITH_LINEAR_ALGEBRA
    struct vector_stack *vs;
#endif
} scheme;
    
extern int       ss_set_ptr(scheme, const char *, void *);
extern ss_result ss_load(scheme, const char*);
extern ss_result ss_eval(scheme, const char*);
extern ss_result ss_get(scheme s, const char *name);
extern void      ss_call1p(scheme, const char *, void *);
extern scheme    ss_init();
extern void      ss_free(scheme);

#ifdef __cplusplus
}
#endif
