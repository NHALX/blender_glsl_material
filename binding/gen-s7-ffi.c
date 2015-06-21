#ifdef __cplusplus
extern "C" {
#endif
    
#include <stdlib.h>
#include <string.h>
#include <math.h> // for NAN
#include <limits.h> // INT_MAX
#include "s7.h"

#include "gen-s7-ffi.h" //(format "#include \"~a\"" output-header-file)
//#include "generated/scheme-s7-imports.h"


static s7_pointer next_arg(s7_pointer *args)
{ 
    s7_pointer result = s7_car(*args);
    *args = s7_cdr(*args);
    return result;
}

unsigned long s7_ulong_safe(s7_scheme *sc, s7_pointer p)
{
    if (s7_is_ulong(p))
        return s7_ulong(p);
    else
    {
        printf("warning: s7_ulong_safe: input is not a ulong.\n");
        return -1; 
    }
}

s7_Int s7_integer_safe(s7_scheme *sc, s7_pointer p)
{
    if (s7_is_integer(p))
        return s7_integer(p);
    else
    {
        printf("warning: s7_integer_safe: input is not a integer.\n");
        return INT_MAX; 
    }
}


s7_Double s7_real_safe(s7_scheme *sc, s7_pointer p)
{
    if (s7_is_real(p))
        return s7_real(p);

    else if (s7_is_number(p))
        return s7_number_to_real(sc, p);

    else
    {
        printf("warning: s7_real_safe: input is not a number.\n");
        return NAN;
    }
}

const char* s7_string_safe(s7_scheme *sc, s7_pointer p)
{
    if (s7_is_string(p))
        return s7_string(p);
    else
    {
        printf("warning: s7_string_safe: input is not a string.\n");
        return "(undefined)";
    }
}

const void* s7_object_value_safe(int type_id, s7_pointer p)
{
    if (s7_is_object(p) && s7_object_type(p) == type_id)
        return s7_object_value(p);
    else
    {
        printf("warning: s7_object_value_safe: type mismatch.\n");
        return NULL;
    }
}

static scheme_result pack_result(s7_scheme *s7, s7_pointer p)
{
    scheme_result result;

    if (s7_is_c_pointer(p)){
        result.pointer = s7_c_pointer(p);
        result.type    = SCHEME_POINTER;
    }
    else if (s7_is_real(p)){
        result.real = s7_real(p);
        result.type = SCHEME_REAL;
    }
    else if (s7_is_number(p)){
        result.integer = s7_integer(p);
        result.type    = SCHEME_INTEGER;
    }
    else
    {
        result.type = SCHEME_UNKNOWN;
    }

    //result.type = s7_object_type(p);
    //result.data = s7_object_value(p);
    //result.string = s7_object_to_c_string(s7, val);
    return result;
}

scheme_result 
scheme_get(scheme s, const char *name)
{
    s7_pointer sym = s7_symbol_table_find_name(s.s7, name);

    if (sym)
    {
        s7_pointer val = s7_symbol_value(s.s7, sym);
        return pack_result(s.s7, val);
    }
    else
    {
        scheme_result r;
        r.type = SCHEME_NOT_FOUND;
        r.pointer = NULL;
        return r;
    }
}

int scheme_set_ptr(scheme s, const char *name, void *v)
{
    s7_pointer sym = s7_symbol_table_find_name(s.s7, name);

    if (sym)
    {
        s7_pointer val = s7_make_c_pointer(s.s7,v);
        s7_symbol_set_value(s.s7, sym, val);
        return 0;
    }
    else
    {
        printf("scheme_set_ptr: warning: symbol '%s' not found.\n", name);
        return -1;
    }
}

scheme_result scheme_load(scheme s, const char *file)
{
    return pack_result(s.s7, s7_load(s.s7, file)); // TODO: eval in tighter env
}

scheme_result scheme_eval(scheme s, const char *buffer)
{
    return pack_result(s.s7, s7_eval_c_string(s.s7, buffer));
}

void scheme_call1p(scheme s, const char *name, void *ptr)
{
    s7_pointer func = s7_name_to_value(s.s7, name);
    s7_call(s.s7, func, s7_cons(s.s7, s7_make_c_pointer(s.s7, ptr), s7_nil(s.s7)));
    return; // TODO: return value, check symbol lookup failure above
}

void scheme_free(scheme s)
{ 
    free(s.s7);
}


#ifdef WITH_LINEAR_ALGEBRA
#define SSHOW_BUF_SIZE 1024

#define XXX_PRINT(XXX)                                                  \
    extern void XXX##_show(char*,size_t,void*);                         \
    static char *XXX##_print(s7_scheme *sc, void *val)                  \
    {                                                                   \
        char *buf = (char *)calloc(SSHOW_BUF_SIZE,1);                   \
        if (!buf)                                                       \
            return NULL;                                                \
        XXX##_show(buf, SSHOW_BUF_SIZE, val);                           \
        buf[SSHOW_BUF_SIZE-1] = 0x0;                                    \
        return buf;                                                     \
    }

XXX_PRINT(_v4)
XXX_PRINT(_v3)
XXX_PRINT(_m44)

extern bool _v3_equal(void* v0, void* v1);
extern bool _v4_equal(void* v0, void* v1);
extern bool _m44_equal(void* v0, void* v1);

extern void _m44_free(void* val);
extern void _v4_free(void* val);
extern void _v3_free(void* val);

#endif

#include "generated/scheme-s7.c"

    
#ifdef __cplusplus
}
#endif 
