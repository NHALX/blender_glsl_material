#lang scribble/text

@(require racket/match
          racket/function
          racket/dict
          racket/path
          racket/format
          "../misc/c-pre.rkt"
          "s7-core.rkt"
          "c-core.rkt")

@(require (planet samsergey/rewrite:1:0))
/*
╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹╹
Auto-generated from: @|__FILE__| 
*/
@emit-#line
#ifdef __cplusplus
extern "C" {
#endif

#define MISSING_TYPES
#include "s7-scheme.h"
 

ss_result pack_result(s7_scheme *s7, s7_pointer p)
{
    ss_result result;

    if (s7_is_c_pointer(p)){
        result.pointer = s7_c_pointer(p);
        result.type    = SS_POINTER;
    }
    else if (s7_is_real(p)){
        result.real = s7_real(p);
        result.type = SS_REAL;
    }
    else if (s7_is_number(p)){
        result.integer = s7_integer(p);
        result.type    = SS_INTEGER;
    }
    else
    {
        result.type = SS_UNKNOWN;
    }

    //result.type = s7_object_type(p);
    //result.data = s7_object_value(p);
    //result.string = s7_object_to_c_string(s7, val);
    return result;
}

static s7_pointer
name_to_value(scheme s, const char *name)
{
    s7_pointer v = s7_name_to_value(s.s7, name);

    if (!v){
        printf("warning: %s: not found.\n", name);
        return NULL;
    } 

    return v;
}


ss_result
ss_get(scheme s, const char *name)
{
    s7_pointer sym = s7_symbol_table_find_name(s.s7, name);

    if (sym)
    {
        s7_pointer val = s7_symbol_value(s.s7, sym);
        return pack_result(s.s7, val);
    }
    else
    {
        ss_result r;
        r.type = SS_NOT_FOUND;
        r.pointer = NULL;
        return r;
    }
}

int ss_set_ptr(scheme s, const char *name, void *v)
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
        printf("ss_set_ptr: warning: symbol '%s' not found.\n", name);
        return -1;
    }
}



void ss_call1p(scheme s, const char *name, void *ptr)
{
    s7_pointer func = s7_name_to_value(s.s7, name);
    s7_call(s.s7, func, s7_cons(s.s7,
                                s7_make_c_pointer(s.s7, ptr), s7_nil(s.s7)));
    return; // TODO: return value, check symbol lookup failure above
}



/*
┏━┓┏━┓   ┏━┓┏━╸┏━┓╺┳┓
┗━┓  ┃╺━╸┣┳┛┣╸ ┣━┫ ┃┃ (support)
┗━┛  ╹   ╹┗╸┗━╸╹ ╹╺┻┛
S7 type conversion/support routines 
*/

  
const void* s7_object_value_s(s7_scheme *sc, int type_id, s7_pointer p)
{
    if (s7_is_object(p) && s7_object_type(p) == type_id)
        return s7_object_value(p);
    else
    {
        printf("warning: s7_object_value_s: type mismatch.\n");
        return NULL;
    }
}
  
@(define (read-safe name test convert invalid result)
    @C{
    
      @|result| @|name|(s7_scheme *sc, s7_pointer p)
      {
          if (@|test|)
              return @|convert|;
          else
          {
              printf("warning: @|name|: input failed type check.\n");
              return @|invalid|;
          }
      }

      })

@(map (⤶ apply read-safe) safe-conversion-table)
  

/*
╻ ╻┏━┓┏━┓   ┏━╸┏━╸╺┳╸ ╻┏━┓┏━╸╺┳╸
┃┏┛┣━┫┣┳┛╺━╸┃╺┓┣╸  ┃ ┏┛┗━┓┣╸  ┃ 
┗┛ ╹ ╹╹┗╸   ┗━┛┗━╸ ╹ ╹ ┗━┛┗━╸ ╹ 
*/
  
@(map-types extern-type? extern-get/set)
@(map-types
  (λ (x) (and (not (equal? x `(primitive-type void)))
              (primitive-type? x)))
  scheme-get/set)

  
/*
┏━┓┏━┓   ╻┏┓╻╻╺┳╸
┗━┓┗━┓╺━╸┃┃┗┫┃ ┃ 
┗━┛┗━┛   ╹╹ ╹╹ ╹ 
*/
  
void ss_free(scheme s)
{
    free(s.s7);
}

scheme ss_init()
{
    scheme s;
    s.s7 = s7_init();
    return s;
}

  
/*
┏━┓┏━┓   ┏━╸╻ ╻┏━┓╻  
┗━┓┗━┓╺━╸┣╸ ┃┏┛┣━┫┃  
┗━┛┗━┛   ┗━╸┗┛ ╹ ╹┗━╸
*/


#define ENV_MAX_SIZE 4096

ss_result ss_env_new(scheme s, const char *name)
{
    char buf[ENV_MAX_SIZE];
    snprintf(buf, sizeof buf, "(define %s (inlet))", name);
    return ss_eval(s, buf, NULL);
}


ss_env ss_env_enter(scheme s, const char *name)
{
    s7_pointer e = name_to_value(s, name);

    if (!e)
        return NULL;
    else
    {
        s7_pointer old = s7_set_curlet(s.s7, e);
        s7_gc_protect(s.s7, old);
        return old;
    }
}

void ss_env_exit(scheme s, ss_env e)
{
    if (e){
        s7_gc_unprotect(s.s7, e);
        s7_set_curlet(s.s7, e);
    }
}


ss_result ss_load(scheme s, const char *file, const char *env_name)
{
    if (env_name)
    {
        char buf[ENV_MAX_SIZE];
        snprintf(buf, sizeof buf, "(load \"%s\" %s)", file, env_name);
        return ss_eval(s, buf, NULL);
    }
    else
        return pack_result(s.s7, s7_load(s.s7, file)); 
}

ss_result ss_eval(scheme s, const char *code, const char *env)
{
    if (env)
    {
        s7_pointer e = name_to_value(s, env);

        if (!e)
            return result_not_found;
        else
            return pack_result(s.s7, 
                               s7_eval_c_string_with_environment(s.s7, 
                                                                 code,
                                                                 e));
    }

    return pack_result(s.s7, s7_eval_c_string(s.s7, code));
}


// caller must free string result
char* ss_seval(scheme s, const char *code, const char *env)
{
    s7_pointer result;

    if (env)
    {
        s7_pointer e = name_to_value(s, env);

        if (!e)
            return NULL;

        result = s7_eval_c_string_with_environment(s.s7, code, e);
    }
    else
        result = s7_eval_c_string(s.s7, code);

    return s7_object_to_c_string(s.s7, result);
}


#ifdef __cplusplus
}
#endif
