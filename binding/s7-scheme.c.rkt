#lang scribble/text

@(require racket/match
          racket/function
          racket/dict
          racket/path
          racket/format
          "../misc/math-symbol.rkt"
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

@(map (← apply read-safe) safe-conversion-table)
  

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
  
ss_result ss_load(scheme s, const char *file)
{
    return pack_result(s.s7, s7_load(s.s7, file)); // TODO: eval in tighter env
}

ss_result ss_eval(scheme s, const char *buffer)
{
    return pack_result(s.s7, s7_eval_c_string(s.s7, buffer));
}

  

#ifdef __cplusplus
}
#endif
