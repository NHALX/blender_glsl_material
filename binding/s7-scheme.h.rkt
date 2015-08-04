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
#include <stdlib.h>
#include <string.h>
#include <math.h>   // NAN
#include <limits.h> // INT_MAX, ULONG_MAX
#include "embed/s7/s7.h"

@(define (map-c-types pred f)
  (define g
   (∘ c-type
      (⤶ filter pred)
      (⤷ dict-ref `types)))
  
  (map f (remove-duplicates
          (apply append
           (map g import-values)))))
    
#ifdef MISSING_TYPES

@(define (extern-or-new? x)
  (or (new-type? x)
      (extern-type? x)))
  
@(map-c-types extern-or-new?
    (⤶ format "typedef void * ~a;\n"))  
#endif

#define SSHOW_BUF_SIZE 1024
  
typedef float         real;
typedef bool          boolean;
typedef const char*   string;
typedef s7_Int        integer;
typedef char          character;
typedef unsigned long ulong;

enum ss_type { SS_POINTER,
               SS_REAL,
               SS_INTEGER,
               SS_UNKNOWN,
               SS_NOT_FOUND };
  
typedef struct { s7_scheme *s7; } scheme;
   
typedef struct {
    enum ss_type type;
    union {
        void  *pointer;
        double real;
        int    integer;
    };
} ss_result;

const ss_result result_not_found = { SS_NOT_FOUND, 0 };
  
extern ss_result ss_get(scheme s, const char *name);
extern int ss_set_ptr(scheme s, const char *name, void *v);
extern void ss_call1p(scheme s, const char *name, void *ptr);


/*
┏━┓┏━┓   ┏━┓┏━╸┏━┓╺┳┓
┗━┓  ┃╺━╸┣┳┛┣╸ ┣━┫ ┃┃ (support)
┗━┛  ╹   ╹┗╸┗━╸╹ ╹╺┻┛
S7 type conversion/support routines 
*/

  
extern const void* s7_object_value_s(s7_scheme *sc, int type_id, s7_pointer p);
  
@(define (read-safe name test convert invalid result)
 @C{
    extern @|result| @|name|(s7_scheme *sc, s7_pointer p);
    
 })

@(map (⤶ apply read-safe) safe-conversion-table)
 

/*
╻ ╻┏━┓┏━┓   ┏━╸┏━╸╺┳╸ ╻┏━┓┏━╸╺┳╸
┃┏┛┣━┫┣┳┛╺━╸┃╺┓┣╸  ┃ ┏┛┗━┓┣╸  ┃ 
┗┛ ╹ ╹╹┗╸   ┗━┛┗━╸ ╹ ╹ ┗━┛┗━╸ ╹ 
all *set functions take ownership of the pointer passed to them,
registering it for garbage collection.
*/
@(map-c-types
  (λ (x) (not (equal? x `(primitive-type void))))
  extern-get/set)

/*
┏━┓┏━┓   ╻┏┓╻╻╺┳╸
┗━┓┗━┓╺━╸┃┃┗┫┃ ┃ 
┗━┛┗━┛   ╹╹ ╹╹ ╹ 
*/
  
extern void ss_free(scheme s);
extern scheme ss_init();
  
/*
┏━┓┏━┓   ┏━╸╻ ╻┏━┓╻  
┗━┓┗━┓╺━╸┣╸ ┃┏┛┣━┫┃  
┗━┛┗━┛   ┗━╸┗┛ ╹ ╹┗━╸
*/
typedef s7_pointer ss_env;
extern ss_result ss_env_new(scheme s, const char *name);
extern ss_env    ss_env_enter(scheme s, const char *name);
extern void      ss_env_exit(scheme s, ss_env old);
extern ss_result ss_load(scheme s, const char *file, const char *maybe_env); 
extern ss_result ss_eval(scheme s, const char *buffer, const char *maybe_env);
extern char*     ss_seval(scheme s, const char *code, const char *env); 
                          // caller must free string result
  
/*
┏━┓┏━┓   ╻┏┳┓┏━┓┏━┓┏━┓╺┳╸
┗━┓┗━┓╺━╸┃┃┃┃┣━┛┃ ┃┣┳┛ ┃ 
┗━┛┗━┛   ╹╹ ╹╹  ┗━┛╹┗╸ ╹
*/

@(map (λ (x)
       @C{
      
         #include "generated/@|x|.h"
         extern void ss_import_@|(c-symbol x)|(scheme ss, void *user_ctx);
            
       })
  
  import-modules)


#ifdef __cplusplus
}
#endif
