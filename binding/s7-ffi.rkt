#lang scribble/text

@(require racket/match
          racket/function
          racket/dict
          racket/path
          racket/format
          "../misc/math-symbol.rkt"
          "../misc/c-pre.rkt"
          "c-core.rkt"
          "s7-core.rkt")

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
  
/*
╺┳╸╻ ╻┏━┓┏━╸   ╺┳┓┏━╸┏━╸
 ┃ ┗┳┛┣━┛┣╸ ╺━╸ ┃┃┣╸ ┣╸ 
 ╹  ╹ ╹  ┗━╸   ╺┻┛┗━╸╹
generate new types
declare external types
*/
  
@(define (type-external-ops X)
  @C{
    
    typedef void* @|X|;
    extern int  _@|X|_type_id;
  })

@(define (type-internal-ops X)
   @C{
      
    // new-type: @|X|
    int         _@|X|_type_id = 0;
    extern void _@|X|_free(void* val);
    extern bool _@|X|_equal(void* v0, void* v1);
    extern void _@|X|_show(char*,size_t,void*);
    
    static char *_@|X|_print(s7_scheme *sc, void *val)
    {
        char *buf = (char *)calloc(SSHOW_BUF_SIZE,1);
        
        if (!buf)
            return NULL;
        
        _@|X|_show(buf, SSHOW_BUF_SIZE, val);
        buf[SSHOW_BUF_SIZE-1] = 0x0;
        
        return buf;
    }

    })

@(define (type-define X)
   @C{
    
      _@|X|_type_id = s7_new_type("@|X|",
                                  _@|X|_print,
                                  _@|X|_free,
                                  _@|X|_equal, NULL, NULL, NULL);
      
   })

////////// extern types //////////  
@(map-types extern-type?
  (∘ type-external-ops c-type))

//////////  new types   //////////
@(map-types new-type?
  (∘ type-internal-ops c-type))



/*
╻ ╻┏━┓┏━┓   ┏━╸┏━╸╺┳╸ ╻┏━┓┏━╸╺┳╸
┃┏┛┣━┫┣┳┛╺━╸┃╺┓┣╸  ┃ ┏┛┗━┓┣╸  ┃ 
┗┛ ╹ ╹╹┗╸   ┗━┛┗━╸ ╹ ╹ ┗━┛┗━╸ ╹ 
generated get/set ops for each type
*/
@(map-types new-type? scheme-get/set)


/*
┏━╸┏━╸╻   ┏━┓╻ ╻╺┳╸┏━┓┏━╸┏━╸┏┓╻
┣╸ ┣╸ ┃╺━╸┣━┫┃ ┃ ┃ ┃ ┃┃╺┓┣╸ ┃┗┫
╹  ╹  ╹   ╹ ╹┗━┛ ╹ ┗━┛┗━┛┗━╸╹ ╹
s7 wrap imported C functions
*/

@(map (λx @C{
             #include "generated/@|x|.h"

          })
  import-modules)


@;;;; Convert to and from S7 values ;;;;
@(define (ffi-transform-arg import)
  (replace-all
    
     (list `arg-ref rw1 rw2 t name)
       -->
         (let [[type-name (c-type (type-find import t))]]
        
         `[(type . ,(format "~a ~a *" (c-access-mode rw1) type-name))
           (name . ,(c-symbol name))
           (make . "s7_make_c_pointer(sc,(void*)result)")
           (read . ,(format "~a ~a * ~a ~a = (~a ~a *) ~a"
                          (c-access-mode rw1)
                          type-name
                          (c-access-mode rw2)
                          (c-symbol name)
                          (c-access-mode rw1) type-name
                          "s7_c_pointer(next_arg(&args))"))])

     (list `arg _ t name)
       -->
          (let* [(tt        (type-find import t))
                 (type-name (c-type tt))
                 (next-arg  (format (type-read tt) "sc" "next_arg(&args)"))
                 (read-arg  @string-append{
                                @|type-name| @|(c-symbol name)| =
                                             (@|type-name|) @|next-arg|
                            })]
        
          `[(type . ,type-name)
            (name . ,(c-symbol name))
            (make . ,(format (type-make tt) "sc" "result"))
            (read . ,read-arg)])))



@;;;; Generate ffi function bodies ;;;;
@(define (ffi-transform-function env)
        
   (define ffi-context
       (format "s7_c_pointer(s7_name_to_value(sc, \"ffi-context-~a\"))" env))
         
   (define (function name arguments [extra `()])
      
       (define-values (params inputs return)
         (let-values
          [[(xs x)
            (split-at-right arguments 1)]]
   
          (values
            (map (→ dict-ref `name) xs)
            (map (→ dict-ref `read) xs)
            (car x))))
      
       (define (call xs)
         (format "~a(~a)"
           (c-symbol name)
           (string-join (append extra xs) ",")))

       @C{     
         /////// @|name| ///////
     
         static s7_pointer
         ffi_@|(c-symbol name)|(s7_scheme *sc, s7_pointer args)
         {
             @| (string-join inputs ";\n    ") |;
             @| (dict-ref return `type) | result = @|(call params)|;
             return @| (dict-ref return `make) |;
         }
         
       })
   
  (replace-all
    
     (list-rest `function name _ xs)
       --> (function name xs)
   
     (list-rest `e-function name _ xs)
       --> (function name xs (list ffi-context))))



static s7_pointer next_arg(s7_pointer *args)
{
    s7_pointer result = s7_car(*args);
    *args = s7_cdr(*args);
    return result;
}


@(map (λ (a b)
         ((∘ (ffi-transform-function a)
             (ffi-transform-arg b))
            
          (dict-ref b `functions)))
    
      import-modules
      import-values)


  
/*
┏━┓┏━┓   ╻┏┳┓┏━┓┏━┓┏━┓╺┳╸
┗━┓┗━┓╺━╸┃┃┃┃┣━┛┃ ┃┣┳┛ ┃ 
┗━┛┗━┛   ╹╹ ╹╹  ┗━┛╹┗╸ ╹ 
add new-types to scheme environment
add wrapped C functions
create user defined implicit global parameter for `e-function types
*/
      
@(define (ffi-import module import)
  
  @list{
    
  void ss_import_@|(c-symbol module)|(scheme s, void *context)
  {
      s7_pointer @|(c-symbol module)| = s7_inlet(s.s7, s7_curlet(s.s7)); 
      s7_gc_protect(s.s7, @|(c-symbol module)|);

      @(map-types new-type?
        (∘ type-define c-type))
        
      s7_define_constant(s.s7,
                         "ffi-context-@|module|",
                         s7_make_c_pointer(s.s7, context));
    
      /////// define functions ///////
      @(let
           [[transform
             
             (replace-all-repeated
                `function                                --> `e-function
                (list `arg _ _ name)                     --> name
                (list `arg-ref _ _ _ name)               --> name
                (list-rest `e-function x description xs) -->
                    @C{
          
                       s7_define(s.s7, @|(c-symbol module)|, 
                                 s7_make_symbol(s.s7, "@|x|"),
                                 s7_make_function(s.s7, "@|x|",
                                                  ffi_@| (c-symbol x) |,
                                                  @| (~a (- (length xs) 1)) |,
                                                  0,
                                                  "false",
                                                  "@|description|"));
                       
                    })]] 

        
         (transform
          (dict-ref import `functions)))
          
      s7_define_variable(s.s7, "@|module|",
                         s7_let_to_list(s.s7, @| (c-symbol module) |));
      
      s7_gc_unprotect(s.s7, @| (c-symbol module) |);
  }
  
  })

///////// ffi module/environment /////////
@(map ffi-import import-modules import-values)


#ifdef __cplusplus
}
#endif
