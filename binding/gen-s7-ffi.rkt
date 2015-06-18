#lang racket
(require "gen-common.rkt")
(require "gen-base-C.rkt")


(define s7-types
  '(begin    
     (define u-bool         "bool")
     (define u-matrix       "matrix")
     (define u-vector       "vector")
     (define u-string       "string")
     (define u-int          "integer")
     (define u-uint         "ulong")
     (define u-float        "real")
     (define u-vector-stack "struct vector_stack") ))


(define C-language-s7-def
  '(begin
     
     (define (flip f)
       (λ (a b) (f b a)))
     
     (define lookup
       (curry (flip dict-ref)))

     ;;;;;;;;;;;;;;;;;;;;;
     
     (define ffi-context
       (format "s7_c_pointer(s7_name_to_value(sc, \"ffi-context-~a\"))" export-environment))
     
     (define (u-e-variadic-function name description return)
       (define body (string-join
         `("static s7_pointer ffi_~a(s7_scheme *sc, s7_pointer args)"
           "{"
           "    size_t i;"
           "    size_t n = s7_list_length(sc, args);"
           "    void **as = (void**) alloca(sizeof(void*)*n);"
           ""
           "    for (i = 0; i < n; ++i)"
           "        if (!s7_is_c_pointer)"
           "            return s7_f(sc);"
           "        else"
           "            as[i] = s7_c_pointer(next_arg(&args));"
           ""
           "    ~a result = ~a~a(~a, (const void**) as, n);"
           "    return ~a;"
           "}")
         "\n"))
       (format body
               (u-symbol name)
               (dict-ref return 'type)
               c-symbol-prefix
               (u-symbol name)
               ffi-context
               (dict-ref return 'make)))

     

     (define (function-gen name params [extra-env '()])
       (define-values (args ret) (split-at-right params 1))
       
       (define (call-fmt xs)
         (format "~a~a(~a)" c-symbol-prefix (u-symbol name) (string-join xs ", ")))
       
       (define call-vars
         (map (lookup 'name) args))
       
       (format (string-join
                `("static s7_pointer ffi_~a(s7_scheme *sc, s7_pointer args)"
                  "{"
                  "    ~a;"
                  "    ~a result = ~a;"
                  "    return ~a;"
                  "}")
                "\n")
               (u-symbol name)
               (string-join (map (lookup 'read) args) ";\n    ")
               (lookup 'type (car ret))
               (call-fmt (append extra-env call-vars))
               (lookup 'make (car ret)))
       )
     
     (define (u-function name description . xs)
       (function-gen name xs))
     
     (define (u-e-function name description . xs)
       (function-gen name xs (list ffi-context)))

     (define (u-arg-ref rw1 rw2 t name)
       `[(type . ,(format "~a ~a *" rw1 t))
         (name . ,(u-symbol name))
         (make . "s7_make_c_pointer(sc,(void*)result)")
         (read . ,(format "~a ~a * ~a ~a = (~a ~a *) ~a"
                          rw1 t
                          rw2 
                          (u-symbol name)
                          rw1 t
                          "s7_c_pointer(next_arg(&args))"))])
     
     (define (u-arg rw t name)
       (define/match (make x)
         [((== u-vector))        "s7_make_c_pointer(sc,~a)"] 
         [((== u-matrix))        "s7_make_c_pointer(sc,~a)"]
         [((== u-bool))          "s7_make_boolean(sc,~a)"]
         [(x)            (format "s7_make_~a(sc,~~a)" x)]
         )
       
       (define/match (get x)
         [((== u-vector))        "s7_c_pointer(~a)"]
         [((== u-matrix))        "s7_c_pointer(~a)"]
         [((== u-bool))          "s7_boolean(sc,~a)"]
         [(x)            (format "s7_~a_safe(sc,~~a)" x)]) 
       
       `[(type . ,t)
         (name . ,(u-symbol name))
         (make . ,(format (make t) "result"))
         (read . ,(format "~a ~a = (~a) ~a"
                          t
                          (u-symbol name)
                          t
                          (format (get t) "next_arg(&args)")))])))


(define C-language-s7-decl-s7
  '(begin
     ;;TODO: support variadic functions with fixed args
     
     (define (s7-deffun x description args-n has-optional)
       (format "
    s7_define(s7, 
              ~a, 
              s7_make_symbol(s7, \"~a\"),
              s7_make_function(s7, \"~a\", ffi_~a, ~a, 0, ~a, \"~a\"));"
               (u-symbol export-environment)
               x
               x
               (u-symbol x)
               args-n
               has-optional
               description))
     
     (define (u-e-variadic-function x description ret)
       (s7-deffun x description 0 "true"))
     
     (define (u-e-function x description . xs)
       (s7-deffun x description (- (length xs) 1) "false"))
     
     (define u-function
       u-e-function)
     
     (define (u-arg rw t name)
       name)
     
     (define (u-arg-ref rw1 rw2 t name)
       name) ))




#;(define monoid-str
  ""
  (λ (a b) (string-join (list a b) "\n") ))

(define (foldMap f xs) (string-join (map f xs) "\n"))

(define c-header
"
#ifdef __cplusplus
extern \"C\" {
#endif

#include \"s7.h\"

typedef float real;
typedef real (*matrix)[4][4]; 
typedef real (*vector)[4];

struct vector_stack;

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
extern scheme        scheme_init(void*);
extern void          scheme_free(scheme);

#ifdef __cplusplus
}
#endif
")

(define (gen-c-src input-files output-header-file declarations definitions)
  
  (define import-modules
    (map file->environment-name input-files))

  (define (u-symbol x)
    (string-replace x "-" "_"))
  
  (define (env-new x)
    (format
     "
#ifdef WITH_~a
    ~a = s7_inlet(s.s7, s7_curlet(s.s7)); 
    s7_gc_protect(s.s7, ~a);
#endif"
     (string-upcase (u-symbol x))
     (u-symbol x)
     (u-symbol x) ))
  
  (define (env-add x)
    (format
     "
#ifdef WITH_~a  
    s7_define_variable(s.s7, \"~a\", s7_let_to_list(s.s7, ~a));
    s7_gc_unprotect(s.s7, ~a);
#endif"
     (string-upcase (u-symbol x))
     x
     (u-symbol x)
     (u-symbol x)))
  
  (string-append
"
#include <stdlib.h>
#include <math.h> // for NAN
#include <limits.h> // INT_MAX
#include \"s7.h\"
"
(format "#include \"~a\"" output-header-file)
"
"
(foldMap (λ (x)
           (format "#ifdef WITH_~a\n#include \"generated/~a.h\"\n#endif"
                   (string-upcase (u-symbol x))
                   x)) import-modules)
"
static s7_pointer next_arg(s7_pointer *args){ 
    s7_pointer result = s7_car(*args);
    *args = s7_cdr(*args);
    return result;
}

unsigned long s7_ulong_safe(s7_scheme *sc, s7_pointer p){
    if (s7_is_ulong(p))
        return s7_ulong(p);
    else
    {
        printf(\"warning: s7_ulong_safe: input is not a ulong.\\n\");
        return -1; 
    }
}

s7_Int s7_integer_safe(s7_scheme *sc, s7_pointer p){
    if (s7_is_integer(p))
        return s7_integer(p);
    else
    {
        printf(\"warning: s7_integer_safe: input is not a integer.\\n\");
        return INT_MAX; 
    }
}


s7_Double s7_real_safe(s7_scheme *sc, s7_pointer p){

    if (s7_is_real(p))
        return s7_real(p);

    else if (s7_is_number(p))
        return s7_number_to_real(sc, p);

    else
    {
        printf(\"warning: s7_real_safe: input is not a number.\\n\");
        return NAN;
    }
}

const char* s7_string_safe(s7_scheme *sc, s7_pointer p){
    if (s7_is_string(p))
        return s7_string(p);
    else
    {
        printf(\"warning: s7_string_safe: input is not a string.\\n\");
        return \"(undefined)\";
    }
}
"
declarations
"
static scheme_result pack_result(s7_scheme *s7, s7_pointer p){
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
scheme_get(scheme s, const char *name){
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

int scheme_set_ptr(scheme s, const char *name, void *v) {
    s7_pointer sym = s7_symbol_table_find_name(s.s7, name);

    if (sym)
    {
        s7_pointer val = s7_make_c_pointer(s.s7,v);
        s7_symbol_set_value(s.s7, sym, val);
        return 0;
    }
    else
    {
        printf(\"scheme_set_ptr: warning: symbol '%s' not found\\n\", name);
        return -1;
    }
}

scheme_result scheme_load(scheme s, const char *file) {
    return pack_result(s.s7, s7_load(s.s7, file)); // TODO: eval in tighter env
}



scheme_result scheme_eval(scheme s, const char *buffer) {
    return pack_result(s.s7, s7_eval_c_string(s.s7, buffer));
}

void scheme_call1p(scheme s, const char *name, void *ptr){
    s7_pointer func = s7_name_to_value(s.s7, name);
    s7_call(s.s7, func, s7_cons(s.s7, s7_make_c_pointer(s.s7, ptr), s7_nil(s.s7)));
    return; // TODO: return value, check symbol lookup failure above
}

void scheme_free(scheme s){ 
#ifdef WITH_LINEAR_ALGEBRA
    vs_free(s.vs); 
#endif
    free(s.s7);
}

scheme scheme_init(void *unf_ctx){
    scheme s;
"
(foldMap (λ (x) (format "    s7_pointer ~a;" (u-symbol x))) import-modules) 
"
    s.s7 = s7_init();
#if defined(WITH_LINEAR_ALGEBRA) || defined(WITH_SHADER_LINK)
    s.vs = vs_init();
    s7_define_constant(s.s7, \"ffi-context-linear-algebra\", s7_make_c_pointer(s.s7,s.vs));
    s7_define_constant(s.s7, \"ffi-context-shader-link\", s7_make_c_pointer(s.s7,s.vs));
#endif
#ifdef WITH_UNIFORM
    s7_define_constant(s.s7, \"ffi-context-uniform\", s7_make_c_pointer(s.s7,unf_ctx));
#endif
"
(foldMap env-new import-modules)
"
    #define s7 s.s7
"
definitions
"
    #undef s7
"
(foldMap env-add import-modules)
"
    return s;
}
 
"))



(define (c-source input-files output-header-file)
  
  (define (C-if-def s file)
    (append
     (list (format "#ifdef WITH_~a"
                   (string-upcase
                    (C-symbol
                     (file->environment-name file)))))
     s
     (list "#endif")
     ))

  (define bodies
    (binding-export "" (list C-language-base s7-types C-language-s7-def) input-files))

  (define decls
    (binding-export "" (list C-language-base s7-types C-language-s7-decl-s7) input-files))
  
  (gen-c-src
   input-files
   output-header-file
   (string-join (flatten (map C-if-def bodies input-files)) "\n\n")
   (string-join (flatten (map C-if-def decls input-files)) "\n    ")))



(define (gen-s7-ffi)
  (define output-header-file
    (string-append (car output-base) ".h"))

  (define output-source-file
    (string-append (car output-base) ".c"))
  
  (call-with-output-file output-header-file
    #:exists 'truncate
    (curry display c-header))
  
  (call-with-output-file output-source-file
    #:exists 'truncate
    (curry display (c-source input-files output-header-file))))


;;;;;;;;;;;;;;;;;;;

(define-values (input-files output-base)
  (split-at-right (vector->list (current-command-line-arguments)) 1))

(if (> (length input-files) 0) (gen-s7-ffi) false)

