;#lang racket
#lang at-exp racket
(require scribble/text racket/list)

(require "gen-common.rkt")
(require "gen-base-C.rkt")

(define (u-symbol x)
   (string-replace x "-" "_"))

(define external-types '(list u-m44 u-v4 u-v3))

(define s7-types
  '(begin    
     (define u-bool         "bool")
     (define u-m44          "m44_t")
     (define u-v4           "v4_t")
     (define u-v3           "v3_t")
     (define u-string       "string")
     (define u-int          "integer")
     (define u-uint         "ulong")
     (define u-float        "real")
     (define u-vector-stack "struct vector_stack")
     
     (define/match (type-make x)
       [((== u-v4))            "s7_make_object(sc,v4_type_id,~a)"]
       [((== u-v3))            "s7_make_object(sc,v3_type_id,~a)"] 
       [((== u-m44))           "s7_make_object(sc,m44_type_id,~a)"]
       [((== u-bool))          "s7_make_boolean(sc,~a)"]
       [(x)            (format "s7_make_~a(sc,~~a)" x)])

     (define/match (type-get x)
       [((== u-v4))            "s7_object_value_safe(v4_type_id,~a)"]
       [((== u-v3))            "s7_object_value_safe(v3_type_id,~a)"]
       [((== u-m44))           "s7_object_value_safe(m44_type_id,~a)"]
       [((== u-bool))          "s7_boolean(sc,~a)"]
       [(x)            (format "s7_~a_safe(sc,~~a)" x)]) ))


(define post
  '(lambda (xs)
     (define (build-get/set TYPE)
       @string-append{
          
          int scheme_set_@TYPE(scheme s, const char *name, @|TYPE| v)
          {
              s7_pointer sym = s7_symbol_table_find_name(s.s7, name);
              if (sym) {
                  s7_symbol_set_value(s.s7, sym, @| (format (type-make TYPE) "v") |);
                  return 0;
              } else {
                  printf("scheme_set_@|TYPE|: warning: symbol '%s' not found.\n", name);
                  return -1;
              }    
          }

          int scheme_get_@|TYPE|(scheme s, const char *name, @|TYPE| *result){
             s7_pointer sym = s7_symbol_table_find_name(s.s7, name);
             if (sym) {
                 *result = @| (format (type-get TYPE) "s7_symbol_value(s.s7, sym)") |;
                 return 0;
             } else {
                 printf("scheme_get_@|TYPE|: warning: symbol '%s' not found.\n", name);
                 return -1;
             }    
          }
          
       })

    (map (curry map build-get/set) xs)))


(display (binding-export "" (list s7-types) (list "test.scm") post))


;;;;;           ;;;;;
;;;;; ffi wraps ;;;;;
;;;;;           ;;;;;

(define rules-ffi-wrappers
  '(begin
     
     (define (flip f)
       (λ (a b) (f b a)))
     
     (define lookup
       (curry (flip dict-ref)))

     ;;;;;;;;;;;;;;;;;;;;;
     
     (define ffi-context
       (format "s7_c_pointer(s7_name_to_value(sc, \"ffi-context-~a\"))" export-environment))
         
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
       `[(type . ,t)
         (name . ,(u-symbol name))
         (make . ,(format (type-make t) "result"))
         (read . ,(format "~a ~a = (~a) ~a"
                          t
                          (u-symbol name)
                          t
                          (format (type-get t) "next_arg(&args)")))])))



;;;;;           ;;;;;
;;;;; s7_define ;;;;;
;;;;;           ;;;;;

(define rules-defs
  '(begin
     
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
     
          
     (define (u-e-function x description . xs)
       (s7-deffun x description (- (length xs) 1) "false"))
     
     (define u-function
       u-e-function)
     
     (define (u-arg rw t name)
       name)
     
     (define (u-arg-ref rw1 rw2 t name)
       name) ))




; (define monoid-str "" (λ (a b) (string-join (list a b) "\n") ))

(define (foldMap f xs) (string-join (map f xs) "\n"))


(define (gen-scheme-init s7-defines)
    
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
scheme scheme_init(void *alg_ctx, void *unf_ctx){
    scheme s;
"
(foldMap (λ (x) (format "    s7_pointer ~a;" (u-symbol x))) import-modules) 
"
    s.s7 = s7_init();
#ifdef WITH_LINEAR_ALGEBRA
    m44_type_id = s7_new_type(\"m44\", _m44_print, _m44_free, _m44_equal, NULL, NULL, NULL);
    v4_type_id  = s7_new_type(\"v4\", _v4_print, _v4_free, _v4_equal, NULL, NULL, NULL);
    v3_type_id  = s7_new_type(\"v3\", _v3_print, _v3_free, _v3_equal, NULL, NULL, NULL);

    s7_define_constant(s.s7, \"ffi-context-linear-algebra\", s7_make_c_pointer(s.s7,alg_ctx));
#endif
#ifdef WITH_SHADER_LINK
    s7_define_constant(s.s7, \"ffi-context-shader-link\", s7_make_c_pointer(s.s7,alg_ctx));
#endif
#ifdef WITH_UNIFORM
    s7_define_constant(s.s7, \"ffi-context-uniform\", s7_make_c_pointer(s.s7,unf_ctx));
#endif
"
(foldMap env-new import-modules)
"
    #define s7 s.s7
"
s7-defines
"
    #undef s7
"
(foldMap env-add import-modules)
"
    return s;
}
 
"
))







(define (gen-s7-ffi)

  (define output-source-file
    (string-append (car output-base) ".c")) 

  (define (generate-source input-files)    
    (define (C-if-def s file)
      (append
       (list (format "#ifdef WITH_~a"
                     (string-upcase
                      (C-symbol
                       (file->environment-name file)))))
       s
       (list "#endif")
       ))

    (define (term-rewrite x)
      (binding-export "" (list C-language-base s7-types x) input-files))
    
    (define (C-if-def-wrap-all x)
      (string-join (flatten (map C-if-def x input-files)) "\n\n"))
    
    (define (rr x)
      (C-if-def-wrap-all (term-rewrite x)))
    
    (string-append
     "/* auto-generated by gen-s7-ffi.rkt */\n"
     "#ifdef WITH_LINEAR_ALGEBRA\n"
     "typedef void* m44_t;\n"
     "typedef void* v4_t;\n"
     "typedef void* v3_t;\n"
     "static int m44_type_id = 0;\n"
     "static int v4_type_id  = 0;\n"
     "static int v3_type_id  = 0;\n"
     "#endif\n"
     
     (foldMap
      (λ (x)
        (format "#ifdef WITH_~a\n#include \"generated/~a.h\"\n#endif\n"
                (string-upcase (u-symbol x))
                x))
 
      import-modules)
        
     (rr rules-ffi-wrappers)
     
     (gen-scheme-init (rr rules-defs))))

  
  (call-with-output-file output-source-file
    #:exists 'truncate
    (curry display (generate-source input-files))))


;;;;;;;;; main ;;;;;;;;;;

(define-values (input-files output-base)
  (split-at-right (vector->list (current-command-line-arguments)) 1))

(define import-modules
  (map file->environment-name input-files))

(if (> (length input-files) 0)
    (gen-s7-ffi)
    false)

