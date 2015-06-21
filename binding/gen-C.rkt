#lang racket
(require "gen-common.rkt")
(require "gen-base-C.rkt")

;; TODO: delete float real?
(define C-pre "
#ifdef __cplusplus
extern \"C\" {
#endif

typedef float real;
struct vector_stack;
")

(define C-post "
#ifdef __cplusplus
}
#endif
")


(define C-types
  '(begin
     (define u-bool    "bool")
     (define u-m44     "m44_t")
     (define u-v4      "v4_t")
     (define u-v3      "v3_t")
     (define u-string  "char *")
     (define u-v-stack "struct vector_stack") ))


(define C-language-decl
  '(begin
     
     (define (pad x)
       (if (eq? x "") "" " "))

     ;;;;;;;;;;;;;
     
     (define (function-decl name xs)
       (define-values (args ret) (split-at-right xs 1))
       
       (format "~a ~a~a(~a);"
               (car ret)
               c-symbol-prefix
               (u-symbol name)
               (string-join args ", ")))
     
     
     (define (u-e-function name description . xs)
       (function-decl name (cons "void *ctx" xs)))

     (define (u-function name description . xs)
       (function-decl name xs))


     (define (u-arg-ref rw1 rw2 type name)
       (string-join (list rw1
                          (pad rw1)
                          type
                          " * "
                          rw2
                          (pad rw2)
                          (pad name)
                          (u-symbol name)) ""))
     
     (define (u-arg rw type name)
       (string-join (list rw
                          (pad rw)
                          type
                          (pad name)
                          (u-symbol name)) "")) ))


(define input-files (vector->list (current-command-line-arguments)))
(define C-environment (list C-types C-language-base C-language-decl))

(if (> (length input-files) 0)
    
    (for-each
     (λ (x) (printf "~a\n" x))
     (append
      (list C-pre)
      (flatten (binding-export "" C-environment input-files))
      (list C-post)))

    false)

