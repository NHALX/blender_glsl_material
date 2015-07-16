#lang scribble/text
@(require racket/dict
          "../misc/c-pre.rkt"
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


@(define (c-transform-arg import)
   (define (pad x)
     (if (eq? x `r) " " ""))
   
   (replace-all
    (list `arg-ref rw1 rw2 type name)
      --> (string-append (c-access-mode rw1)
                         (pad rw1)
                         (c-type (type-find import type))
                         " * "
                         (c-access-mode rw2)
                         (pad rw2)
                         " "
                         (c-symbol name))
    
    (list `arg rw type name)
      --> (string-append (c-access-mode rw)
                         (pad rw)
                         (c-type (type-find import type))
                         " "
                         (c-symbol name))))
  
@(define (c-transform-function _)
  (define (function name xs)
    (define-values (args ret)
      (split-at-right xs 1))
     
    (string-append (car ret)
                   (c-symbol name)
                   "("
                   (string-join args ", ")
                   ");\n"))

  (replace-all        
   (list-rest `e-function name description xs)
     --> (function name (cons "void *ctx" xs))

   (list-rest `function name description xs)
     --> (function name xs)))



/*
┏━╸╻ ╻┏┓╻┏━╸   ╺┳┓┏━╸┏━╸╻  
┣╸ ┃ ┃┃┗┫┃  ╺━╸ ┃┃┣╸ ┃  ┃  
╹  ┗━┛╹ ╹┗━╸   ╺┻┛┗━╸┗━╸┗━╸
*/

@(map (λ (x)
         ((∘ (c-transform-function x)
             (c-transform-arg x))
            
          (dict-ref x `functions)))
      
      import-values)

#ifdef __cplusplus
}
#endif
