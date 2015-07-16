#lang racket/base
(require (for-syntax racket/base))
(require "math-symbol.rkt")
(require scribble/text)

(provide (all-from-out "math-symbol.rkt")
         (all-from-out racket/base)
         (all-from-out scribble/text))

(provide __FILE__ __LINE__ emit-#line C export import import* source)


(define-syntax (__FILE__ stx)
  (with-syntax ([file (syntax-source stx)])
    (syntax-case stx ()
      [_ #'file])))

(define-syntax (__LINE__ stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-case stx ()
      [_ #'line])))

(define-syntax (import stx)
  (syntax-case stx ()
    ((_ m)
     (with-syntax
         ([name   (format "~a:"       (syntax->datum #'m))]
          [module (format "~a.cc.rkt" (syntax->datum #'m))]
          )
       #`(require (prefix-in
                   name
                   #,(datum->syntax stx `(submod ,#'module ,#'c-header))))))))

;undecorated version
(define-syntax (import* stx)
  (syntax-case stx ()
    ((_ m)
     (with-syntax ([name (format "~a:" (syntax->datum #'m))])
       #`(require (prefix-in
                   name
                   #,(datum->syntax stx `(submod ,#'m ,#'c-header))))))))


(define (c-line n f)
  "";(format "#line ~a \"~a\"\n" (+ 1 n) f)
  )

(define-syntax (emit-#line stx)
  (with-syntax ([line (syntax-line stx)]
                [file (syntax-source stx)])
    (syntax-case stx ()
      [_ #`(c-line line file)])))

(define C list)
#;
(define-syntax (C stx)
  (with-syntax ([line (syntax-line stx)]
                [file (syntax-source stx)])
    
    (syntax-case stx ()
      [(C . xs) (datum->syntax
                 stx
                 (append
                  `(string-append "\n" ,#`(c-line line file))
                  (syntax->list #`xs)) )])))

(define-syntax (source stx)
  (syntax-case stx ()
    ((_ toplevel body ...) 
     #`(module* main #f
       (require scribble/text)
       (require "../../misc/c-pre.rkt")
       (import* "..")
       toplevel
       (output (begin/text body ...))
       ))))


(define-syntax (export stx)
  (syntax-case stx ()
    ((_ init body ...) 
     #`(module c-header racket/base
       (require scribble/text)
       (provide header-source)
       (require "../../misc/c-pre.rkt")
       init
       (define header-source (begin/text body ...))))))
