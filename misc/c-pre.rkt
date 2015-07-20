#lang racket/base
(require (for-syntax racket/base))
;(require "math-symbol.rkt")
(require "NHA.rkt")
(require "c-pre-stdlib.rkt")
(require scribble/text)
(require racket/function)

(provide (all-from-out "NHA.rkt")
         (all-from-out racket/base racket/function)
         (all-from-out scribble/text)
         (all-from-out "c-pre-stdlib.rkt"))

(provide __FILE__ __LINE__ emit-#line C export import import* source 
         hash:__FILE__:__LINE__)


(define-syntax (__FILE__ stx)
  (with-syntax ([file (syntax-source stx)])
    (syntax-case stx ()
      [_ #''file])))

(define-syntax (__LINE__ stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-case stx ()
      [_ #'line])))

(define-syntax (hash:__FILE__:__LINE__ stx)
    (syntax-case stx ()
      [(_ prefix) #`(let [[h (equal-hash-code  
                              (format "~a:~a"
                                      #,(syntax-source stx)
                                      #,(syntax-line stx)))]]
             (if (< h 0 )
                 (string-append prefix "1" (number->string (abs h)))
                 (string-append prefix "0" (number->string h))))]))


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
