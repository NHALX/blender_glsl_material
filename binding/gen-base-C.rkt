#lang racket
(provide C-language-base C-symbol)

(define (C-symbol x)
  (string-replace
   (string-replace x "-" "_")
   "*" "mul"))

(define C-language-base
  '(begin
     (define u-int          "int")
     (define u-uint         "unsigned int")
     (define u-float        "float")
     (define u-void         "void")
     
     (define (u-symbol x) (string-replace
                           (string-replace x "-" "_")
                           "*" "mul")) ;; TODO: use C-symbol
              
     (define r "const")
     (define w "")
     (define rw "")))
