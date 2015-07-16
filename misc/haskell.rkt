#lang typed/racket
(define-syntax (⤶ stx)
  (syntax-case stx ()
    ((_ f x) 
     #`(λ (y) (f x y)))))

(define-syntax (⤷ stx)
  (syntax-case stx ()
    ((_ f y) 
     #`(λ (x) (f x y)))))

(define-syntax nest
  (syntax-rules ()
    [(nest x f)         (f x)]
    [(nest x f fs ...)  (f (nest x fs ...))] ))

(define-syntax ∘
  (syntax-rules (: ->)
     [(∘ f ... : in-t -> out-t) 
      (λ ([x : in-t]) (nest x f ...))] ))
    

;;;;;;;;;;;;;;;;;;

(define (h [z : Number])
  (+ z 1))

(define (plus-1 [x : Number]) : Number
  (+ 1 x))

(define (minus-1 [x : Number]) : Number
  (- x 1))

((∘ h h h h : Number -> Number) 0)

((∘ plus-1 
    minus-1 
    plus-1
    (⤷ + 1)
    (⤶ - 1)
    (⤷ - 1)
    : Number -> Number) 0)
