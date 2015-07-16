#lang racket
(require racket/stxparam)

(provide uncurry uncurry* → ← ∘ λx x map² map³)

(define ∘ compose)
(define ← curry)
(define → curryr)

(define (uncurry* f . arglist)
  (if (null? arglist) f
      (apply uncurry* (f (car arglist)) (cdr arglist))))

(define (uncurry f)
  (curry uncurry* f))




(define (map² f . xs)
  (apply map (λ x
         (apply map f x)) xs))

(define (map³ f . xs)
  (apply map (λ x
         (apply map (λ z
                (apply map f z)) x)) xs))

(define-syntax-parameter x #f)
(define-syntax (λx stx)
  (syntax-case stx () 
   [(_ body ...) #'(λ (arg)
                     (syntax-parameterize
                      ([x (make-rename-transformer #'arg)])
                      body ...))]))


(define (*** f g)
  (λ (x) (cons (f (car x)) 
               (g (cdr x)))))

(define (&&& f g)
  (λ (x) ((f *** g) x)))

