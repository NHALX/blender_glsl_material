#lang racket/base
(require "NHA.rkt")
(provide multi-set
         list->multi-set
         multi-set->list)

#|
;; ╺┳╸╻ ╻┏━┓┏━╸┏━┓
;;  ┃ ┗┳┛┣━┛┣╸ ┗━┓
;;  ╹  ╹ ╹  ┗━╸┗━┛

(define-type (Multi-Set T) (HashTable T Index))


(: multi-set       (∀ (a) (→ (Pairof a Index) *
                             (Multi-Set a))))

(: list->multi-set (∀ (a) (→ (Listof a)
                             (Multi-Set a))))

(: multi-set->list (∀ (a) (→ (Multi-Set a)
                             (Listof a))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list->multi-set xs)
  (make-immutable-hash   
   (map (λ (xs)
          (cons (car xs)
                (length xs)))
        
        (equiv-classes equal? xs))))


(define (multi-set->list set)
  (apply append
         (hash-map set (λ (k v)
                         (build-list v (λ (x) k))))))

(define (multi-set . xs)
  (make-immutable-hash xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ╻ ╻┏┓╻╻╺┳╸   ╺┳╸┏━╸┏━┓╺┳╸
;; ┃ ┃┃┗┫┃ ┃ ╺━╸ ┃ ┣╸ ┗━┓ ┃ 
;; ┗━┛╹ ╹╹ ╹     ╹ ┗━╸┗━┛ ╹

(module* test racket/base
  (require (submod ".."))
  (require rackunit)
  (require "NHA.rkt")
  
  (check-equal?
   (multi-set->list (list->multi-set '(0 1 1 1 2 3)))
   '(0 1 1 1 2 3)
   "multi-set->list"))


