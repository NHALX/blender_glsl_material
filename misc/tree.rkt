#lang racket/base
(require racket/match)
(require "NHA.rkt")
(provide tree tree-value tree-children
         tree-map-edges tree-unfold)

#|
(define-type (Tree T) (Pairof T (Listof (Tree T))))

(: tree (∀ (a) (→ a
                  (Listof (Tree a))
                  (Tree a))))

(: tree-value (∀ (a) (→ (Tree a) a)))

(: tree-children (∀ (a) (→ (Tree a) (Listof (Tree a)))))

(: tree-map-edges (∀ (a b) (→ (→ a a b)
                         (Tree a)
                              (Listof b))))

(: tree-unfold (∀ (v n) (→ (→ n (values v (Listof n)))
                           n
                           (Listof v))))

|#

(define (tree x xs)
  (cons x xs))

(define (tree-value x)
  (car x))

(define (tree-children x)
  (cdr x))




(define (tree-map-edges f root)
  
  (define (g n)
    
    (values
     (map (λ (x)
            (f (tree-value n)
               (tree-value x)))
          (tree-children n))
     
     (tree-children n)))
  
  (apply append (tree-unfold g root)))



(define (tree-unfold f root)
  
  (define/match (t _)
    [((list)) (void)]
    [((cons x queue))

     (define-values (val next)
       (f x))
     
     (cons val
           (append queue next))])
  
  (unfold t (list root)))


(module* test racket/base
  (require (submod ".."))
  (require "NHA.rkt")
  (require rackunit)
    
  (define tree1
    (tree "1" (list
               (tree "2" (list '("3")))
               '("4"))))

  (check-equal?
   (tree-map-edges cons tree1)
   '(("1" . "2") ("1" . "4") ("2" . "3"))))

