#lang racket/base
(require racket/match racket/function racket/list)
(require "NHA.rkt")
(provide tree tree-map tree-value tree-children
         tree-find
         tree-edges tree-nodes tree-unfold)

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

(struct tree (value children) #:transparent)
#|
(define (tree x xs)
  (cons x xs))

(define (tree-value x)
  (car x))

(define (tree-children x)
  (cdr x))

|#

(define (tree-map f n)
  (tree (f (tree-value n))
        (map (⤶ tree-map f) (tree-children n))))
           

(define (tree-nodes root #:depth-first [dfs #f])
  (tree-unfold #:depth-first dfs
   (λ (x) (values (tree-value x)
                   (tree-children x))) root))


(define (tree-edges root)
  
  (define (g n)
    
    (values
     (map (λ (x)
            (cons (tree-value n)
                  (tree-value x)))
          (tree-children n))
     
     (tree-children n)))
  
  (apply append (tree-unfold g root)))



(define (tree-unfold f root #:depth-first [dfs #f])
  
  (define/match (t _)
    [((list)) (void)]
    [((cons x queue))

     (define-values (val next)
       (f x))
     
     (cons val
           (if dfs
               (append next queue)
               (append queue next)))])
  
  (unfold t (list root)))


(define (tree-find v root)
  (if (equal? v (tree-value root))
      root
      (for/or [[x (tree-children root)]]
        (tree-find v x))))


(module* test racket/base
  (require (submod ".."))
  (require "NHA.rkt")
  (require racket/list)
  (require rackunit)
    
  (define tree1
    (tree 1 (list
               (tree 2 (list (tree 3 empty)))
               (tree 4 empty))))

  (check-equal?
   (tree-find 2 tree1)
   (tree 2 (list (tree 3 empty))))
  
  (check-equal?
   (tree-edges tree1)
   '((1 . 2) (1 . 4) (2 . 3)))
  
  (check-equal?
   (tree-edges
    (tree-map add1 tree1))
   '((2 . 3) (2 . 5) (3 . 4)))

  (check-equal? (tree-nodes tree1 #:depth-first #t)
                (list 1 2 3 4))
  
  (check-equal? (tree-nodes tree1)
                (list 1 2 4 3)))

