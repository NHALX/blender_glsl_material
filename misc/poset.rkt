#lang racket/base
(provide (all-defined-out))
(require racket/match)
(require racket/list)
(require racket/set)
(require "NHA.rkt")
(require "multi-set.rkt")

#|
;; ╺┳╸╻ ╻┏━┓┏━╸┏━┓
;;  ┃ ┗┳┛┣━┛┣╸ ┗━┓
;;  ╹  ╹ ╹  ┗━╸┗━┛

(define-type (Chainof T)     (Listof T))   ; Totally ordered set.
(define-type (Order T)       (Pairof T T)) ; Order relation
(define-type (Adjacencies T) (Listof T))   
(define-type (Node T)        (Pairof T (Adjacencies T)))
(define-type (Posetof T)     (HashTable T (Adjacencies T)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: node             (∀ (a) (→ a (Adjacencies a) (Node a))))
(: node-value       (∀ (a) (→ (Node a) a)))
(: node-in-degree   (∀ (a) (→ (Node a) Nonnegative-Integer)))
(: node-adjacencies (∀ (a) (→ (Node a) (Adjacencies a))))
(: sort-nodes       (∀ (a) (→ (Listof (Node a))
                              (Listof (Node a)))))
|#

(define (node a b)
  (cons a b))

(define (node-adjacencies x)
  (cdr x))

(define (node-value x)
  (car x))

(define (node-in-degree x)
  (length (cdr x)))

(define (sort-nodes xs)
  (sort xs <
   #:cache-keys? #t
   #:key node-in-degree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(: ≼             (∀ (a) (→ a a (Order a))))
(: poset         (∀ (a) (→ (Listof (Order a)) (Posetof a))))
(: poset->chain  (∀ (a) (→ (Posetof a) (Chainof a)))) 
(: poset-empty?  (∀ (a) (→ (Posetof a) Boolean)))
(: poset-orders  (∀ (a) (→ (Posetof a) (Multi-Set (Order a)))))

(: group-arrows  (∀ (a) (→ (Listof (Order a))
                           (Listof (Pairof a (Adjacencies a))))))

(: drop-minimal  (∀ (a) (→ (Listof (Node a))
                           (values (Node a) (Listof (Node a))))))

|#

(define (≼ x y) ; ⪯ or ≺ or ≼ ?
  (cons x y))


(define (poset-empty? ps)
  (empty? ps))


(define (poset arrows)

  (define (arrows->objs xys)
    (match-define (cons x y) (unzip xys))
    (append x y))
  
  (define all-id 
    (list->set (map (λ (x) (≼ x x))
                    (arrows->objs arrows))))  
    
  (define missing-id
    (set->list
     (set-subtract all-id
                   (set-intersect (list->set arrows) all-id))))
  
  ((∘ make-immutable-hash group-arrows)
   (append missing-id arrows)))


(define (poset-orders ps)
  
  (define (f x) 
     (map (⤶ ≼ (node-value x)) (node-adjacencies x)))
  
  (list->multi-set
   (apply append
          (map f (hash->list ps)))))


(define (poset->chain objects)
  
  ; (: pop-min (→ (Listof (Node a)) (U Void (Pairof a (Listof (Node a))))))
  
  (define/match (pop-min ps)
    [((? empty?)) (void)]
    [(_)
     (define-values (x xs)
       (drop-minimal ps))
        
     (when (< 1 (length (cdr x)))
       (error "poset->chain: cycle detected."))
     ;(printf "hmm: ~a\n" (car x))
     ;(for-each displayln xs)
     (cons (car x) xs)])
  ; Since we only store the outgoing edges we need to operate on the dual
  ; category 
  (reverse (unfold pop-min (hash->list objects))))


; TODO: map macro
; TODO: reverse unfold?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; group-arrows: convert order relations
;; into a graph (adjacency list of outgoing edges)

(define (group-arrows lst)
  
  ;;(: deconstruct (→ (Listof (Order a)) (Pairof a (Listof a))))
  
  (define (deconstruct xs)
    (cons (caar xs)
          (map cdr xs)))

  (map deconstruct (equiv-classes (↫ : 2 equal? car) lst)))

(define (drop-minimal input)
  
  (match-define (cons dropped remaining)
    (sort-nodes input))
  
  (define (cull n)
    (node (node-value n)
          (filter-not (⤶ equal? (node-value dropped))
                      (node-adjacencies n))))
    
  (values dropped
          (map cull remaining)))



;; ╻ ╻┏┓╻╻╺┳╸   ╺┳╸┏━╸┏━┓╺┳╸
;; ┃ ┃┃┗┫┃ ┃ ╺━╸ ┃ ┣╸ ┗━┓ ┃ 
;; ┗━┛╹ ╹╹ ╹     ╹ ┗━╸┗━┛ ╹ 

(module* test racket/base
  (require (submod ".."))
  (require "NHA.rkt")
  (require "multi-set.rkt")
  (require rackunit)
  (require (only-in srfi/1 list-index))
  

  (define (ordered? ps chain)
    
    (define (test x)
      
      (define (index y)
        ;(printf "index:~a ~a\n" y chain)
        (list-index (⤶ equal? y) chain))
          
     #;
      (printf "~a(~a) <= ~a(~a)\n" (car x)  (index (car x)) (cdr x)
              (index (cdr x)))
      
      (<= (index (car x))
          (index (cdr x))))
    
    (andmap test (multi-set->list (poset-orders ps))))

  ;;╺┳┓┏━┓╺┳╸┏━┓
  ;; ┃┃┣━┫ ┃ ┣━┫
  ;;╺┻┛╹ ╹ ╹ ╹ ╹

  (define example-poset 
    (poset 
    ; (set 10 1 7 2 9 5 8 18 49 29)
     (list (≼ 1 2)  (≼ 1 5)  (≼ 5 7)   (≼ 7 8)   
           (≼ 8 9)  (≼ 9 10) (≼ 18 49) (≼ 29 49)
           (≼ 29 1) (≼ 8 18))))
  (display (poset->chain example-poset))  
  ;(for-each displayln example-poset)

  ;(poset-orders example-poset)
  

  ;; poset-orders

  (let [[xs (list (≼ 1 2) (≼ 1 5) (≼ 3 4)
                  (≼ 1 1) (≼ 2 2) (≼ 3 3)
                  (≼ 1 1) 
                  (≼ 4 4) (≼ 5 5))]]
    (check-equal?
     (poset-orders (poset xs))
     (list->multi-set xs)
     "poset-orders"))


  ;; group-arrows

  (check-equal?
   (group-arrows (list (≼ 1 2) (≼ 1 5) (≼ 3 4)))
   '((1 . (2 5))
     (3 . (4)))
   "group-arrows")

  ;; poset

  (check-equal?
   (poset (list (≼ 1 1) (≼ 1 2) (≼ 1 5) (≼ 3 4)))
   '#hash((2 . (2))
          (4 . (4))
          (5 . (5))
          (1 . (1 2 5))
          (3 . (3 4)))
         
   "poset")


  ;; poset->chain
  
  (check-true
   (ordered? example-poset
             (poset->chain example-poset))
   "poset->chain")

  )
