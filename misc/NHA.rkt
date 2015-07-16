#lang racket/base
(require racket/match)
(require racket/list)

(require (for-syntax racket/base))
(provide (all-defined-out))


(define-for-syntax (generate-n-temporaries stx)
  (generate-temporaries (build-list (syntax->datum stx) (λ (i) stx))))

(define-syntax nest
  (syntax-rules ()
    [(nest x f)         (f x)]
    [(nest x f fs ...)  (f (nest x fs ...))] ))

;;;;;;;;;;;

;; projection: (↫ equal? car 2) ---> (λ (x y) (equal? (car x) (car y)))

(define-syntax (↫ stx)
  (syntax-case stx (:)
    [(↫ : n f g)
     (with-syntax ([(xs ...)
                    (generate-n-temporaries #'n)])
       
       (let [[args  (map (λ (x) #`#,x)
                         ; for typed racket insert type annotation around #,x here
                         (syntax->list #'(xs ...)))]
             
             [body  (cons #'f (map (λ (x) #`(g #,x))
                                   (syntax->list #'(xs ...))))]]
         
         #`#,(list #'λ args body)))]
    ))



;; left curry | (⤶      f x) --> (λ (y) (f x y))
;;            | (⤶ : T  f x) --> (λ ([y : T]) (f x y))

(define-syntax (⤶ stx)
  (syntax-case stx (:)
    [(_ : in-t f x) 
     #`(λ ([y : in-t]) (f x y))]
    [(_ f x) 
     #`(λ (y) (f x y))]))


;; right curry | (⤷      f y) --> (λ (x) (f x y))
;;             | (⤷ : T  f y) --> (λ ([x : T]) (f x y))

(define-syntax (⤷ stx)
  (syntax-case stx (:)
    [(_ : in-t f y) 
     #`(λ ([x : in-t]) (f x y))]
    [(_ f y) 
     #`(λ (x) (f x y))] ))




;; function composition | (∘ f g h)            --> (λ (x) (f (g (h x))))
;;                      | (∘ : T1 → T2  f g h) --> (λ ([x : T1]) : T2
;;                                                             (f (g (h x))))

(define-syntax ∘
  (syntax-rules (: →)
    [(∘ : in-t → out-t f ...) 
     (λ ([x : in-t]) : out-t (nest x f ...))]
    
    [(∘ f ...) 
     (λ (x) (nest x f ...))]))



(define (map² f . xs)
  (apply map (λ x
               (apply map f x)) xs))

(define (map³ f . xs)
  (apply map (λ x
               (apply map (λ z
                            (apply map f z)) x)) xs))


;; similar to the haskell version: unfoldr

(define (unfold f seed [tail-gen (lambda (x) '())])
  (let [[x (f seed)]]
    (if (void? x)
        (tail-gen seed)
        (cons (car x)
              (unfold f (cdr x))))))


;; (equiv-classes equal? '(1 1 1 3 4 5)) --> '((1 1 1)  (3)  (4)  (5)))

(define (equiv-classes cmp lst)
  
  (define/match (split xs)

    [((list))
     (void)]
    
    [((cons target _))
     
     (define-values (eqv un)
       (partition (⤶ cmp target) xs))
     
     (cons eqv un)])
  
  
  (unfold split lst))



;; unzip: '((1 . a) (2 . b)) --> ((1 2) . (a b))

(define (unzip lst)
  
  (match lst
    ['()                  (cons '() '())]
    [(cons (cons a b) tl) (define xs (unzip tl))
     (cons (cons a (car xs))
           (cons b (cdr xs)))]))



(module* test racket/base
  (require (submod ".."))
  (require typed/rackunit)

  (define (unfold-test n)
    (if (= n 10)
        (void)
        (cons n (+ 1 n))))
  
  (check-equal?
   (unfold unfold-test 0)
   '(0 1 2 3 4 5 6 7 8 9)
   "unfold")
  
  (check-equal?
   (unzip '((1 . a) (2 . b)))
   '((1 2) . (a b))
   "unzip")
  
  (check-equal?
   (equiv-classes equal? '(1 1 1 2 3))
   '((1 1 1) (2) (3))
   "equiv-classes")


  (define (deconstruct xs)
    (cons (caar xs)
          (map cdr xs)))

  (check-equal?
   (map deconstruct
        (equiv-classes
         ;; TODO: how can this be more useful? lets say i want to project car/dr to equal?
         (↫ : 2  equal? car)
   
         (list (cons 1 2)
               (cons 1 5)
               (cons 3 4))))
   
   '((1 2 5) (3 4))
   "equiv-classes"))
