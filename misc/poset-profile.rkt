#lang racket

(require profile)
(require profile/render-graphviz)
(require contract-profile)

(require "poset.rkt")

(define example-poset 
  (poset 
   (list (≼ 1 2)  (≼ 1 5)  (≼ 5 7)   (≼ 7 8)
         (≼ 8 9)  (≼ 9 10) (≼ 18 49) (≼ 29 49)
         (≼ 29 1) (≼ 8 18))))

(define example-set
  (set 10 1 7 2 9))





(define (test-typed n)
  (repeat n (λ () (poset->chain example-poset))))


(define (repeat n f) 
  (for ((i (in-range n))) (f)))




(define test-main
  (thunk (begin
           (define n 1000000)
           (repeat 3 collect-garbage)
           (test-typed n)
           ;(repeat 3 collect-garbage)
           ;(test-untyped n)
           ;(repeat 100000 test-sub-faster)
           ;(repeat 100000 test-sub-slower)
           )))

(dump-memory-stats)
(repeat 3 collect-garbage)
(profile-thunk test-main)
(contract-profile-thunk test-main)
