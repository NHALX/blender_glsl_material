#lang racket/base
(require racket/string)
(require racket/match)
(require racket/list)
(require racket/bool)
(require syntax/modresolve)
(require syntax/modcode)

(require "NHA.rkt")
(require "poset.rkt")
(require "tree.rkt")
(provide resolve-deps)

#|
(define-type Submodule
  (Pairof (U Symbol Path) (Listof Symbol)))

(define-type FilenamePath
  (U Symbol 
     Path 
     (Pairof Symbol 
             (Pairof (U Symbol Path) 
                     (Listof Symbol)))))

(require/typed syntax/modresolve
  [resolve-module-path (-> Any Any FilenamePath)]
  
  [resolve-module-path-index (→ Module-Path-Index
                                (U Path False (→ Any))
                                FilenamePath)])

(require/typed syntax/modcode
               [get-module-code (-> Path Compiled-Module-Expression)])

(require/typed racket
               [flatten (All (a b) (-> a (Listof b)))]
               [module-compiled-submodules (-> Compiled-Module-Expression
                                               Boolean
                                               (Listof
                                                Compiled-Module-Expression))])

(define-type Imports 
  (Listof Module-Path-Index))

(define-type PhaseImports
  (Pairof (U Integer False) Imports))


(: all-imports (-> Path
                   (Listof Path)))

(: resolve-deps (→ (→ Path Boolean) String (Chainof Path)))

(: concat (All (a) (-> (Listof (Listof a))
                       (Listof a))))

(: unlines (-> (Listof String) String))

|#

;; TODO: move these
(define (concat xs)
  (apply append xs))

(define (unlines xs)
  (string-join xs "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (all-imports target-path)
  
  (define (imports code) 
    (concat 
     (map cdr 
          (module-compiled-imports code))))
  
  (define all-subs
    (let [[code (get-module-code target-path)]]
      (append (module-compiled-submodules code #f)
              (module-compiled-submodules code #t))))
    
  (define all-imports
    (map (⤷ resolve-module-path-index target-path)
         (concat
          (map imports all-subs))))

  #|
  (printf "for:~a -- subs: ~a\nall:~a\n"
          target-path
          (map module-compiled-name all-subs)          
          all-imports)
  |#
  
  (define (not-this x)
    (if (equal? x target-path) #f x))
  
  (define/match (adjust x)
    [((? path?))                           (not-this x)]
    [((? symbol?))                         #f]
    [((cons 'submod (cons (? path?) _)))   (not-this (cadr x))]
    [((cons 'submod (cons (? symbol?) _))) #f])

  (filter-not false? (map adjust all-imports)))



(define (resolve-deps pred path)
  
  (define root
    (resolve-module-path path #f))

  (define (order n) 
    
    (define xs 
      (filter pred (all-imports n)))
    ;;(printf "target: ~a\nchildren: ~a\n" n xs)
    
    
    (values (map (λ (x) (≼ x n)) xs)
            xs)) 

  ((∘ poset->chain
      poset
      concat)
   (tree-unfold order root)))







(module* test racket/base
  (require (submod ".."))
  (require "NHA.rkt")
  (require "tree.rkt")
  (require rackunit)

  (define pred 
    (let* {[suffix    (regexp-quote ".cc.rkt")]
           [criteria  (regexp (string-append ".*" suffix))]}
      
      (λ (x) (regexp-match? criteria 
                            (path->string x)))))

  (define main-module
    "data/blender_glsl_material/render-osg/src/ShaderTest.cc.rkt")
  
  (define modules
    (resolve-deps pred main-module))

  (for-each displayln modules))
