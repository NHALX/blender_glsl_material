#lang racket/base
(require racket/string
         racket/match
         racket/list
         racket/bool
         racket/set)

(require syntax/modresolve
         syntax/modcode)

(require "NHA.rkt")
(require "poset.rkt")
(require "tree.rkt")
(provide resolve-deps subset graph-map)
(require graph)
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

;; TODO: recursively collect subs of subs
(define (all-subs target-path)
    (let [[code (get-module-code target-path)]]
      (append (module-compiled-submodules code #f)
              (module-compiled-submodules code #t))))


(define (all-imports relative-path module-code)
  
  (define (get-imports code) 
    (concat 
     (map cdr 
          (module-compiled-imports code))))
  
  (define imports
    (map (⤷ resolve-module-path-index relative-path)
         (get-imports module-code)))
  
  #;(define all-imports
    (map (⤷ resolve-module-path-index target-path)
         (concat
          (map imports (all-subs target-path)))))

  
  #;(printf "for:~a -- subs: ~a\nall:~a\n"
          target-path
          (map module-compiled-name (all-subs target-path))          
          imports)
        
  (define/match (select x)
    [((? path?))                           x]
    [((? symbol?))                         #f]
    [((cons 'submod (cons (? path?) _)))   (cadr x)]
    [((cons 'submod (cons (? symbol?) _))) #f])

  (filter-not false? (map select imports)))



(define (resolve-deps pred path)

  (define root     (resolve-module-path path #f))
  (define visited  (mutable-set))
  (define graph    (directed-graph empty))
    
  (define (recurse! n)
    (define file   (simplify-path n))
    (define code   (get-module-code n))
    (define xs     (all-imports n code))
    (define ys
      (concat (map (⤶ all-imports n) (all-subs n))))

    (define-values (new old)
      (partition (λ (x) (not (set-member? visited x)))
                 (filter pred
                         (append xs ys))))
    
    #;(printf (string-append
             "target: ~a\n"
             "sub: ~a\n"
             "imports: ~a\n"
             "visited: ~a\n")
            n
            ys
            xs visited)
    
    (set-add! visited n)
    (for-each (λ (x) (add-directed-edge! graph file (simplify-path x)))
              (append new old))
    (for-each recurse! new))

  (recurse! root)
  graph)

(define (graph-map f graph)
  (define g2
    (graph-copy graph))
    
  (for-each (λ (v)
              (let [[v2 (f v)]]
                (unless (equal? v2 v)
                  (rename-vertex! g2 v (f v)))))
              (get-vertices g2))
  g2)

(define (subset graph src)
    (do-bfs graph src
                  #:init null
                  #:on-enqueue: (cons $v $acc)))



(module* test racket/base

  (require (submod ".."))
  (require "NHA.rkt")
  (require "tree.rkt")
  (require rackunit)
  (require racket/function racket/string racket/path)
  (require graph)


  
  (define cwd
    (path->string (current-directory)))
  
  (define pred 
    (let* {[suffix    (regexp-quote ".rkt")]
           [prefix    (regexp-quote cwd)]
           [criteria  (regexp (string-append prefix ".*" suffix))]}
      
      (λ (x) (regexp-match? criteria 
                            (path->string x)))))

  (define main-module
    "../render-osg/src/ShaderTest.cc.rkt")
  
  (define modules
    (resolve-deps pred main-module))

  (define edges
    (map
     (λ (x)
       (format "DirectedEdge[\"~a\", \"~a\"]"
               (car x)
               (cadr x)))
     (get-edges
      (graph-map (⤶ find-relative-path cwd)
                 modules))))

  (displayln
   (format "Graph[{~a}, VertexLabels -> \"Name\"]"
           (string-join edges ",\n")))
  
  (displayln
   (subset modules
            (string->path "/home/nha/data/blender_glsl_material/render-osg/src/BlenderMaterial.cc.rkt"))))
