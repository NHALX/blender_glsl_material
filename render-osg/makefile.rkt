#lang at-exp s-exp "../misc/make.rkt"
(provide build-unit)

(define racket "/usr/local/bin/racket")
(define cc     "/usr/bin/gcc")
(define c++    "/usr/bin/g++")

(define includes "-I. -I../ -I../../osg/include/ -I../embed/s7/ -Isrc")
(define c-flags
  @S{@|includes| -ggdb -g3 -g -O0 -L../../osg/lib -L../obj/})
(define ld-flags
  @S{-L../../osg/lib -L../obj/ -Wl,-Bdynamic -losg -losgViewer 
                     -losgManipulator -losgGA -losgDB -losg 
                     -losgShadow -losgUtil -losgAnimation
                     -lzmq})

(define obj-dir "../obj/render-osg/")




(define files:hh⟹gch
  
  (let [[hhs     (list "precompile/OpenSceneGraph.hh")]
        [gen-out (∘ (⤶ build-path "src")
                    file-name-from-path
                    (⤷ path-replace-suffix ".hh.gch"))]]
    
    (map cons hhs (map gen-out hhs))))

 
(define (files:ccr⟹obj deps)
  ;; TODO: file-extension? filter doesnt handle when changes are made to c-pre.
  (let* [[pred  (file-extension? ".cc.rkt")]
         [xs    (filter pred
                        (reverse
                         (tsort deps)))]]
    
    (for-each (⤶ printf "wtf: ~a\n") xs)
    (map cons 
         xs 
         (map (⤶ gen-obj-file obj-dir) xs))))

(define files:glsl-in⟹glsl
  (let [[xs (filter (file-extension? ".in") 
                    (directory-list "test/" #:build? #t))]
        [gen-out (⤷ path-replace-suffix "")]]

    (map cons xs (map gen-out xs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (glsl-in⟹glsl in out)
  (printf "glsl-in⟹glsl: ~a → ~a\n" in out)
  (cat "test/header" in out)) ; TODO: error check




(define (hh⟹gch in out)
  ((subsys) @S{@|cc| -c @|c-flags| @|in| -o @|out|}))




(define (link obj-paths output)
  
  (define objs
    (map path->string obj-paths))
  
  (define command
    @S{@|c++| ../generated/s7-ffi-linear-algebra.c 
              ../generated/s7-ffi-uniform.c
              ../generated/s7-scheme.c               
              ../obj/embed/s7/s7.o               
              @|c-flags| @|ld-flags| @|(string-join objs)| -o @|output|})
  
  (when (findf (λ (x) (outdated? #f (cons x output))) objs)
    ((subsys) command))) ; TODO: return value check

(define (all root)
  (define deps
    (module-deps (file-in-subdir? "src/") root))
  
  (make glsl-in⟹glsl files:glsl-in⟹glsl)
  (make hh⟹gch       files:hh⟹gch)
  (make ccr⟹obj      (files:ccr⟹obj deps)
        #:dep-tree deps)
  
  (link (map cdr files:ccr⟹obj) "bind"))


(define (execute #:in-fp [in #f]
                 #:out-fp [out #f]
                 #:exists [exists-flag 'error]
                 . xs)

  (apply exec-simulate #:in-fp in #:out-fp out #:exists exists-flag xs)
  (apply exec #:in-fp in #:out-fp out #:exists exists-flag xs))


(define (build-unit file)
  (parameterize [[subsys execute]] (all file)))

(module+ test
    (parameterize [[subsys execute]]
      (all "src/Shadow.cc.rkt")
      #;(all "src/ShaderTest.cc.rkt")))


;(all)









