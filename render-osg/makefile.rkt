#lang at-exp s-exp "../misc/make.rkt"


(define racket "/usr/local/bin/racket")
(define cc     "/usr/bin/gcc")
(define c++    "/usr/bin/g++")

(define includes "-I../ -I../../osg/include/ -I../embed/s7/ -Isrc")
(define c-flags
  @S{@|includes| -ggdb -g3 -g -O0 -L../../osg/lib -L../obj/})
(define ld-flags
  @S{-L../../osg/lib -L../obj/ -Wl,-Bdynamic -losg -losgViewer 
                     -losgManipulator -losgGA -losgDB -losg 
                     -losgShadow -losgUtil})

(define obj-dir "../obj/render-osg/")

(define main-module "src/ShaderTest.cc.rkt")

(define files:hh⟹gch
  
  (let [[hhs     (list "precompile/OpenSceneGraph.hh")]
        [gen-out (∘ path->string
                    (⤶ build-path "src")
                    file-name-from-path
                    (⤷ path-replace-suffix ".hh.gch"))]]
    
    (map cons hhs (map gen-out hhs))))


(define files:ccr⟹obj
  (let [[xs (module-deps ext-ccr? main-module)]]
    (map cons xs (map (⤶ gen-obj-file obj-dir) xs))))

(define files:glsl-in⟹glsl
  (let [[xs (list "test/material.vert.in"
                  "test/material.frag.in")]
        [gen-out (⤷ path-replace-suffix "")]]
    (map cons xs (map gen-out xs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (glsl-in⟹glsl in out)
  (printf "glsl-in⟹glsl: ~a → ~a\n" in out)
  (cat "test/header" in out)) ; TODO: error check




(define (hh⟹gch in out)
  ((subsys) @S{@|cc| -c @|c-flags| @|in| -o @|out|}))

(define (ccr⟹obj in out)
  
  (define gen-c
    @S{@|racket| @|in|})
  
  (define commands
    (list gen-c
          @S{@|c++| -xc++ -c @|c-flags| -o @|out| -}))
      
  (unless (apply (subsys) commands)
    (begin
      ((subsys) gen-c)
      (error 'ccr⟹obj "external process signaled failure.")))

  (void)) 


(define (link objs output)
  (define command
    @S{@|c++| ../generated/s7-ffi-linear-algebra.c 
              ../generated/s7-ffi-uniform.c
              ../generated/s7-scheme.c               
              ../obj/embed/s7/s7.o               
              @|c-flags| @|ld-flags| @|(string-join objs)| -o @|output|})
  (when (findf (λ (x) (outdated? (cons x output))) objs)
    ((subsys) command))) ; TODO: return value check

(define (all)
  (make glsl-in⟹glsl files:glsl-in⟹glsl)
  (make hh⟹gch       files:hh⟹gch)
  (make ccr⟹obj      files:ccr⟹obj)
  (link (map cdr files:ccr⟹obj) "bind"))


(define (execute . xs)
  (apply exec-simulate xs)
  (apply exec xs))



(parameterize [[subsys execute]] (all))

;(all)








