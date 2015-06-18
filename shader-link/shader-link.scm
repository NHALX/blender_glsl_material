;;apply varlet (curlet) linear-algebra)

;;Current implementation of orientation-matrix system in Blender is not reliable: there are limitations / some nasty problems with mirrored/non-uniform-scaled geometry in Blender
;; see http://wiki.blender.org/index.php/User:Migius/orientation_matrix

(define (lamp-dyn-energy neg x)
  (if neg (- x) x))

(define (lamp-shadow-bias x) ;; TODO: this isnt being export atm...
  (* 0.02 x 0.25))

(define (lamp-spotsize x)
  (define Degree
    (/ pi 180.0))
  ;(cos (* 0.5 (min (* 170.0 Degree) x)))
  (cos (* 0.5 x)))
                                        ; TODO: if (lamp->mode & LA_HALO) only

(define (bool-to-float x)
  (if x 1.0 0.0))

(define (mist-type x)
  (cond
   ((string=? x "QUADRATIC") 0.0) 
   ((string=? x "LINEAR") 1.0)
   ((string=? x "INVERSE_QUADRATIC") 2.0)))

;; TODO: submit patch where duplicate spotsize is generated instead of spotblend.
;; ./source/blender/gpu/intern/gpu_material.c:580

(define (lamp-spotblend size blend) 
  (* blend (- 1 (lamp-spotsize size)))) 

;; NOTE: world-to-lamp and lamp-to-world:
;; the 3x4 rotation sub matrix is assumed to be
;; column-vector normalized.
;; TODO: make this happen
(define (lamp-imat world-to-lamp cam-to-world)
  (m44* world-to-lamp cam-to-world))
  ;(m44* (m44-invert lamp-to-world) cam-to-world)

(define (lamp-dynco lamp-to-world world-to-cam)
  (define location (m44-col lamp-to-world 3))
  ;(v4-print (m44*-v3 world-to-cam location))
  (m44*-v3 world-to-cam location))

(define (lamp-dynvec lamp-to-world world-to-cam)
  (define rot (v3-negate (v3-normalize (m44-col lamp-to-world 2))))
;   (v4-print rot)
;;  (m44-print world-to-cam)
                                        ; (v4-print (m44-col lamp-to-world 2))
  ;(v4-print rot)
  ;(m44-print world-to-cam)
  ;(v4-print (m44*-v3 world-to-cam rot))
 ; (display material-specular-hardness)
  (v3-normalize (m33*-v3 world-to-cam rot))) ;; can't pad vec with 1

(define (frustum left right bottom top zNear zFar)
  
  (define A (/ (+ right left) 
               (- right left)))
  
  (define B (/ (+ top bottom) 
               (- top bottom)))
  
  (define C (- (/ (+ zFar zNear) 
                  (- zFar zNear))))
  
  (define D (- (/ (* 2.0 zFar zNear) 
                  (- zFar zNear))))
  
  (define u (/ (* 2.0 zNear) 
               (- right left)))
  
  (define v (/ (* 2.0 zNear) 
               (- top bottom)))
  
  (m44 u   0.0  A   0.0
       0.0 v    B   0.0
       0.0 0.0  C   D
       0.0 0.0 -1.0 0.0))

;; TODO: ortho sun lamp
(define (lamp-to-perspective spotsize clip0 clip1)
  (define w (* clip0 (tan (/ spotsize 2))))
  (frustum (- w) w (- w) w clip0 clip1))


(define perspective-to-depth (m44 0.5 0.0 0.0 0.5
                                  0.0 0.5 0.0 0.5
                                  0.0 0.0 0.5 0.5
                                  0.0 0.0 0.0 1.0))

(define (lamp-perspective-matrix spot-size clip-start clip-end world-to-lamp cam-to-world)
  
  (m44* perspective-to-depth
        (lamp-to-perspective spot-size clip-start clip-end)
        world-to-lamp
        cam-to-world))


;(define (uniform-generic type name val) (display (list type name val "\n")))

;(define (uniform-1i n v) (uniform-generic "li" n v))
;(define (uniform-1fv n v) (uniform-generic "1fv" n v))
;(define (uniform-3fv n v) (uniform-generic "3fv" n v))
;(define (uniform-4fv n v) (uniform-generic "4fv" n v))
;(define (uniform-Matrix4fv n v) (uniform-generic "Matrix4fv" n v))


;(display (lamp-perspective-matrix 25 1.0 100.0 test test))


;(load "test/material.scm")



