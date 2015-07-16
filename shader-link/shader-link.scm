
;; ╻ ╻╺┳╸╻╻  
;; ┃ ┃ ┃ ┃┃  
;; ┗━┛ ╹ ╹┗━╸

(define (foldl fn accum list)
    (if (null? list)
        accum
        (foldl fn
              (fn accum (car list))
              (cdr list))))


(define (m44-product . xs)
  (foldl m44* (car xs) (cdr xs)))

;;apply varlet (curlet) linear-algebra)
(define (m44-col-v3 m i)
  (v3-v4 (m44-col m i)))



;; ┏┓ ╻  ┏━╸┏┓╻╺┳┓┏━╸┏━┓
;; ┣┻┓┃  ┣╸ ┃┗┫ ┃┃┣╸ ┣┳┛
;; ┗━┛┗━╸┗━╸╹ ╹╺┻┛┗━╸╹┗╸

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

(define (lamp-dynco lamp-to-world world-to-cam)
  (define location (m44-col-v3 lamp-to-world 3))
  (m44*-v3 world-to-cam location))

(define (lamp-dynvec lamp-to-world world-to-cam)
  (define rot (v3-negate (v3-normalize (m44-col-v3 lamp-to-world 2))))
  (v3-normalize (m33*-v3 world-to-cam rot))) ;; must use 3x3 sub matrix


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



(define (lamp-perspective-matrix spot-size
                                 clip-start
                                 clip-end
                                 world-to-lamp
                                 cam-to-world)
 
  (m44-product perspective-to-depth
               (lamp-to-perspective spot-size clip-start clip-end)
               world-to-lamp
               cam-to-world))



;; ╻ ╻┏┓╻╻╺┳╸   ╺┳╸┏━╸┏━┓╺┳╸
;; ┃ ┃┃┗┫┃ ┃ ╺━╸ ┃ ┣╸ ┗━┓ ┃ 
;; ┗━┛╹ ╹╹ ╹     ╹ ┗━╸┗━┛ ╹

(define (unit-test:shader-link)
    
  (define lamp-obmat 
    (m44 0.99281394481658936 0.063824214041233063 -0.10122808814048767 10
         -0.072077855467796326 0.99417835474014282 -0.080088801681995392 0
         0.09552716463804245 0.086809568107128143 0.99163442850112915 32
         0 0 0 1))

  (define lamp-viewmat 
    (m44 0.99281388521194458 -0.072077862918376923 0.095527172088623047 -12.985008239746094
         0.063824214041233063 0.99417835474014282 0.08680957555770874 -3.4161486625671387
         -0.10122808814048767 -0.080088809132575989 0.99163436889648438 -30.72001838684082
         0 0 0 1))

  (define lamp-winmat 
    (m44 2.6050894260406494 0 0 0
         0 2.6050894260406494 0 0
         0 0 -1.069017767906189 -2.0706708431243896
         0 0 -1 0))

  (define lamp-winmat*lamp-viewmat 
    (m44 2.5863690376281738 -0.18776927888393402 0.24885682761669159 -33.827106475830078
         0.16626778244972229 2.589923620223999 0.22614671289920807 -8.8993730545043945
         0.10821462422609329 0.085616357624530792 -1.0600748062133789 30.769573211669922
         0.10122808814048767 0.080088809132575989 -0.99163436889648438 30.72001838684082))

  (define prep-to-depth*lamp-winmat*lamp-viewmat* 
    (m44 1.3437985181808472 -0.053840234875679016 -0.3713887631893158 -1.5535440444946289
         0.13374793529510498 1.3350062370300293 -0.38274383544921875 10.910322189331055
         0.10472135245800018 0.082852587103843689 -1.0258545875549316 30.744796752929688
         0.10122808814048767 0.080088809132575989 -0.99163436889648438 30.72001838684082))
  (define view-mat 
    (m44 -0.2588191032409668 0.96592581272125244 1.0187646637405123e-07 -2.6909308433532715
         -0.24999995529651642 -0.066987417638301849 0.96592587232589722 -11.329010009765625
         0.93301272392272949 0.25000002980232239 0.25881901383399963 -1.1920928955078125e-06
         0 0 0 1))

  
  (define spot-size 0.7330382466316223)
  (define z-start   1.0007989406585693)
  (define z-end     30.00200080871582)

  (define identity
    (m44 1 0 0 0
         0 1 0 0
         0 0 1 0
         0 0 0 1))

  (define (test-error msg a b)
    (if (not (m44-eqf a b 0.00001))
        (begin
          (display "<<<")
          (display msg)
          (display ">>>")
          (newline)
          (display a)
          (display b)
          (newline)
          1)
        
          0))
  
  (foldl + 0
         (list
          (test-error "lamp-to-perspective"
                      lamp-winmat
                      (lamp-to-perspective spot-size z-start z-end ))
          
          (test-error "lamp-perspective-matrix"
                      prep-to-depth*lamp-winmat*lamp-viewmat*
                      (lamp-perspective-matrix spot-size z-start z-end
                                               lamp-viewmat identity)))))


