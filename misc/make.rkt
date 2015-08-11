#lang racket/base
(require racket/port
         racket/path
         racket/undefined
         racket/match)

(require scribble/text)
(require "shell-pipe.rkt")
(require "dep-graph.rkt")
(require "../misc/NHA.rkt")
(require graph)

(provide gen-obj-file outdated? subsys module-deps S make 
         file-extension?
         file-in-subdir?
         exec exec-simulate
         cat
         (all-from-out graph)
         (all-from-out racket/base)
         (all-from-out racket/port)
         (all-from-out racket/path)
         (all-from-out scribble/text)
         (all-from-out "../misc/NHA.rkt"))

(define (gen-obj-file dir x)
  
  (define (sign-extension v)
    (if (< v 0)
        (format "1~X" (abs v))
        (format "0~X" (abs v))))
  
  ;; TODO: deal with hash collisions
  (full-filename
   (simplify-path
    (string->path
     (format "~a/~a-~a.~a.o"
             dir
             (sign-extension (equal-hash-code x))
             (sign-extension (equal-secondary-hash-code x)) 
             (file-name-from-path x))))))


#;
(define (unary-forest is os)
  (tree undefined
        (map (λ (i o) (tree i (list o))) is os)))


(define (newer? a b)
  (or
   (not (file-exists? b))
   (> (file-or-directory-modify-seconds a)
      (file-or-directory-modify-seconds b))))


(define/match (outdated? deps _)
  [(#f   (cons in out)) (newer? in out)]
  [(deps (cons in out))
   (define full-in (full-filename in))
   
   (printf "OUTDATED: ~a\n" in)
   (for-each
    (λ (i)
      (printf "(newer? ~a) = ~a\n" i (newer? i out)))
    (cons full-in
          (subset deps (full-filename in))))
   
   (ormap (λ (i) (newer? i out))
          (cons full-in
                (subset deps full-in)))])


(define S string-append)


(define (exec-simulate 
         #:in-fp [in #f]
         #:out-fp [out #f]
         #:exists [exists-flags 'error]
         . xs)
  (begin
    (displayln (string-join xs " | "))
    #t))

(define subsys
  (make-parameter exec-simulate))


(define full-filename
  path->complete-path)


(define (module-deps criteria m)
  (graph-map full-filename
       (resolve-deps criteria m)))


(define (make transform files #:dep-tree [deps #f])

  (define cwd
    (current-directory))
  
  (define (apply-cons f x)
    (f (car x) (cdr x)))
  
  (define-values (need skip)
    (partition (⤶ outdated? deps) files))
  
  
  (for-each (λ (x)
              (printf "[skipping]: ~a ⟹ ~a\n"
                      (find-relative-path cwd (car x))
                      (find-relative-path cwd (cdr x))))
            skip)
  
  (for-each (⤶ apply-cons transform) need))

 



(define (file-extension? suffix)
  (define criteria
    (regexp (S ".*" (regexp-quote suffix))))

  (λ (x) (regexp-match? criteria 
                        (if (path? x)
                            (path->string x)
                            x))))


(define (file-in-subdir? cwd)
  (let* {[prefix    (regexp-quote cwd)]
         [criteria  (regexp (string-append prefix ".*"))]}
      
    (λ (x) (regexp-match? criteria 
                          (if (path? x)
                              (path->string x)
                              x)))))

(define (cat a b c)
  (call-with-input-file a
    (λ (header)
      (call-with-input-file b
        (λ (input)
          (call-with-output-file #:exists 'replace c
            (λ (output)
              (write-bytes
               (bytes-append
                (port->bytes header)
                (port->bytes input))
               output))))))))
