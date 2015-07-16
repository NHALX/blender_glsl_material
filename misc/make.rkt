#lang racket/base
(require racket/path)
(require racket/port)
(require scribble/text)
(require "shell-pipe.rkt")
(require "dep-graph.rkt")
(require "../misc/NHA.rkt")

(provide gen-obj-file outdated? subsys module-deps S make ext-ccr?
         exec exec-simulate
         cat
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
  (format "~a/~a-~a.~a.o"
          dir
          (sign-extension (equal-hash-code x))
          (sign-extension (equal-secondary-hash-code x)) 
          (file-name-from-path x)))


(define (outdated? x)
  (or
   (not (file-exists? (cdr x)))
   (> (file-or-directory-modify-seconds (car x))
      (file-or-directory-modify-seconds (cdr x)))))

(define S string-append)

(define (exec-simulate . xs)
  (begin
    (displayln (string-join xs " | "))
    #t))

(define subsys
  (make-parameter exec-simulate))

(define (module-deps criteria m)
  (map (∘ path->string simplify-path)
       (resolve-deps criteria m)))


(define (make transform files)
  
  (define (apply-cons f x)
    (f (car x) (cdr x)))
  
  (for-each (⤶ apply-cons transform)
            (filter outdated? files)))

 

(define (ext-ccr? x)
  (let* {[suffix    (regexp-quote ".cc.rkt")]
         [criteria  (regexp (S ".*" suffix))]}
    
    (regexp-match? criteria 
                   (path->string x))))

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
