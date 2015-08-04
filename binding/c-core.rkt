#lang racket/base
(require racket/path
         racket/dict
         racket/string
         racket/list
         "../misc/c-pre.rkt")

(require (planet samsergey/rewrite:1:0))

(provide input-files
         import-modules
         import-values
         c-symbol
         c-access-mode
         c-type
         type-find
         new-type?
         extern-type?
         primitive-type?
         map-types)

;; ┏┳┓╻┏━┓┏━╸
;; ┃┃┃┃┗━┓┃  
;; ╹ ╹╹┗━┛┗━╸

(define (file->environment-name file)
  (path->string
   (path-replace-suffix
    (file-name-from-path file) "")))


;; ╻┏┳┓┏━┓┏━┓┏━┓╺┳╸   ┏━╸╻╻  ┏━╸┏━┓
;; ┃┃┃┃┣━┛┃ ┃┣┳┛ ┃ ╺━╸┣╸ ┃┃  ┣╸ ┗━┓
;; ╹╹ ╹╹  ┗━┛╹┗╸ ╹    ╹  ╹┗━╸┗━╸┗━┛

(define input-files
  (vector->list (current-command-line-arguments)))

(define import-modules
  (map file->environment-name input-files))

(define import-values
  (map (λ (x) (call-with-input-file x read)) input-files))

(define (type-find import x)  
  (findf (λ y (equal? x (cadar y)))
         (dict-ref import `types)))

(define (new-type? x)
  (equal? `new-type (car x)))

(define (extern-type? x)
  (equal? `extern-type (car x)))

(define (primitive-type? x)
  (equal? `primitive-type (car x)))

(define (map-types pred f)
  (map f
       (remove-duplicates
        (apply append
         (map (∘ (⤶ filter pred)
                 (⤷ dict-ref `types))
              import-values)))))


;; ┏━┓┏━╸╻ ╻┏━┓╻╺┳╸┏━╸   ┏━┓╻ ╻╻  ┏━╸┏━┓
;; ┣┳┛┣╸ ┃╻┃┣┳┛┃ ┃ ┣╸ ╺━╸┣┳┛┃ ┃┃  ┣╸ ┗━┓
;; ╹┗╸┗━╸┗┻┛╹┗╸╹ ╹ ┗━╸   ╹┗╸┗━┛┗━╸┗━╸┗━┛

(define (c-symbol x)
  (string-replace
   (string-replace x "-" "_")
   "*" "mul"))

(define c-access-mode
  (replace-all
   `r  --> "const"
   `w  --> ""
   `rw --> ""))

(define c-type
  (replace-all-repeated
   (list `new-type x)       --> (format "~a_t" x)
   (list `extern-type x)    --> (format "~a_t" x)
   (list `primitive-type x) --> x
   `bool                    --> "boolean"
   `string                  --> "string"
   `int                     --> "integer"
   `uint                    --> "ulong"
   `real                    --> "real"
   `void                    --> "void"))



;; ┏━╸┏┓╻╺┳╸┏━┓╻ ╻   ┏━┓┏━┓╻┏┓╻╺┳╸
;; ┣╸ ┃┗┫ ┃ ┣┳┛┗┳┛╺━╸┣━┛┃ ┃┃┃┗┫ ┃ 
;; ┗━╸╹ ╹ ╹ ╹┗╸ ╹    ╹  ┗━┛╹╹ ╹ ╹

(when (< (vector-length (current-command-line-arguments)) 1)
  (display "usage: no input modules specified\n")
  (exit))

