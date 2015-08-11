#lang at-exp racket/base
(require (for-syntax racket/base))
(require (prefix-in r: scribble/reader))
(require scribble/text)

(require racket/match
         racket/port
         racket/dict
         racket/function)

(require "NHA.rkt"
         "c-pre-stdlib.rkt")


(provide (except-out (all-from-out racket/base) read read-syntax) 
         (all-from-out racket/function)
         (all-from-out scribble/text)
         (all-from-out "NHA.rkt")
         (all-from-out "c-pre-stdlib.rkt"))

 
;;╻  ╻┏┓╻┏━╸   ╻┏┓╻┏━╸┏━┓
;;┃  ┃┃┗┫┣╸ ╺━╸┃┃┗┫┣╸ ┃ ┃
;;┗━╸╹╹ ╹┗━╸   ╹╹ ╹╹  ┗━┛

(provide __FILE__ __LINE__ C hash:__FILE__:__LINE__)
 
(define C list)

(define-syntax (__FILE__ stx)
  (with-syntax ([file (syntax-source stx)])
    (syntax-case stx ()
      [_ #''file])))

(define-syntax (__LINE__ stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-case stx ()
      [_ #'line])))

(define-syntax (hash:__FILE__:__LINE__ stx)
    (syntax-case stx ()
      [(_ prefix) #`(let [[h (equal-hash-code  
                              (format "~a:~a"
                                      #,(syntax-source stx)
                                      #,(syntax-line stx)))]]
             (if (< h 0 )
                 (string-append prefix "1" (number->string (abs h)))
                 (string-append prefix "0" (number->string h))))]))


;;┏━┓┏━╸┏━┓╺┳┓┏━╸┏━┓
;;┣┳┛┣╸ ┣━┫ ┃┃┣╸ ┣┳┛
;;╹┗╸┗━╸╹ ╹╺┻┛┗━╸╹┗

(provide (rename-out [new-read read]
                     [new-read-syntax read-syntax]))

;; TODO: this parser is fragile and basicly garbage
(define file-layout-pattern
  (pregexp (string-append
            "^[\\s]*"
            "(?>\\|[\\s]*top-level[\\s]*([^\\|]*) \\|)?[\\s]*"
            "(?>\\|[\\s]*head[\\s]*([^\\|]*) \\|)?[\\s]*"
            "(?>\\|[\\s]*import[\\s]*([^\\|]*) \\|)?[\\s]*"
            "(.*)"
            "$")))

(define (make-$-readtable)
  (make-readtable (current-readtable)
                  #\$ 'terminating-macro read-dollar))

 
(define read-dollar
  (case-lambda
   [(ch in)
    (display 910281)
    1234]
   [(ch in src line col pos)
    (display 91809182)
    5678]))

(define (new-read in)
  (syntax->datum (new-read-syntax #f in)))


(define (new-read-syntax src in)
  
  (define (begin-wrap xs)
    (string-append
     "@begin/text{" xs "}"))


  (define/match (import txt)
    [((pregexp #px"^[\\s]*#include ([\"<].*[\">])[\\s]*$" (list _ x)))
     (list `(c:namespace++ 'imports ,(format "#include ~a\n" x)))]
    
    [((pregexp #px"^[\\s]*\"(.*)\"[\\s]*$" (list _ name)))
     (list
      `(require (prefix-in
                 ,(string->symbol (format "~a:" name))
                 ,(format "~a.cc.rkt" name)))
      
      #;`(c:namespace++ 'imports
                   (flatten
                    (append
                     (show-header
                     ,(string->symbol (format "~a:header-source" name)))
                     "///////////header///////////\n")))
      `(c:namespace++ 'imports ,(format "#include \"generated/~a.cc.h\"\n" name))
      )])
      
  (define (fmap-m f x)
    (if x (f x) empty))
  
  (match-define
    (pregexp file-layout-pattern
             (list _
                   outer-txt
                   head-txt
                   import-txt
                   body-txt))
    
    (port->string in))

  (define (imports xs)
    (fmap-m (∘ (⤶ map import)
               (⤷ string-split "\n")) xs))
  
  (define (globals xs)
    
    (define (get-all-expr port)
      (define (get-expr x)
        (define y (read x))
        (unless (eof-object? y) (cons y x)))
      
      (unfold get-expr port))
    
    (fmap-m (∘ get-all-expr open-input-string) xs))
  
  (define (body s)
    ;; TODO: remove this unused read table
    (parameterize ([current-readtable (make-$-readtable)])
      (r:read
       (open-input-string
        (begin-wrap s)))))
  
  #`(module anon racket/base
      (module main racket/base
         (require (file #,(path->string __FILE__)))
         #,@(globals outer-txt)      
         #,@(apply append (imports import-txt))
         (require scribble/text)
         #;(provide header-source)
         
         (let [[module-source #,(body body-txt)]]
           (cond
             [(equal? (current-command-line-arguments) (vector "--exports"))
              (output (show-header header-source))]
             [(> (vector-length (current-command-line-arguments)) 0)
              (begin
                (displayln "usage: [--exports]" (current-error-port))
                (exit -1))]
             [else
              (output
               (append
                (list #,head-txt "\n")
                (show-header header-source)
                module-source))])))))
         

;;╻ ╻┏━╸┏━┓╺┳┓┏━╸┏━┓   ┏━┓╺┳╸┏━┓╺┳╸┏━╸
;;┣━┫┣╸ ┣━┫ ┃┃┣╸ ┣┳┛╺━╸┗━┓ ┃ ┣━┫ ┃ ┣╸ 
;;╹ ╹┗━╸╹ ╹╺┻┛┗━╸╹┗╸   ┗━┛ ╹ ╹ ╹ ╹ ┗━╸
;; TODO: param header-src etc

(provide header-source c:namespace++ h-group-order h-group-show show-header)

(define (show-header src)
  (add-between
           (map h-group-show
                (sort (hash->list header-source)
                      < #:key (∘ h-group-order cdr)))
           
           "\n"))

(define (make-counter)
  (define n 0)
  (λ () (let [[i n]]
          (begin
            (set! n (+ 1 n))
            i))))

(struct h-group (order extract data))

(define (h-group-show x)
  ((h-group-extract (cdr x))
   (h-group-data (cdr x))))

          
(define insert-counter (make-counter))
  
(define header-source
  (let [[null-group
    (λ (n) (h-group n (λ (v) v) empty))]]
    
  (make-hash (list
              (cons 'imports (null-group (insert-counter)))
              (cons 'top-level (null-group (insert-counter)))))))

(define (update-list export xs)
    (append xs (if (list? export)
                   export
                   (list export))))

;; Append text to namespace

(define (c:namespace++ key v)
  (dict-update! header-source key
                (match-lambda [(struct h-group (n t xs))
                               (h-group n t (update-list v xs))])))




;;┏━╸╻ ╻┏┓╻┏━╸╺┳╸╻┏━┓┏┓╻
;;┣╸ ┃ ┃┃┗┫┃   ┃ ┃┃ ┃┃┗┫
;;╹  ┗━┛╹ ╹┗━╸ ╹ ╹┗━┛╹ ╹
(provide ƒ constructor destructor)


@(define-syntax (function-internal stx)
   (syntax-case stx ()
     [(_ class name initializer-list (scope virtual/static ret-t (t var) ...) body ...)
      (with-syntax
         [[(args ...)
           #'(list (format "~a ~a" 't 'var) ...)]]
       
        #`(begin
            ;; place args and their type values in racket scope
            (define-values (var ...)
              (values (c:type-info (symbol->string 'var) 't) ...))
            
            (let*
               [[class::name
                 (if (equal? 'top-level 'class)
                     'name
                     (format "~a::~a" 'class 'name))]
                
                [maybe-init
                 (if (equal? "" initializer-list)
                     ""
                      (format ": ~a" initializer-list))]
                
                [maybe-scope
                 (if (equal? "" 'scope)
                     ""
                     (format "~a: " 'scope))]
                
                [function-decl
                 @list{
                 @|maybe-scope|@|'virtual/static| @|`ret-t| @|`name|(@|(string-join (args ...) ", ")|);
      
                 }]
                
                [function-def
                 @list{
                 @|`ret-t|
                 @|class::name|(@|(string-join (args ...) ",\n    ")|)@|maybe-init|
                 {
                     @begin/text{@|body ...|}
                 }
                 @#\newline
                 }]]
             
               (c:namespace++ 'class (list function-decl))
               function-def)))]))

@(define-syntax (destructor stx)
  (syntax-case stx ()
    [(_ class scope body ...)
     #`(function-internal class
          #,(format "~~~a" (syntax->datum #'class))
          ""
          (scope "" "") body ...)]))

@(define-syntax (constructor stx)
  (syntax-case stx (init:)
    [(_ class scope ((t var) ...) init: init body ...)
     #'(function-internal class class init (scope "" "" (t var) ...) body ...)]
    
    [(_ class scope ((t var) ...) body ...)
     #'(function-internal class class "" (scope "" "" (t var) ...) body ...)]))


(define-syntax (ƒ stx)
  (syntax-case stx (:)
            
    [(ƒ name (ret-t (t var) ...) body ...)
     #'(function-internal top-level name ""
                          ("" "" ret-t (t var) ...) body ...)]
    
    [(ƒ name (static ret-t (t var) ...) body ...)
     #'(function-internal top-level name ""
                          ("" static ret-t (t var) ...) body ...)]

    [(ƒ class name (ret-t (t var) ...) body ...)
     #'(function-internal class name ""
                          ("" "" ret-t (t var) ...) body ...)]
    
    [(ƒ class name (scope ret-t (t var) ...) body ...)
     #'(function-internal class name ""
                          (scope "" ret-t (t var) ...) body ...)]
    
    [(ƒ class name (scope virtual/static ret-t (t var) ...) body ...)
     #'(function-internal class name ""
                          (scope virtual/static ret-t (t var) ...) body ...)] ))


;;╺┳┓┏━┓╺┳╸┏━┓
;; ┃┃┣━┫ ┃ ┣━┫
;;╺┻┛╹ ╹ ╹ ╹ ╹
(provide c:namespace c:class c:struct c:data c:typedef)


(define (c:typedef t a)
  (c:namespace++ 'top-level @list{typedef @|t| @|a|;@#\newline}))

                                        
(define-syntax (c:data stx)
  (syntax-case stx (:)
    [(_ name : scope type)
     #'(c:namespace++ 'top-level
                   @list{@|'scope|: @|'type| @|'name|;@#\newline})]

    [(_ class (name : scope type) ...)
     #'(c:namespace++ 'class
                   (list @list{@|'scope|: @|'type| @|'name|;@#\newline} ...))]))


(define (show-class/struct class/struct class-name inherit class-data)
  
  (define super-class
    (if inherit
        (format ": ~a " inherit)
        ""))
  
   @list{
   @|class/struct| @|class-name| @|super-class|{
       @class-data
   };
    
   })

(define-syntax-rule (cs-add class/struct name inherit xs ...)
  (begin
    (dict-set!
     header-source 'name
     (h-group (insert-counter)
              (curry show-class/struct class/struct 'name inherit)
              empty))
    (c:data name xs ...)))

(define-syntax c:class
  (syntax-rules (||)
    [(c:class name || inherit xs ...) (cs-add 'class name inherit xs ...)]
    [(c:class name xs ...)            (cs-add 'class name #f xs ...)]))

(define-syntax c:struct
  (syntax-rules (||)
    [(c:struct name || inherit xs ...) (cs-add 'struct name inherit xs ...)]
    [(c:struct name xs ...)            (cs-add 'struct name #f xs ...)]))

(define-syntax c:namespace
  (syntax-rules ()
    [(c:namespace name xs ...)
     (cs-add 'namespace name #f xs ...)]))

;;╺┳╸┏━╸┏━┓╺┳╸
;; ┃ ┣╸ ┗━┓ ┃ 
;; ╹ ┗━╸┗━┛ ╹           

(module+ test #f
    
  (new-read (open-input-string
     "| top-level (display 'a) (display 'b) |
      | head \"#include \"precompile.h\"\"
      | import \"xxxx\"
               \"qqq\"
               #include <zzzz>
               \"yyyy\"
               #include <uuuu> |
      1234")))

