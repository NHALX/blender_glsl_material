#lang racket
(provide binding-export file->environment-name)

(define (file->environment-name file)
  (path->string
   (path-replace-suffix
    (file-name-from-path file) "")))


(define (binding-export lib-prefix es import-files)
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'racket/string)
    (namespace-require 'racket/list)
    (namespace-require 'racket/match)
    (namespace-require 'racket/function)
    (namespace-require 'racket/dict)
    
    (eval `(begin
             (define c-symbol-prefix (unquote lib-prefix))
             (define export-environment "none")))
        
    (define (exec file)
      (begin
        (eval `(set! export-environment ,(file->environment-name file)))
        (for-each eval es)
        (load file)))
    
    (map exec import-files) ))



