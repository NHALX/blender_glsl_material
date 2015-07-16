#lang racket/base
(require racket/function racket/port racket/match racket/string racket/set)
(require "NHA.rkt")
(provide exec)

(define (output-sink port)
  (thread (thunk
           (display (port->string port))
           (close-input-port port))))


(define (shell-pipe-spawn commands)
  
  (define/match (f cmd st)
    [(_ (cons xs stdout->stdin))
     (define-values (pid stdout _ stderr)
       (apply subprocess (append
                          (list #f stdout->stdin #f)
                          (string-split cmd))))

     (cons (cons
            (list pid (output-sink stderr) stdout)
            xs)
           stdout)])

  (foldl f (cons '() #f) commands))


(define (shell-pipe-wait results)
  
  (define/match (f _)
    [((list))
     (void)]
    
    [(xs) 
     (let* [[z     (apply sync xs)]
            [error (subprocess-status z)]]
     
     (if (= error 0)
         (cons error (set-remove xs z))
         (begin ; TODO: make sure this kill interacts with groups properly
           (for-each (⤷ subprocess-kill #f) xs)
           (cons error '()))))])
  
  (unfold f (map car results)))


(define (shell-pipe-cleanup results)
  
  (define (cleanup pid tid port)
    (close-input-port port))
  
  (for-each (⤶ apply cleanup) results))


(define (exec . commands)
  (match-define (cons results output)
    (shell-pipe-spawn commands))
  
  (display (port->string output))

  (define success
    (andmap (⤶ = 0)
            (shell-pipe-wait results)))
  
  (shell-pipe-cleanup results)
  success)


;(shell-compose "/bin/ls -l" "/usr/bin/wc -l" "/bin/ls")
;(shell "/bin/ls -l")
