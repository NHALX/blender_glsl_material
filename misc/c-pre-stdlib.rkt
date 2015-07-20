#lang scribble/text

@(provide c:define c:λ c:for-each c:find uid-counter S)

@(define-syntax-rule (c:define xs ys ...)
   (define xs (list ys ...) ))

@(define-syntax-rule (c:λ xs ys ...)
   (λ xs (list ys ...) ))

@(define S string-append)


@(define uid-total 0)

@(define (uid-counter prefix)
  (set! uid-total (+ uid-total 1))
  (format "~a~a" prefix uid-total))


@c:define[(c:for-each 
           #:temp-prefix [temp (uid-counter "__c_for_each")]
           type xs f)]{

    //// c:for-each ////
    {
        const @|type| &@|temp|_xs = @|xs|;
        for (@|type|::const_iterator @|temp|_it = @|temp|_xs.begin(); 
             @|temp|_it != @|temp|_xs.end(); 
             @|temp|_it++)
        {
            @|(f @S{(*@|temp|_it)})|
        }
    }
  }

@c:define[(c:find 
           #:temp-prefix [temp (uid-counter "__c_find")]
           type result pred xs)]{

    //// c:find ////
    {
        const @|type| &@|temp|_xs = @|xs|;
        @|result| = NULL;
        for (@|type|::const_iterator @|temp|_it = @|temp|_xs.begin(); 
             @|temp|_it != @|temp|_xs.end(); 
             @|temp|_it++)
        {
            bool @|temp|_test = false;

            @|(pred @S{(*@|temp|_it)} @S{@|temp|_test})|

            if (@|temp|_test)
            {
                 @|result| = (*@|temp|_it);
                 break;
            }
        }
    }
}
