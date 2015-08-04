#lang at-exp racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/format))

(require racket/function
         racket/format
         racket/list)

(require scribble/text)

(provide c:guard
          c:vector
          c:dictionary
          c:var 
          c:type-info
          c:type-info-type
          c:type-info-symbol
          Λ c:λ c:for-each c:map c:find uid-counter S)


;;;;;;;;;;; guard ;;;;;;;;;;;

(define (handle-negation constant)
   (if (equal? (string-ref constant 0) #\!)
       
       (values "==" (list->string
                       (drop (string->list constant) 1)))
       (values "!=" constant)))


(define-for-syntax (guard-assign bail condition type symbol expr)
  #`(let-values [[(test val) (handle-negation
                                (~a '#,condition))]]
  
    (format "~a ~a = (~a) ~a;\nif (~a ~a ~a){\n    ~a;\n}\n"
            '#,type '#,symbol '#,type '#,expr
            '#,symbol test val
            #,bail)))

(define-for-syntax (guard-regular bail condition expr)
  #`(let-values [[(test val) (handle-negation
                              (~a '#,condition))]]
  
      (format "if (~a ~a ~a){\n    ~a;\n}\n"
              '#,expr test val #,bail)))

(define-syntax (c:guard stx)
  (syntax-case stx ()
    [(_ bail rest ...)
     (syntax-case #'(rest ...) (= :)
       [(condition = expr)
        #`(list #,(guard-regular #'bail #'condition #'expr))]
             
       [(condition = expr xs ...) 
        #`(cons #,(guard-regular #'bail #'condition #'expr)
                (c:guard bail xs ...))]
       
       [(condition : type symbol = expr)
        #`(list #,(guard-assign #'bail #'condition #'type #'symbol #'expr))]
       
       [(condition : type symbol = expr xs ...)
        #`(cons #,(guard-assign #'bail #'condition #'type #'symbol #'expr)
                (c:guard bail xs ...))])]))



;;;;;;;;;;;;;;;;;;;;;;

(struct c:type-info (symbol type-raw))

(define (c:type-info-type x)
   (string-trim
    (c:type-info-type-raw x)
    "&" #:left? #f)) ;TODO: how to handle ref-ptrs properly?
   
(define (c:type-info-fmap f x)
  (define-values (s t) (f (c:type-info-symbol x)
                          (c:type-info-type x)))
    
  (c:type-info s t))


(define (c-struct-body a b symbol [assignment #f])
  (format "~a { ~a fst; ~a snd; } ~a~a;"
          (c:type-info-type symbol)
          a
          b
          (c:type-info-symbol symbol)
          (if assignment
              (string-append " = " assignment)
              "")))

(define (c-struct-type symbol)
  (c:type-info symbol
        (format "struct ~a" (uid-counter "tuple"))))

(define-syntax (c:var stx)
  (syntax-case stx ()
    [(_ (tuple a b) symbol . rest)
     #`(begin
         (define symbol (c-struct-type 'symbol))
         (c-struct-body a b symbol
                        #,(syntax-case #'rest (=)
                            [(= assignment) #'assignment]
                            [()             #'#f])))]
   [(_ type symbol . rest)
    #`(begin (define symbol (c:type-info 'symbol type))
             (format "~a ~a~a;"
                     type
                     (c:type-info-symbol symbol)
                     #,(syntax-case #'rest (=)
                         [(= assignment) #'(S " = " assignment)]
                         [()             #'""])))]))



(define-syntax-rule (Λ xs ys ...)
   (define xs (list ys ...) ))

(define-syntax-rule (c:λ xs ys ...)
   (λ xs (list ys ...) ))

(define S string-append)


(define uid-total 0)

(define (uid-counter prefix)
  (set! uid-total (+ uid-total 1))
  (format "~a~a" prefix uid-total))



@Λ[(c:for-each
           f
           [iter #f]
           #:begin [begin (c:type-info-fmap (λ (a b)
                                            (values (S a ".begin()")
                                                    (S b "::const_iterator")))
                                          iter)]

           #:end   [end   (format "~a.end()"
                                  (c:type-info-symbol iter))]
           
           #:temp-prefix [temp (uid-counter "__c_for_each")]
           )]{
    //// c:for-each ////
    for (@|(c:type-info-type   begin)| @|temp|_it =
         @|(c:type-info-symbol begin)|; 
         @|temp|_it != @|end|; 
         @|temp|_it++)
    {
         @|(f @S{(*@|temp|_it)})|
    }
 }


(struct container (def insert element-type symbol-name))

(define (c:vector element-type symbol-name)
  (container 
   (λ (size) @S{
                std::vector<@|element-type|> @|symbol-name|;
                @|symbol-name|.reserve(@|size|);
               })

   (λ (x) @S{
            @|symbol-name|.push_back(@|x|);
            })

   element-type
   symbol-name))

(define (c:dictionary key-type element-type symbol-name)
  (container 
   (λ (size) @S{
                std::map<@|key-type|, @|element-type|> @|symbol-name|;
               })

   (λ (x) @S{
            @|symbol-name|.insert(@|x|);
            })

   @S{std::pair<@|key-type|, @|element-type|>}
   symbol-name))


@Λ[(c:map
           #:temp-prefix [temp (uid-counter "__c_map")]
           output
           in-expr f)]{

    //// c:map ////
    const @|(c:type-info-type in-expr)| &@|temp|_xs =
                                              @|(c:type-info-symbol in-expr)|;

    @((container-def output) @S{@|temp|_xs.size()})

    for (@|(c:type-info-type in-expr)|::const_iterator
         @|temp|_it =  @|temp|_xs.begin(); 
         @|temp|_it != @|temp|_xs.end(); 
         @|temp|_it++)
    {
        @|(container-element-type output)| @|temp|_result;

        @|(f @S{(*@|temp|_it)} @S{@|temp|_result})|

        @((container-insert output) @S{@|temp|_result});
    }  
}


@Λ[(c:find 
           #:temp-prefix [temp (uid-counter "__c_find")]
           type result pred expr)]{

    //// c:find ////
    {
        const @|type| &@|temp|_xs = @|expr|;
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
