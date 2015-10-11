#lang at-exp racket/base ;reader "../../CSPL15/CSPL15.rkt"
(require syntax/parse (for-syntax racket/base syntax/parse))
(require scribble/text)
(require "../../CSPL15/private/type-sig.rkt"
         "../../CSPL15/private/data.rkt"
         "../../CSPL15/private/loops.rkt"
         "../../CSPL15/private/symbol-table.rkt"
         "../../misc/NHA.rkt")

(provide 𝑟: node:fold node:for-each)


(define-syntax (𝑟: stx)
  (syntax-parse stx
    [(_ path init:assignment type:type-annotation)
     
     #`(symbol-table-set!*
        `path
        (λ (x) (info:var x
                         init.val init.cast 'init.test
                         (transform-codomain
                          (⤶ format "osg::ref_ptr<~a>")
                          type.sig))))]))




(define (osg-visitor obj f callback [init #f])
  
  (define name       (uid-counter "__nodvis"))
  (define node       (format "~a_node" name))
  (define state      (and init (format "~a_state" name)))
  (define constructor
    
    (let [[parent-init
           "osg::NodeVisitor(osg::NodeVisitor::TRAVERSE_ALL_CHILDREN)"]]
    
    (if init
        @list{decltype(@init) @|state|;

               @|name|(decltype(@init) initial):
                  @parent-init,
                  initial(@state)
               {}}

        @list{@|name|(): @parent-init {}})))
       
  @begin/text{
            
     [&@|f|, &@|obj|] 
     {
        struct @|name|: public osg::NodeVisitor
        {
            @constructor

            void apply(osg::Node& @|node|)
            {
                @|(callback state node)|; 
                traverse(@|node|);
            }

        } @|name|_callback(@|init|);

        @|obj|->accept(@|name|_callback);
        return@(and init (format " ~a_callback.~a" name state));
     }()
   })


(define-syntax-rule (node:fold f init obj)

    (osg-visitor `obj
                 `f
                 (λ (st x) (format "~a = ~a(~a, ~a)" st `f st x))
                 `init))


(define-syntax-rule (node:for-each f obj)

    (osg-visitor `obj
                 `f
                 (λ (st x) (format "~a(~a)" `f x))))


(module+ test
  (define (render x)
    (define s (open-output-string))
    (output x s)
    (get-output-string s))
  
  (displayln (render (node:fold f 0 object)))
  (displayln (render (node:for-each f object))) )

