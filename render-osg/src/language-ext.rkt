#lang at-exp s-exp "../../misc/c-pre.rkt"

(provide 
 node:fold
 node:for-each)


@;; generates boilerplate for OSGs visitor pattern

@Λ[(node:fold 
           result-var obj f ;; TODO: handle state-type right
           #:class-name [name (uid-counter "__node_fold_struct_")] 
           #:state-type [stype (c:type-info-type result-var)])]{

 //// node:fold ////
 {
     struct @|name|: public osg::NodeVisitor
     {
         @stype @|name|_state;

         @|name|(@stype initial) :
             osg::NodeVisitor(osg::NodeVisitor::TRAVERSE_ALL_CHILDREN),
             @|name|_state(initial)
         {}

         void apply(osg::Node& @|name|_node) 
         {
             @|(f @S{@|name|_state} @S{@|name|_node})|
             traverse(@|name|_node);
         }

     } @|name|_callback(@|(c:type-info-symbol result-var)|);

     @|obj|->accept(@|name|_callback);
     @|(c:type-info-symbol result-var)| = @|name|_callback.@|name|_state;
 }}


@Λ[(node:for-each 
           #:class-name [name (uid-counter "__node_for_each_struct_")]
           obj f)]{

 //// node:for-each ////
 {
     struct @|name|: public osg::NodeVisitor
     {
         @|name|() : osg::NodeVisitor(osg::NodeVisitor::TRAVERSE_ALL_CHILDREN)
         {}

         void apply(osg::Node& @|name|_node) 
         {
             @|(f @S{@|name|_node})|
             traverse(@|name|_node);
         }

     } @|name|_callback;
     @|obj|->accept(@|name|_callback);
 }}
