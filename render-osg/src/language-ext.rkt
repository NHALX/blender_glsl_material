#lang at-exp s-exp "../../misc/c-pre.rkt"

(provide 
 node:fold
 node:for-each)


@;; generates boilerplate for OSGs visitor pattern

@c:define[(node:fold 
           #:class-name [name (uid-counter "__node_fold_struct_")]
           type result obj f)]{

 //// node:fold ////
 {
     struct @|name|: public osg::NodeVisitor
     {
         @|type| @|name|_state;

         @|name|(@|type| initial) :
             osg::NodeVisitor(osg::NodeVisitor::TRAVERSE_ALL_CHILDREN),
             @|name|_state(initial)
         {}

         void apply(osg::Node& @|name|_node) 
         {
             @|(f @S{@|name|_state} @S{@|name|_node})|
             traverse(@|name|_node);
         }

     } @|name|_callback(@|result|);

     @|obj|->accept(@|name|_callback);
     @|result| = @|name|_callback.@|name|_state;
 }}


@c:define[(node:for-each 
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
