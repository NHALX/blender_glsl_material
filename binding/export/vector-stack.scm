(list
 (u-function "vs-init" "Initialize vector allocator stack."
             (u-arg-ref rw w u-vector-stack ""))

 (u-function "vs-free" "Destroy vector stack."
             (u-arg-ref rw r u-vector-stack "vs-ctx")
             (u-arg w u-int ""))
 
 (u-function "vs-reset-full" "Reset vector stack pointer to the start."
             (u-arg-ref rw r u-vector-stack "vs-ctx")
             (u-arg w u-int ""))

 (u-function "vs-reset-to" "Reset stack pointer to the given location."
             (u-arg-ref rw r u-vector-stack "vs-ctx")
             (u-arg-ref r r u-float "vs-ptr") 
             (u-arg w u-int ""))

 (u-function "vs-mark" "Get the current stack pointer location."
             (u-arg-ref rw r u-vector-stack "vs-ctx")
             (u-arg-ref r r u-float ""))
 
 
 (u-function "vs-alloc" "Allocate an n-dimensional vector."
             (u-arg-ref rw r u-vector-stack "vs-ctx")
             (u-arg r u-uint "n")
             (u-arg-ref w r u-float "")))
