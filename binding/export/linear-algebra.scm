(list
 
 (u-e-function "v4-from3" "Construct a 4d vector from 3d, padding with 1.0."
               (u-arg r u-float "v0")
               (u-arg r u-float "v1")
               (u-arg r u-float "v2")
               (u-arg r u-vector ""))
 
 (u-e-function "v4" "Construct 4d vector."
               (u-arg r u-float "v0")
               (u-arg r u-float "v1")
               (u-arg r u-float "v2")
               (u-arg r u-float "v3")
               (u-arg r u-vector ""))
 
 (u-e-function "v3-negate" "(-x,-y,-z,1.0)"
               (u-arg r u-vector "vec")
               (u-arg r u-vector ""))

 (u-e-function "v3-normalize" "(x / |xyz|, y / |xyz|, z / |xyz|, 1)"
               (u-arg r u-vector "vec")
               (u-arg r u-vector ""))

 (u-e-function "v4-print" "Print vector."
               (u-arg r u-vector "vec")
               (u-arg r u-int ""))
 
 (u-e-function "m44" "Construct 4x4 matrix - row major."
               (u-arg r u-float "x00")
               (u-arg r u-float "x01")
               (u-arg r u-float "x02")
               (u-arg r u-float "x03")
               (u-arg r u-float "x10")
               (u-arg r u-float "x11")
               (u-arg r u-float "x12")
               (u-arg r u-float "x13")
               (u-arg r u-float "x20")
               (u-arg r u-float "x21")
               (u-arg r u-float "x22")
               (u-arg r u-float "x23")
               (u-arg r u-float "x30")
               (u-arg r u-float "x31")
               (u-arg r u-float "x32")
               (u-arg r u-float "x33")
               (u-arg r u-matrix ""))
 
 (u-e-variadic-function "m44*" "matrix product of all inputs (variadic)."
                        (u-arg r u-matrix ""))

 (u-e-function "m44*-v3" "matrix * (x,y,z,1.0)."
               (u-arg r u-matrix "mrx")
               (u-arg r u-vector "vec")
               (u-arg r u-vector ""))

 (u-e-function "m33*-v3" "matrix * (x,y,z). Needed when we can't pad 4th dimension to 1.0"
               (u-arg r u-matrix "mrx")
               (u-arg r u-vector "vec")
               (u-arg r u-vector ""))
  
 (u-e-function "m44-invert" "Calculate matrix inverse."
               (u-arg r u-matrix "mrx")
               (u-arg r u-matrix ""))

 (u-e-function "m44-transpose" "Transpose matrix."
               (u-arg r u-matrix "mrx")
               (u-arg r u-matrix ""))
 
 (u-e-function "m44-col" "Extract given column."
               (u-arg r u-matrix "mrx")
               (u-arg r u-uint "col")
               (u-arg r u-vector ""))
 
 (u-e-function "m44-frustum" "Construct a matrix that produces a perspective projection."
               (u-arg r u-float "left")
               (u-arg r u-float "right")
               (u-arg r u-float "bottom")
               (u-arg r u-float "top")
               (u-arg r u-float "znear")
               (u-arg r u-float "zfar")
               (u-arg r u-matrix ""))
 
 (u-e-function "m44-print" "Print matrix."
               (u-arg r u-matrix "mrx")
               (u-arg r u-int ""))


 
 (u-function "vs-init" "Initialize vector allocator stack."
             (u-arg-ref rw w u-vector-stack ""))
 
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
 
 (u-function "vs-free" "Destroy vector stack."
             (u-arg-ref rw r u-vector-stack "vs-ctx")
             (u-arg w u-int ""))
 
 (u-function "vs-alloc" "Allocate an n-dimensional vector."
             (u-arg-ref rw r u-vector-stack "vs-ctx")
             (u-arg r u-uint "n")
             (u-arg-ref w r u-float "")))
