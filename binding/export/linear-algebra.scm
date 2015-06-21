(list
 
 (u-e-function "v3" "Construct a 3d vector."
               (u-arg r u-float "v0")
               (u-arg r u-float "v1")
               (u-arg r u-float "v2")
               (u-arg r u-v3 ""))
 
 (u-e-function "v4" "Construct 4d vector."
               (u-arg r u-float "v0")
               (u-arg r u-float "v1")
               (u-arg r u-float "v2")
               (u-arg r u-float "v3")
               (u-arg r u-v4 ""))

 (u-e-function "v4-v3" "(x,y,z) -> (x,y,z,1)"
               (u-arg r u-v3 "v3")
               (u-arg r u-v4 ""))
 
 (u-e-function "v3-v4" "(x,y,z,w) -> (x,y,z)"
               (u-arg r u-v4 "v4")
               (u-arg r u-v3 ""))
 
 (u-e-function "v3-negate" "(-x,-y,-z)"
               (u-arg r u-v3 "vec")
               (u-arg r u-v3 ""))

 (u-e-function "v3-normalize" "(x / |xyz|, y / |xyz|, z / |xyz|)"
               (u-arg r u-v3 "vec")
               (u-arg r u-v3 ""))

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
               (u-arg r u-m44 ""))
 
 (u-e-function "m44*" "matrix product."
               (u-arg r u-m44 "mx0")
               (u-arg r u-m44 "mx1")
               (u-arg r u-m44 ""))

 (u-e-function "m44*-v3" "matrix * (x,y,z,1.0)."
               (u-arg r u-m44 "mx")
               (u-arg r u-v3 "vec")
               (u-arg r u-v3 ""))

 (u-e-function "m33*-v3" "Multiply the sub-3x3 matrix by v3."
               (u-arg r u-m44 "mx")
               (u-arg r u-v3 "vec")
               (u-arg r u-v3 ""))
 
 (u-e-function "m44-invert" "Calculate matrix inverse."
               (u-arg r u-m44 "mx")
               (u-arg r u-m44 ""))

 (u-e-function "m44-transpose" "Transpose matrix."
               (u-arg r u-m44 "mx")
               (u-arg r u-m44 ""))
 
 (u-e-function "m44-col" "Extract given column."
               (u-arg r u-m44 "mx")
               (u-arg r u-uint "col")
               (u-arg r u-v4 ""))
 
 (u-e-function "m44-frustum" "Construct a matrix that produces a perspective projection."
               (u-arg r u-float "left")
               (u-arg r u-float "right")
               (u-arg r u-float "bottom")
               (u-arg r u-float "top")
               (u-arg r u-float "znear")
               (u-arg r u-float "zfar")
               (u-arg r u-m44 ""))
 
 )

