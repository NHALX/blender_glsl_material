((types
  (new-type v3)
  (new-type v4)
  (new-type m44)
  (primitive-type bool)
  (primitive-type real)
  (primitive-type uint))
 
 (functions
  (e-function "v3" "Construct a 3d vector."
                (arg r real "v0")
                (arg r real "v1")
                (arg r real "v2")
                (arg r v3 ""))
 
  (e-function "v4" "Construct 4d vector."
                (arg r real "v0")
                (arg r real "v1")
                (arg r real "v2")
                (arg r real "v3")
                (arg r v4 ""))

  (function "v4-eqf" "|a-b| < epsilon"
                (arg r v4 "a")
                (arg r v4 "b")
                (arg r real "epsilon")
                (arg r bool ""))

  (function "v3-eqf" "|a-b| < epsilon"
                (arg r v3 "a")
                (arg r v3 "b")
                (arg r real "epsilon")
                (arg r bool ""))

  (function "m44-eqf" "|a-b| < epsilon"
                (arg r m44 "a")
                (arg r m44 "b")
                (arg r real "epsilon")
                (arg r bool ""))

  (e-function "v4-v3" "(x,y,z) -> (x,y,z,1)"
                (arg r v3 "v3")
                (arg r v4 ""))
 
  (e-function "v3-v4" "(x,y,z,w) -> (x,y,z)"
                (arg r v4 "v4")
                (arg r v3 ""))
 
  (e-function "v3-negate" "(-x,-y,-z)"
                (arg r v3 "vec")
                (arg r v3 ""))

  (e-function "v3-normalize" "(x / |xyz|, y / |xyz|, z / |xyz|)"
                (arg r v3 "vec")
                (arg r v3 ""))

  (e-function "m44" "Construct 4x4 matrix - row major."
                (arg r real "x00")
                (arg r real "x01")
                (arg r real "x02")
                (arg r real "x03")
                (arg r real "x10")
                (arg r real "x11")
                (arg r real "x12")
                (arg r real "x13")
                (arg r real "x20")
                (arg r real "x21")
                (arg r real "x22")
                (arg r real "x23")
                (arg r real "x30")
                (arg r real "x31")
                (arg r real "x32")
                (arg r real "x33")
                (arg r m44 ""))
 
  (e-function "m44*" "matrix product."
                (arg r m44 "mx0")
                (arg r m44 "mx1")
                (arg r m44 ""))

  (e-function "m44*-v3" "matrix * (x,y,z,1.0)."
                (arg r m44 "mx")
                (arg r v3 "vec")
                (arg r v3 ""))

  (e-function "m33*-v3" "Multiply the sub-3x3 matrix by v3."
                (arg r m44 "mx")
                (arg r v3 "vec")
                (arg r v3 ""))
 
  (e-function "m44-invert" "Calculate matrix inverse."
                (arg r m44 "mx")
                (arg r m44 ""))

  (e-function "m44-transpose" "Transpose matrix."
                (arg r m44 "mx")
                (arg r m44 ""))
 
  (e-function "m44-col" "Extract given column."
                (arg r m44 "mx")
                (arg r uint "col")
                (arg r v4 "")) ))

