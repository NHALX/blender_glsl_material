((types
  
  (extern-type v3)
  (extern-type v4)
  (extern-type m44)
  (primitive-type bool)
  (primitive-type real)
  (primitive-type string))
 
 (functions
  
  (e-function "bool-to-float" "True -> 1.0, False -> 0.0."
              (arg r bool "val")
              (arg w real ""))
  
  (e-function "mist-type" "Convert blender mist-falloff names to their numeric constant."
              (arg r string "bl-falloff-type")
              (arg w real ""))
  
  (e-function "lamp-spotsize" "Convert blender spot-size to shader parameter."
              (arg r real "bl-spot-size")
              (arg w real ""))
  
  (e-function "lamp-spotblend" "Convert blender spot-blend to shader parameter."
              (arg r real "bl-spot-size")
              (arg r real "bl-blend")
              (arg w real ""))
  
  (e-function "lamp-imat" "matrix that converts camera space to lamp space."
              (arg r m44 "lamp-to-world")
              (arg r m44 "cam-to-world")
              (arg w m44 ""))
  
  (e-function "lamp-dynco" "Position of the light in camera space."
              (arg r m44 "lamp-to-world")
              (arg r m44 "world-to-cam")
              (arg w v4 ""))
  
  (e-function "lamp-dynvec" "Direction that the light is facing in camera space."
              (arg r m44 "lamp-to-world")
              (arg r m44 "world-to-cam")
              (arg w v4 ""))
  
  (e-function "lamp-to-perspective" "Converts lamp space to camera space."
              (arg r real "bl-spot-size")
              (arg r real "clip-near")
              (arg r real "clip-far")
              (arg w m44 ""))
  
  (e-function "lamp-perspective-matrix" "Converts camera space to shadow buffer depth space."
              (arg r real "bl-spot-size")
              (arg r real "clip-near")
              (arg r real "clip-far")
              (arg r m44 "lamp-to-world")
              (arg r m44 "cam-to-world")
              (arg w m44 ""))))
