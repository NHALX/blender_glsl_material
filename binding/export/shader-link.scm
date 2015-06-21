(list

 (u-e-function "bool-to-float" "True -> 1.0, False -> 0.0."
               (u-arg r u-bool "val")
               (u-arg w u-float ""))
 
 #|
 (u-e-function "color4"
 (u-arg r u-v "rgba")
 (u-arg w u-color4 ""))
 
 (u-e-function "color3"
 (u-arg r u-v "rgb")
 (u-arg w u-color3 ""))
 |#
 
 
 (u-e-function "mist-type" "Convert blender mist-falloff names to their numeric constant."
               (u-arg r u-string "bl-falloff-type")
               (u-arg w u-float ""))
 
 (u-e-function "lamp-spotsize" "Convert blender spot-size to shader parameter."
               (u-arg r u-float "bl-spot-size")
               (u-arg w u-float ""))
 
 (u-e-function "lamp-spotblend" "Convert blender spot-blend to shader parameter."
               (u-arg r u-float "bl-spot-size")
               (u-arg r u-float "bl-blend")
               (u-arg w u-float ""))
 
 (u-e-function "lamp-imat" "matrix that converts camera space to lamp space."
               (u-arg r u-m44 "lamp-to-world")
               (u-arg r u-m44 "cam-to-world")
               (u-arg w u-m44 ""))
 
 (u-e-function "lamp-dynco" "Position of the light in camera space."
               (u-arg r u-m44 "lamp-to-world")
               (u-arg r u-m44 "world-to-cam")
               (u-arg w u-v4 ""))
 
 (u-e-function "lamp-dynvec" "Direction that the light is facing in camera space."
               (u-arg r u-m44 "lamp-to-world")
               (u-arg r u-m44 "world-to-cam")
               (u-arg w u-v4 ""))
 
 (u-e-function "lamp-to-perspective" "Converts lamp space to camera space."
               (u-arg r u-float "bl-spot-size")
               (u-arg r u-float "clip-near")
               (u-arg r u-float "clip-far")
               (u-arg w u-m44 ""))
 
 (u-e-function "lamp-perspective-matrix" "Converts camera space to shadow buffer depth space."
               (u-arg r u-float "bl-spot-size")
               (u-arg r u-float "clip-near")
               (u-arg r u-float "clip-far")
               (u-arg r u-m44 "lamp-to-world")
               (u-arg r u-m44 "cam-to-world")
               (u-arg w u-m44 "")))
