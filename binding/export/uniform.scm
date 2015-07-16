((types
  
  (extern-type v3)
  (extern-type v4)
  (extern-type m44)
  (primitive-type bool)
  (primitive-type real)
  (primitive-type int)
  (primitive-type string)
  (primitive-type void))
 
 (functions
  
  (e-function "uniform-3fv" "Assign vector to the uniform variable - trunctate to 3D."
              (arg r string "name")
              (arg r v3 "value")
              (arg-ref r r void "shader-obj")			   
              (arg w int ""))
  
  (e-function "uniform-4fv" "Assign vector to the uniform variable."
              (arg r string "name")
              (arg r v4 "value")
              (arg-ref r r void "shader-obj")			   
              (arg w int ""))
  
  (e-function "uniform-1fv" "Assign a float to the uniform variable."
              (arg r string "name")
              (arg r real "value")
              (arg-ref r r void "shader-obj")			   
              (arg w int ""))
  
  (e-function "uniform-1i" "Assign an int to the uniform variable."
              (arg r string "name")
              (arg r int "value")
              (arg-ref r r void "shader-obj")			   
              (arg w int ""))
  
  (e-function "uniform-Matrix4fv" "Assign a matrix to the uniform variable."
              (arg r string "name")
              (arg r m44 "value")
              (arg-ref r r void "shader-obj")
              (arg w int ""))
  
  (e-function "attribute-2fv" "Bind a vertex attribute - type: {vertex-texture-coordinates, vertex-color, vertex-original-coordinates, vertex-tangent-vectors}"
              (arg r string "name")
              (arg r int "channel")
              (arg r string "type")
              (arg-ref r r void "geometry-obj")   
              (arg w int ""))
  
  (e-function "attribute-3fv" "Bind a vertex attribute - type: {vertex-texture-coordinates, vertex-color, vertex-original-coordinates, vertex-tangent-vectors}"
              (arg r string "name")
              (arg r int "channel")
              (arg r string "type")
              (arg-ref r r void "geometry-obj")   
              (arg w int ""))
  
  (e-function "attribute-4fv" "Bind a vertex attribute - type: {vertex-texture-coordinates, vertex-color, vertex-original-coordinates, vertex-tangent-vectors}"
              (arg r string "name")
              (arg r int "channel")
              (arg r string "type")
              (arg-ref r r void "geometry-obj")               
              (arg w int ""))

  (e-function "attribute-4ubv" "Bind a vertex attribute - type: {vertex-texture-coordinates, vertex-color, vertex-original-coordinates, vertex-tangent-vectors}"
              (arg r string "name")
              (arg r int "channel")
              (arg r string "type")
              (arg-ref r r void "geometry-obj")   
              (arg w int ""))
  
  (e-function "preload-image" "Load image. Result is passed to sampler() unchanged."
              (arg r int "size-x")
              (arg r int "size-y")
              (arg r string "filename")
              (arg-ref r w void "db-obj")
              (arg-ref r r void ""))
  
  (e-function "preload-buffer" "Input buffer, dimensions are square. Result is passed to sampler() unchanged"
              (arg r string "buffer-id")
              (arg r int "width")
              (arg-ref r w void "db-obj")			   
              (arg-ref r r void ""))
  
  (e-function "preload-shadow-buffer" "Create shadow buffer. Result is passed to sampler() unchanged"
              (arg r string "lamp-id")
              (arg r int "width")
              (arg-ref r w void "db-obj")
              (arg-ref r r void ""))

  (e-function "sampler" "Bind texture object to sampler channel."
              (arg r string "uniform-name")               
              (arg r int "channel")
              (arg-ref r r void "texture-obj")
              (arg-ref r w void "shader-obj")			   
              (arg w int ""))))



