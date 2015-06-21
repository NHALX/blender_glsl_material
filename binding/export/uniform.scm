(list
 
 (u-e-function "uniform-3fv" "Assign vector to the uniform variable - trunctate to 3D."
               (u-arg r u-string "name")
               (u-arg r u-v3 "value")
			   (u-arg-ref r r u-void "shader-obj")			   
               (u-arg w u-int ""))
 
 (u-e-function "uniform-4fv" "Assign vector to the uniform variable."
               (u-arg r u-string "name")
               (u-arg r u-v4 "value")
			   (u-arg-ref r r u-void "shader-obj")			   
               (u-arg w u-int ""))
 
 (u-e-function "uniform-1fv" "Assign a float to the uniform variable."
               (u-arg r u-string "name")
               (u-arg r u-float "value")
			   (u-arg-ref r r u-void "shader-obj")			   
               (u-arg w u-int ""))
 
 (u-e-function "uniform-1i" "Assign an int to the uniform variable."
               (u-arg r u-string "name")
               (u-arg r u-int "value")
			   (u-arg-ref r r u-void "shader-obj")			   
               (u-arg w u-int ""))
 
 (u-e-function "uniform-Matrix4fv" "Assign a matrix to the uniform variable."
               (u-arg r u-string "name")
               (u-arg r u-m44 "value")
			   (u-arg-ref r r u-void "shader-obj")
               (u-arg w u-int ""))
    
 (u-e-function "attribute-2fv" "Bind a vertex attribute - type: {vertex-texture-coordinates, vertex-color, vertex-original-coordinates, vertex-tangent-vectors}"
               (u-arg r u-string "name")
               (u-arg r u-int "channel")
               (u-arg r u-string "type")
               (u-arg-ref r r u-void "geometry-obj")   
               (u-arg w u-int ""))
 
  (u-e-function "attribute-3fv" "Bind a vertex attribute - type: {vertex-texture-coordinates, vertex-color, vertex-original-coordinates, vertex-tangent-vectors}"
               (u-arg r u-string "name")
               (u-arg r u-int "channel")
               (u-arg r u-string "type")
               (u-arg-ref r r u-void "geometry-obj")   
               (u-arg w u-int ""))
  
 (u-e-function "attribute-4fv" "Bind a vertex attribute - type: {vertex-texture-coordinates, vertex-color, vertex-original-coordinates, vertex-tangent-vectors}"
               (u-arg r u-string "name")
               (u-arg r u-int "channel")
               (u-arg r u-string "type")
               (u-arg-ref r r u-void "geometry-obj")               
               (u-arg w u-int ""))

  (u-e-function "attribute-4ubv" "Bind a vertex attribute - type: {vertex-texture-coordinates, vertex-color, vertex-original-coordinates, vertex-tangent-vectors}"
               (u-arg r u-string "name")
               (u-arg r u-int "channel")
               (u-arg r u-string "type")
               (u-arg-ref r r u-void "geometry-obj")   
               (u-arg w u-int ""))
   
 (u-e-function "preload-image" "Load image. Result is passed to sampler() unchanged."
               (u-arg r u-int "size-x")
               (u-arg r u-int "size-y")
               (u-arg r u-string "filename")
               (u-arg-ref r w u-void "db-obj")
               (u-arg-ref r r u-void ""))
 
 (u-e-function "preload-buffer" "Input buffer, dimensions are square. Result is passed to sampler() unchanged"
			   (u-arg r u-string "buffer-id")
               (u-arg r u-int "width")
               (u-arg-ref r w u-void "db-obj")			   
               (u-arg-ref r r u-void ""))
 
 (u-e-function "preload-shadow-buffer" "Create shadow buffer. Result is passed to sampler() unchanged"
			   (u-arg r u-string "lamp-id")
               (u-arg r u-int "width")
			   (u-arg-ref r w u-void "db-obj")
               (u-arg-ref r r u-void ""))

 (u-e-function "sampler" "Bind texture object to sampler channel."
               (u-arg r u-string "uniform-name")               
               (u-arg r u-int "channel")
               (u-arg-ref r r u-void "texture-obj")
               (u-arg-ref r w u-void "shader-obj")			   
               (u-arg w u-int "")))



