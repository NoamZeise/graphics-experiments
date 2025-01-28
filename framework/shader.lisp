(in-package :framework)

(defclass shader ()
  ((shader :type gficl:shader)
   (new-obj :initform t))
  (:documentation "encapsulates an opengl shader object.
Update shader uniforms with the following:
- Model props is called each time an object is draw.
- Scene props is called each time a scene is draw with the shader."))

(defgeneric shader-model-props (obj props)
  (:documentation
   "set shader model uniforms using props (an alist"))

(defgeneric shader-mesh-props (obj props)
  (:documentation "set shader props (an alist) per mesh"))

(defgeneric shader-scene-props (obj scene)
  (:documentation "set shader scene uniforms"))

(defmethod initialize-instance :after ((obj shader) &key &allow-other-keys)
  (reload obj))

(defmethod reload ((obj shader))
  (with-slots (new-obj) obj
    (if new-obj (setf new-obj nil)
      (gficl:delete-gl (slot-value obj 'shader)))))

(defmacro shader-reload-files ((shader paths
			       &key (folder +shader-folder+)
			            (load-fn nil))
			       shader-var
			       &body body)
  "Only recompile the shader if any of the files have been modified.
By default needs a vert and frag path if load-fn not supplied (uses GFICL/LOAD:SHADER)"
  (let ((n-o (gensym)) (error-var (gensym)))
    `(with-slots ((,n-o new-obj)) ,shader
       (if ,n-o (watch-files (list ,@paths) :folder ,folder))
       (cond ((or ,n-o (files-modified (list ,@paths) :folder ,folder))
	      (format t "loading shader (~{~a~^ + ~}) in ~a~%" (list ,@paths) ,folder)
	      (handler-case
		  (let ((,shader-var
			 ,(if load-fn
			      `(funcall ,load-fn ,@paths)
			    (destructuring-bind (&optional (vert nil) (frag nil)) paths
			      (if (not (and vert frag))
				  (error
				   "Must pass vertex and fragment paths to shader-reload-files"))
			      `(gficl/load:shader ,vert ,frag :shader-folder ,folder)))))
		    (call-next-method)
		    (setf (slot-value ,shader 'shader) ,shader-var)
		    (gficl:bind-gl ,shader-var)
		    ,@body)
		(error (,error-var)
                       (if ,n-o (error ,error-var))
		       (format t "~%shader compile error~%~a~%~%" ,error-var))))))))

(defmacro compute-shader-reload-files ((shader path
			       &key (folder +shader-folder+))
			       shader-var &body body)
  "takes a single shader path. 
Uses GFICL/LOAD:COMPUTE-SHADER to load the shader."
  `(shader-reload-files
    (,shader ,(list path) :folder ,folder
	     :load-fn (lambda (path) (gficl/load:compute-shader path :shader-folder ,folder)))
    ,shader-var ,@body))

(defmethod shader-model-props ((obj shader) props)
	   (gficl:bind-matrix (slot-value obj 'shader) "model"
			      (cdr (assoc :model props))))

(defmethod shader-mesh-props ((obj shader) props))

(defmethod shader-scene-props ((obj shader) (scene scene))
  (gficl:bind-matrix (slot-value obj 'shader) "viewproj" (view-projection scene)))

(defmethod draw :before ((obj shader) arg)
  (gficl:bind-gl (slot-value obj 'shader)))

(defmethod draw ((shader shader) scene)
  (draw scene shader))

(defmethod free ((obj shader))
  (gficl:delete-gl (slot-value obj 'shader)))

;;; normals subclass

(defclass normals-shader (shader)
  () (:documentation "a shader that uses normal vectors for lighting, sets 'norm_mat' shader unform to the model's normal mat"))

(defmethod shader-model-props ((obj normals-shader) props)
  (gficl:bind-matrix (slot-value obj 'shader) "norm_mat"
		     (cdr (assoc :normal props)))
  (call-next-method))

(defclass normals-cam-shader (normals-shader)
  () (:documentation "a shader that uses normal vectors for lighting, sets 'norm_mat' and 'cam' shader unforms to the model's normal mat and the scenes camera pos"))

(defmethod shader-scene-props ((obj normals-cam-shader) (scene scene))
  (gficl:bind-vec (slot-value obj 'shader) "cam" (cam-pos scene))
  (call-next-method))

(defmethod shader-scene-props ((obj normals-cam-shader) (scene scene-3d))
  (gficl:bind-vec (slot-value obj 'shader) "light_dir" (light-dir scene))
  (call-next-method))

;;; post-process subclass

(defclass post-shader (shader)
  () (:documentation "draw method draws 3 dummy verts for a post processing step"))

(defmethod draw ((shader post-shader) (scene post-scene))
  (draw scene shader)
  (gficl:bind-gl (get-asset 'dummy-data))
  (gl:draw-arrays :triangles 0 3))
