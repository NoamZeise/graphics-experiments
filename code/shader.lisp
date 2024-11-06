(in-package :project)

(defclass shader ()
  ((shader :type gficl:shader)
   (new-obj :initform t))
  (:documentation "encapsulates an opengl shader object.
Update shader uniforms with the following:
- Model props is called each time an object is draw.
- Scene props is called each time a scene is draw with the shader."))

(defgeneric shader-model-props (obj model normal)
  (:documentation "set shader model uniforms"))

(defgeneric shader-scene-props (obj scene)
  (:documentation "set shader scene uniforms"))

(defmethod initialize-instance :after ((obj shader) &key &allow-other-keys)
  (reload obj))

(defmethod reload ((obj shader))
  (with-slots (new-obj) obj
    (if new-obj (setf new-obj nil)
      (gficl:delete-gl (slot-value obj 'shader)))))

(defmacro shader-reload-files ((shader files &key (folder +shader-folder+)) &body body)
  "Only recompile the shader if any of the files have been modified."
  (let ((n-o (gensym)))
    `(with-slots ((,n-o new-obj)) ,shader
      (if ,n-o (watch-files ,files :folder ,folder))
      (cond ((or ,n-o (files-modified ,files :folder ,folder))
	     (format t "loading shaders ~a in ~a~%" ,files ,folder)
	     (call-next-method)
	     ,@body)))))

(defmacro reload-body (shader shader-files))

(defmethod shader-model-props ((obj shader) model normal)
  (gficl:bind-matrix (slot-value obj 'shader) "model" model))

(defmethod shader-scene-props ((obj shader) (scene scene))
  (gficl:bind-matrix (slot-value obj 'shader) "viewproj" (view-projection scene)))

(defmethod draw :before ((obj shader) arg)
  (gficl:bind-gl (slot-value obj 'shader)))

(defmethod draw ((shader shader) (scene scene))
  (draw scene shader))

(defmethod free ((obj shader))
  (gficl:delete-gl (slot-value obj 'shader)))

;;; normals subclass

(defclass normals-shader (shader)
  () (:documentation "a shader that uses normal vectors for lighting, sets 'norm_mat' shader unform to the model's normal mat"))

(defmethod shader-model-props ((obj normals-shader) model normal)
  (gficl:bind-matrix (slot-value obj 'shader) "norm_mat" normal)
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
