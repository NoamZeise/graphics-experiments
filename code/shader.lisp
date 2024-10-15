(in-package :project)

(defclass shader ()
  ((shader :initarg :shader :type gficl:shader))
  (:documentation "encapsulates an opengl shader object.
Update shader uniforms with the following:
- Model props is called each time an object is draw.
- Scene props is called each time a scene is draw with the shader."))

(defgeneric shader-model-props (obj model normal)
  (:documentation "set shader model uniforms"))

(defgeneric shader-scene-props (obj scene)
  (:documentation "set shader scene uniforms"))

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
  () (:documentation "a shader that uses normal vectors for lighting, sets 'norm_mat' and 'cam' shader unforms to the model's normal mat and the scenes camera pos"))

(defmethod shader-model-props ((obj normals-shader) model normal)	   
  (gficl:bind-matrix (slot-value obj 'shader) "norm_mat" normal)
  (call-next-method))

(defmethod shader-scene-props ((obj normals-shader) (scene scene))
  (gficl:bind-vec (slot-value obj 'shader) "cam" (cam-pos scene))
  (call-next-method))

;;; post-process subclass

(defclass post-shader (shader)
  () (:documentation "draw method draws 3 dummy verts for a post processing step"))

(defmethod draw ((shader post-shader) (scene post-scene))
  (draw scene shader)
  (gficl:bind-gl (get-asset 'dummy-data))
  (gl:draw-arrays :triangles 0 3))
