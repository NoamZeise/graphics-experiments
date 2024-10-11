(in-package :project)

(defclass shader ()
  ((shader :initarg :shader :type gficl:shader)))

(defgeneric shader-model-props (obj model normal)
  (:documentation "Set shader model uniforms"))

(defgeneric shader-scene-props (obj scene)
  (:documentation "Set shader scene uniforms"))

(defmethod shader-model-props ((obj shader) model normal)
  (gficl:bind-matrix (slot-value obj 'shader) "model" model))

(defmethod shader-scene-props ((obj shader) (scene scene))
  (gficl:bind-matrix (slot-value obj 'shader) "viewproj" (view-projection scene)))

(defmethod draw :before ((obj shader) scene)
  (gficl:bind-gl (slot-value obj 'shader)))

(defmethod draw ((shader shader) scene)
  (draw scene shader))

(defmethod free ((obj shader))
  (gficl:delete-gl (slot-value obj 'shader)))

(defclass lighting-shader (shader)
  () (:documentation
      "A shader that uses normal vectors for lighting"))

(defmethod shader-model-props ((obj lighting-shader) model normal)	   
  (gficl:bind-matrix (slot-value obj 'shader) "norm_mat" normal)
  (call-next-method))

(defmethod shader-scene-props ((obj lighting-shader) (scene scene))
  ;; (gficl:bind-vec (shader obj) "cam" (cam-pos scene))
  (call-next-method))
