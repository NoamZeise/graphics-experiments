(in-package :project)

(defclass shader ()
  ((shader :accessor shader :type gficl:shader :initarg :shader)))

(defgeneric shader-model-props (obj model normal)
	    (:documentation "Set shader model uniforms"))

(defgeneric shader-scene-props (obj scene)
	    (:documentation "Set shader scene uniforms"))

(defmethod shader-model-props ((obj shader) model normal)
	   (gficl:bind-matrix (shader obj) "model" model))

(defmethod shader-scene-props ((obj shader) (scene scene))
	   (gficl:bind-matrix (shader obj) "viewproj" (view-projection scene)))

(defmethod draw :before ((obj shader) scene)
	   (gficl:bind-gl (shader obj)))

(defmethod draw ((shader shader) scene)
	   (draw scene shader))

(defmethod free ((obj shader))
	   (gficl:delete-gl (shader obj)))

(defclass lighting-shader (shader)
  () (:documentation
      "A shader that uses normal vectors for lighting"))

(defmethod shader-model-props ((obj lighting-shader) model normal)	   
	   (gficl:bind-matrix (shader obj) "norm_mat" normal)
	   (call-next-method))

(defmethod shader-scene-props ((obj lighting-shader) (scene scene))
	   ;(gficl:bind-vec (shader obj) "cam" (cam-pos scene))
	   (call-next-method))
