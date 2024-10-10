(in-package :project)

(defclass shader ()
  ((shader :accessor shader :type gficl:shader :initarg :shader)))

(defgeneric shader-model-props (obj model normal)
	    (:documentation "Set shader model uniforms"))

(defmethod shader-model-props ((obj shader) model normal)
	   (gficl:bind-matrix (shader obj) "model" model)
	   (gficl:bind-matrix (shader obj) "norm_mat" normal))

(defmethod draw :before ((obj shader) scene)
	   (gficl:bind-gl (shader obj)))

(defmethod draw ((obj shader) scene)
	   (loop for o in scene do
		 (draw o (shader obj))))

(defmethod free ((obj shader))
	   (gficl:delete-gl (shader obj)))

(defclass shader-no-normals (shader)
  ())

(defmethod shader-model-props ((obj shader-no-normals) model normal)
	   (gficl:bind-matrix (shader obj) "model" model))

(defconstant +shader-folder+ #p"shaders/")
