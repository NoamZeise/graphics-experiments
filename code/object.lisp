(in-package :project)

(defclass object ()
  ((meshes :initarg :meshes)
   (diffuse :initarg :diffuse)
   (model :type gficl:matrix)
   (normal :type gficl:matrix)))

(defun make-object (meshes model-matrix &key diffuse-texs)
  (let ((model (make-instance 'object :meshes meshes :diffuse diffuse-texs)))
    (update-model model model-matrix)
    model))

(defun update-model (obj model-matrix)
  (setf (slot-value obj 'model) model-matrix)
  (setf (slot-value obj 'normal) (gficl:transpose-matrix (gficl:inverse-matrix model-matrix))))

(defmethod draw ((obj object) shader)
  (with-slots (meshes model normal diffuse) obj
    (shader-model-props shader
			(list (cons :model model)
			      (cons :normal normal)))
    (if (listp meshes)
	(if diffuse
	    (loop for mesh in meshes for d in diffuse do
		  (progn
		    (shader-mesh-props shader (list (cons :diffuse d)))
		    (gficl:draw-vertex-data mesh)))
	    (loop for mesh in meshes do
		  (progn
		    (shader-mesh-props shader (list))
		    (gficl:draw-vertex-data mesh))))
      (progn
	(shader-mesh-props shader (list))
	(gficl:draw-vertex-data meshes)))))

(defmacro object-matrix (position &optional (size ''(1 1 1)))
  `(gficl:*mat (gficl:translation-matrix ,position)
	       (gficl:scale-matrix ,size)
	       (gficl:scale-matrix '(1 1 1))))
