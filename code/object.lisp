(in-package :project)

(defclass object ()
  ((meshes :initarg :meshes)
   (model :type gficl:matrix)
   (normal :type gficl:matrix)))

(defun make-object (meshes model-matrix)
  (let ((model (make-instance 'object :meshes meshes)))
    (update-model model model-matrix)
    model))

(defun update-model (obj model-matrix)
  (setf (slot-value obj 'model) model-matrix)
  (setf (slot-value obj 'normal) (gficl:transpose-matrix (gficl:inverse-matrix model-matrix))))

(defmethod draw ((obj object) shader)
  (with-slots (meshes model normal) obj
    (shader-model-props shader model normal)
    (if (listp meshes)
	(loop for mesh in meshes do
	      (gficl:draw-vertex-data mesh))
      (gficl:draw-vertex-data meshes))))
