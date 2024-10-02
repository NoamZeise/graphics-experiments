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

(defgeneric draw (obj shader)
   (:documentation "Draw the object using the supplied shader."))

(defmethod draw ((obj object) shader)
  (with-slots (meshes model normal) obj
    (gficl:bind-gl shader)
    (gficl:bind-matrix shader "model" model)
    (gficl:bind-matrix shader "norm_mat" normal)
    (if (listp meshes)
	(loop for mesh in meshes do
	      (gficl:draw-vertex-data mesh))
      (gficl:draw-vertex-data meshes))))
