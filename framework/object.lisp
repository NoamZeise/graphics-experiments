(in-package :framework)

(defclass object ()
  ((meshes :initarg :meshes)
   (diffuse-tex :initarg :diffuse)
   (model :type gficl:matrix)
   (normal :type gficl:matrix)
   (light? :initarg :light :initform nil :type bool)
   (colour :initarg :colour :initform (gficl:make-vec '(1 1 1 1)))))

(defun make-object (meshes model-matrix
			   &key diffuse-texs
			   (colour (gficl:make-vec '(1 1 1 1)))
			   light)
  (let ((model (make-instance 'object :meshes meshes :diffuse diffuse-texs
			      :colour colour
			      :light light)))
    (update-model model model-matrix)
    model))

(defun extract-model-texs (tex-list)
  (loop for tex in tex-list collecting
	(cdr (assoc :tex tex))))

(defun make-object-from-model+tex (model+tex model-matrix
					     &key (colour (gficl:make-vec '(1 1 1 1))) light)
  (let* ((model (cdr (assoc :model model+tex)))
	 (diffuse-list (cdr (assoc :diffuse model+tex)))
	 (diffuse-texs (extract-model-texs diffuse-list)))
    (make-object model model-matrix :diffuse-texs diffuse-texs :colour colour :light light)))

(defun update-model (obj model-matrix)
  (setf (slot-value obj 'model) model-matrix)
  (setf (slot-value obj 'normal) (gficl:transpose-matrix (gficl:inverse-matrix model-matrix))))

(defmethod draw ((obj object) shader)
  (with-slots (meshes model normal diffuse-tex colour light?) obj
    (shader-model-props shader (list (cons :model model)
				     (cons :normal normal)))
    (let ((mesh-props (list (cons :colour colour)
			    (cons :light light?))))
      (if (listp meshes)
	  (if diffuse-tex
	      (loop for mesh in meshes for d in diffuse-tex do
		    (progn
		      (shader-mesh-props shader (cons (cons :diffuse d) mesh-props))
		      (gficl:draw-vertex-data mesh)))
	    (loop for mesh in meshes do
		  (progn
		    (shader-mesh-props shader mesh-props)
		    (gficl:draw-vertex-data mesh))))
	(progn
	  (shader-mesh-props shader mesh-props)
	  (gficl:draw-vertex-data meshes))))))

(defun obj-prop (props key)
  (cdr (assoc key props)))

(defmacro object-matrix (position &optional (size ''(1 1 1)))
  `(gficl:*mat (gficl:translation-matrix ,position)
	       (gficl:scale-matrix ,size)
	       (gficl:scale-matrix '(1 1 1))))
