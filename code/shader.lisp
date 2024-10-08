(in-package :project)

(defclass shader ()
  ((shader :accessor shader :type gficl:shader :initarg :shader)))

(defmethod draw :before ((obj shader) scene)
	   (gficl:bind-gl (shader obj)))

(defmethod draw ((obj shader) scene)
	   (loop for o in scene do
		 (draw o (shader obj))))

(defmethod free ((obj shader))
	   (gficl:delete-gl (shader obj)))

(defconstant +shader-folder+ #p"shaders/")
