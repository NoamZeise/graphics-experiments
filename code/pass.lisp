(in-package :project)

(defstruct (framebuffer-descrption (:conc-name fb-))
	   (attachments)
	   (samples 1 :type integer))

(defclass pass ()
  ((description :accessor description :type framebuffer-descrption)
   (framebuffer :accessor framebuffer :type gficl:framebuffer)
   (resolve-framebuffer :accessor resolve-framebuffer)
   (shaders :accessor shaders)
   (width :accessor width :type integer)
   (height :accessor height :type integer)))

(defmethod resize ((obj pass) (width integer) (height integer))
	   (setf (width obj) width)
	   (setf (height obj) height)
	   (with-slots (samples attachments) (description obj)
	     (gficl:delete-gl (framebuffer obj))
	     (setf (framebuffer obj)
		   (gficl:make-framebuffer attachments width height :samples samples))
	     (cond ((> samples 1)
		    (gficl:delete-gl (resolve-framebuffer obj))
		    (setf (resolve-framebuffer obj)
			  (gficl:make-framebuffer attachments width height :samples 1))))))

(defmethod draw :before ((obj pass) scene)
	   (gficl:bind-gl (framebuffer obj)))

(defmethod draw ((obj pass) scene)
	   (loop for shader in (shaders obj) do
		 (draw shader scene)))

(defmethod draw :after ((obj pass) scene)
	   (if (resolve-framebuffer obj)
	       (gficl:blit-framebuffers
		(framebuffer obj) (resolve-framebuffer obj)
		(width obj) (height obj))))

(defmethod free ((obj pass))
	   (if (framebuffer obj)
	       (gficl:delete-gl (framebuffer obj)))
	   (if (resolve-framebuffer obj)
	       (gficl:delete-gl (resolve-framebuffer obj)))
	   (loop for shader in (shaders obj) do (free shader)))
