(in-package :project)

(defstruct (framebuffer-descrption (:conc-name fb-))
	   (attachments)
	   (samples 1 :type integer))

(defclass pass ()
  ((description :accessor description :type framebuffer-descrption :initarg :description)
   (framebuffer :initform nil :accessor framebuffer)
   (resolve-framebuffer :initform nil :accessor resolve-framebuffer)
   (shaders :accessor shaders :initarg :shaders)
   (width  :initform 0 :accessor width :type integer)
   (height :initform 0 :accessor height :type integer)
   (clear-buffers :initform () :accessor clear-buffers)))

(defgeneric get-framebuffer-textures (pass)
	    (:documentation "Return a list of textures for each attachment of type texture."))

(defmethod initialize-instance :after ((instance pass) &key &allow-other-keys)
	   (resize instance (gficl:window-width) (gficl:window-height)))

(defun correct-multisample-attachments (samples attachments)
  "Change any :texture types from multisamples attachments to :renderbuffer"
  (if (= samples 1) attachments
    (map 'list #'(lambda (a)
		   (cond ((eq :texture (gficl:attach-desc-type a))
			  (let ((ad (copy-structure a)))
			    (setf (gficl:attach-desc-type ad) :renderbuffer) ad))
			 (t a)))
	 attachments)))

(defmethod resize ((obj pass) (width integer) (height integer))
	   (setf (width obj) width)
	   (setf (height obj) height)
	   (with-slots (samples attachments) (description obj)
	     (if (framebuffer obj) (gficl:delete-gl (framebuffer obj)))
	     (setf (framebuffer obj) (gficl:make-framebuffer
				      (correct-multisample-attachments samples attachments)
				      width height :samples samples))
	     (cond ((> samples 1)
		    (if (resolve-framebuffer obj) (gficl:delete-gl (resolve-framebuffer obj)))
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

(defmethod get-framebuffer-textures ((pass pass))
	   (let ((fb (if (= 1 (fb-samples (description pass)))
			 (framebuffer pass) (resolve-framebuffer pass))))
	     (loop for i from 0 for a in (fb-attachments (description pass))
		   when (eq :texture (gficl:attach-desc-type a))
		   collecting (gficl:framebuffer-texture-id fb i))))

(defmethod free ((obj pass))
	   (if (framebuffer obj)
	       (gficl:delete-gl (framebuffer obj)))
	   (if (resolve-framebuffer obj)
	       (gficl:delete-gl (resolve-framebuffer obj)))
	   (loop for shader in (shaders obj) do (free shader)))
