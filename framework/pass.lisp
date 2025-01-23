(in-package :framework)

(defstruct (framebuffer-descrption (:conc-name fb-) (:constructor nil))
	   (attachments ())
	   (samples 1 :type integer))

(defun make-framebuffer-description (attachments &key (samples 1))
  (let ((fbd (make-instance 'framebuffer-descrption)))
    (setf (fb-attachments fbd) attachments)
    (setf (fb-samples fbd) (gficl:msaa-samples samples))
    fbd))

(defclass pass ()
  ((description :type framebuffer-descrption :initarg :description)
   (framebuffer :initform nil)
   (resolve-framebuffer :initform nil)
   (shaders :initarg :shaders)
   (width  :initform 0 :type integer)
   (height :initform 0 :type integer)
   (clear-buffers)
   (clear-colour :initarg :clear-colour :initform '(0.7 0.9 1.0 0.0))
   (resolve-multisamples :initform t :type boolean :documentation
			 "If t, blit multisampled framebuffers into a resolve buffer"))
  (:documentation
   "A framebuffer-backed object that holds a list of shaders.
When drawn with, draws the scene using all of it's shaders"))

(defgeneric get-textures (pass)
  (:documentation "Return an alist of (attachment-pos . texture)"))

(defgeneric get-final-framebuffer (pass)
	    (:documentation "Return resulting framebuffer"))

(defmethod initialize-instance :after ((instance pass) &key &allow-other-keys)
  (with-slots (description clear-buffers) instance
    (setf clear-buffers
	  (loop for a in (fb-attachments description) nconcing
		(gficl:attach-desc-clear-bits a)))))

(defun correct-multisample-attachments (samples attachments)
  "Change any :texture types from multisamples attachments to :renderbuffer"
  (if (= samples 1) attachments
    (map 'list #'(lambda (a)
		   (cond ((eq :texture (gficl:attach-desc-type a))
			  (let ((ad (copy-structure a)))
			    (setf (gficl:attach-desc-type ad) :renderbuffer) ad))
			 (t a)))
	 attachments)))

(defmethod reload ((obj pass))
  (with-slots (shaders) obj (loop for s in shaders do (reload s))))

(defmethod resize ((obj pass) (w integer) (h integer))
  (with-slots (width height description (fb framebuffer) (rfb resolve-framebuffer)) obj 
    (with-slots (samples attachments) description
      (setf width w) (setf height h)
      (if fb (gficl:delete-gl fb))
      (setf fb (gficl:make-framebuffer
		(correct-multisample-attachments samples attachments) w h :samples samples))
      (cond ((and (slot-value obj 'resolve-multisamples) (> samples 1))
	     (if rfb (gficl:delete-gl rfb))
	     (setf rfb (gficl:make-framebuffer attachments w h :samples 1)))))))

(defmethod draw :before ((obj pass) scenes)
  (with-slots (framebuffer (rfb resolve-framebuffer) clear-buffers clear-colour width height) obj
    (apply #'gl:clear-color clear-colour)
    (gficl:bind-gl framebuffer)
    (gl:viewport 0 0 width height)
    (if rfb (gl:enable :multisample) (gl:disable :multisample))
    (apply #'gl:clear clear-buffers)))

(defmethod draw ((obj pass) scenes)
  (loop for shader in (slot-value obj 'shaders) do
    (loop for scene in scenes do (draw shader scene))))

(defmethod draw :after ((obj pass) scenes)
  (with-slots ((fb framebuffer) (rfb resolve-framebuffer) width height) obj
    (if rfb (gficl:blit-framebuffers fb rfb width height
				     :buffer-list (list :color-buffer-bit :depth-buffer-bit)))))

(defmethod get-textures ((pass pass))
  (with-slots (description (final-fb framebuffer) (resolve-fb resolve-framebuffer)) pass
    (let ((fb (if (= 1 (fb-samples description)) final-fb resolve-fb)))
      (loop for i from 0 for a in (fb-attachments description)
	    when (eq :texture (gficl:attach-desc-type a))
	    collecting
	    (cons (gficl:attach-desc-position a) (gficl:framebuffer-texture-id fb i))))))

(defun get-pass-texture (pass &optional (attachment-position :color-attachment0))
  (cdr (assoc attachment-position (get-textures pass))))

(defmethod get-final-framebuffer ((pass pass))
  (with-slots ((fb framebuffer) (rfb resolve-framebuffer)) pass
    (if rfb rfb fb)))

(defmethod free ((obj pass))
  (with-slots (shaders (fb framebuffer) (rfb resolve-framebuffer)) obj
    (if fb (gficl:delete-gl fb))
    (if rfb (gficl:delete-gl rfb))
    (loop for shader in shaders do (free shader))))

;;; --- post-processing pass ---

(defclass post-pass (pass) ())

(defmethod draw ((pass post-pass) scenes)
  (error "Tried to draw an arbitrary scene with a POST-PASS. 
Can only draw a POST-SCENE with a POST-PASS"))

(defmethod draw ((pass post-pass) (scene post-scene))
  (with-slots (shaders) pass
    (gl:disable :depth-test :cull-face)
    (loop for shader in	shaders do (draw shader scene))))
