(in-package :project)

(defclass basic-shader (lighting-shader)
  ())

(defun make-basic-shader ()
  (let ((shader (gficl/load:shader #p"vert.vs" #p"frag.fs" :shader-folder +shader-folder+)))
    (make-instance 'basic-shader :shader shader)))

(defmethod draw ((obj basic-shader) scene)
  (gl:enable :depth-test)
  (call-next-method))

(defclass basic-pass (pass)
  ())

(defun make-basic-pass ()
  (make-instance
   'basic-pass
   :shaders (list (make-basic-shader))
   :description
   (make-framebuffer-descrption
    :attachments
    (list (gficl:make-attachment-description)
	  (gficl:make-attachment-description :position :depth-attachment))
    :samples 1)))
