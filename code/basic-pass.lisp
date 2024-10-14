(in-package :project)

(defclass basic-shader (normals-shader)
  ())

(defun make-basic-shader ()
  (let ((shader (gficl/load:shader #p"vert.vs" #p"frag.fs" :shader-folder +shader-folder+)))
    (gficl:bind-gl shader)
    (gl:uniformi (gficl:shader-loc shader "tex") 0)
    (make-instance 'basic-shader :shader shader)))

(defmethod draw ((obj basic-shader) scene)
  (gl:enable :depth-test)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'uv))
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
    (list (gficl:make-attachment-description :type :texture)
	  (gficl:make-attachment-description :position :depth-attachment))
    :samples (msaa-samples 16))))
