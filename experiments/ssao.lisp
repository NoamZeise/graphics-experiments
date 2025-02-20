(in-package :experiments)

;;; deferred shader

(defclass deferred-shader (normals-cam-shader)
  ())

(defmethod reload ((s deferred-shader))
  (shader-reload-files
   (s (#p"deferred.vs" #p"deferred.fs")
      :folder (shader-subfolder #p"ssao/"))
   shader))

(defmethod shader-scene-props ((obj deferred-shader) (scene scene-3d))
  (call-next-method)
  (with-slots ((it-view inverse-transpose-view-mat)
	       (view view-mat)
	       (proj projection-mat))
      scene
    (with-slots (shader) obj
      (gficl:bind-matrix shader "norm_view" it-view)
      (gficl:bind-matrix shader "view" view)
      (gficl:bind-matrix shader "proj" proj))))

(defclass deferred-pass (pass) ())

(defun make-deferred-pass ()
  (make-instance
   'deferred-pass
   :shaders (list (make-instance 'deferred-shader))
   :description
   (make-framebuffer-description
    (list (gficl:make-attachment-description :type :texture
	   :position :color-attachment0 :internal-format :rgba32f)
	  (gficl:make-attachment-description :type :texture
	   :position :color-attachment1 :internal-format :rgba32f)
	  (gficl:make-attachment-description :position :depth-attachment)))))

(defmethod draw ((obj deferred-pass) scenes)
  (gl:enable :cull-face :depth-test)
  (call-next-method))

;;; ssao shader

(defclass ssao-shader (post-shader)
  ((noise-tex :type gficl:texture)))

(defmethod initialize-instance :after ((s ssao-shader) &key &allow-other-keys)
  (cffi:with-foreign-pointer (data (* 4 4 3))
    (loop for i from 0 to 16 do
	  (let ((v (gficl:make-vec
		    (loop for _ from 1 to 3 collecting
			  (- (random 2.0) 1)))))
	    (setf v (gficl:normalise v))))
    (setf
     (slot-value
      s 'noise-tex)
     (gficl:make-texture
      4 4 :format :rgb
      :internal-format :rgba16f
      :data data)))
  (reload s))

(defmethod free ((obj ssao-shader))
  (gficl:delete-gl (slot-value obj 'noise-tex))
  (call-next-method))

(defmethod reload ((s ssao-shader))
	   (shader-reload-files
	    (s (#p"post.vs" #p"ssao/ssao.fs"))
	    shader
	    (gl:uniformi (gficl:shader-loc shader "position") 0)
	    (gl:uniformi (gficl:shader-loc shader "normal") 1)
	    (gl:uniformi (gficl:shader-loc shader "noise") 2)))

(defmethod shader-scene-props ((s ssao-shader) (scene post-scene))
  (gficl:bind-gl (slot-value s 'shader))
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d
		   (get-post-tex scene :deferred :color-attachment0))
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d
		   (get-post-tex scene :deferred :color-attachment1))
  (gl:active-texture :texture2)
  (gficl:bind-gl (slot-value s 'noise-tex)))

(defclass ssao-pass (post-pass) ())

(defun make-ssao-pass ()
  (make-instance
   'ssao-pass
   :shaders (list (make-instance 'ssao-shader))
   :description
   (make-framebuffer-description
    (list (gficl:make-attachment-description :type :texture)))))

;;; ssao pipeline 

(defclass ssao-post-scene (post-scene) ())

(defclass ssao-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type ssao-post-scene)))

(defun make-ssao-pipeline ()
  (make-instance
   'ssao-pipeline
   :post-scene (make-instance 'ssao-post-scene)
   :passes (list (cons :deferred (make-deferred-pass))
		 (cons :ssao (make-ssao-pass)))))

(defmethod resize ((pl ssao-pipeline) w h)
  (call-next-method)
  (with-slots (post-scene) pl
    (resize post-scene w h)
    (set-post-texs post-scene (alist-fb-textures pl '(:deferred)))))

(defmethod draw ((pl ssao-pipeline) scenes)
  (with-slots (post-scene) pl
    (draw (get-pass pl :deferred) scenes)
    (with-slots (post-scene) pl
      (draw (get-pass pl :ssao) post-scene))
    (gficl:blit-framebuffers
     (get-final-framebuffer (get-pass pl :ssao))
     nil
     (gficl:window-width) (gficl:window-height))))
