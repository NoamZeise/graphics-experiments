(in-package :experiments)

;;; variance shadow map shader

(defclass vsm-shader (shader) ())

(defmethod reload ((s vsm-shader))
  (shader-reload-files (s (#p"basic.vs" #p"vsm.fs")
			  :folder (shader-subfolder #p"shadows/"))
		       shader))

(defmethod shader-scene-props ((obj vsm-shader) (scene scene-3d))
  (with-slots (light-vp) scene
    (gficl:bind-matrix (slot-value obj 'shader) "viewproj" light-vp)))

(defmethod draw ((obj vsm-shader) scenes)
  (gl:enable :depth-test :cull-face)
  (call-next-method))

;;; shadow pass

(defclass vsm-pass (pass) ())

(defun make-vsm-pass ()
  (make-instance 'vsm-pass
    :shaders (list (make-instance 'vsm-shader))
    :description
    (make-framebuffer-description
     (list (gficl:make-attachment-description
	    :type              :texture
	    :internal-format   :rgba32f)
	   (gficl:make-attachment-description
	    :position :depth-attachment))
     :samples 16)))

(defmethod resize :after ((obj vsm-pass) (w integer) (h integer))
  (gl:bind-texture :texture-2d (get-pass-texture obj))
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear))

(defmethod draw :after ((obj vsm-pass) scenes)
  (gl:bind-texture :texture-2d (get-pass-texture obj))
  (gl:generate-mipmap :texture-2d))

;;; shadow map deferred

(defclass shadow-deferred-post-shader (post-shader) ())

(defmethod reload ((s shadow-deferred-post-shader))
  (compute-shader-reload-files (s #p"shadows/deferred-post.cs") shader
    (gl:uniformi (gficl:shader-loc shader "blight") 1)
    (gl:uniformi (gficl:shader-loc shader "bcolour") 2)))

(defmethod draw ((s shadow-deferred-post-shader) (scene post-scene))
  (let ((draw-target (get-post-tex scene :post :color-attachment0)))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d draw-target)
    (gl:bind-image-texture 0 draw-target 0 nil 0 :write-only :rgba32f))
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d (get-post-tex scene :deferred :color-attachment1))
  (gl:active-texture :texture2)
  (gl:bind-texture :texture-2d (get-post-tex scene :deferred :color-attachment0))
  (%gl:dispatch-compute (gficl:window-width) (gficl:window-height) 1)
  (%gl:memory-barrier '(:shader-image-access-barrier)))

(defclass shadow-deferred-post-pass (post-pass) ())

(defun make-shadow-deferred-post-pass ()
  (make-instance
   'shadow-deferred-post-pass
   :shaders (list (make-instance 'shadow-deferred-post-shader))
   :description (make-framebuffer-description
		 (list (gficl:make-attachment-description :type :texture :internal-format :rgba32f)))))

(defclass shadow-deferred-post-scene (post-scene) ())

(defclass shadow-deferred-pipeline (pipeline)
  ((post-scene :initform (make-instance 'shadow-deferred-post-scene))))

(defparameter *default-shadow-map-size* 2048)

(defun make-shadow-deferred-pipeline ()
  (make-instance
   'shadow-deferred-pipeline
   :passes (list (cons :shadow (let ((s (make-vsm-pass))) (resize s *default-shadow-map-size* *default-shadow-map-size*) s))
		 (cons :deferred (make-deferred-pass))
		 (cons :post (make-shadow-deferred-post-pass)))))

(defmethod initialize-instance :after ((pl shadow-deferred-pipeline) &key &allow-other-keys)
  (setf (slot-value (car (slot-value (get-pass pl :deferred) 'shaders)) 'shadow-map)
	(get-pass-texture (get-pass pl :shadow))))

(defmethod resize ((pl shadow-deferred-pipeline) w h)
  (resize (get-pass pl :deferred) w h)
  (resize (get-pass pl :post) w h)
  (with-slots (post-scene) pl
    (resize post-scene w h)
    (set-post-texs post-scene (alist-fb-textures pl '(:deferred :post)))))

(defmethod draw ((pl shadow-deferred-pipeline) scenes)
  (draw (get-pass pl :shadow) scenes)
  (draw (get-pass pl :deferred) scenes)
  (with-slots (post-scene) pl
    (draw (get-pass pl :post) post-scene))
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :post))
   nil
   (gficl:window-width) (gficl:window-height)))
