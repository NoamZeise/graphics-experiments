(in-package :experiments)

;;; post scene

(defclass ssao-post-scene (post-scene)
  ((projection-mat :initform (gficl:make-matrix) :type gficl:matrix)
   (light-dir-view :initform (gficl:make-vec (list 0 0 0)))))

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

(defmethod shader-mesh-props ((obj deferred-shader) props)
  (with-slots (shader) obj
    (let ((dt (obj-prop props :diffuse))
	  (col (obj-prop props :colour)))
      (gl:uniformi (gficl:shader-loc shader "use_texture") (if dt 1 0))
      (gficl:bind-vec shader "obj_colour" col)
      (if dt (gficl:bind-gl dt)))))

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
	  (gficl:make-attachment-description :type :texture
	   :position :color-attachment2 :internal-format :rgba32f)
	  (gficl:make-attachment-description :position :depth-attachment)))))

(defmethod draw ((obj deferred-pass) scenes)
  (gl:enable :cull-face :depth-test)
  (call-next-method))

;;; ssao shader

(defclass ssao-shader (post-shader)
  ((noise-tex :type gficl:texture)))

(defmethod initialize-instance :after ((s ssao-shader) &key &allow-other-keys)
  (cffi:with-foreign-pointer (data (* 16 2 (cffi:foreign-type-size :float)))
    (loop for i from 0 to 15 do
	  (let ((v (gficl:make-vec
		    (list (- (random 2.0) 1) (- (random 2.0) 1)))))
	    (setf v (gficl:normalise v))
	    (loop for j from 0 to 1 do
		  (setf (cffi:mem-aref data :float (+ j (* i 2)))
			(gficl:vec-ref v j)))
	    ))
    (setf
     (slot-value
      s 'noise-tex)
     (gficl:make-texture
      4 4 :format :rg
      :internal-format :rgba32f
      :data data :data-type :float)))
  (reload s))

(defmethod free ((obj ssao-shader))
  (gficl:delete-gl (slot-value obj 'noise-tex))
  (call-next-method))

(defmethod reload ((s ssao-shader))
  (shader-reload-files
   (s (#p"post.vs" #p"ssao/ssao.fs"))
   shader
   (gl:uniformi (gficl:shader-loc shader "bposition") 0)
   (gl:uniformi (gficl:shader-loc shader "bnormal") 1)
   (gl:uniformi (gficl:shader-loc shader "bnoise") 2)
   (gl:uniformf (gficl:shader-loc shader "radius") 0.5)
   (let ((kernel-size 64))
     (loop for i from 0 to (- kernel-size 1) do
	   (let ((v (gficl:make-vec
		     (loop for i from 1 to 3 collecting
			   (- (random 2.0) 1))))
		 (s (/ (float i) kernel-size)))
	     (flet ((lerp (a b r) (+ a (* r (- b a)))))
		   (setf v (gficl:*vec (* (lerp 0.1 1.0 (* s s)) (random 1.0))
				       (gficl:normalise v)))	   
		   (gficl:bind-vec
		    shader
		    (concatenate 'string "samples[" (format nil "~a" i) "]")
		    v)))))))

(defmethod shader-scene-props ((s ssao-shader) (scene ssao-post-scene))
  (call-next-method)
  (with-slots (shader) s    
    (gficl:bind-vec shader "screen_res"
		    (gficl:make-vec (list (gficl:window-width) (gficl:window-height))))
    (gficl:bind-matrix shader "proj" (slot-value scene 'projection-mat)))
    
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

;;; blur pass

(defclass ssao-blur-shader (post-shader)
  (target-tex))

(defmethod reload ((s ssao-blur-shader))
  (compute-shader-reload-files (s #p"ssao/blur.cs") shader
    (gl:uniformi (gficl:shader-loc shader "ssao_buff") 1)))

(defmethod draw ((s ssao-blur-shader) (scene ssao-post-scene))
  (with-slots (target-tex) s
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d target-tex)
    (gl:bind-image-texture 0 target-tex 0 nil 0 :write-only :rgba32f))
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d (get-post-tex scene :ssao :color-attachment0))
  (%gl:dispatch-compute (gficl:window-width) (gficl:window-height) 1)
  (%gl:memory-barrier '(:shader-image-access-barrier)))

(defclass ssao-blur-pass (post-pass) ())

(defun make-ssao-blur-pass ()
  (make-instance
   'ssao-blur-pass
   :shaders (list (make-instance 'ssao-blur-shader))
   :description (make-framebuffer-description
		 (list (gficl:make-attachment-description :type :texture
							  :internal-format :rgba32f)))))

(defmethod resize ((obj ssao-blur-pass) (w integer) (h integer))
  (call-next-method)
  (with-slots (shaders) obj
    (setf (slot-value (car shaders) 'target-tex) (get-pass-texture obj))))

;;; deferred lighting

(defclass ssao-lighting-shader (post-shader)
  (target-tex))

(defmethod reload ((s ssao-lighting-shader))
  (compute-shader-reload-files (s #p"ssao/post.cs") shader
    (gl:uniformi (gficl:shader-loc shader "bposition") 1)
    (gl:uniformi (gficl:shader-loc shader "bnormal") 2)
    (gl:uniformi (gficl:shader-loc shader "bcolour") 3)
    (gl:uniformi (gficl:shader-loc shader "bssao") 4)))

(defmethod draw ((s ssao-lighting-shader) (scene ssao-post-scene))
  (with-slots (target-tex) s
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d target-tex)
    (gl:bind-image-texture 0 target-tex 0 nil 0 :write-only :rgba32f))
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d (get-post-tex scene :deferred :color-attachment0))
  (gl:active-texture :texture2)
  (gl:bind-texture :texture-2d (get-post-tex scene :deferred :color-attachment1))
  (gl:active-texture :texture3)
  (gl:bind-texture :texture-2d (get-post-tex scene :deferred :color-attachment2))
  (gl:active-texture :texture4)
  (gl:bind-texture :texture-2d (get-post-tex scene :blur :color-attachment0))
  (with-slots (light-dir-view) scene    
    (gficl:bind-vec (slot-value s 'shader) "light_dir" light-dir-view))
  (%gl:dispatch-compute (gficl:window-width) (gficl:window-height) 1)
  (%gl:memory-barrier '(:shader-image-access-barrier)))

(defclass ssao-lighting-pass (post-pass) ())

(defun make-ssao-lighting-pass ()
  (make-instance
   'ssao-lighting-pass
   :shaders (list (make-instance 'ssao-lighting-shader))
   :description (make-framebuffer-description
		 (list (gficl:make-attachment-description :type :texture
							  :internal-format :rgba32f)))))

(defmethod resize ((obj ssao-lighting-pass) (w integer) (h integer))
  (call-next-method)
  (with-slots (shaders) obj
    (setf (slot-value (car shaders) 'target-tex) (get-pass-texture obj))))

;;; ssao pipeline 

(defclass ssao-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type ssao-post-scene)))

(defun make-ssao-pipeline ()
  (make-instance
   'ssao-pipeline
   :post-scene (make-instance 'ssao-post-scene)
   :passes (list (cons :deferred (make-deferred-pass))
		 (cons :ssao (make-ssao-pass))
		 (cons :blur (make-ssao-blur-pass))
		 (cons :lighting (make-ssao-lighting-pass)))))

(defmethod resize ((pl ssao-pipeline) w h)
  (call-next-method)
  (with-slots (post-scene) pl
    (resize post-scene w h)
    (set-post-texs post-scene (alist-fb-textures pl '(:deferred :ssao :blur)))))

(defmethod draw ((pl ssao-pipeline) scenes)
  (with-slots (post-scene) pl
    (draw (get-pass pl :deferred) scenes)
    (with-slots (post-scene) pl
      (with-slots (projection-mat light-dir-view) post-scene
	(with-slots ((it-view inverse-transpose-view-mat)
		     light-dir (proj projection-mat))
	    (car scenes)
	  (setf projection-mat proj)
	  (setf light-dir-view
		(gficl:get-n-vec 3 (gficl:mat*vec it-view (gficl:get-n-vec 4 light-dir))))))
      (draw (get-pass pl :ssao) post-scene)
      (draw (get-pass pl :blur) post-scene)
      (draw (get-pass pl :lighting) post-scene))
    (gficl:blit-framebuffers
     (get-final-framebuffer (get-pass pl :lighting))
     nil
     (gficl:window-width) (gficl:window-height))))
