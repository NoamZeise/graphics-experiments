(in-package :experiments)

;;; post scene

(defclass ssao-post-scene (post-scene)
  ((projection-mat :initform (gficl:make-matrix) :type gficl:matrix)
   (light-dir-view :initform (gficl:make-vec (list 0 0 0)))))

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
			(gficl:vec-ref v j)))))
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
  (compute-shader-reload-files
   (s #p"ssao/ssao.cs")
   shader
   (gl:uniformi (gficl:shader-loc shader "bposition") 1)
   (gl:uniformi (gficl:shader-loc shader "bnormal") 2)
   (gl:uniformi (gficl:shader-loc shader "bnoise") 3)
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

(defmethod draw ((s ssao-shader) (scene ssao-post-scene))
  (with-slots (shader) s
    (gficl:bind-vec shader "screen_res"
		    (gficl:make-vec (list (gficl:window-width) (gficl:window-height))))
    (gficl:bind-matrix shader "proj" (slot-value scene 'projection-mat)))
  (let ((target-tex (get-post-tex scene :ssao :color-attachment0)))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d target-tex)
    (gl:bind-image-texture 0 target-tex 0 nil 0 :write-only :rgba32f))

  ;; position texture
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d
		   (get-post-tex scene :deferred :color-attachment2))
  ;; normal texture
  (gl:active-texture :texture2)
  (gl:bind-texture :texture-2d
		   (get-post-tex scene :deferred :color-attachment3))
  (gl:active-texture :texture3)
  (gficl:bind-gl (slot-value s 'noise-tex))
  (%gl:dispatch-compute (gficl:window-width) (gficl:window-height) 1)
  (%gl:memory-barrier '(:shader-image-access-barrier)))

(defclass ssao-pass (post-pass) ())

(defun make-ssao-pass ()
  (make-instance
   'ssao-pass
   :shaders (list (make-instance 'ssao-shader))
   :description
   (make-framebuffer-description
    (list (gficl:make-attachment-description :type :texture :internal-format :rgba32f)))))

;;; blur pass

(defclass ssao-blur-shader (post-shader) ())

(defmethod reload ((s ssao-blur-shader))
  (compute-shader-reload-files (s #p"ssao/blur.cs") shader
    (gl:uniformi (gficl:shader-loc shader "ssao_buff") 1)))

(defmethod draw ((s ssao-blur-shader) (scene ssao-post-scene))
  (let ((target-tex (get-post-tex scene :blur :color-attachment0)))
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

;;; deferred lighting

(defclass ssao-lighting-shader (post-shader) ())

(defmethod reload ((s ssao-lighting-shader))
  (compute-shader-reload-files (s #p"ssao/post.cs") shader
    (gl:uniformi (gficl:shader-loc shader "bcolour") 1)
    (gl:uniformi (gficl:shader-loc shader "blight") 2)
    (gl:uniformi (gficl:shader-loc shader "bssao") 3)))

(defmethod draw ((s ssao-lighting-shader) (scene ssao-post-scene))
  (let ((target-tex (get-post-tex scene :lighting :color-attachment0))) s
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d target-tex)
    (gl:bind-image-texture 0 target-tex 0 nil 0 :write-only :rgba32f))
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d (get-post-tex scene :deferred :color-attachment0))
  (gl:active-texture :texture2)
  (gl:bind-texture :texture-2d (get-post-tex scene :deferred :color-attachment1))
  (gl:active-texture :texture3)
  (gl:bind-texture :texture-2d (get-post-tex scene :blur :color-attachment0))
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

;;; ssao pipeline 

(defclass ssao-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type ssao-post-scene)))

(defun make-ssao-pipeline ()
  (make-instance
   'ssao-pipeline
   :post-scene (make-instance 'ssao-post-scene)
   :passes (list (cons :shadow
		       (let ((s (make-vsm-pass)))
			 (resize s *default-shadow-map-size* *default-shadow-map-size*) s))
	         (cons :deferred (make-deferred-pass))
		 (cons :ssao (make-ssao-pass))
		 (cons :blur (make-ssao-blur-pass))
		 (cons :lighting (make-ssao-lighting-pass)))))

(defmethod initialize-instance :after ((pl ssao-pipeline) &key &allow-other-keys)
  (setf (slot-value (car (slot-value (get-pass pl :deferred) 'shaders)) 'shadow-map)
	(get-pass-texture (get-pass pl :shadow))))

(defmethod resize ((pl ssao-pipeline) w h)
  (resize (get-pass pl :deferred) w h)
  (resize (get-pass pl :ssao) w h)
  (resize (get-pass pl :blur) w h)
  (resize (get-pass pl :lighting) w h)
  (with-slots (post-scene) pl
    (resize post-scene w h)
    (set-post-texs post-scene (alist-fb-textures pl '(:deferred :ssao :blur :lighting)))))

(defmethod draw ((pl ssao-pipeline) scenes)
  (with-slots (post-scene) pl
    (draw (get-pass pl :shadow) scenes)
    (draw (get-pass pl :deferred) scenes)
    (with-slots (projection-mat) post-scene
      (with-slots ((it-view inverse-transpose-view-mat) (proj projection-mat)) (car scenes)
	(setf projection-mat proj)))
    (draw (get-pass pl :ssao) post-scene)
    (draw (get-pass pl :blur) post-scene)
    (draw (get-pass pl :lighting) post-scene)
    (gficl:blit-framebuffers
     (get-final-framebuffer (get-pass pl :lighting))
     nil
     (gficl:window-width) (gficl:window-height))))
