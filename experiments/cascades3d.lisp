;; 3d cascades experiment
(in-package :experiments)

(defclass cascade3d-shader (post-shader)
  ((interval-buffer :type gficl:storage-buffer)
   (init :initform nil)
   (cascade0-width :initform 100)
   (cascade0-height :initform 100)
   (cascade0-depth :initform 100)
   (cascade0-samples :initform 6)))

(defmethod reload ((s cascade3d-shader))
  (compute-shader-reload-files (s #p"cascade3d/cascade.cs") shader
    (gl:uniformi (gficl:shader-loc shader "colour_buff") 0)
    (gl:uniformi (gficl:shader-loc shader "light_buff") 1)
    (gl:uniformi (gficl:shader-loc shader "depth_buff") 2)
    (with-slots
	(interval-buffer
	 init
	 (w cascade0-width)
	 (h cascade0-height)
	 (d cascade0-depth)
	 (samples cascade0-samples))
	s
      (gl:uniformi (gficl:shader-loc shader "max_cascade_level") 1)
      (if init (gficl:delete-gl interval-buffer)
	(setf init t))
      (setf interval-buffer
	    (gficl:make-storage-buffer
	     :dynamic-copy
	     (* w h d samples
		;; vec4
		(* 4 (cffi:foreign-type-size :float))))))))

(defun get-cascade3d-dim  (cascade3d-shader)
  (with-slots ((w cascade0-width)
	       (h cascade0-height)
	       (d cascade0-depth)
	       (samples cascade0-samples))
      cascade3d-shader
    (list w h d samples)))

(defmethod free ((shader cascade3d-shader))
	   (gficl:delete-gl (slot-value shader 'interval-buffer))
	   (call-next-method))

(defmethod draw ((shader cascade3d-shader) (scene cascade-post-scene))
  (with-slots
      ((w cascade0-width) (h cascade0-height) (d cascade0-depth) (samples cascade0-samples))
      shader
    (let ((colour-buff (get-post-tex scene :colour :color-attachment0))
	  (light-buff (get-post-tex scene  :colour :color-attachment1))
	  (depth-buff (get-post-tex scene  :colour :color-attachment2))
	  (shader-obj (slot-value shader 'shader)))
      (gficl:bind-storage-buffer (slot-value shader 'interval-buffer) 0)
      (gl:active-texture :texture0) (gl:bind-texture :texture-2d colour-buff)
      (gl:active-texture :texture1) (gl:bind-texture :texture-2d light-buff)
      (gl:active-texture :texture2) (gl:bind-texture :texture-2d depth-buff)
      (gl:uniformi (gficl:shader-loc shader-obj "dim") w h d samples)
      (gl:uniformi (gficl:shader-loc shader-obj "cascade_level") 0)
      (%gl:dispatch-compute (* w samples) h d)
      (%gl:memory-barrier '(:shader-storage-barrier-bit))
      (gl:uniformi (gficl:shader-loc shader-obj "cascade_level") -1)
      (%gl:dispatch-compute w h d)
      (%gl:memory-barrier '(:shader-storage-barrier-bit)))))

;; final

(defclass final-cascade3d-shader (post-shader)
  ((cascade-dim :initarg :cascade-dim)))

(defmethod reload ((s final-cascade3d-shader))
  (compute-shader-reload-files (s #p"cascade3d/post.cs") shader
    (gl:uniformi (gficl:shader-loc shader "colour_buff") 1)
    (gl:uniformi (gficl:shader-loc shader "light_buff") 2)
    (gl:uniformi (gficl:shader-loc shader "depth_buff") 3)
    (with-slots ((dim cascade-dim)) s
      (gl:uniformiv (gficl:shader-loc shader "dim")
		    (coerce dim 'vector)))))

(defmethod draw ((shader final-cascade3d-shader) (scene cascade-post-scene))
  (with-slots ((w width) (h height)) scene
    (let ((target (get-post-tex scene :final :color-attachment0))
	  (colour-buff (get-post-tex scene :colour :color-attachment0))
	  (light-buff (get-post-tex scene  :colour :color-attachment1))
	  (depth-buff (get-post-tex scene  :colour :color-attachment2)))

      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d target)
      (gl:bind-image-texture 0 target 0 nil 0 :read-write :rgba32f)
      
      (gficl:bind-storage-buffer (slot-value scene 'interval-buffer) 1)
      
      (gl:active-texture :texture1) (gl:bind-texture :texture-2d colour-buff)
      (gl:active-texture :texture2) (gl:bind-texture :texture-2d light-buff)
      (gl:active-texture :texture3) (gl:bind-texture :texture-2d depth-buff)
      (%gl:dispatch-compute w h 1)
      (%gl:memory-barrier '(:shader-image-access-barrier)))))

(defclass cascade3d-post-pass (post-pass) ())

(defun make-cascade3d-post-pass (cascade-shader)
  (make-instance
   'cascade3d-post-pass
   :shaders (list (make-instance 'final-cascade3d-shader :cascade-dim (get-cascade3d-dim cascade-shader)))
   :description
   (make-framebuffer-description
    (list (gficl:make-attachment-description :type :texture :internal-format :rgba32f)))
   :samples 4))

;; pipeline

(defclass cascade3d-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type cascade-post-scene)
   (cascade-shader :initarg :cascade-shader :type cascade3d-shader)))

(defun make-cascade3d-pipeline ()
  (let ((cascade-shader (make-instance 'cascade3d-shader)))
    (make-instance
     'cascade3d-pipeline
     :passes (list (cons :colour (make-cascade-colour-pass))
		   (cons :final (make-cascade3d-post-pass cascade-shader)))
     :shaders (list (cons :cascade cascade-shader))
     :post-scene
     (make-instance
      'cascade-post-scene
      :interval-buffer (slot-value cascade-shader 'interval-buffer)))))

(defmethod resize ((pl cascade3d-pipeline) w h)
  (call-next-method)
  (with-slots ((scene post-scene)) pl
    (resize scene w h)
    (set-post-texs scene (alist-fb-textures pl '(:colour :final)))))

(defmethod reload ((pl cascade3d-pipeline))
  (call-next-method)
  (with-slots ((scene post-scene)) pl
    (setf (slot-value scene 'interval-buffer)
	  (slot-value (get-shader pl :cascade) 'interval-buffer))))

(defmethod draw ((pl cascade3d-pipeline) scenes)
  (with-slots (post-scene) pl
    (draw (get-pass pl :colour) scenes)
    (draw (get-shader pl :cascade) post-scene)
    (draw (get-pass pl :final) post-scene)
    (gficl:blit-framebuffers
     (get-final-framebuffer (get-pass pl :final)) nil
     (gficl:window-width) (gficl:window-height))))
