(in-package :project)

(defclass cascade-post-scene (post-scene) ())

(defclass test-compute-shader (post-shader)
  ((ssbo :type gficl:storage-buffer)
   (init :initform nil)))

(defmethod reload ((s test-compute-shader))
  (compute-shader-reload-files (s #p"cascade/test.cs") shader
    (gl:uniformi (gficl:shader-loc shader "colour_buff") 1)
    (gl:uniformi (gficl:shader-loc shader "light_buff") 2)
    (gl:uniformi (gficl:shader-loc shader "depth_buff") 3)))

(defmethod resize ((shader test-compute-shader) (w integer) (h integer))
  (with-slots (ssbo init) shader
    (if init (gficl:delete-gl ssbo)
      (setf init t))
    (setf ssbo
	  (gficl:make-storage-buffer-from-array :static-read :float (* w h)
	    (make-array (* w h)
			:element-type '(float)
			:initial-contents
			(loop for x from w downto 1 nconcing
			      (loop for y from 1 to h collecting
				    (+ (/ x (float w)) (/ y (float h))))))))))

(defmethod draw ((shader test-compute-shader) (scene cascade-post-scene))
  (with-slots ((w width) (h height)) scene
    (let ((compute-target (get-post-tex scene :compute :color-attachment0))
	  (colour-buff (get-post-tex scene :colour :color-attachment0))
	  (light-buff (get-post-tex scene  :colour :color-attachment1))
	  (depth-buff (get-post-tex scene  :colour :depth-attachment)))
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d compute-target)
      (gl:bind-image-texture 0 compute-target 0 nil 0 :read-write :rgba32f)
      (gficl:bind-storage-buffer (slot-value shader 'ssbo) 2)
      (gl:active-texture :texture1) (gl:bind-texture :texture-2d colour-buff)
      (gl:active-texture :texture2) (gl:bind-texture :texture-2d light-buff)
      (gl:active-texture :texture3) (gl:bind-texture :texture-2d depth-buff)
      (%gl:dispatch-compute w h 1)
      (%gl:memory-barrier '(:shader-image-access-barrier)))))

(defmethod free ((shader test-compute-shader))
  (gficl:delete-gl (slot-value shader 'ssbo))
  (call-next-method))

(defclass test-compute-pass (post-pass) ())

(defun make-test-compute-pass ()
  (make-instance
   'test-compute-pass
   :shaders (list (make-instance 'test-compute-shader))
   :description
   (make-framebuffer-descrption
    (list (gficl:make-attachment-description :type :texture :internal-format :rgba32f)))
   :samples 1))

(defmethod resize ((pass test-compute-pass) (w integer) (h integer))
  (call-next-method)
  (resize (car (slot-value pass 'shaders)) w h))

;; initial pass

(defclass cascade-light-shader (normals-cam-shader) ())

(defmethod reload ((s cascade-light-shader))
  (shader-reload-files (s (#p"standard.vs" #p"cascade/light.fs")) shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)))

(defmethod draw ((obj cascade-light-shader) scene)
   (gl:active-texture :texture0)
   (call-next-method))

(defmethod shader-mesh-props ((obj cascade-light-shader) props)
   (with-slots (shader) obj
     (let ((dt (obj-prop props :diffuse))
	   (col (obj-prop props :colour))
	   (light? (obj-prop props :light)))
       (gl:uniformi (gficl:shader-loc shader "use_texture") (if dt 1 0))
       (gl:uniformi (gficl:shader-loc shader "is_light") (if light? 1 0))
       (gficl:bind-vec shader "obj_colour" col)
       (if dt (gficl:bind-gl dt)))))

(defclass cascade-colour-pass (pass) ())

(defun make-cascade-colour-pass ()
  (make-instance
   'cascade-colour-pass
   :shaders (list (make-instance 'cascade-light-shader))
   :description
   (make-framebuffer-descrption
    (list (gficl:make-attachment-description :type :texture)
	  (gficl:make-attachment-description :position :color-attachment1 :type :texture)
	  (gficl:make-attachment-description :position :depth-attachment  :type :texture))
    :samples 16)))

(defmethod draw ((obj cascade-colour-pass) scenes)
  (gl:enable :cull-face :depth-test)
  (call-next-method))

;; cascade

(defclass cascade-compute-shader (post-shader)
  ((interval-buffer :type gficl:storage-buffer)
   (init :initform nil)
   (cascade0-width :initform 100)
   (cascade0-height :initform 100)
   (cascade0-depth :initform 100)
   (cascade0-samples :initform 6)))

(defmethod reload ((s cascade-compute-shader))
  (compute-shader-reload-files (s #p"cascade/cascade.cs") shader
    (gl:uniformi (gficl:shader-loc shader "colour_buff") 0)
    (gl:uniformi (gficl:shader-loc shader "light_buff") 1)
    (gl:uniformi (gficl:shader-loc shader "depth_buff") 2)
    (with-slots
	(interval-buffer
	 init
	 (w cascade0-width) (h cascade0-height) (d cascade0-depth) (samples cascade0-samples))
	s
      (if init (gficl:delete-gl interval-buffer)
	(setf init t))
      (setf interval-buffer
	    (gficl:make-storage-buffer
	     :dynamic-copy
	     (* w h d samples
		;; vec4
		(* 4 (cffi:foreign-type-size :float))))))))

(defmethod free ((shader cascade-compute-shader))
  (gficl:delete-gl (slot-value shader 'interval-buffer))
  (call-next-method))

(defmethod draw ((shader cascade-compute-shader) (scene cascade-post-scene))
  (with-slots
      ((w cascade0-width) (h cascade0-height) (d cascade0-depth) (samples cascade0-samples))
      shader
    (let ((colour-buff (get-post-tex scene :colour :color-attachment0))
	  (light-buff (get-post-tex scene  :colour :color-attachment1))
	  (depth-buff (get-post-tex scene  :colour :depth-attachment)))
      (gficl:bind-storage-buffer (slot-value shader 'interval-buffer) 0)
      (gl:active-texture :texture0) (gl:bind-texture :texture-2d colour-buff)
      (gl:active-texture :texture1) (gl:bind-texture :texture-2d light-buff)
      (gl:active-texture :texture2) (gl:bind-texture :texture-2d depth-buff)
      (%gl:dispatch-compute (* w samples) h d)
      (%gl:memory-barrier '(:shader-image-access-barrier)))))

;; pipeline

(defclass cascade-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type cascade-post-scene)
   (cascade-shader :initarg :cascade-shader :type cascade-compute-shader)))

(defun make-cascade-pipeline ()
  (make-instance
   'cascade-pipeline
   :passes (list (cons :compute (make-test-compute-pass))
		 (cons :colour (make-cascade-colour-pass)))
   :cascade-shader (make-instance 'cascade-compute-shader)
   :post-scene (make-instance 'cascade-post-scene)))

(defmethod resize ((pl cascade-pipeline) w h)
  (call-next-method)
  (with-slots ((scene post-scene)) pl
    (resize scene w h)
    (set-post-texs scene (alist-fb-textures pl '(:compute :colour)))))

(defmethod reload ((pl cascade-pipeline))
  (reload (slot-value pl 'cascade-shader))
  (call-next-method))

(defmethod free ((pl cascade-pipeline))
  (free (slot-value pl 'cascade-shader))
  (call-next-method))

(defmethod draw ((pl cascade-pipeline) scenes)
  (with-slots (post-scene) pl
    (draw (get-pass pl :colour) scenes)
    (draw (slot-value pl 'cascade-shader) post-scene)
    (draw (get-pass pl :compute) post-scene)
    (gficl:blit-framebuffers
     (get-final-framebuffer (get-pass pl :compute)) nil
     (gficl:window-width) (gficl:window-height))))
