(in-package :project)

(defclass cascade-post-scene (post-scene)
  ((interval-buffer :initarg :interval-buffer :type gficl:storage-buffer)))

;; initial pass

(defclass cascade-light-shader (normals-cam-shader) ())

(defmethod reload ((s cascade-light-shader))
  (shader-reload-files
   (s (#p"draw.vs" #p"draw.fs") :folder (shader-subfolder #p"cascade/")) shader
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
    (list (gficl:make-attachment-description :position :color-attachment0 :type :texture)
	  (gficl:make-attachment-description :position :color-attachment1 :type :texture)
	  (gficl:make-attachment-description :position :color-attachment2 :type :texture
					     :internal-format :rgba32f)
	  (gficl:make-attachment-description :position :depth-attachment))
    :samples 1)))

(defmethod draw ((obj cascade-colour-pass) scenes)
  (gl:clear-tex-image (cdar (get-textures obj)) 0 :rgba :float #(0 0 0 0))
  (gl:enable :cull-face :depth-test)
  (call-next-method))

;; cascade

(defclass cascade-compute-shader (post-shader)
  ((interval-buffer :type gficl:storage-buffer)
   (init :initform nil)
   (cascade0-width :initform 50)
   (cascade0-height :initform 50)
   (cascade0-depth :initform 50)
   (cascade0-samples :initform 6)))

(defmethod reload ((s cascade-compute-shader))
  (compute-shader-reload-files (s #p"cascade/cascade.cs") shader
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
      (gl:uniformi (gficl:shader-loc shader "dim") w h d samples)
      (gl:uniformi (gficl:shader-loc shader "cascade_level") 0)
      (gl:uniformi (gficl:shader-loc shader "max_cascade_level") 1)
      (if init (gficl:delete-gl interval-buffer)
	(setf init t))
      (setf interval-buffer
	    (gficl:make-storage-buffer
	     :dynamic-copy
	     (* w h d samples
		;; vec4
		(* 4 (cffi:foreign-type-size :float))))))))

(defun get-cascade-dim  (cascade-compute-shader)
  (with-slots ((w cascade0-width)
	       (h cascade0-height)
	       (d cascade0-depth)
	       (samples cascade0-samples))
      cascade-compute-shader
    (list w h d samples)))

(defmethod free ((shader cascade-compute-shader))
	   (gficl:delete-gl (slot-value shader 'interval-buffer))
	   (call-next-method))

(defmethod draw ((shader cascade-compute-shader) (scene cascade-post-scene))
  (with-slots
      ((w cascade0-width) (h cascade0-height) (d cascade0-depth) (samples cascade0-samples))
      shader
    (let ((colour-buff (get-post-tex scene :colour :color-attachment0))
	  (light-buff (get-post-tex scene  :colour :color-attachment1))
	  (depth-buff (get-post-tex scene  :colour :color-attachment2)))
      (gficl:bind-storage-buffer (slot-value shader 'interval-buffer) 0)
      (gl:active-texture :texture0) (gl:bind-texture :texture-2d colour-buff)
      (gl:active-texture :texture1) (gl:bind-texture :texture-2d light-buff)
      (gl:active-texture :texture2) (gl:bind-texture :texture-2d depth-buff)
      (%gl:dispatch-compute (* w samples) h d)
      (%gl:memory-barrier '(:shader-storage-barrier-bit)))))

;; final

(defclass final-cascade-compute-shader (post-shader)
  ((cascade-dim :initarg :cascade-dim)))

(defmethod reload ((s final-cascade-compute-shader))
  (compute-shader-reload-files (s #p"cascade/final.cs") shader
    (gl:uniformi (gficl:shader-loc shader "colour_buff") 1)
    (gl:uniformi (gficl:shader-loc shader "light_buff") 2)
    (gl:uniformi (gficl:shader-loc shader "depth_buff") 3)
    (with-slots ((dim cascade-dim)) s
      (gl:uniformiv (gficl:shader-loc shader "dim")
		    (coerce dim 'vector)))))

(defmethod draw ((shader final-cascade-compute-shader) (scene cascade-post-scene))
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

(defclass final-cascade-pass (post-pass) ())

(defun make-final-cascade-pass (cascade-shader)
  (make-instance
   'final-cascade-pass
   :shaders (list (make-instance 'final-cascade-compute-shader :cascade-dim (get-cascade-dim cascade-shader)))
   :description
   (make-framebuffer-descrption
    (list (gficl:make-attachment-description :type :texture :internal-format :rgba32f)))
   :samples 2))

;; pipeline

(defclass cascade-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type cascade-post-scene)
   (cascade-shader :initarg :cascade-shader :type cascade-compute-shader)))

(defun make-cascade-pipeline ()
  (let ((cascade-shader (make-instance 'cascade-compute-shader)))
    (make-instance
     'cascade-pipeline
     :passes (list (cons :colour (make-cascade-colour-pass))
		   (cons :final (make-final-cascade-pass cascade-shader)))
     :shaders (list (cons :cascade cascade-shader))
     :post-scene
     (make-instance
      'cascade-post-scene
      :interval-buffer (slot-value cascade-shader 'interval-buffer)))))

(defmethod resize ((pl cascade-pipeline) w h)
  (call-next-method)
  (with-slots ((scene post-scene)) pl
    (resize scene w h)
    (set-post-texs scene (alist-fb-textures pl '(:colour :final)))))

(defmethod reload ((pl cascade-pipeline))
  (call-next-method)
  (with-slots ((scene post-scene)) pl
    (setf (slot-value scene 'interval-buffer)
	  (slot-value (get-shader pl :cascade) 'interval-buffer))))

(defmethod draw ((pl cascade-pipeline) scenes)
  (with-slots (post-scene) pl
    (draw (get-pass pl :colour) scenes)
    (draw (get-shader pl :cascade) post-scene)
    (draw (get-pass pl :final) post-scene)
    (gficl:blit-framebuffers
     (get-final-framebuffer (get-pass pl :final)) nil
     (gficl:window-width) (gficl:window-height))))
