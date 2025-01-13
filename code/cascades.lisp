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

(defun make-cascade-colour-pass (cascade-shader)
  (make-instance
   'cascade-colour-pass
   :shaders (list (make-instance 'cascade-light-shader)
		  ;; (make-instance 'cascade-debug-shader
		  ;; 		 :cascade-dim (get-cascade-dim cascade-shader))
		  )
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


;; debug view

(defclass cascade-debug-shader (shader)
  ((cascade-dim :initarg :cascade-dim)))

(defmethod reload ((s cascade-debug-shader))
  (shader-reload-files
   (s (#p"debug_probes.vs" #p"debug_probes.fs") :folder (shader-subfolder #p"cascade/")) shader
   (with-slots ((dim cascade-dim)) s
     (gl:uniformiv (gficl:shader-loc shader "dim")
		   (coerce dim 'vector)))))

(defmethod draw ((obj cascade-debug-shader) scene)
   (gficl:draw-vertex-data (get-asset 'cube)
			   :instances (apply #'* (slot-value obj 'cascade-dim))))

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
   :samples 4))

;; pipeline

(defclass cascade-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type cascade-post-scene)
   (cascade-shader :initarg :cascade-shader :type cascade-compute-shader)))

(defun make-cascade-pipeline ()
  (let ((cascade-shader (make-instance 'cascade-compute-shader)))
    (make-instance
     'cascade-pipeline
     :passes (list (cons :colour (make-cascade-colour-pass cascade-shader))
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


;;; 2d screenspace cascades

(defclass cascade-properties ()
  ((width :initarg :w :initform 100 :type integer)
   (height :initarg :h :initform 100 :type integer)
   (samples :initarg :s :initform 8 :type integer)
   (levels :initarg :levels :initform 1 :type integer)))

(defun cascade-prop-vec (props)
  (with-slots (width height samples levels) props
    (coerce (list width height samples levels) 'vector)))

(defclass cascade-obj ()
  ((cascade-props
    :initarg cascade-props
    :initform (make-instance 'cascade-properties) :type cascade-properties)))

(defgeneric update-cascade-props (obj props))

(defmethod update-cascade-props ((obj cascade-obj) (props cascade-properties))
  (setf (slot-value obj 'cascade-props) props))


(defclass cascade2d-compute-shader (post-shader cascade-obj)
  ((interval-buffer :type gficl:storage-buffer)
   (init :initform nil)))

(defmethod reload ((s cascade2d-compute-shader))
  (compute-shader-reload-files (s #p"cascade/cascade2d.cs") shader
    (gl:uniformi (gficl:shader-loc shader "colour_buff") 0)
    (gl:uniformi (gficl:shader-loc shader "light_buff") 1)
    (gl:uniformi (gficl:shader-loc shader "depth_buff") 2)
    (update-cascade-props s (slot-value s 'cascade-props))))

(defmethod update-cascade-props ((obj cascade2d-compute-shader) (props cascade-properties))
  (call-next-method)	   
  (with-slots (interval-buffer init) obj
    (with-slots (width height samples) props
      (if init (gficl:delete-gl interval-buffer)
	(setf init t))
      (setf interval-buffer
	    (gficl:make-storage-buffer
	     :dynamic-copy
	     (* width height samples
		;; vec4 used to store each sample 
		(* 4 (cffi:foreign-type-size :float))))))))

(defmethod free ((shader cascade2d-compute-shader))
  (gficl:delete-gl (slot-value shader 'interval-buffer))
  (call-next-method))

(defmethod draw ((shader cascade2d-compute-shader) (scene cascade-post-scene))
  (with-slots
      (width height samples levels) (slot-value shader 'cascade-props)
    (let ((colour-buff (get-post-tex scene :colour :color-attachment0))
	  (light-buff (get-post-tex scene  :colour :color-attachment1))
	  (depth-buff (get-post-tex scene  :colour :color-attachment2))
	  (shader-obj (slot-value shader 'shader)))
      (gficl:bind-storage-buffer (slot-value shader 'interval-buffer) 0)
      (gl:active-texture :texture0) (gl:bind-texture :texture-2d colour-buff)
      (gl:active-texture :texture1) (gl:bind-texture :texture-2d light-buff)
      (gl:active-texture :texture2) (gl:bind-texture :texture-2d depth-buff)
      (gl:uniformi (gficl:shader-loc shader-obj "dim") width height samples levels)
      (gl:uniformi (gficl:shader-loc shader-obj "cascade_level") 0)
      (%gl:dispatch-compute width height samples)
      (%gl:memory-barrier '(:shader-storage-barrier-bit))
      (gl:uniformi (gficl:shader-loc shader-obj "cascade_level") -1)
      (%gl:dispatch-compute width height 1)
      (%gl:memory-barrier '(:shader-storage-barrier-bit)))))

;; post shader cascade2d

(defclass cascade2d-post-shader (post-shader cascade-obj) ())

(defmethod reload ((s cascade2d-post-shader))
 (compute-shader-reload-files (s #p"cascade/final2d.cs") shader
			      (gl:uniformi (gficl:shader-loc shader "colour_buff") 1)
			      (gl:uniformi (gficl:shader-loc shader "light_buff") 2)
			      (gl:uniformi (gficl:shader-loc shader "depth_buff") 3)
			      (update-cascade-props s (slot-value s 'cascade-props))))

(defmethod update-cascade-props ((obj cascade2d-post-shader) (props cascade-properties))
  (call-next-method)
  (gl:uniformiv (gficl:shader-loc (slot-value obj 'shader) "dim") (cascade-prop-vec props)))

(defmethod draw ((shader cascade2d-post-shader) (scene cascade-post-scene))
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

(defclass cascade2d-post-pass (post-pass) ())

(defun make-cascade2d-post-pass (cascade-props)
  (make-instance
   'cascade2d-post-pass
   :shaders (list (make-instance 'cascade2d-post-shader :cascade-props cascade-props))
   :description
   (make-framebuffer-descrption
    (list (gficl:make-attachment-description :type :texture :internal-format :rgba32f)))
   :samples 4))

(defmethod update-cascade-props ((obj cascade2d-post-pass) (props cascade-properties))
  (loop for shader in (slot-value obj 'shaders) do
	(update-cascade-props shader props)))

;; 2d cascade pipeline

(defclass cascade-2d-pipeline (pipeline cascade-obj)
  ((post-scene :initarg :post-scene :type cascade-post-scene)))

(defun make-cascade-2d-pipeline ()
  (let* ((props (make-instance 'cascade-properties))
	 (cascade-shader (make-instance 'cascade2d-compute-shader
					:cascade-props props)))
    (make-instance
     'cascade-2d-pipeline
     :passes (list (cons :colour (make-cascade-colour-pass cascade-shader))
		   (cons :final (make-cascade2d-post-pass props)))
     :shaders (list (cons :cascade cascade-shader))
     :post-scene
     (make-instance
      'cascade-post-scene
      :interval-buffer (slot-value cascade-shader 'interval-buffer)))))

(defmethod resize ((pl cascade-2d-pipeline) w h)
  (call-next-method)
  (with-slots ((scene post-scene)) pl
    (resize scene w h)
    (set-post-texs scene (alist-fb-textures pl '(:colour :final)))))

(defmethod reload ((pl cascade-2d-pipeline))
  (call-next-method)
  (with-slots ((scene post-scene)) pl
    (setf (slot-value scene 'interval-buffer)
	  (slot-value (get-shader pl :cascade) 'interval-buffer))))

(defmethod draw ((pl cascade-2d-pipeline) scenes)
  (with-slots (post-scene) pl
    (draw (get-pass pl :colour) scenes)
    ;(draw (get-shader pl :cascade) post-scene)
    ;(draw (get-pass pl :final) post-scene)
    (gficl:blit-framebuffers
     (get-final-framebuffer (get-pass pl :colour)) nil
     (gficl:window-width) (gficl:window-height))))
