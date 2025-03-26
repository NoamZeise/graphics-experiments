(in-package :experiments)

(defclass cascade-post-scene (post-scene)
  ((interval-buffer :initarg :interval-buffer :type gficl:storage-buffer)
   (projection-mat :initform (gficl:make-matrix) :type gficl:matrix)))

;;; modifiable cascade parameters

(defclass cascade-properties ()
  ((width :initarg :w :initform 256
	  :type integer)
   (height :initarg :h :initform 256
	   :type integer)
   (samples :initarg :s :initform 8
	    :type integer)
   (levels :initarg :levels :initform 6
	   :type integer)))

(defun cascade-prop-vec (props)
  (with-slots (width height samples levels) props
    (coerce (list width height samples levels) 'vector)))

(defclass cascade-params ()
  ((steps :initarg :steps :initform 5 :type integer)
   (step-size :initarg :step-size :initform 0.05 :type float)
   (merge-rays :initarg :merge :initform t :type boolean)
   (stop-at-level :initarg :stop-at :initform 0 :type integer)
   (trace-after-level :initarg :trace-after :initform 0 :type integer)
   (trace-before-level :initarg :trace-before :initform 0 :type integer)
   (thickness :initarg :thickness :initform 0.1 :type float)
   (debug-view :initarg :debug-view :initform nil :type boolean)))

(defun set-params-in-shader (shader params)
  (with-slots (steps step-size merge-rays thickness trace-after-level trace-before-level) params
    (gl:uniformi (gficl:shader-loc shader "params.steps") steps)
    (gl:uniformf (gficl:shader-loc shader "params.step_size") step-size)
    (gl:uniformi (gficl:shader-loc shader "params.merge_rays") (if merge-rays 1 0))
    (gl:uniformi (gficl:shader-loc shader "params.trace_after") trace-after-level)
    (gl:uniformi (gficl:shader-loc shader "params.trace_before") trace-before-level)
    (gl:uniformf (gficl:shader-loc shader "params.thickness") thickness )))

(defclass cascade-obj ()
  ((cascade-props
    :initarg cascade-props
    :initform (make-instance 'cascade-properties) :type cascade-properties)
   (cascade-params
    :initarg cascade-params
    :initform (make-instance 'cascade-params) :type cascade-params)))

(defgeneric update-cascade-obj (obj props))

(defmethod update-cascade-obj ((obj cascade-obj) (props cascade-properties))
  (setf (slot-value obj 'cascade-props) props))

(defmethod update-cascade-obj ((obj cascade-obj) (params cascade-params))
  (setf (slot-value obj 'cascade-params) params))

;;; cascade shader

(defclass cascade-compute-shader (post-shader cascade-obj)
  ((interval-buffer :type gficl:storage-buffer)
   (init :initform nil)))

(defmethod reload ((s cascade-compute-shader))
  (compute-shader-reload-files (s #p"cascade/cascade.cs") shader
    (gl:uniformi (gficl:shader-loc shader "colour_buff") 0)
    (gl:uniformi (gficl:shader-loc shader "light_buff") 1)
    (gl:uniformi (gficl:shader-loc shader "depth_buff") 2)
    (gl:uniformi (gficl:shader-loc shader "normal_buff") 3)
    (update-cascade-obj s (slot-value s 'cascade-props))
    (update-cascade-obj s (slot-value s 'cascade-params))))

(defmethod update-cascade-obj ((obj cascade-compute-shader) (props cascade-properties))
  (call-next-method)
  (with-slots (interval-buffer init) obj
    (with-slots (width height samples levels) props
      (if init (gficl:delete-gl interval-buffer)
	(setf init t))
      (with-slots (shader) obj
	(gficl:bind-gl shader)
	(gl:uniformi (gficl:shader-loc shader "dim") width height samples levels))
      (setf interval-buffer
	    (gficl:make-storage-buffer
	     :dynamic-copy
	     (* width height samples
		;; copy of buffer for read/write
		2
		;; vec4 used to store each sample 
		(* 4 (cffi:foreign-type-size :float))))))))

(defmethod update-cascade-obj ((obj cascade-compute-shader) (params cascade-params))
  (call-next-method)
  (with-slots (shader) obj
    (gficl:bind-gl shader)
    (set-params-in-shader shader params)))

(defmethod free ((shader cascade-compute-shader))
  (gficl:delete-gl (slot-value shader 'interval-buffer))
  (call-next-method))

(defmethod draw ((shader cascade-compute-shader) (scene cascade-post-scene))
  (with-slots
      (width height samples levels) (slot-value shader 'cascade-props)
    (with-slots (stop-at-level) (slot-value shader 'cascade-params)
      (let* ((factor (expt 2 stop-at-level))
	       (w (/ width factor))
	       (h (/ height factor))
	       (s (* samples factor)))	
	  (let ((colour-buff (get-post-tex scene :deferred :color-attachment0))
		(light-buff (get-post-tex scene  :deferred :color-attachment1))
		(depth-buff (get-post-tex scene  :deferred :color-attachment2))
		(normal-buff (get-post-tex scene  :deferred :color-attachment3))
		(shader-obj (slot-value shader 'shader)))
	    (gficl:bind-storage-buffer (slot-value shader 'interval-buffer) 0)
	    (gl:active-texture :texture0) (gl:bind-texture :texture-2d colour-buff)
	    (gl:active-texture :texture1) (gl:bind-texture :texture-2d light-buff)
	    (gl:active-texture :texture2) (gl:bind-texture :texture-2d depth-buff)
	    (gl:active-texture :texture3) (gl:bind-texture :texture-2d normal-buff)
	    (with-slots (projection-mat) scene
	      (gficl:bind-matrix (slot-value shader 'shader) "projection" projection-mat))
	    (gl:uniformi (gficl:shader-loc shader-obj "dim") w h s levels)
	    (loop for level from (- levels 1) downto stop-at-level
		  for write-other = (mod (+ level 1 (mod stop-at-level 2)) 2)
		  for factor = (expt 2 level) do
		  (progn
		    (gl:uniformi (gficl:shader-loc shader-obj "write_other_buff") write-other)
		    (gl:uniformi (gficl:shader-loc shader-obj "cascade_level") level)
		    (%gl:dispatch-compute
		     (/ width factor)
		     (/ height factor)
		     (* samples factor))
		    (%gl:memory-barrier '(:shader-storage-barrier-bit))))
	    (gl:uniformi (gficl:shader-loc shader-obj "write_other_buff") 0)
	    (gl:uniformi (gficl:shader-loc shader-obj "cascade_level") -1)
	    (%gl:dispatch-compute w h 1)
	    (%gl:memory-barrier '(:shader-storage-barrier-bit)))))))

;; debug shader

(defclass cascade-debug-shader (shader cascade-obj) ())

(defmethod reload ((s cascade-debug-shader))
  (shader-reload-files
   (s (#p"debug.vs" #p"debug.fs") :folder (shader-subfolder #p"cascade/")) shader
   (gl:uniformi (gficl:shader-loc shader "depth_buff") 0)
   (update-cascade-obj s (slot-value s 'cascade-props))))

(defmethod update-cascade-obj ((obj cascade-debug-shader) (props cascade-properties))
  (call-next-method)
  (with-slots (shader) obj
    (gficl:bind-gl shader)
    (gl:uniformiv (gficl:shader-loc shader "dim") (cascade-prop-vec props))))

(defmethod draw ((obj cascade-debug-shader) (scene cascade-post-scene))  
  (with-slots (width height samples levels) (slot-value obj 'cascade-props)
    (with-slots (stop-at-level debug-view) (slot-value obj 'cascade-params)
      (with-slots (projection-mat) scene
	(gficl:bind-matrix (slot-value obj 'shader) "projection" projection-mat))
      (cond
       (debug-view
	(gl:active-texture :texture0)
	(gl:bind-texture :texture-2d (get-post-tex scene :deferred :color-attachment2))
	(let* ((level stop-at-level)
	       (factor (expt 2 level))
	       (w (/ width factor))
	       (h (/ height factor))
	       (s (* samples factor)))
	  (gl:uniformi (gficl:shader-loc (slot-value obj 'shader) "dim") w h s levels)
	  (gficl:bind-storage-buffer (slot-value scene 'interval-buffer) 0)
	  (gficl:draw-vertex-data
	   (get-asset 'cube)
	   :instances
	   (* w h))))))))

;; post shader

(defclass cascade-post-shader (post-shader cascade-obj) ())

(defmethod reload ((s cascade-post-shader))
  (compute-shader-reload-files (s #p"cascade/post.cs") shader
    (gl:uniformi (gficl:shader-loc shader "colour_buff") 1)
    (gl:uniformi (gficl:shader-loc shader "light_buff") 2)
    (gl:uniformi (gficl:shader-loc shader "depth_buff") 3)
    (gl:uniformi (gficl:shader-loc shader "normal_buff") 4)
    (update-cascade-obj s (slot-value s 'cascade-props))))

(defmethod update-cascade-obj ((obj cascade-post-shader) (props cascade-properties))
  (call-next-method)
  (with-slots (shader) obj
    (gficl:bind-gl shader)
    (gl:uniformiv (gficl:shader-loc shader "dim") (cascade-prop-vec props))))

(defmethod draw ((shader cascade-post-shader) (scene cascade-post-scene))
  (with-slots ((w width) (h height) projection-mat) scene
    (let ((target (get-post-tex scene :final :color-attachment0))
	  (colour-buff (get-post-tex scene :deferred :color-attachment0))
	  (light-buff (get-post-tex scene  :deferred :color-attachment1))
	  (depth-buff (get-post-tex scene  :deferred :color-attachment2))
	  (normal-buff (get-post-tex scene  :deferred :color-attachment3)))
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d target)
      (gl:bind-image-texture 0 target 0 nil 0 :read-write :rgba32f)
      (gficl:bind-storage-buffer (slot-value scene 'interval-buffer) 1)      
      (gl:active-texture :texture1) (gl:bind-texture :texture-2d colour-buff)
      (gl:active-texture :texture2) (gl:bind-texture :texture-2d light-buff)
      (gl:active-texture :texture3) (gl:bind-texture :texture-2d depth-buff)
      (gl:active-texture :texture4) (gl:bind-texture :texture-2d normal-buff)
      (with-slots (width height samples levels) (slot-value shader 'cascade-props)
	(let* ((level (slot-value (slot-value shader 'cascade-params) 'stop-at-level))
	       (factor (expt 2 level))
	       (w (/ width factor))
	       (h (/ height factor))
	       (s (* samples factor)))	
	  (gl:uniformi (gficl:shader-loc (slot-value shader 'shader) "dim")
		       w h s levels)))
      (gficl:bind-matrix (slot-value shader 'shader) "projection" projection-mat)
      (%gl:dispatch-compute w h 1)
      (%gl:memory-barrier '(:shader-image-access-barrier)))))

(defclass cascade-post-pass (post-pass) ())

(defun make-cascade-post-pass (cascade-props cascade-params)
  (make-instance
   'cascade-post-pass
   :shaders (list (make-instance 'cascade-post-shader :cascade-props cascade-props :cascade-params cascade-params)
		  (make-instance 'cascade-debug-shader :cascade-props cascade-props :cascade-params cascade-params)
		  )
   :description
   (make-framebuffer-description
    (list (gficl:make-attachment-description :type :texture :internal-format :rgba32f)))
   :samples 4))

(defmethod update-cascade-obj ((obj cascade-post-pass) (props cascade-properties))
  (loop for shader in (slot-value obj 'shaders) do
	(update-cascade-obj shader props)))

(defmethod update-cascade-obj ((obj cascade-post-pass) (params cascade-params))
  (loop for shader in (slot-value obj 'shaders) do
	(update-cascade-obj shader params)))

;; cascade pipeline

(defclass cascade-pipeline (pipeline cascade-obj)
  ((post-scene :initarg :post-scene :type cascade-post-scene)))

(defun make-cascade-pipeline ()
  (let* ((props (make-instance 'cascade-properties))
	 (params (make-instance 'cascade-params))
	 (shadow-pass (make-vsm-pass))
	 (cascade-shader (make-instance 'cascade-compute-shader
					:cascade-props props :cascade-params params)))
    (resize shadow-pass *default-shadow-map-size* *default-shadow-map-size*)
    (make-instance
     'cascade-pipeline
     :passes (list
	      (cons :shadow shadow-pass)
	      (cons :deferred (make-deferred-pass))
	      (cons :final (make-cascade-post-pass props params)))
     :shaders (list (cons :cascade cascade-shader))
     :post-scene
     (make-instance
      'cascade-post-scene
      :interval-buffer (slot-value cascade-shader 'interval-buffer)))))

(defmethod initialize-instance :after ((pl cascade-pipeline) &key &allow-other-keys)
  (setf (slot-value (car (slot-value (get-pass pl :deferred) 'shaders)) 'shadow-map)
	(get-pass-texture (get-pass pl :shadow))))

(defmethod update-cascade-obj ((pl cascade-pipeline) (props cascade-properties))
  (update-cascade-obj (get-pass pl :final) props)
  (update-cascade-obj (get-shader pl :cascade) props)
  (setf (slot-value (slot-value pl 'post-scene) 'interval-buffer)
	(slot-value (get-shader pl :cascade) 'interval-buffer)))

(defmethod update-cascade-obj ((pl cascade-pipeline) (params cascade-params))
  (update-cascade-obj (get-pass pl :final) params)
  (update-cascade-obj (get-shader pl :cascade) params))

(defmethod resize ((pl cascade-pipeline) w h)
  (resize (get-pass pl :deferred) w h)
  (resize (get-pass pl :final) w h)
  (with-slots ((scene post-scene)) pl
    (resize scene w h)
    (set-post-texs scene (alist-fb-textures pl '(:deferred :final)))))

(defmethod reload ((pl cascade-pipeline))
  (call-next-method)
  (with-slots ((scene post-scene)) pl
    (setf (slot-value scene 'interval-buffer)
	  (slot-value (get-shader pl :cascade) 'interval-buffer))))

(defmethod draw ((pl cascade-pipeline) scenes)
  (with-slots (post-scene) pl
    (draw (get-pass pl :shadow) scenes)
    (draw (get-pass pl :deferred) scenes)
    (let ((proj (slot-value (car scenes) 'projection-mat)))
      (setf (slot-value post-scene 'projection-mat) proj))
    (draw (get-shader pl :cascade) post-scene)
    (draw (get-pass pl :final) post-scene)
    (gficl:blit-framebuffers
     (get-final-framebuffer (get-pass pl :final)) nil
     (gficl:window-width) (gficl:window-height))))
