(in-package :project)

(defclass test-compute-post-scene (post-scene) ())

(defclass test-compute-shader (post-shader)
  ((ssbo :type gficl:storage-buffer)
   (init :initform nil)))

(defmethod reload ((s test-compute-shader))
  (compute-shader-reload-files (s #p"test-compute.cs") shader
    (gl:uniformi (gficl:shader-loc shader "colour_buff") 1)
    (gl:uniformi (gficl:shader-loc shader "depth_buff") 2)))

(defmethod resize ((shader test-compute-shader) (w integer) (h integer))
  (with-slots (ssbo init) shader
    (if init (gficl:delete-gl ssbo)
      (setf init t))
    (setf ssbo
	  (gficl:make-storage-buffer-from-array (* w h) :float :static-read
	    (make-array (* w h)
			:element-type '(float)
			:initial-contents
			(loop for x from w downto 1 nconcing
			      (loop for y from 1 to h collecting
				    (+ (/ x (float w)) (/ y (float h))))))))))

(defmethod draw ((shader test-compute-shader) (scene test-compute-post-scene))
  (with-slots ((w width) (h height)) scene
    (let ((compute-target (get-post-tex scene :compute))
	  (buffs (get-post-tex scene :colour)))
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d compute-target)
      (gl:bind-image-texture 0 compute-target 0 nil 0 :read-write :rgba32f)
      (gficl:bind-storage-buffer (slot-value shader 'ssbo) 2)
      (gl:active-texture :texture1)
      (gl:bind-texture :texture-2d (car buffs))
      (gl:active-texture :texture2)
      (gl:bind-texture :texture-2d (cadr buffs))
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
       (cond (dt (gficl:bind-gl dt))))))

(defclass cascade-colour-pass (pass) ())

(defun make-cascade-colour-pass ()
  (make-instance
   'cascade-colour-pass
   :shaders (list (make-instance 'cascade-light-shader))
   :description
   (make-framebuffer-descrption
    (list (gficl:make-attachment-description :type :texture)
	  (gficl:make-attachment-description :position :depth-attachment :type :texture)))
   :samples 16))

(defmethod draw ((obj cascade-colour-pass) scenes)
  (gl:enable :cull-face :depth-test)
  (call-next-method))

(defclass test-compute-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type test-compute-post-scene)))

(defun make-test-compute-pipeline ()
  (make-instance
   'test-compute-pipeline
   :passes (list (cons :compute (make-test-compute-pass))
		 (cons :colour (make-cascade-colour-pass)))
   :post-scene (make-instance 'test-compute-post-scene)))

(defmethod resize ((pl test-compute-pipeline) w h)
  (call-next-method)
  (with-slots ((scene post-scene)) pl
    (resize scene w h)
    (set-post-texs scene (alist-fb-textures pl '(:compute :colour)))))

(defmethod draw ((pl test-compute-pipeline) scenes)
  (draw (get-pass pl :colour) scenes)
  (draw (get-pass pl :compute) (slot-value pl 'post-scene))
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :compute)) nil
   (gficl:window-width) (gficl:window-height)))
