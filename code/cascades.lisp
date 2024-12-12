(in-package :project)

(defclass test-compute-post-scene (post-scene) ())

(defclass test-compute-shader (post-shader)
  ((ssbo :initform 0 :type integer)))

(defmethod reload ((s test-compute-shader))
  (compute-shader-reload-files (s #p"test-compute.cs") shader))

(defmethod resize ((shader test-compute-shader) (w integer) (h integer))
  (with-slots (ssbo) shader
    (if (not (= ssbo 0)) (gl:delete-buffers (list ssbo)))
    (setf ssbo (gl:gen-buffer))
    (gl:bind-buffer :shader-storage-buffer ssbo)
    (cffi:with-foreign-array
     (arr (make-array (* w h)
		      :element-type '(float)
		      :initial-contents
		      (loop for x from w downto 1 nconcing
			    (loop for y from 1 to h collecting
				  (+ (/ x (float w)) (/ y (float h))))))
	  `(:array :float ,(* w h)))		       
     (%gl:buffer-data :shader-storage-buffer (* w h (cffi:foreign-type-size :float))
		      arr :static-read))))

(defmethod draw ((shader test-compute-shader) (scene test-compute-post-scene))
  (with-slots ((w width) (h height)) scene
    (let ((compute-target (get-post-tex scene :compute)))
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d compute-target)
      (gl:bind-image-texture 0 compute-target 0 nil 0 :read-write :rgba32f)
      (%gl:bind-buffer-base :shader-storage-buffer 2 (slot-value shader 'ssbo))
      (%gl:dispatch-compute w h 1)
      (%gl:memory-barrier '(:shader-image-access-barrier)))))

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

(defclass test-compute-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type test-compute-post-scene)))

(defun make-test-compute-pipeline ()
  (make-instance
   'test-compute-pipeline
   :passes (list (cons :compute (make-test-compute-pass)))
   :post-scene (make-instance 'test-compute-post-scene)))

(defmethod resize ((pl test-compute-pipeline) w h)
  (call-next-method)
  (with-slots ((scene post-scene)) pl
    (resize scene w h)
    (set-post-texs scene (alist-fb-textures pl '(:compute)))))

(defmethod draw ((pl test-compute-pipeline) scenes)
  (draw (get-pass pl :compute) (slot-value pl 'post-scene))
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :compute)) nil
   (gficl:window-width) (gficl:window-height)))
