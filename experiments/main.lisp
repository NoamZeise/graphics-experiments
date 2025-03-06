(in-package :experiments)

(defun run ()
  (setf trivial-main-thread:*on-error* #'invoke-debugger)
  (trivial-main-thread:with-body-in-main-thread () (program)))

(defun program ()
  (gficl:with-window
   (:title "experiments"
	   :resize-callback #'resize-callback
	   :opengl-version-major 4
	   :opengl-version-minor 6)
   (setup)
   (loop until (gficl:closedp)
	 do (update-step)
	 do (render))
   (cleanup)))

(defun load-assets ()
  (setup-asset-table)
  (load-model 'sphere #p"sphere.obj")
  (load-model 'cube #p"cube.obj")
  (load-model 'cone #p"cone.obj")
  (load-model 'bunny #p"bunny.obj")
  (load-model 'plane #p"plane.obj")
  ;;(load-model+texs 'street #p"street/street.obj")
  ;;(load-image 'test #p"assets/test.png")
  ;;(load-image 'metatexture-noise #p"assets/noise.png")
  (load-image 'uv #p"assets/uv.png")
  ;;(load-image 'colours #p"assets/colours.png")
  ;;(load-image 'light-colours #p"assets/light-colours.png")
  ;;(load-image 'xtoon #p"assets/xtoon.png")
  ;;(load-image 'brush #p"assets/brush-test.png")
  ;;(load-image 'brush2 #p"assets/brush-test2.png")
  ;;(load-image 'sky-matcap #p"assets/sky-matcap.png")
  ;;(load-image 'rust-matcap #p"assets/rust-matcap.png")
  ;;(load-image 'rim-matcap #p"assets/rim-matcap.png")
  )

(defun get-pipeline (pl)
  (if (listp pl) (car pl) pl))

(defun reused-pipeline (pl)
  (and (listp pl) (equalp :reused (caddr pl))))

(defun create-pipelines ()
  (setf *pipelines*
	(let ((cascade-pipeline (make-cascade-2d-pipeline)))
	  (list
	   ;;(cons "ssao" (make-ssao-pipeline))
	   (cons "cascade2d" cascade-pipeline)
	   (cons "cascade2d-alt"
		 (list cascade-pipeline
		       #'(lambda (pl)
			   (update-cascade-obj
			    pl (make-instance 'cascade-properties :levels 2)))
		       :reused))
	   ;;(cons "cascade3d" (make-cascade3d-pipeline))
	   (cons "pbr" (make-pbr-pipeline))
	   ;;(cons "aos" (make-aos-pipeline))
	   ;; (cons "outline" (make-outline-pipeline))
	   ;; (cons "xtoon" (make-xtoon-pipeline))
	   ;; (cons "brush" (make-brush-pipeline))
	   ;; (cons "halftone" (make-halftone-pipeline))
	   ;; (cons "lit-sphere" (make-lit-sphere-pipeline))
	   )))
  (if (not *active-pipeline*) (setf *active-pipeline* (caar *pipelines*)))

  (setf *performance-analyser*
	(make-performace-analyser
	 *pipelines*
	 *analysed-scenes*)))

(defun create-scenes ()
  (setf *active-scenes*
	(list
	 (make-simple-3d-scene)
	 ;;(make-street-scene)
	 ;;(make-square-scene)
	 ))
  (setf *analysed-scenes*
	(list (cons "basic" *active-scenes*))))

(defun setup ()
  (init-watched)
  (load-assets)
  (setf *signal-fn* nil)
  (setf *active-pipeline* nil)
  (create-scenes)
  (create-pipelines)
  (resize-callback (gficl:window-width) (gficl:window-height))
  (gl:enable :depth-test)
  (gl:front-face :cw)
  (gl:cull-face :front))

(defun cleanup-pipelines ()
  (foreach-al *pipelines* (p)
    (if (not (reused-pipeline p))
	(free (get-pipeline p)))))
  
(defun cleanup ()
  (cleanup-pipelines)
  (cleanup-assets))

(defun resize-callback (w h)
  (loop for scene in *active-scenes* do
	(resize scene w h))
  (foreach-al *pipelines* (p) (resize (get-pipline p) w h)))

(defun update-step ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen t))
     (:m
      (cond ((not *run-performance-analyser*)
	     (setf *active-pipeline*
		   (loop for ((k . _) . r) on *pipelines*
			 when (equalp k *active-pipeline*)
			 return
			 (if r (caar r) (caar *pipelines*))))	     
	     (format t "using ~a pipeline~%" *active-pipeline*))))
     (:p
      (setf *run-performance-analyser* (not *run-performance-analyser*))
      (if *run-performance-analyser*
	  (start-performance-analyser *performance-analyser*)
	(print (end-performance-analyser *performance-analyser*)))))
    ;(format t "fps: ~d~%" (round (/ 1 (float dt))))
    (if *run-performance-analyser*
	(update *performance-analyser* dt)
      (loop for scene in *active-scenes* do
	    (update-scene scene dt)))
    (cond (*signal-fn*
	   (funcall *signal-fn*)
	   (setf *signal-fn* nil)))
    (cond ((process-watched)
	   (foreach-al *pipelines* (p) (reload (get-pipeline p)))
	   (set-all-unmodified)))))

(defun render ()
  (gficl:with-render
   (if *run-performance-analyser*
       (draw *performance-analyser* nil)
     (draw (get-pipline (cdr (assoc *active-pipeline* *pipelines*))) *active-scenes*))))

;;; signal running program functions

(defun signal-quit ()
  (glfw:set-window-should-close))

(defun signal-reload ()
  "manually trigger shader reload"
  (set-all-modified))

(defun signal-fn-lambda (fn)
  (setf *signal-fn* fn))

(defmacro signal-fn (&body body)
  "call fn during next update loop"
  `(signal-fn-lambda (function (lambda () ,@body))))

(defun signal-recreate-scenes ()
  (signal-fn (create-scenes)))

(defun signal-recreate-pipelines ()
  (signal-fn (cleanup-pipelines) (create-pipelines)))

(defun signal-cascade-update (params/props)
  (signal-fn
   (update-cascade-obj (cdr (assoc *active-pipeline* *pipelines*))
		       params/props)))

;;; Global Variables

(defparameter *pipelines* nil)

(defparameter *performance-analyser* nil)

(defparameter *run-performance-analyser* nil)

(defparameter *analysed-scenes* nil)

(defparameter *active-pipeline* nil)

(defparameter *active-scenes* nil)

(defparameter *signal-fn* nil)
