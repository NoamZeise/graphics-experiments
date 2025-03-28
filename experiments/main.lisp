(in-package :experiments)

(defun run (&key (pipelines :medley))
  (setf *performance-comparison* pipelines)
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
  (load-model+texs 'street #p"street/street.obj")
  (load-image 'metatexture-noise #p"assets/noise.png")
  (load-image 'uv #p"assets/uv.png")
  (load-image 'colours #p"assets/colours.png")
  (load-image 'light-colours #p"assets/light-colours.png")
  ;;(load-image 'test #p"assets/test.png")
  (load-image 'xtoon #p"assets/xtoon.png")
  ;;(load-image 'brush #p"assets/brush-test.png")
  ;;(load-image 'brush2 #p"assets/brush-test2.png")
  ;;(load-image 'sky-matcap #p"assets/sky-matcap.png")
  (load-image 'rust-matcap #p"assets/rust-matcap.png")
  ;;(load-image 'rim-matcap #p"assets/rim-matcap.png")
  )

(defun create-pipelines ()
  (setf *pipelines*
	(ecase *performance-comparison*
	       (:medley (pipeline-medley))
	       (:methods (different-methods))
	       (:levels (levels-comparison))
	       (:resolution (resolution-comparison))
	       (:samples (samples-comparison))
	       (:steps (steps-comparison))))
  (if (not *active-pipeline*) (setf *active-pipeline* (caar *pipelines*)))

  (setf *analyser*
	(make-performace-analyser
	 *pipelines*
	 *scenes*))
  (resize-callback (gficl:window-width) (gficl:window-height)))

(defun create-scenes ()
  (setf *scenes*
	(list ;(cons "basic" (list :scenes (list (make-simple-3d-scene)) :duration 15.0))
	      (cons "street" (list :scenes (list (make-street-scene)) :duration 30.0))))
  (setf *active-scene* (getf (cdr (assoc "street" *scenes*)) :scenes)))

(defun setup ()
  (init-watched)
  (load-assets)
  (setf *signal-fn* nil)
  (setf *active-pipeline* nil)
  (create-scenes)
  (create-pipelines)
  (gl:enable :depth-test)
  (glfw:swap-interval 0) ; disable vsync - better for checking performance
  (gl:front-face :cw)
  (gl:cull-face :front))

(defun cleanup-pipelines ()
  (foreach-al *pipelines* (p) (if (not (reused-pipeline p)) (free (get-pipeline p)))))
  
(defun cleanup ()
  (cleanup-pipelines)
  (cleanup-assets))

(defun resize-callback (w h)
  (loop for scene in *active-scene* do (resize scene w h))
  (foreach-al *pipelines* (p) (resize (get-pipeline p) w h)))

(defun update-step ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     ;; Controls
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen t))
     (:m (if (not *analyser-running?*)
	     (setf *active-pipeline* (switch-active-pipeline *active-pipeline* *pipelines*))))
     (:p (setf *analyser-running?* (not *analyser-running?*))
	 (if *analyser-running?*
	     (start-performance-analyser *analyser*)
	   (format t "ended performance analyser early~%"))))
    ;; Update Scenes
    (if *analyser-running?*
	(setf *analyser-running?*
	      (update-performance-analyser *analyser* dt))
      (loop for scene in *active-scene* do
	    (update-scene scene dt)))
    ;; Handle Signals
    (cond (*signal-fn*
	   (funcall *signal-fn*)
	   (setf *signal-fn* nil)))
    (cond ((process-watched)
	   (foreach-al *pipelines* (p) (reload (get-pipeline p)))
	   (set-all-unmodified)))))

(defun render ()
  (gficl:with-render
   (if *analyser-running?*
       (draw *analyser* nil)
     (draw (get-pipeline (cdr (assoc *active-pipeline* *pipelines*))) *active-scene*))))

;;; update helpers

(defun switch-active-pipeline (active pipelines)
  (setf active
	(loop for ((k . _) . r) on pipelines
	      when (equalp k active)
	      return (if r (caar r) (caar pipelines))))     
  (format t "using ~a pipeline~%" active)
  (init-pipeline (assoc active pipelines))
  active)

(defun update-performance-analyser (pa dt)
  "returns nil when performance analyser has finished running."
  (update pa dt)
  (cond ((finished pa)
	 (let ((filename "performance"))
	   (format t "finished performance test, saving to ~a~%" filename)
	   ;; save results as tex file
	   (save-performance-table (make-pathname :name filename :type "tex") pa)
	   ;; save results as csv file
	   (save-performance-table
	    (make-pathname :name filename :type "csv") pa
	    :seperator "," :format-string "~{~*~}~{~a~^,~}~%~{~a~^~%~}"))
	 nil)
	;; return t if not finished
	(t t)))

;;; signal running process from repl

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
   (update-cascade-obj (get-pipeline (cdr (assoc *active-pipeline* *pipelines*)))
		       params/props)))

;;; global variables

(defparameter *performance-comparison* :methods)

(defparameter *scenes* nil)
(defparameter *active-scene* nil)

(defparameter *pipelines* nil)
(defparameter *active-pipeline* nil)

(defparameter *analyser* nil)

(defparameter *analyser-running?* nil)

(defparameter *signal-fn* nil)
