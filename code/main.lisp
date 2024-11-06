(in-package :project)

(defun run ()
  (setf trivial-main-thread:*on-error* #'invoke-debugger)
  (trivial-main-thread:with-body-in-main-thread
   ()
   (gficl:with-window
    (:title "project"
	    :resize-callback #'resize-callback
	    :opengl-version-major 4
	    :opengl-version-minor 6)
    (setup)
    (loop until (gficl:closedp)
	  do (update)
	  do (render))
    (cleanup))))

(defun load-assets ()
  (setup-asset-table)
  (load-model 'sphere #p"sphere.obj")
  (load-model 'cube #p"cube.obj")
  (load-model 'cone #p"cone.obj")
  (load-model 'bunny #p"bunny.obj")
  (load-model 'plane #p"plane.obj")
  (add-asset  'dummy-data
	      (gficl:make-vertex-data
	       (gficl:make-vertex-form (list (gficl:make-vertex-slot 1 :int))) '(((0))) '(0 0 0)))
  (load-image 'test #p"assets/test.png")
  (load-image 'metatexture-noise #p"assets/noise.png")	3
  (load-image 'uv #p"assets/uv.png")
  (load-image 'colours #p"assets/colours.png")
  (load-image 'xtoon #p"assets/xtoon.png")
  (load-image 'brush #p"assets/brush-test.png")
  (load-image 'brush2 #p"assets/brush-test2.png"))

(defun create-pipelines ()
  (setf *aos-pipeline* (make-aos-pipeline))
  (setf *outline-pipeline* (make-outline-pipeline))
  (setf *pipelines* (list *outline-pipeline* *aos-pipeline*
			  (make-xtoon-pipeline) (make-brush-pipeline)))
  (setf *active-pipeline* *pipelines*))

(defun create-scenes ()
  (setf *3d-scene* (make-plane-scene))
  (setf *quad-scene* (make-square-scene)))

(defun setup ()
  (init-watched)
  (load-assets)
  (setf *signal-fn* nil)
  (create-pipelines)
  (create-scenes)
  (resize-callback (gficl:window-width) (gficl:window-height))
  (gl:enable :depth-test)
  (gl:front-face :ccw)
  (gl:cull-face :front))
  
(defun cleanup ()
  (loop for p in *pipelines* do (free p))
  (cleanup-assets))

(defun resize-callback (w h)
  (resize *3d-scene* w h)
  (resize *quad-scene* w h)
  (loop for p in *pipelines* do (resize p w h)))

(defun update ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen))
     (:m
      (print *active-pipeline*)
      (setf *active-pipeline* (cdr *active-pipeline*))
      (if (not *active-pipeline*) (setf *active-pipeline* *pipelines*))))
    (update-scene *3d-scene* dt)
    (update-scene *quad-scene* dt)
    (process-watched)
    (cond (*signal-fn*
	   (funcall *signal-fn*)
	   (setf *signal-fn* nil)))
    (cond (*file-change*
	   (setf *file-change* nil)
	   (loop for p in *pipelines*
		 do (reload p))
	   (set-all-unmodified)))))

(defun render ()
  (gficl:with-render
   (draw (car *active-pipeline*) (list *3d-scene* *quad-scene*))))

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

;;; Global Variables

(defparameter *aos-pipeline* nil)
(defparameter *outline-pipeline* nil)
(defparameter *pipelines* nil)

(defparameter *active-pipeline* nil)

(defparameter *3d-scene* nil)
(defparameter *quad-scene* nil)

(defparameter *signal-fn* nil)
