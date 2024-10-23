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
  (load-image 'metatexture-noise #p"assets/noise.png")
  (load-image 'uv #p"assets/uv.png"))

(defun setup ()
  (init-watched)
  (load-assets)
  (setf *signal-fn* nil)
  (setf *aos-pipeline* (make-aos-pipeline))
  (setf *outline-pipeline* (make-outline-pipeline))
  (setf *3d-scene* (make-plane-scene))
  (setf *quad-scene* (make-square-scene))
  (resize-callback (gficl:window-width) (gficl:window-height))
  (gl:enable :depth-test))

(defun cleanup ()
  (cleanup-assets)
  (free *aos-pipeline*)
  (free *outline-pipeline*))

(defun resize-callback (w h)
  (resize *3d-scene* w h)
  (resize *quad-scene* w h)
  (resize *aos-pipeline* w h)
  (resize *outline-pipeline* w h))

(defun update ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen)))
    (update-scene *3d-scene* dt)
    (update-scene *quad-scene* dt)
    (process-watched)
    (cond (*signal-fn*
	   (funcall *signal-fn*)
	   (setf *signal-fn* nil)))
    (cond (*file-change*
	   (setf *file-change* nil)
	   (loop for pl in (list *aos-pipeline* *outline-pipeline*)
		 do (reload pl))
	   (set-all-unmodified)))))

(defun render ()
  (gficl:with-render
   (draw *outline-pipeline* (list *3d-scene* *quad-scene*))))

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

;;; Global Variables

(defparameter *aos-pipeline* nil)
(defparameter *outline-pipeline* nil)

(defparameter *3d-scene* nil)
(defparameter *quad-scene* nil)

(defparameter *signal-fn* nil)
