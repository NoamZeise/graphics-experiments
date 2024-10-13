(in-package :project)

(defun run ()
  (gficl:with-window
   (:title "project"
    :resize-callback #'resize-callback
    :opengl-version-major 4
    :opengl-version-minor 6)
   (setup)
   (loop until (gficl:closedp)
	 do (update)
	 do (render))
   (cleanup)))

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
  (load-image 'metatexture-noise #p"assets/test.png"))

(defun setup ()
  (load-assets)

  (setf *basic-pass* (make-basic-pass))
  (setf *metatexture-pass* (make-metatexture-pass))

  (setf *aos-pipeline* (make-aos-pipeline))
  
  (setf *3d-scene* (make-plane-scene))
  (setf *quad-scene* (make-square-scene))
  
  (resize-callback (gficl:window-width) (gficl:window-height))
  (gl:enable :depth-test))

(defun cleanup ()
  (cleanup-assets)
  (free *metatexture-pass*)
  (free *basic-pass*)
  (free *aos-pipeline*))

(defun resize-callback (w h)
  (resize *metatexture-pass* w h)
  (resize *basic-pass* w h)
  (resize *3d-scene* w h)
  (resize *quad-scene* w h)
  (resize *aos-pipeline* w h))

(defun update ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen)))
   
    (update-scene *3d-scene* dt)
    (update-scene *quad-scene* dt)))

(defun render ()
  (gficl:with-render
   (draw *aos-pipeline* (list *3d-scene* *quad-scene*)))
  ;; (let ((pass *metatexture-pass*))
  ;;    (draw pass (list *3d-scene* *quad-scene*))
  ;;    (gficl:blit-framebuffers
  ;;     (get-final-framebuffer pass) nil (gficl:window-width) (gficl:window-height))))
  )

;;; Global Variables

(defparameter *metatexture-pass* nil)
(defparameter *basic-pass* nil)

(defparameter *aos-pipeline* nil)

(defparameter *3d-scene* nil)
(defparameter *quad-scene* nil)
