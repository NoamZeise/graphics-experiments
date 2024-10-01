(in-package :project)

(defun run ()
  (gficl:with-window
   (:title "project" :resize-callback #'resize)
   (setup)
   (loop until (gficl:closedp)
	 do (update)
	 do (draw))
   (cleanup)))

(defun setup ()
  (setup-assets)
  (setf *shader* (gficl/load:shader #p"vert.vs" #p"frag.fs" :shader-folder +shader-folder+))
  (gficl:bind-gl *shader*)
  (setf *sphere* (car (get-asset 'sphere)))
  (setf *cam-pos* (gficl:make-vec '(2 0 2)))
  (setf *cam-target* (gficl:make-vec '(0 0 0)))  
  (resize (gficl:window-width) (gficl:window-height))
  (gl:enable :depth-test))

(defun cleanup ()
  (gficl:delete-gl *shader*)
  (gficl:delete-gl *sphere*))

(defun resize (w h)
  (setf *projection-mat* (gficl:screen-perspective-matrix w h (* pi 0.3) 0.05)))

(defun update ()
  (gficl:with-update
   (dt)
   (gficl:map-keys-pressed
    (:escape (glfw:set-window-should-close))
    (:f (gficl:toggle-fullscreen)))
   (setf *cam-pos* (gficl:rotate-vec *cam-pos* (* dt 0.1) *world-up*))
   (setf *view-mat*
	 (gficl:view-matrix *cam-pos* (gficl:-vec *cam-target* *cam-pos*) *world-up*))
   (gficl:bind-matrix *shader* "viewproj" (gficl:*mat *projection-mat* *view-mat*))
   (gficl:bind-vec *shader* "cam" *cam-pos*)))

(defun draw ()
  (gficl:with-render
   (gl:clear :color-buffer :depth-buffer)
   (let* ((model (gficl:make-matrix))
	  (norm (gficl:transpose-matrix (gficl:inverse-matrix model))))
     (gficl:bind-matrix *shader* "model" model)
     (gficl:bind-matrix *shader* "norm_mat" norm)
     (gficl:draw-vertex-data *sphere*))))

;; Global Variables

(defparameter *shader* nil)

(defparameter *cam-pos* nil)
(defparameter *cam-target* nil)
(defparameter *view-mat* nil)
(defparameter *projection-mat* nil)

(defparameter *sphere* nil)

(defparameter *world-up* (gficl:make-vec'(0 1 0)))

(defconstant +shader-folder+ #p"shaders/")
