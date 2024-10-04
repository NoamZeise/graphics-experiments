(in-package :project)

(defun run ()
  (gficl:with-window
   (:title "project"
    :resize-callback #'resize
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
  (load-model 'plane #p"plane.obj"))

(defmacro object-matrix (position &optional (size ''(1 1 1)))
  `(gficl:*mat (gficl:translation-matrix ,position)
	       (gficl:scale-matrix ,size)))

(defun setup ()
  (load-assets)
  (setf *shader* (gficl/load:shader #p"vert.vs" #p"frag.fs" :shader-folder +shader-folder+))
  (gficl:bind-gl *shader*)
  
  (setf *scene*
	(list
	 (make-object (get-asset 'sphere) (object-matrix '(2 0 1)))
	 (make-object (get-asset 'cube) (object-matrix '(0 0 -2)))
	 (make-object (get-asset 'cone) (object-matrix '(0 2 -2) '(1 1.5 1)))
	 (make-object (get-asset 'bunny) (object-matrix '(-1 0 1) '(3 3 3)))
	 (make-object (get-asset 'plane)
		      (let* ((size 50) (offset (- (/ size 2))))
			(object-matrix (gficl:make-vec `(,offset -1.2 ,offset))
				       (gficl:make-vec `(,size ,size ,size)))))))
  
  (setf *cam-pos* (gficl:make-vec '(4 2 4)))
  (setf *cam-target* (gficl:make-vec '(0 0 0)))
  (resize (gficl:window-width) (gficl:window-height))
  (gl:enable :depth-test))

(defun cleanup ()
  (cleanup-assets)
  (gficl:delete-gl *shader*))

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

(defun render ()
  (gficl:with-render
   (gl:clear :color-buffer :depth-buffer)
   (loop for obj in *scene* do (draw obj *shader*))))

;; Global Variables

(defparameter *shader* nil)

(defparameter *cam-pos* nil)
(defparameter *cam-target* nil)
(defparameter *view-mat* nil)
(defparameter *projection-mat* nil)

(defparameter *scene* nil)

(defparameter *world-up* (gficl:make-vec'(0 1 0)))

(defconstant +shader-folder+ #p"shaders/")
