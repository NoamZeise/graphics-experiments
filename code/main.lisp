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

  (load-image 'test-tex #p"assets/test.png"))

(defmacro object-matrix (position &optional (size ''(1 1 1)))
  `(gficl:*mat (gficl:translation-matrix ,position)
	       (gficl:scale-matrix ,size)))

(defparameter *meta-tex* nil)
(defparameter *meta-shader* nil)
(defparameter *tex-size* nil)

(defun setup ()
  (load-assets)
  (setf *shader* (gficl/load:shader #p"vert.vs" #p"frag.fs" :shader-folder +shader-folder+))
  (gficl:bind-gl *shader*)

  (setf *meta-shader* (gficl/load:shader #p"vert.vs" #p"metatexture.fs"
					 :shader-folder +shader-folder+))
  (setf *meta-tex* (get-asset 'test-tex))
  (gficl:bind-gl *meta-shader*)
  (gl:uniformi (gficl:shader-loc *meta-shader* "tex") 0)
  
  (setf *scene*
	(list
	 ;(make-object (get-asset 'sphere) (object-matrix '(2 0 1)))
	 (make-object (get-asset 'cube) (object-matrix '(0 0 -2)
					 ;'(0 0 0)
					 ))
	 ;(make-object (get-asset 'cone) (object-matrix '(0 2 -2) '(1 1.5 1)))
	 (make-object (get-asset 'bunny) (object-matrix '(-1 0 1) '(3 3 3)))
	 (make-object (get-asset 'plane)
		      (let* ((size 50) (offset (- (/ size 2))))
			(object-matrix (gficl:make-vec `(,offset -1.2 ,offset))
				       (gficl:make-vec `(,size ,size ,size)))))
	 ))
  (setf *quad* (make-object (get-asset 'plane) (gficl:make-matrix)))
  (setf *tex-size* 100)
  (update-size)
  
  (setf *cam-pos* (gficl:make-vec '(4 0 0)	;'(4 2 4)
		   ))
  (setf *cam-target* (gficl:make-vec '(0 0 0)))
  (resize (gficl:window-width) (gficl:window-height))
  (gl:enable :depth-test))

(defun cleanup ()
  (cleanup-assets)
  (gficl:delete-gl *shader*)
  (gficl:delete-gl *meta-shader*))

(defun resize-callback (w h)
  (setf *projection-mat* (gficl:screen-perspective-matrix w h (* pi 0.3) 0.05))
  (setf *ortho-mat* (gficl:screen-orthographic-matrix w h)))

(defun update-size ()
  (update-model
   *quad*
   (gficl:*mat
    (gficl:translation-matrix (list *tex-size* *tex-size* 0))
    (gficl:scale-matrix (list *tex-size* *tex-size* 1))
    (gficl:make-matrix-from-data
     `((1 0 0 0)
       (0 0 1 0)
       (0 1 0 0)
       (0 0 0 1))))))

(defun update ()
  (gficl:with-update
   (dt)
   (gficl:map-keys-pressed
    (:escape (glfw:set-window-should-close))
    (:f (gficl:toggle-fullscreen)))
   (gficl:map-keys-down
    (:equal (setf *tex-size* (+ *tex-size* (* 100 dt)))
	    (update-size))
    (:minus (setf *tex-size* (- *tex-size* (* 100 dt)))
	    (update-size))
    (:space (setf *cam-pos* (gficl:rotate-vec *cam-pos* (* dt 0.1) *world-up*))))
   (setf *view-mat*
	 (gficl:view-matrix *cam-pos* (gficl:-vec *cam-target* *cam-pos*) *world-up*))
   (gficl:bind-gl *meta-shader*)
   (gficl:bind-matrix *meta-shader* "viewproj" (gficl:*mat *projection-mat* *view-mat*))
   ;;(gficl:bind-vec *shader* "cam" *cam-pos*)
   ))

(defun render ()
  (gficl:with-render
   (gl:clear :color-buffer :depth-buffer)
   (gl:enable :depth-test)
   (gficl:bind-gl *meta-shader*)
   (gl:active-texture :texture0)
   (gficl:bind-gl *meta-tex*) 
   (loop for obj in *scene* do
	 (draw obj *meta-shader*))
   (gl:disable :depth-test)
   (gficl:bind-matrix *meta-shader* "viewproj" *ortho-mat*)
   (draw *quad* *meta-shader*)))

;;; Global Variables

(defparameter *shader* nil)

(defparameter *cam-pos* nil)
(defparameter *cam-target* nil)
(defparameter *view-mat* nil)
(defparameter *projection-mat* nil)

(defparameter *quad* nil)
(defparameter *ortho-mat* nil)

(defparameter *scene* nil)

(defparameter *world-up* (gficl:make-vec'(0 1 0)))

(defconstant +shader-folder+ #p"shaders/")
