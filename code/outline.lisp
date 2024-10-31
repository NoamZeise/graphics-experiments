(in-package :project)

;; backface shader

(defclass backface-shader (normals-cam-shader)
  ((polygon-offset :initform -2)))

(defmethod reload ((s backface-shader))
  (let ((folder (merge-pathnames #p"outline/" (merge-pathnames +shader-folder+)))
	(files (list #p"backface.vs" #p"backface.fs")))
    (shader-reload-files (s files :folder folder)
      (let ((shader (gficl/load:shader (car files) (cadr files) :shader-folder folder)))
	(setf (slot-value s 'shader) shader)))))

(defmethod draw ((obj backface-shader) scene)
  (gl:enable :depth-test :cull-face :polygon-offset-fill)
  (gl:cull-face :back)
  (gl:polygon-offset (slot-value obj 'polygon-offset) -10)
  (gl:depth-func :lequal)
  (call-next-method)
  (gl:polygon-offset 0 0)
  (gl:depth-func :less)
  (gl:cull-face :front)
  (gl:disable :polygon-offset-fill))

;; dont draw outline for 2d scenes
(defmethod draw ((obj backface-shader) (scene scene-2d)))

(defmethod shader-scene-props ((obj backface-shader) (scene scene-3d))
  (gficl:bind-matrix (slot-value obj 'shader) "viewproj"
    (with-slots (cam-pos cam-target (fov cam-fov) (near cam-near)) scene
      (gficl:*mat
       (gficl:screen-perspective-matrix (gficl:window-width) (gficl:window-height)
					(* pi fov) (* near 1.005) 100)
       (gficl:view-matrix cam-pos (gficl:-vec cam-target cam-pos) +world-up+)))))

;; backface pass

(defclass backface-mt-shader (backface-shader)
  ((polygon-offset :initform -100)))

(defmethod reload ((s backface-mt-shader))
  (let ((files (list #p"outline/backface.vs" #p"metatexture/metatexture.fs")))
    (shader-reload-files (s files :folder +shader-folder+)
      (let ((shader (gficl/load:shader (car files) (cadr files) :shader-folder +shader-folder+)))
	(gficl:bind-gl shader)
	(gl:uniformi (gficl:shader-loc shader "tex") 0)
	(gficl:bind-vec shader "tex_dim"
          (list (get-asset-prop 'metatexture-noise :width)
		(get-asset-prop 'metatexture-noise :height)))
	(setf (slot-value s 'shader) shader)))))

(defmethod draw ((obj backface-mt-shader) scene)
  (gficl:bind-gl (get-asset 'metatexture-noise))
  (setf (slot-value obj 'polygon-offset) -30)
  (call-next-method))

;; colour + backfaces pass

(defclass backface-colour-pass (pass) ())

(defun make-backface-colour-pass ()
  (make-instance 'backface-colour-pass
     :shaders (list (make-instance 'cel-shader)
		    (make-instance 'backface-shader)
		    )
     :description
     (make-framebuffer-descrption
      (list (gficl:make-attachment-description :type :texture)
	    (gficl:make-attachment-description :position :depth-attachment))
      :samples 16)))

;; mt outline pass

(defclass backface-mt-colour-pass (pass) ())

(defun make-backface-mt-colour-pass ()
  (make-instance 'backface-mt-colour-pass
     :shaders (list (make-instance 'backface-mt-shader))
     :description
     (make-framebuffer-descrption
      (list (gficl:make-attachment-description :type :texture))
      :samples 16)))

(defclass outline-post-scene (post-scene) ())

;; pipeline

(defclass outline-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type outline-post-scene)))

(defun make-outline-pipeline ()
  (make-instance 'outline-pipeline
    :post-scene (make-instance 'outline-post-scene)
    :passes (list (cons :col (make-backface-colour-pass))
		  (cons :mt (make-backface-mt-colour-pass))
		  (cons :mt-post (make-mt-post-pass)))))

(defmethod resize ((pl outline-pipeline) (w integer) (h integer))
  (call-next-method)
  (with-slots (framebuffer) (get-pass pl :mt)
    (gficl:bind-gl framebuffer)
    (with-slots ((col-fb framebuffer)) (get-pass pl :col)
      (loop for a in (gficl::attachments col-fb) 
	    when (eql :depth-attachment (gficl::attachment-position a))
	    do (gficl::attach-to-framebuffer a))))
  (with-slots ((scene post-scene)) pl
    (resize scene w h)
    (set-post-texs scene (alist-fb-textures pl '(:mt :col)))))

(defmethod draw ((pl outline-pipeline) scenes)
  (draw (get-pass pl :col) scenes)
  (draw (get-pass pl :mt) scenes)
  (draw (get-pass pl :mt-post) (slot-value pl 'post-scene))
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :mt-post)) nil
   (gficl:window-width) (gficl:window-height)))
