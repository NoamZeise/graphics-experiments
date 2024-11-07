(in-package :project)

;; backface shader

(defclass backface-shader (normals-cam-shader)
  ((polygon-offset :initarg :polygon-offset :initform -2)
   (normal-divisor :initarg :normal-divisor :initform 200.0 :type float)
   (near-factor :initarg :near-factor :initform 1.005)
   (shader-folder :initform (merge-pathnames #p"outline/" (merge-pathnames +shader-folder+)))
   (vert-shader :initform #p"backface.vs")
   (frag-shader :initform #p"backface.fs")
   (outline-colour :initarg :outline-colour
		   :initform (gficl:make-vec '(1 1 1 1)) :type gficl:vec)
   (outline-size :initarg :outline-size
		 :initform 1.0 :type float)))

(defmethod reload ((s backface-shader))
  (with-slots (vert-shader frag-shader shader-folder) s
    (let ((files (list vert-shader frag-shader)))
      (shader-reload-files (s files :folder shader-folder)
        (let ((shader (gficl/load:shader (car files) (cadr files) :shader-folder shader-folder)))
	  (gficl:bind-gl shader)
	  (gl:uniformf (gficl:shader-loc shader "normal_divisor")
		       (slot-value s 'normal-divisor))
	  (setf (slot-value s 'shader) shader))))))

(defmethod draw ((obj backface-shader) scene)
  (with-slots (shader outline-colour outline-size) obj
    (gficl:bind-vec shader "outline" outline-colour)
    (gl:enable :depth-test :cull-face :polygon-offset-fill)
    (gl:cull-face :back)
    (gl:polygon-offset (slot-value obj 'polygon-offset) (* -10 outline-size))
    (gl:depth-func :lequal)
    (call-next-method)
    (gl:polygon-offset 0 0)
    (gl:depth-func :less)
    (gl:cull-face :front)
    (gl:disable :polygon-offset-fill)))

;; dont draw outline for 2d scenes
(defmethod draw ((obj backface-shader) (scene scene-2d)))

(defmethod shader-scene-props ((obj backface-shader) (scene scene-3d))
  (if (and (> (gficl:window-width) 0) (> (gficl:window-height) 0))
    (gficl:bind-matrix (slot-value obj 'shader) "viewproj"
      (with-slots (cam-pos cam-target (fov cam-fov) (near cam-near)) scene
	(gficl:*mat
	 (gficl:screen-perspective-matrix (gficl:window-width) (gficl:window-height)
					  (* pi fov) (* near (slot-value obj 'near-factor)) 100)
	 (gficl:view-matrix cam-pos (gficl:-vec cam-target cam-pos) +world-up+))))))

;; backface metatexture outline

(defclass backface-mt-shader (backface-shader)
  ((polygon-offset :initform -10)
   (normal-divisor :initform 80)
   (near-factor :initform 1.02)
   (shader-folder :initform +shader-folder+)
   (vert-shader :initform #p"outline/backface.vs")
   (frag-shader :initform #p"metatexture/metatexture.fs")))

(defmethod reload ((s backface-mt-shader))
  (call-next-method)
  (gficl:bind-gl (slot-value s 'shader))
  (gficl:bind-vec (slot-value s 'shader) "tex_dim"
		  (list (get-asset-prop 'metatexture-noise :width)
			(get-asset-prop 'metatexture-noise :height))))

(defmethod draw ((obj backface-mt-shader) scene)
  (gficl:bind-gl (get-asset 'metatexture-noise))
  (call-next-method))

;; colour + backfaces pass

(defclass backface-colour-pass (pass) ())

(defun make-backface-colour-pass ()
  (make-instance 'backface-colour-pass
     :shaders (list (make-instance 'cel-shader)
		    (make-instance 'backface-shader))
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

(defmethod draw :before ((obj backface-mt-colour-pass) scenes)
  (gl:clear-color 0.5 0.5 0.5 0))

(defmethod draw :after ((obj backface-mt-colour-pass) scenes)
  (gl:clear-color 0 0 0 0))

;; outline mt post shader

(defclass outline-mt-post-shader (mt-post-shader)
  ())

(defmethod reload ((s outline-mt-post-shader))
  (call-next-method)
  (gl:uniformf (gficl:shader-loc (slot-value s 'shader) "offset_intensity") 0.02))

(defclass outline-mt-post-pass (post-pass) ())

(defun make-outline-mt-post-pass ()
  (make-instance
   'outline-mt-post-pass
   :shaders (list (make-instance 'outline-mt-post-shader))
   :description
   (make-framebuffer-descrption (list (gficl:make-attachment-description)))))

(defclass outline-post-scene (post-scene) ())

;; pipeline

(defclass outline-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type outline-post-scene)))

(defun make-outline-pipeline ()
  (make-instance 'outline-pipeline
    :post-scene (make-instance 'outline-post-scene)
    :passes (list (cons :col (make-backface-colour-pass))
		  (cons :mt (make-backface-mt-colour-pass))
		  (cons :mt-post (make-outline-mt-post-pass)))))

(defmethod resize ((pl outline-pipeline) (w integer) (h integer))
  (call-next-method)
  (with-slots (framebuffer) (get-pass pl :mt)
    (with-slots ((col-fb framebuffer)) (get-pass pl :col)
      (gficl:framebuffer-add-external-attachment framebuffer col-fb :depth-attachment)))
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
