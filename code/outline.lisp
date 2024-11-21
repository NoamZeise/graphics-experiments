(in-package :project)

;; backface metatexture outline

(defclass backface-mt-shader (backface-shader)
  ((polygon-offset :initform -2)
   (normal-divisor :initform 200)
   (near-factor :initform 1.01)
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
		    (make-instance 'backface-shader
				   :outline-colour
				   (gficl:make-vec '(0.03 0.04 0.09 0))))
     :description
     (make-framebuffer-descrption
      (list (gficl:make-attachment-description :type :texture)
	    (gficl:make-attachment-description :position :depth-attachment))
      :samples 16)))

;; mt outline pass

(defclass backface-mt-colour-pass (metatexture-pass) ())

(defun make-backface-mt-colour-pass ()
  (make-instance 'backface-mt-colour-pass
     :shaders (list (make-instance 'backface-mt-shader))
     :description
     (make-framebuffer-descrption
      (list (gficl:make-attachment-description :type :texture))
      :samples 16)))

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
