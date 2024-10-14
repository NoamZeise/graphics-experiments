(in-package :project)

;;; Metatexture Noise Pass

(defclass metatexture-shader (normals-shader)
  ())

(defun make-metatexture-shader ()
  (let ((shader (gficl/load:shader #p"metatexture.vs" #p"metatexture.fs" :shader-folder +shader-folder+)))
    (gficl:bind-gl shader)
    (gl:uniformi (gficl:shader-loc shader "tex") 0)
    (gficl:bind-vec shader "tex_dim"
		    (list (get-asset-prop 'metatexture-noise :width)
			  (get-asset-prop 'metatexture-noise :height)))
    (make-instance 'metatexture-shader :shader shader)))

(defmethod draw ((obj metatexture-shader) scene)
  (gl:enable :depth-test)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'metatexture-noise))
  (call-next-method))

(defclass metatexture-pass (pass)
  ())

(defun make-metatexture-pass ()
  (make-instance
   'metatexture-pass
   :shaders (list (make-metatexture-shader))
   :description
   (make-framebuffer-descrption
    :attachments
    (list (gficl:make-attachment-description :type :texture)
	  (gficl:make-attachment-description :position :depth-attachment))
    :samples (msaa-samples 8))))

;;; Metatexture Post Processing Pass

(defclass metatexture-post-shader (post-shader)
  ())

(defun make-metatexture-post-shader ()
  (let ((shader (gficl/load:shader
		 #p"metatex-post.vs" #p"metatex-post.fs" :shader-folder +shader-folder+)))
    (gficl:bind-gl shader)
    (gl:uniformi (gficl:shader-loc shader "mt") 0)
    (gl:uniformi (gficl:shader-loc shader "col") 1)
    (make-instance 'metatexture-post-shader :shader shader)))

(defmethod draw ((s metatexture-post-shader) pt-alist)
  (gficl:bind-matrix (slot-value s 'shader) "transform" (cdr (assoc :transform pt-alist)))
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (cadr (assoc :mt pt-alist)))
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d (cadr (assoc :col pt-alist)))
  (call-next-method))

(defclass metatexture-post-pass (pass)
  ((post-transform :initform (gficl:make-matrix))))

(defun make-metatexture-post-pass ()
  (make-instance 'metatexture-post-pass
		 :shaders (list (make-metatexture-post-shader))
		 :description
		 (make-framebuffer-descrption
		  :attachments
		  (list (gficl:make-attachment-description)))))

(defmethod resize ((pass metatexture-post-pass) w h)
	   ;; (setf (slot-value pass 'post-transform)
	   ;; 	 (gficl:target-resolution-matrix w h w h))
	   (call-next-method))

(defmethod draw ((pass metatexture-post-pass) tex-alist)
	   (gl:disable :depth-test)
	   (draw (car (slot-value pass 'shaders))
		 (cons (cons :transform (slot-value pass 'post-transform))
		       tex-alist)))

;;; Pipline

(defclass aos-pipeline (pipeline)
  ())

(defun make-aos-pipeline ()
  (make-instance 'aos-pipeline
		 :passes (list (cons :mt (make-metatexture-pass))
			       (cons :col (make-basic-pass))
			       (cons :post (make-metatexture-post-pass)))))

(defmethod draw ((pl aos-pipeline) scenes)
	   (draw (get-pass pl :mt) scenes)
	   (draw (get-pass pl :col) scenes)
	   (draw (get-pass pl :post)
		 (list (cons :mt (get-textures (get-pass pl :mt)))
		       (cons :col (get-textures (get-pass pl :col)))))
	   (gficl:blit-framebuffers
	    (get-final-framebuffer (get-pass pl :post))
	    nil (gficl:window-width) (gficl:window-height)))
