(in-package :project)

;;; Colour Pass

(defclass mt-colour-shader (normals-shader) ())

(defmethod reload ((s mt-colour-shader))
  (let ((shader (gficl/load:shader #p"vert.vs" #p"frag.fs" :shader-folder +shader-folder+)))
    (gficl:bind-gl shader)
    (gl:uniformi (gficl:shader-loc shader "tex") 0)
    (setf (slot-value s 'shader) shader)))

(defmethod draw ((obj mt-colour-shader) scene)
  (gl:enable :depth-test)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'uv))
  (call-next-method))

(defclass mt-colour-pass (pass) ())

(defun make-mt-colour-pass ()
  (make-instance
   'mt-colour-pass
   :shaders (list (make-instance 'mt-colour-shader))
   :description
   (make-framebuffer-descrption
    :attachments
    (list (gficl:make-attachment-description :type :texture)
	  (gficl:make-attachment-description :position :depth-attachment))
    :samples (msaa-samples 16))))

;;; Metatexture Pass

(defclass metatexture-shader (normals-shader) ())

(defmethod reload ((s metatexture-shader))
  (let ((shader (gficl/load:shader
		 #p"metatexture.vs" #p"metatexture.fs" :shader-folder +shader-folder+)))
    (gficl:bind-gl shader)
    (gl:uniformi (gficl:shader-loc shader "tex") 0)
    (gficl:bind-vec shader "tex_dim"
		    (list (get-asset-prop 'metatexture-noise :width)
			  (get-asset-prop 'metatexture-noise :height)))
    (setf (slot-value s 'shader) shader)))

(defmethod draw ((obj metatexture-shader) scene)
  (gl:enable :depth-test)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'metatexture-noise))
  (call-next-method))

(defclass metatexture-pass (pass) ())

(defun make-metatexture-pass ()
  (make-instance
   'metatexture-pass
   :shaders (list (make-instance 'metatexture-shader))
   :description
   (make-framebuffer-descrption
    :attachments
    (list (gficl:make-attachment-description :type :texture)
	  (gficl:make-attachment-description :position :depth-attachment))
    :samples (msaa-samples 16))))

;;; Post Processing Pass

(defclass mt-post-shader (post-shader)
  ())

(defmethod reload ((s mt-post-shader))
  (let ((shader (gficl/load:shader
		 #p"metatex-post.vs" #p"metatex-post.fs" :shader-folder +shader-folder+)))
    (gficl:bind-gl shader)
    (gl:uniformi (gficl:shader-loc shader "mt") 0)
    (gl:uniformi (gficl:shader-loc shader "col") 1)
    (setf (slot-value s 'shader) shader)))

(defmethod shader-scene-props ((s mt-post-shader) (scene post-scene))
  (with-slots (transform) scene
    (gficl:bind-gl (slot-value s 'shader))
    (gficl:bind-matrix (slot-value s 'shader) "transform" transform)
    (gl:active-texture :texture0)    
    (gl:bind-texture :texture-2d (get-post-tex scene :mt))
    (gl:active-texture :texture1)
    (gl:bind-texture :texture-2d (get-post-tex scene :col))))

(defclass mt-post-pass (pass) ())

(defun make-mt-post-pass ()
  (make-instance
   'mt-post-pass
   :shaders (list (make-instance 'mt-post-shader))		
   :description
   (make-framebuffer-descrption
    :attachments
    (list (gficl:make-attachment-description)))))

(defmethod resize ((pass mt-post-pass) w h)	   
  (call-next-method))

(defmethod draw ((pass mt-post-pass) (scene post-scene))
  (with-slots (shaders) pass
    (gl:disable :depth-test)
    (loop for shader in	shaders do (draw shader scene))))

;;; Post Scene

(defclass mt-post-scene (post-scene) ())

;;; Pipeline

(defclass aos-pipeline (pipeline)
  ((post-scene :initarg :post-scene :type mt-post-scene)))

(defun make-aos-pipeline ()  
  (make-instance
   'aos-pipeline
   :post-scene (make-instance 'mt-post-scene)
   :passes (list (cons :col (make-mt-colour-pass))
		 (cons :mt (make-metatexture-pass))		 
		 (cons :post (make-mt-post-pass)))))

(defmethod resize ((pl aos-pipeline) w h)
  (call-next-method)
  (with-slots ((scene post-scene)) pl
    (resize scene w h)
    (set-post-texs scene (alist-fb-textures pl '(:mt :col)))))

(defmethod draw ((pl aos-pipeline) scenes)
  (draw (get-pass pl :mt) scenes)
  (draw (get-pass pl :col) scenes)
  (draw (get-pass pl :post) (slot-value pl 'post-scene))
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :post))
   nil (gficl:window-width) (gficl:window-height)))
