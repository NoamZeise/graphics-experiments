(in-package :project)

(defclass metatexture-shader (shader)
  ())

(defun make-metatexture-shader ()
  (load-image 'metatexture-noise #p"assets/test.png")
  (let ((shader (gficl/load:shader #p"vert.vs" #p"metatexture.fs" :shader-folder +shader-folder+)))
    (gficl:bind-gl shader)
    (gl:uniformi (gficl:shader-loc shader "tex") 0)
    (make-instance 'metatexture-shader :shader shader)))

(defmethod draw ((obj metatexture-shader) scene)
	   (gl:enable :depth-test)
	   (gl:clear :color-buffer-bit :depth-buffer-bit)
	   ;;hack - until scenes formalized
	   (gficl:bind-gl (shader obj))
	   (gficl:bind-matrix (shader obj) "viewproj"
			      (gficl:*mat *projection-mat* *view-mat*))
	   
	   (gl:active-texture :texture0)
	   (gficl:bind-gl (get-asset 'metatexture-noise))

	   (call-next-method)
	   
	   ;;another hack
	   (gficl:bind-matrix (shader obj) "viewproj" *ortho-mat*)
	   (draw *quad* obj))

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
    :samples 4)))
