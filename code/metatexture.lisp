(in-package :project)

(defclass metatexture-shader (shader)
  ())

(defun make-metatexture-shader ()
  (load-image 'metatexture-noise #p"assets/test.png")
  (let ((shader (gficl/load:shader #p"vert.vs" #p"frag.fs" :shader-folder +shader-folder+)))
    (gficl:bind-gl shader)
    (gl:uniformi (gficl:shader-loc *meta-shader* "tex") 0)
    (make-instance 'metatexture-shader :shader shader)))

(defmethod draw ((obj metatexture-shader) scene)
	   (gl:active-texture :texture0)
	   (gficl:bind-gl (get-asset 'metatexture-noise)))

(defclass metatexture-pass (pass)
  ())
