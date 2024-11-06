(in-package :project)

;;; 3D Lambertian shader with uv texture

(defclass standard-colour-shader (normals-cam-shader) ())

(defmethod reload ((s standard-colour-shader))
  (shader-reload-files (s '(#p"vert.vs" #p"frag.fs"))			
    (let ((shader (gficl/load:shader #p"vert.vs" #p"frag.fs" :shader-folder +shader-folder+)))
      (gficl:bind-gl shader)
      (gl:uniformi (gficl:shader-loc shader "tex") 0)
      (setf (slot-value s 'shader) shader))))

(defmethod draw ((obj standard-colour-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'uv))
  (call-next-method))

;;; View Normals Shader

(defclass show-normals-shader (normals-shader) ())

(defmethod reload ((s show-normals-shader))
  (let ((files (list #p"normals.vs" #p"normals.fs")))
    (shader-reload-files (s files)
      (let ((shader (gficl/load:shader (car files) (cadr files)
				       :shader-folder +shader-folder+)))
	(setf (slot-value s 'shader) shader)))))

(defmethod draw ((obj show-normals-shader) scene)
  (gl:enable :depth-test :cull-face)
  (call-next-method))

;;; cel shaded shader

(defclass cel-shader (normals-cam-shader) ())

(defmethod reload ((s cel-shader))
  (shader-reload-files (s '(#p"cel-shaded.vs" #p"cel-shaded.fs"))			
    (let ((shader (gficl/load:shader #p"cel-shaded.vs" #p"cel-shaded.fs"
				     :shader-folder +shader-folder+)))
      (gficl:bind-gl shader)
      (gl:uniformi (gficl:shader-loc shader "tex") 0)
      (setf (slot-value s 'shader) shader))))

(defmethod draw ((obj cel-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'colours))
  (call-next-method))

;;; xtoon shader

(defclass xtoon-shader (normals-cam-shader) ())

(defmethod reload ((s xtoon-shader))
  (shader-reload-files (s '(#p"cel-shaded.vs" #p"xtoon.fs"))			
    (let ((shader (gficl/load:shader #p"cel-shaded.vs" #p"xtoon.fs"
				     :shader-folder +shader-folder+)))
      (gficl:bind-gl shader)
      (gl:uniformi (gficl:shader-loc shader "toontex") 0)
      (setf (slot-value s 'shader) shader))))

(defmethod draw ((obj xtoon-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'xtoon))
  (call-next-method))

;;; brush shader

(defclass brush-shader (normals-cam-shader) ())

(defmethod reload ((s brush-shader))
  (shader-reload-files (s '(#p"cel-shaded.vs" #p"brush.fs"))			
    (let ((shader (gficl/load:shader #p"cel-shaded.vs" #p"brush.fs"
				     :shader-folder +shader-folder+)))
      (gficl:bind-gl shader)
      (gl:uniformi (gficl:shader-loc shader "tex") 0)
      (gl:uniformi (gficl:shader-loc shader "brushtex") 1)
      (setf (slot-value s 'shader) shader))))

(defmethod draw ((obj brush-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'uv))
  (gl:active-texture :texture1)
  (gficl:bind-gl (get-asset 'brush2))
  (call-next-method))
