(in-package :project)

;;; 3D Lambertian shader with uv texture

(defclass standard-colour-shader (normals-cam-shader) ())

(defmethod reload ((s standard-colour-shader))
  (shader-reload-files (s #p"standard.vs" #p"standard.fs") shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)))

(defmethod draw ((obj standard-colour-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'uv))
  (call-next-method))

;;; View Normals Shader

(defclass show-normals-shader (normals-shader) ())

(defmethod reload ((s show-normals-shader))
  (shader-reload-files (s #p"normals.vs" #p"normals.fs") shader))

(defmethod draw ((obj show-normals-shader) scene)
  (gl:enable :depth-test :cull-face)
  (call-next-method))

;;; cel shaded shader

(defclass cel-shader (normals-cam-shader) ())

(defmethod reload ((s cel-shader))
  (shader-reload-files (s #p"cel-shaded.vs" #p"cel-shaded.fs") shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)))

(defmethod draw ((obj cel-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'colours))
  (call-next-method))
