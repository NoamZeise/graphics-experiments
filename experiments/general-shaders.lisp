(in-package :experiments)

;;; 3D Lambertian shader with uv texture

(defclass standard-colour-shader (normals-cam-shader) ())

(defmethod reload ((s standard-colour-shader))
  (shader-reload-files (s (#p"standard.vs" #p"direct.fs")) shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)))

(defmethod draw ((obj standard-colour-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (car (get-asset 'uv)))
  (call-next-method))

(defmethod shader-mesh-props ((obj standard-colour-shader) props)
  (let ((dt (cdr (assoc :diffuse props))))
    (gl:active-texture :texture0)
    (cond (dt (gficl:bind-gl (if (listp dt) (car dt) dt)))
	  (t  (gficl:bind-gl (car (get-asset 'uv)))))))

;;; View Normals Shader

(defclass show-normals-shader (normals-shader) ())

(defmethod reload ((s show-normals-shader))
  (shader-reload-files (s (#p"standard.vs" #p"normals.fs")) shader))

(defmethod draw ((obj show-normals-shader) scene)
  (gl:enable :depth-test :cull-face)
  (call-next-method))

;;; cel shaded shader

(defclass cel-shader (normals-cam-shader) ())

(defmethod reload ((s cel-shader))
  (shader-reload-files (s (#p"standard.vs" #p"cel-shaded.fs")) shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)))

(defmethod draw ((obj cel-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (car (get-asset 'colours)))
  (call-next-method))

(defmethod shader-mesh-props ((obj cel-shader) props)
  (let ((dt (cdr (assoc :diffuse props))))
    (gl:active-texture :texture0)
    (cond (dt (gficl:bind-gl (if (listp dt) (car dt) dt)))
	  (t  (gficl:bind-gl (car (get-asset 'colours)))))))

;; backface shader - for outlines

(defclass backface-shader (normals-cam-shader)
  ((polygon-offset :initarg :polygon-offset :initform -1.2)
   (normal-divisor :initarg :normal-divisor :initform 150.0 :type float)
   (near-factor :initarg :near-factor :initform 1.001)
   (shader-folder :initform (merge-pathnames #p"outline/" (merge-pathnames +shader-folder+)))
   (vert-shader :initform #p"backface.vs")
   (frag-shader :initform #p"backface.fs")
   (outline-colour :initarg :outline-colour
		   :initform (gficl:make-vec '(1 1 1 1)) :type gficl:vec)
   (outline-size :initarg :outline-size
		 :initform 1.0 :type float)))

(defmethod reload ((s backface-shader))
  (with-slots (vert-shader frag-shader shader-folder) s
    (shader-reload-files (s (vert-shader frag-shader) :folder shader-folder) shader
      (gl:uniformf (gficl:shader-loc shader "normal_divisor")
		   (slot-value s 'normal-divisor)))))

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
