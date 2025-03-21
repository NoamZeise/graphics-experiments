(in-package :experiments)

;;; simple lambertian shader with uv texture

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

;;; normals debug shader

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

(defmethod draw ((obj backface-shader) (scene scene-2d))
  ;; dont draw outline for 2d scenes
  )

(defmethod shader-scene-props ((obj backface-shader) (scene scene-3d))
  (if (and (> (gficl:window-width) 0) (> (gficl:window-height) 0))
    (gficl:bind-matrix (slot-value obj 'shader) "viewproj"
      (with-slots (cam-pos cam-target (fov cam-fov) (near cam-near)) scene
	(gficl:*mat
	 (gficl:screen-perspective-matrix (gficl:window-width) (gficl:window-height)
					  (* pi fov) (* near (slot-value obj 'near-factor)) 100)
	 (gficl:view-matrix cam-pos (gficl:-vec cam-target cam-pos) +world-up+))))))

;;; partially deferred lighting shader with a shadow map

(defclass deferred-shader (normals-cam-shader)
  ((shadow-map)))

(defmethod reload ((s deferred-shader))
  (shader-reload-files
   (s (#p"deferred.vs" #p"deferred.fs") :folder (shader-subfolder #p"deferred/")) shader
   (gl:uniformi (gficl:shader-loc shader "tex") 0)
   (gl:uniformi (gficl:shader-loc shader "shadow_map") 1)))

(defmethod draw ((obj deferred-shader) scene)
   (gl:active-texture :texture1)
   (gl:bind-texture :texture-2d (slot-value obj 'shadow-map))
   (gl:active-texture :texture0)
   (call-next-method))

(defmethod shader-scene-props ((obj deferred-shader) (scene scene-3d))
  (call-next-method)
  (with-slots ((norm-view inverse-transpose-view-mat) (view view-mat)
	       (proj projection-mat)
	       light-vp)
      scene
    (with-slots (shader) obj
      (gficl:bind-matrix shader "norm_view" norm-view)
      (gficl:bind-matrix shader "view" view)
      (gficl:bind-matrix shader "proj" proj)
      (gficl:bind-matrix shader "light_vp" light-vp))))

(defmethod shader-mesh-props ((obj deferred-shader) props)
  (with-slots (shader) obj
    (let ((dt (obj-prop props :diffuse))
	  (col (obj-prop props :colour))
	  (light? (obj-prop props :light)))
      (gl:uniformi (gficl:shader-loc shader "use_texture") (if dt 1 0))
      (gl:uniformi (gficl:shader-loc shader "is_light") (if light? 1 0))
      (gficl:bind-vec shader "obj_colour" col)
      (if dt (gficl:bind-gl dt)))))

(defclass deferred-pass (pass) ())

(defun make-deferred-pass ()
  (make-instance
   'deferred-pass
   :shaders (list (make-instance 'deferred-shader))
   :description
   (make-framebuffer-description
    (list (gficl:make-attachment-description :position :color-attachment0 :type :texture)
	  (gficl:make-attachment-description :position :color-attachment1 :type :texture
					     :internal-format :rgba32f)
	  (gficl:make-attachment-description :position :color-attachment2 :type :texture
					     :internal-format :rgba32f)
	  (gficl:make-attachment-description :position :color-attachment3 :type :texture
					     :internal-format :rgba32f)
	  (gficl:make-attachment-description :position :depth-attachment))
    :samples 8)))

(defmethod draw ((obj deferred-pass) scenes)
  (gl:clear-buffer-fv :color 0 #(0 0 0 0)) ; colour
  (gl:clear-buffer-fv :color 3 #(0 0 0 0)) ; position
  (gl:enable :cull-face :depth-test)
  (call-next-method))
