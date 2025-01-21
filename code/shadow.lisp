(in-package :project)

;;; variance shadow map shader

(defclass vsm-shader (shader) ())

(defmethod reload ((s vsm-shader))
  (shader-reload-files (s (#p"basic.vs" #p"vsm.fs")
			  :folder (shader-subfolder #p"shadows/"))
		       shader))

(defmethod shader-scene-props ((obj vsm-shader) (scene scene-3d))
  (with-slots (light-vp) scene
    (gficl:bind-matrix (slot-value obj 'shader) "viewproj" light-vp)))

(defmethod draw ((obj vsm-shader) scenes)
  (gl:enable :depth-test :cull-face)
  (call-next-method))

;;; shadow pass

(defclass vsm-pass (pass) ())

(defun make-vsm-pass ()
  (make-instance 'vsm-pass
    :shaders (list (make-instance 'vsm-shader))
    :description
    (make-framebuffer-descrption
     (list (gficl:make-attachment-description
	    :type              :texture
	    :internal-format   :rgba32f)
	   (gficl:make-attachment-description
	    :position :depth-attachment))
     :samples 16)))

(defmethod resize :after ((obj vsm-pass) (w integer) (h integer))
  (gl:bind-texture :texture-2d (get-pass-texture obj))
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear))

(defmethod draw :after ((obj vsm-pass) scenes)
  (gl:bind-texture :texture-2d (get-pass-texture obj))
  (gl:generate-mipmap :texture-2d))
