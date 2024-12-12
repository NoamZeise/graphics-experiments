(in-package :project)

(in-package :project)

;;; shader

(defclass lit-sphere-shader (normals-cam-shader) ())

(defmethod reload ((s lit-sphere-shader))
  (shader-reload-files (s (#p"standard.vs" #p"lit-sphere/lit-sphere.fs")) shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)
    (gl:uniformi (gficl:shader-loc shader "matcap") 1)))

(defmethod draw ((obj lit-sphere-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'uv))
  (gl:active-texture :texture1)
  (gficl:bind-gl (get-asset 'rust-matcap))
  (call-next-method))

(defmethod shader-mesh-props ((obj lit-sphere-shader) props)
  (let ((dt (cdr (assoc :diffuse props))))
    (gl:active-texture :texture0)
    (let ((enable-mc 0))
      (cond (dt (gficl:bind-gl dt))
	    (t  (gficl:bind-gl (get-asset 'uv))
		(setf enable-mc 1)))
      (gl:uniformi (gficl:shader-loc (slot-value obj 'shader) "enableMC") enable-mc))))

(defmethod shader-scene-props ((obj lit-sphere-shader) (scene scene-3d))
  (call-next-method)
  (if (and (> (gficl:window-width) 0) (> (gficl:window-height) 0))
      (with-slots (cam-pos cam-target) scene
	(gficl:bind-matrix
	 (slot-value obj 'shader)
	 "viewmat"
	 (gficl:view-matrix cam-pos (gficl:-vec cam-target cam-pos) +world-up+)))))

;;; colour pass

(defclass lit-sphere-colour-pass (pass) ())

(defun make-lit-sphere-colour-pass ()
  (make-instance 'lit-sphere-colour-pass
     :shaders (list (make-instance 'lit-sphere-shader))
     :description
     (make-framebuffer-descrption
      (list (gficl:make-attachment-description :type :texture)
	    (gficl:make-attachment-description :position :depth-attachment))
      :samples 16)))

;;; pipeline

(defclass lit-sphere-pipeline (pipeline) ())

(defun make-lit-sphere-pipeline ()
  (make-instance 'lit-sphere-pipeline
    :passes (list (cons :col (make-lit-sphere-colour-pass)))))

(defmethod draw ((pl lit-sphere-pipeline) scenes)
  (draw (get-pass pl :col) scenes)
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :col)) nil
   (gficl:window-width) (gficl:window-height)))
