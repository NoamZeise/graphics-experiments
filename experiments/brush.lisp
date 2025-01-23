(in-package :experiments)

;;; brush shader

(defclass brush-shader (normals-cam-shader) ())

(defmethod reload ((s brush-shader))
  (shader-reload-files (s (#p"standard.vs"  #p"brush.fs")) shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)
    (gl:uniformi (gficl:shader-loc shader "brushtex") 1)))

(defmethod draw ((obj brush-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'uv))
  (gl:active-texture :texture1)
  (gficl:bind-gl (get-asset 'brush2))
  (call-next-method))

(defmethod shader-mesh-props ((obj brush-shader) props)
  (let ((dt (cdr (assoc :diffuse props))))
    (gl:active-texture :texture0)
    (cond (dt (gficl:bind-gl dt))
	  (t  (gficl:bind-gl (get-asset 'uv))))))

;;; brush pipeline

(defclass brush-colour-pass (pass) ())

(defun make-brush-colour-pass ()
  (make-instance 'brush-colour-pass
     :shaders (list (make-instance 'brush-shader)
		    (make-instance 'backface-shader))
     :description
     (make-framebuffer-description
      (list (gficl:make-attachment-description :type :texture)
	    (gficl:make-attachment-description :position :depth-attachment))
      :samples 16)))

(defclass brush-pipeline (pipeline) ())

(defun make-brush-pipeline ()
  (make-instance 'brush-pipeline
    :passes (list (cons :col (make-brush-colour-pass)))))

(defmethod draw ((pl brush-pipeline) scenes)
  (draw (get-pass pl :col) scenes)
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :col)) nil
   (gficl:window-width) (gficl:window-height)))

