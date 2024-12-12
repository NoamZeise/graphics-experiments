(in-package :project)

;;; shader

(defclass pbr-shader (normals-cam-shader) ())

(defmethod reload ((s pbr-shader))
  (shader-reload-files (s (#p"standard.vs" #p"pbr.fs")) shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)))

(defmethod draw ((obj pbr-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'uv))
  (call-next-method))

(defmethod shader-mesh-props ((obj pbr-shader) props)
  (let ((dt (cdr (assoc :diffuse props))))
    (gl:active-texture :texture0)
    (cond (dt (gficl:bind-gl dt))
	  (t  (gficl:bind-gl (get-asset 'uv))))))

;;; colour pass

(defclass pbr-colour-pass (pass) ())

(defun make-pbr-colour-pass ()
  (make-instance 'pbr-colour-pass
     :shaders (list (make-instance 'pbr-shader))
     :description
     (make-framebuffer-descrption
      (list (gficl:make-attachment-description :type :texture)
	    (gficl:make-attachment-description :position :depth-attachment))
      :samples 16)))

;;; pipeline

(defclass pbr-pipeline (pipeline) ())

(defun make-pbr-pipeline ()
  (make-instance 'pbr-pipeline
    :passes (list (cons :col (make-pbr-colour-pass)))))

(defmethod draw ((pl pbr-pipeline) scenes)
  (draw (get-pass pl :col) scenes)
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :col)) nil
   (gficl:window-width) (gficl:window-height)))
