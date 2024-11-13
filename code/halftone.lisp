(in-package :project)

;;; xtoon shader

(defclass halftone-shader (normals-cam-shader) ())

(defmethod reload ((s halftone-shader))
  (shader-reload-files (s #p"cel-shaded.vs" #p"halftone.fs") shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)))

(defmethod draw ((obj halftone-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'light-colours))
  (call-next-method))

(defmethod shader-mesh-props ((obj halftone-shader) props)
  (let ((dt (cdr (assoc :diffuse props))))
    (if dt (gficl:bind-gl dt)
      (gficl:bind-gl (get-asset 'light-colours)))))

;; xtoon pipeline

(defclass halftone-colour-pass (pass) ())

(defun make-halftone-colour-pass ()
  (make-instance 'halftone-colour-pass
     :shaders (list (make-instance 'halftone-shader)
		    (make-instance 'backface-shader
				   :outline-colour (gficl:make-vec '(0 0 0 0))
				   :outline-size 1
				   :normal-divisor 1000.0
				   :near-factor 1.000
				   :polygon-offset -0))
     :clear-colour '(0.7 0.9 1.0 0.0)
     :description
     (make-framebuffer-descrption
      (list (gficl:make-attachment-description :type :texture)
	    (gficl:make-attachment-description :position :depth-attachment))
      :samples 16)))

(defclass halftone-pipeline (pipeline) ())

(defun make-halftone-pipeline ()
  (make-instance 'halftone-pipeline
    :passes (list (cons :col (make-halftone-colour-pass)))))

(defmethod draw ((pl halftone-pipeline) scenes)  
  (draw (get-pass pl :col) scenes)
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :col)) nil
   (gficl:window-width) (gficl:window-height)))
