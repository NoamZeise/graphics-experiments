(in-package :project)

;;; xtoon shader

(defclass halftone-shader (normals-cam-shader) ())

(defmethod reload ((s halftone-shader))
  (let ((files '(#p"cel-shaded.vs" #p"halftone.fs")))
    (shader-reload-files (s files)
      (let ((shader (gficl/load:shader
		     (car files) (cadr files)
		     :shader-folder +shader-folder+)))
	(gficl:bind-gl shader)
	(gl:uniformi (gficl:shader-loc shader "tex") 0)
	(setf (slot-value s 'shader) shader)))))

(defmethod draw ((obj halftone-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'uv))
  (call-next-method))

;; xtoon pipeline

(defclass halftone-colour-pass (pass) ())

(defun make-halftone-colour-pass ()
  (make-instance 'halftone-colour-pass
     :shaders (list (make-instance 'halftone-shader)
		    (make-instance 'backface-shader
				   :outline-colour (gficl:make-vec '(0 0 0 0))
				   :outline-size 5
				   :normal-divisor 100.0
				   :near-factor 1.009))
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
