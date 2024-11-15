(in-package :project)

;;; xtoon shader

(defclass xtoon-shader (normals-cam-shader) ())

(defmethod reload ((s xtoon-shader))
  (shader-reload-files (s #p"standard.vs" #p"xtoon.fs") shader
    (gl:uniformi (gficl:shader-loc shader "toontex") 0)))

(defmethod draw ((obj xtoon-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture0)
  (gficl:bind-gl (get-asset 'xtoon))
  (call-next-method))

;; xtoon pipeline

(defclass xtoon-colour-pass (pass) ())

(defun make-xtoon-colour-pass ()
  (make-instance 'xtoon-colour-pass
     :shaders (list (make-instance 'xtoon-shader)
		    (make-instance 'backface-shader))
     :description
     (make-framebuffer-descrption
      (list (gficl:make-attachment-description :type :texture)
	    (gficl:make-attachment-description :position :depth-attachment))
      :samples 16)))

(defclass xtoon-pipeline (pipeline) ())

(defun make-xtoon-pipeline ()
  (make-instance 'xtoon-pipeline
    :passes (list (cons :col (make-xtoon-colour-pass)))))

(defmethod draw ((pl xtoon-pipeline) scenes)
  (draw (get-pass pl :col) scenes)
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :col)) nil
   (gficl:window-width) (gficl:window-height)))
