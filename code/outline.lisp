(in-package :project)

;; backface pass

(defclass backface-shader (normals-cam-shader) ())

(defmethod reload ((s backface-shader))
  (let ((folder (merge-pathnames #p"outline/" +shader-folder+))
	(files (list #p"backface.vs" #p"backface.fs")))
    (shader-reload-files (s files :folder folder)
      (let ((shader (gficl/load:shader (car files) (cadr files) :shader-folder folder)))
	(setf (slot-value s 'shader) shader)))))

(defmethod draw ((obj backface-shader) scene)
  (gl:enable :depth-test :cull-face :polygon-offset-fill)
  (gl:cull-face :back)
  (gl:polygon-offset -10 -5)
  (gl:depth-func :lequal)
  (call-next-method)
  (gl:polygon-offset 0 0)
  (gl:depth-func :less)
  (gl:cull-face :front)
  (gl:disable :polygon-offset-fill))

;; dont draw outline for 2d scenes
(defmethod draw ((obj backface-shader) (scene scene-2d)))

;; colour + backfaces pass

(defclass backface-colour-pass (pass) ())

(defun make-backface-colour-pass ()
  (make-instance 'backface-colour-pass
     :shaders (list (make-instance 'cel-shader)
		    (make-instance 'backface-shader))
     :description
     (make-framebuffer-descrption
      (list (gficl:make-attachment-description)
	    (gficl:make-attachment-description :position :depth-attachment))
      :samples 16)))

;; pipeline

(defclass outline-pipeline (pipeline) ())

(defun make-outline-pipeline ()
  (make-instance 'outline-pipeline
  :passes (list (cons :col (make-backface-colour-pass)))))

(defmethod draw ((pl outline-pipeline) scenes)
  (draw (get-pass pl :col) scenes)
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :col)) nil
   (gficl:window-width) (gficl:window-height)))
