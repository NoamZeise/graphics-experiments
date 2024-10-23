(in-package :project)

;; normal shader

(defclass show-normals (normals-shader) ())

(defmethod reload ((s show-normals))
  (let ((files (list #p"normals.vs" #p"normals.fs")))
    (shader-reload-files (s files)
      (let ((shader (gficl/load:shader (car files) (cadr files)
				       :shader-folder +shader-folder+)))
	(setf (slot-value s 'shader) shader)))))

(defmethod draw ((obj show-normals) scene)
  (gl:enable :depth-test)
  (call-next-method))

(defclass normals-pass (pass) ())

(defun make-normals-pass ()
  (make-instance 'normals-pass
   :shaders (list (make-instance 'show-normals))
   :description
   (make-framebuffer-descrption
    :attachments
    (list (gficl:make-attachment-description)
	  (gficl:make-attachment-description :position :depth-attachment))
    :samples (msaa-samples 16))))

;; pipeline

(defclass outline-pipeline (pipeline) ())

(defun make-outline-pipeline ()
  (make-instance 'outline-pipeline
  :passes (list (cons :col (make-mt-colour-pass))
		(cons :norm (make-normals-pass)))))

(defmethod draw ((pl outline-pipeline) scenes)
  (draw (get-pass pl :norm) scenes)
  (draw (get-pass pl :col) scenes)
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :col)) nil
   (gficl:window-width) (gficl:window-height)))
