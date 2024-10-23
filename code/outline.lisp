(in-package :project)

;; backface pass

(defclass backface-shader (normals-shader) ())

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
  (gl:disable :polygon-offset-fill))

;; colour + backfaces pass

(defclass backface-colour-pass (pass) ())

(defun make-backface-colour-pass ()
  (make-instance 'backface-colour-pass
     :shaders (list (make-instance 'mt-colour-shader)
		    (make-instance 'backface-shader))
     :description
     (make-framebuffer-descrption
      (list (gficl:make-attachment-description)
	    (gficl:make-attachment-description :position :depth-attachment))
      :samples 16)))

(defmethod draw ((obj backface-colour-pass) scenes)
	   (loop for shader in (slot-value obj 'shaders) do
	(loop for scene in scenes do (draw shader scene))))

;; normal shader

(defclass show-normals (normals-shader) ())

(defmethod reload ((s show-normals))
  (let ((files (list #p"normals.vs" #p"normals.fs")))
    (shader-reload-files (s files)
      (let ((shader (gficl/load:shader (car files) (cadr files)
				       :shader-folder +shader-folder+)))
	(setf (slot-value s 'shader) shader)))))

(defmethod draw ((obj show-normals) scene)
  (gl:enable :depth-test :cull-face)
  (gl:cull-face :front)
  (call-next-method))

(defclass normals-pass (pass) ())

(defun make-normals-pass ()
  (make-instance 'normals-pass
   :shaders (list (make-instance 'show-normals))
   :description
   (make-framebuffer-descrption
    (list (gficl:make-attachment-description)
	  (gficl:make-attachment-description :position :depth-attachment))
    :samples 16)))

;; pipeline

(defclass outline-pipeline (pipeline) ())

(defun make-outline-pipeline ()
  (make-instance 'outline-pipeline
  :passes (list (cons :col (make-backface-colour-pass))
		(cons :norm (make-normals-pass)))))

(defmethod draw ((pl outline-pipeline) scenes)
  (draw (get-pass pl :norm) scenes)
  (draw (get-pass pl :col) scenes)
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :col)) nil
   (gficl:window-width) (gficl:window-height)))
