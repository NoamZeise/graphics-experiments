(in-package :project)

(defclass pipeline ()
  ((passes :initarg :passes :documentation "alist of keys and passes"))
  (:documentation "Ecapsulates an entire graphics pipeline for drawing a list of scenes.
Comprises of multiple passes."))

(defmacro foreach-pass (pipeline (p) fn)
  (let ((pass (gensym)))
    `(loop for (nil . ,pass) in (slot-value ,pipeline 'passes) do (let ((,p ,pass)) ,fn))))

(defmethod initialize-instance :after ((instance pipeline) &key &allow-other-keys)	   
  (resize instance (gficl:window-width) (gficl:window-height)))

(defun get-pass (pipeline key)
  (cdr (assoc key (slot-value pipeline 'passes))))

(defmethod reload ((pl pipeline))
  (foreach-pass pl (p) (reload p)))

(defmethod resize ((pl pipeline) (w integer) (h integer))
  (foreach-pass pl (p) (resize p w h)))

(defmethod free ((pl pipeline))
  (foreach-pass pl (p) (free p)))

(defun alist-fb-textures (pl passes)
  "Given a PIPELINE and a list of keys for an alist,
return an alist of GFICL:TEXTURE objects from each pass specificed by the keys in passes.
The keys to the texture alist are the same as the keys used for the passes."
  (loop for p in passes collecting
	(cons p (let ((texs (get-textures (get-pass pl p))))
		  (if (not texs) (error "pipeline: no textures found for pass ~a" p))
		  texs))))
