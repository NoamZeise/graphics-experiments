(in-package :framework)

(defclass pipeline ()
  ((passes :initarg :passes :documentation "alist of keys and passes")
   (shaders :initarg :shaders :initform () :documentation "alist of keys and shaders (ie for compute shaders not dependant on a pass)"))
  (:documentation "Ecapsulates an entire graphics pipeline for drawing a list of scenes.
Comprises of multiple passes."))

(defmacro foreach-pl (pipeline (p slot) fn)
  (let ((pass (gensym)))
    `(loop for (nil . ,pass) in (slot-value ,pipeline ,slot) do (let ((,p ,pass)) ,fn))))

(defmethod initialize-instance :after ((instance pipeline) &key &allow-other-keys)	   
	   (resize instance (gficl:window-width) (gficl:window-height)))

(defun get-pl (pipeline key slot)
  (cdr (assoc key (slot-value pipeline slot))))

(defun get-pass (pipeline key)
  (get-pl pipeline key 'passes))

(defun get-shader (pipeline key)
  (get-pl pipeline key 'shaders))

(defmethod reload ((pl pipeline))
   (foreach-pl pl (p 'passes) (reload p))
   (foreach-pl pl (s 'shaders) (reload s)))

(defmethod resize ((pl pipeline) (w integer) (h integer))
  (foreach-pl pl (p 'passes) (resize p w h)))

(defmethod free ((pl pipeline))
  (foreach-pl pl (p 'passes) (free p))
  (foreach-pl pl (s 'shaders) (free s)))

(defun alist-fb-textures (pl passes)
  "Given a PIPELINE and a list of keys for an alist,
return an alist of GFICL:TEXTURE objects from each pass specificed by the keys in passes.
The keys to the texture alist are the same as the keys used for the passes."
  (loop for p in passes collecting
	(cons p (let ((texs (get-textures (get-pass pl p))))
		  (if (not texs) (error "pipeline: no textures found for pass ~a" p))
		  texs))))
