(in-package :project)

(defclass pipeline ()
  ((passes :initarg :passes :documentation "alist of keys and passes")))

(defmacro foreach-pass (pipeline (p) fn)
  (let ((pass (gensym)))
    `(loop for (nil . ,pass) in (slot-value ,pipeline 'passes) do (let ((,p ,pass)) ,fn))))

(defun get-pass (pipeline key)
  (cdr (assoc key (slot-value pipeline 'passes))))

(defmethod resize ((pl pipeline) (w integer) (h integer))
  (foreach-pass pl (p) (resize p w h)))

(defmethod free ((pl pipeline))
  (foreach-pass pl (p) (free p)))

(defun alist-fb-textures (pl passes)
  "Given a PIPELINE and a list of keys for an alist,
return an alist of GFICL:TEXTURE objects from each pass specificed by the keys in passes.
The keys to the texture alist are the same as the keys used for the passes."
  (print (loop for p in passes collecting
	       (cons p (get-textures (get-pass pl p))))))
