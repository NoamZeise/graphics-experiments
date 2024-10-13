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
