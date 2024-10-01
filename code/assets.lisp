(in-package :project)

(defun setup-assets ()
  (setf *assets* (make-hash-table))
  (load-model 'sphere #p"sphere.obj"))

(defun cleanup-assets ()
  (loop for vd being the hash-value of *assets* do
	(if (listp vd)
	    (loop for v in vd do (gficl:delete-gl v))
	    (gficl:delete-gl vd))))

(defun add-asset (key vertex-data)
  (setf (gethash key *assets*) vertex-data))

(defun load-model (key filename)
  (add-asset key (gficl/load:model (merge-pathnames filename +asset-folder+)
				   :vertex-form '(:position :normal))))

(defun get-asset (key)
  (gethash key *assets*))

;;; ---- Globals ----

(defparameter *assets* nil
	      "Holds the loaded game assets.")

(defconstant +asset-folder+ #p"assets/")
