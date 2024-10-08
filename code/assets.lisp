(in-package :project)

(defun setup-asset-table ()
  (setf *assets* (make-hash-table)))

(defun cleanup-assets ()
  (loop for vd being the hash-value of *assets* do
	(if (listp vd)
	    (loop for v in vd do (gficl:delete-gl v))
	    (gficl:delete-gl vd))))

(defun add-asset (key asset)
  (cond ((gethash key *assets*)
	 (warn "Added asset (~a) with key that already exits (~a), deleting old asset."
	       asset key)
	 (gficl:delete-gl (gethash key *assets*))))
  (setf (gethash key *assets*) asset))

(defun load-model (key filename)
  (add-asset
   key
   (let  ((data (gficl/load:model (merge-pathnames filename +asset-folder+)
				  :vertex-form '(:position :normal :uv))))
     (if (= 1 (length data)) (car data) data))))

(defun load-image (key filename)
  (add-asset key (gficl/load:image filename)))

(defun get-asset (key)
  (let ((a (gethash key *assets*)))
    (if (not a) (error "asset not found ~a" key) a)))

;;; ---- Globals ----

(defparameter *assets* nil
	      "Holds the loaded game assets.")

(defconstant +asset-folder+ #p"assets/")
