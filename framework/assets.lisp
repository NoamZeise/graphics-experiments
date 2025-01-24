(in-package :framework)

(defun setup-asset-table ()
  (setf *assets* (make-hash-table))
  (setf *asset-objects* (list)))

(defun cleanup-assets ()
  (loop for vd in *asset-objects* do
	(if (listp vd)
	    (loop for v in vd when v do (gficl:delete-gl v))
	  (if vd (gficl:delete-gl vd))))) 

(defun add-to-asset-table (key asset)
  (cond ((gethash key *assets*)
	 (warn "Added asset (~a) with key that already exits (~a), forgetting old asset."
	       asset key)))
  (setf (gethash key *assets*) asset))

(defun add-asset-object (obj)
  (setf *asset-objects* (cons obj *asset-objects*)))

(defun add-asset (key asset)
  (add-asset-object asset)
  (add-to-asset-table key asset))

(defun internal-load-model (filename)
  (multiple-value-bind
   (data maps) (gficl/load:model
		(merge-pathnames filename +asset-folder+)
		:vertex-form '(:position :normal :uv))
   (add-asset-object data)
   (values data maps)))

(defun load-model (key filename)
  (format t "loading model ~a from ~a~%" key filename)
  (multiple-value-bind (data maps) (internal-load-model filename)
    (add-to-asset-table key (if (= 1 (length data)) (car data) data))
    (values (get-asset key) maps)))

(defun add-tex-alist (alist type filename)
  (handler-case
      (progn
	(format t "-> loading ~a image ~a~%" type filename)
	(multiple-value-bind (tex w h) (gficl/load:image filename)
	  (let ((tex-alist (pairlis '(:tex :w :h) (list tex w h))))
	    (add-asset-object tex)
	    (if (assoc type alist)
		(setf (cdr (assoc type alist))
		      (cons tex-alist (cdr (assoc type alist))))
	      (setf alist (acons type (list tex-alist) alist)))))
	alist)    
    (error (e) (format t "error ~a~%" e))))

(defun load-model+texs (key filename)
  (multiple-value-bind
   (model maps) (internal-load-model filename)
   (let ((asset (acons :model model ())))
     (loop for map in maps do
	   (let ((diff-file (cdr (assoc :diffuse map))))
	     (if diff-file
		 (setf asset (add-tex-alist asset :diffuse diff-file)))))
     (add-to-asset-table key asset))))

(defun load-image (key filename)
  (format t "loading image ~a from ~a~%" key filename)
  (add-to-asset-table key
    (multiple-value-bind (tex w h) (gficl/load:image filename)
      (add-asset-object tex)
      ;(pairlis '(:tex :width :height) (list tex w h))
      tex
      )))

(defun get-asset (key)
  (let ((a (gethash key *assets*)))
    (if (not a) (error "asset not found ~a" key) a)))

;;; ---- Globals ----

(defparameter *assets* nil
	      "Holds the loaded game assets.")

(defparameter *asset-objects* nil)
