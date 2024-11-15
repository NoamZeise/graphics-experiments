(in-package :project)

(defun setup-asset-table ()
  (setf *assets* (make-hash-table))
  (setf *asset-props* (make-hash-table)))

(defun cleanup-assets ()
  (loop for vd being the hash-value of *assets* do
	(if (listp vd)
	    (loop for v in vd when v do (gficl:delete-gl v))
	  (if vd (gficl:delete-gl vd))))) 

(defun add-asset (key asset)
  (cond ((gethash key *assets*)
	 (warn "Added asset (~a) with key that already exits (~a), deleting old asset."
	       asset key)
	 (gficl:delete-gl (gethash key *assets*))))
  (format t "added ~a : ~a~%" key asset)
  (setf (gethash key *assets*) asset))

(defun add-asset-props (key props)
  (setf (gethash key *asset-props*) props))

(defun get-asset-prop (key prop)
  (cdr (assoc prop (gethash key *asset-props*))))

(defun load-model (key filename)
  (multiple-value-bind (data maps)
    (gficl/load:model (merge-pathnames filename +asset-folder+)
		      :vertex-form '(:position :normal :uv))     
    (add-asset key (if (= 1 (length data)) (car data) data))
    (values (get-asset key) maps)))

(defun load-model+texs (key filename)
  (let ((maps (nth-value 1 (load-model key filename))))
    (let ((diff-symb (intern (concatenate 'string (symbol-name key)
					  "-DIFFUSE")
			     :project))
	  (diffuse-texs nil) (diffuse-props nil))
      (loop for map in maps do
	    (let ((diff-file (cdr (assoc :diffuse map))))
	      (if diff-file
		  (handler-case
		      (multiple-value-bind
		       (tex w h) (gficl/load:image diff-file)
		       (setf diffuse-texs (cons tex diffuse-texs))
		       (setf diffuse-props (cons (pairlis '(:width :height) (list w h))
						 diffuse-props)))
		    (error (e) (format t "error ~a~%" e)))
		(progn
		  (setf diffuse-texs (cons nil diffuse-texs))
		  (setf diffuse-props (cons nil diffuse-props))))))
      (cond ((> (length (remove-if #'not diffuse-texs)) 0)
	     (add-asset diff-symb diffuse-texs)
	     (add-asset-props diff-symb diffuse-props))))))

(defun load-image (key filename)
  (add-asset key
	     (multiple-value-bind (tex w h) (gficl/load:image filename)
				  (add-asset-props key (pairlis '(:width :height) (list w h)))
				  tex)))

(defun get-asset (key)
  (let ((a (gethash key *assets*)))
    (if (not a) (error "asset not found ~a" key) a)))

;;; ---- Globals ----

(defparameter *assets* nil
	      "Holds the loaded game assets.")

(defparameter *asset-props* nil)
