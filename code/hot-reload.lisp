(in-package :project)

(defstruct watched
  events
  (modified t :type boolean))

(defparameter *watched* nil)

(defun init-watched ()
  (setf *watched* (make-hash-table)))

(defun watch-file (file events)
  (cond ((not (gethash file *watched*))
	 (setf (gethash file *watched*)
	       (make-watched :events events))
	 (notify:watch file :events events))))

(defun watch-files (files &key (events (list :modify))
			  (folder +shader-folder+))
  (loop for file in files do (watch-file (merge-pathnames folder file) events)))

(defun file-modified (file)
  (let ((fw (gethash file *watched*)))
    (if fw (let ((modified (watched-modified fw)))
	     (if modified (setf (watched-modified fw) nil))
	     modified)
      fw)))

(defun files-modified (files)
  (loop for file in files do (file-modified file)))

(defun unwatch-all ()
  (loop while (notify:unwatch (car (notify:list-watched)))))

(defun process-watched ()
  (flet ((process
	  (f e)
	  (print (list "->" f e))
	  (cond ((eql e :modify) (print f)))))
	(notify:process-events #'process)))
