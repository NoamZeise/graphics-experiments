(in-package :framework)

(defstruct watched
  events
  (modified nil :type boolean))

(defparameter *watched* nil)
(defparameter *file-change* nil)

(defun init-watched ()
  (setf *watched* (make-hash-table))
  (setf *file-change* nil))

(defun watch-file (file events)
  (cond ((not (gethash file *watched*))
	 (setf (gethash file *watched*)
	       (make-watched :events events))
	 (notify:watch file :events events))))

(defun watch-files (files &key (events (list :modify)) (folder +shader-folder+))
  (loop for file in files do
	(watch-file (probe-file (merge-pathnames file (merge-pathnames folder))) events)))

(defun file-modified (file)
  (let ((fw (gethash file *watched*)))
    (if fw (watched-modified fw) nil)))

(defun files-modified (files &key (folder +shader-folder+))
  (loop for file in files
	when (file-modified (probe-file (merge-pathnames file (merge-pathnames folder))))
	return t finally 'nil))

(defun set-all-unmodified ()
  "after reloading shaders, set all to unmodified"
  (loop for v being the hash-values of *watched* do
	(setf (watched-modified v) nil)))

(defun unwatch-all ()
  (loop while (notify:unwatch (car (notify:list-watched)))))

(defun set-all-modified ()
  "manual override incase hot reload not working"
  (loop for v being the hash-values of *watched* do
	(setf (watched-modified v) t))
  (setf *file-change* t))

(defun process-watched ()
  (flet
   ((process (f e)
      (print (list "->" f e))
      (let ((fw (gethash (probe-file f) *watched*)))
	(if (and fw (loop for ev in (watched-events fw)
			  when (eql e ev) return t
			  finally 'nil))
	    (progn (setf *file-change* t)
		   (setf (watched-modified fw) t))))))
   (notify:process-events #'process))
  (let ((changed *file-change*))
    (setf *file-change* nil)
    changed))
