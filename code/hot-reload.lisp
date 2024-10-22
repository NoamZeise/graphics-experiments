(in-package :project)

(defstruct watched
  events
  (modified t :type boolean))

(defparameter *watched* nil)
(defparameter *should-reload* nil)
(defparameter *file-change* nil)

(defun init-watched ()
  (setf *watched* (make-hash-table))
  (setf *should-reload* nil)
  (setf *file-change* nil))

(defun watch-file (file events)
  (cond ((not (gethash file *watched*))
	 (setf (gethash file *watched*)
	       (make-watched :events events))
	 (notify:watch file :events events))))

(defun watch-files (files &key (events (list :modify))
			  (folder +shader-folder+))
  (loop for file in files do
	(watch-file (probe-file (merge-pathnames folder file)) events)))

(defun file-modified (file)
  (let ((fw (gethash file *watched*)))
    (if fw (let ((modified (watched-modified fw)))
	     (if modified
		 (setf (watched-modified fw)
		       ;; file watch not working on windows for now
		       #+windows t
		       #-windows nil
		       ))
	     modified)
      fw)))

(defun files-modified (files &key (folder +shader-folder+))
  (let ((mod nil))
    (loop for file in files
	  when (file-modified (probe-file (merge-pathnames folder file)))
	  do (setf mod t))
    mod))

(defun unwatch-all ()
  (loop while (notify:unwatch (car (notify:list-watched)))))

(defun process-watched ()
  (flet ((process (f e)
	   (print (list "->" f e))
	   (let ((fw (gethash f *watched*)))
	     (if (and fw (loop for ev in (watched-events fw)
			       when (eql e ev) return t
			       finally 'nil))
		 (progn (print "to-update")
			(setf *file-change* t)
			(setf (watched-modified fw) t))))))
	(notify:process-events #'process)))
