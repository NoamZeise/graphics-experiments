(in-package :experiments)

;;; Performance Structs

(defstruct frame-performance
  (fps 0.0))

(defstruct scene-performance
   (frames nil)
   (time 0.0))

(defstruct final-performance
   (pipeline "")
   (scene "")
   (avg-fps 0.0)
   (1%-low 0.0)
   (peak-fps 0.0)
   (runtime 0.0)
   (resolution (cons 0 0)))

(defun generate-final-performance (scene-performance pipeline-name scene-name)
  (let* ((final (make-final-performance))
	 (frames (scene-performance-frames scene-performance))
	 (frame-count (length frames))
	 (1% (ceiling (/ frame-count 100)))
	 (lows (subseq (sort frames (lambda (a b) (< (frame-performance-fps a)
						     (frame-performance-fps b))))
		       0 1%)))
    (if (> 1% 0)
	(setf (final-performance-1%-low final)
	      (/ (loop for f in lows summing (frame-performance-fps f)) 1%)))
    (if (> frame-count 0)
	(setf (final-performance-avg-fps final)
	      (/ (loop for f in frames summing
		       (let ((fps (frame-performance-fps f)))
			 (if (> fps (final-performance-peak-fps final))
			     (setf (final-performance-peak-fps final)
				   fps))
			 fps))
		 (length frames))))
    (setf (final-performance-runtime final)
	  (scene-performance-time scene-performance))
    (setf (final-performance-pipeline final) pipeline-name)
    (setf (final-performance-scene final) scene-name)
    (setf (final-performance-resolution final)
	  (cons (gficl:window-width) (gficl:window-height)))
    final))

;;; Performance Analyser

(defclass performance-analyser ()
  ((pipelines :initarg :pipelines)
   (scenes :initarg :scenes)
   (current-pipeline :initform nil)
   (current-scene :initform nil)
   (scene-performance :initform (make-scene-performance))
   (performance-list :initform (list))
   (finished-performance-analysis :initform nil :accessor finished)))

(defgeneric update (obj dt))

(defgeneric start-performance-analyser (obj))

(defgeneric end-performance-analyser (obj))

(defgeneric end-current-scene (obj))

(defgeneric get-performance-report (obj))

;;; performance analyser implementation

(defmethod initialize-instance :after ((a performance-analyser) &key &allow-other-keys)
	   (start-performance-analyser a))

(defun make-performace-analyser (pipelines scenes)
  (make-instance
   'performance-analyser
   :pipelines pipelines
   :scenes scenes))

(defun update-scene-performance (scene-performance dt)
  (setf (scene-performance-time scene-performance)
	(+ dt (scene-performance-time scene-performance)))
  (setf (scene-performance-frames scene-performance)
	(cons (make-frame-performance :fps (/ 1.0 dt))
	      (scene-performance-frames scene-performance)))
  scene-performance)

(defmethod update ((a performance-analyser) dt)
  (with-slots (current-scene scene-performance) a
    (loop for s in (getf (cdr current-scene) :scenes) do (update-scene s dt))
    (update-scene-performance scene-performance dt)
    (if (> (scene-performance-time scene-performance) (getf (cdr current-scene) :duration))
	(end-current-scene a))))

(defmethod draw ((a performance-analyser) _)
  (with-slots (current-pipeline current-scene) a
    (draw (cdr current-pipeline) (getf (cdr current-scene) :scenes))))

(defmethod start-performance-analyser ((a performance-analyser))
  (with-slots (scene-performance current-scene scenes current-pipeline pipelines finished-performance-analysis performance-list) a
    (setf scene-performance (make-scene-performance))
    (setf current-pipeline (car pipelines))
    (setf current-scene (car scenes))
    (setf finished-performance-analysis nil)
    (setf performance-list nil)))

(defmethod end-performance-analyser ((a performance-analyser))
   (with-slots (scene-performance current-pipeline current-scene) a
     (generate-final-performance
      scene-performance
      (car current-pipeline)
      (car current-scene))))

(defmethod end-current-scene ((a performance-analyser))
  (with-slots (performance-list scene-performance current-scene scenes current-pipeline pipelines finished-performance-analysis) a
    (let ((performance (end-performance-analyser a)))
      (format t "ran analyser - pipeline: ~a - scene: ~a ~%~a~%"
	      current-pipeline current-scene performance)
      (setf performance-list (cons performance performance-list)))
    (setf scene-performance (make-scene-performance))
    (destructuring-bind
     (s . p)
     (loop for (scene . rest) on scenes
	   when (equal scene current-scene)
	   return (if rest (cons (car rest) current-pipeline)
		    (cons (car scenes)
			  (loop for (pipeline . rest) on pipelines
				when (equal pipeline current-pipeline)
				return (if rest (car rest)
					 (progn
					   (setf finished-performance-analysis t)
					   (car pipelines)))))))
     (setf current-scene s)
     (setf current-pipeline p))))

(defmethod get-performance-report ((a performance-analyser))
  (slot-value a 'performance-list))
