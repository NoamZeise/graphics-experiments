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
   (runtime 0.0))

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
    final))

;;; Performance Analyser

(defclass performance-analyser ()
  ((pipelines :initarg :pipelines)
   (scenes :initarg :scenes)
   (current-pipeline :initform nil)
   (current-scene :initform nil)
   (scene-performance :initform (make-scene-performance))))

(defmethod initialize-instance :after ((a performance-analyser) &key &allow-other-keys)
  (with-slots (pipelines scenes current-pipeline current-scene) a
    (setf current-pipeline (car pipelines))
    (setf current-scene (car scenes))))

(defun make-performace-analyser (pipelines scenes)
  (make-instance
   'performance-analyser
   :pipelines pipelines
   :scenes scenes))

(defgeneric update (obj dt))

(defun update-scene-performance (scene-performance dt)
  (setf (scene-performance-time scene-performance)
	(+ dt (scene-performance-time scene-performance)))
  (setf (scene-performance-frames scene-performance)
	(cons (make-frame-performance :fps (/ 1.0 dt))
	      (scene-performance-frames scene-performance)))
  scene-performance)

(defmethod update ((a performance-analyser) dt)
  (with-slots (current-scene scene-performance) a
    (loop for s in (cdr current-scene) do (update-scene s dt))
    (update-scene-performance scene-performance dt)))

(defmethod draw ((a performance-analyser) _)
  (with-slots (current-pipeline current-scene) a
      (draw (cdr current-pipeline) (cdr current-scene))))

(defgeneric start-performance-analyser (obj))

(defmethod start-performance-analyser ((a performance-analyser))
  (with-slots (scene-performance) a
    (setf scene-performance (make-scene-performance))))

(defgeneric end-performance-analyser (obj))

(defmethod end-performance-analyser ((a performance-analyser))
   (with-slots (scene-performance current-pipeline current-scene) a
     (generate-final-performance
      scene-performance
      (car current-pipeline)
      (car current-scene))))
