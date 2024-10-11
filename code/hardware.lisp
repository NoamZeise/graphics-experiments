(in-package :project)

(defparameter *max-msaa-samples* 0)

(defun msaa-samples (samples)
  "Return the minimum of SAMPLES and the maximum supported number of samples.
Returns maximum supported samples if SAMPLES is 0"
  (if (= *max-msaa-samples* 0)
      (setf *max-msaa-samples* (gl:get-integer :max-samples)))
  (if (> samples 0) (min samples *max-msaa-samples*) *max-msaa-samples*))
