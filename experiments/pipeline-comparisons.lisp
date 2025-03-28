(in-package :experiments)

;;; functions that return pipeline lists
;;; for performance comparisons

(defun different-methods ()
  (let ((cascade-pipeline (make-cascade-pipeline)))
    (list
     (cons "shadow mapping" (make-shadow-deferred-pipeline))
     (cons "radiance cascades"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :w 256 :h 256 :s 6))
		     (update-cascade-obj pl (make-instance 'cascade-params)))))
	  (cons "ssao" (make-ssao-pipeline))	  
	  ;;(cons "simple" (make-pbr-pipeline))
	  )))

(defun resolution-comparison ()
  (let ((cascade-pipeline (make-cascade-pipeline)))
    (list
     (cons "64x64"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 5 :w 64 :h 64))
		     (update-cascade-obj pl (make-instance 'cascade-params)))))
     (cons "128x128"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 5 :w 128 :h 128))
		     (update-cascade-obj pl (make-instance 'cascade-params)))
		 :reused t))
     (cons "256x256"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 5 :w 256 :h 256))
		     (update-cascade-obj pl (make-instance 'cascade-params)))
		 :reused t))
     (cons "512x512"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 5 :w 512 :h 512))
		     (update-cascade-obj pl (make-instance 'cascade-params)))
		 :reused t))
     (cons "1024x1024"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 5 :w 1024 :h 1024))
		     (update-cascade-obj pl (make-instance 'cascade-params)))
		 :reused t)))))

(defun samples-comparison ()
  (let ((cascade-pipeline (make-cascade-pipeline)))
    (loop for i from 1 to 8 collecting
	  (cons (format nil "~a" i)
		(list cascade-pipeline
		      :init-fn
		      (let ((x i))
			#'(lambda (pl)
			    (update-cascade-obj pl (make-instance 'cascade-properties
								  :levels 5 :w 256 :h 256
								  :s (* x 2)))
			    (update-cascade-obj pl (make-instance 'cascade-params))))
		      :reused (not (= i 1)))))))

(defun levels-comparison ()
  (let ((cascade-pipeline (make-cascade-pipeline)))
    (loop for i from 1 to 9 collecting
	  (cons (format nil "~a" i)
		(list cascade-pipeline
		      :init-fn
		      (let ((x i))
			#'(lambda (pl)
			    (update-cascade-obj pl (make-instance 'cascade-properties
								  :levels x :w 512 :h 512))
			    (update-cascade-obj pl (make-instance 'cascade-params))))
		      :reused (not (= i 1)))))))

(defun steps-comparison ()
  (let ((cascade-pipeline (make-cascade-pipeline)))
    (loop for i from 1 to 10 collecting
	  (cons (format nil "~a" (* i 3))
		(list cascade-pipeline
		      :init-fn
		      (let ((x i))
			#'(lambda (pl)
			    (update-cascade-obj pl (make-instance 'cascade-properties
								  :levels 5 :w 256 :h 256))
			    (update-cascade-obj pl (make-instance 'cascade-params
								  :steps (* x 3)))))
		      :reused (not (= i 1)))))))
