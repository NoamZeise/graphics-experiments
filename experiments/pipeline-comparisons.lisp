(in-package :experiments)

;;; functions that return pipeline lists
;;; for performance comparisons

(defun different-methods ()
  (let ((cascade-pipeline (make-cascade-pipeline)))
    (list (cons "radiance cascades"
		(list cascade-pipeline
		      :init-fn
		      #'(lambda (pl)
			  (update-cascade-obj pl (make-instance 'cascade-properties))
			  (update-cascade-obj pl (make-instance 'cascade-params)))))
	  (cons "shadow mapping" (make-shadow-deferred-pipeline))
	  (cons "ssao" (make-ssao-pipeline))	  
	  (cons "simple" (make-pbr-pipeline)))))

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
		 :reused t)))))

(defun samples-comparison ()
  (let ((cascade-pipeline (make-cascade-pipeline)))
    (list
     (cons "4"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 6 :w 256 :h 256 :s 4))
		     (update-cascade-obj pl (make-instance 'cascade-params)))))
     (cons "8"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 6 :w 256 :h 256 :s 8))
		     (update-cascade-obj pl (make-instance 'cascade-params)))
		 :reused t))
     (cons "16"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 6 :w 256 :h 256 :s 16))
		     (update-cascade-obj pl (make-instance 'cascade-params)))
		 :reused t))
     (cons "32"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 6 :w 256 :h 256 :s 32))
		     (update-cascade-obj pl (make-instance 'cascade-params)))
		 :reused t)))))

(defun levels-comparison ()
  (let ((cascade-pipeline (make-cascade-pipeline)))
    (list
     (cons "8 levels"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 8))
		     (update-cascade-obj pl (make-instance 'cascade-params)))))
     (cons "6 levels"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 6))
		     (update-cascade-obj pl (make-instance 'cascade-params)))
		 :reused t))
     (cons "4 levels"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 4))
		     (update-cascade-obj pl (make-instance 'cascade-params)))
		 :reused t))
     (cons "2 levels"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 2))
		     (update-cascade-obj pl (make-instance 'cascade-params)))
		 :reused t))
     (cons "1 level"
	   (list cascade-pipeline
		 :init-fn
		 #'(lambda (pl)
		     (update-cascade-obj pl (make-instance 'cascade-properties :levels 1))
		     (update-cascade-obj pl (make-instance 'cascade-params)))
		 :reused t)))))
