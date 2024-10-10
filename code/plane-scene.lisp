(in-package :project)

(defclass plane-scene (scene-3d)
  ())

(defun make-plane-scene ()
  (make-instance 'plane-scene
		 :cam-pos (gficl:make-vec '(5 0 5))
		 :cam-target (gficl:make-vec '(0 0 0))
		 :objects
		 (list
		  (make-object (get-asset 'sphere) (object-matrix '(2 0 1)))
		  (make-object (get-asset 'cube) (object-matrix '(0 0 -2)))
		  (make-object (get-asset 'bunny) (object-matrix '(-1 0 1) '(3 3 3)))
		  (make-object (get-asset 'plane)
			       (let* ((size 50) (offset (- (/ size 2))))
				 (object-matrix (gficl:make-vec `(,offset -1.2 ,offset))
						(gficl:make-vec `(,size ,size ,size))))))))

(defmethod update-scene ((obj plane-scene) dt)
	   (gficl:map-keys-down
	    (:space (setf *cam-pos* (gficl:rotate-vec *cam-pos* (* dt 0.1) +world-up+)))))
