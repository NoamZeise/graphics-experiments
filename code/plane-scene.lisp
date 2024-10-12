(in-package :project)

(defclass plane-scene (scene-3d)
  ())

(defun make-plane-scene ()
  (make-instance
   'plane-scene
   :cam-pos (gficl:make-vec '(4 2 4))
   :cam-target (gficl:make-vec '(0 0 0))
   :objects
   (list
    (make-object (get-asset 'sphere) (object-matrix '(2 0 1)))
    (make-object (get-asset 'cube) (object-matrix '(0 0 -2)))
    (make-object (get-asset 'bunny) (object-matrix '(-1 0 1) '(3 3 3)))
    (make-object (get-asset 'plane) (let* ((size 50) (offset (- (/ size 2))))
				      (object-matrix (gficl:make-vec `(,offset -1 ,offset))
						     (gficl:make-vec `(,size ,size ,size))))))))

(defmethod update-scene ((obj plane-scene) dt)
  (with-slots (cam-pos) obj
    (gficl:map-keys-down
     (:space (setf cam-pos (gficl:rotate-vec cam-pos (* dt 0.1) +world-up+))))))

(defclass square-scene (scene-2d)
  ((quad-size :initform 100 :type number)))

(defun make-square-scene ()
  (make-instance
   'square-scene
   :cam-pos (gficl:make-vec '(0 0 0))
   :objects
   (list (make-object (get-asset 'plane) (gficl:make-matrix)))))

(defmethod initialize-instance :after ((instance square-scene) &key &allow-other-keys)
  (update-square-scene-quad
   (car (slot-value instance 'objects)) (slot-value instance 'quad-size)))

(defun update-square-scene-quad (obj size)
  (update-model
   obj (gficl:*mat
	(gficl:translation-matrix (list size size 0))
	(gficl:scale-matrix (list size size 1))
	(gficl:make-matrix-from-data
	 `((1 0 0 0)
	   (0 0 1 0)
	   (0 1 0 0)
	   (0 0 0 1))))))

(defmethod update-scene ((obj square-scene) dt)
  (with-slots (objects (size quad-size)) obj
    (gficl:map-keys-down
     (:equal (setf size (+ size (* 100 dt)))
	     (update-square-scene-quad (car objects) size))
     (:minus (setf size (- size (* 100 dt)))
	     (update-square-scene-quad (car objects) size)))))
