(in-package :project)

(defclass plane-scene (scene-3d)
  ((rotating :initform t :type boolean)))

(defun make-plane-scene ()
  (make-instance
   'plane-scene
   :cam-pos (gficl:make-vec '(4 2 4))
   :cam-target (gficl:-vec '(0 0 0))
   :objects
   (list
    (make-object (get-asset 'sphere) (object-matrix '(4.5 1 -5) '(0.4 0.4 0.4)))
    (make-object (get-asset 'cube) (object-matrix '(4.5 1 16) '(0.8 0.8 0.8)))
    (make-object (get-asset 'bunny) (object-matrix '(2 1 1.8) '(2 2 2)))
    (make-object (get-asset 'street) (object-matrix '(4 -1 4) '(0.5 0.5 0.5)))
    ;; (make-object (get-asset 'plane) (let* ((size 50) (offset (- (/ size 2))))
    ;; 				      (object-matrix (gficl:make-vec `(,offset -4 ,offset))
    ;; 						     (gficl:make-vec `(,size ,size ,size))))))
    )))

(defmethod update-scene ((obj plane-scene) dt)
  (with-slots (cam-pos cam-target cam-fov rotating) obj
    (let* ((speed (* dt 0.8 (if (gficl:key-down :enter) 4 1)))
	   (move-speed (* 2.5 speed)) (fov-speed (* speed 1.2))
	   (fw (gficl:normalise (gficl:-vec cam-target cam-pos)))
	   (rw (gficl:normalise (gficl:cross fw +world-up+)))
	   (target (gficl:-vec cam-pos cam-target))
	   (rotate rotating))
      (gficl:map-keys-down
       ;; raise/lower camera
       (:space (setf cam-pos (gficl:+vec cam-pos (gficl:*vec move-speed +world-up+))))
       (:left-shift (setf cam-pos (gficl:-vec cam-pos (gficl:*vec move-speed +world-up+))))
       ;; move forward direction
       (:w (setf target (gficl:rotate-vec target speed rw)))
       (:a (setf target (gficl:rotate-vec target (- speed) +world-up+)))
       (:s (setf target (gficl:rotate-vec target (- speed) rw)))
       (:d (setf target (gficl:rotate-vec target speed +world-up+)))
       ;; move camera
       (:up (setf cam-pos (gficl:+vec cam-pos (gficl:*vec move-speed fw))))
       (:down (setf cam-pos (gficl:-vec cam-pos (gficl:*vec move-speed fw))))
       (:left (setf cam-pos (gficl:+vec cam-pos (gficl:*vec move-speed rw))))
       (:right (setf cam-pos (gficl:-vec cam-pos (gficl:*vec move-speed rw))))
       ;; zoom
       (:z (setf cam-fov (* cam-fov (- 1 fov-speed)))
	   (resize obj (gficl:window-width) (gficl:window-height)))
       (:x (setf cam-fov (* cam-fov (+ 1 fov-speed)))
	   (resize obj (gficl:window-width) (gficl:window-height)))
       ;; change light dir
       (:t
	(setf (light-dir obj) (gficl:rotate-vec (light-dir obj) speed
						(gficl:make-vec '(1 0 0)))))
       (:y
	(setf (light-dir obj) (gficl:rotate-vec (light-dir obj) speed
						(gficl:make-vec '(0 1 0))))))

      (gficl:map-keys-pressed
       (:r (setf rotating (not rotating))))
      
      (cond (rotate (setf cam-pos (gficl:rotate-vec cam-pos (* speed 0.1) +world-up+))
		    (setf target (gficl:+vec cam-pos))))
      (setf cam-target (gficl:-vec cam-pos target)))))

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
	 `((-1 0 0 0)
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
