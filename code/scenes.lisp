(in-package :project)

(defclass camera-scene (scene-3d)
  ((rotating :initform t :type boolean)))

(defmethod update-scene ((obj camera-scene) dt)
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
       (:a (setf target (gficl:rotate-vec target speed +world-up+)))
       (:s (setf target (gficl:rotate-vec target (- speed) rw)))
       (:d (setf target (gficl:rotate-vec target (- speed) +world-up+)))
       ;; move camera
       (:up (setf cam-pos (gficl:+vec cam-pos (gficl:*vec move-speed fw))))
       (:down (setf cam-pos (gficl:-vec cam-pos (gficl:*vec move-speed fw))))
       (:left (setf cam-pos (gficl:-vec cam-pos (gficl:*vec move-speed rw))))
       (:right (setf cam-pos (gficl:+vec cam-pos (gficl:*vec move-speed rw))))
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
      ;; toggle rotate
      (gficl:map-keys-pressed
       (:r (setf rotating (not rotating))))
      
      (cond (rotate (setf cam-pos (gficl:rotate-vec cam-pos (* (- speed) 0.1) +world-up+))
		    (setf target (gficl:+vec cam-pos))))
      (setf cam-target (gficl:-vec cam-pos target)))))

(defclass simple-3d-scene (camera-scene) ())

(defun make-simple-3d-scene ()
  (make-instance
   'simple-3d-scene
   :cam-pos (gficl:make-vec '(1.5 1.5 1.5))
   :cam-target (gficl:make-vec '(0 0 0.5))
   :objects
   (list
    (make-object (get-asset 'bunny) (object-matrix '(0 0 0) '(1 1 1)))
    (make-object (get-asset 'plane) (object-matrix '(0 -0.38 0) '(3 1 3)))
    (make-object (get-asset 'cube) (object-matrix '(-1 0 -1) '(0.1 0.1 0.1)) :light t))))

(defclass street-scene (camera-scene) ())

(defun make-street-scene ()  
  (make-instance
   'street-scene
   :cam-pos (gficl:make-vec '(10 10 10))
   :cam-target (gficl:-vec '(0 0 0))
   :objects
   (list
    (make-object (get-asset 'sphere) (object-matrix '(0.5 1 -7) '(0.4 0.4 0.4)))
    (make-object (get-asset 'cone) (object-matrix '(0.5 1 14) '(0.8 0.8 0.8)))
    (make-object (get-asset 'bunny) (object-matrix '(-2 1 0.5) '(2 2 2)))
    (make-object (get-asset 'street) (object-matrix '(0 -1 2) '(0.5 0.5 0.5))
		 :diffuse-texs (get-asset 'street-diffuse)))))

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
