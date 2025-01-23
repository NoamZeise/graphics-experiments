(in-package :framework)

(defclass scene ()
  ((objects :initarg :objects)
   (view-projection :accessor view-projection :initform (gficl:make-matrix) :type gficl:matrix)
   (cam-pos :accessor cam-pos :initarg :cam-pos :type gficl:vec)
   (width :initform 0)
   (height :initform 0))
  (:documentation "A state to be drawn by a shader. Usually a list of objects and a camera. Also encapsulates updating the scene each frame (ie in response to user input)."))

(defgeneric update-scene (scene dt)
  (:documentation "Update the scene's objects"))

(defmethod initialize-instance :after ((obj scene) &key &allow-other-keys)
  (resize obj (gficl:window-width) (gficl:window-height)))

(defmethod resize ((obj scene) w h)
  (with-slots (width height) obj
    (setf width w)
    (setf height h)))

(defmethod update-scene ((obj scene) dt))

(defmethod draw ((scene scene) shader)
  (shader-scene-props shader scene)
  (loop for o in (slot-value scene 'objects) do (draw o shader)))

;;; 3d camera scene

(defclass scene-3d (scene)
  ((projection-mat :initform (gficl:make-matrix) :type gficl:matrix)
   (cam-target :initarg :cam-target :type gficl:vec)
   (cam-fov :initform 0.3 :type float)
   (cam-near :initform 0.05 :type float)
   (light-dir :accessor light-dir
	      :initform
	      (gficl:normalise (gficl:make-vec '(-0.1 3 2)))
	      :type gficl:vec)
   (light-view :type gficl:matrix)
   (light-proj :type gficl:matrix
	       :initform
	       (gficl:orthographic-matrix 16 -12 16 -16 0 -200))
   (light-vp :type gficl:matrix :initform (gficl:make-matrix))
   (light-near :initform 1)
   (light-far :initform 50)))

(defmethod resize ((obj scene-3d) w h)
  (call-next-method)
  (with-slots ((proj projection-mat) (fov cam-fov) (near cam-near)) obj
    (setf proj (gficl:screen-perspective-matrix w h (* pi fov) near 100))))

(defmethod update-scene :after ((obj scene-3d) (dt number))
  (with-slots ((vp view-projection) (proj projection-mat) cam-pos cam-target light-dir light-view light-proj light-vp) obj
    (let ((view (gficl:view-matrix cam-pos (gficl:-vec cam-target cam-pos) +world-up+)))
      (setf vp (gficl:*mat proj view)))
    (setf light-view (gficl:view-matrix (gficl:*vec 20 light-dir)
					(gficl:-vec light-dir)
					+world-up+))
    (setf light-vp (gficl:*mat light-proj light-view))))

;;; 2d camera scene

(defclass scene-2d (scene)
  ((projection-mat :initform (gficl:make-matrix) :type gficl:matrix)))

(defmethod resize ((obj scene-2d) w h)
  (call-next-method)
  (setf (slot-value obj 'projection-mat)
	(gficl:screen-orthographic-matrix w h)))

(defmethod update-scene :after ((obj scene-2d) dt)
  (with-slots ((vp view-projection) (proj projection-mat) (cam cam-pos)) obj
    (setf vp (gficl:*mat proj (gficl:translation-matrix (gficl:get-n-vec 3 cam))))))

;;; post-processing scene

(defclass post-scene (scene)
  ((target-width :initarg :target-width :initform 0 :type number)
   (target-height :initarg :target-height :initform 0 :type number)
   (transform :initform (gficl:make-matrix) :type gficl:matrix)
   (tex-alist :initform nil)))

(defmethod resize ((obj post-scene) w h)
  (call-next-method)
  (with-slots ((tw target-width) (th target-height) transform) obj
    (if (and (> tw 0) (> th 0))
	(setf transform (gficl:target-resolution-matrix tw th w h))
      (setf transform (gficl:make-matrix)))))

(defun set-post-texs (post-scene tex-alist)
  (setf (slot-value post-scene 'tex-alist) tex-alist))

(defun get-post-tex-alist (post-scene key)
  (let ((texs (assoc key (slot-value post-scene 'tex-alist))))
    (cdr texs)))

(defun get-post-tex (post-scene key framebuffer-position)
  (cdr (assoc framebuffer-position (get-post-tex-alist post-scene key))))

(defmethod draw ((scene post-scene) shader)
  "draw scene without calling draw on objects (only calls shader scene props)"
  (shader-scene-props shader scene))
