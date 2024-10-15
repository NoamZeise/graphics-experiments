(in-package :project)

(defclass scene ()
  ((objects :initarg :objects)
   (view-projection :accessor view-projection :initform (gficl:make-matrix) :type gficl:matrix)
   (cam-pos :accessor cam-pos :initarg :cam-pos :type gficl:vec)))

(defgeneric update-scene (scene dt)
  (:documentation "Update the scene's objects"))

(defmethod initialize-instance :after ((obj scene) &key &allow-other-keys)
  (resize obj (gficl:window-width) (gficl:window-height)))

(defmethod resize ((obj scene) w h))

(defmethod update-scene ((obj scene) dt))

(defmethod draw ((scene scene) shader)
  (shader-scene-props shader scene)
  (loop for o in (slot-value scene 'objects) do (draw o shader)))

;;; 3d camera scene

(defclass scene-3d (scene)
  ((projection-mat :initform (gficl:make-matrix) :type gficl:matrix)
   (cam-target :initarg :cam-target :type gficl:vec)
   (cam-fov :initform 0.3 :type float)
   (cam-near :initform 0.05 :type float)))

(defmethod resize ((obj scene-3d) w h)
  (with-slots ((proj projection-mat) (fov cam-fov) (near cam-near)) obj
      (setf proj (gficl:screen-perspective-matrix w h (* pi fov) near))))

(defmethod update-scene :after ((obj scene-3d) (dt number))
  (with-slots ((vp view-projection) (proj projection-mat) cam-pos cam-target) obj
    (let ((view (gficl:view-matrix cam-pos (gficl:-vec cam-target cam-pos) +world-up+)))
      (setf vp (gficl:*mat proj view)))))

;;; 2d camera scene

(defclass scene-2d (scene)
  ((projection-mat :initform (gficl:make-matrix) :type gficl:matrix)))

(defmethod resize ((obj scene-2d) w h)
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
  (with-slots ((tw target-width) (th target-height) transform) obj
    (if (and (> tw 0) (> th 0)) 
	(setf transform (gficl:target-resolution-matrix tw th w h)))))

(defun set-post-texs (post-scene tex-alist)
  (setf (slot-value post-scene 'tex-alist) tex-alist))

(defun get-post-tex (post-scene key)
  (cadr (assoc key (slot-value post-scene 'tex-alist))))

(defmethod draw ((scene post-scene) shader)
  "draw scene without calling draw on objects (only calls shader scene props)"
  (shader-scene-props shader scene))
