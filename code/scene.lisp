(in-package :project)

(defclass scene ()
  ((objects :initarg :objects)
   (view-projection :accessor view-projection :initform (gficl:make-matrix) :type gficl:matrix)
   (cam-pos :initarg :cam-pos :type gficl:vec)))

(defgeneric update-scene (scene dt)
  (:documentation "Update the scene's objects"))

(defmethod initialize-instance :after ((obj scene) &key &allow-other-keys)
  (resize obj (gficl:window-width) (gficl:window-height)))

(defmethod resize ((obj scene) w h))

(defmethod update-scene ((obj scene) dt))

(defmethod draw ((scene scene) shader)
  (shader-scene-props shader scene)
  (loop for o in (slot-value scene 'objects) do (draw o shader)))

(defclass scene-3d (scene)
  ((projection-mat :initform (gficl:make-matrix) :type gficl:matrix)
   (cam-pos :initarg :cam-pos :type gficl:vec)
   (cam-target :initarg :cam-target :type gficl:vec)))

(defmethod resize ((obj scene-3d) w h)
  (setf (slot-value obj 'projection-mat)
	(gficl:screen-perspective-matrix w h (* pi 0.3) 0.05)))

(defmethod update-scene ((obj scene-3d) (dt number))
  (with-slots ((vp view-projection) (proj projection-mat) cam-pos cam-target) obj
    (let ((view (gficl:view-matrix cam-pos (gficl:-vec cam-target cam-pos) +world-up+)))
      (setf vp (gficl:*mat proj view)))))

(defclass scene-2d (scene)
  ((projection-mat :initform (gficl:make-matrix) :type gficl:matrix)
   (cam-target :initform (gficl:make-vec '(0 0)) :type gficl:vec)))

(defmethod resize ((obj scene-2d) w h)
  (setf (slot-value obj 'projection-mat)
	(gficl:screen-orthographic-matrix w h)))

(defmethod update-scene ((obj scene-2d) dt)
  (with-slots ((vp view-projection) (proj projection-mat) (cam cam-target)) obj
    (setf vp (gficl:*mat proj (gficl:translation-matrix (gficl:get-n-vec 3 cam))))))
