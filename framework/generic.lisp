(in-package :framework)

(defgeneric reload (shader)
  (:documentation "reload asset (ie shader code changed)"))

(defgeneric resize (obj width height)
  (:documentation "resize the obj to the supplied width and height"))

(defgeneric draw (obj arg)
  (:documentation "draw using the given argument"))

(defgeneric free (obj)
  (:documentation "free the resources held by the object"))
