(deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)
#+windows (deploy:define-library deploy::libwinpthread :dont-deploy t)

(defpackage experiments
  (:use :cl :framework)
  (:export #:run))
