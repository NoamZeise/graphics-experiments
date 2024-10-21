(deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)
#+windows (deploy:define-library deploy::libwinpthread :dont-deploy t)

(defpackage project
  (:use :cl)
  (:local-nicknames (:notify :org.shirakumo.file-notify))
  (:export #:run))
