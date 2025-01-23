(in-package :framework)

(defconstant +asset-folder+ #p"assets/")
(defconstant +shader-folder+ #p"shaders/")

(alexandria:define-constant +world-up+ (gficl:make-vec'(0 1 0))
			    :test #'(lambda (x y) (gficl:=vec x y)))

(defun shader-subfolder (folder)
  (merge-pathnames folder (merge-pathnames +shader-folder+)))
