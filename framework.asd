;; assume https://github.com/NoamZeise/gficl is cloned in this folder
(load "gficl/gficl.asd")
(require 'asdf)
(in-package :asdf-user)

(defsystem :framework
  :depends-on (:gficl
	       :gficl/load
	       :alexandria
	       :trivial-main-thread
	       :file-notify)
  :components ((:module "framework"
		:components
		((:file "package")
		 (:file "framework")
		 (:file "constants")
		 (:file "generic")
		 (:file "assets")
		 (:file "object")
		 (:file "scene")
		 (:file "shader")
		 (:file "pass")
		 (:file "pipeline")
		 (:file "hot-reload")))))
