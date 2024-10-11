;; assume https://github.com/NoamZeise/gficl is cloned in this folder
(load "gficl/gficl.asd")
(require 'asdf)
(in-package :asdf-user)

(defsystem :project
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "project"
  :entry-point "project:run"
  :depends-on (:gficl
	       :gficl/load
	       :alexandria)
  :components ((:module "code"
		:components
		((:file "package")
		 (:file "constant")
		 (:file "generic")
		 (:file "main")
		 (:file "assets")
		 (:file "object")
		 (:file "scene")
		 (:file "shader")
		 (:file "pass")
		 (:file "basic-pass")
		 (:file "metatexture")
		 (:file "plane-scene")))))
