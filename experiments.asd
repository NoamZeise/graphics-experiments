(load "framework.asd")

(require 'asdf)
(in-package :asdf-user)

(defsystem :experiments
	   :defsystem-depends-on (:deploy)
	   :build-operation "deploy-op"
	   :build-pathname "experiments"
	   :entry-point "experiments:run"
	   :depends-on (:framework)
	   :components ((:module "experiments"
			 :components
			 ((:file "package")
			  (:file "main")
			  (:file "general-shaders")
			  (:file "shadow")
			  (:file "scenes")
			  
			  (:file "metatexture")
			  (:file "outline")
			  (:file "xtoon")
			  (:file "brush")
			  (:file "halftone")
			  (:file "pbr")
			  (:file "lit-sphere")
			  (:file "cascades")
			  (:file "ssao")))))
