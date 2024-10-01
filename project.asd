;; assume https://github.com/NoamZeise/gficl is cloned in path above this one
;; TODO better system to pull it if unavailable
(load "../gficl/gficl.asd")
(require 'asdf)
(in-package :asdf-user)

(defsystem :project
  :depends-on (:gficl
	       :gficl/load
	       :alexandria)
  :components ((:module "code"
		:components
		((:file "package")
		 (:file "main")
		 (:file "assets")))))
