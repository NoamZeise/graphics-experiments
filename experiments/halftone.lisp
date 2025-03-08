(in-package :experiments)

;;; halftone shader

(defclass halftone-shader (normals-cam-shader)
  ((shadow-map)))

(defmethod reload ((s halftone-shader))
  (shader-reload-files (s (#p"halftone.vs"  #p"halftone.fs")
			  :folder (shader-subfolder #p"halftone/"))
		       shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)
    (gl:uniformi (gficl:shader-loc shader "shadow_map") 1)))

(defmethod draw ((obj halftone-shader) scene)
  (gl:enable :depth-test :cull-face)
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d (slot-value obj 'shadow-map))
  (call-next-method))

(defmethod shader-mesh-props ((obj halftone-shader) props)
  (let ((dt (cdr (assoc :diffuse props))))
    (gl:active-texture :texture0)
    (cond (dt (gficl:bind-gl (if (listp dt) (car dt) dt)))
	  (t  (gficl:bind-gl (car (get-asset 'light-colours)))))))

(defmethod shader-scene-props ((obj halftone-shader) (scene scene-3d))
  (call-next-method)
  (with-slots (light-vp) scene
    (gficl:bind-matrix (slot-value obj 'shader) "shadow_vp" light-vp)))

;;; colour pass 

(defclass halftone-colour-pass (pass) ())

(defun make-halftone-colour-pass ()
  (make-instance 'halftone-colour-pass
     :shaders (list (make-instance 'halftone-shader)
		    (make-instance 'backface-shader
				   :outline-colour (gficl:make-vec '(0 0 0 0))
				   :outline-size 1
				   :normal-divisor 700.0
				   :near-factor 1.000
				   :polygon-offset -0.8))
     :clear-colour '(0.7 0.9 1.0 0.0)
     :description
     (make-framebuffer-description
      (list (gficl:make-attachment-description :type :texture)
	    (gficl:make-attachment-description :position :depth-attachment))
      :samples 16)))

;;; pipeline

(defclass halftone-pipeline (pipeline) ())

(defun make-halftone-pipeline ()
  (make-instance 'halftone-pipeline
    :passes (list (cons :col (make-halftone-colour-pass))
		  (cons :shadow (make-vsm-pass)))))

(defmethod initialize-instance :after ((pl halftone-pipeline) &key &allow-other-keys)	   
  (resize (get-pass pl :shadow) 1024 1024)
  (setf (slot-value (car (slot-value (get-pass pl :col) 'shaders)) 'shadow-map)
	(get-pass-texture (get-pass pl :shadow))))

(defmethod resize ((pl halftone-pipeline) (w integer) (h integer))
  (resize (get-pass pl :col) w h))

(defmethod draw ((pl halftone-pipeline) scenes)
  (draw (get-pass pl :shadow) scenes)
  (draw (get-pass pl :col) scenes)
  (gficl:blit-framebuffers
   (get-final-framebuffer (get-pass pl :col)) nil
   (gficl:window-width) (gficl:window-height)))
