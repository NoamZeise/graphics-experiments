(defpackage framework
  (:use :cl)
  (:local-nicknames (:notify :org.shirakumo.file-notify))
  (:export

   ;; globals
   #:+asset-folder+
   #:+shader-folder+
   #:+world-up+
   #:shader-subfolder
   
   ;; file reload
   #:init-watched
   #:process-watched
   #:watch-file
   #:watch-files
   #:file-modified
   #:files-modified
   #:set-all-unmodified
   #:unwatch-all
   #:set-all-modified
   
   ;; assets
   #:setup-asset-table
   #:cleanup-assets
   #:add-asset
   #:load-model
   #:load-model+texs
   #:load-image
   #:get-asset

   ;; generic ops
   #:reload
   #:resize
   #:draw
   #:free

   ;; objects
   #:object
   #:make-object
   #:make-object-from-model+tex
   #:update-model
   #:obj-prop
   #:object-matrix
   
   ;; scenes
   #:scene
   #:update-scene
   #:objects
   #:view-projection
   #:cam-pos
   #:width
   #:height
   ;; specialised scene types
   #:scene-3d
   #:projection-mat
   #:cam-target
   #:cam-fov
   #:cam-near
   #:light-dir
   #:light-view
   #:light-proj
   #:light-vp
   #:light-near
   #:light-far
   #:scene-2d
   ;; post-processing scene
   #:post-scene
   #:set-post-texs
   #:get-post-tex-alist
   #:get-post-tex

   ;; shader
   #:shader
   #:shader-model-props
   #:shader-mesh-props
   #:shader-scene-props
   ;; macros
   #:shader-reload-files
   #:compute-shader-reload-files
   ;; specialised shader types
   #:normals-shader
   #:normals-cam-shader
   #:post-shader

   ;; render pass
   #:framebuffer-description
   #:make-framebuffer-description
   #:pass
   #:get-textures
   #:get-pass-texture
   #:get-final-framebuffer
   #:shaders
   ;; specialised pass types
   #:post-pass

   ;; render pipeline
   #:pipeline
   ;; utility
   #:foreach-pl
   #:get-pl
   #:get-pass
   #:get-shader
   #:alist-fb-textures))
