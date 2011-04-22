;;;; roswell.asd

(asdf:defsystem #:roswell
  :serial t
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "package")
               (:file "roswell")))

