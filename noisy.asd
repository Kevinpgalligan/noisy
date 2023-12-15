(defpackage :noisy-asd
  (:use :cl :asdf))

(in-package :noisy-asd)

(defsystem noisy
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:random-state)
  :pathname "src"
  :serial t
  :components ((:file "noisy")))
