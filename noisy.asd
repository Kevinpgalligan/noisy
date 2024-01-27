(defpackage :noisy-asd
  (:use :cl :asdf))

(in-package :noisy-asd)

(defsystem noisy
  :license "MIT"
  :author "Kevin Galligan"
  :description "Perlin noise for arbitrary numbers of dimensions."
  :version "1.0.0"
  :depends-on (:random-state)
  :pathname "src"
  :serial t
  :components ((:file "noisy")))
