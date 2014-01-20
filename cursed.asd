(defpackage :cursed-asd
  (:use :cl :asdf))

(in-package :re-asd)

(defsystem :cursed
  :name "cursed"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "libcurses-style output-pane for LispWorks."
  :serial t
  :components ((:file "cursed")))
