#|
 This file is a part of Trivial-Indent
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem trivial-arguments
  :name "Trivial-Arguments"
  :version "1.1.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple library to retrieve the lambda-list of a function."
  :homepage "https://Shinmera.github.io/trivial-arguments/"
  :bug-tracker "https://github.com/Shinmera/trivial-arguments/issues"
  :source-control (:git "https://github.com/Shinmera/trivial-arguments.git")
  :serial T
  :components ((:file "arguments"))
  :depends-on ((:feature :sbcl (:require :sb-introspect))))
