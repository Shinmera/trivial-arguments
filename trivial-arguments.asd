(asdf:defsystem trivial-arguments
  :name "Trivial-Arguments"
  :version "1.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A simple library to retrieve the lambda-list of a function."
  :homepage "https://shinmera.com/docs/trivial-arguments/"
  :bug-tracker "https://shinmera.com/project/trivial-arguments/issues"
  :source-control (:git "https://shinmera.com/project/trivial-arguments.git")
  :serial T
  :components ((:file "arguments"))
  :depends-on ((:feature :sbcl (:require :sb-introspect))))
