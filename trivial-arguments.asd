#|
 This file is a part of Trivial-Indent
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem trivial-arguments
  :name "Trivial-Arguments"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple library to retrieve the lambda-list of a function."
  :homepage "https://github.com/Shinmera/trivial-arguments"
  :serial T
  :components ((:file "arguments"))
  :depends-on ())
