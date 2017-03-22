#|
 This file is a part of Trivial-Indent
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:trivial-arguments
  (:use #:cl)
  (:nicknames #:org.shirakumo.trivial-arguments #:arg)
  (:export
   #:arglist))
(in-package #:org.shirakumo.trivial-arguments)

#-(or :swank :abcl :allegro :ccl :clisp :ecl :lispworks :sbcl :scl :clasp)
(warn "TRIVIAL-ARGUMENTS NOTICE: SWANK not present and implementation not directly supported. Falling back to FUNCTION-LAMBDA-EXPRESSION.")

(defun arglist (function)
  "Returns the lambda-list of the function if possible, :unknown otherwise."
  (let ((function (etypecase function
                    ((or list symbol function) function))))
    #+:swank
    (let ((result (#.(find-symbol "ARGLIST" (cond ((find-package "SWANK-BACKEND") "SWANK-BACKEND")
                                                  ((find-package "SWANK/BACKEND") "SWANK/BACKEND")
                                                  (T (error "Swank backend package could not be found."))))
                     function)))
      (if (eq result :not-provided) :unknown result))

    #+(and :abcl (not :swank))
    (multiple-value-bind (list provided) (sys::arglist function)
      (if provided list :unknown))

    #+(and :allegro (not :swank))
    (or (ignore-errors (excl:arglist function))
        :unknown)

    #+(and :ccl (not :swank))
    (multiple-value-bind (list provided) (ccl:arglist function)
      (if provided list :unknown))

    #+(and :clisp (not :swank))
    (or (ignore-errors (ext:arglist function))
        :unknown)

    #+(and :ecl (not :swank) (not :clasp))
    (multiple-value-bind (list provided) (ext:function-lambda-list function)
      (if provided list :unknown))

    #+(and :lispworks (not :swank))
    (let ((list (lw:function-lambda-list function)))
      (if (eq list :dont-know) :unknown list))
    
    #+(and :sbcl (not :swank))
    (sb-introspect:function-lambda-list function)

    #+(and :scl (not :swank))
    (multiple-value-bind (list provided) (ext:function-arglist function)
      (if provided list :unknown))

    #+(and :clasp (not :swank))
    (core:function-lambda-list function)

    #-(or :swank :abcl :allegro :ccl :clisp :ecl :lispworks :sbcl :scl :clasp)
    (let ((lambda (function-lambda-expression (etypecase function
                                                ((or list symbol) (fdefinition function))
                                                (function function)))))
      (if lambda (second lambda) :unknown))))
