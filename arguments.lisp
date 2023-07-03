(defpackage #:trivial-arguments
  (:use #:cl)
  (:nicknames #:org.shirakumo.trivial-arguments #:arg)
  (:export
   #:arglist))
(in-package #:org.shirakumo.trivial-arguments)

#-(or :abcl :allegro :ccl :clasp :clisp :cmucl :corman :ecl :lispworks :mezzano :sbcl :scl)
(warn "TRIVIAL-ARGUMENTS NOTICE: Your implementation is not directly supported. Falling back to FUNCTION-LAMBDA-EXPRESSION.")

(defmacro with-unknown-on-error (&body body)
  `(handler-case (progn ,@body)
     (error () :unknown)))

(defun arglist (function)
  "Returns the lambda-list of the function if possible, :unknown otherwise."
  (let ((function (etypecase function
                    ((or list symbol function) function))))
    #+:abcl
    (multiple-value-bind (list provided) (sys::arglist function)
      (if provided list :unknown))

    #+:allegro
    (with-unknown-on-error
      (excl:arglist function))

    #+:ccl
    (multiple-value-bind (list provided) (ccl:arglist function)
      (if provided list :unknown))

    #+:clasp
    (multiple-value-bind (list provided) (core:function-lambda-list function)
      (if provided list :unknown))

    #+:clisp
    (with-unknown-on-error
      (ext:arglist function))

    #+:cmucl
    (with-unknown-on-error
      (cond ((eval:interpreted-function-p function)
             (eval:interpreted-function-arglist function))
            ((pcl::generic-function-p function)
             (pcl:generic-function-lambda-list function))
            ((c::byte-function-or-closure-p function)
             (byte-code-function-arglist function))
            ((kernel:%function-arglist (kernel:%function-self function))
             (read-arglist function))
            (T
             (debug-function-arglist (di::function-debug-function function)))))

    #+:corman
    (with-unknown-on-error
      (cond ((and (symbolp function) (macro-function function))
             (ccl::macro-lambda-list (symbol-function function)))
            (T
             (when (symbolp function)
               (setf function (symbol-function function)))
             (if (eq (class-of name) cl::the-class-standard-gf)
                 (generic-function-lambda-list name)
                 (ccl:function-lambda-list name)))))

    #+:ecl
    (multiple-value-bind (list provided) (ext:function-lambda-list function)
      (if provided list :unknown))

    #+:lispworks
    (let ((list (lw:function-lambda-list function)))
      (if (eq list :dont-know) :unknown list))

    #+mezzano
    (with-unknown-on-error
      (mezzano.debug:function-lambda-list function))

    #+:sbcl
    (multiple-value-bind (list unknown) (sb-introspect:function-lambda-list function)
      (if unknown :unknown list))

    #+:scl
    (multiple-value-bind (list provided) (ext:function-arglist function)
      (if provided list :unknown))

    #-(or :abcl :allegro :ccl :clasp :clisp :cmucl :corman :ecl :lispworks :mezzano :sbcl :scl)
    (let ((lambda (function-lambda-expression (etypecase function
                                                ((or list symbol) (fdefinition function))
                                                (function function)))))
      (if lambda (second lambda) :unknown))))
