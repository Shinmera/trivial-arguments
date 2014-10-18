About Trivial-Arguments
-----------------------
Often times I need to know about the lambda-list of a function for some kind of automatic construction business. Since this is implementation-dependant and SWANK isn't always handy, there's this.

How To
------
Simple:

    (arg:arglist 'gethash)
    (arg:arglist #'gethash)
    (arg:arglist '(setf gethash))
    (arg:arglist #'(lambda (foo) (declare (ignore foo))))

And that's all there is to it. If the lambda-list could not be determined, `:unknown` is returned instead.
