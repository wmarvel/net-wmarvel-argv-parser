;;; net-wmarvel-argv-parser-tests : Test the net-wmarvel-argv-parser package

;; Copyright (c) 1999-2016 Wendall A. Marvel 

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
(in-package net-wmarvel-argv-parser-tests)
(provide 'net-wmarvel-argv-parser-tests)

;;; Probably just need one suite, for now
(def-suite argv-parser-suite :description "Argument parser testing suite")
(in-suite argv-parser-suite)

(def-fixture argv-test-fixture (expected-count-list)
  (let ((count-table (make-hash-table :test #'eq))
	(handler-table (make-argument-handler-table)))
    (flet ((upcount (symbol)
	     (setf (gethash symbol count-table)
		   (1+ (gethash symbol count-table 0))))
	   (getcount (symbol)
	     (gethash symbol count-table)))
      (let ((program-function #'(lambda () (upcount :parsing-completed))))
	(flet ((doparse (&rest arglist)
		 (parse-argv arglist handler-table program-function)))
	  (&body)))
      (dolist (expected expected-count-list)
	(destructuring-bind (key value) expected
	  (is (= value (getcount key))))))))

(test function-called
  "Test that the function passed to parse-argv gets called"
  (with-fixture argv-test-fixture ('((:parsing-completed 1)))
    (doparse)))

(test default-handler-called
  "Test that the default handler is run"
  (with-fixture argv-test-fixture ('((:parsing-completed 1)
				     (:bare-calls 1)))
    (defargumenthandler bare-argument-handler 
	(arg argv *default-argument-handler-key* handler-table)
      (upcount :bare-calls)
      (finish-parsing argv))
    (doparse "foo")))

(test handler-called
  "Test that a non-default handler is run"
  (with-fixture argv-test-fixture ('((:parsing-completed 1)
				     (:called 1)))
    (defargumenthandler foo-handler (arg argv "--foo" handler-table)
      (upcount :called)
      (finish-parsing argv))
    (doparse "--foo")))

(test unknown-argument-error
  "Test that the unknown-argument-error is correctly signaled"
  (signals unknown-argument-error 
    (with-fixture argv-test-fixture ('(:parsing-completed 0))
      (doparse "foo"))))

(test ignore-argument-restart
  "Test that the ignore-argument-restart works"
  (with-fixture argv-test-fixture ('((:parsing-completed 1)
				     (:ignored 1)
				     (:muffled 1)))
    (restartable-handler-case
	(doparse "--foo")
      (unknown-argument-error ()
	(upcount :ignored)
	(invoke-restart 'ignore-argument))
      (warning ()
	(upcount :muffled)
	(invoke-restart 'muffle-warning)))))

(test replace-argument-restart
  "Test that the replace-argument-restart works"
  (with-fixture argv-test-fixture ('((:parsing-completed 1)
				     (:restarts 1)
				     (:called 1)))
    (defargumenthandler foo-handler (arg argv "--foo" handler-table)
      (upcount :called)
      (finish-parsing argv))
    (restartable-handler-case
	(doparse "--bar")
      (unknown-argument-error ()
	(upcount :restarts)
	(invoke-restart 'replace-argument "--foo")))))

(test prepend-argument-restart
  "Test that the prepend-argument-restart works"
  (with-fixture argv-test-fixture ('((:parsing-completed 1)
				     (:restarts 1)
				     (:called 1)))
    (defargumenthandler foo-handler (arg argv "--foo" handler-table)
      (upcount :called)
      (finish-parsing (rest argv)))
    (restartable-handler-case
	(doparse "--bar")
      (unknown-argument-error ()
        (upcount :restarts)
	(invoke-restart 'prepend-argument "--foo")))))

(test replace-current-arguments
  "Test that the replace-current-arguments restart works"
  (with-fixture argv-test-fixture ('((:parsing-completed 1)
				     (:restarts 1)
				     (:foo-calls 2)
				     (:bar-calls 2)))
    (defargumenthandler foo-handler (arg argv "--foo" handler-table)
      (upcount :foo-calls)
      (finish-parsing argv))
    (defargumenthandler bar-handler (arg argv "--bar" handler-table)
      (upcount :bar-calls)
      (finish-parsing argv))
    (restartable-handler-case
	(doparse "--foo" "--bar" "baz" "bey" "--foo" "--foo" "--bar" "--bar")
      (unknown-argument-error ()
	(upcount :restarts)
	(invoke-restart 'replace-current-arguments '("--foo" "--bar"))))))
