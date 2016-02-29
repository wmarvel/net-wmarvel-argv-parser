;;; net-wmarvel-argv-parser-tests : Test the net-wmarvel-argv-parser package
;;  Copyright (C) 1999-2016 Wendall A. Marvel

;;  This library is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU Lesser General Public
;;  License as published by the Free Software Foundation; either
;;  version 2.1 of the License, or (at your option) any later version.

;;  This library is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  Lesser General Public License for more details.

;;  You should have received a copy of the GNU Lesser General Public
;;  License along with this library; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(in-package net-wmarvel-argv-parser-tests)
(provide 'net-wmarvel-argv-parser-tests)

;;; Probably just need one suite, for now
(def-suite argv-parser-tests :description "Argument parser testing suite")
(in-suite argv-parser-tests)

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
      (unknown-argument-error (condition)
	(princ condition)
	(upcount :restarts)
	(invoke-restart 'replace-current-arguments '("--foo" "--bar"))))))
