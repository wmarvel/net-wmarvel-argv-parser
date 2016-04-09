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

(defun make-binding-form (binding)
  (let ((condition (first binding))
	(lambda-list (second binding))
	(body (rest (rest binding))))
    (let ((lambda-clauses (if lambda-list 
			      (list lambda-list)
			      (let ((sym (gensym)))
				`((,sym)(declare (ignorable ,sym)))))))
      `(,condition
	(lambda ,@lambda-clauses
	  ,@body)))))

(defmacro restartable-handler-case (form &body body)
  (let ((binding-forms (map 'list #'make-binding-form body)))
    `(handler-bind ,binding-forms
       ,form)))

(defun make-count-function ()
  (let ((count 0))
    (lambda (&optional (type :count))
      (ecase type
	(:count (incf count))
	(:report count)))))


