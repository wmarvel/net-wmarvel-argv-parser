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
;;;

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


