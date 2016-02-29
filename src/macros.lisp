;;; net-wmarvel-argv-parser : A simple command line argument parser
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

(in-package net-wmarvel-argv-parser)
(provide 'net-wmarvel-argv-parser)

(defmacro defargumenthandler (name (argsym argvsym arg table) &body body)
  (let ((handler (gensym))
	(tablesym (gensym))
	(funcsym (gensym))
	(finsym (gensym)))
    `(let ((,handler #'(lambda (,argsym ,argvsym ,tablesym ,funcsym)
			 (declare (ignorable ,argsym))
			 (flet ((finish-parsing (,finsym)
				  (parse-argv ,finsym ,tablesym ,funcsym)))
			   ,@body))))
       (set-argument-handler ,arg  ,handler ,table)
       (set-argument-handler ',name ,handler ,table))))
