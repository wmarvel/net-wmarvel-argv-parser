;;; net-wmarvel-argv-parser : A simple command line argument parser
;;  Copyright (C) 1999-20016 Wendall A. Marvel

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

(require 'asdf)

(defsystem :net-wmarvel-argv-parser
    :author "Wendall A. Marvel"
    :licence "LGPL"
    :depends-on (:split-sequence)
    :components ((:module "src"
		  :serial t
		  :components ((:file "package")
			       (:file "macros")
			       (:file "argv-parser")))))

