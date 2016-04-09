;;; net-wmarvel-argv-parser : A simple command line argument parser

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

(in-package net-wmarvel-argv-parser)
(provide 'net-wmarvel-argv-parser)

;; Simple approach: We have a function, parse-argv, which takes three arguments:
;; The argv (e.g. *posix-argv* in sbcl), a table of argument to argument handler
;; mappings, and a function to call at the bottom of the recursion.

;; The table should contain functions (or a symbol that has a function binding)
;; that take four arguments: the current argument, a list of the rest of the
;; arguments, in order, the table, and the function to call 

;; The handler function, when it's done whatever it needs to do, needs to make
;; a recursive call to parse-argv, handing it everything in the argv list that
;; it didn't need to take as an argument.

;; At the bottom of the recursion, parse-argv will just call the function.
;; The intent is that the function called at the bottom is the function that
;; actually does the work of the application - allowing the hook handlers to
;; rebind special variables if they need to.

;; If an application wants the parser to just setf modify everything, and
;; then do whatever it needs to do outside of a parse-argv call stack, it can
;; pass an empty lambda to parse-argv, and do whatever it wants to do after
;; parse-argv returns

;; You can manually set up handlers in the table, but it's probably easier to
;; do it with defargumenthandler.

;; defargumenthandler is provided to update the table with the function, 
;; with flet binding for finish-parsing around the body. finish-parsing
;; must be called with only the remaining arguments that need to be handled
;; somewhere within the body of the handler. This hides the fact that we're
;; actually calling parse-argv when finish-parsing is called.

;; If finish-parsing is *not* called within the body, we will end up returning
;; up the stack immediately, which is probably not what you want at all.

;; For instance, the following sets up a default handler that warns about
;; bare arguments, except for the first one (skipping argv[0], which is the
;; name of the program)

;;(defvar *warn-on-bare-arguments* nil)
;;(defvar *argument-handlers* (make-argument-handler-table))

;;(defargumenthandler bare-argument
;;    (switch list *default-argument-handler-key* *argument-handlers*)
;;  (cond
;;    (*warn-on-bare-arguments*
;;     (format T "~&Warning: skipping bare argument ~S~%" switch)
;;     (finish-parsing list))
;;    (T
;;     (let ((*warn-on-bare-arguments* t))
;;       (finish-parsing list)))))

;; The default handler, if set, should handle bare arguments
(defvar *default-argument-handler-key* 'default-argument-handler
  "The key used to look up the default argument handler when no handler is
   found using the argument being processed")

(defvar *remaining-arguments-format-control*
  "The remaining unprocessed arguments are \"~{~A~^ ~}\"~%"
  "The format control used to report the remaining arguments in errors
   and warnings")

(defvar *unknown-argument-format-control*
  (concatenate 'string
	       "No argument handler found for argument \"~A\".  "
	       *remaining-arguments-format-control*)
  "The format control used to report unhandled-argument-condition")

(defvar *ignore-argument-warning-format-control*
  (concatenate 'string
	       "Ignoring argument \"~A\".  "
	       *remaining-arguments-format-control*)
  "The format control used to warn that an argument is being ignored")

(defvar *argument-whitespace*
  '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)
  "Whitespace characters used to trim the string read when the use-argument
   restart is interactive")

(define-condition unknown-argument-error (error)
  ((argument :accessor unhandled-argument
	     :initarg :argument)
   (remaining :accessor remaining-arguments
	      :initarg :remaining))
  (:report (lambda (condition stream)
	     (format stream 
		     *unknown-argument-format-control*
		     (unhandled-argument condition)
		     (remaining-arguments condition))))
  (:documentation
   "Signaled when no argument handler is found in the argument handler table
    passed to parse-argv"))

(defun make-argument-handler-table ()
  "make-argument-handler-table => table
   Returns a table of argument strings to argument handler functions"
  (make-hash-table :test #'equal))

(defun set-argument-handler (switch function argument-table)
  "set-argument-handler switch function argument-table => function
   Sets function as the argument handler to be used to handle the given
   switch when argument-table is passed to parse-argv"
  (setf (gethash switch argument-table) function))

(defun get-argument-handler (switch argument-table)
  "sget-argument-handler switch argument-table => function
   Get the argument handler to be used to handle the given switch
   when argument-table is passed to parse-argv"
  (or (gethash switch argument-table)
      (gethash 'default-argument-handler argument-table)))

(defun read-argument (type)
  (let ((type (ecase type (:replacing "replace") (:prepending "add before"))))
    (format T "Enter an argument to ~A the original argument: " type)
    (list (string-trim *argument-whitespace* (read-line)))))

(defun read-replacing-argument ()
  (read-argument :replacing))

(defun read-prepending-argument ()
  (read-argument :prepending))

(defun read-argument-list ()
  (format T "Enter the new arguments: ")
  (list
   (split-sequence #\Space 
		   (string-trim *argument-whitespace* (read-line))
		   :remove-empty-subseqs T)))

(defun unknown-argument-error (arg argv handlers function)
  (restart-case (error 'unknown-argument-error :argument arg :remaining argv)
    (ignore-argument ()
      :report "Ignore the argument."
      (warn *ignore-argument-warning-format-control* arg argv)
      (parse-argv argv handlers function))
    (replace-argument (new-arg)
      :report "Replace the original argument with a new argument."
      :interactive read-replacing-argument
      (parse-argv (cons new-arg argv) handlers function))
    (prepend-argument (new-arg)
      :report "Add a new argument before the original argument."
      :interactive read-prepending-argument
      (parse-argv `(,new-arg ,arg . ,argv) handlers function))
    (replace-current-arguments (new-argv)
      :report "Replace all unprocessed arguments with new arguments."
      :interactive read-argument-list
      (parse-argv new-argv handlers function))))

(defun parse-argv (argv handlers function)
  "Parse a command line, e.g. *posix-argv*, using handlers as the table
   of argument handlers. The given function will be called without arguments
   once all argument handlers have been processed."
  (if (null argv)
      (funcall function)
      (let* ((arg (first argv))
	     (argv (rest argv))
	     (argparser (get-argument-handler arg handlers)))
	(if argparser
	    (funcall argparser arg argv handlers function)
	    (unknown-argument-error arg argv handlers function)))))

		
		  
		
