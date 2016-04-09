net-wmarvel-argv-parser is a simple Common Lisp utility useful for
parsing command line arguments such as sb-ext:*posix-argv* that was originally
written to parse the command line arguments of the WMBot IRC bot.

The code is ANSI and does not reference the internals of any specific
Common Lisp implementation.

Simple approach: We have a function, parse-argv, which takes three arguments:
The argv (e.g. *posix-argv* in sbcl), a table of argument to argument handler
mappings, and a function to call at the bottom of the recursion.

The table should contain functions (or a symbol that has a function binding)
that take four arguments: the current argument, a list of the rest of the
arguments, in order, the table, and the function to call 

The argument handler must make a recursive call to parse-argv, with all
unconsumed arguments from the argv list.

At the bottom of the recursion, parse-argv will just call the function.
The intent is that the function called at the bottom is the function that
actually does the work of the application - allowing the argument handlers to
rebind special variables if they need to.

If you need your application to destructively modify variables, and run
outside of a parse-argv call stack, you can pass an empty lambda to parse-argv,
and start  wants to do after parse-argv returns

You can manually set up handlers in the table, but it's probably easier to
do it with defargumenthandler.

defargumenthandler is provided to update the table with the function, 
with flet binding for finish-parsing around the body. finish-parsing
must be called with only the remaining arguments that need to be handled
somewhere within the body of the handler. This hides the fact that we're
actually calling parse-argv when finish-parsing is called.

If finish-parsing is *not* called within the body, we will end up returning
up the stack immediately, and never call the program function.

For instance, the following sets up a default handler that warns about
bare arguments, except for the first one (which is the name of the program
itself). 

(defvar *warn-on-bare-arguments* nil)
(defvar *argument-handlers* (make-argument-handler-table))

(defargumenthandler bare-argument
   (switch list *default-argument-handler-key* *argument-handlers*)
 (cond
   (*warn-on-bare-arguments*
    (format T "~&Warning: skipping bare argument ~S~%" switch)
    (finish-parsing list))
   (T
    (let ((*warn-on-bare-arguments* t))
      (finish-parsing list)))))




