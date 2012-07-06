;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (common-lisp-indent-function) "slime/contrib/slime-cl-indent"
;;;;;;  "slime/contrib/slime-cl-indent.el" (20470 19277 942977 664000))
;;; Generated autoloads from slime/contrib/slime-cl-indent.el

(autoload 'common-lisp-indent-function "slime/contrib/slime-cl-indent" "\
Function to indent the arguments of a Lisp function call.
This is suitable for use as the value of the variable
`lisp-indent-function'.  INDENT-POINT is the point at which the
indentation function is called, and STATE is the
`parse-partial-sexp' state at that position.  Browse the
`lisp-indent' customize group for options affecting the behavior
of this function.

If the indentation point is in a call to a Lisp function, that
function's common-lisp-indent-function property specifies how
this function should indent it.  Possible values for this
property are:

* defun, meaning indent according to `lisp-indent-defun-method';
  i.e., like (4 &lambda &body), as explained below.

* any other symbol, meaning a function to call.  The function should
  take the arguments: PATH STATE INDENT-POINT SEXP-COLUMN NORMAL-INDENT.
  PATH is a list of integers describing the position of point in terms of
  list-structure with respect to the containing lists.  For example, in
  ((a b c (d foo) f) g), foo has a path of (0 3 1).  In other words,
  to reach foo take the 0th element of the outermost list, then
  the 3rd element of the next list, and finally the 1st element.
  STATE and INDENT-POINT are as in the arguments to
  `common-lisp-indent-function'.  SEXP-COLUMN is the column of
  the open parenthesis of the innermost containing list.
  NORMAL-INDENT is the column the indentation point was
  originally in.  This function should behave like `lisp-indent-259'.

* an integer N, meaning indent the first N arguments like
  function arguments, and any further arguments like a body.
  This is equivalent to (4 4 ... &body).

* a list starting with `as' specifies an indirection: indentation is done as
  if the form being indented had started with the second element of the list.

* any other list.  The list element in position M specifies how to indent the
  Mth function argument.  If there are fewer elements than function arguments,
  the last list element applies to all remaining arguments.  The accepted list
  elements are:

  * nil, meaning the default indentation.

  * an integer, specifying an explicit indentation.

  * &lambda.  Indent the argument (which may be a list) by 4.

  * &rest.  When used, this must be the penultimate element.  The
    element after this one applies to all remaining arguments.

  * &body.  This is equivalent to &rest lisp-body-indent, i.e., indent
    all remaining elements by `lisp-body-indent'.

  * &whole.  This must be followed by nil, an integer, or a
    function symbol.  This indentation is applied to the
    associated argument, and as a base indent for all remaining
    arguments.  For example, an integer P means indent this
    argument by P, and all remaining arguments by P, plus the
    value specified by their associated list element.

  * a symbol.  A function to call, with the 6 arguments specified above.

  * a list, with elements as described above.  This applies when the
    associated function argument is itself a list.  Each element of the list
    specifies how to indent the associated argument.

For example, the function `case' has an indent property
\(4 &rest (&whole 2 &rest 1)), meaning:
  * indent the first argument by 4.
  * arguments after the first should be lists, and there may be any number
    of them.  The first list element has an offset of 2, all the rest
    have an offset of 2+1=3.

\(fn INDENT-POINT STATE)" nil nil)

;;;***

;;;### (autoloads nil nil ("el-get/el-get-install.el" "el-get/el-get.el"
;;;;;;  "slime/contrib/bridge.el" "slime/contrib/inferior-slime.el"
;;;;;;  "slime/contrib/slime-asdf.el" "slime/contrib/slime-autodoc.el"
;;;;;;  "slime/contrib/slime-banner.el" "slime/contrib/slime-c-p-c.el"
;;;;;;  "slime/contrib/slime-clipboard.el" "slime/contrib/slime-compiler-notes-tree.el"
;;;;;;  "slime/contrib/slime-editing-commands.el" "slime/contrib/slime-enclosing-context.el"
;;;;;;  "slime/contrib/slime-fancy-inspector.el" "slime/contrib/slime-fancy.el"
;;;;;;  "slime/contrib/slime-fontifying-fu.el" "slime/contrib/slime-fuzzy.el"
;;;;;;  "slime/contrib/slime-highlight-edits.el" "slime/contrib/slime-hyperdoc.el"
;;;;;;  "slime/contrib/slime-indentation.el" "slime/contrib/slime-mdot-fu.el"
;;;;;;  "slime/contrib/slime-media.el" "slime/contrib/slime-motd.el"
;;;;;;  "slime/contrib/slime-mrepl.el" "slime/contrib/slime-package-fu.el"
;;;;;;  "slime/contrib/slime-parse.el" "slime/contrib/slime-presentation-streams.el"
;;;;;;  "slime/contrib/slime-presentations.el" "slime/contrib/slime-references.el"
;;;;;;  "slime/contrib/slime-repl.el" "slime/contrib/slime-sbcl-exts.el"
;;;;;;  "slime/contrib/slime-scheme.el" "slime/contrib/slime-scratch.el"
;;;;;;  "slime/contrib/slime-snapshot.el" "slime/contrib/slime-sprof.el"
;;;;;;  "slime/contrib/slime-tramp.el" "slime/contrib/slime-typeout-frame.el"
;;;;;;  "slime/contrib/slime-xref-browser.el" "slime/hyperspec.el"
;;;;;;  "slime/slime-autoloads.el" "slime/slime.el") (20470 19284
;;;;;;  130261 905000))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
