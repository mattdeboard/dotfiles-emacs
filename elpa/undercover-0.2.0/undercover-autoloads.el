;;; undercover-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "undercover" "undercover.el" (21582 43683 908221
;;;;;;  527000))
;;; Generated autoloads from undercover.el

(autoload 'undercover "undercover" "\
Enable test coverage for files matched by WILDCARDS.
Example of WILDCARDS: (\"*.el\" \"subdir/*.el\" (:exclude \"exclude-*.el\")).

If running under Travic CI automatically generate report
on `kill-emacs' and send it to coveralls.io.

\(fn &rest WILDCARDS)" nil t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; undercover-autoloads.el ends here
