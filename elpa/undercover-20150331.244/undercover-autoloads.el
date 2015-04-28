;;; undercover-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "undercover" "undercover.el" (21818 52472 469238
;;;;;;  160000))
;;; Generated autoloads from undercover.el

(autoload 'undercover "undercover" "\
Enable test coverage for files matched by CONFIGURATION.
Example of CONFIGURATION: (\"*.el\" \"subdir/*.el\" (:exclude \"exclude-*.el\")).

If running under Travic CI automatically generate report
on `kill-emacs' and send it to coveralls.io.

\(fn &rest CONFIGURATION)" nil t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; undercover-autoloads.el ends here
