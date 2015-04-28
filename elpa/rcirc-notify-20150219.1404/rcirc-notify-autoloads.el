;;; rcirc-notify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "rcirc-notify" "rcirc-notify.el" (21818 52476
;;;;;;  225238 6000))
;;; Generated autoloads from rcirc-notify.el

(autoload 'rcirc-notify-me "rcirc-notify" "\
Notify the current user when someone sends a message that
matches the current nick.

\(fn PROC SENDER RESPONSE TARGET TEXT)" t nil)

(autoload 'rcirc-notify-privmsg "rcirc-notify" "\
Notify the current user when someone sends a private message
to them.

\(fn PROC SENDER RESPONSE TARGET TEXT)" t nil)

(autoload 'rcirc-notify-add-hooks "rcirc-notify" "\
Initialize rcirc-notify into rcirc with hooks.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; rcirc-notify-autoloads.el ends here
