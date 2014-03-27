;;; cider-tracing-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "cider-tracing" "cider-tracing.el" (21230 64269
;;;;;;  454238 103000))
;;; Generated autoloads from cider-tracing.el

(autoload 'cider-toggle-trace "cider-tracing" "\
Toggle tracing for the given QUERY.
Defaults to the symbol at point.  With prefix arg or no symbol at
point, prompts for a var.

\(fn QUERY)" t nil)

(eval-after-load 'clojure-mode '(progn (define-key clojure-mode-map (kbd "C-c M-t") 'cider-toggle-trace)))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cider-tracing-autoloads.el ends here
