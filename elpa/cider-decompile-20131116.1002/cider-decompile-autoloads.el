;;; cider-decompile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "cider-decompile" "cider-decompile.el" (21230
;;;;;;  64270 122238 80000))
;;; Generated autoloads from cider-decompile.el

(autoload 'cider-decompile-func "cider-decompile" "\
Asks for the func name (FN-NAME) in the current namespace.and decompiles.

\(fn)" t nil)

(autoload 'cider-decompile-ns-func "cider-decompile" "\
Asks for the func name (FN-NAME) in a specific namespace and decompiles it.
The FN-NAME should be prefixed with the namespace.

\(fn FN-NAME)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cider-decompile-autoloads.el ends here
