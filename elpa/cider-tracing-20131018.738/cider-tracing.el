;;; cider-tracing.el --- Tracing extension for CIDER

;; Copyright © 2013 Robert Ewald
;;
;; Author: Robert Ewald
;; URL: http://www.github.com/clojure-emacs/cider-tracing
;; Version: 20131018.738
;; X-Original-Version: 0.0.1
;; Keywords: languages, clojure, cider
;; Package-Requires: ((cider "0.3.0") (clojure-mode "2.1.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides an `cider-toggle-trace' command.

;;; Installation:

;; Available as a package in marmalade-repo.org and melpa.milkbox.net.

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;;
;; or
;;
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;
;; M-x package-install cider-tracing

;;; Usage:

;; M-x cider-toggle-trace

;;; Code:

(require 'cider)
(require 'clojure-mode)
(eval-when-compile (require 'cl))

;;;###autoload
(defun cider-toggle-trace (query)
  "Toggle tracing for the given QUERY.
Defaults to the symbol at point.  With prefix arg or no symbol at
point, prompts for a var."
  (interactive "P")
  (save-excursion
    (cider-read-symbol-name
     "Var: "
     (lexical-let ((buffer (current-buffer)))
       (lambda (x)
         (nrepl-send-string
          (format "(require 'clojure.tools.trace)
                   (let [cur-ns-name '%s
                          aliased-var '%s]
                      (if-let [cur-ns (find-ns cur-ns-name)]
                        (when-let [the-var (ns-resolve cur-ns aliased-var)]
                          (if (clojure.tools.trace/traced? the-var)
                            (str \"untracing: \" (clojure.tools.trace/untrace-var* the-var))
                            (str \"tracing: \" (clojure.tools.trace/trace-var* the-var))))
                        (print \"Namespace \" cur-ns-name \" not found. Is is loaded?\")))"
                  (cider-current-ns)
                  x)
          (cider-interactive-eval-handler buffer))))
     query)))

;;;###autoload
(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c M-t") 'cider-toggle-trace)))

(provide 'cider-tracing)
;;; cider-tracing.el ends here
