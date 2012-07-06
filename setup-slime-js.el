(setq slime-js-target-url "http://localhost:4005")
(setq slime-js-connect-url "http://localhost:8009")
(setq slime-js-starting-url "/swank-js/test.html")
(setq slime-js-swank-command "swank-js")
(setq slime-js-swank-args '())
(setq slime-js-browser-command "google-chrome")
(setq slime-js-browser-jacked-in nil)
(setq slime-js-minor-mode-map (make-sparse-keymap))
;;(add-hook 'js2-mode-hook (lambda () (slime-js-minor-mode 1)))

;; Some functions from https://github.com/magnars/js2-refactor.el to make
;; this thing work.
(defun js2r--guard ()
  (when js2-parsed-errors
    (error "Can't refactor while buffer has parse errors.")))

(defun js2r--closest-node-where (p node)
  (if (or (null node)
          (apply p node nil))
      node
    (js2r--closest-node-where p (js2-node-parent node))))

(defun js2r--closest (p)
  (save-excursion
    (cond
     ((bolp) (back-to-indentation))
     ((looking-at ";") (forward-char -1))
     ((looking-back ";") (forward-char -2)))
    (js2r--closest-node-where p (js2-node-at-point))))

(defun js2r--goto-and-delete-node (node)
  (goto-char (js2-node-abs-pos node))
  (delete-char (js2-node-len node)))

(defun slime-js-run-swank ()
  "Runs the swank side of the equation."
  (interactive)
  (unless (boundp 'slime-js-swank-buffer)
    (setq slime-js-swank-buffer
          (apply #'make-comint "swank-js"  slime-js-swank-command nil slime-js-swank-args))))

(defun slime-js-jack-in-node ()
  "Start a swank-js server and connect to it, opening a repl."
  (interactive)
  (slime-js-run-swank)
  (sleep-for 1)
  (setq slime-protocol-version 'ignore)
  (slime-connect "localhost" 4005))

(defun slime-js-jack-in-browser ()
  "Start a swank-js server, connect to it, open a repl, open a browser, connect to that."
  (interactive)
  (slime-js-jack-in-node)
  (sleep-for 2)
  (slime-js-set-target-url slime-js-target-url)
  (shell-command (concat slime-js-browser-command " " slime-js-connect-url slime-js-starting-url))
  (sleep-for 3)
  (setq slime-remote-history nil)
  (slime-js-sticky-select-remote (caadr (slime-eval '(js:list-remotes))))
  (setq slime-js-browser-jacked-in t)
  (global-set-key [f5] 'slime-js-reload))

(defadvice save-buffer (after save-css-buffer activate)
  (when (and slime-js-browser-jacked-in (eq major-mode 'css-mode))
    (slime-js-refresh-css)))

(defun js2-is-eval-friendly-node (n)
  (or (and (js2-stmt-node-p n) (not (js2-block-node-p n)))
      (and (js2-function-node-p n) (js2-function-node-name n))))

(defun slime-js--echo-result (result &rest _)
  (message result))

(defun slime-js--replace-with-result (replacement beg end)
  (save-excursion
    (goto-char beg)
    (delete-char (- end beg))
    (insert replacement)))

(defun slime-js-eval-region (beg end &optional func)
  (lexical-let ((func (or func 'slime-js--echo-result))
                (beg beg)
                (end end))
    (slime-flash-region beg end)
    (slime-js-eval
     (buffer-substring-no-properties beg end)
     #'(lambda (s) (funcall func (cadr s) beg end)))))

(defun slime-js-eval-statement (&optional func)
  (let ((node (js2r--closest 'js2-is-eval-friendly-node)))
    (slime-js-eval-region (js2-node-abs-pos node)
                          (js2-node-abs-end node)
                          func)))

(defun slime-js-eval-current ()
  (interactive)
  (if (use-region-p)
      (slime-js-eval-region (point) (mark))
    (slime-js-eval-statement)))

(defun slime-js-eval-and-replace-current ()
  (interactive)
  (if (use-region-p)
      (slime-js-eval-region (point) (mark) 'slime-js--replace-with-result)
    (slime-js-eval-statement 'slime-js--replace-with-result)))

(define-key slime-js-minor-mode-map (kbd "C-x C-e") 'slime-js-eval-current)
(define-key slime-js-minor-mode-map (kbd "C-c C-e") 'slime-js-eval-and-replace-current)

(provide 'setup-slime-js)

