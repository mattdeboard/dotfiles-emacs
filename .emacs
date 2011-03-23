;;TODO:
;;1. Get scpaste and set up a pastebin
;;
(setq debug-on-error nil)
(setq dotfiles-dir "/home/matt/.emacs.d/")
(setq tab-width 4)
(visual-line-mode)
(column-number-mode)
(setq fx4-executable "/media/hd2/firefox/firefox")

;Path bindings
(add-to-list 'load-path "/home/matt/pianobar")
(add-to-list 'load-path (concat dotfiles-dir "auto-complete/"))
(add-to-list 'load-path (concat dotfiles-dir "elpa/"))
(add-to-list 'load-path (concat dotfiles-dir "misc/"))
(add-to-list 'load-path (concat dotfiles-dir "misc/tnt/"))
(add-to-list 'load-path (concat dotfiles-dir "magit/"))
(add-to-list 'load-path (concat dotfiles-dir "auto-complete/"))
(add-to-list 'load-path (concat dotfiles-dir "tnt-2.6"))
(add-to-list 'load-path (concat dotfiles-dir "nxhtml"))

;code style requires
(require 'auto-complete)
(require 'highlight-80+)
(require 'highlight-parentheses)


;nxhtml multi-major-mode
(load "autostart")
(setq mumamo-background-colors nil)
(add-to-list 'auto-mode-alist `("\\.html$" . html-mumamo-mode))
(add-to-list 'auto-mode-alist `("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist `("\\.js$" . js2-mode))

;Get rid of the annoying splash and startup screens
(setq inhibit-splash-screen t
      inhibit-startup-message t)

;browse-url
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program fx4-executable)

;hooks for programming modes
(add-hook 'coding-hook 'auto-complete-mode)
(add-hook 'coding-hook 'highlight-80+-mode)
(add-hook 'coding-hook 'highlight-parentheses-mode)

(defun run-coding-hook ()
  (run-hooks 'coding-hook))

(mapc (lambda (mode-hook)
	(add-hook mode-hook 'run-coding-hook))
      '(emacs-lisp-mode-hook
				js2-mode-hook
				python-mode-hook
				lisp-mode-hook
	slime-repl-mode-hook
	html-mumamo-mode-hook
	css-mode))

(add-hook 'python-mode-hook 'run-coding-hook
	  'python-mode-hook
	  (lambda ()
	    (setq ac-sources '(ac-source-python))))

;theme prefs
;; color-theme
; C-u C-x = to get font info at point
(require 'color-theme)
(setq color-theme-is-global t
      frame-background-mode 'dark)
(load-file (concat dotfiles-dir "themes/blackboard.el"))
;;(load-file (concat dotfiles-dir "themes/irblack.el"))
;;(load-file (concat dotfiles-dir "themes/naquadah.el"))
(color-theme-blackboard)
;;(color-theme-irblack)
;;(color-theme-naquadah)

;Load modules
(require 'js2-mode)
(require 'python)
(require 'magit)
(require 'auto-complete)

;Ido config
(require 'ido)
(ido-mode t)
(ido-everywhere t)

;package.el bindings
(require 'package)
(setq package-archive-base "http://marmalade-repo.org/packages/")

;; multi-term
; To get special control codes use: cat, xev, od -a
; Use `list-colors-display' to view all available colors
(require 'multi-term)
(setq term-default-bg-color "#0C1021"
      term-default-fg-color "white"
      ansi-term-color-vector [unspecified "black" "red2" "green2" "yellow2"
                                          "DodgerBlue2" "magenta2" "cyan2"
					  "white"])
;;Pianobar config
;;(let ((pianobar-password-file (expand-file-name "~/.pb-auth.el")))
;; (when (file-regular-p pianobar-password-file)
;;  (load pianobar-password-file)

(require 'pianobar)


;;python.el integration with auto-complete.el
(defvar ac-source-python '((candidates . 
				       (lambda()
					 mapcar '(lambda (completion)
						   (first (last (split-string completion "\\." t))))))))

;;load authinfo for all processes handled by rcirc
;;rcirc config
(let ((irc-password-file (expand-file-name "~/.rcirc-authinfo.el")))
  (when (file-regular-p irc-password-file)
    (load irc-password-file)
    ;require rcirc-notify
    (require 'rcirc)
    (require 'rcirc-notify)
    
;;rcirc default settings
    (setq rcirc-prompt "$> "
	  rcirc-default-nick "mdeboard"
	  rcirc-default-user-name "mdeboard"
	  rcirc-default-full-name "Matt"
	  rcirc-keywords '("matt" "indiana")
	  rcirc-authinfo `(("freenode" nickserv "mdeboard"
			    ,rcirc-freenode-password)
			   ("localhost" bitlbee "mdeboard" 
			    ,rcirc-bitlbee-password)
			   ("mozilla" nickserv "mdeboard"
			    ,rcirc-mozilla-password))
	  rcirc-server-alist '(("localhost")
			       ("irc.freenode.net"
				:channels ("#django" "#postfarm"))
			       ("irc.mozilla.org" 
				:channels ("#amo" "#metrics" "#labs")))))    
  (add-hook 'rcirc-mode-hook
	    '(lambda ()
	       (rcirc-track-minor-mode 1)))
  ;;/reconnect command to re-establish connection w/o opening a new buffer.
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
	    (port (process-contact process :service))
	    (nick (rcirc-nick process))
	    channels query-buffers)
       (dolist (buf (buffer-list))
	 (with-current-buffer buf
	   (when (eq process (rcirc-buffer-process))
	     (remove-hook 'change-major-mode-hook
			  'rcirc-change-major-mode-hook)
	     (if (rcirc-channel-p rcirc-target)
		 (setq channels (cons rcirc-target channels))
	       (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
		      rcirc-default-user-name
		      rcirc-default-full-name
		      channels)))

  (defun-rcirc-command clear (arg)
    "Clear rcirc buffer."
    (interactive "i")
    (let ((inhibit-read-only t))
      (delete-region
       (point-min) rcirc-prompt-start-marker))))  


(eval-after-load 'rcirc
  '(defun-rcirc-command ld (arg)
	 "Look of disapproval"
	 (interactive "i")
	 (rcirc-send-message process target "ಠ_ಠ")))


;; Other crap
;Duplicate a line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)

;Keybinds
(global-set-key (kbd "C-c C-e") 'duplicate-line)
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key "\C-xy" 'clipboard-yank)
(global-set-key "\d" 'delete-backward-char)
(global-set-key (kbd "C-$") 'multi-term)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-indent-on-enter-key t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
