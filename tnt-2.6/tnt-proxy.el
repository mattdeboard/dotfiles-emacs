;;; @(#) tnt-proxy.el -- proxy support for TNT
;;; @(#) $Id: tnt-proxy.el,v 1.1 2006/02/12 18:51:55 northboundtrain Exp $

;; This file is not part of Emacs

;; Copyright (C) 2006 by Joseph L. Casadonte Jr.
;;
;; The contents of this file are covered under the Artistic License, a
;; copy of which should have accompanied this distribution.  In the
;; event that it did not, you can find a copy here:
;;
;;     http://www.perl.com/pub/a/language/misc/Artistic.html

;; Currently only unauthenticated HTTP proxy support is implemented.
;; If you need support for other proxy types, please feel free to
;; contribute to the TNT project or contact the developers, both of
;; which can be done at:
;;
;;     http://tnt.sourceforge.net
;;
;; Autoloads you may want to consider:
;;
;;     (autoload 'tnt-proxy-switch-servers "tnt" "Switch TNT proxy servers." t)
;;     (autoload 'tnt-proxy-toggle-proxy-use "tnt" "Toggle use of TNT proxy server." t)

;; The base skeleton for the proxy process approach was taken from
;; W3's socks.el, although it did not itself have support for HTTP
;; proxies.

;; ***************************************************************************
;; references (in no particular order)
;; ***************************************************************************
;; http://www.unicom.com/sw/pxytest/ - tests for open proxies (perl)
;; http://zippo.taiyo.co.jp/~gotoh/ssh/connect.html - socks 4/4a/5 & https
;; http://www.faqs.org/rfcs/rfc2068.html - old http/1.1
;; http://www.faqs.org/rfcs/rfc2616.html - http/1.1
;; http://www.faqs.org/rfcs/rfc2617.html - http authentication
;; http://tinyproxy.sourceforge.net/ - complete http proxy
;; http://uppertank.net/blog/ -- setting up a proxy server (kernel setting)
;; ***************************************************************************

(require 'custom)

;;; ***************************************************************************
;;; Customization support
;;; ***************************************************************************
(defgroup tnt-proxy nil
  "TNT proxy options"
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-proxy-use-proxy nil
  "Determines whether or not to use the proxy server.

Non-nil means to use the proxy server.  Proxy servers are defined in
`tnt-proxy-server-alist'."
  :type 'boolean
  :group 'tnt-proxy)

;; ---------------------------------------------------------------------------
(defcustom tnt-proxy-default-server ""
  "The default proxy server to use."
  :type 'string
  :group 'tnt-proxy)

;; ---------------------------------------------------------------------------
(defcustom tnt-proxy-server-alist nil
  "Proxy server definitions for TNT."
  :type '(repeat
		  (cons :tag "Proxy Server Definition"
				(string :tag "Name")
				(list :tag "Definition"
					  (string :tag "-Server")
					  (integer :tag "-Port" :value 8080)
					  (choice :tag "-Proxy type (* = not yet supported)" :value http
							  (const :tag "HTTP  " http)
							  (const :tag "HTTPS (*)  " https)
							  (const :tag "SOCKS v4 (*) " socks4)
							  (const :tag "SOCKS v5 (*)"   socks5)
							  )
					  (string :tag "-Username")
					  (string :tag "-Password (optional)")
					  )))
  :group 'tnt-proxy)

;; ---------------------------------------------------------------------------
(defcustom tnt-proxy-load-hook nil
  "Hook run when TNT-PROXY is loaded."
  :type 'hook
  :group 'tnt-proxy)

;;; ***************************************************************************
;;; Switching servers
;;; ***************************************************************************
(defvar tnt-proxy-current-server nil
  "Holds the current proxy server.

If nil, the value of `tnt-proxy-default-server' will be used.")

;;; ***************************************************************************
(defun tnt-proxy-switch-servers ()
  "Switch to the next proxy server."
  (interactive)
  (if (null tnt-proxy-server-alist)
      (error "No proxy server list defined")
	(setq tnt-proxy-server-alist (tnt-rotate-left tnt-proxy-server-alist))
	(setq tnt-proxy-current-server (caar tnt-proxy-server-alist))
	(message "Proxy server to be used on the next login: %s" tnt-proxy-current-server)
	))

;;; ***************************************************************************
(defun tnt-proxy-toggle-proxy-use ()
  "Toggle use of proxy server on and off."
  (interactive)
  (setq tnt-proxy-use-proxy (not tnt-proxy-use-proxy))
  (message "Proxy server will%s be used next login."
		   (if tnt-proxy-use-proxy "" " not")))

;;; ***************************************************************************
;;; Main functions
;;; ***************************************************************************
(defvar tnt-proxy-proc-name "tnt-proxy"
  "Name of the process for the TNT Proxy.")

;;; ***************************************************************************
(defun tnt-proxy-open-network-stream (name buffer host service)
  "Open connection NAME in BUFFER to HOST on port SERVICE.

Assume: proxy info is defined, service is a number"
  (let* ((server-def (assoc (or tnt-proxy-current-server
								tnt-proxy-default-server) tnt-proxy-server-alist))
		 (proxy-server (nth 1 server-def))
		 (proxy-port   (nth 2 server-def))
		 (proxy-type   (nth 3 server-def))
		 (proxy-user   (nth 4 server-def))
		 (proxy-pwd    (nth 5 server-def))
		 proxy-proc info version atype)
	;; only support HTTP for the moment
	(unless (eq proxy-type 'http)
	  (error (concat "[TNT-Proxy] Unknown proxy type: " (symbol-name proxy-type))))

	;; doesn't do authentication (yet)
	(when (and proxy-user (> (length proxy-user) 0))
	  (error (concat "[TNT-Proxy] Authentication not yet supported")))

	;; open a network connection to the proxy server itself
	(setq proxy-proc
		  (if (eq proxy-type 'https)
			  (open-ssl-stream tnt-proxy-proc-name
							   nil proxy-server proxy-port)
			(open-network-stream tnt-proxy-proc-name
									nil proxy-server proxy-port)))

	;; initialize process and info about the process
	(set-process-filter proxy-proc 'tnt-proxy-filter)
	(process-kill-without-query proxy-proc)

	;; tell it where to go
	(tnt-proxy-establish-proxy proxy-proc proxy-type host service)

	;; wait for filter to process return before proceeding
	(let ((cnt 0)
		  (num-iterations (if tnt-running-xemacs 5 50)))
	  (while (process-filter proxy-proc)
		(setq cnt (1+ cnt))
		(when (> cnt num-iterations)
		  (set-process-filter proxy-proc nil)
		  (delete-process proxy-proc))
		(if tnt-running-xemacs
			(sleep-for 1)
		  (sleep-for 0 100))))
	proxy-proc))

;;; ***************************************************************************
(defun tnt-proxy-establish-proxy (proc type host service)
  "Connect to proxy via process PROC and establish forward connection.

Connection syntax is based on the TYPE of proxy, for HOST:SERVICE.
Currently only HTTP proxies are implemented."
  (let (request)
	(if (eq type 'http)
		(setq request (format (eval-when-compile
								(concat
								 "CONNECT %s:%d HTTP/1.1\r\n"
								 "Host: %s:%d\r\n"
								 "User-Agent: TNT\r\n"
								 "\r\n"))
							  host service
							  host service))
	  (error "[TNT-Proxy] Unknown protocol version: %s" (symbol-name type)))

    (process-send-string proc request)
    ))

;;; ***************************************************************************
(defun tnt-proxy-filter (proc string)
  "Check process PROC for successful return code in STRING."
;;  (message (concat "[TNT-Proxy DEBUG] " string))
  (save-excursion
	(when (string-match "^HTTP" string)
		(unless (string-match "200 Connection Established" string)
		  (error "[TNT-Proxy] Unexpected response from proxy server : <%s>" string))
		(set-process-filter proc nil))))

;;; **************************************************************************
;;; ***** we're done
;;; **************************************************************************
(provide 'tnt-proxy)
(run-hooks 'tnt-proxy-load-hook)

;;; tnt-proxy.el ends here
;;; **************************************************************************
;;;; *****  EOF  *****  EOF  *****  EOF  *****  EOF  *****  EOF  *************
