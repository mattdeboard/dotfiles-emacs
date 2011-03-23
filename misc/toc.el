; -*- indent-tabs-mode: nil -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TOC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Copyright (c) 1998 America Online, Inc. All Rights Reserved.
;;;;
;;;; AOL grants you ("Licensee") a non-exclusive, royalty free, license to
;;;; use, modify and redistribute this software in source and binary code
;;;; form, provided that i) this copyright notice and license appear on all
;;;; copies of the software; and ii) Licensee does not utilize the software
;;;; in a manner which is disparaging to AOL.
;;;;
;;;; This software is provided "AS IS," without a warranty of any kind. ALL
;;;; EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING
;;;; ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE
;;;; OR NON-INFRINGEMENT, ARE HEREBY EXCLUDED. AOL AND ITS LICENSORS SHALL NOT
;;;; BE LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING,
;;;; MODIFYING OR DISTRIBUTING THE SOFTWARE OR ITS DERIVATIVES. IN NO EVENT
;;;; WILL AOL OR ITS LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DATA,
;;;; OR FOR DIRECT, INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE
;;;; DAMAGES, HOWEVER CAUSED AND REGARDLESS OF THE THEORY OF LIABILITY, ARISING
;;;; OUT OF THE USE OF OR INABILITY TO USE SOFTWARE, EVEN IF AOL HAS BEEN
;;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
;;;;
;;;; This software is not designed or intended for use in on-line control of
;;;; aircraft, air traffic, aircraft navigation or aircraft communications;
;;;; or in the design, construction, operation or maintenance of any nuclear
;;;; facility. Licensee represents and warrants that it will not use or
;;;; redistribute the Software for such purposes.

;;;; TODO:
;;;;   turn callbacks into hooks
;;;;   signon time overflows 24 bits


(provide 'toc)
(require 'tocstr)


;; Public hooks.  Multiple "subscribers" may use `add-hook' to get called
;; when the corrosponding protocol event occurs.  Until better documented,
;; see toc-handle-receive to learn what the signature of the hook is.

(defvar toc-opened-hooks nil)
(defvar toc-closed-hooks nil)
(defvar toc-sign-on-hooks nil)
(defvar toc-config-hooks nil)
(defvar toc-nick-hooks nil)
(defvar toc-im-in-hooks nil)
(defvar toc-update-buddy-hooks nil)
(defvar toc-error-hooks nil)
(defvar toc-eviled-hooks nil)
(defvar toc-chat-join-hooks nil)
(defvar toc-chat-in-hooks nil)
(defvar toc-chat-update-buddy-hooks nil)
(defvar toc-chat-invite-hooks nil)
(defvar toc-chat-left-hooks nil)
(defvar toc-goto-url-hooks nil)
(defvar toc-pause-hooks nil)
(defvar toc-client-event-hooks nil)
(defvar toc-buddy-caps2-hooks nil)
(defvar toc-bart2-hooks nil)


;;; Private State
(defvar toc-permit-mode nil
  "If non-nil, the toc server believes we are in permit mode.")

(defvar toc-permit-list nil
  "The permit/deny list that the server is maintaining for us.
Depending on toc-permit-mode, it is a permit or deny list.")


;;;----------------------------------------------------------------------------
;;; Public functions
;;;----------------------------------------------------------------------------
(defun toc-open (host port sname)
  (setq tocstr-opened-function  'toc-handle-opened
        tocstr-closed-function  'toc-handle-closed
        tocstr-receive-function 'toc-handle-receive
        toc-permit-mode nil
        toc-permit-list nil)
  (tocstr-open host port sname))

(defun toc-close ()
  (tocstr-close))



;; toc2_signon <address> <port> <screenname> <roasted pw> <language> <version*> <??**> <code***>
;;
;; * The version string MUST start with "TIC:" otherwise, no dice.  For
;;   example, "TIC:AIMM" is ok, but "AIMM2" would be rejected.
;;
;; ** I have no idea what this is.  By default it's 160, but you can
;;    change it to other numbers and it still connects.
;;
;; *** This is a simple code created with the first letter of the screen
;;     name and password. (see comments in toc-generate-signon-code)
(defun toc-signon (host port username password language version)
  (tocstr-send (format "toc2_login %s %d %s %s %s %s %d %s %d"
                       host
                       port
                       (toc-normalize username)
                       (toc-roast password)
                       language
                       (toc-encode (concat "TIC:" version))
                       160
                       "US \"\" \"\" 3 0 21030 -kentucky -utf8"  ;; (?!?) lifted from miranda code
                       (toc-generate-signon-code (toc-normalize username) password))))


(defun toc-init-done ()
  (tocstr-send "toc_init_done"))

(defun toc-send-im (user message &optional auto)
  ;; Apparently there is a toc2_send_im but it's the same.
  (tocstr-send (format "toc_send_im %s %s%s"
                       (toc-normalize user)
                       (toc-encode message)
                       (if auto " auto" ""))))

(defun toc-send-typing-status (user &optional status)
  (tocstr-send (format "toc2_client_event %s %s"
                       (toc-normalize user)
                       (if status status "2"))))

(defun toc-add-buddies (blist)
  (if blist
    (let ((command "toc2_new_buddies {"))
      (while blist
        (let* ((group      (car blist))
               (group-name (car group))
               (buddies    (cdr group)))

          (setq command (concat command "g:" group-name "\n"))

          (while buddies
            (setq command (concat command "b:" (car buddies) "\n"))
            (setq buddies (cdr buddies)))

          (setq blist (cdr blist))))
      (setq command (concat command "}"))
      (tocstr-send command))))


(defun toc-remove-buddies (group buddies)
  "Remove one or more buddies from a group.
 BUDDIES is a list of strings of buddy names that are in GROUP."
  (if buddies
      (tocstr-send (format "toc2_remove_buddy %s \"%s\""
                           (substring (format "%S" buddies) 1 -1)
                           group))))

(defun toc-remove-group (group)
  "Remove GROUP.  GROUP must contain no buddies, or TOC2
won't do anything."
  (tocstr-send (format "toc2_del_group \"%s\"" group)))

(defun toc-set-config (config)
  (tocstr-send (format "toc_set_config %s" (toc-encode config))))

(defun toc-evil (user anon)
  "Warn USER.  Do so anonymously if ANON"
  (tocstr-send
   (format "toc_evil %s %s" (toc-normalize user) (if anon "anon" "norm"))))

(defun toc-add-permit (&optional users)
  (if toc-permit-mode
      (setq toc-permit-list (append toc-permit-list users))
    (setq toc-permit-mode nil
          toc-permit-list users))
  (tocstr-send (mapconcat 'identity (cons "toc_add_permit" users) " ")))

(defun toc-add-deny (&optional users)
  (if (not toc-permit-mode)
      (setq toc-permit-list (append toc-permit-list users))
    (setq toc-permit-mode t
          toc-permit-list users))
  (tocstr-send (mapconcat 'identity (cons "toc_add_deny" users) " ")))

(defun toc-permit-all ()
  (if (or toc-permit-mode toc-permit-list)
      (progn
        (toc-add-permit)
        (toc-add-deny nil))))

(defun toc-deny-all ()
  (if (or (not toc-permit-mode) toc-permit-list)
      (progn
        (toc-add-deny)
        (toc-add-permit nil))))

;;; The next two could be optimized a bit.  If we happen to be in the right
;;; mode already, and BUDDIES is a superset of the current list, we should
;;; just add them.  This method means people on our permit list might see
;;; us "offline" for an instant just because we add someone to our permit
;;; list.  (or worse if in deny mode, someone could see us online for an
;;; instant).  Of course, clever users of the toc interface could be
;;; calling toc-add-permit/deny instead.

(defun toc-permit-only (buddies)
  (if (and toc-permit-mode (equal buddies toc-permit-list))
      ()
    (toc-add-deny)                      ; Ensure deny mode
    (toc-add-permit buddies)))          ; Permit only who we want

(defun toc-deny-only (buddies)
  (if (and (not toc-permit-mode) (equal buddies toc-permit-list))
      ()
  (toc-add-permit)                      ; Ensure permit mode
  (toc-add-deny buddies)))              ; Deny only who we want to exclude

(defun toc-chat-join (room)
  (tocstr-send (format "toc_chat_join 4 %s" (toc-encode room))))

(defun toc-chat-send (roomid message)
  (tocstr-send (format "toc_chat_send %s %s" roomid (toc-encode message))))

(defun toc-chat-whisper (roomid user message)
  (tocstr-send
   (format "toc_chat_whisper %s %s %s" roomid user (toc-encode message))))

(defun toc-chat-evil (roomid user anon)
  "Warn USER in ROOMID.  Do so anonymously if ANON."
  (tocstr-send (format "toc_chat_evil %s %s %s"
                       roomid (toc-normalize user) (if anon "anon" "norm"))))

(defun toc-chat-invite (roomid message buddies)
  (tocstr-send (format "toc_chat_invite %s %s %s" roomid (toc-encode message)
                       (mapconcat 'toc-normalize buddies " "))))

(defun toc-chat-leave (roomid)
  (tocstr-send (format "toc_chat_leave %s" roomid)))

(defun toc-chat-accept (roomid)
  (tocstr-send (format "toc_chat_accept %s" roomid)))

(defun toc-get-info (user)
  (tocstr-send (format "toc_get_info %s" (toc-normalize user))))

(defun toc-set-info (info)
  (tocstr-send (format "toc_set_info %s" (toc-encode info))))

(defun toc-set-idle (secs)
  (tocstr-send (format "toc_set_idle %d" secs)))


;; The following two are not well documented in PROTOCOL.
(defun toc-set-away (message)
  (if message
      (tocstr-send (concat "toc_set_away " (toc-encode message)))
    (tocstr-send "toc_set_away")))

(defun toc-keepalive ()
  "Send a keepalive packet to the server."
  (tocstr-send-flap 5 ""))


;;;----------------------------------------------------------------------------
;;; Handlers for tocstr events
;;;----------------------------------------------------------------------------

(defun toc-handle-opened ()
  (toc-run-hooks toc-opened-hooks))


(defun toc-handle-closed ()
  (toc-run-hooks toc-closed-hooks))


(defun toc-handle-receive (str)
  ;;(save-excursion
  ;;  (set-buffer (get-buffer-create "*gse-debug*"))
  ;;  (goto-char (point-max))
  ;;  (insert str)
  ;;  (insert "\n-------------------------\n"))

  (let* ((index 0)
         (cmd (toc-lop-field str 'index)))
    (cond
     ((string= cmd "SIGN_ON")
      (let ((version (toc-lop-field str 'index)))
        (toc-run-hooks toc-sign-on-hooks version)))

     ;;((string= cmd "CONFIG")
     ;; (let ((config (toc-lop-field str 'index)))
     ;;   (toc-run-hooks toc-config-hooks config)))

     ;; 2005.08.21 gse: added for TOC2
     ((string= cmd "CONFIG2")
      ;;(let ((config (toc-lop-field str 'index)))
      (let ((config str))
        (toc-run-hooks toc-config-hooks config)))

     ((string= cmd "NICK")
      (let ((nick (toc-lop-field str 'index)))
        (toc-run-hooks toc-nick-hooks nick)))

     ((string= cmd "IM_IN2")
      (let ((user    (toc-lop-field str 'index))
            (auto    (string= "T" (toc-lop-field str 'index)))
            (unknown (toc-lop-field str 'index))
            (message (substring str index)))
        (toc-run-hooks toc-im-in-hooks user auto message)))


     ;; 2005.08.22 gse: No idea what the difference is between IM_IN2
     ;; and IM_IN_ENC2 -- there are a bunch of extra fields.
     ((string= cmd "IM_IN_ENC2")
      (let ((user     (toc-lop-field str 'index))
            (auto     (string= "T" (toc-lop-field str 'index)))
            (unknown1 (toc-lop-field str 'index))
            (unknown2 (toc-lop-field str 'index))
            (unknown3 (toc-lop-field str 'index))
            (unknown4 (toc-lop-field str 'index))
            (unknown5 (toc-lop-field str 'index))
            (unknown6 (toc-lop-field str 'index)) ;; language?
            (message  (substring str index)))
        (toc-run-hooks toc-im-in-hooks user auto message)))

     ((string= cmd "UPDATE_BUDDY2")
      (let ((nick    (toc-lop-field str 'index))
            (online  (string= "T" (toc-lop-field str 'index)))
            (evil    (string-to-number (toc-lop-field str 'index)))
            (signon  (string-to-number (toc-lop-field str 'index)))
            (idle    (string-to-number (toc-lop-field str 'index)))
            (away    (toc-lop-field str 'index))
            (unknown (toc-lop-field str 'index)))
        (toc-run-hooks toc-update-buddy-hooks
                       nick online evil signon idle away)))

     ((string= cmd "ERROR")
      (let ((code (string-to-number (toc-lop-field str 'index)))
            (args nil)
            (arg  nil))
        (while (setq arg (toc-lop-field str 'index))
          (setq args (cons arg args)))
        (toc-run-hooks toc-error-hooks code (nreverse args))))

     ((string= cmd "EVILED")
      (let ((evil   (string-to-number (toc-lop-field str 'index)))
            (eviler (toc-lop-field str 'index)))
        (toc-run-hooks toc-eviled-hooks evil eviler)))

     ((string= cmd "CHAT_JOIN")
      (let ((roomid (toc-lop-field str 'index))
            (room   (toc-lop-field str 'index)))
        (toc-run-hooks toc-chat-join-hooks roomid room)))

     ((string= cmd "CHAT_IN")
      (let ((roomid  (toc-lop-field str 'index))
            (user    (toc-lop-field str 'index))
            (whisper (string= "T" (toc-lop-field str 'index)))
            (message (substring str index)))
        (toc-run-hooks toc-chat-in-hooks roomid user whisper message)))

     ((string= cmd "CHAT_IN_ENC")
      (let ((roomid   (toc-lop-field str 'index))
            (user     (toc-lop-field str 'index))
            (whisper  (string= "T" (toc-lop-field str 'index)))
            (unknown1 (toc-lop-field str 'index))
            (unknown2 (toc-lop-field str 'index))
            (message  (substring str index)))
        (toc-run-hooks toc-chat-in-hooks roomid user whisper message)))

     ((string= cmd "CHAT_UPDATE_BUDDY")
      (let ((roomid (toc-lop-field str 'index))
            (inside (string= "T" (toc-lop-field str 'index)))
            (users  (let (user (users nil))
                      (while (setq user (toc-lop-field str 'index))
                        (setq users (cons user users)))
                      users)))
        (toc-run-hooks toc-chat-update-buddy-hooks roomid inside users)))

     ((string= cmd "CHAT_INVITE")
      (let ((room    (toc-lop-field str 'index))
            (roomid  (toc-lop-field str 'index))
            (sender  (toc-lop-field str 'index))
            (message (substring str index)))
        (toc-run-hooks toc-chat-invite-hooks room roomid sender message)))

     ((string= cmd "CHAT_LEFT")
      (let ((roomid (toc-lop-field str 'index)))
        (toc-run-hooks toc-chat-left-hooks cmd roomid)))

     ((string= cmd "GOTO_URL")
      (let ((windowid (toc-lop-field str 'index))
            (url      (substring str index))) ; Url might have a colon?
        (toc-run-hooks toc-goto-url-hooks windowid url)))

     ;; We probably ought to handle this internally.  Does it ever really
     ;; get sent?
     ((string= cmd "PAUSE")
      (toc-run-hooks toc-pause-hooks cmd))

     ;; known event codes:
     ;;   2 = buddy is typing
     ((string= cmd "CLIENT_EVENT2")
      (let ((user  (toc-lop-field str 'index))
            (event (toc-lop-field str 'index)))
        (toc-run-hooks toc-client-event-hooks user event)))

     ;; don't know what this event is for
     ((string= cmd "BUDDY_CAPS2")
      (let ((user      (toc-lop-field str 'index))
            (remainder str))
        (toc-run-hooks toc-buddy-caps2-hooks user remainder)))

     ;; don't know what this event is for
     ((string= cmd "BART2")
      (let ((user      (toc-lop-field str 'index))
            (remainder str))
        (toc-run-hooks toc-bart2-hooks user remainder)))

     (t
      (message (concat "Recieved unknown command: " cmd " <" str ">")))
     )))


(defun toc-run-hooks (hooks &rest args)
  "Run each function in HOOKS with ARGS."
  (if (symbolp hooks)
      (if hooks (apply hooks args))
    (mapcar '(lambda (f) (apply f args)) hooks)))


;;;----------------------------------------------------------------------------
;;; String utilities
;;;----------------------------------------------------------------------------

(defun toc-lop-field (str index-var)
  ;; Returns the substring of STR that starts at the index given by the
  ;; value of INDEX-VAR and ends at the next colon.  Updates INDEX-VAR
  ;; to index of the character after colon.
  (let ((start-index (eval index-var)))
    (if (< start-index (length str))
        (let ((colon-index (or (string-match ":" str start-index)
                               (length str))))
          (set index-var (1+ colon-index))
          (substring str start-index colon-index)))))


(defun toc-roast (str)
  ;; Obfuscates password STR for network transmission.
  (let* ((roaster "Tic/Toc")
         (rstr "0x")
         (slen (length str))
         (rlen (length roaster))
         (i 0))
    (while (< i slen)
      (setq rstr (concat rstr
                         (format "%02x" (logxor (aref str i)
                                                (aref roaster (% i rlen))))))
      (setq i (1+ i)))
    rstr))

(defun toc-get-ascii-value (char)
  (if (fboundp 'char-to-int)
      (char-to-int char)
    (identity char)))

(defun toc-generate-signon-code (user password)
  ;; Wow, this is silly:
  ;;   sn = ascii value of the first letter of the screen name - 96
  ;;   pw = ascii value of the first character of the password - 96
  ;;
  ;;   a = sn * 7696 + 738816
  ;;   b = sn * 746512
  ;;   c = pw * a
  ;;
  ;;   return c - a + b + 71665152
  (let* ((first-user-char (toc-get-ascii-value (string-to-char user)))
         (first-pw-char   (toc-get-ascii-value (string-to-char password)))
         (sn (- first-user-char 96))
         (pw (- first-pw-char 96))
         (a (+ (* sn 7696) 738816))
         (b (* sn 746512))
         (c (* pw a)))

         (+ c (- a) b 71665152)))


(defun toc-encode (str)
  ;; Encloses STR in quotes and backslashes special characters in it.
  (let ((list nil)
        (pos 0))
    (while (string-match "\\([^][{}()\\'\"$]*\\)\\([][{}()\\'\"$]\\)" str pos)
      (setq list (cons (substring str (match-beginning 1) (match-end 1)) list))
      (setq list (cons "\\" list))
      (setq list (cons (substring str (match-beginning 2) (match-end 2)) list))
      (setq pos (match-end 0)))
    (if (< pos (length str))
        (setq list (cons (substring str pos) list)))
    (apply 'concat "\"" (nreverse (cons "\"" list)))))

(defun toc-normalize (str)
  "Removes spaces and smashes STR to lowercase."
  (mapconcat '(lambda (char)
                (if (not (equal char ? ))
                    (char-to-string (downcase char))))
             str ""))
