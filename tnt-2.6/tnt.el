;;; -*- indent-tabs-mode: nil -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TNT
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
;;;;
;;;; ----------------------------------------------------------------------
;;;; INSTALLATION
;;;;   There is a separate "INSTALL" document.  But here's the short version:
;;;;     (setq load-path (cons "/full/path/to/tnt" load-path))
;;;;     (load "tnt")
;;;;
;;;; LATEST VERSION
;;;;   The TNT project is hosted at SourceForge:
;;;;   http://tnt.sourceforge.net/
;;;; ----------------------------------------------------------------------
;;;;
;;;; $Id: tnt.el,v 1.190 2006/03/29 05:13:24 gse Exp $

(require 'toc)
(require 'tnt-proxy)

;; only using cl functions in one place...  maybe we should just
;; implement those functions locally?
(require 'cl)


(defconst tnt-version "TNT 2.6")

;;; **************************************************************************
;;; ***** Configuration variables / Compatability
;;; **************************************************************************
  ; check whether this version of emacs has the "run-at-time" function
(defconst tnt-timers-available (fboundp 'run-at-time))

;;; --------------------------------------------------------------------------
(defconst tnt-running-xemacs
  (save-match-data (string-match "XEmacs" (emacs-version)))
  "Non-nil if we are running in XEmacs.")

(when (and tnt-running-xemacs tnt-timers-available)
  (unless (fboundp 'cancel-timer)
    (fset 'cancel-timer 'delete-itimer)))

;; ---------------------------------------------------------------------------
(unless (fboundp 'propertize)
  ;; built-in to GNU Emacs 21, soon to be included into XEmacs
  (defun propertize (string &rest properties)
    "Return a copy of STRING with text properties added.
First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result.

\[Taken from XEmacs source 21.5.9]"
    (let ((str (copy-sequence string)))
      (add-text-properties 0 (length str)
                           properties
                           str)
      str))
  )

;;; ***************************************************************************
;;; ***** global variables
;;; ***************************************************************************
(defvar tnt-current-user nil)
(defvar tnt-pipe-to-email-now nil)

(defvar tnt-buddy-alist nil)
(defvar tnt-buddy-blist nil)

(defvar tnt-permit-mode 1)
(defvar tnt-permit-list nil)
(defvar tnt-deny-list nil)

(defvar tnt-event-ring nil)     ; (buffer-name . (message . callback))
(defvar tnt-show-inactive-buddies-now nil)

(defvar tnt-archive-datestamp-alist nil)

;;; **************************************************************************
;;; ***** Custom support - james@ja.ath.cx
;;; *****  with additions by - Joe Casadonte (emacs@northbound-train.com)
;;; **************************************************************************
(require 'custom)

;; ---------------------------------------------------------------------------
(defgroup tnt nil
  "The TNT AIM Client"
  :group 'comm)

;; ---------------------------------------------------------------------------
;; ----- customization routines
;; ---------------------------------------------------------------------------
;;;###autoload
(defun tnt-customize ()
  "Customization of the group 'tnt'."
  (interactive)
  (customize-group 'tnt))

;; ---------------------------------------------------------------------------
;; ----- basic, essential configuration
;; ---------------------------------------------------------------------------
(defcustom tnt-default-username nil
  "Should be nil or a string containing your username.

If you set this, you will not have to type in your username every
time you want to log in."
  :type '(choice :tag "Default User options"
                 (string :tag "Username")
                 (const :tag "No Default Username" nil))
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-default-password nil
  "Should be nil or your password.

If you set this, you will not have to type in your password every time
you want to log in.  However, if the file you set this in is readable
by other users on your system, they could log in as you, so if you set
this, be careful.  Also note that if you use multiple usernames, you
should not set the tnt-default-password (unless all your usernames use
the same password) -- see `tnt-username-alist' instead."
  :type '(choice :tag "Default Password options"
                 (string :tag "Password")
                 (const :tag "No Default Password" nil))
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-default-info-message
  "<a href=\"http://tnt.sourceforge.net/\">TNT</a>---AIM for grownups</a>"
  "Default message to put in your profile."
  :type '(choice
          (string :tag "Profile message" "<a href=\"http://tnt.sourceforge.net/\">TNT</a>---AIM for grownups</a>")
          (const :tag "None (will be prompted each time)" nil))
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-username-alist nil
  "Should be nil or a list of usernames and (optionally) passwords.

If you have more than one username, set the list this way:
  (setq tnt-username-alist '((\"UserName1\" . \"Password1\")
                             (\"UserName2\" . \"Password2\")
                             (\"UserName3\")))

Then you can use \"C-x t s\" to change which username will be used the
next time you connect.  Any number of usernames can be listed in this
way, but note that each element of the list MUST be either of the form
\(\"UserName\") or of the form (\"UserName\" . \"Password\"), and note the
apostrophe.  When you log in as a username for which the password is
not stored here, you will be prompted."
  :type '(repeat
          (cons :tag "ID/Password pairs"
                (string :tag "AIM Buddy Name")
                (choice :tag "Password options"
                        (string :tag "Password")
                        (const :tag "No password" nil))
                ))

  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-default-away-message nil
  "Default message to use when away."
  :type '(choice
          (string :tag "Away message" "I'm away.")
          (const :tag "None (will be prompted each time)" nil))
  :group 'tnt)

;; ---------------------------------------------------------------------------
;; ----- mode line
;; ---------------------------------------------------------------------------
(defun tnt-customize-mode-line-setting (symbol newval)
  (set-default symbol newval)

  (when tnt-current-user
    (tnt-set-mode-string t)))

;; ...........................................................................
(defcustom tnt-mode-indicator 'nick
  "Indicator used to show you're online.

nick - you're nickname
string - some arbitrary string
none - no indicator"
  :type '(choice :tag "Mode indicator options"
                 (const :tag "Nickname" nick)
                 (string :tag "Arbitrary string" "TNT")
                 (const :tag "None (no indicator)" nil))
  :set 'tnt-customize-mode-line-setting
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-show-events-in-mode nil
  "If non-nil, pre-pend '*' to mode indicator when events are pending."
  :type 'boolean
  :set 'tnt-customize-mode-line-setting
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-show-away-in-mode ":away"
  "If non-nil, append string to mode indicator when you're away."
  :type '(choice :tag "Away indicator options"
                 (const :tag "None (no indicator)" nil)
                 (string :tag "String indicator" ":away"))
  :set 'tnt-customize-mode-line-setting
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-show-idle-in-mode nil
  "If non-nil, append string to mode indicator when you're away."
  :type '(choice :tag "Idle indicator options"
                 (const :tag "None (no indicator)" nil)
                 (string :tag "String indicator" ":idle"))
  :set 'tnt-customize-mode-line-setting
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-show-email-in-mode nil
  "If non-nil, append string to mode indicator when you're away.

When piping to email, use ':email' instead (see `tnt-email-to-pipe-to')."
  :type '(choice :tag "Email indicator options"
                 (const :tag "None (no indicator)" nil)
                 (string :tag "String indicator" ":email"))
  :set 'tnt-customize-mode-line-setting
  :group 'tnt)

;; ---------------------------------------------------------------------------
;; ----- notifications
;; ---------------------------------------------------------------------------
(defcustom tnt-message-on-buddy-signonoff t
  "If non-nil, a minibuffer message appears when buddies sign on and off.

See also `tnt-beep-on-buddy-signon' and `tnt-beep-on-buddy-signoff'."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-message-on-chatroom-message t
  "If non-nil, a message appears when there are messages pending in a chatroom buffer.

See also `tnt-beep-on-chat-message'."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-send-typing-notifications tnt-timers-available
  "If non-nil, tells buddy that you are currently typing a message.

Some AIM clients can send and receive notifications that one buddy is
currently typing a message to the other.  This allows you to send
those messages.  This capability is only available if timers are
available.

See also `tnt-receive-typing-notifications' and
`tnt-typing-notications-idle-time'."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-receive-typing-notifications '(message)
  "If non-nil, lets buddy tell you that they are currently typing a message.

Some AIM clients can send and receive notifications that one buddy is
currently typing a message to the other.  This allows you to receive
those messages.  This capability is only available if timers are
available.

Inbound notification methods:

Message area - transient messages show up in the Message Area
Mode-line - statically shows up in the mode-line of IM buffers (only)

See also `tnt-send-typing-notifications'."
  :type '(radio :tag "Inbound methods"
                 (const :tag "No inbound notifications" nil)
				 (set :tag "Receive inbound notifications via:"
				  (const :tag "Message area" message)
				  (const :tag "Mode-line" mode-line)
				  ))
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-typing-notications-idle-time 3
  "Idle time when a 'no longer typing' message is sent.

Can be as short or long as you'd like, but AIM uses 3 seconds."
  :type 'integer
  :group 'tnt)

;; ---------------------------------------------------------------------------
;; ----- Buddy list/mode
;; ---------------------------------------------------------------------------
(defcustom tnt-buddy-list-buffer-name "*buddies*"
  "Name to use for Buddy list buffer."
  :type 'string
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-use-split-buddy nil
  "If non-nil, tnt will split the window when going to the buddy list.

Note that this rule does not apply when in in the *scratch* buffer, or
when already in the *buddies* buffer."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-show-inactive-buddies nil
  "If non-nil, show inactive Buddies in Buddy listing.

If nil, inactive Buddies will not appear in the Buddy listing at all."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-buddy-list-backup-filename "%s-buddies"
  "If non-nil, tnt will backup buddy list into this file.

Occasionally, the toc server loses people's buddy lists.  If a
\(non-nil) filename is given, tnt will backup your buddy list to a
file, and if the server loses it, tnt will restore from the backup.
If the given filename includes \"%s\", then your username will be
substituted.

Defaults to \"%s-buddies\"."
  :type 'string
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-buddy-fullname-alist nil
  "A mapping of Buddy names to full names.

If a Buddy's nickname appears in this list, then the real name for the
Buddy will appear in the Buddy list, with the nickname in square
brackets at the end of the line.  Example:

Buddies
  Mom and Dad [kww64nnh72]"
  :type '(repeat
          (list
           (string :tag "Buddy Name")
           (string :tag "Full Name")))
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-sort-buddies-by nil
  "If non-nil, sort buddy list.  Possible values are
'fullname and 'buddyname."
  :type '(choice
          (const :tag "No sorting" nil)
          (const :tag "Buddy name" buddyname)
          (const :tag "Fullname"   fullname))
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-group-away-buddies nil
  "Non-nil means \"away\" buddies are grouped together
at the end of the buddy list."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-group-idle-buddies nil
  "Non-nil means \"idle\" buddies are grouped together
at the end of the buddy list."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-group-offline-buddies nil
  "Non-nil means offline buddies are grouped together
at the end of the buddy list.  This setting is really
only relevant if tnt-show-inactive-buddies is set."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-very-idle-minimum 0
  "Minimum idle time, in seconds, after which a buddy will
be marked \"very idle\".  Very idle buddies are grouped with
away buddies if tnt-group-away-buddies is set.

If 0, nobody will go \"very idle\"."
  :type 'integer
  :group 'tnt)

;; ---------------------------------------------------------------------------
;; ----- IM/Chat formatting
;; ---------------------------------------------------------------------------
(defcustom tnt-separator "\n\n"
  "String printed between IMs.

This controls what is printed between message.  One newline is pretty
much required in order separate messages.  Two newlines is the default,
but other strings, suchs as \"\n---\n\" may be desirable."
  :type 'string
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-recenter-windows t
  "If non-nil, recenters text to bottom of window when messages are printed."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-default-chatroom 'tnt-default-chatroom-function
  "Expression used to generate the default chat room name.

When creating a chatromm (e.g. with C-x t j), use this to generate the
chatroom name.  This can be an expression such as the default somewhat
how one would bind a function to a key or it may be a string.  Note
that the function must return a string."
  :type '(choice :tag "Chatroom Name"
                 (string :tag "Name")
                 (function :tag "Function"))
  :group 'tnt)

;; ...........................................................................
(defun tnt-default-chatroom-function ()
  "Generates a chatroom name user the current user name and a random number."
  (format "%s Chat%03d" tnt-current-user (random 1000)))

;; ---------------------------------------------------------------------------
(defcustom tnt-use-timestamps nil
  "If non-nil, shows timestamps in TNT conversations.

This will add a timestamp for each message you receive or send.
This is useful for logging, and for people who are on very often.
It's also convenient if you leave yourself logged in while away
from your computer, so then when you come back, you can see how
long ago it was that your friend said \"hi\" while you were gone.

Defaults to nil."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-datestamp-format "%Y %b %d"
  "String used as datestamp format.

This string will be passed to `format-time-string' (which see) and the
result used when inserting a datestamp.

Example of default: 2002 Jul 09."
  :type 'string
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-timestamp-format "%T "
  "String used as timestamp format.

This string will be passed to `format-time-string' (which see) and the
result prepended to messages if `tnt-use-timestamps' is non-nil.  \"%r
\" gives a 12 hour format while the default of \"%T \" gives a 24 hour
format.

Example of default: 22:16:31"
  :type 'string
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-me-statement-format "* %s *"
  "Format variable to replace any messages starting with \"/me \"

This variable holds the format string containing exactly one %s to
be replaced with the message sans the \"/me \" which will replace
the message and be sent.  This will also be done on incoming messages.
This value may be nil to prevent any such action."
  :type 'string
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-use-flyspell-mode nil
  "Use flyspell mode when in IM or Chat buffer.

Works on outgoing messages only; inbound messages and old messages are
ignored."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
;; ----- systemic setup
;; ---------------------------------------------------------------------------
(defcustom tnt-directory "~/.tnt"
  "The directory tnt will use to store data.

Note that this should NOT be the same place that the elisp files, the
README, and so on are."
  :type 'string
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-email-to-pipe-to nil
  "Should be nil or a string containing an email address.

Setting this allows you to toggle forwarding of all incoming IMs to
the specified address.  This might be useful if you have an email
address which goes to an alphanumeric pager, and you want to be able
to receive IMs anywhere!  Once the email address is specified, turn
forwarding on and off with \"C-x t M\"."
  :type '(choice :tag "Email options"
                 (string :tag "Email Address")
                 (const :tag "Do Not Forward to Email address" nil))
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-email-from-domain nil
  "Used for two-way email forwarding.

In addition to incoming IMs going out over email, emails that come
back can then go out as IMs, so if they are forwarded to a two-way
pager or cell phone, you can reply to the email and the message body
will be sent as an IM.  Requires some procmail setup, and that you own
a personal domain.  See <tnt_dir>/procmail/README for config details."
  :type '(choice :tag "Email domain options"
                 (string :tag "Email domain")
                 (const :tag "No two-way email forwarding." nil))
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-email-use-subject t
  "Whether to include a subject header when forwarding IMs as email."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-email-include-user-in-body t
  "Whether to include the username in the body when forwarding IMs as email."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-email-binary "/bin/mail"
  "Should be set to the executable of your mail binary.

Note that you only need to set this if you're using the pipe-to-email
feature.  defaults to /bin/mail"
  :type '(choice :tag "email binary options"
                 (string :tag "Email Binary Path & Name")
                 (const :tag "Emacs internal via smtpmail.el" use-smtpmail))
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-persistent-timeout 5
  "Timeout between redisplays of persistent messages.

This number is the time between redisplays of messages created with
`tnt-persistent-message'.  It should not be too small as you'd never
see anything else in the minibuffer but it should be sufficiently
small to allow you to see the message now and then until you notice
it.  If set to nil or < 0, persistent messages are disabled."
  :type 'integer
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-im-buffers-read-only t
  "If non-nil, TNT buffers will be read-only.

Except for the current message being typed.  That is, once messages
have been sent, you can't change them."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-auto-reconnect t
  "If non-nil, TNT will attempt to reconnect if the connection drops."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-inhibit-key-bindings nil
  "If non-nil, do not set up default keybindings."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-kill-window-on-shutdown nil
  "If non-nil, kill the Buddy list window whenshutting down."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-supress-pounce-when-away nil
  "If non-nil, don't pounce if you're away."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-archive-conversations nil
  "If non-nil, tnt will archive all conversations.

Defaults to nil.  See `tnt-archive-directory-hierarchy' to
determine how the archives are organized."
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-archive-directory-hierarchy 'monthly
  "Determines the file organization of the archived conversations.

All archives will be kept under the `tnt-directory' (which see).  From
there, further organization is possible:

Daily - <TNT-DIR>/<year>/<month>/<day>/xxx
Monthly - <TNT-DIR>/<year>/<month>/xxx
Yearly - <TNT-DIR>/<year>/xxx
Single File - <TNT-DIR>/xxx

where 'xxx' is the actual name of the archive file (one per user).

Defaults to Monthly."
  :type '(choice
          (const :tag "Daily" daily)
          (const :tag "Monthly" monthly)
          (const :tag "Yearly" yearly)
          (const :tag "Single file" nil))
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-archive-max-single-file-size 0
  "The maximum file size (in bytes) of archive file.

Only applies when `tnt-archive-directory-hierarchy' is set to \"Single
file\".  A value of 0 means that there is no maximum and the file will
grow indefinitely"
  :type 'integer
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-include-datestamp-in-buffer-header nil
  ""
  :type 'boolean
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-im-mode-hook nil
  "Hook run when TNT IM mode is invoked."
  :type 'hook
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-chat-mode-hook nil
  "Hook run when TNT Chat mode is invoked."
  :type 'hook
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-buddy-list-mode-hook nil
  "Hook run when TNT Buddy List mode is invoked."
  :type 'hook
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-buddy-edit-mode-hook nil
  "Hook run when TNT Buddy Edit mode is invoked."
  :type 'hook
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-load-hook nil
  "Hook run when TNT is loaded."
  :type 'hook
  :group 'tnt)

;; ---------------------------------------------------------------------------
;; ----- faces group
;; ---------------------------------------------------------------------------
(defgroup tnt-faces nil
  "TNT font options"
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defface tnt-my-name-face '((((class color)) (:foreground "red"))
                            (t (:bold t)))
  "The face used for my name on messages sent by this user"
  :group 'tnt-faces)

;; ---------------------------------------------------------------------------
(defface tnt-other-name-face '((((class color)) (:foreground "blue"))
                               (t (:bold t)))
  "The face used for my name on messages sent by another user"
  :group 'tnt-faces)

;; ---------------------------------------------------------------------------
(defface tnt-buddy-list-group-face
  '((((class color) (background light)) (:foreground "Black" :bold t))
    (((class color) (background dark)) (:foreground "White" :bold t))
    (t (:bold t)))
  "Face used for displaying Buddy group names."
  :group 'tnt-faces)

;; ---------------------------------------------------------------------------
(defface tnt-buddy-list-active-face
  '((((class color) (background light)) (:foreground "Forest Green" :bold t))
    (((class color) (background dark)) (:foreground "Yellow Green" :bold t))
    (t (:bold t)))
  "Face used for displaying online Buddies."
  :group 'tnt-faces)

;; ---------------------------------------------------------------------------
(defface tnt-buddy-list-away-face
  '((((class color) (background light)) (:foreground "Steel Blue" :italic t))
    (((class color) (background dark)) (:foreground "Light Steel Blue" :italic t))
    (t (:italic t)))
  "Face used for displaying away Buddies."
  :group 'tnt-faces)

;; ---------------------------------------------------------------------------
(defface tnt-buddy-list-idle-face
  '((((class color) (background light)) (:foreground "Forest Green"))
    (((class color) (background dark)) (:foreground "Yellow Green"))
    (t (:inverse-video t :bold t)))
  "Face used for displaying idle Buddies."
  :group 'tnt-faces)

;; ---------------------------------------------------------------------------
(defface tnt-buddy-list-inactive-face
  '((((class color) (background light)) (:foreground "Red" :italic t))
    (((class color) (background dark)) (:foreground "Pink" :italic t))
    (t (:inverse-video t :bold t)))
  "Face used for displaying inactive Buddies."
  :group 'tnt-faces)

;; ---------------------------------------------------------------------------
(defface tnt-buddy-list-pounce-face
  '((((class color) (background light)) (:foreground "Red" :italic t :bold t))
    (((class color) (background dark)) (:foreground "Pink" :italic t :bold t))
    (t (:inverse-video t :bold t)))
  "Face used for displaying Buddies with pending pounce messages."
  :group 'tnt-faces)

;; ---------------------------------------------------------------------------
(defface tnt-buddy-list-message-waiting-face
  '((((class color) (background light)) (:foreground "Red" :italic t :bold t))
    (((class color) (background dark)) (:foreground "Pink" :italic t :bold t))
    (t (:inverse-video t)))
  "Face used for displaying Buddies with pending messages."
  :group 'tnt-faces)

;; ---------------------------------------------------------------------------
;; ----- beep/sound group
;; ----- Sound support is built in to XEmacs, so it'll likely work better there.
;; ---------------------------------------------------------------------------
(defgroup tnt-sound nil
  "TNT sound options"
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-sound-exec nil
  "On non-XEmacs systems, the executable used to play sounds.  Program's
final argument should be the filename of the sound to play.  Other options
can be specified with tnt-sound-exec-args.

For instance, on Windows you might do:
  (setq tnt-sound-exec \"sndrec32.exe\")
  (setq tnt-sound-exec-args
        (list \"/play\" \"/close\" \"/embedding\"))"
  :type '(choice :tag "Sound options"
                 (string :tag "Sound executable")
                 (const :tag "No Sound Executable" nil))
  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-sound-exec-args nil
  "On non-XEmacs systems, a list of strings representing the arguments
to be passed to tnt-sound-exec.

For instance, on Windows you might do:
  (setq tnt-sound-exec \"sndrec32.exe\")
  (setq tnt-sound-exec-args
        (list \"/play\" \"/close\" \"/embedding\"))"
  :type '(choice :tag "Sound command line arguments"
                 (string :tag "Command Line Arguments")
                 (const :tag "No Command Line Arguments" nil))
  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-incoming-message 'current
  "If non-nil, beeps when giving the \"Message from ... available\" message.

Settings:
 nil        Do not beep.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-first-incoming-message nil
  "If non-nil, beeps the first time an incoming message comes from
someone (i.e. that buffer doesn't exist).

If nil, the value of tnt-beep-on-incoming-message is used instead.

Settings:
 nil        Use the value of tnt-beep-on-incoming-message.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-visible-incoming-message nil
  "If non-nil, beeps every time a message comes into a visible IM buffer.

Settings:
 nil        Do not beep.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-outgoing-message nil
  "If non-nil, beep when you send a message.  I have no idea why
anyone would want to use this.

Settings:
 nil        Do not beep.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-chat-invitation 'current
  "If non-nil, beeps when you are invited to a chatroom.

Settings:
 nil        Do not beep.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-chat-message 'current
  "If non-nil, beeps when there is activity in a hidden chat buffer.

Settings:
 nil        Do not beep.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-visible-chat-message nil
  "If non-nil, beeps when there is activity in a visible chat buffer.

Settings:
 nil        Do not beep.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-buddy-signon nil
  "If non-nil, beeps when buddies sign on.

Settings:
 nil        Do not beep.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec.

Note that whatever value this variable has, you will still get
messages in your minibuffer saying \"MyBuddy online\" (provided that
the tnt-message-on-buddy-signonoff variable is non-nil)."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-buddy-signoff nil
  "If non-nil, beeps when buddies sign off.

Settings:
 nil        Do not beep.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec.

Note that whatever value this variable has, you will still get
messages in your minibuffer saying \"MyBuddy offline\" (provided that
the tnt-message-on-buddy-signonoff variable is non-nil)."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-signon nil
  "If non-nil, beeps when you sign on.

Settings:
 nil        Do not beep.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-signoff nil
  "If non-nil, beeps you sign off or get disconnected.

Settings:
 nil        Do not beep.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-error 'current
  "If non-nil, beeps when an error occurs.

Settings:
 nil        Do not beep.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
(defcustom tnt-beep-on-persistent-messages nil
  "If non-nil, beeps when a persistent message is displayed.

Settings:
 nil        Do not beep.
 'visible   Visible bell.
 'audible   Audible bell.
 'current   Current emacs setting (visible or audible).
 filename   You can also set this value to a string, which is the
            filename of a soundfile to play.  On non-XEmacs systems,
            you'll need to set tnt-sound-exec."
  :type '(choice
          (const :tag "No beep/bell" nil)
          (const :tag "Visible bell" visible)
          (const :tag "Audible bell" audible)
          (const :tag "Current Emacs default (visible or audible)" current)
          (file :must-match t :tag "Filename"))

  :group 'tnt-sound)

;; ---------------------------------------------------------------------------
;; ----- advanced TNT options group
;; ---------------------------------------------------------------------------
(defgroup tnt-advanced nil
  "Advanced TNT options"
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-toc-host "toc.oscar.aol.com"
  "TOC hostname -- do NOT change unless you know what you're doing!"
  :type 'string
  :group 'tnt-advanced)

;; ---------------------------------------------------------------------------
(defcustom tnt-toc-port 5190
  "TOC port # -- do NOT change unless you know what you're doing!"
  :type 'integer
  :group 'tnt-advanced)

;; ---------------------------------------------------------------------------
(defcustom tnt-login-host "login.oscar.aol.com"
  "TNT hostname -- do NOT change unless you know what you're doing!"
  :type 'string
  :group 'tnt-advanced)

;; ---------------------------------------------------------------------------
(defcustom tnt-login-port 5190
  "TNT port # -- do NOT change unless you know what you're doing!"
  :type 'integer
  :group 'tnt-advanced)

;; ---------------------------------------------------------------------------
(defcustom tnt-language "english"
  "TNT language -- do NOT change unless you know what you're doing!"
  :type 'string
  :group 'tnt-advanced)

;;; ***************************************************************************
;;; ***** font lock
;;; ***************************************************************************
(defvar tnt-buddy-list-group-face   'tnt-buddy-list-group-face
  "Face name to use for Buddy group names.")

;; ---------------------------------------------------------------------------
(defvar tnt-buddy-list-active-face  'tnt-buddy-list-active-face
  "Face name to use for online Buddies.")

;; ---------------------------------------------------------------------------
(defvar tnt-buddy-list-away-face    'tnt-buddy-list-away-face
  "Face name to use for aways Buddies.")

;; ---------------------------------------------------------------------------
(defvar tnt-buddy-list-idle-face    'tnt-buddy-list-idle-face
  "Face name to use for idle Buddies.")

;; ---------------------------------------------------------------------------
(defvar tnt-buddy-list-pounce-face  'tnt-buddy-list-pounce-face
  "Face name to use for Buddies with pending pounce messages.")

;; ---------------------------------------------------------------------------
(defvar tnt-buddy-list-message-waiting-face  'tnt-buddy-list-message-waiting-face
  "Face name to use for Buddies with pending pounce messages.")

;; ---------------------------------------------------------------------------
(defvar tnt-buddy-list-inactive-face    'tnt-buddy-list-inactive-face
  "Face name to use for inactive Buddies.")

;; ---------------------------------------------------------------------------
(defvar tnt-buddy-list-font-lock-keywords)

;; ---------------------------------------------------------------------------
(setq tnt-buddy-list-font-lock-keywords
      (list
       '("^\\(.*(MESSAGE WAITING.+\\)$"   1 tnt-buddy-list-message-waiting-face)
       '("^\\(.*(pounce.+\\)$"   1 tnt-buddy-list-pounce-face)
       '("^\\(.*(idle .+\\)$"   1 tnt-buddy-list-idle-face)
       '("^\\(.*(v idle .+\\)$" 1 tnt-buddy-list-away-face)
       '("^\\(.*(away.+\\)$"     1 tnt-buddy-list-away-face)
       '("^\\(.*(offline.+\\)$"  1 tnt-buddy-list-inactive-face)
       '("^\\(\\S-+.+\\)$"       1 tnt-buddy-list-group-face)
       '("^\\(.+\\)$"            1 tnt-buddy-list-active-face)
       ))

;; ---------------------------------------------------------------------------
;; ----- what to do with these?
;; ---------------------------------------------------------------------------
(defvar tnt-use-keepalive tnt-timers-available
  "If non-nil, sends a keepalive packet once a minute")

;; ---------------------------------------------------------------------------
(defvar tnt-use-buddy-update-timer tnt-timers-available
  "If non-nil, updates the idle times in the buddy list each minute.")

;; ---------------------------------------------------------------------------
(defvar tnt-use-idle-timer tnt-timers-available
  "If non-nil, tells TOC server when emacs has been idle for 10 minutes.")

;;; ***************************************************************************
;;; ***** keybindings
;;; ***************************************************************************
(unless tnt-inhibit-key-bindings
  (global-set-key "\C-xt?" 'tnt-show-help)
  (global-set-key "\C-xta" 'tnt-accept)
  (global-set-key "\C-xtA" 'tnt-toggle-away)
  (global-set-key "\C-xtb" 'tnt-show-buddies)
  (global-set-key "\C-xtB" 'tnt-edit-buddies)
  (global-set-key "\C-xtc" 'tnt-customize)
  (global-set-key "\C-xtf" 'tnt-set-info)
  (global-set-key "\C-xti" 'tnt-im)
  (global-set-key "\C-xtj" 'tnt-join-chat)
  (global-set-key "\C-xtl" 'tnt-leave-chat)
  (global-set-key "\C-xtL" 'tnt-pounce-list)
  (global-set-key "\C-xtm" 'tnt-toggle-mute)
  (global-set-key "\C-xtM" 'tnt-toggle-email)
  (global-set-key "\C-xtn" 'tnt-next-event)
  (global-set-key "\C-xto" 'tnt-open)
  (global-set-key "\C-xtp" 'tnt-prev-event)
  (global-set-key "\C-xtP" 'tnt-toggle-pounce)
  (global-set-key "\C-xtq" 'tnt-kill)
  (global-set-key "\C-xtr" 'tnt-reject)
  (global-set-key "\C-xts" 'tnt-switch-user)
  (global-set-key "\C-xtv" 'tnt-archive-view-archive-dwim)
  (global-set-key "\C-xtx" 'tnt-proxy-toggle-proxy-use)
  (global-set-key "\C-xtX" 'tnt-proxy-switch-servers)
  )

;;; ***************************************************************************
;;; ***** Pounce Package
;;; ***************************************************************************
(defvar tnt-pounce-alist nil)

;;; ***************************************************************************
(defun tnt-toggle-pounce (&optional pnick)
  "Toggle pounce.

Pounce if I have no pounce message currently for that Buddy, or delete
the current pounce message if I do have one.

PNICK - optional Buddy nickname (will be prompted for a Buddy's name
if nil)"
  (interactive)
  (let ((nick pnick))
    (when (and (not nick)
               (eq major-mode 'tnt-buddy-list-mode))
      ;; get nick on this line
      (let ((buddy-at-point (tnt-get-buddy-at-point)))
        (when (string= (car buddy-at-point) "im")
          (setq nick (toc-normalize (cdr buddy-at-point))))))

    (unless nick
      (setq nick (toc-normalize
                  (completing-read "Buddy name: "
                                   (mapcar 'list
                                           (tnt-extract-normalized-buddies
                                            tnt-buddy-blist))
                                   ))))

    ;; do something with it
    (if (assoc nick tnt-pounce-alist)
        (tnt-pounce-delete nick)
      (tnt-pounce-add nick))
    ))

;;; ***************************************************************************
(defun tnt-pounce-add (&optional pnick)
  "Allows a user to store a pounce message for a buddy.

PNICK - optional Buddy nickname (will be prompted for a Buddy's name
if nil)"
  (interactive)
  (let* ((completion-ignore-case t)
         (nick (or pnick
                   (toc-normalize
                    (completing-read "Buddy to Pounce on: "
                                     (mapcar 'list
                                             (tnt-extract-normalized-buddies
                                              tnt-buddy-blist))))))
         (msg_tmp (read-from-minibuffer "Pounce message to send (enter for none): "))
         (msg (if (string= msg_tmp "") "" msg_tmp)))
    (setq tnt-pounce-alist (tnt-addassoc nick msg tnt-pounce-alist))
    (message "%s has been added to your pounce list" nick)

    (tnt-build-buddy-buffer)
    ))

;;; ***************************************************************************
(defun tnt-pounce-delete (&optional nick)
  "Deletes a stored pounce message for the given buddy.

PNICK - optional Buddy nickname (will be prompted for a Buddy's name
if nil)"
  (interactive)
  (if (null tnt-pounce-alist)
      (message "No pounce messages to delete")
    (if (not nick)
        (let* ((completion-ignore-case t))
          (setq nick (toc-normalize (completing-read "Delete pounce for user: "
                                                     tnt-pounce-alist nil t)))))
    (if (not (assoc nick tnt-pounce-alist))
        (message "There is no pounce stored for %s" nick)
      (progn
        (setq tnt-pounce-alist (tnt-remassoc nick tnt-pounce-alist))
        (message "The pounce for %s has been deleted." nick))

      (tnt-build-buddy-buffer)
      )))

;;; ***************************************************************************
(defun tnt-pounce-list ()
  "List current pounce message in new buffer."
  (interactive)
  (let ((pounce-alist tnt-pounce-alist)
        (fmt "%-16s   %s\n")
        current)
    (with-output-to-temp-buffer "*tnt pounce list*"
      (with-current-buffer standard-output
        (insert (format fmt "Buddy" "Pounce Message"))
        (insert (format fmt "-----" "--------------"))

        (if (> (length pounce-alist) 0)
            (while pounce-alist
              (setq current (car pounce-alist))
              (setq pounce-alist (cdr pounce-alist))

              (insert (format fmt (car current) (cdr current)))
              )
          (insert (format fmt "<none>" "")))
        ))
    ))

;;; ***************************************************************************
(defun tnt-send-pounce (user)
  "Send any queued pounce messages to USER."
  (unless (and tnt-away tnt-supress-pounce-when-away)
    (let* ((msg (cdr (assoc user tnt-pounce-alist)))
           (ourmsg (if (string= msg "")
                       (format "<POUNCE MSG> %s is now available" user) msg)))
      (if msg
          (let ((buffer (tnt-im-buffer user))
                (buffer-name (tnt-im-buffer-name user)))
            (toc-send-im user msg)
            (tnt-append-message-and-adjust-window buffer ourmsg
                                                  tnt-current-user)
            (tnt-beep tnt-beep-on-incoming-message)
            (tnt-push-event (format "You have pounced on %s" user)
                            buffer-name nil)
            (tnt-pounce-delete user))
        ))
    ))

;;; ***************************************************************************
(defun tnt-send-pending-pounces-maybe ()
  ""
  (let ((pounce-list tnt-pounce-alist)
        current nick)
    (while pounce-list
      (setq current (car pounce-list))
      (setq pounce-list (cdr pounce-list))

      (setq nick (car current))

      (when (tnt-buddy-status nick)
        (tnt-send-pounce nick)))
    ))

;;; ***************************************************************************
;;; ***** Keepalive/Away Packages - jnwhiteh@syr.edu
;;; ***************************************************************************
(defvar tnt-keepalive-interval 60)
(defvar tnt-last-away-sent nil)
(defvar tnt-away-msg nil)
(defvar tnt-away-alist nil)
(defvar tnt-away nil)
(defvar tnt-keepalive-timer nil)
(defvar tnt-reconnecting nil)
(defvar tnt-reconnecting-away nil)
(defvar tnt-reconnecting-away-msg nil)
(defvar tnt-reconnecting-idle-time nil)
(defvar tnt-just-reconnected nil)
(defvar tnt-just-reconnected-unset-after 3)

;;; ***************************************************************************
(defun tnt-unset-just-reconnected ()
  "Sets `tnt-just-reconnected' to nil."
  (setq tnt-just-reconnected nil))

;;; ***************************************************************************
(defun tnt-is-buddy-away (nick)
  "Return t if buddy NICK is away; nil otherwise."
  (cdr (assoc (toc-normalize nick) tnt-away-alist)))

;;; ***************************************************************************
(defun tnt-toggle-away (prefix)
  "Toggles current away status.

The value of `tnt-default-away-message' is used as the away message,
unless PREFIX arg is given."
  (interactive "P")
  (if tnt-away
      (tnt-not-away)
    (tnt-set-away (tnt-get-away-msg prefix))))

;;; ***************************************************************************
(defun tnt-not-away ()
  "Sets you as NOT away."
  (interactive)
  (let ((away tnt-away))
    (setq tnt-away nil)
    (setq tnt-last-away-sent nil)
    (when away
      (message "You have returned.")
      (tnt-send-pending-pounces-maybe))
    (toc-set-away nil)
    (tnt-set-online-state t)
    )
  )

;;; ***************************************************************************
(defvar tnt-away-msg-history nil)

;;; ---------------------------------------------------------------------------
(defun tnt-get-away-msg (prefix)
  "Gets the away message.  If PREFIX is non-nil, prompt for message."
  (if (or prefix (not tnt-default-away-message))
      (read-from-minibuffer "Away Message: "
                            (cons
                             (if tnt-away-msg-history
                                 (car tnt-away-msg-history)
                               "I'm away.")
                             0)
                            nil nil 'tnt-away-msg-history)
    tnt-default-away-message)
  )

;;; ***************************************************************************
(defun tnt-set-away (away-msg)
  "Sets user as away, using AWAY-MSG."
  (setq tnt-away-msg away-msg)
  (setq tnt-away t)
  (toc-set-away tnt-away-msg)
  (tnt-set-online-state t)
  (message "Set as away: %s" tnt-away-msg)
  )

;;; ***************************************************************************
(defun tnt-send-away-msg (user)
  "Send the current away message to USER."
  (if (not (string= user tnt-last-away-sent))
      (let ((buffer (tnt-im-buffer user)))
        (setq tnt-last-away-sent user)
        (toc-send-im user tnt-away-msg t)
        (tnt-append-message-and-adjust-window
         buffer tnt-away-msg tnt-current-user "Auto-response"))))

;;; ***************************************************************************
;;; ***** giving the TOC server our profile
;;; ***************************************************************************

(defvar tnt-info-msg-history nil)

(defun tnt-set-info (&optional prefix)
  "Send profile to TOC server."
  (interactive "p")
  (toc-set-info (tnt-get-info-msg prefix)))

;;; ---------------------------------------------------------------------------
(defun tnt-get-info-msg (prefix)
  "Gets the info message."
  (if (or (/= prefix 1)
          (not tnt-default-info-message))
      (read-from-minibuffer "Info Message: "
                            (cons
                             (if tnt-info-msg-history
                                 (car tnt-info-msg-history)
                               "<a href=\"http://tnt.sourceforge.net/\">TNT</a>---AIM for grownups")
                             0)
                            nil nil 'tnt-info-msg-history)
    tnt-default-info-message)
  )

;;; ***************************************************************************
;;; ***** telling the TOC server we've gone idle
;;; ***************************************************************************
;;  the timers are created in tnt-handle-sign-on below
(defvar tnt-idle-timer nil)
(defvar tnt-send-idle-after 600) ;; could be defcustom??
(defvar tnt-currently-idle nil)

;;; ***************************************************************************
(defun tnt-send-idle (&optional idle-secs)
  (if (and tnt-current-user (not tnt-currently-idle))
      (let ((idle-secs (if idle-secs idle-secs tnt-send-idle-after)))
        (add-hook 'pre-command-hook 'tnt-send-unidle)
        (setq tnt-currently-idle t)
        (toc-set-idle idle-secs)
        (tnt-build-buddy-buffer)
        (tnt-set-mode-string t)
        )))

;;; ***************************************************************************
(defun tnt-send-unidle ()
  (remove-hook 'pre-command-hook 'tnt-send-unidle)
  (if tnt-currently-idle
      (progn
        (setq tnt-currently-idle nil)
        (if tnt-current-user (toc-set-idle 0))
        (tnt-set-mode-string t)
        )))

;;; ***************************************************************************
;;; ***** Signon/Signoff
;;; ***************************************************************************
(defvar tnt-username)
(defvar tnt-password)

;;; ***************************************************************************
(defun tnt-open-or-show-buddies ()
  "Sign on (if not already) or show Buddies buffer."
  (interactive)
  (if tnt-current-user
      (tnt-show-buddies)
    (call-interactively 'tnt-open)))

;;; ***************************************************************************
(defun tnt-open (username password)
  "Starts a new TNT session."
  (interactive "p\np") ;; gag!
  (if tnt-current-user
      (error "Already online as %s" tnt-current-user)
    (setq tnt-username (or (and (stringp username) username)
                           tnt-default-username
                           (and tnt-username-alist
                                (caar tnt-username-alist))
                           (read-from-minibuffer "Screen name: "))
          tnt-password (or (and (stringp password) password)
                           tnt-default-password
                           (and tnt-username-alist
                                (cdar tnt-username-alist))
                           (if (fboundp 'read-passwd)
                               (read-passwd (format "Password for %s: " tnt-username))
                             (tnt-read-from-minibuffer-no-echo
                              (format "Password for %s: " tnt-username))))
          )
    (if (string-equal tnt-password "")
        (error "No password given")
      (message "Attempting to sign on...")
      (add-hook 'toc-opened-hooks 'tnt-handle-opened)
      (add-hook 'toc-closed-hooks 'tnt-handle-closed)
      (add-hook 'toc-sign-on-hooks 'tnt-handle-sign-on)
      (add-hook 'toc-config-hooks 'tnt-handle-config)
      (add-hook 'toc-nick-hooks 'tnt-handle-nick)
      (add-hook 'toc-im-in-hooks 'tnt-handle-im-in)
      (add-hook 'toc-update-buddy-hooks 'tnt-handle-update-buddy)
      (add-hook 'toc-error-hooks 'tnt-handle-error)
      (add-hook 'toc-eviled-hooks 'tnt-handle-eviled)
      (add-hook 'toc-chat-join-hooks 'tnt-handle-chat-join)
      (add-hook 'toc-chat-in-hooks 'tnt-handle-chat-in)
      (add-hook 'toc-chat-update-buddy-hooks 'tnt-handle-chat-update-buddy)
      (add-hook 'toc-chat-invite-hooks 'tnt-handle-chat-invite)
      (add-hook 'toc-goto-url-hooks 'tnt-handle-goto-url)
      (add-hook 'toc-client-event-hooks 'tnt-handle-client-events)
      (toc-open tnt-toc-host tnt-toc-port tnt-username))))

;;; ***************************************************************************
(defun tnt-kill ()
  "Ends the current TNT session and signs off from the host."
  (interactive)

  (let ((user tnt-current-user))

    (toc-close)
    (tnt-shutdown)

    (when (null user) (error "Already offline"))

    (message "Signed off")
    (tnt-beep tnt-beep-on-signoff)

    (when tnt-kill-window-on-shutdown
      (kill-buffer tnt-buddy-list-buffer-name))
    ))

;;; ***************************************************************************
(defun tnt-switch-user ()
  "Switches the default username to log in as."
  (interactive)
  (if (null tnt-username-alist)
      (message "No username list defined.")
    (if tnt-current-user
        (message "Can't switch username while online.")
      (progn
        (setq tnt-username-alist (tnt-rotate-left tnt-username-alist))
        (if tnt-default-username
            (setq tnt-default-username (caar tnt-username-alist)))
        (if tnt-default-password
            (setq tnt-default-password (cdar tnt-username-alist)))
        (message "Next login will be as user %s" (caar tnt-username-alist))
        (tnt-build-buddy-buffer))
      )))

;;; ***************************************************************************
;;; ***** Instant message mode
;;; ***************************************************************************
(defvar tnt-im-mode-map nil)
(defvar tnt-im-user)
(defvar tnt-message-marker)
(defvar tnt-last-datestamp)

(make-variable-buffer-local 'tnt-im-user)
(make-variable-buffer-local 'tnt-last-datestamp)

(unless tnt-im-mode-map
  (setq tnt-im-mode-map (make-sparse-keymap "tnt-im-mode-map"))
  (define-key tnt-im-mode-map "\r" 'tnt-send-text-as-instant-message)

  (define-key tnt-im-mode-map (read-kbd-macro "C-<return>")
    'tnt-send-text-as-instant-message-no-format)
  (define-key tnt-im-mode-map (read-kbd-macro "M-<return>")
    (function (lambda () "" (interactive) (insert "
"))))
  )

;;; ***************************************************************************
(defun tnt-im-mode ()
  "Major mode for sending Instant Messages.
Special commands:
\\{tnt-im-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tnt-im-mode-map)
  (setq mode-name "IM")
  (setq major-mode 'tnt-im-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (auto-fill-mode 1)
  (when tnt-use-flyspell-mode (flyspell-mode))
  (run-hooks 'tnt-im-mode-hook))

;;; ***************************************************************************
(defun tnt-im (user)
  "Opens an instant-message conversation with a user."
  (interactive "p")
  (let* ((completion-ignore-case t)
         (input (or (and (stringp user) user)
                    (completing-read "Send IM to: "
                                     (tnt-online-buddies-collection)))))
    (tnt-remove-im-event input)
    (switch-to-buffer (tnt-im-buffer input))
    (if tnt-recenter-windows (recenter -1))))

;;; ***************************************************************************
(defun tnt-im-buffer-name (user)
  "Returns the name of the IM buffer for USER."
  (format "*im-%s*" (toc-normalize user)))

;;; ***************************************************************************
(defun tnt-im-archive-filename (user)
  "Returns the archive file filename (not full path) for IMs with USER."
  (format "im-%s" (toc-normalize user)))

;;; ***************************************************************************
(defun tnt-im-buffer (user)
  "Returns the IM buffer for USER."
  (let ((buffer-name (tnt-im-buffer-name user)))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (tnt-im-mode)
            (setq tnt-archive-filename (tnt-im-archive-filename user))
            (setq tnt-im-user user)
            (if tnt-include-datestamp-in-buffer-header
                (progn
                  (setq tnt-last-datestamp (format-time-string tnt-datestamp-format))
                  (insert (format "[Conversation with %s on %s]%s"
                                  (tnt-get-fullname-and-nick (tnt-buddy-official-name user))
                                  (current-time-string) tnt-separator)))
              (setq tnt-last-datestamp "")
              (insert (format "[Conversation with %s]%s"
                              (tnt-get-fullname-and-nick (tnt-buddy-official-name user))
                              tnt-separator)))
            (setq tnt-message-marker (make-marker))
            (set-marker tnt-message-marker (point))

            (tnt-mark-read-only-maybe)

            (when (and tnt-send-typing-notifications
                       tnt-timers-available)
              (make-local-hook 'after-change-functions)
              (add-hook 'after-change-functions
                        'tnt-typing-notification-hook nil t)
              (setq tnt-typing-notification-state nil)
              (make-local-hook 'kill-buffer-hook)
              (add-hook 'kill-buffer-hook 'tnt-typing-notification-kill-hook nil t))

            buffer)))))

;;; ***************************************************************************
(defun tnt-send-text-as-instant-message (&optional no-reformat)
  "Sends text at end of buffer as an IM."
  (interactive)
  (let ((message (tnt-get-input-message no-reformat)))
    (if (string= message "")
        (message "Please enter a message to send")
      (tnt-append-message message tnt-current-user nil no-reformat))
    (tnt-remove-im-event tnt-im-user)
    (if tnt-away (message "Reminder: You are still set as away"))
    (if tnt-recenter-windows (recenter -1))
    (if (string= message "") ()
      (progn
        (toc-send-im tnt-im-user message)
        (tnt-beep tnt-beep-on-outgoing-message)
        (when tnt-typing-notification-timer
          (cancel-timer tnt-typing-notification-timer)
          (setq tnt-typing-notification-timer nil))
        (setq tnt-typing-notification-state nil)
        ))))

;;; ***************************************************************************
(defun tnt-send-text-as-instant-message-no-format ()
  "Sends text at end of buffer as an IM without any formating."
  (interactive)
  (tnt-send-text-as-instant-message t))

;;; ***************************************************************************
(defun tnt-send-external-text-as-instant-message (user message)
  "Sends the given message to the given user."
  (interactive)
  (let ((buffer (tnt-im-buffer user)))
    ;; should maybe check if user is online ??
    (with-current-buffer buffer
      (tnt-append-message message tnt-current-user nil nil)
      (toc-send-im tnt-im-user message)
      )))

;;; ***************************************************************************
(defun tnt-show-help ()
  "Displays help for TNT."
  (interactive)
  (let* ((buffer-name "*tnt-help*")
         (help-buffer (get-buffer buffer-name)))
    (or (and help-buffer
             (switch-to-buffer help-buffer))
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (insert "
+-------------------+-------------+-------------------------------------------+
|  Function         | Key Binding |               Summary                     |
+-------------------+-------------+-------------------------------------------+
| tnt-show-help     |   C-x t ?   | Displays this help information            |
| tnt-open          |   C-x t o   | Starts a new TNT session                  |
| tnt-kill          |   C-x t q   | Terminates the current session            |
| tnt-im            |   C-x t i   | Starts an instant-message conversation    |
| tnt-join-chat     |   C-x t j   | Joins a chat room                         |
| tnt-leave-chat    |   C-x t l   | Leaves a chat room                        |
| tnt-show-buddies  |   C-x t b   | Shows the buddy list                      |
| tnt-edit-buddies  |   C-x t B   | Invokes the buddy list editor             |
| tnt-accept        |   C-x t a   | Accepts a message or a chat invitation    |
| tnt-reject        |   C-x t r   | Rejects a message or a chat invitation    |
| tnt-next-event    |   C-x t n   | Shows next event in notification ring     |
| tnt-prev-event    |   C-x t p   | Shows previous event in notification ring |
| tnt-switch-user   |   C-x t s   | Switches between usernames for next login |
| tnt-toggle-away   |   C-x t A   | Toggles away status, sets away message    |
| tnt-toggle-pounce |   C-x t P   | Adds or deletes a pounce message          |
| tnt-pounce-list   |   C-x t L   | Shows the pounce list                     |
| tnt-toggle-email  |   C-x t M   | Toggles forwarding incoming IMs to email  |
| tnt-toggle-mute   |   C-x t m   | Toggles sounds on/off                     |
+-------------------+-------------+-------------------------------------------+

"))
          (tnt-switch-to-buffer buffer)))))

;;; ***************************************************************************
;;; ***** Chat mode
;;; ***************************************************************************
(defvar tnt-chat-mode-map nil)
(defvar tnt-chat-alist nil)             ; room id to room name

(defvar tnt-chat-room)
(defvar tnt-chat-roomid)
(defvar tnt-chat-participants)

(make-variable-buffer-local 'tnt-chat-room)
(make-variable-buffer-local 'tnt-chat-roomid)
(make-variable-buffer-local 'tnt-chat-participants)

(unless tnt-chat-mode-map
  (setq tnt-chat-mode-map (make-sparse-keymap "tnt-chat-mode-map"))
  (define-key tnt-chat-mode-map "\r"   'tnt-send-text-as-chat-message)
  (define-key tnt-chat-mode-map "\n"   'tnt-send-text-as-chat-whisper)
  (define-key tnt-chat-mode-map "\t"   'tnt-send-text-as-chat-invitation)
  (define-key tnt-chat-mode-map "\M-p" 'tnt-show-chat-participants))

;;; ***************************************************************************
(defun tnt-chat-mode ()
  "Major mode for sending Instant Messages.
Special commands:
\\{tnt-chat-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tnt-chat-mode-map)
  (setq mode-name "Chat")
  (setq major-mode 'tnt-chat-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (auto-fill-mode 1)
  (when tnt-use-flyspell-mode (flyspell-mode))
  (run-hooks 'tnt-chat-mode-hook))

;;; ***************************************************************************
(defun tnt-join-chat (room)
  "Joins a chat room.  If in a chat buffer assume that is the one to join."
  (interactive "p")
  (if (null tnt-current-user)
      (error "You must be online to join a chat room")
    (let* ((input (or (and (stringp room) room)
                      (and (boundp 'tnt-chat-room) tnt-chat-room)
                      (tnt-read-string-with-default "Join chat room"
                                                    (funcall tnt-default-chatroom)))))
      (tnt-remove-chat-event input)
      (toc-chat-join input)
      (switch-to-buffer (tnt-chat-buffer input))
      (if tnt-recenter-windows (recenter -1)))))

;;; ***************************************************************************
(defun tnt-leave-chat (room)
  "Leaves a chat room.  If in a chat buffer assume that is the one to leave."
  (interactive "p")
  (if (null tnt-current-user)
      (error "You must be online to leave a chat room")
    (let* ((completion-ignore-case t)
           (input (or (and (stringp room) room)
                      (and (boundp 'tnt-chat-room) tnt-chat-room)
                      (and (eq major-mode 'tnt-buddy-list-mode)
                           (let ((buddy-at-point (tnt-get-buddy-at-point)))
                             (when (string= (car buddy-at-point) "chat")
                               (cdr buddy-at-point))))
                      (completing-read "Leave chat room: "
                                       (mapcar (lambda (x) (list (cdr x)))
                                               tnt-chat-alist)))))
      (with-current-buffer (tnt-chat-buffer input)
        (setq tnt-chat-participants nil)
        (setq tnt-chat-alist (tnt-remassoc tnt-chat-roomid tnt-chat-alist))
        (tnt-append-message (format "%s left" tnt-current-user))
        ;; do this last, it changes current-buffer
        (toc-chat-leave tnt-chat-roomid))
      (tnt-build-buddy-buffer)
      )))

;;; ***************************************************************************
(defun tnt-chat-buffer-name (room)
  "Returns the name of the chat buffer for ROOM."
  (format "*chat-%s*" (toc-normalize room)))

;;; ***************************************************************************
(defun tnt-chat-archive-filename (room)
  "Returns the archive file filename (not full path) for ROOM."
  (format "chat-%s" (toc-normalize room)))

;;; ***************************************************************************
(defun tnt-chat-buffer (room)
  "Returns the chat buffer for ROOM."
  (let ((buffer-name (tnt-chat-buffer-name room)))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (tnt-chat-mode)
            (setq tnt-archive-filename (tnt-chat-archive-filename room))
            (make-local-hook 'kill-buffer-hook)
            (add-hook 'kill-buffer-hook 'tnt-chat-buffer-killed nil t)
            (setq tnt-chat-room room)
            (setq tnt-chat-participants nil)

            (if tnt-include-datestamp-in-buffer-header
                (progn
                  (setq tnt-last-datestamp (format-time-string tnt-datestamp-format))
                  (insert (format "[Chat room \"%s\" on %s]%s"
                                  room (current-time-string) tnt-separator)))
              (setq tnt-last-datestamp "")
              (insert (format "[Chat room \"%s\"]%s" room tnt-separator)))

            (setq tnt-message-marker (make-marker))
            (set-marker tnt-message-marker (point))

            (tnt-mark-read-only-maybe)

            buffer)))))

;;; ***************************************************************************
(defun tnt-chat-buffer-killed ()
  (if tnt-current-user
      (tnt-leave-chat tnt-chat-room)))

;;; ***************************************************************************
(defun tnt-send-text-as-chat-message (&optional no-reformat)
  (interactive)
  (let ((message (tnt-get-input-message no-reformat)))
    (tnt-remove-chat-event tnt-chat-room)
    (toc-chat-send tnt-chat-roomid message)))

;;; ***************************************************************************
(defun tnt-send-text-as-chat-whisper (user &optional no-reformat)
  (interactive "p")
  (let* ((completion-ignore-case t)
         (user (or (and (stringp user) user)
                   (completing-read "Whisper to user: "
                                    (tnt-participant-collection))))
         (message (tnt-get-input-message no-reformat)))
    (if (= (length message) 0)
        (setq message (read-from-minibuffer "Message: ")))
    (tnt-append-message
     message tnt-current-user
     (format "whispers to %s" (tnt-buddy-official-name user)))
    (if tnt-recenter-windows (recenter -1))
    (toc-chat-whisper tnt-chat-roomid user message)))

;;; ***************************************************************************
(defun tnt-participant-collection ()
  (mapcar 'list tnt-chat-participants))

;;; ***************************************************************************
(defun tnt-send-text-as-chat-invitation (users &optional no-reformat)
  (interactive "p")

  (let ((user-list
         (if (and (listp users) users)
             users

           ;; replace (buddy-list) groups in the user-list with the
           ;; members of those groups who are online, and not already
           ;; in the chat, then prompt again to confirm.

           (let ((completion-ignore-case t)
                 (user-list-typed '(""))
                 (user-list-processed nil))

             (while (not (equal user-list-typed user-list-processed))
               (setq user-list-typed (tnt-completing-read-list
                                      "Users to invite: "
                                      (tnt-online-buddies-and-groups-collection)
                                      user-list-processed))
               (setq user-list-processed (tnt-expand-groups-for-chat-invitation
                                          user-list-typed
                                          tnt-chat-participants))
               )
             user-list-processed)
           )
         ))

    (if user-list

        (let (msg (tnt-get-input-message no-reformat))
          (if (= (length msg) 0)
              (setq msg (read-from-minibuffer "Message: "
                                              "Join me in this Buddy Chat.")))
          (tnt-append-message msg tnt-current-user
                              (format "invites %s"
                                      (mapconcat 'tnt-buddy-official-name
                                                 user-list ", ")))
          (if tnt-recenter-windows (recenter -1))
          (toc-chat-invite tnt-chat-roomid msg user-list)
          )
      )
    )
  )

;;; ***************************************************************************
(defun tnt-show-chat-participants ()
  "Append a list of chat room participants to a chat buffer."
  (interactive)
  (let ((string (mapconcat 'tnt-get-fullname-and-nick tnt-chat-participants ", ")))
    (tnt-append-message (format "Participants: %s" string))))

;;; ***************************************************************************
(defun tnt-chat-event-pop-function (accept)
  ;; Called when chat event is popped.  If event is accepted, the
  ;; current buffer is the chat buffer.
  (if accept
      (toc-chat-accept tnt-chat-roomid)))

;;; ***************************************************************************
(defun tnt-expand-groups-for-chat-invitation (user-list exclude-list)
  (remove-duplicates
   (apply 'append
          (mapcar (lambda (name)
                    (let ((group (assoc name tnt-buddy-blist)))
                      (if (null group) (list name)
                        (intersection (mapcar (lambda (x)
                                                (if (null (cdr x)) nil
                                                  (car x)))
                                              tnt-buddy-alist)
                                      (set-difference (cdr group)
                                                      exclude-list
                                                      ':test 'string=)
                                      ':test 'string=)
                        )))
                  user-list)
          )
   ':test 'string=)
  )

;;; ***************************************************************************
;;; ***** Utilites for the messaging modes (im, chat)
;;; ***************************************************************************
(make-variable-buffer-local 'tnt-message-marker)
(make-variable-buffer-local 'tnt-archive-filename)

(defun tnt-append-message-and-adjust-window (buffer message &optional user mod)
  (let ((window (get-buffer-window buffer)))
    (with-current-buffer buffer
      (tnt-append-message (tnt-reformat-text message) user mod)
      (if (and window tnt-recenter-windows)
          (let ((old-window (selected-window)))
            (select-window window)
            (recenter -1)
            (select-window old-window))))))

;;; ***************************************************************************
(defun tnt-get-fullname-and-nick (nick)
  ""
  (tnt-get-fullname-or-nick nick t))

;;; ---------------------------------------------------------------------------
(defun tnt-get-fullname-or-nick (nick &optional append)
  ""
  (let* ((fullname (tnt-fullname-for-nick nick))
         (rc (if fullname (if append (concat fullname " [" nick "]") fullname) nick)))
    rc))

;;; ***************************************************************************
(defun tnt-append-message (message &optional user modified no-reformat)
  "Prepends USER (MODIFIED) to MESSAGE and appends the result to the buffer."
  (save-excursion
    (let ((old-point (marker-position tnt-message-marker))
          (today-datestamp (format-time-string tnt-datestamp-format))
          (latest-archive-datestamp (assoc (buffer-name) tnt-archive-datestamp-alist))
          (tnt-inhibit-typing-notifications t))
      (goto-char tnt-message-marker)

      ;; datestamp -- print if the date-stamp has changed OR if we
      ;; want the datestamp in the header and we're archiving and it's
      ;; not in the alist
      (when (or (and tnt-last-datestamp
                     (not (string= tnt-last-datestamp today-datestamp)))
                (and tnt-include-datestamp-in-buffer-header
                     tnt-archive-conversations
                     (or (not latest-archive-datestamp)
                         (not (string= (cdr latest-archive-datestamp) today-datestamp)))))
        (setq tnt-last-datestamp today-datestamp)
        (insert-before-markers tnt-separator "[--- " today-datestamp " ---]" tnt-separator)

        (if latest-archive-datestamp
            (setcdr latest-archive-datestamp today-datestamp)
          (setq tnt-archive-datestamp-alist
                (cons (cons (buffer-name) today-datestamp) tnt-archive-datestamp-alist))))

      ;; optional timestamp
      (when tnt-use-timestamps
          (insert-before-markers (format-time-string tnt-timestamp-format)))

      ;; user
      (if (not user)
          (insert-before-markers "[" (tnt-replace-me-statement message) "]")

        (let ((start (point)))
          (insert-before-markers (tnt-get-fullname-or-nick user))
          (if modified
              (insert-before-markers " (" modified ")"))
          (insert-before-markers ":")
          ;; Change color of user text.
          (if (string-equal (toc-normalize user) (toc-normalize tnt-current-user))
              (add-text-properties start (point) '(face tnt-my-name-face))
            (add-text-properties start (point) '(face tnt-other-name-face)))
          (insert-before-markers " " (tnt-replace-me-statement message))))

      ;; formatting
      (insert-before-markers tnt-separator)

      (unless no-reformat
        (fill-region old-point (point)))

      ;; mark all text as read-only, if asked to
      (tnt-mark-read-only-maybe)

      ;; save to archive file
      (when tnt-archive-conversations
        (let* ((dir (tnt-archive-directory))
               (full-path (format "%s/%s" dir tnt-archive-filename)))
          (when (and dir tnt-archive-filename tnt-archive-conversations)
            (make-directory dir t)
            (append-to-file old-point (point) full-path)
            (message "")

            ;; see if we need to truncate the file
            (when (and (not tnt-archive-directory-hierarchy)
                       (> tnt-archive-max-single-file-size 0))
              (save-excursion
                ;; open archive file
                (with-temp-buffer
                  (insert-file-contents full-path t)

                  ;; determine size
                  (when (> (point-max) tnt-archive-max-single-file-size)
                    ;; go to N bytes from end
                    (goto-char (- (point-max) tnt-archive-max-single-file-size))

                    ;; goto beg of line
                    (backward-paragraph)

                    ;; delete from there back
                    (delete-region (point-min) (point))

                    ;; save
                    (save-buffer)
                    (message "")
                    )))
              ))))
      ))

  ;; Torches the entire undo history -- but we really don't want the
  ;; user to be able to undo TNT inserts.  The right fix is probably
  ;; to remove only certain undo information (or hold the undo info
  ;; before the insert happens, the restore it), then adjust the point
  ;; positions in the undo-list.
  (setq buffer-undo-list nil))

;;; ***************************************************************************
(defun tnt-mark-read-only-maybe ()
  "If `tnt-im-buffers-read-only' is non-nil, make read-only."
  ;; Make inserted text read-only.  I'm not really sure why we
  ;; need the inhibit-read-only code; IM buffers should never be
  ;; read-only (right?).
  (when tnt-im-buffers-read-only
    (let ((inhibit-read-only t))
      ;; gse: These need to be different in FSF/XEmacs.
      ;; XEmacs 21.4 on MS-Windows breaks if you try to copy
      ;; text that has a read-only property -- the copy routine
      ;; tries to strip out ^M's and the text is read-only.
      ;; Duh.  Using put-nonduplicable-text-property seems to
      ;; fix it.  Of course that doesn't exist on FSF.
      (if tnt-running-xemacs
          (put-nonduplicable-text-property (point-min) (point)
                                           'read-only t)
        (add-text-properties (point-min) (point)
                             '(read-only t front-sticky t rear-sticky t))
        (add-text-properties (- (point) 1) (point) '(rear-nonsticky t)))
      )))

;;; ***************************************************************************
(defun tnt-replace-me-statement (message)
  (when tnt-me-statement-format
    (if (and (>= (length message) 4) (string= (substring message 0 4) "/me "))
        (format tnt-me-statement-format (substring message 4))
      message)))

;;; ***************************************************************************
(defun tnt-get-input-message (&optional no-reformat)
  (let ((message (buffer-substring-no-properties tnt-message-marker (point-max))))
    (delete-region tnt-message-marker (point-max))
    (goto-char (point-max))
    (if tnt-recenter-windows (recenter -1))
    (tnt-replace-me-statement
     (if no-reformat message (tnt-neliminate-newlines message)))))

;;; ***************************************************************************
;;; Archive file functions
;;; ***************************************************************************
(defun tnt-archive-directory ()
  "Returns the directory into which conversations should be archived."
  (format "%s/%s%s" tnt-directory tnt-current-user
          (format-time-string
           (let ((freq tnt-archive-directory-hierarchy))
             (cond ((eq freq 'daily) "/%Y/%m/%d")
                   ((eq freq 'monthly) "/%Y/%m")
                   ((eq freq 'yearly) "/%Y")
                   ((null freq) "")
                   (t ""))))))

;;; ***************************************************************************
(defun tnt-archive-view-archive-dwim (&optional user)
  ""
  (interactive)
  (let (filename)
    ;; no user name given
    (if (not user)
        (cond
         ;; IM or Chat mode?
         ((or (eq major-mode 'tnt-im-mode)
              (eq major-mode 'tnt-chat-mode))
          (setq filename tnt-archive-filename))

         ;; Buddy list mode?
         ((eq major-mode 'tnt-buddy-list-mode)
          (let* ((buddy-at-point (tnt-get-buddy-at-point))
                 (type (car buddy-at-point))
                 (nick (cdr buddy-at-point)))
            (setq filename
                  (if (string= type "chat")
                      (tnt-chat-archive-filename nick)
                    (tnt-im-archive-filename nick)))))

         ;; prompt for buddy or chat room
         (t (let ((buddies-and-chats (nconc (mapcar 'list (tnt-extract-normalized-buddies tnt-buddy-blist))
                                            (mapcar 'list (mapcar 'cdr tnt-chat-alist))
                                            (mapcar 'cdr tnt-buddy-fullname-alist))))
              (setq user (completing-read "Name of archive to view: " buddies-and-chats nil nil))
              ;; was it a user, chat room or fullname?
              (let (elem)
                (setq elem (member user (mapcar 'cdr tnt-chat-alist)))
                (if elem
                    (setq filename (tnt-chat-archive-filename user))
                  (setq elem (rassoc (list user) tnt-buddy-fullname-alist))
                  (if elem
                      (setq filename (tnt-im-archive-filename (car elem)))
                    (setq filename (tnt-im-archive-filename user)))))
              )))

      ;; user was known
      (let (elem)
        (setq elem (assoc-ignore-case user tnt-chat-alist))
        (if elem
            (setq filename (tnt-chat-archive-filename (cdr elem)))
          (setq filename (tnt-im-archive-filename user)))))

      ;; check for fname and open if possible
      (unless filename
        (error "Unknown filename"))

      (let* ((dir (tnt-archive-directory))
             (full-path (format "%s/%s" dir filename)))
        (when (and dir full-path)
          (unless (file-exists-p full-path)
            (error (concat "No current archive file found [" filename "]")))
          (view-file-other-window full-path)))
      ))

;;; ***************************************************************************
(defun tnt-archive-delete-buddy-archive-file ()
  ""
  (interactive)
  (unless (eq major-mode 'tnt-buddy-list-mode)
    (error "Not in TNT Buddy List buffer"))

  (let* ((buddy-at-point (tnt-get-buddy-at-point))
         (type (car buddy-at-point))
         (nick (cdr buddy-at-point))
         (dir (tnt-archive-directory))
         (full-path (format "%s/%s" dir
                            (if (string= type "chat")
                                (tnt-chat-archive-filename nick)
                              (tnt-im-archive-filename nick)))))
    ;; file may not exist
    (unless (file-exists-p full-path)
      (error "No current archive file found"))

    ;; confirm
    (if (yes-or-no-p (concat "Delete archive file for '"
                             (tnt-get-fullname-and-nick nick) "'? "))
        (progn
          (delete-file full-path)
          (let ((buf (get-file-buffer full-path)))
            (when (and buf
                       (y-or-n-p "Kill buffer, too? "))
               (save-excursion
                 (kill-buffer buf)))))
      (error "Action canceled by user"))))

;;; ***************************************************************************
;;; ***** Flyspell-mode support
;;; ***************************************************************************
;; basically just ignore anything that's read-only....
(defun tnt-im-mode-flyspell-verify ()
  "This function is used for `flyspell-generic-check-word-p' in TNT."
  (not (get-text-property (point) 'read-only)))

(put 'tnt-im-mode 'flyspell-mode-predicate 'tnt-im-mode-flyspell-verify)
(put 'tnt-chat-mode 'flyspell-mode-predicate 'tnt-im-mode-flyspell-verify)

;;; ***************************************************************************
;;; ***** Buddy list mode
;;; ***************************************************************************
(defvar tnt-buddy-list-mode-map nil)

(defvar tnt-idle-alist nil)
(defvar tnt-away-alist nil)

(defvar tnt-just-signedonoff-alist nil)
(defvar tnt-just-signedonoff-delay 60)

(defvar tnt-login-flag-unset-after 10)
(defvar tnt-login-flag nil)
(defvar tnt-login-flag-timer nil)
(defvar tnt-login-flag-unset-ran-once nil)

(defvar tnt-buddy-update-timer nil)
(defvar tnt-buddy-update-interval 60)

(defvar tnt-current-menu 0)

(unless tnt-buddy-list-mode-map
  (setq tnt-buddy-list-mode-map (make-sparse-keymap "tnt-buddy-list-mode-map"))
  (define-key tnt-buddy-list-mode-map "?"    'tnt-show-help)
  (define-key tnt-buddy-list-mode-map "a"    'tnt-accept)
  (define-key tnt-buddy-list-mode-map "A"    'tnt-toggle-away)
  (define-key tnt-buddy-list-mode-map "b"    'tnt-show-buddies)
  (define-key tnt-buddy-list-mode-map "B"    'tnt-edit-buddies)
  (define-key tnt-buddy-list-mode-map "c"    'tnt-customize)
  (define-key tnt-buddy-list-mode-map "d"    'tnt-archive-delete-buddy-archive-file)
  (define-key tnt-buddy-list-mode-map "f"    'tnt-set-info)
  (define-key tnt-buddy-list-mode-map "ga"   'tnt-toggle-group-away-buddies)
  (define-key tnt-buddy-list-mode-map "gi"   'tnt-toggle-group-idle-buddies)
  (define-key tnt-buddy-list-mode-map "go"   'tnt-toggle-group-offline-buddies)
  (define-key tnt-buddy-list-mode-map "i"    'tnt-im-buddy)
  (define-key tnt-buddy-list-mode-map "I"    'tnt-fetch-info)
  (define-key tnt-buddy-list-mode-map "j"    'tnt-join-chat)
  (define-key tnt-buddy-list-mode-map "l"    'tnt-leave-chat)
  (define-key tnt-buddy-list-mode-map "L"    'tnt-pounce-list)
  (define-key tnt-buddy-list-mode-map "m"    'tnt-toggle-mute)
  (define-key tnt-buddy-list-mode-map "M"    'tnt-toggle-email)
  (define-key tnt-buddy-list-mode-map "n"    'tnt-next-buddy)
  (define-key tnt-buddy-list-mode-map "\M-n" 'tnt-next-group)
  (define-key tnt-buddy-list-mode-map "o"    'tnt-open)
  (define-key tnt-buddy-list-mode-map "O"    'tnt-toggle-inactive-buddies)
  (define-key tnt-buddy-list-mode-map "p"    'tnt-prev-buddy)
  (define-key tnt-buddy-list-mode-map "P"    'tnt-toggle-pounce)
  (define-key tnt-buddy-list-mode-map "\M-p" 'tnt-prev-group)
  (define-key tnt-buddy-list-mode-map "q"    'tnt-kill)
  (define-key tnt-buddy-list-mode-map "r"    'tnt-reject)
  (define-key tnt-buddy-list-mode-map "s"    'tnt-switch-user)
  (define-key tnt-buddy-list-mode-map "S"    'tnt-cycle-buddies-sort)
  (define-key tnt-buddy-list-mode-map "u"    'tnt-next-menu)
  (define-key tnt-buddy-list-mode-map "v"    'tnt-archive-view-archive-dwim)
  (define-key tnt-buddy-list-mode-map "x"    'tnt-proxy-toggle-proxy-use)
  (define-key tnt-buddy-list-mode-map "X"    'tnt-proxy-switch-servers)
  (define-key tnt-buddy-list-mode-map " "    'tnt-show-buddies)
  (define-key tnt-buddy-list-mode-map "\C-m" 'tnt-im-buddy)
  (define-key tnt-buddy-list-mode-map [down-mouse-2] 'tnt-im-buddy-mouse-down)
  (define-key tnt-buddy-list-mode-map [mouse-2] 'tnt-im-buddy-mouse-up)
  )

;;; ***************************************************************************
(defun tnt-buddy-list-mode ()
  "Major mode for viewing a buddy list.
Special commands:
\\{tnt-buddy-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tnt-buddy-list-mode-map)
  (setq mode-name "Buddy List")
  (setq major-mode 'tnt-buddy-list-mode)
  (set-syntax-table text-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(tnt-buddy-list-font-lock-keywords t))
  (setq tnt-archive-filename nil)
  (run-hooks 'tnt-buddy-list-mode-hook))

;;; ***************************************************************************
(defun tnt-show-buddies ()
  "Shows the buddy list in the selected window."
  (interactive)
  (tnt-switch-to-buffer (tnt-buddy-buffer))
  (tnt-build-buddy-buffer)
  (font-lock-mode 1)
  (tnt-show-top-event)
  )

;;; ***************************************************************************
(defun tnt-switch-to-buffer (buffer)
  (if (and tnt-use-split-buddy
           (not (string-equal (buffer-name) "*scratch*"))
           (not (string-equal (buffer-name) tnt-buddy-list-buffer-name)))
      (switch-to-buffer-other-window buffer)
    (switch-to-buffer buffer)))

;;; ***************************************************************************
(defun tnt-buddy-buffer ()
  (let ((buffer-name tnt-buddy-list-buffer-name))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (tnt-buddy-list-mode)
            (setq buffer-read-only t))
          buffer))))

;;; ***************************************************************************
(defun tnt-current-line-in-buffer ()
  ""
  (save-excursion
    (let ((col (current-column))
          (line-num (count-lines 1 (point))))
      (when (= col 0)
        (setq line-num (1+ line-num)))

      line-num)))

;;; ***************************************************************************
(defun tnt-next-menu ()
  (interactive)
  (setq tnt-current-menu (if (>= tnt-current-menu 4) 0 (1+ tnt-current-menu)))
  (tnt-build-buddy-buffer))

;;; ***************************************************************************
(defun tnt-build-buddy-buffer ()
  (with-current-buffer (tnt-buddy-buffer)
    (let* ((buffer-read-only nil)
           (col (current-column))
           (current-line (tnt-current-line-in-buffer)))
      ;; Insert contents of buddy buffer.
      (erase-buffer)
      (tnt-insert-blist tnt-buddy-blist t t)
      (tnt-non-buddy-messages)
      (tnt-chat-alist-to-buffer tnt-chat-alist)
      (tnt-buddy-list-menu)

      (set-buffer-modified-p nil)

      ;; Put the cursor somewhere useful.
      (goto-char 0)
      (if (and tnt-event-ring
               (search-forward "(MESSAGE WAITING)" nil t))
          (beginning-of-line)
        (goto-line current-line)
        (move-to-column col))
      )))

;;; ***************************************************************************

(defun tnt-insert-blist (blist decorate reorder)
  "Insert the contents of BLIST into the current buffer.
If DECORATE is non-nil, adds display information (fullname,
idle/away/pounce/message waiting, etc).

If REORDER is non-nil, calls tnt-reorder-annotated-blist, which
will strip/sort/group buddies based on the tnt-sort-buddies
and tnt-group-*-buddies settings."
  (let ((annotated-blist (tnt-annotated-blist blist)))
    (when reorder
      (setq annotated-blist (tnt-reorder-annotated-blist annotated-blist)))

    (while annotated-blist
      (let* ((group (car annotated-blist))
             (group-name (car group))
             (nick-list (cdr group)))

        (insert group-name "\n")

        (while nick-list
          (let* ((nick-list-entry (car nick-list))
                 (buddylist-name (car nick-list-entry))
                 (buddy-properties (cdr nick-list-entry))
                 (fullname   (plist-get buddy-properties 'fullname))
                 (unick      (plist-get buddy-properties 'unick))
                 (online     (plist-get buddy-properties 'online))
                 (away       (plist-get buddy-properties 'away))
                 (idle-desc  (plist-get buddy-properties 'idle-desc))
                 (idle-secs  (plist-get buddy-properties 'idle-secs))
                 (pounced    (plist-get buddy-properties 'pounced))
                 (event      (plist-get buddy-properties 'event))
                 (just-onoff (plist-get buddy-properties 'just-onoff))
                 )

            (if decorate
                (insert
                 (concat
                  "  "
                  (propertize (or fullname unick) 'mouse-face 'highlight)
                  (when fullname (concat " [" unick "]"))
                  (cond ((not online)
                         (format " (offline)"))
                        ((and away idle-desc)
                         (format " (away %s)" idle-desc))
                        ((and away (not idle-desc))
                         (format " (away)"))
                        ((and (not away) idle-desc)
                         (if (and (> tnt-very-idle-minimum 0)
                                  (> idle-secs tnt-very-idle-minimum))
                             (format " (v idle %s)" idle-desc)
                           (format " (idle %s)" idle-desc)))
                        (t ""))
                  just-onoff
                  (when pounced " (pounce)")
                  (when event " (MESSAGE WAITING)")
                  "\n"))
              ;; Undecorated version uses raw buddylist name.
              (insert (concat "  " buddylist-name "\n")))
          (setq nick-list (cdr nick-list)))))
      (insert "\n")
      (setq annotated-blist (cdr annotated-blist)))))


;;; ***************************************************************************

(defun tnt-annotated-blist (blist)
  "Return a buddy list with detailed information about each buddy.

The structure of the annotated blist is slightly different than
the regular blist -- instead of a string for each buddy name,
there is a cons cell.  Its car is the buddy name as it appears in the
buddy list; its car is a plist of buddy properties.

See tnt-nick-properties for a list of the properties."
  (let ((annotated-blist nil)
        (groupcount -1))
    (while blist
      (setq groupcount (+ 1 groupcount))
      (let* ((group (car blist))
             (group-name (car group))
             (nick-list (cdr group))
             (nickcount -1))
        ;; insert group name.
        (setq annotated-blist (cons (list group-name) annotated-blist))
        ;; add plists for each buddy in the group.
        (while nick-list
          (setq nickcount (+ 1 nickcount))
          (let* ((nick (car nick-list))
                 (nick-details (tnt-nick-properties nick groupcount nickcount)))
            (setcar annotated-blist (cons nick-details (car annotated-blist))))
          (setq nick-list (cdr nick-list)))
        )
      (setq blist (cdr blist)))

    (mapcar 'nreverse (nreverse annotated-blist))
    ))

;;; ***************************************************************************

(defun tnt-nick-properties (nick groupcount nickcount)
  "Return a cons cell containing detailed buddy information
about NICK.

car is the buddy name.
cdr is a plist containing details about that buddy's state:
  'online     - t if logged in; nil otherwise
  'unick      - un-normalized nick
  'nnick      - normalized nick
  'idle-secs  - idle time in seconds (if idle; nil otherwise)
  'idle-desc  - string describing idle time (if idle; nil otherwise)
  'away       - t if away, nil otherwise
  'just-onoff - if just on/off, a string saying so; nil otherwise
  'event      - non-nil if a message is waiting from this buddy
  'pounced    - pounce message, if one is set for this buddy; nil otherwise
  'fullname   - fullname string if buddy has one; nil otherwise
  'groupcount - first group is 0, second group is 1, etc
  'nickcount  - first nick in a group is 0, second nick is 1, etc
"
  (let* ((status (tnt-buddy-status nick))
         (online (char-or-string-p status))
         (unick (or status nick))
         (nnick (toc-normalize nick))
         (idle-secs (tnt-buddy-idle-secs nick))
         (idle (tnt-buddy-idle nick))
         (away (tnt-is-buddy-away nick))
         (just-onoff (tnt-get-just-signedonoff nnick))
         (event (assoc (tnt-im-buffer-name nick) tnt-event-ring))
         (pounced (cdr-safe (assoc nnick tnt-pounce-alist)))
         (fullname (tnt-fullname-for-nick nick))
         )

    (cons
     nick
     (list
      'online     online
      'unick      unick
      'nnick      nnick
      'idle-secs  idle-secs
      'idle-desc  idle
      'away       away
      'just-onoff just-onoff
      'event      event
      'pounced    pounced
      'fullname   fullname
      'groupcount groupcount
      'nickcount  nickcount
      ))))

;;; ***************************************************************************

(defun tnt-reorder-annotated-blist (blist)
  "Return re-ordered version BLIST (which is an annotated blist, as
created by tnt-annotated-blist).

Strips offline buddies according to tnt-show-inactive-buddies-now.
Sorts/groups buddies according to tnt-sort-buddies, tnt-group-*-buddies."
  ;; Have to take apart the annotated blist and reassemble it.
  ;; Maybe there's a clever lisp way to do this in-place, but I don't
  ;; know it.
  (let ((result nil))
    (while blist
      (let* ((group (car blist))
             (nick-list (cdr group)))

        ;; strip.
        (when (not tnt-show-inactive-buddies-now)
          (setq nick-list (tnt-strip-offline-nicks nick-list)))

        ;; sort.
        (setq nick-list (sort nick-list 'tnt-nick-less-than))
        ;;(setq nick-list (tnt-sort-nicks nick-list))

        (setcdr group nick-list)
        (setq result (append result (list group))))
      (setq blist (cdr blist)))

    result))

;;; ***************************************************************************

;; Used in tnt-nick-less-than.  If users want, it's easy to make this
;; pay attention to sort-fold-case.
(defun tnt-downcase (string)
  "Only downcase if non-nil."
  (if string (downcase string) string))

;;---------------------------------------------------------------------------

(defun tnt-nick-less-than (arg1 arg2)
  (let* ((nick1-props (cdr arg1))
         (nick2-props (cdr arg2))
         (nick1-blist-name (car arg1))
         (nick2-blist-name (car arg2))
         (nick1-nnick      (tnt-downcase (plist-get nick1-props 'nnick)))
         (nick2-nnick      (tnt-downcase (plist-get nick2-props 'nnick)))
         (nick1-fullname   (tnt-downcase (plist-get nick1-props 'fullname)))
         (nick2-fullname   (tnt-downcase (plist-get nick2-props 'fullname)))
         (nick1-idle-secs (plist-get nick1-props 'idle-secs))
         (nick2-idle-secs (plist-get nick2-props 'idle-secs))
         (nick1-away      (plist-get nick1-props 'away))
         (nick2-away      (plist-get nick2-props 'away))
         (nick1-online    (plist-get nick1-props 'online))
         (nick2-online    (plist-get nick2-props 'online))
         (nick1-offline   (not nick1-online))
         (nick2-offline   (not nick2-online))
         (nick1-nickcount (plist-get nick1-props 'nickcount))
         (nick2-nickcount (plist-get nick2-props 'nickcount))
         (nick1-sortname nick1-nnick)
         (nick2-sortname nick2-nnick)
         (nick1-vidle nil)
         (nick2-vidle nil)
         (nick1-score 0)
         (nick2-score 0)
         (name-less-than nil)
         )

    ;; Compare names and stow the result so we can use it as
    ;; a secondary sort after doing any grouping comparisons.
    ;;
    ;; Note that even if tnt-sort-buddies-by is nil, our sort
    ;; routine needs to reflect the order of the names in the blist.
    ;; (a user might have tnt-group-away-buddies set, but *not* have
    ;; tnt-sort-buddies-by set, in which case we want to group the
    ;; away buddies, but keep them in the same order as their buddy
    ;; list.)  That's why 'nickcount exists.
    (setq name-less-than
          (if tnt-sort-buddies-by
              (progn
                (when (eq tnt-sort-buddies-by 'fullname)
                  (when nick1-fullname (setq nick1-sortname nick1-fullname))
                  (when nick2-fullname (setq nick2-sortname nick2-fullname)))
                (if (equal nick1-sortname nick2-sortname)
                    (string< nick1-nnick nick2-nnick)
                  (string< nick1-sortname nick2-sortname)))
            (< nick1-nickcount nick2-nickcount)))

    ;; Check for "very idle".
    (setq nick1-vidle (and (> tnt-very-idle-minimum 0)
                           nick1-idle-secs
                           (> nick1-idle-secs tnt-very-idle-minimum)))
    (setq nick2-vidle (and (> tnt-very-idle-minimum 0)
                           nick2-idle-secs
                           (> nick2-idle-secs tnt-very-idle-minimum)))

    ;; Create sorting positions list by creating a bitmask for each
    ;; nick.  This is the easiest way I can think of to impose the
    ;; sorting hierarchy... offline > away > idle > namecomparison.
    ;; I feel like this is a goofy approach but it's pretty clean.

    (if name-less-than
        (setq nick2-score (+ 1 nick2-score))
      (setq nick1-score (+ 1 nick1-score)))


    ;; Note that these are cond clauses for a reason.  Since users can
    ;; be away *and* idle, only the highest-priority value should be
    ;; applied.  Otherwise a user who is away but not idle will be
    ;; sorted strangely compared to a user who is away/idle.
    (cond
     ((and tnt-group-offline-buddies nick1-offline)
      (setq nick1-score (+ nick1-score 1000)))
     ((and tnt-group-away-buddies (or nick1-away nick1-vidle))
      (setq nick1-score (+ nick1-score 100)))
     ((and tnt-group-idle-buddies nick1-idle-secs)
      (setq nick1-score (+ nick1-score 10)))
     )

    (cond
     ((and tnt-group-offline-buddies nick2-offline)
      (setq nick2-score (+ nick2-score 1000)))
     ((and tnt-group-away-buddies (or nick2-away nick2-vidle))
      (setq nick2-score (+ nick2-score 100)))
     ((and tnt-group-idle-buddies nick2-idle-secs)
      (setq nick2-score (+ nick2-score 10)))
     )

    (< nick1-score nick2-score)))

;;; ***************************************************************************

(defun tnt-strip-offline-nicks (nick-list)
  "Strips nicks that are offline, except for those that have pending
messages or pounces."
  (let ((result nil))
    (while nick-list
      (let* ((nick (car nick-list))
             (nick-props (cdr nick)))
        (when (or
               (plist-get nick-props 'online)
               (plist-get nick-props 'event)
               (plist-get nick-props 'pounced)
               (plist-get nick-props 'just-onoff))
          (setq result (append result (list nick)))))
      (setq nick-list (cdr nick-list)))
    result))

;;; ***************************************************************************

(defun tnt-non-buddy-messages ()
  ;; this is ugly, making use of the buffer name in each event like this,
  ;; rather than storing the information we actually need in the event.
  (let ((event-list tnt-event-ring)
        (event nil)
        (non-buddies nil))
    (while event-list
      (setq event (car event-list))
      (let ((event-buffer-name (car event)))
        (save-match-data
          (when (string-match "\\*\\(im\\|chat\\)-\\([^*]*\\)\\*"
                              event-buffer-name)
            (when (and (string= "im" (match-string 1 event-buffer-name))
                       (not (member (match-string 2 event-buffer-name)
                                    (tnt-extract-normalized-buddies
                                     tnt-buddy-blist))))
              (setq non-buddies (cons (match-string 2 event-buffer-name)
                                      non-buddies))))))
      (setq event-list (cdr event-list)))
    (when non-buddies
      (insert "\nnon-buddies\n")
      (mapcar '(lambda(x) (insert "  "
                                  (propertize x 'mouse-face 'highlight)
                                  " (MESSAGE WAITING)\n")) non-buddies)
      )))

;;; ***************************************************************************
(defun tnt-chat-alist-to-buffer (alist)
  (if alist (insert "\nchat rooms\n"))
  (while alist
    (let ((name (cdar alist)))
      (insert "  " (propertize name 'mouse-face 'highlight))
      (when (assoc (tnt-chat-buffer-name name) tnt-event-ring)
        (insert " (MESSAGE WAITING)"))
      (insert "\n")
      (setq alist (cdr alist)))))

;;; ***************************************************************************
(defun tnt-buddy-list-menu ()
  (if tnt-current-user
      (progn
        (insert "\n\n--------------------------------------------------------------------------\n")
        (cond ((and (= tnt-current-menu 0) tnt-event-ring)
               (insert "[a]ccept message     "
                       "[r]eject message     "
                       "                     "
                       "next men[u]"
                       "\n"
                       "                     "
                       "                     "
                       "                     "
                       "[?] help"
                       "\n"
                       ))

              ((or (= tnt-current-menu 1)
                   (and (= tnt-current-menu 0) (null tnt-event-ring)))
               (setq tnt-current-menu 1)
               (insert "[p]rev buddy         "
                       "[n]ext buddy         "
                       "[RET] IM buddy       "
                       "next men[u]"
                       "\n"
                       "[M-p]rev group       "
                       "[M-n]ext group       "
                       "[q]uit tnt           "
                       "[?] help"
                       "\n"
                       ))

              ((= tnt-current-menu 2)
               (insert "[j]oin chat room     "
                       (if tnt-away "unset [A]way status  "
                         "set [A]way status    ")
                       "edit [B]uddy list    "
                       "next men[u]"
                       "\n"
                       "[l]eave chat room    "
                       "[P]ounce on buddy    "
                       (if tnt-show-inactive-buddies-now "hide" "show")
                       " [O]ffline       "
                       "[?] help"
                       "\n"
                       ))

              ((= tnt-current-menu 3)
               (insert "[c]ustomize tnt      "
                       (if tnt-muted "un[m]ute tnt sounds  "
                         "[m]ute tnt sounds    ")
                       (if tnt-pipe-to-email-now
                           "turn off e[M]ail     "
                         "turn on e[M]ail      ")
                       "next men[u]"
                       "\n"
                       "[v]iew archive file  "
                       "[d]el archive file   "
                       "                     "
                       "[?] help"
                       "\n"
                       ))

              ((= tnt-current-menu 4)
               (insert (if tnt-group-away-buddies
                           "[ga]ungroup away     "
                         "[ga]group away       ")
                       (if tnt-group-idle-buddies
                           "[gi]ungroup idle     "
                         "[gi]group idle       ")
                       (if tnt-group-offline-buddies
                           "[go]ungroup offline  "
                         "[go]group offline    ")
                       "next men[u]"
                       "\n"
                       "[S]ort order         "
                       "toggle pro[x]y       "
                       "switch pro[X]ies     "
                       "[?] help   "
                       "\n"
                       ))

              (t (insert "\n"))))
    (insert "\n"
            "tnt currently offline"
            "\n\n"
            "[o]pen connection"
            (let ((username (or tnt-default-username
                                (and tnt-username-alist
                                     (caar tnt-username-alist)))))
              (if username (concat " as user: " username) ""))
            "\n"
            (if tnt-username-alist "[s]witch user" "")
            "\n")
    ))

;;; ***************************************************************************
(defun tnt-fetch-info ()
  "Requests the user info of a buddy (and launches browser with browse-url)."
  (interactive)

  (let ((nick (tnt-get-buddy-at-point)))
    (toc-get-info (cdr nick))))

;;; ***************************************************************************
(defun tnt-toggle-inactive-buddies ()
  "Toggle whether we show inactive buddies in buddy list."
  (interactive)
  (setq tnt-show-inactive-buddies-now (not tnt-show-inactive-buddies-now))
  (tnt-build-buddy-buffer))

;;; ***************************************************************************

(defun tnt-cycle-buddies-sort ()
  "Cycles through buddy list sorting options:
No sort -> Buddy name -> Fullname"
  (interactive)
  (setq tnt-sort-buddies-by
        (cond ((eq tnt-sort-buddies-by nil)        'buddyname)
              ((eq tnt-sort-buddies-by 'buddyname) 'fullname)
              ((eq tnt-sort-buddies-by 'fullname)  nil)))

  (tnt-build-buddy-buffer)

  (message
   (cond ((eq tnt-sort-buddies-by nil)        "Buddy list unsorted.")
         ((eq tnt-sort-buddies-by 'buddyname) "Buddy list sorted by buddy name.")
         ((eq tnt-sort-buddies-by 'fullname)  "Buddy list sorted by fullname.")))
  )

;;; ***************************************************************************

(defun tnt-toggle-group-away-buddies ()
  "Toggles whether 'away' buddies are grouped at the end of the buddy list."
  (interactive)
  (setq tnt-group-away-buddies (not tnt-group-away-buddies))
  (tnt-build-buddy-buffer)
  (if tnt-group-away-buddies
      (message "Away buddies grouped together.")
    (message "No grouping of 'away' buddies.")))

;;; ***************************************************************************

(defun tnt-toggle-group-idle-buddies ()
  "Toggles whether 'idle' buddies are grouped at the end of the buddy list."
  (interactive)
  (setq tnt-group-idle-buddies (not tnt-group-idle-buddies))
  (tnt-build-buddy-buffer)
  (if tnt-group-idle-buddies
      (message "Idle buddies grouped together.")
    (message "No grouping of 'idle' buddies.")))

;;; ***************************************************************************

(defun tnt-toggle-group-offline-buddies ()
  "Toggles whether offline buddies are grouped at the end of the buddy list."
  (interactive)
  (setq tnt-group-offline-buddies (not tnt-group-offline-buddies))
  (tnt-build-buddy-buffer)
  (if tnt-group-offline-buddies
      (message "Offline buddies grouped together.")
    (message "No grouping of offline buddies.")))

;;; ***************************************************************************

(defun tnt-im-buddy ()
  "Initiates an IM conversation with the selected buddy."
  (interactive)
  (let* ((buddy-at-point (tnt-get-buddy-at-point))
         (type (car buddy-at-point))
         (nick (cdr buddy-at-point))
         (nnick (toc-normalize nick)))
    (cond
     ((string= type "chat") (tnt-join-chat nick))
     ((tnt-buddy-status nick) (tnt-im nick))
     ((assoc (tnt-im-buffer-name nick) tnt-event-ring) (tnt-im nick))
     ((assoc nnick tnt-pounce-alist)
      (and (y-or-n-p (format "%s is offline; delete pounce? " nick))
           (tnt-pounce-delete nnick)))
     (t
      (and (y-or-n-p (format "%s is offline; pounce instead? " nick))
           (tnt-pounce-add nnick)))
     )))

;;; ***************************************************************************
(defun tnt-buddy-list-menu-line ()
  (save-excursion
    (save-match-data
      (goto-char 0)
      (re-search-forward "^-+$" nil t)
      (match-beginning 0))))

;;; ***************************************************************************
(defun tnt-get-buddy-at-point ()
  "Returns the nickname of the buddy at point."
  (save-excursion
    (save-match-data
      (end-of-line)
      (let ((eol-point (point)))
        (beginning-of-line)
        (if (or (null (or (re-search-forward "\\[\\([^]]+\\)\\]" eol-point t)
                          (re-search-forward "^ +\\([^(]+\\)" eol-point t)))
                (> (match-beginning 1) (tnt-buddy-list-menu-line)))
            (error "Position cursor on a buddy name")
          (let* ((match-b (match-beginning 1))
                 (match-e (match-end 1))
                 (nick (buffer-substring-no-properties match-b match-e))
                 (nick (substring nick 0
                                  (or (string-match "\\s-+$" nick)
                                      (length nick)))))
            (goto-char match-b)
            (if (re-search-backward "^chat rooms$" nil t)
                (cons "chat" nick)
              (cons "im" nick))))
        ))))

;;; ***************************************************************************
(defvar tnt-buddy-on-mouse-down "")

(defun tnt-im-buddy-mouse-down (event)
  "Stores nickname of the buddy selected by mouse click."
  (interactive "e")
  (mouse-set-point event)
  (setq tnt-buddy-on-mouse-down (tnt-get-buddy-at-point)))

;;; ***************************************************************************
(defun tnt-im-buddy-mouse-up (event)
  "Initiates an IM conversation if still clicking same buddy as on mouse-down."
  (interactive "e")
  (mouse-set-point event)
  (let* ((buddy-at-point (tnt-get-buddy-at-point))
         (type (car buddy-at-point))
         (nick (cdr buddy-at-point)))
    (if (string= (concat type nick) (concat (car tnt-buddy-on-mouse-down)
                                            (cdr tnt-buddy-on-mouse-down)))
        (if (string= type "chat")
            (tnt-join-chat nick)
          (tnt-im nick)))))

;;; ***************************************************************************
(defun tnt-next-buddy ()
  "Moves the cursor to the next buddy."
  (interactive)
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (if (or (null (re-search-forward "\n " nil t))
              (> (match-beginning 0) (tnt-buddy-list-menu-line)))
          (error "No next buddy")))
    (goto-char (match-beginning 0))
    (forward-char)
    ))

;;; ***************************************************************************
(defun tnt-prev-buddy ()
  "Moves the cursor to the previous buddy."
  (interactive)
  (save-match-data
    (beginning-of-line)
    (if (null (re-search-backward "\n " nil t))
        (error "No previous buddy"))
    (goto-char (match-beginning 0))
    (forward-char)
    ))

;;; ***************************************************************************
(defun tnt-next-group ()
  "Moves the cursor to the first buddy of the next group."
  (interactive)
  (save-match-data
    (beginning-of-line)
    (if (null (re-search-forward "\n[^ ]" nil t))
        (error "No next group"))
    (tnt-next-buddy)
    ))

;;; ***************************************************************************
(defun tnt-prev-group ()
  "Moves the cursor to the last buddy of the previous group."
  (interactive)
  (save-match-data
    (beginning-of-line)
    (if (null (re-search-backward "\n[^ ]" nil t))
        (error "No previous group"))
    (goto-char (match-beginning 0))
    (tnt-prev-buddy)
    ))

;;; ***************************************************************************
(defun tnt-shutdown ()
  ;; cancel timers
  (if tnt-keepalive-timer (cancel-timer tnt-keepalive-timer))
  (setq tnt-keepalive-timer nil)
  (if tnt-buddy-update-timer (cancel-timer tnt-buddy-update-timer))
  (setq tnt-buddy-update-timer nil)
  (if tnt-idle-timer (cancel-timer tnt-idle-timer))
  (setq tnt-idle-timer nil)
  (if tnt-login-flag-timer (cancel-timer tnt-login-flag-timer))
  (setq tnt-login-flag-timer nil)
  (setq tnt-login-flag nil)
  (setq tnt-login-flag-unset-ran-once nil)

  (setq tnt-current-user nil
        tnt-buddy-alist nil
        tnt-buddy-blist nil
        tnt-permit-list nil
        tnt-deny-list nil
        tnt-permit-mode 1
        tnt-away-alist nil
        tnt-idle-alist nil
        tnt-just-signedonoff-alist nil
        tnt-away nil
        tnt-last-away-sent nil)

  ;; this needs to happen after the current-user is set to nil, so it
  ;; knows we're no longer online
  (if tnt-currently-idle (tnt-send-unidle))

  (tnt-set-online-state nil)
  (tnt-build-buddy-buffer))

;;; ***************************************************************************
(defun tnt-set-buddy-status (nick onlinep idle away)
  (let ((nnick (toc-normalize nick))
        (status (if onlinep nick))
        (prevaway (tnt-is-buddy-away nick))
        (idletime (if (and onlinep idle (> idle 0))
                      ;; see NOTE below about (current-time)
                      (- (cadr (current-time))
                         (* 60 idle))))
        (state (if onlinep "online" "offline"))
        (fullname (tnt-get-fullname-or-nick nick)))

    (if (and (not tnt-login-flag)
             (not (string= status (tnt-buddy-status nick))))
        (progn
          ;; Beep appropriately.
          (if onlinep
              (tnt-beep tnt-beep-on-buddy-signon)
            (tnt-beep tnt-beep-on-buddy-signoff))
          (let ((buffer (get-buffer (tnt-im-buffer-name nick))))
            (if buffer
                (with-current-buffer buffer
                  (tnt-append-message (format "%s %s" fullname state)))))

          (if tnt-message-on-buddy-signonoff
              (message "%s %s" fullname state))

          (if tnt-timers-available (tnt-set-just-signedonoff nnick onlinep))
          ))

    (let ((just-onoff (assoc nick tnt-just-signedonoff-alist))
          (buffer (get-buffer (tnt-im-buffer-name nick))))
      (if (and (not just-onoff) buffer)
          (if (and away (not prevaway))
              (with-current-buffer buffer
                (tnt-append-message (format "%s has gone away." fullname)))
            (if (and (not away) prevaway)
                (with-current-buffer buffer
                  (tnt-append-message (format "%s has returned." fullname))))
            )))

    (if onlinep
        (tnt-send-pounce nnick))
    (setq tnt-buddy-alist (tnt-addassoc nnick status tnt-buddy-alist))
    (setq tnt-idle-alist (tnt-addassoc nnick idletime tnt-idle-alist))
    (setq tnt-away-alist (tnt-addassoc nnick away tnt-away-alist))

    (tnt-build-buddy-buffer)))

;;; ***************************************************************************
(defun tnt-buddy-status (nick)
  (cdr (assoc (toc-normalize nick) tnt-buddy-alist)))

;;; ***************************************************************************
(defun tnt-buddy-idle (nick)
  (let* ((idle-secs-or-nil (tnt-buddy-idle-secs nick))
         (idle-secs (if idle-secs-or-nil idle-secs-or-nil 0))
         (idle-mins (/ idle-secs 60)))
    (cond ((= 0 idle-mins) nil)
          ((< idle-mins 60) (format "%dm" idle-mins))
          (t (format "%dh%dm" (/ idle-mins 60) (mod idle-mins 60))))))

;;; ***************************************************************************
(defun tnt-buddy-idle-secs (nick)
  ;; NOTE: (current-time) doesn't actually give seconds since the
  ;; epoch, because elisp only allocates 28 bits for an integer (i
  ;; believe the remaining four bits are used to store what type that
  ;; word is storing, in this case, an int).  so current-time instead
  ;; gives a list containing the upper 16 bits and then the lower 16
  ;; bits.  so i'm just using the lower 16 bits, and assuming it won't
  ;; wrap around more than once.  which means that if someone is
  ;; actually idle for more than 65536 seconds (about 18 hours), then
  ;; it'll reset...
  (let ((idle-since (cdr (assoc (toc-normalize nick) tnt-idle-alist))))
    (if (null idle-since) nil
      (let* ((now (cadr (current-time)))
             (diff (- now idle-since)))
        (if (< diff 0) (+ diff 65536) diff)))))

;;; ***************************************************************************
(defun tnt-buddy-official-name (buddy)
  "Return official screen name of BUDDY if known, otherwise return BUDDY."
  (or (tnt-buddy-status buddy) buddy))

;;; ***************************************************************************
(defun tnt-online-buddies-collection ()
  ;; Return a "collection" of online buddies for completion commands.
  ;; (Remove all nil entries -- these turn up when a buddy logs off).
  (delete '(nil) (mapcar '(lambda(x) (list (cdr x))) tnt-buddy-alist)))

;;; ***************************************************************************
(defun tnt-online-buddies-and-groups-collection ()
  (append (mapcar (lambda(x) (list (car x))) tnt-buddy-blist)
          (tnt-online-buddies-collection)))

;;; ***************************************************************************
(defun tnt-set-just-signedonoff (nick onlinep)
  (let ((timestamp (cadr (current-time))))
    (setq tnt-just-signedonoff-alist
          (tnt-addassoc nick (list timestamp onlinep)
                        tnt-just-signedonoff-alist))
    (run-at-time tnt-just-signedonoff-delay nil
                 'tnt-unset-just-signedonoff nick timestamp)
    ))

;;; ***************************************************************************
(defun tnt-unset-just-signedonoff (nick timestamp-to-remove)
  (let ((most-recent (cadr (assoc nick tnt-just-signedonoff-alist))))
    (if (= most-recent timestamp-to-remove)
        (progn
          (setq tnt-just-signedonoff-alist
                (tnt-remassoc nick tnt-just-signedonoff-alist))
          (tnt-build-buddy-buffer)
          ))))

;;; ***************************************************************************
(defun tnt-get-just-signedonoff (nick)
  (let ((just-onoff (assoc nick tnt-just-signedonoff-alist)))
    (if (null just-onoff) nil
      (format " (just signed %s)"
              (if (caddr just-onoff) "on" "off")))))

;;; ***************************************************************************
(defun tnt-unset-login-flag ()
  (if tnt-login-flag-unset-ran-once
      (progn (cancel-timer tnt-login-flag-timer)
             (setq tnt-login-flag-timer nil
                   tnt-login-flag nil
                   tnt-login-flag-unset-ran-once nil))
    (setq tnt-login-flag-unset-ran-once t)))

;;; ***************************************************************************
;;; ***** Buddy-list edit mode
;;; ***************************************************************************
(defvar tnt-buddy-edit-mode-map nil)

(unless tnt-buddy-edit-mode-map
  (setq tnt-buddy-edit-mode-map (make-sparse-keymap "tnt-buddy-edit-mode-map"))
  (define-key tnt-buddy-edit-mode-map "\C-x\C-s" 'tnt-save-buddy-list)
  (define-key tnt-buddy-edit-mode-map "\C-c\C-c"
    (function (lambda () "" (interactive) (tnt-save-buddy-list t))))
  )

;;; ***************************************************************************
(defun tnt-buddy-edit-mode ()
  "Major mode for editing a buddy list.
Special commands:
\\{tnt-buddy-edit-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tnt-buddy-edit-mode-map)
  (setq mode-name "Buddy Edit")
  (setq major-mode 'tnt-buddy-edit-mode)
  (set-syntax-table text-mode-syntax-table)
  (run-hooks 'tnt-buddy-edit-mode-hook))

;;; ***************************************************************************
(defun tnt-edit-buddies ()
  "Shows the buddy-list editor in the selected window."
  (interactive)
  (switch-to-buffer (tnt-buddy-edit-buffer)))

;;; ***************************************************************************
(defconst tnt-buddy-edit-buffer-name "*edit-buddies*")

;;; ---------------------------------------------------------------------------
(defun tnt-buddy-edit-buffer ()
  (let ((buffer-name tnt-buddy-edit-buffer-name))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (tnt-buddy-edit-mode)
            ;; make-local-hook doesn't work here; tries to call t
            (make-local-variable 'kill-buffer-query-functions)
            (add-hook 'kill-buffer-query-functions 'tnt-buddy-edit-kill-query)
            (tnt-insert-blist tnt-buddy-blist nil nil)
            (set-buffer-modified-p nil))
          buffer))))

;;; ***************************************************************************
(defun tnt-buddy-edit-kill-query ()
  (or (null (buffer-modified-p))
      (yes-or-no-p "Buddy list modified; kill anyway? ")))

;;; ***************************************************************************
(defun tnt-nick-in-blist-group (blist nick group)
  "Returns nick if GROUP exists in BLIST, and contains NICK."
  (car (member nick (cdr (tnt-group-in-blist group blist)))))

;;; ***************************************************************************
(defun tnt-group-in-blist (group blist)
  "Returns group if GROUP exists in BLIST."
  (let ((result nil))
    (while (and blist (not result))
      (let* ((cur-group (car blist))
             (group-name (car cur-group)))
        (when (string-equal group-name group)
          (setq result cur-group)))
      (setq blist (cdr blist)))

    result))

;;; ***************************************************************************
(defun tnt-grouped-new-buddies (old-blist new-blist)
  "Returns a blist of groups, or buddies (in their respective groups)
that are in NEW-BLIST, but not OLD-BLIST.

This is all group-centric; so given
  old-blist = ((g1 b1 b2) (g2 b3 b4))
  new-blist = ((g0 b1) (g1 b2) (g2 b4 b5) (g3))
this would return
  ((g0 b1) (g2 b5) (g3))"
  (let ((result nil))
    (while new-blist
      (let* ((group       (car new-blist))
             (group-name  (car group))
             (nick-list   (cdr group))
             (new-group   (list group-name)))

        (while nick-list
          (let ((nick (car nick-list)))
            (when (not (tnt-nick-in-blist-group old-blist nick group-name))
              (setq new-group (cons nick new-group)))
            (setq nick-list (cdr nick-list))))
        (setq new-group (reverse new-group))

        (when (or
               (> (length new-group) 1)
               (not (tnt-group-in-blist group-name old-blist)))
          (setq result (cons new-group result))))
      (setq new-blist (cdr new-blist)))

    (reverse result)))

;;; ***************************************************************************
(defun tnt-save-buddy-list (&optional kill-buffer-after-save)
  "Saves a buddy-edit buffer on the host."
  (interactive)
  (when (null tnt-current-user)
    (error "You must be online to save a buddy list"))

  (let* ((new-blist (tnt-buffer-to-blist))
         (old-blist tnt-buddy-blist)
         ;;(add-blist (tnt-grouped-new-buddies old-blist new-blist))
         (del-blist (tnt-grouped-new-buddies new-blist old-blist)))

    ;; Remove deleted buddies and groups.  This must be done with
    ;; multiple commands.
    (while del-blist
      (let* ((group      (car del-blist))
             (group-name (car group))
             (buddies    (cdr group)))

        (when (> (length buddies) 0)
          (toc-remove-buddies group-name buddies))

        (when (not (tnt-group-in-blist group-name new-blist))
          (toc-remove-group group-name))

        (setq del-blist (cdr del-blist))))

    ;; Add new buddies/groups.  This can all be done in one command.
    ;; Rather than building a list of only the new buddies/groups, we
    ;; send the entire list.  Why:
    ;;   * it's easy
    ;;   * the server doesn't seem to mind
    ;;   * deleting all the buddies in a group deletes the group on
    ;;     the server, and we don't want that unless we explictly want
    ;;     it, so this will add it back.
    ;; Only problem is that it's kind of slow to send the entire buddy
    ;; list.  Could change this call to (toc-add-buddies add-blist),
    ;; but need code to deal with the third point listed above.
    (toc-add-buddies new-blist)

    (setq tnt-buddy-blist new-blist))

  (set-buffer-modified-p nil)
  (tnt-backup-buddy-list)

  (if (not kill-buffer-after-save)
      (tnt-build-buddy-buffer)
    (kill-buffer (current-buffer))
    (tnt-show-buddies)))

;;; ***************************************************************************
;;; ***** Buddy-list backup/restore
;;; ***************************************************************************
(defun tnt-backup-or-restore-buddy-list ()
  "If buddy list is empty and backup file exists, restore, otherwise backup."
  (if (tnt-buddy-list-is-empty-p)
      (tnt-restore-buddy-list)
    (tnt-backup-buddy-list))
  (kill-buffer (current-buffer)))

;;; ***************************************************************************
(defun tnt-buddy-list-is-empty-p ()
  "Checks whether buddy list should be considered \"empty\"."
  (let ((num-groups (length tnt-buddy-blist)))
    (or (= num-groups 0)
        (and (= num-groups 1)
             (let ((len (length (car tnt-buddy-blist))))
               (or (<= len 1)
                   (and (= len 2)
                        (string= (caar tnt-buddy-blist) "Buddies")
                        (string= (toc-normalize tnt-username)
                                 (toc-normalize (cadar tnt-buddy-blist)))
                        )))
             ))))

;;; ***************************************************************************
(defun tnt-restore-buddy-list ()
  "Restores the buddy list from the backup file."
  (interactive)
  (if (null tnt-directory)
      (error "Variable tnt-directory undefined")
    (if (not (file-accessible-directory-p tnt-directory))
        (error "Directory %s not accessible" tnt-directory)
      (if (null tnt-buddy-list-backup-filename)
          (error "Variable tnt-buddy-list-backup-filename undefined")

        (let ((filename (format "%s/%s"
                                tnt-directory
                                (format tnt-buddy-list-backup-filename
                                        (toc-normalize tnt-username)))))
          (if (file-exists-p filename)
              (if (not (file-readable-p filename))
                  (error "File %s not readable" filename)
                (tnt-edit-buddies)
                (insert-file-contents filename nil nil nil t)
                (tnt-save-buddy-list)))
          )))))

;;; ***************************************************************************
(defun tnt-backup-buddy-list ()
  "Saves the buddy list to a backup file."
  (interactive)
  (if (null tnt-directory)
      (error "Variable tnt-directory undefined")
    (if (not (file-exists-p tnt-directory))
        (make-directory tnt-directory))
    (if (not (file-accessible-directory-p tnt-directory))
        (error "Directory %s not accessible" tnt-directory)
      (if (null tnt-buddy-list-backup-filename)
          (error "Variable tnt-buddy-list-backup-filename undefined")

        (let* ((filename (format tnt-buddy-list-backup-filename
                                 (toc-normalize tnt-username)))
               (fullpath (format "%s/%s" tnt-directory filename))
               (new-buffer nil)
               (new-buf-len 0)
               (old-buffer nil)
               (old-buf-len 0))

          (if (and (file-exists-p fullpath)
                   (not (file-writable-p fullpath)))
              (error "File %s not writable" filename)
            (tnt-edit-buddies)
            (setq new-buffer (current-buffer)
                  new-buf-len (point-max))
            (find-file fullpath)
            (setq old-buffer (current-buffer)
                  old-buf-len (point-max))

            (if (= (compare-buffer-substrings new-buffer 1 new-buf-len
                                              old-buffer 1 old-buf-len)
                   0)
                (kill-buffer old-buffer)
              (progn
                (kill-buffer old-buffer)
                (switch-to-buffer new-buffer)
                (write-file fullpath)
                ;; note: we used to just rename the buffer after
                ;; writing the file, but then a later call to
                ;; find-file for the backup file will not create a new
                ;; buffer for it, it'll use the edit-buddies buffer
                ;; instead.  so we kill the buffer and re-create it.
                (let ((pos (point)))
                  (kill-buffer (current-buffer))
                  (tnt-edit-buddies)
                  (goto-char pos)))
              )))))))

;;; ***************************************************************************
;;; ***** Buddy utilities
;;; ***************************************************************************
(defun tnt-buffer-to-blist ()
  (save-excursion
    (save-match-data
      (goto-char (point-max))
      (insert "\n")

      (goto-char (point-min))
      (let ((blist nil))
        (while (re-search-forward "\\([ \t]*\\)\\([^\n]*\\)\n" nil t)
          (let ((pref (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
                (body (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
            (goto-char (match-end 0))
            (let ((has-pref (> (length pref) 0))
                  (has-body (string-match "[^ \t]" body)))
              (cond
               ((and has-body has-pref) ; is a buddy
                (setcar blist (cons body (car blist))))
               (has-body                ; is a group
                (setq blist (cons (list body) blist)))))))
        (mapcar 'nreverse (nreverse blist))
        ))))

;;; ***************************************************************************

(defun tnt-config-to-blist (config)
  (setq tnt-permit-list nil)
  (setq tnt-deny-list nil)
  (save-match-data
    (let ((index 0)
          (blist nil))
      (while (and config (string-match "\\([^:]+\\):\\([^\n]*\\)\n" config index))
        (let* ((end (match-end 0))
               (code (match-string 1 config))
               (arg  (match-string 2 config)))
          (cond
           ((string-equal code "g")
            (setq blist (cons (list arg) blist)))
           ((string-equal code "b")
            (setcar blist (cons arg (car blist))))
           ((string-equal code "p")
            (setq tnt-permit-list (cons arg tnt-permit-list)))
           ((string-equal code "d")
            (setq tnt-deny-list (cons arg tnt-deny-list)))
           ((string-equal code "m")
            (setq tnt-permit-mode (string-to-number arg))))
           ;; last line should always be "done:" but that doesn't matter
          (setq index end)))
      (mapcar 'nreverse (nreverse blist))
      )))

;;; ***************************************************************************
(defun tnt-blist-to-config (blist)
  (concat (mapconcat '(lambda (name-list)
                        (concat "g " (car name-list) "\n"
                                (mapconcat '(lambda (b) (concat "b " b "\n"))
                                           (cdr name-list) ""))) blist "")
          (mapconcat '(lambda (d) (concat "d " d "\n")) tnt-deny-list "")
          (mapconcat '(lambda (p) (concat "p " p "\n")) tnt-permit-list "")
          (format "m %d\n" tnt-permit-mode)))

;;; ***************************************************************************
(defun tnt-extract-normalized-buddies (blist)
  (tnt-nsort-and-remove-dups (mapcar 'toc-normalize
                                     (apply 'append (mapcar 'cdr blist)))))

;;; ***************************************************************************

(defun tnt-fullname-for-nick (nick)
  "Returns the fullname (or nil) for the given nickname."
  (car-safe
   (cdr-safe (or (assoc-ignore-case nick tnt-buddy-fullname-alist)
                 (assoc-ignore-case (toc-normalize nick) tnt-buddy-fullname-alist)))
   ))


;;; ***************************************************************************
;;; ***** Pending-event ring
;;; ***************************************************************************
(defun tnt-remove-im-event (nick)
  "Removes an instant message event from the event-ring."
  (interactive)
  (let ((event (assoc (tnt-im-buffer-name nick) tnt-event-ring)))
    (if event (setq tnt-event-ring (delete event tnt-event-ring)))
    (tnt-show-top-event)))

;;; ***************************************************************************
(defun tnt-remove-chat-event (room)
  "Removes a chat event from the event-ring."
  (interactive)
  (let ((event (assoc (tnt-chat-buffer-name room) tnt-event-ring)))
    (if event (setq tnt-event-ring (delete event tnt-event-ring)))
    (tnt-show-top-event)))

;;; ***************************************************************************
(defun tnt-accept ()
  "Accepts an instant message or chat invitation."
  (interactive)
  (tnt-pop-event t))

;;; ***************************************************************************
(defun tnt-reject (warn)
  "Rejects an instant message or chat invitation; warns if prefix arg."
  (interactive "P")
  (tnt-pop-event nil))

;;; ***************************************************************************
(defun tnt-next-event ()
  "Shows the next event in the notification ring."
  (interactive)
  (setq tnt-event-ring (tnt-rotate-right tnt-event-ring))
  (if tnt-event-ring
      (tnt-show-top-event)
    (message "No events in ring.")))

;;; ***************************************************************************
(defun tnt-prev-event ()
  "Show the previous event in the notification ring."
  (interactive)
  (setq tnt-event-ring (tnt-rotate-left tnt-event-ring))
  (if tnt-event-ring
      (tnt-show-top-event)
    (message "No events in ring.")))

;;; ***************************************************************************
(defun tnt-push-event (message buffer-name function)
  ;; Push new event onto the event ring.
  (if (assoc buffer-name tnt-event-ring)
      ()
    (setq tnt-event-ring (cons (cons buffer-name (cons message function))
                               tnt-event-ring))
    (tnt-show-top-event)
    (setq tnt-current-menu 0)
    (tnt-build-buddy-buffer)
    ))

;;; ***************************************************************************
(defun tnt-pop-event (accept)
  ;; Remove the top event from the event ring.
  (if tnt-event-ring
      (let* ((event (car tnt-event-ring))
             (buffer-name (car event))
             (function (cdr (cdr event))))
        (setq tnt-event-ring (cdr tnt-event-ring))
        (if accept
            (progn (switch-to-buffer buffer-name)
                   (if tnt-recenter-windows (recenter -1)))
          (kill-buffer buffer-name))
        (if function (funcall function accept))
        (tnt-show-top-event)
        (tnt-build-buddy-buffer))
    (message "No event to %s." (if accept "accept" "reject"))
    ))

;;; ***************************************************************************
(defun tnt-show-top-event ()
;; Display the message associated with the top event in the minibuffer.
  (if tnt-event-ring
      (let* ((event (car tnt-event-ring))
             (message (car (cdr event))))
        (tnt-persistent-message "%s ('%s' to accept) %s"
                                message
                                (substitute-command-keys "\\[tnt-accept]")
                                (let ((len (length tnt-event-ring)))
                                  (if (= len 1)
                                      ""
                                    (format "[%d more]" (1- len))))))
    (tnt-persistent-message ""))
  (tnt-set-mode-string))

;;; ***************************************************************************
;;; ***** Mode line
;;; ***************************************************************************
(defvar tnt-mode-string "")

;;; ---------------------------------------------------------------------------
(defun tnt-set-online-state (is-online)
  ;; Sets or clears the mode-line online indicator.
  (tnt-set-mode-string)

  (or global-mode-string
      (setq global-mode-string '("")))

  (or (memq 'tnt-mode-string global-mode-string)
      (setq global-mode-string (append global-mode-string '(tnt-mode-string))))

  (force-mode-line-update))

;;; ***************************************************************************
(defun tnt-set-mode-string (&optional update-mode-line)
  ""
  (interactive)
  (setq tnt-mode-string
        (if (and tnt-current-user tnt-mode-indicator)
            (format "  [%s%s%s%s%s]"
                    (if (and tnt-show-events-in-mode
                             (> (length tnt-event-ring) 0)) "*" "")

                    (if (eq tnt-mode-indicator 'nick)
                        tnt-current-user
                      tnt-mode-indicator)

                    (if (and tnt-away tnt-show-away-in-mode)
                        tnt-show-away-in-mode "")
                    (if (and tnt-pipe-to-email-now tnt-show-email-in-mode)
                        tnt-show-email-in-mode "")
                    (if (and tnt-currently-idle tnt-show-idle-in-mode)
                        tnt-show-idle-in-mode ""))
          ""))
  (when update-mode-line (force-mode-line-update)))

;;; ***************************************************************************
;;; ***** Handlers for TOC events
;;; ***************************************************************************
(defun tnt-debug (&rest args)
  "Generic debugging handler.  Used to learn more."
  (message "Got a packet. Look in *tnt-debug* for info.")
  (let ((log-buffer (get-buffer-create "*tnt-debug*")))
    (prin1 args log-buffer)
    (princ "\n" log-buffer)))

;;; ***************************************************************************
(defun tnt-handle-opened ()
  (toc-signon tnt-login-host tnt-login-port tnt-username tnt-password
              tnt-language tnt-version))

;;; ***************************************************************************
(defun tnt-handle-closed ()
  ;; if we just reconnected, don't try to reconnect again
  (if tnt-just-reconnected
      (progn
        (tnt-shutdown)
        (tnt-error "TNT connection closed immediately on reconnect"))

    ;; save these values -- NOTE: must be done before tnt-shutdown
    (setq tnt-reconnecting-away tnt-away)
    (setq tnt-reconnecting-away-msg tnt-away-msg)
    (setq tnt-reconnecting-idle-time (tnt-buddy-idle-secs tnt-current-user))

    ;; reset everything
    (tnt-shutdown)

    ;; if we're forwarding to email, send notification
    (if (and tnt-email-to-pipe-to tnt-pipe-to-email-now)
        (tnt-pipe-message-to-email "TOC-server"
                                   "TNT connection closed by server"))

    ;; beep
    (tnt-beep tnt-beep-on-signoff)

    ;; and finally, attempt to reconnect
    (if tnt-auto-reconnect
        (progn
          (setq tnt-reconnecting t)
          (message (format-time-string
                    (concat tnt-timestamp-format
                            "TNT connection closed, trying to reconnect...")))
          (tnt-open tnt-username tnt-password))
      (message (format-time-string
                (concat tnt-timestamp-format "TNT connection closed")))
      )))

;;; ***************************************************************************
(defun tnt-handle-sign-on (version)
  (message (format-time-string (concat tnt-timestamp-format "Signed on")))
  (tnt-beep tnt-beep-on-signon)
  (if tnt-use-keepalive
      (setq tnt-keepalive-timer
            (tnt-repeat tnt-keepalive-interval 'toc-keepalive)))
  (if tnt-use-buddy-update-timer
      (setq tnt-buddy-update-timer
            (tnt-repeat tnt-buddy-update-interval 'tnt-build-buddy-buffer)))
  (if tnt-timers-available
      (progn
        (setq tnt-login-flag t)
        (setq tnt-login-flag-timer (run-at-time 1 tnt-login-flag-unset-after
                                                'tnt-unset-login-flag))))
  (if tnt-use-idle-timer
      (setq tnt-idle-timer (run-with-idle-timer tnt-send-idle-after t
                                                'tnt-send-idle)))
  )

;;; ***************************************************************************
(defun tnt-handle-config (config)
  (setq tnt-buddy-blist (tnt-config-to-blist config))
  (tnt-backup-or-restore-buddy-list)

  ;; 2005.08.21 gse: not needed in TOC2
  ;;(toc-add-buddies (tnt-extract-normalized-buddies tnt-buddy-blist))
  (cond ((= tnt-permit-mode 1) (toc-permit-all))
        ((= tnt-permit-mode 2) (toc-deny-all))
        ((= tnt-permit-mode 3) (toc-permit-only tnt-permit-list))
        ((= tnt-permit-mode 4) (toc-deny-only tnt-deny-list))
        (t (error "Bad permit mode %s" tnt-permit-mode)))
  (toc-init-done)
  )

;;; ***************************************************************************
(defun tnt-handle-nick (nick)
;; This should fix the truncation of nickname problem we've been having.
  (if (string= nick tnt-username)
      (setq tnt-current-user nick)
    (setq tnt-current-user tnt-username))

  (or tnt-buddy-blist
      (setq tnt-buddy-blist (list (list "Buddies" nick))))

  (tnt-set-online-state t)

  (if tnt-reconnecting
      (progn
        (setq tnt-reconnecting nil)

        (if tnt-reconnecting-away
            (tnt-set-away tnt-reconnecting-away-msg))
        (if tnt-reconnecting-idle-time
            (tnt-send-idle tnt-reconnecting-idle-time))

        (setq tnt-reconnecting-away nil)
        (setq tnt-reconnecting-away-msg nil)
        (setq tnt-reconnecting-idle-time nil)

        (if (and tnt-email-to-pipe-to tnt-pipe-to-email-now)
            (tnt-pipe-message-to-email "TOC-server"
                                       "TNT successfully reconnected"))
        (if tnt-timers-available
            (progn
              (setq tnt-just-reconnected t)
              (run-at-time tnt-just-reconnected-unset-after nil
                           'tnt-unset-just-reconnected)))
        )
    (setq tnt-show-inactive-buddies-now tnt-show-inactive-buddies)
    (tnt-show-buddies)))

;;; ***************************************************************************
(defun tnt-handle-im-in (user auto message)
  (let* ((buffer-exists (get-buffer (tnt-im-buffer-name user)))
         (buffer (tnt-im-buffer user))
         (fullname (tnt-get-fullname-or-nick user)))

    (tnt-append-message-and-adjust-window
     buffer message user (if auto "(Auto-response)"))

    (if (and tnt-email-to-pipe-to
             tnt-pipe-to-email-now)
        (tnt-pipe-message-to-email user message))

    (if (get-buffer-window buffer 'visible)
        (progn
          (tnt-beep tnt-beep-on-visible-incoming-message)
          (tnt-remove-im-event user))

      ;; Buffer is not visible.  If it didn't exist, this is a "first"
      ;; message from that user.  Check to see if first message sound
      ;; is non-nil and use it if so.
      (if (and (not buffer-exists) tnt-beep-on-first-incoming-message)
          (tnt-beep tnt-beep-on-first-incoming-message)
        (tnt-beep tnt-beep-on-incoming-message))

      (tnt-push-event (format "Message from %s available" fullname)
                      (tnt-im-buffer-name user) nil))

    (if tnt-away (tnt-send-away-msg user))

    (when tnt-receive-typing-notifications
      (tnt-handle-client-events user "0"))
    ))

;;; ***************************************************************************
(defun tnt-toggle-email ()
  "Turns email piping on or off (only if tnt-email-to-pipe-to is set)."
  (interactive)
  (when (null tnt-email-to-pipe-to)
    (error "No email address set in variable tnt-email-to-pipe-to"))

  (setq tnt-pipe-to-email-now (not tnt-pipe-to-email-now))
  (if tnt-pipe-to-email-now
      (message (format "Now forwarding any incoming IMs to %s"
                       tnt-email-to-pipe-to))
    (message (format "No longer forwarding incoming IMs")))

  (tnt-set-online-state t)
  (tnt-build-buddy-buffer)
  )

;;; ***************************************************************************
(defun tnt-pipe-message-to-email (user message)
  (let* ((proc-name "piping-process")
         (proc-out-buf "*piping-program-output*")
         (process-connection-type nil)
         (start-process-args
          (list proc-name proc-out-buf
                ;; the executable:
                tnt-email-binary
                ;; and then any cmd-line args:
                tnt-email-to-pipe-to ;; email address to send to
                ;; and in this case, we conditionally add further args below
                ))
         (formatted-message
          (if tnt-email-include-user-in-body
              (format "%s: %s\n" user (tnt-reformat-text message))
            (format "%s\n" (tnt-reformat-text message)))))

    (if tnt-email-from-domain
        (let* ((nuser (toc-normalize user))
               (email-from-header (format "From: %s_IM_@%s" nuser
                                          tnt-email-from-domain)))
          (setq start-process-args (append start-process-args
                                           (list "-a" ;; additional header
                                                 email-from-header)))))

    (if tnt-email-use-subject
        (let ((email-subject (format "IM from %s" user)))
          (setq start-process-args (append start-process-args
                                           (list "-s" ;; subject arg
                                                 email-subject)))))

    ;; start the process, then pipe in the message and EOF
    (apply 'start-process start-process-args)
    (process-send-string proc-name formatted-message)
    (process-send-eof proc-name)
    (message "Reminder: IMs are being forwarded to %s" tnt-email-to-pipe-to)))

;;; ***************************************************************************
(defun tnt-handle-update-buddy (nick online evil signon idle away)
;; The modes for this section are listed in protocol, but here they are.
  ;; " U"  == Oscar Trial/Available.
  ;; " UU" == Oscar Trial/Away
  ;; " OU" == Oscar/Away
  ;; " O"  == Oscar/Available
;; " AU" == Aol/Oscar Trial/Here (the reason for U being here here is unknown)
  ;; " A"  == Aol/Here
  (if (or (string= away " UU")
          (string= away " OU")
          )
      (tnt-set-buddy-status nick online idle t)
    (tnt-set-buddy-status nick online idle nil))
  (if online
      (tnt-send-pounce (toc-normalize nick)))
  )

;;; ***************************************************************************
(defun tnt-handle-error (code args)
  (cond
   ((= code 901)
    (tnt-error "User %s not online" (car args)))
   ((= code 902)
    (tnt-error "Warning of %s is not allowed" (car args)))
   ((= code 903)
    (tnt-error "Message dropped - you are sending too fast"))
   ((= code 950)
    (tnt-error "Chat room %s is not available" (car args)))
   ((= code 960)
    (tnt-error "Message dropped - sending too fast for %s" (car args)))
   ((= code 961)
    (tnt-error "Message from %s dropped - too big" (car args)))
   ((= code 962)
    (tnt-error "Message from %s dropped - sent too fast" (car args)))
   ((= code 970)
    (tnt-error "Failure"))
   ((= code 971)
    (tnt-error "Too many matches"))
   ((= code 972)
    (tnt-error "Need more qualifiers"))
   ((= code 973)
    (tnt-error "Dir service temporarily unavailable"))
   ((= code 974)
    (tnt-error "Email lookup restricted"))
   ((= code 975)
    (tnt-error "Keyword ignored"))
   ((= code 976)
    (tnt-error "No keywords"))
   ((= code 977)
    (tnt-error "User has no dir info"))
   ((= code 978)
    (tnt-error "Country not supported"))
   ((= code 979)
    (tnt-error "Failure unknown: %s" (car args)))
   ((= code 980)
    (tnt-error "Incorrect screen name or password"))
   ((= code 981)
    (tnt-error "Service temporarily unavailable"))
   ((= code 982)
    (tnt-error "Your warning level is currently too high to log in."))
   ((= code 983)
    (tnt-error "You have been logging in and out too rapidly.  Wait ten minutes or longer."))
   ((= code 989)
    (tnt-error "An unknown signon error occured: %s" (car args)))
   (t (tnt-error "Unknown error %d:%S" code args)))

  ;; if the error was in signing on, we need to delete the process so
  ;; we don't keep trying to log in
  (if (and (>= code 980) (< code 990)) (toc-close))
  )

;;; ***************************************************************************
(defun tnt-handle-eviled (amount eviler)
  (message "You have been warned %s (%d)."
           (if (equal eviler "")
               "anonymously"
             (concat "by " eviler))
           amount))

;;; ***************************************************************************
(defun tnt-handle-chat-join (roomid room)
  (with-current-buffer (tnt-chat-buffer room)
    (setq tnt-chat-roomid roomid))
  (setq tnt-chat-alist (tnt-addassoc roomid room tnt-chat-alist)))

;;; ***************************************************************************
(defun tnt-handle-chat-in (roomid user whisperp message)
  (let* ((room-name (cdr (assoc roomid tnt-chat-alist)))
         (buffer (tnt-chat-buffer room-name))
         (buffer-name (tnt-chat-buffer-name room-name)))
    (tnt-append-message-and-adjust-window
     buffer message user (if whisperp "whispers"))

    ;; Beep/push message event if appropriate.
    (if (and tnt-message-on-chatroom-message
             (not (string-equal user tnt-current-user)))
        (if (get-buffer-window buffer 'visible)
            (progn
              (tnt-beep tnt-beep-on-visible-chat-message)
              (tnt-remove-chat-event room-name))
          (progn
            (tnt-beep tnt-beep-on-chat-message)
            (tnt-push-event (format "Chat message from %s available" user)
                            buffer-name nil))))))

;;; ***************************************************************************
(defun tnt-handle-chat-update-buddy (roomid inside users)
  (with-current-buffer (tnt-chat-buffer (cdr (assoc roomid tnt-chat-alist)))
    (let ((user-string (mapconcat 'tnt-get-fullname-and-nick users ", ")))
      (tnt-append-message (if tnt-chat-participants
                              (format "%s %s"
                                      user-string (if inside "joined" "left"))
                            (format "Participants: %s" user-string))))
    (if inside
        (setq tnt-chat-participants (append users tnt-chat-participants))
      (while users
        (let ((user (car users)))
          (setq tnt-chat-participants (delete user tnt-chat-participants))
          (setq users (cdr users)))))))

;;; ***************************************************************************
(defun tnt-handle-chat-invite (room roomid sender message)
  (tnt-handle-chat-join roomid room)    ; associate roomid with room
  (let ((buffer (tnt-chat-buffer room))
        (buffer-name (tnt-chat-buffer-name room)))
    (with-current-buffer buffer
      (tnt-append-message (tnt-reformat-text message) sender "invitation"))
    (tnt-push-event (format "Chat invitation from %s arrived" sender)
                    buffer-name 'tnt-chat-event-pop-function)
    (tnt-beep tnt-beep-on-chat-invitation)
    ))

;;; ***************************************************************************
(defun tnt-handle-goto-url (windowid url)
  (setq url (concat "http://" tnt-toc-host "/" url))
  (browse-url url))

;;; ***************************************************************************
(defun tnt-handle-client-events (buddy event)
  ""
  (when tnt-receive-typing-notifications
    (let ((buffer-name (tnt-im-buffer-name buddy))
          (nick (tnt-get-fullname-or-nick buddy))
          event-str short-str)
      (cond ((string= event "1")
             (setq event-str (concat "[TNT] " nick " has entered text")
                   short-str " *paused*"))
            ((string= event "2")
             (setq event-str (concat "[TNT] " nick " is typing...")
                   short-str " *typing*"))
            ((string= event "0")
             (setq event-str nil
                   short-str nil))
            (t
             (setq event-str (concat "[TNT] Unknown event: <" event "> from "
                                     (tnt-get-fullname-and-nick buddy))
                   short-str (concat " *UNKNOWN EVENT:: <" event "> from "
                                     (tnt-get-fullname-and-nick buddy) "*")))
            )
      (when (memq 'message tnt-receive-typing-notifications)
        (message event-str))
      (when (and (memq 'mode-line tnt-receive-typing-notifications)
                 (get-buffer buffer-name))
        (with-current-buffer (tnt-im-buffer buddy)
          (setq mode-line-process short-str)))
      )))

;;; ***************************************************************************
;;; ***** Minibuffer utilities
;;; ***************************************************************************
(defun tnt-read-from-minibuffer-no-echo (prompt)
  ;; Reads a string from the minibuffer without echoing it.
  (let ((keymap (make-keymap))
        (i ? ))
    (while (<= i 126)
      (define-key keymap (char-to-string i)
        '(lambda ()
           (interactive)
           (insert last-command-char)
           (put-text-property (1- (point)) (point) 'invisible t)))
      (setq i (1+ i)))
    (define-key keymap "\r" 'exit-minibuffer)
    (define-key keymap "\C-g" 'keyboard-escape-quit)
    (let ((str (read-from-minibuffer prompt "" keymap)))
      (set-text-properties 0 (length str) nil str)
      str)))

;;; ***************************************************************************
(defun tnt-completing-read-list (prompt collection &optional initial-input)
  "Reads a list from the minibuffer with completion for each element
of the list, delimited by commas."
  (let* ((initial-input-str (mapconcat 'identity initial-input ", "))
         (str (completing-read prompt 'tnt-completion-func
                               nil nil initial-input-str)))
    (split-string str ",")))

;;; ***************************************************************************
(defvar tnt-persistent-message-disable-id nil)

;;; ---------------------------------------------------------------------------
(defun tnt-persistent-message-persist (m)
  (when (and tnt-persistent-timeout (> tnt-persistent-timeout 0))
    (when (and tnt-beep-on-persistent-messages
               tnt-persistent-message-disable-id)
      (tnt-beep tnt-beep-on-persistent-messages))
    (setq tnt-persistent-message-disable-id
          (add-timeout tnt-persistent-timeout 'tnt-persistent-message-persist m)))
  (message m))

;;; ***************************************************************************
(defun tnt-persistent-message (&optional fmt &rest args)
  (when tnt-persistent-message-disable-id
    (disable-timeout tnt-persistent-message-disable-id)
    (setq tnt-persistent-message-disable-id nil))
                                        ;never more than one!
  (when (and fmt (not (equal fmt "")))
    (tnt-persistent-message-persist (apply 'format fmt args))))

;;; ***************************************************************************
(defun tnt-error (&rest args)
  ;; Displays message in echo area and beeps.  Use this instead
  ;; of (error) for asynchronous errors.
  (apply 'message args)
  (tnt-beep tnt-beep-on-error))

;;; ***************************************************************************
(defvar collection)                     ; to shut up byte compiler

(defun tnt-completion-func (str pred flag)
;; Minibuffer completion function that allows lists of comma-separated
 ;; item to be entered, with completion applying to each item.  Before
;; calling, bind COLLECTION to the collection to be used for completion.
  (let* ((str-pos (save-match-data (string-match "[^,]*$" str)))
         (first-part (substring str 0 str-pos))
         (last-word (substring str str-pos)))
    (cond
     ((eq flag nil)
      (let ((completion (try-completion last-word collection pred)))
        (if (stringp completion) (concat first-part completion) completion)))
     ((eq flag t)
      (all-completions last-word collection pred)))))

;;; ***************************************************************************
;;; ***** Beep/Sound support
;;; ***************************************************************************

(defvar tnt-muted nil)

(defun tnt-play-sound (sound-file)
  "On non-XEmacs systems, requires that tnt-sound-exec be set."
  (if (file-readable-p sound-file)
      (cond
       (tnt-sound-exec
        (with-output-to-string
          (call-process shell-file-name nil t nil shell-command-switch (concat tnt-sound-exec " " tnt-sound-exec-args " " sound-file))))
       (tnt-running-xemacs (play-sound-file sound-file))
       (t (message "Warning: tnt-sound-exec is not set")))
  (message "Warning: %s is not a readable file" sound-file)))

;;; ***************************************************************************
(defun tnt-beep (beep-type)
  ;; beep-type      action
  ;; ---------      ------
  ;; nil            no beep
  ;; 'visible       visible beep
  ;; 'audible       audible beep
  ;; 'current       whichever emacs is currently set to
  ;; filename       audio file to play
  ;;
  ;; Also have to check tnt-muted.
  (if (not tnt-muted)
      (cond
       ((or (eq beep-type 'visible) (eq beep-type 'audible))
        (let ((orig-visible visible-bell))
          (setq visible-bell (eq beep-type 'visible))
          (beep)
          (setq visible-bell orig-visible)))
       ((eq beep-type 'current)
        (beep))
       ((stringp beep-type)
        (tnt-play-sound beep-type)))))

;;; ***************************************************************************
(defun tnt-toggle-mute ()
  "Toggles muting of all TNT sounds."
  (interactive)
  (setq tnt-muted (not tnt-muted))
  (if tnt-muted
      (message "All TNT sounds muted.")
    (message "TNT sound on."))
  (tnt-build-buddy-buffer))

;;; ***************************************************************************
(defun tnt-read-string-with-default (p d)
  (let ((reply (read-string (format "%s (%s): " p d))))
    (if (equal reply "")
        d
      reply)))

;;; ***************************************************************************
;;; ***** String list utilities
;;; ***************************************************************************
(defun tnt-sorted-list-diff (old-list new-list)
  ;; Compares OLD-LIST and NEW-LIST.  Returns a cons of whose car is a
  ;; list of deletions and whose cdr is a list of insertions.
  (let ((insert-list nil)
        (delete-list nil))
    (while (or old-list new-list)
      (let ((old-item (car old-list))
            (new-item (car new-list)))
        (cond
         ((or (null new-item)
              (and old-item (string< old-item new-item)))
          (setq delete-list (cons old-item delete-list))
          (setq old-list (cdr old-list)))
         ((or (null old-item)
              (and new-item (string< new-item old-item)))
          (setq insert-list (cons new-item insert-list))
          (setq new-list (cdr new-list)))
         (t
          (setq new-list (cdr new-list))
          (setq old-list (cdr old-list))))))
    (cons delete-list insert-list)))

;;; ***************************************************************************
(defun tnt-nsort-and-remove-dups (list)
;; Sorts LIST into alphabetical order and removes duplicates.  Returns
  ;; the sorted list.  The original list is modified in the process.
  (setq list (sort list 'string<))
  (let ((p list))
    (while p
      (while (and (cdr p) (string= (car p) (car (cdr p))))
        (setcdr p (cdr (cdr p))))
      (setq p (cdr p))))
  list)

;;; ***************************************************************************
;;; *****  Typing Notifications (new in TOC2)
;;; ***************************************************************************
(defvar tnt-typing-notification-timer nil)
(make-variable-buffer-local 'tnt-typing-notification-timer)

(defvar tnt-typing-notification-state nil
  "nil = last event is [0,1], non-nil = last event is [2]")
(make-variable-buffer-local 'tnt-typing-notification-state)

(defvar tnt-inhibit-typing-notifications nil)
(make-variable-buffer-local 'tnt-inhibit-typing-notifications)

;;; ***************************************************************************
(defun tnt-send-typing-notification (buddy event)
  "Sends the given typing EVENT to buddy BUDDY.

EVENT should be one of:

\"0\" : Typing has been erased
\"1\" : Typing has paused
\"2\" : Typing has started"
  (save-excursion
    (toc-send-typing-status buddy event)))

;;; ***************************************************************************
(defun tnt-typing-notification-hook (&rest ignore)
  "Called from `after-change-functions'.

Decides which event to send (0, 2 or none) and sets timer to
possibly send pause event (1)."
  (unless tnt-inhibit-typing-notifications
    (let ((message (buffer-substring-no-properties tnt-message-marker (point-max))))
      ;; cancel existing timer, if any
      (when tnt-typing-notification-timer
        (cancel-timer tnt-typing-notification-timer)
        (setq tnt-typing-notification-timer nil))

      (if (= (length message) 0)
          ;; message has been erased
          (progn
            (tnt-send-typing-notification tnt-im-user 0)
            (setq tnt-typing-notification-state nil))

        ;; already sent typing state?
        (when (not tnt-typing-notification-state)
          (tnt-send-typing-notification tnt-im-user 2)
          (setq tnt-typing-notification-state t))

        ;; set timer to run

        ;; Note: XEmacs itimers has a bug that runs the callback
        ;; immediately, in addition to after 'n' seconds.  Nothing to
        ;; be done about it for the moment
        (setq tnt-typing-notification-timer
              (run-at-time tnt-typing-notications-idle-time nil
                           'tnt-typing-notification-callback
                           (current-buffer))))
      )))

;;; ***************************************************************************
(defun tnt-typing-notification-callback (im-buffer)
  "Timer callback to send 'pause' event (1).

IM-BUFFER is the buffer that the timer is meant for."
  (with-current-buffer im-buffer
    (setq tnt-typing-notification-timer nil
          tnt-typing-notification-state nil)
    (tnt-send-typing-notification tnt-im-user 1)
    ))

;;; ***************************************************************************
(defun tnt-typing-notification-kill-hook ()
  "Called from `kill-buffer-hook'.

Sends a clear event (0) and cancels any pending timer."
  (tnt-send-typing-notification tnt-im-user 0)
  (when tnt-typing-notification-timer
    (cancel-timer tnt-typing-notification-timer)
    (setq tnt-typing-notification-timer nil)))

;;; ***************************************************************************
;;; ***** String utilities
;;; ***************************************************************************
(defvar tnt-html-tags-to-strip
  ;; could be defcustom??
  (concat "</?HTML>\\|"
          "<BODY[^>]*>\\|</BODY>\\|"
          "<FONT[^>]*>\\|</FONT>\\|"
          "<PRE>\\|</PRE>\\|"
          "</?[Uu]>\\|"
          "</A>"))

;;; ***************************************************************************
(defvar tnt-html-regexps-to-replace
  ;; could be defcustom??
  (list
   '("<BR>\\|<br>\\|<br />" "\n")
   '("</?[Ii]>"    "_")
   '("</?[Bb]>"    "*")
        ;; these must be after any html tags (which have "<" and ">"):
        '("&lt;" "<")
        '("&gt;" ">")
        '("&quot;" "\"")
        ;; and this must be after any escape sequences which have "&":
        '("&amp;" "&")
        ))

;;; ***************************************************************************
;; for example, you might put in your .emacs (or wherever):
;;    (tnt-add-html-tag-to-strip "<B>\\|</B>")
(defun tnt-add-html-tag-to-strip (str)
  (setq tnt-html-tags-to-strip
        (concat tnt-html-tags-to-strip "\\|" str)))

;;; ***************************************************************************
;; for example, you might put in your .emacs (or wherever):
;;    (tnt-add-html-regexp-to-replace "<U>\\|</U>" "_")
(defun tnt-add-html-regexp-to-replace (replace-regexp replace-with)
  (setq tnt-html-regexps-to-replace
        (cons (list replace-regexp replace-with)
              tnt-html-regexps-to-replace)))

;;; ***************************************************************************
(defun tnt-strip-a-href (str)
  ;; Replaces the substring
  ;; <a href="http://www.derf.net/">derf!
  ;; with
  ;; ( http://www.derf.net/ ) derf!
  ;; which will not get stripped out by tnt-strip-html
  (save-match-data
    (let ((start-index 0)
          end-index the-url the-url-no-scheme
          (segs nil))
      (while (setq end-index (string-match "<a href=\"" str start-index))
        ;; skip past "<a href=\""
        (setq segs (cons (substring str start-index end-index) segs))
        (setq start-index (match-end 0))
        (setq end-index (string-match "\"" str start-index))
        ;; get stuff up to "\""
        (if (null end-index) nil
          (setq the-url (substring str start-index end-index))
          (setq start-index (match-end 0))
          (let ((first-seven (substring the-url 0 7)))
            (if (or (string= "http://" first-seven)
                    (string= "mailto:" first-seven))
                (setq the-url-no-scheme (substring the-url 7))))
          )
        ;; skip past ">"
        (setq end-index (string-match ">" str start-index))
        (if (null end-index) nil
          (setq start-index (match-end 0)))
        ;; if the link url is the same as the link text,
        ;; we don't need to see it twice
        (if (or (and the-url
                     (string-match (tnt-backslashify-string the-url)
                                   str start-index))
                (and the-url-no-scheme
                     (string-match (tnt-backslashify-string the-url-no-scheme)
                                   str start-index)))
            nil
          (setq segs (cons (format "( %s ) " the-url) segs)))
        )
      (setq segs (cons (substring str start-index) segs))
      (apply 'concat (nreverse segs))
      )))

;;; ***************************************************************************
(defun tnt-backslashify-string (str)
  ;; adds "\" chars as necessary to enable matching the given string
  ;; exactly.
  (save-match-data
    (let ((chars-regexp "[.+?]")
          (start-index 0)
          end-index
          (segs nil))
      (while (setq end-index (string-match chars-regexp str start-index))
        (setq segs (cons (substring str start-index end-index) segs))
        (setq segs (cons "\\" segs))
        (setq start-index (match-end 0))
        (setq segs (cons (substring str end-index start-index) segs))
        )
      (setq segs (cons (substring str start-index) segs))
      (apply 'concat (nreverse segs))
      )))

;;; ***************************************************************************
(defun tnt-strip-html (str)
  ;; Strips all HTML tags out of STR.
  (save-match-data
    (let ((start-index 0)
          end-index
          (segs nil))
      (while (setq end-index (string-match "<[^ ][^>]*>" str start-index))
        (setq segs (cons (substring str start-index end-index) segs))
        (setq start-index (match-end 0)))
      (setq segs (cons (substring str start-index) segs))
      (apply 'concat (nreverse segs))
      )))

;;; ***************************************************************************
(defun tnt-strip-some-html (str regexp-and-replace)
  ;; strips out all occurrances of the given regexp, and replaces with
  ;; the given replace string.  the regexp and replace strings are two
  ;; elements of a list (rather than being two separate params) so
  ;; that we can use "reduce" in tnt-reformat-text, below.
  (save-match-data
    (let ((start-index 0)
          end-index
          (segs nil)
          (replace-regexp (car regexp-and-replace))
          (replace-with (cadr regexp-and-replace))
          )
      (while (setq end-index (string-match replace-regexp str start-index))
        (setq segs (cons replace-with
                         (cons (substring str start-index end-index)
                               segs)))
        (setq start-index (match-end 0)))
      (setq segs (cons (substring str start-index) segs))
      (apply 'concat (nreverse segs))
      )))

;;; ***************************************************************************
(defun tnt-neliminate-newlines (str)
  ;; Converts newlines and carriage returns to spaces.  Modifies STR.
  (let ((pos 0)
        (len (length str)))
    (while (< pos len)
      (if (or (= (aref str pos) ?\n)
              (= (aref str pos) ?\r))
          (aset str pos ? ))
      (setq pos (1+ pos)))
    str))

;;; ***************************************************************************
(defun tnt-reformat-text (str)
  ;; calls tnt-strip-some-html repeatedly with different substitutions
  (tnt-strip-a-href
   (reduce 'tnt-strip-some-html tnt-html-regexps-to-replace
           :initial-value
           (tnt-strip-some-html str
                                (list tnt-html-tags-to-strip "")
                                ))))

;;; ***************************************************************************
(defun tnt-repeat (interval function)
  (run-at-time interval interval function))

;;; ***************************************************************************
;;; ***** List utilities
;;; ***************************************************************************
(defun tnt-rotate-left (l)
  "Moves the first element of L to the end, destructively."
  (if l (nconc (cdr l) (list (car l)))))

;;; ***************************************************************************
(defun tnt-rotate-right (l)
  "Moves the last element of L to the front destructively."
  (nreverse (tnt-rotate-left (nreverse l))))

;;; ***************************************************************************
(defun tnt-addassoc (key value alist)
  "Add an association between KEY and VALUE to ALIST and return the new alist."
  (let ((pair (assoc key alist)))
    (if (null pair)
        (cons (cons key value) alist)
      (setcdr pair value)
      alist)))

;;; ***************************************************************************
(defun tnt-remassoc (key alist)
  "Remove an association KEY from ALIST, and return the new ALIST."
  (delete (assoc key alist) alist))

;;; ***************************************************************************
;;; ***** debugging
;;; ***************************************************************************
(defun tnt-turn-on-debugging ()
  (interactive)
  (tnt-affect-debugging 'add-hook))

;;; ***************************************************************************
(defun tnt-turn-off-debugging ()
  (interactive)
  (tnt-affect-debugging 'remove-hook))

;;; ***************************************************************************
(defun tnt-affect-debugging (f)
  (funcall f 'toc-opened-hooks 'tnt-debug)
  (funcall f 'toc-closed-hooks 'tnt-debug)
  (funcall f 'toc-sign-on-hooks 'tnt-debug)
  (funcall f 'toc-config-hooks 'tnt-debug)
  (funcall f 'toc-nick-hooks 'tnt-debug)
  (funcall f 'toc-im-in-hooks 'tnt-debug)
  (funcall f 'toc-update-buddy-hooks 'tnt-debug)
  (funcall f 'toc-error-hooks 'tnt-debug)
  (funcall f 'toc-eviled-hooks 'tnt-debug)
  (funcall f 'toc-chat-join-hooks 'tnt-debug)
  (funcall f 'toc-chat-in-hooks 'tnt-debug)
  (funcall f 'toc-chat-update-buddy-hooks 'tnt-debug)
  (funcall f 'toc-chat-invite-hooks 'tnt-debug)
  (funcall f 'toc-chat-left-hooks 'tnt-debug)
  (funcall f 'toc-goto-url-hooks 'tnt-debug)
  (funcall f 'toc-pause-hooks 'tnt-debug))

;;; **************************************************************************
;;; ***** we're done
;;; **************************************************************************
(provide 'tnt)
(run-hooks 'tnt-load-hook)

;;; tnt.el ends here
;;; **************************************************************************
;;;; *****  EOF  *****  EOF  *****  EOF  *****  EOF  *****  EOF  *************
