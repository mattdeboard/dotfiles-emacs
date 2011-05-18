<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: rcirc-notify+.el</title><link rel="alternate" type="application/wiki" title="Edit this page" href="http://www.emacswiki.org/emacs?action=edit;id=rcirc-notify%2b.el" /><link type="text/css" rel="stylesheet" href="/emacs/wiki.css" /><meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="http://www.emacswiki.org/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: rcirc-notify+.el" href="http://www.emacswiki.org/emacs?action=rss;rcidonly=rcirc-notify%2b.el" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content"
      href="http://www.emacswiki.org/emacs/full.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content and diff"
      href="http://www.emacswiki.org/emacs/full-diff.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki including minor differences"
      href="http://www.emacswiki.org/emacs/minor-edits.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Changes for rcirc-notify+.el only"
      href="http://www.emacswiki.org/emacs?action=rss;rcidonly=rcirc-notify%2b.el" />
<script type="text/javascript">
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-2101513-1']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
</script>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/></head><body class="http://www.emacswiki.org/emacs"><div class="header"><a class="logo" href="http://www.emacswiki.org/emacs/SiteMap"><img class="logo" src="/emacs_logo.png" alt="[Home]" /></a><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span>
<!-- Google CSE Search Box Begins  -->
<form class="tiny" action="http://www.google.com/cse" id="searchbox_004774160799092323420:6-ff2s0o6yi"><p>
<input type="hidden" name="cx" value="004774160799092323420:6-ff2s0o6yi" />
<input type="text" name="q" size="25" />
<input type="submit" name="sa" value="Search" />
</p></form>
<script type="text/javascript" src="http://www.google.com/coop/cse/brand?form=searchbox_004774160799092323420%3A6-ff2s0o6yi"></script>
<!-- Google CSE Search Box Ends -->
<h1><a title="Click to search for references to this page" rel="nofollow" href="http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&amp;q=%22rcirc-notify%2b.el%22">rcirc-notify+.el</a></h1></div><div class="wrapper"><div class="content browse"><p class="download"><a href="download/rcirc-notify%2b.el">Download</a></p><pre class="code"><span class="linecomment">;;; rcirc-notify+.el --- Rcirc notify library</span>

<span class="linecomment">;; Author: Andy Stewart &lt;lazycat.manatee@gmail.com&gt;</span>
<span class="linecomment">;; Maintainer: Andy Stewart &lt;lazycat.manatee@gmail.com&gt;</span>
<span class="linecomment">;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.</span>
<span class="linecomment">;; Created: 2008-06-08 12:45:24</span>
<span class="linecomment">;; Version: 1.0</span>
<span class="linecomment">;; Last-Updated: 2008-06-08 12:45:27</span>
<span class="linecomment">;; URL:  http://www.emacswiki.org/emacs/download/rcirc-notify+.el</span>
<span class="linecomment">;; Keywords: rcirc, notify</span>
<span class="linecomment">;; Compatibility: GNU Emacs 23.0.60.1</span>

<span class="linecomment">;; This program is free software; you can redistribute it and/or modify</span>
<span class="linecomment">;; it under the terms of the GNU General Public License as published by</span>
<span class="linecomment">;; the Free Software Foundation; either version 3, or (at your option)</span>
<span class="linecomment">;; any later version.</span>

<span class="linecomment">;; This program is distributed in the hope that it will be useful,</span>
<span class="linecomment">;; but WITHOUT ANY WARRANTY; without even the implied warranty of</span>
<span class="linecomment">;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</span>
<span class="linecomment">;; GNU General Public License for more details.</span>

<span class="linecomment">;; You should have received a copy of the GNU General Public License</span>
<span class="linecomment">;; along with this program; see the file COPYING.  If not, write to</span>
<span class="linecomment">;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth</span>
<span class="linecomment">;; Floor, Boston, MA 02110-1301, USA.</span>

<span class="linecomment">;; Features that might be required by this library:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;  `rcirc'</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; Commentary:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Notify popup for rcirc</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This extension use `notify-send' for notify.</span>
<span class="linecomment">;; So make you have install `notify-send' in your system.</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; Installation:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Copy rcirc-notify+.el to your load-path and add to your ~/.emacs</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;  (require 'rcirc-notify+)</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Rcirc will notify you automatically when have a message is reach, blow is open</span>
<span class="linecomment">;; rcirc notify switcher:</span>
<span class="linecomment">;;  (setq rcirc-notify+-open t)</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Little tips:</span>
<span class="linecomment">;;  Function 'rcirc-notify+-jump-last-message-channel' can jump last channel that</span>
<span class="linecomment">;;  message notify you.</span>
<span class="linecomment">;;  And feel free to binding it to you like. ^_^</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; Change log:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 2008/06/08</span>
<span class="linecomment">;;         First release.</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; Acknowledgments:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;      Will Farrington     for rcirc-notify+.el</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; TODO</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; None</span>
<span class="linecomment">;;</span>

<span class="linecomment">;; Require</span>
(require 'rcirc)

<span class="linecomment">;;; Code:</span>
(defgroup rcirc-notify+ nil
  "<span class="quote">Notify popup for ircirc.</span>"
  :group 'rcirc)

(defcustom rcirc-notify+-open t
  "<span class="quote">The switcher that notify me.</span>"
  :type 'boolean
  :group 'rcirc-notify+)

(defcustom rcirc-notify+-delay 1
  "<span class="quote">Number of seconds that will elapse between notifications from the same person.</span>"
  :type 'integer
  :group 'rcirc-notify+)

(defcustom rcirc-notify+-cmd "<span class="quote">notify-send</span>"
  "<span class="quote">The command that use for notify.</span>"
  :type 'string
  :group 'rcirc-notify+)

(defcustom rcirc-notify+-icon "<span class="quote">~/MyEmacs/Image/Irc.png</span>"
  "<span class="quote">Specifies an icon filename or stock icon to display.</span>"
  :type 'string
  :group 'rcirc-notify+)

(defcustom rcirc-notify+-timeout 10000
  "<span class="quote">Specifies the timeout in milliseconds at which to expire the notification.</span>"
  :type 'number
  :group 'rcirc-notify+)

(defcustom rcirc-notify+-urgency "<span class="quote">low</span>"
  "<span class="quote">Specifies the urgency level (low, normal, critical).</span>"
  :type 'string
  :group 'rcirc-notify+)

(defcustom rcirc-notify+-category "<span class="quote">im.received</span>"
  "<span class="quote">Specifies the notification category.</span>"
  :type 'string
  :group 'rcirc-notify+)

(defvar rcirc-notify+-nick-alist nil
  "<span class="quote">An alist of nicks and the last time they tried to trigger a notification.</span>")

(defvar rcirc-last-position nil
  "<span class="quote">The last message position in rcirc buffers.</span>")

(defun rcirc-notify+ (sender &optional private)
  "<span class="quote">Rcirc notify.</span>"
  (interactive)
  (let ((last-channel nil)
        (last-server nil))
    (when (and rcirc-notify+-open       <span class="linecomment">;if notify switcher is open</span>
               rcirc-target)            <span class="linecomment">;if is a null channel (ignore the first message from server)</span>
      (setq last-channel rcirc-target)  <span class="linecomment">;get channel name or use name (from private message)</span>
      (setq last-server (with-rcirc-server-buffer rcirc-server-name)) <span class="linecomment">;get random server name</span>
      (string-match "<span class="quote">^[^.]*</span>" last-server) <span class="linecomment">;replace random server name use string @irc</span>
      (setq last-server (replace-match "<span class="quote">@irc</span>" nil nil last-server 0))
      (setq rcirc-last-position (concat last-channel last-server)) <span class="linecomment">;general irc buffer name that last message to me</span>
      (if private
          (rcirc-notify+-popup (format "<span class="quote">%s send a private message.</span>" sender))
        (rcirc-notify+-popup (format "<span class="quote">%s send message.</span>" sender))))))

(defun rcirc-notify+-popup (msg)
  "<span class="quote">Popup notify window.</span>"
  (shell-command (concat rcirc-notify+-cmd
                         "<span class="quote"> -i </span>" rcirc-notify+-icon
                         "<span class="quote"> -t </span>" (int-to-string
                                 rcirc-notify+-timeout)
                         "<span class="quote"> -u </span>" rcirc-notify+-urgency
                         "<span class="quote"> -c </span>" rcirc-notify+-category
                         "<span class="quote"> -- </span>"
                         "<span class="quote"> \"</span>" rcirc-last-position "<span class="quote">\"</span>"
                         "<span class="quote"> \"</span>"
                         (if (boundp 'msg)
                             msg "<span class="quote"></span>")
                         "<span class="quote">\"</span>")))

(defun rcirc-notify+-jump-last-message-channel()
  "<span class="quote">Jump to last message that call you.</span>"
  (interactive)
  (if rcirc-last-position
      (switch-to-buffer rcirc-last-position)
    (cycle-buffer-in-special-mode 'rcirc-mode)))

(defun rcirc-notify+-toggle()
  "<span class="quote">Toggle rcirc notify.</span>"
  (interactive)
  (if rcirc-notify+-open
      (progn
        (setq rcirc-notify+-open nil)
        (message "<span class="quote">Closed IRC Notify</span>"))
    (setq rcirc-notify+-open t)
    (message "<span class="quote">Opened IRC Notify</span>")))

(defun rcirc-notify+-allowed (nick &optional delay)
  "<span class="quote">Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`rcirc-notify+-delay'.</span>"
  (unless delay (setq delay rcirc-notify+-delay))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick rcirc-notify+-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (&gt; (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) rcirc-notify+-nick-alist)
      t)))

(defun rcirc-notify+-me (proc sender response target text)
  "<span class="quote">Notify the current user when someone sends a message that matches a regexp in `rcirc-keywords'.</span>"
  (interactive)
  (when (and (string-match (rcirc-nick proc) text)
             (not (string= (rcirc-nick proc) sender))
             (not (string= (rcirc-server-name proc) sender))
             (rcirc-notify+-allowed sender))
    (rcirc-notify+ sender nil)))

(defun rcirc-notify+-privmsg (proc sender response target text)
  "<span class="quote">Notify the current user when someone sends a private message to them.</span>"
  (interactive)
  (when (and (string= response "<span class="quote">PRIVMSG</span>")
             (not (string= sender (rcirc-nick proc)))
             (not (rcirc-channel-p target)))
    (rcirc-notify+ sender t)))

(add-hook 'rcirc-print-hooks 'rcirc-notify+-privmsg)
(add-hook 'rcirc-print-hooks 'rcirc-notify+-me)


(provide 'rcirc-notify+)

<span class="linecomment">;;; rcirc-notify+.el ends here</span>

<span class="linecomment">;;; LocalWords:  Farrington cmd im msg privmsg</span></span></pre></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span><span class="translation bar"><br />  <a class="translation new" rel="nofollow" href="http://www.emacswiki.org/emacs?action=translate;id=rcirc-notify+.el;missing=de_en_es_fr_it_ja_ko_pt_ru_se_zh">Add Translation</a></span><span class="edit bar"><br /> <a class="edit" accesskey="e" title="Click to edit this page" rel="nofollow" href="http://www.emacswiki.org/emacs?action=edit;id=rcirc-notify%2b.el">Edit this page</a> <a class="history" rel="nofollow" href="http://www.emacswiki.org/emacs?action=history;id=rcirc-notify%2b.el">View other revisions</a> <a class="admin" rel="nofollow" href="http://www.emacswiki.org/emacs?action=admin;id=rcirc-notify%2b.el">Administration</a></span><span class="time"><br /> Last edited 2009-01-11 03:03 UTC by <a class="author" title="from 68.131.212.222.broad.cd.sc.dynamic.163data.com.cn" href="http://www.emacswiki.org/emacs/AndyStewart">AndyStewart</a> <a class="diff" rel="nofollow" href="http://www.emacswiki.org/emacs?action=browse;diff=2;id=rcirc-notify%2b.el">(diff)</a></span><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a href="http://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="/pics/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="http://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="http://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p class="legal">
This work is licensed to you under version 2 of the
<a href="http://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="http://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="http://creativecommons.org/">CreativeCommons</a>
<a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
</div>
</body>
</html>
