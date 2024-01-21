;;; url.el,v --- Uniform Resource Locator retrieval tool
;; Author: wmperry
;; Created: 1995/09/20 14:06:59
;; Version: 1.377
;; Keywords: comm, data, processes, hypermedia

;;; LCD Archive Entry:
;;; url|William M. Perry|wmperry@spry.com|
;;; Major mode for manipulating URLs|
;;; 1995/09/20 14:06:59|1.377|Location Undetermined
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994, 1995 by William M. Perry (wmperry@spry.com)
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994, 1995 by William M. Perry (wmperry@spry.com)   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'mm)
(require 'md5)
(require 'base64)
(require 'url-hash)
(require 'timezone)
(or (featurep 'efs)
    (featurep 'efs-auto)
    (require 'ange-ftp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions that might not exist in old versions of emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-save-error (errobj)
  (save-excursion
    (set-buffer (get-buffer-create " *url-error*"))
    (erase-buffer))
  (display-error errobj (get-buffer-create " *url-error*")))

(cond
 ((fboundp 'display-warning)
  (fset 'url-warn 'display-warning))
 ((fboundp 'w3-warn)
  (fset 'url-warn 'w3-warn))
 ((fboundp 'warn)
  (defun url-warn (class message &optional level)
    (warn "(%s/%s) %s" class (or level 'warning) message)))
 (t
  (defun url-warn (class message &optional level)
    (save-excursion
      (set-buffer (get-buffer-create "*W3-WARNINGS*"))
      (goto-char (point-max))
      (save-excursion
	(insert (format "(%s/%s) %s\n" class (or level 'warning) message)))
      (display-buffer (current-buffer))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various nntp-related macros that are useful from gnus.el, but I don't
;;; want to have to (require 'gnus) just for them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro nntp-header-number (header)
  "Return article number in HEADER."
  (` (aref (, header) 0)))

(defmacro nntp-header-subject (header)
  "Return subject string in HEADER."
  (` (aref (, header) 1)))

(defmacro nntp-header-from (header)
  "Return author string in HEADER."
  (` (aref (, header) 2)))

(defmacro nntp-header-xref (header)
  "Return xref string in HEADER."
  (` (aref (, header) 3)))

(defmacro nntp-header-lines (header)
  "Return lines in HEADER."
  (` (aref (, header) 4)))

(defmacro nntp-header-date (header)
  "Return date in HEADER."
  (` (aref (, header) 5)))

(defmacro nntp-header-id (header)
  "Return Id in HEADER."
  (` (aref (, header) 6)))

(defmacro nntp-header-references (header)
  "Return references in HEADER."
  (` (aref (, header) 7)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variable definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst url-version (let ((x "1.381"))
			(if (string-match "Revision: \\([^ \t\n]+\\)" x)
			    (substring x (match-beginning 1) (match-end 1))
			  x))
  "Version # of URL package.")

(defvar url-current-time-string-has-args
  (cond
   ((string-match "XEmacs" emacs-version) t)
   ((string-match "Lucid" emacs-version)
    (not (string-match "Win-Emacs" emacs-version)))
   ((string-match "^19" emacs-version)
    t))
  "Non-nil iff `current-time-string' can take an argument.")

(defvar url-current-can-be-cached t
  "*Whether the current URL can be cached.")

(defvar url-current-object nil
  "A parsed representation of the current url")

(defvar url-current-callback-func nil
  "*The callback function for the current buffer.")

(defvar url-current-callback-data nil
  "*The data to be passed to the callback function.  This should be a list,
each item in the list will be an argument to the url-current-callback-func.")

(mapcar 'make-variable-buffer-local '(
				      url-current-callback-data
				      url-current-callback-func
				      url-current-can-be-cached
				      url-current-content-length
				      url-current-file
				      url-current-isindex
				      url-current-mime-encoding
				      url-current-mime-headers
				      url-current-mime-type
				      url-current-mime-viewer
				      url-current-object
				      url-current-port
				      url-current-referer
				      url-current-type
				      url-current-user
				      ))

(defvar url-default-retrieval-proc
  (function (lambda (buf)
	      (cond
	       ((save-excursion (set-buffer buf)
				(and url-current-callback-func
				     (fboundp url-current-callback-func)))
		(save-excursion
		  (save-window-excursion
		    (set-buffer buf)
		    (cond
		     ((listp url-current-callback-data)
		      (apply url-current-callback-func
			     url-current-callback-data))
		     (url-current-callback-data
		      (funcall url-current-callback-func
			       url-current-callback-data))
		     (t
		      (funcall url-current-callback-func))))))
	       ((fboundp 'w3-sentinel)
		(set-variable 'w3-working-buffer buf)
		(w3-sentinel))
	       (t
		(message "Retrieval for %s complete." buf)))))
  "*The default action to take when an asynchronous retrieval completes.")

(defvar url-honor-refresh-requests t
  "*Whether to do automatic page reloads at the request of the document
author or the server via the `Refresh' header in an HTTP/1.0 response.
If nil, no refresh requests will be honored.
If t, all refresh requests will be honored.
If non-nil and not t, the user will be asked for each refresh request.")

(defvar url-emacs-minor-version
  (if (boundp 'emacs-minor-version)
      (symbol-value 'emacs-minor-version)
    (if (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
	(string-to-int
	 (substring emacs-version
		    (match-beginning 1) (match-end 1)))
      0))
  "What minor version of emacs we are using.")

(defvar url-inhibit-mime-parsing nil
  "Whether to parse out (and delete) the MIME headers from a message.")

(defvar url-forms-based-ftp nil
  "*If non-nil, local and remote file access of directories will be shown
as an HTML 3.0 form, allowing downloads of multiple files at once.")

(defvar url-automatic-caching nil
  "*If non-nil, all documents will be automatically cached to the local
disk.")

(defvar url-cache-expired
  (function (lambda (t1 t2) (>= (- (car t2) (car t1)) 5)))
  "*A function (`funcall'able) that takes two times as its arguments, and
returns non-nil if the second time is 'too old' when compared to the first
time.")

(defvar url-check-md5s nil
  "*Whether to check md5s of retrieved documents or not.")

(defvar url-expected-md5 nil "What md5 we expect to see.")

(defvar url-broken-resolution nil
  "*Whether to use [ange|efs]-ftp-nslookup-host.")

(defvar url-bug-address "wmperry@aventail.com" "Where to send bug reports.")

(defvar url-personal-mail-address nil
  "*Your full email address.  This is what is sent to HTTP/1.0 servers as
the FROM field.  If not set when url-do-setup is run, it defaults to
the value of url-pgp/pem-entity.")

(defvar url-mule-retrieval-coding-system (if (boundp 'MULE) *euc-japan*
					  nil)
  "Coding system for retrieval, used before hexified.")

(defvar url-directory-index-file "index.html"
  "*The filename to look for when indexing a directory.  If this file
exists, and is readable, then it will be viewed instead of
automatically creating the directory listing.")

(defvar url-pgp/pem-entity nil
  "*The users PGP/PEM id - usually their email address.")

(defvar url-privacy-level 'none
  "*How private you want your requests to be.
HTTP/1.0 has header fields for various information about the user, including
operating system information, email addresses, the last page you visited, etc.
This variable controls how much of this information is sent.

This should a symbol or a list.
Valid values if a symbol are:
none     -- Send all information
low      -- Don't send the last location
high     -- Don't send the email address or last location
paranoid -- Don't send anything

If a list, this should be a list of symbols of what NOT to send.
Valid symbols are:
email    -- the email address
os       -- the operating system info
lastloc  -- the last location

Samples:

(setq url-privacy-level 'high)
(setq url-privacy-level '(os lastloc))    ;; equivalent to 'high
(setq url-privacy-level '(os))

::NOTE::
This variable controls several other variables and is _NOT_ automatically
updated.  Call the function `url-setup-privacy-info' after modifying this
variable.
")

(defvar url-uudecode-program "uudecode" "*The UUdecode executable.")

(defvar url-uuencode-program "uuencode" "*The UUencode executable.")

(defvar url-history-list nil "List of urls visited this session.")

(defvar url-inhibit-uncompression nil "Do decompression if non-nil.")

(defvar url-keep-history nil
  "*Controls whether to keep a list of all the URLS being visited.  If
non-nil, url will keep track of all the URLS visited.
If eq to `t', then the list is saved to disk at the end of each emacs
session.")

(defvar url-uncompressor-alist '((".z"  . "x-gzip")
				(".gz" . "x-gzip")
				(".uue" . "x-uuencoded")
				(".hqx" . "x-hqx")
				(".Z"  . "x-compress"))
  "*An assoc list of file extensions and the appropriate uncompression
programs for each.")

(defvar url-xterm-command "xterm -title %s -ut -e %s %s %s"
  "*Command used to start an xterm window.")

(defvar url-tn3270-emulator "tn3270"
  "The client to run in a subprocess to connect to a tn3270 machine.")

(defvar url-use-transparent nil
  "*Whether to use the transparent package by Brian Tompsett instead of
the builtin telnet functions.  Using transparent allows you to have full
vt100 emulation in the telnet and tn3270 links.")

(defvar url-mail-command 'mail
  "*This function will be called whenever url needs to send mail.  It should
enter a mail-mode-like buffer in the current window.
The commands mail-to and mail-subject should still work in this
buffer, and it should use mail-header-separator if possible.")

(defvar url-local-exec-path nil
  "*A list of possible locations for x-exec scripts.")

(defvar url-proxy-services nil
  "*An assoc list of access types and servers that gateway them.
Looks like ((\"http\" . \"url://for/proxy/server/\") ....)  This is set up
from the ACCESS_proxy environment variables in url-do-setup.")

(defvar url-global-history-file nil
  "*The global history file used by both Mosaic/X and the url package.
This file contains a list of all the URLs you have visited.  This file
is parsed at startup and used to provide URL completion.")

(defvar url-global-history-save-interval 3600
  "*The number of seconds between automatic saves of the history list.
Default is 1 hour.  Note that if you change this variable after `url-do-setup'
has been run, you need to run the `url-setup-save-timer' function manually.")

(defvar url-global-history-timer nil)

(defvar url-passwd-entry-func nil
  "*This is a symbol indicating which function to call to read in a
password.  It will be set up depending on whether you are running EFS
or ange-ftp at startup if it is nil.  This function should accept the
prompt string as its first argument, and the default value as its
second argument.")

(defvar url-gopher-labels
  '(("0" . "(TXT)")
    ("1" . "(DIR)")
    ("2" . "(CSO)")
    ("3" . "(ERR)")
    ("4" . "(MAC)")
    ("5" . "(PCB)")
    ("6" . "(UUX)")
    ("7" . "(???)")
    ("8" . "(TEL)")
    ("T" . "(TN3)")
    ("9" . "(BIN)")
    ("g" . "(GIF)")
    ("I" . "(IMG)")
    ("h" . "(WWW)")
    ("s" . "(SND)"))
  "*An assoc list of gopher types and how to describe them in the gopher
menus.  These can be any string, but HTML/HTML+ entities should be
used when necessary, or it could disrupt formatting of the document
later on.  It is also a good idea to make sure all the strings are the
same length after entity references are removed, on a strictly
stylistic level.")

(defvar url-gopher-icons
  '(
    ("0" . "&text.document;")
    ("1" . "&folder;")
    ("2" . "&index;")
    ("3" . "&stop;")
    ("4" . "&binhex.document;")
    ("5" . "&binhex.document;")
    ("6" . "&uuencoded.document;")
    ("7" . "&index;")
    ("8" . "&telnet;")
    ("T" . "&tn3270;")
    ("9" . "&binary.document;")
    ("g" . "&image;")
    ("I" . "&image;")
    ("s" . "&audio;"))
  "*An assoc list of gopher types and the graphic entity references to
show when possible.")

(defvar url-standalone-mode nil "*Rely solely on the cache?")
(defvar url-working-buffer " *URL*" "The buffer to do all the processing in.")
(defvar url-current-annotation nil "URL of document we are annotating...")
(defvar url-current-referer nil "Referer of this page.")
(defvar url-current-content-length nil "Current content length.")
(defvar url-current-file nil "Filename of current document.")
(defvar url-current-isindex nil "Is the current document a searchable index?")
(defvar url-current-mime-encoding nil "MIME encoding of current document.")
(defvar url-current-mime-headers nil "An alist of MIME headers.")
(defvar url-current-mime-type nil "MIME type of current document.")
(defvar url-current-mime-viewer nil "How to view the current MIME doc.")
(defvar url-current-nntp-server nil "What nntp server currently opened.")
(defvar url-current-passwd-count 0 "How many times password has failed.")
(defvar url-current-port nil "Port # of the current document.")
(defvar url-current-server nil "Server of the current document.")
(defvar url-current-user nil "Username for ftp login.")
(defvar url-current-type nil "We currently in http or file mode?")
(defvar url-gopher-types "0123456789+gIThws:;<"
  "A string containing character representations of all the gopher types.")
(defvar url-mime-separator-chars (mapcar 'identity
					(concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
						"abcdefghijklmnopqrstuvwxyz"
						"0123456789'()+_,-./=?"))
  "Characters allowable in a MIME multipart separator.")

(defvar url-bad-port-list
  '("25" "119" "19")
  "*List of ports to warn the user about connecting to.  Defaults to just
the mail, chargen, and NNTP ports so you cannot be tricked into sending
fake mail or forging messages by a malicious HTML document.")

(defvar url-be-anal-about-file-attributes nil
  "*Whether to use HTTP/1.0 to figure out file attributes
or just guess based on file extension, etc.")

(defvar url-be-asynchronous nil
  "*Controls whether document retrievals over HTTP should be done in
the background.  This allows you to keep working in other windows
while large downloads occur.")
(make-variable-buffer-local 'url-be-asynchronous)

(defvar url-request-data nil "Any data to send with the next request.")

(defvar url-request-extra-headers nil
  "A list of extra headers to send with the next request.  Should be
an assoc list of headers/contents.")

(defvar url-request-method nil "The method to use for the next request.")

(defvar url-mime-encoding-string nil
  "String to send to the server in the Accept-encoding: field in HTTP/1.0
requests.  This is created automatically from mm-content-transfer-encodings.")

(defvar url-mime-language-string "*/*"
  "String to send to the server in the Accept-language: field in
HTTP/1.0 requests.")

(defvar url-mime-accept-string nil
  "String to send to the server in the Accept: field in HTTP/1.0 requests.
This is created automatically from url-mime-viewers, after the mailcap file
has been parsed.")

(defvar url-registered-protocols nil
  "Internal structure - do not modify!  See `url-register-protocol'")

(defvar url-package-version "Unknown" "Version # of package using URL.")

(defvar url-package-name "Unknown" "Version # of package using URL.")

(defvar url-default-session-id nil
  "The default session ID, if none is defined for the current server.
This is regenerated each time `url-do-setup' is called")

(defvar url-session-id-alist nil
  "An assoc list of Session-ID headers.  Keyed off of server:portnum")

(defvar url-system-type nil "What type of system we are on.")
(defvar url-os-type nil "What OS we are on.")

(defvar url-max-password-attempts 5
  "*Maximum number of times a password will be prompted for when a
protected document is denied by the server.")

(defvar url-wais-to-mime
  '(
    ("WSRC" . "application/x-wais-source") 	; A database description
    ("TEXT" . "text/plain")			; plain text
    )
  "An assoc list of wais doctypes and their corresponding MIME
content-types.")

(defvar url-waisq-prog "waisq"
  "*Name of the waisq executable on this system.  This should be the
waisq program from think.com's wais8-b5.1 distribution.")

(defvar url-wais-gateway-server "www.ncsa.uiuc.edu"
  "*The machine name where the WAIS gateway lives.")

(defvar url-wais-gateway-port "8001"
  "*The port # of the WAIS gateway.")

(defvar url-temporary-directory "/tmp" "*Where temporary files go.")

(defvar url-show-status t
  "*Whether to show a running total of bytes transferred.  Can cause a
large hit if using a remote X display over a slow link, or a terminal
with a slow modem.")

(defvar url-using-proxy nil
  "Either nil or the fully qualified proxy URL in use, e.g.
http://www.domain.com/")

(defvar url-news-server nil
  "*The default news server to get newsgroups/articles from if no server
is specified in the URL.  Defaults to the environment variable NNTPSERVER
or \"news\" if NNTPSERVER is undefined.")

(defvar url-gopher-to-mime
  '((?0 . "text/plain")			; It's a file
    (?1 . "www/gopher")			; Gopher directory
    (?2 . "www/gopher-cso-search")	; CSO search
    (?3 . "text/plain")			; Error
    (?4 . "application/mac-binhex40")	; Binhexed macintosh file
    (?5 . "application/pc-binhex40")	; DOS binary archive of some sort
    (?6 . "archive/x-uuencode")		; Unix uuencoded file
    (?7 . "www/gopher-search")		; Gopher search!
    (?9 . "application/octet-stream")	; Binary file!
    (?g . "image/gif")			; Gif file
    (?I . "image/gif")			; Some sort of image
    (?h . "text/html")			; HTML source
    (?s . "audio/basic")		; Sound file
    )
  "*An assoc list of gopher types and their corresponding MIME types.")

(defvar url-use-hypertext-gopher t
  "*Controls how gopher documents are retrieved.
If non-nil, the gopher pages will be converted into HTML and parsed
just like any other page.  If nil, the requests will be passed off to
the gopher.el package by Scott Snyder.  Using the gopher.el package
will lose the gopher+ support, and inlined searching.")

(defvar url-global-history-hash-table nil
  "Hash table for global history completion.")

(defvar url-nonrelative-link
  "^\\([-a-zA-Z0-9+.]+:\\)"
  "A regular expression that will match an absolute URL.")

(defvar url-confirmation-func 'y-or-n-p
  "*What function to use for asking yes or no functions.  Possible
values are 'yes-or-no-p or 'y-or-n-p, or any function that takes a
single argument (the prompt), and returns t only if a positive answer
is gotten.")

(defvar url-connection-retries 5
  "*# of times to try for a connection before bailing.
If for some reason url-open-stream cannot make a connection to a host
right away, it will sit for 1 second, then try again, up to this many
tries.")

(defvar url-find-this-link nil "Link to go to within a document.")

(defvar url-show-http2-transfer t
  "*Whether to show the total # of bytes, size of file, and percentage
transferred when retrieving a document over HTTP/1.0 and it returns a
valid content-length header.  This can mess up some people behind
gateways.")

(defvar url-gateway-method 'native
  "*The type of gateway support to use.
Should be a symbol specifying how we are to get a connection off of the
local machine.

Currently supported methods:
'program	:: Run a program in a subprocess to connect
                   (examples are itelnet, an expect script, etc)
'native		:: Use the native open-network-stream in emacs
'tcp            :: Use the excellent tcp.el package from gnus.
                   This simply does a (require 'tcp), then sets
                   url-gateway-method to be 'native.")

(defvar url-gateway-shell-is-telnet nil
  "*Whether the login shell of the remote host is telnet.")

(defvar url-gateway-program-interactive nil
  "*Whether url needs to hand-hold the login program on the remote machine.")

(defvar url-gateway-handholding-login-regexp "ogin:"
  "*Regexp for when to send the username to the remote process.")

(defvar url-gateway-handholding-password-regexp "ord:"
  "*Regexp for when to send the password to the remote process.")

(defvar url-gateway-host-prompt-pattern "^[^#$%>;]*[#$%>;] *"
  "*Regexp used to detect when the login is finished on the remote host.")

(defvar url-gateway-telnet-ready-regexp "Escape character is .*"
  "*A regular expression that signifies url-gateway-telnet-program is
ready to accept input.")

(defvar url-local-rlogin-prog "rlogin"
  "*Program for local telnet connections.")

(defvar url-remote-rlogin-prog "rlogin"
  "*Program for remote telnet connections.")

(defvar url-local-telnet-prog "telnet"
  "*Program for local telnet connections.")

(defvar url-remote-telnet-prog "telnet"
  "*Program for remote telnet connections.")  

(defvar url-gateway-telnet-program "itelnet"
  "*Program to run in a subprocess when using gateway-method 'program.")

(defvar url-gateway-local-host-regexp nil
  "*If a host being connected to matches this regexp then the
connection is done natively, otherwise the process is started on
`url-gateway-host' instead.")

(defvar url-use-hypertext-dired t
  "*How to format directory listings.

If value is non-nil, use directory-files to list them out and
transform them into a hypertext document, then pass it through the
parse like any other document.

If value nil, just pass the directory off to dired using find-file.")

(defconst monthabbrev-alist
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defvar url-default-ports '(("http"   .  "80")
			    ("gopher" .  "70")
			    ("telnet" .  "23")
			    ("news"   . "119")
			    ("https"  . "443")
			    ("shttp"  .  "80"))
  "An assoc list of protocols and default port #s")

(defvar url-setup-done nil "*Has setup configuration been done?")

(defvar url-source nil
  "*Whether to force a sourcing of the next buffer.  This forces local
files to be read into a buffer, no matter what.  Gets around the
optimization that if you are passing it to a viewer, just make a
symbolic link, which looses if you want the source for inlined
images/etc.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File-name-handler-alist functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-setup-file-name-handlers ()
  ;; Setup file-name handlers.
  '(cond
    ((not (boundp 'file-name-handler-alist))
     nil)				; Don't load if no alist
    ((rassq 'url-file-handler file-name-handler-alist)
     nil)				; Don't load twice
    ((and (string-match "XEmacs\\|Lucid" emacs-version)
	  (< url-emacs-minor-version 11)) ; Don't load in lemacs 19.10
     nil)
    (t
     (setq file-name-handler-alist
	   (let ((new-handler (cons
			       (concat "^/*"
				       (substring url-nonrelative-link1 nil))
			       'url-file-handler)))
	     (if file-name-handler-alist
		 (append (list new-handler) file-name-handler-alist)
	       (list new-handler)))))))
  
(defun url-file-handler (operation &rest args)
  ;; Function called from the file-name-handler-alist routines.  OPERATION
  ;; is what needs to be done ('file-exists-p, etc).  args are the arguments
  ;; that would have been passed to OPERATION."
  (let ((fn (get operation 'url-file-handlers))
	(url (car args))
	(myargs (cdr args)))
    (if (= (string-to-char url) ?/)
	(setq url (substring url 1 nil)))
    (if fn (apply fn url myargs)
      (let (file-name-handler-alist)
	(apply operation url myargs)))))

(defun url-file-handler-identity (&rest args)
  (car args))

(defun url-file-handler-null (&rest args)
  nil)

(put 'file-directory-p 'url-file-handlers 'url-file-handler-null)
(put 'substitute-in-file-name 'url-file-handlers 'url-file-handler-identity)
(put 'file-writable-p 'url-file-handlers 'url-file-handler-null)
(put 'file-truename 'url-file-handlers 'url-file-handler-identity)
(put 'insert-file-contents 'url-file-handlers 'url-insert-file-contents)
(put 'expand-file-name 'url-file-handlers 'url-expand-file-name)
(put 'directory-files 'url-file-handlers 'url-directory-files)
(put 'file-directory-p 'url-file-handlers 'url-file-directory-p)
(put 'file-writable-p 'url-file-handlers 'url-file-writable-p)
(put 'file-readable-p 'url-file-handlers 'url-file-exists)
(put 'file-executable-p 'url-file-handlers 'null)
(put 'file-symlink-p 'url-file-handlers 'null)
(put 'file-exists-p 'url-file-handlers 'url-file-exists)
(put 'copy-file 'url-file-handlers 'url-copy-file)
(put 'file-attributes 'url-file-handlers 'url-file-attributes)
(put 'file-name-all-completions 'url-file-handlers
     'url-file-name-all-completions)
(put 'file-name-completion 'url-file-handlers 'url-file-name-completion)
(put 'file-local-copy 'url-file-handlers 'url-file-local-copy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic URL parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro url-type (urlobj)
  (` (aref (, urlobj) 0)))

(defmacro url-user (urlobj)
  (` (aref (, urlobj) 1)))

(defmacro url-password (urlobj)
  (` (aref (, urlobj) 2)))

(defmacro url-host (urlobj)
  (` (aref (, urlobj) 3)))

(defmacro url-fullness (urlobj)
  (` (aref (, urlobj) 7)))

(defmacro url-port (urlobj)
  (` (or (aref (, urlobj) 4)
	 (if (url-fullness (, urlobj))
	     (cdr-safe (assoc (url-type (, urlobj)) url-default-ports))))))

(defmacro url-filename (urlobj)
  (` (aref (, urlobj) 5)))

(defmacro url-target (urlobj)
  (` (aref (, urlobj) 6)))

(defmacro url-set-type (urlobj type)
  (` (aset (, urlobj) 0 (, type))))

(defmacro url-set-user (urlobj user)
  (` (aset (, urlobj) 1 (, user))))

(defmacro url-set-password (urlobj pass)
  (` (aset (, urlobj) 2 (, pass))))

(defmacro url-set-host (urlobj host)
  (` (aset (, urlobj) 3 (, host))))

(defmacro url-set-port (urlobj port)
  (` (aset (, urlobj) 4 (, port))))

(defmacro url-set-filename (urlobj file)
  (` (aset (, urlobj) 5 (, file))))

(defmacro url-set-target (urlobj targ)
  (` (aset (, urlobj) 6 (, targ))))

(defmacro url-set-full (urlobj val)
  (` (aset (, urlobj) 7 (, val))))
  
(defun url-recreate-url (urlobj)
  (concat (url-type urlobj) ":" (if (url-host urlobj) "//" "")
	  (if (url-user urlobj)
	      (concat (url-user urlobj)
		      (if (url-password urlobj)
			  (concat ":" (url-password urlobj)))
		      "@"))
	  (url-host urlobj)
	  (if (and (url-port urlobj)
		   (not (equal (url-port urlobj)
			       (cdr-safe (assoc (url-type urlobj)
						url-default-ports)))))
	      (concat ":" (url-port urlobj)))
	  (or (url-filename urlobj) "/")
	  (if (url-target urlobj)   (concat "#" (url-target urlobj)))))
	    
(defun url-generic-parse-url (url)
  "Return a vector of the parts of URL.
Format is [protocol username password hostname portnumber file reference]"
  (cond
   ((null url)
    (make-vector 8 nil))
   ((or (not (string-match url-nonrelative-link url))
	(= ?/ (string-to-char url)))
    (let ((retval (make-vector 8 nil)))
      (url-set-filename retval url)
      (url-set-full retval nil)
      retval))
   (t
    (save-excursion
      (set-buffer (get-buffer-create " *urlparse*"))
      (erase-buffer)
      (insert url)
      (goto-char (point-min))
      (set-syntax-table url-mailserver-syntax-table)
      (let ((save-pos (point))
	    (prot nil)
	    (user nil)
	    (pass nil)
	    (host nil)
	    (port nil)
	    (file nil)
	    (refs nil)
	    (full nil))
	(if (not (looking-at "//"))
	    (progn
	      (skip-chars-forward "a-zA-Z+.\\-")
	      (downcase-region save-pos (point))
	      (setq prot (buffer-substring save-pos (point)))
	      (skip-chars-forward ":")
	      (setq save-pos (point))))

	;; We are doing a fully specified URL, with hostname and all
	(if (looking-at "//")
	    (progn
	      (setq full t)
	      (forward-char 2)
	      (setq save-pos (point))
	      (skip-chars-forward "^/")
	      (downcase-region save-pos (point))
	      (setq host (buffer-substring save-pos (point)))
	      (if (string-match "^\\([^@]+\\)@" host)
		  (setq user (url-match host 1)
			host (substring host (match-end 0) nil)))
	      (if (and user (string-match "\\([^:]+\\):\\(.*\\)" user))
		  (setq pass (url-match user 2)
			user (url-match user 1)))
	      (if (string-match ":\\([0-9+]+\\)" host)
		  (setq port (url-match host 1)
			host (substring host 0 (match-beginning 0))))
	      (setq save-pos (point))))

	;; Now check for references
	(skip-chars-forward "^#")
	(setq file (buffer-substring save-pos (point)))
	(if (eobp)
	    nil
	  (skip-chars-forward "#")
	  (setq refs (buffer-substring (point) (point-max))))
	(and port (string= port (or (cdr-safe (assoc prot url-default-ports))
				    ""))
	     (setq port nil))
	(vector prot user pass host port file refs full))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;; -----------------
;;; Various functions used around the url code.
;;; Some of these qualify as hacks, but hey, this is elisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'mm-string-to-tokens)
    (fset 'url-string-to-tokens 'mm-string-to-tokens)
  (defun url-string-to-tokens (str &optional delim)
    "Return a list of words from the string STR"
    (setq delim (or delim ? ))
    (let (results y)
      (mapcar
       (function
	(lambda (x)
	  (cond
	   ((and (= x delim) y) (setq results (cons y results) y nil))
	   ((/= x delim) (setq y (concat y (char-to-string x))))
	   (t nil)))) str)
      (nreverse (cons y results)))))

(defun url-days-between (date1 date2)
  ;; Return the number of days between date1 and date2.
  (- (url-day-number date1) (url-day-number date2)))

(defun url-day-number (date)
  (let ((dat (mapcar (function (lambda (s) (and s (string-to-int s)) ))
		     (timezone-parse-date date))))
    (timezone-absolute-from-gregorian 
     (nth 1 dat) (nth 2 dat) (car dat))))

(defun url-seconds-since-epoch (date)
  ;; Returns a number that says how many seconds have
  ;; lapsed between Jan 1 12:00:00 1970 and DATE."
  (let* ((tdate (mapcar (function (lambda (ti) (and ti (string-to-int ti))))
			(timezone-parse-date date)))
	 (ttime (mapcar (function (lambda (ti) (and ti (string-to-int ti))))
			(timezone-parse-time
			 (aref (timezone-parse-date date) 3))))
	 (edate (mapcar (function (lambda (ti) (and ti (string-to-int ti))))
			(timezone-parse-date "Jan 1 12:00:00 1970")))
	 (tday (- (timezone-absolute-from-gregorian 
		   (nth 1 tdate) (nth 2 tdate) (nth 0 tdate))
		  (timezone-absolute-from-gregorian 
		   (nth 1 edate) (nth 2 edate) (nth 0 edate)))))
    (+ (nth 2 ttime)
       (* (nth 1 ttime) 60)
       (* (nth 0 ttime) 60 60)
       (* tday 60 60 24))))

(defun url-match (s x)
  ;; Return regexp match x in s.
  (substring s (match-beginning x) (match-end x)))

(defun url-split (str del)
  ;; Split the string STR, with DEL (a regular expression) as the delimiter.
  ;; Returns an assoc list that you can use with completing-read."
  (let (x y)
    (while (string-match del str)
      (setq y (substring str 0 (match-beginning 0))
	    str (substring str (match-end 0) nil))
      (if (not (string-match "^[ \t]+$" y))
	  (setq x (cons (list y y) x))))
    (if (not (equal str ""))
	(setq x (cons (list str str) x)))
    x))

(defun url-replace-regexp (regexp to-string)
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-string t nil)))

(defun url-clear-tmp-buffer ()
  (set-buffer (get-buffer-create url-working-buffer))
  (if buffer-read-only (toggle-read-only))
  (erase-buffer))  

(defun url-maybe-relative (url)
  (url-retrieve (url-expand-file-name url)))

(defun url-buffer-is-hypertext (&optional buff)
  "Return t if a buffer contains HTML, as near as we can guess."
  (setq buff (or buff (current-buffer)))
  (save-excursion
    (set-buffer buff)
    (let ((case-fold-search t))
      (goto-char (point-min))
      (re-search-forward
       "<\\(TITLE\\|HEAD\\|BASE\\|H[0-9]\\|ISINDEX\\|P\\)>" nil t))))

(defun nntp-after-change-function (&rest args)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (message "Read %d bytes" (point-max))))

(defun url-percentage (x y)
  (if (fboundp 'float)
      (round (* 100 (/ x (float y))))
    (/ (* x 100) y)))

(defun url-after-change-function (&rest args)
  ;; The nitty gritty details of messaging the HTTP/1.0 status messages
  ;; in the minibuffer."
  (save-excursion
    (set-buffer url-working-buffer)
    (let (status-message)
      (if url-current-content-length
	  nil
	(goto-char (point-min))
	(skip-chars-forward " \t\n")
	(if (not (looking-at "HTTP/[0-9]\.[0-9]"))
	    (setq url-current-content-length 0)
	  (setq url-current-isindex
		(and (re-search-forward "$\r*$" nil t) (point)))
	  (if (re-search-forward
	       "^content-type:[ \t]*\\([^\r\n]+\\)\r*$"
	       url-current-isindex t)
	      (setq url-current-mime-type (downcase
					  (url-eat-trailing-space
					   (buffer-substring
					    (match-beginning 1)
					    (match-end 1))))))
	  (if (re-search-forward "^content-length:\\([^\r\n]+\\)\r*$"
				 url-current-isindex t)
	      (setq url-current-content-length
		    (string-to-int (buffer-substring (match-beginning 1)
						     (match-end 1))))
	    (setq url-current-content-length nil))))
      (goto-char (point-min))
      (if (re-search-forward "^status:\\([^\r]*\\)" url-current-isindex t)
	  (progn
	    (setq status-message (buffer-substring (match-beginning 1)
						   (match-end 1)))
	    (replace-match (concat "btatus:" status-message))))
      (goto-char (point-max))
      (cond
       (status-message (url-lazy-message "%s" status-message))
       ((and url-current-content-length (> url-current-content-length 1)
	     url-current-mime-type)
	(url-lazy-message "Read %d of %d bytes (%d%%) [%s]"
			 (point-max) url-current-content-length
			 (url-percentage (point-max) url-current-content-length)
			 url-current-mime-type))
       ((and url-current-content-length (> url-current-content-length 1))
	(url-lazy-message "Read %d of %d bytes (%d%%)"
			 (point-max) url-current-content-length
			 (url-percentage (point-max)
					 url-current-content-length)))
       ((and (/= 1 (point-max)) url-current-mime-type)
	(url-lazy-message "Read %d bytes. [%s]" (point-max)
			 url-current-mime-type))
       ((/= 1 (point-max))
	(url-lazy-message "Read %d bytes." (point-max)))
       (t (url-lazy-message "Waiting for response."))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End hacks section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun url-format-directory (dir)
  ;; Format the files in DIR into hypertext
  (let ((files (directory-files dir nil)) file
	div attr mod-time size typ title)
    (if (and url-directory-index-file
	     (file-exists-p (expand-file-name url-directory-index-file dir))
	     (file-readable-p (expand-file-name url-directory-index-file dir)))
	(save-excursion
	  (set-buffer url-working-buffer)
	  (erase-buffer)
	  (mm-insert-file-contents
	   (expand-file-name url-directory-index-file dir)))
      (save-excursion
	(if (string-match "/\\([^/]+\\)/$" dir)
	    (setq title (url-match dir 1))
	  (setq title "/"))
	(setq div (1- (length files)))
	(set-buffer url-working-buffer)
	(erase-buffer)
	(insert "<html>\n"
		" <head>\n"
		"  <title>" title "</title>\n"
		" </head>\n"
		" <body>\n"
		"  <div1>\n"
		"   <h1 align=center> Index of " title "</h1>\n"
		(if url-forms-based-ftp
		    "   <form method=mget enctype=application/batch-fetch>\n"
		  "")
		"   <pre>\n"
		"       Name                     Last modified                Size\n"
		"<hr>\n")
	(while files
	  (url-lazy-message "Building directory list... (%d%%)"
			    (/ (* 100 (- div (length files))) div))
	  (setq file (expand-file-name (car files) dir)
		attr (file-attributes file)
		file (car files)
		mod-time (nth 5 attr)
		size (nth 7 attr)
		typ (or (mm-extension-to-mime (url-file-extension file)) ""))
	  (if (or (equal '(0 0) mod-time) ; Set to null if unknown or
                                        ; untranslateable
		  (not url-current-time-string-has-args))
	      (setq mod-time "Unknown                 ")
	    (setq mod-time (current-time-string mod-time)))
	  (if (or (equal size 0) (equal size -1) (null size))
	      (setq size "   -")
	    (setq size
		  (cond
		   ((< size 1024) (concat "   " "1K"))
		   ((< size 1048576) (concat "   "
					     (int-to-string
					      (max 1 (/ size 1024))) "K"))
		   (t
		    (let* ((megs (max 1 (/ size 1048576)))
			   (kilo (/ (- size (* megs 1048576)) 1024)))
		      (concat "   "  (int-to-string megs)
			      (if (> kilo 0)
				  (concat "." (int-to-string kilo))
				"") "M"))))))
	  (cond
	   ((or (equal "." (car files)) (equal "/.." (car files)) )nil)
	   ((equal ".." (car files))
	    (if (not (= ?/ (aref file (1- (length file)))))
		(setq file (concat file "/")))
	    (insert (if url-forms-based-ftp "   " "")
		    "[DIR] <a href=\"" file "\">Parent directory</a>\n"))
	   ((stringp (nth 0 attr))	; Symbolic link handling
	    (insert (if url-forms-based-ftp "   " "")
		    "[LNK] <a href=\"" file "\">" (car files) "</a>"
		    (make-string (max 0 (- 25 (length (car files)))) ? )
		    mod-time size "\n"))
	   ((nth 0 attr)		; Directory handling
	    (insert (if url-forms-based-ftp "   " "")
		    "[DIR] <a href=\"" file "/\">" (car files) "</a>"
		    (make-string (max 0 (- 25 (length (car files)))) ? )
		    mod-time size "\n"))
	   ((string-match "image" typ)
	    (insert (if url-forms-based-ftp
			(concat "<input type=checkbox name=file value=\""
				(car files) "\">")
		      "")
		    "[IMG] <a href=\"" file "\">" (car files) "</a>"
		    (make-string (max 0 (- 25 (length (car files)))) ? )
		    mod-time size "\n"))
	   ((string-match "application" typ)
	    (insert (if url-forms-based-ftp
			(concat "<input type=checkbox name=file value=\""
				(car files) "\">")
		      "")
		    "[APP] <a href=\"" file "\">" (car files) "</a>"
		    (make-string (max 0 (- 25 (length (car files)))) ? )
		    mod-time size "\n"))
	   ((string-match "text" typ)
	    (insert (if url-forms-based-ftp
			(concat "<input type=checkbox name=file value=\""
				(car files) "\">")
		      "")
		    "[TXT] <a href=\"" file "\">" (car files) "</a>"
		    (make-string (max 0 (- 25 (length (car files)))) ? )
		    mod-time size "\n"))
	   (t
	    (insert (if url-forms-based-ftp
			(concat "<input type=checkbox name=file value=\""
				(car files) "\">")
		      "")
		    "[UNK] <a href=\"" file "\">" (car files) "</a>"
		    (make-string (max 0 (- 25 (length (car files)))) ? )
		    mod-time size "\n")))
	  (setq files (cdr files)))
	(insert "   </pre>\n"
		(if url-forms-based-ftp
		    (concat "  <input type=submit value=\"Copy files\">\n"
			    "  </form>\n")
		  "")
		"  </div1>\n"
		" </body>\n"
		"</html>\n"
		"<!-- Automatically generated by URL v" url-version
		" -->\n")))))

(defun url-have-visited-url (url &rest args)
  "Return non-nil iff the user has visited URL before.
The return value is a cons of the url and the date last accessed as a string"
  (url-gethash url url-global-history-hash-table))

(defun url-directory-files (url &rest args)
  "Return a list of files on a server."
  nil)

(defun url-file-writable-p (url &rest args)
  "Return t iff a url is writable by this user"
  nil)

(defun url-copy-file (url &rest args)
  "Copy a url to the specified filename."
  nil)

(defun url-file-directly-accessible-p (url)
  "Returns t iff the specified URL is directly accessible
on your filesystem.  (nfs, local file, etc)."
  (let* ((urlobj (if (vectorp url) url (url-generic-parse-url url)))
	 (type (url-type urlobj)))
    (and (member type '("file" "ftp"))
	 (not (url-host urlobj)))))

;;;###autoload
(defun url-file-attributes (url &rest args)
  "Return a list of attributes of URL.
Value is nil if specified file cannot be opened.
Otherwise, list elements are:
 0. t for directory, string (name linked to) for symbolic link, or nil.
 1. Number of links to file.
 2. File uid.
 3. File gid.
 4. Last access time, as a list of two integers.
  First integer has high-order 16 bits of time, second has low 16 bits.
 5. Last modification time, likewise.
 6. Last status change time, likewise.
 7. Size in bytes. (-1, if number is out of range).
 8. File modes, as a string of ten letters or dashes as in ls -l.
    If URL is on an http server, this will return the content-type if possible.
 9. t iff file's gid would change if file were deleted and recreated.
10. inode number.
11. Device number.

If file does not exist, returns nil."
  (and url
       (let* ((urlobj (url-generic-parse-url url))
	      (type (url-type urlobj))
	      (url-automatic-caching nil)
	      (data nil)
	      (exists nil))
	 (cond
	  ((equal type "http")
	   (cond
	    ((not url-be-anal-about-file-attributes)
	     (setq data (list
			 (url-file-directory-p url) ; Directory
			 1		; number of links to it
			 0		; UID
			 0		; GID
			 (cons 0 0)	; Last access time
			 (cons 0 0)	; Last mod. time
			 (cons 0 0)	; Last status time
			 -1		; file size
			 (mm-extension-to-mime
			  (url-file-extension (url-filename urlobj)))
			 nil		; gid would change
			 0		; inode number
			 0		; device number
			 )))
	    (t				; HTTP/1.0, use HEAD
	     (let ((url-request-method "HEAD")
		   (url-request-data nil)
		   (url-working-buffer " *url-temp*"))
	       (save-excursion
		 (url-retrieve url)
		 (setq data (and (setq exists
				       (cdr (assoc "status"
						   url-current-mime-headers)))
				 (>= exists 200)
				 (< exists 300)
				 (list
				  (url-file-directory-p url) ; Directory
				  1	; links to
				  0	; UID
				  0	; GID
				  (cons 0 0) ; Last access time
				  (cons 0 0) ; Last mod. time
				  (cons 0 0) ; Last status time
				  (or	; Size in bytes
				   (cdr (assoc "content-length"
					       url-current-mime-headers))
				   -1)
				  (or
				   (cdr (assoc "content-type"
					       url-current-mime-headers))
				   (mm-extension-to-mime
				    (url-file-extension
				     (url-filename urlobj)))) ; content-type
				  nil	; gid would change
				  0	; inode number
				  0	; device number
				  )))
		 (and (not data)
		      (setq data (list (url-file-directory-p url)
				       1 0 0 (cons 0 0) (cons 0 0) (cons 0 0)
				       -1 (mm-extension-to-mime
					   (url-file-extension
					    url-current-file))
				       nil 0 0)))
		 (kill-buffer " *url-temp*"))))))
	  ((member type '("ftp" "file"))
	   (let ((fname (if (url-host urlobj)
			    (concat "/"
				    (if (url-user urlobj)
					(concat (url-user urlobj) "@")
				      "")
				    (url-host urlobj) ":"
				    (url-filename urlobj))
			  (url-filename urlobj))))
	     (setq data (or (file-attributes fname) (make-list 12 nil)))
	     (setcar (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr data))))))))
		     (mm-extension-to-mime (url-file-extension fname)))))
	  (t nil))
	 data)))

(defun url-file-name-all-completions (file dirname &rest args)
  "Return a list of all completions of file name FILE in directory DIR.
These are all file names in directory DIR which begin with FILE."
  ;; need to rewrite
  )

(defun url-file-name-completion (file dirname &rest args)
  "Complete file name FILE in directory DIR.
Returns the longest string
common to all filenames in DIR that start with FILE.
If there is only one and FILE matches it exactly, returns t.
Returns nil if DIR contains no name starting with FILE."
  (apply 'url-file-name-all-completions file dirname args))

(defun url-file-local-copy (file &rest args)
  "Copy the file FILE into a temporary file on this machine.
Returns the name of the local copy, or nil, if FILE is directly
accessible."
  nil)

(defun url-insert-file-contents (url &rest args)
  "Insert the contents of the URL in this buffer."
  (save-excursion
    (url-retrieve url))
  (insert-buffer url-working-buffer)
  (setq buffer-file-name url)
  (kill-buffer url-working-buffer))

(defun url-file-directory-p (url &rest args)
  "Return t iff a url points to a directory"
  (equal (substring url -1 nil) "/"))

(defun url-file-exists (url &rest args)
  "Return t iff a file exists."
  (let* ((urlobj (url-generic-parse-url url))
	 (type (url-type urlobj))
	 (exists nil))
    (cond
     ((equal type "http")		; use head
      (let ((url-request-method "HEAD")
	    (url-request-data nil)
	    (url-working-buffer " *url-temp*"))
	(save-excursion
	  (url-retrieve url)
	  (setq exists (or (cdr
			    (assoc "status" url-current-mime-headers)) 500))
	  (kill-buffer " *url-temp*")
	  (setq exists (and (>= exists 200) (< exists 300))))))
     ((member type '("ftp" "file"))	; file-attributes
      (let ((fname (if (url-host urlobj)
		       (concat "/"
			       (if (url-user urlobj)
				   (concat (url-user urlobj) "@")
				 "")
			       (url-host urlobj) ":"
			       (url-filename urlobj))
		     (url-filename urlobj))))
	(setq exists (file-exists-p fname))))
     (t nil))
    exists))

;;;###autoload
(defun url-normalize-url (url)
  "Return a 'normalized' version of URL.  This strips out default port
numbers, etc."
  (let (type data grok retval)
    (setq data (url-generic-parse-url url)
	  type (url-type data))
    (if (member type '("www" "about" "mailto" "mailserver" "info"))
	(setq retval url)
      (setq retval (url-recreate-url data)))
    retval))

;;;###autoload
(defun url-buffer-visiting (url)
  "Return the name of a buffer (if any) that is visiting URL."
  (setq url (url-normalize-url url))
  (let ((bufs (buffer-list))
	(found nil))
    (if (condition-case ()
	    (string-match "\\(.*\\)#" url)
	  (error nil))
	(setq url (url-match url 1)))
    (while (and bufs (not found))
      (save-excursion
	(set-buffer (car bufs))
	(setq found (if (and
			 (not (equal (buffer-name (car bufs))
				     url-working-buffer))
			 (memq major-mode '(url-mode w3-mode))
			 (equal (url-view-url t) url)) (car bufs) nil)
	      bufs (cdr bufs))))
    found))

(defun url-file-size (url &rest args)
  "Return the size of a file in bytes, or -1 if can't be determined."
  (let* ((urlobj (url-generic-parse-url url))
	 (type (url-type urlobj))
	 (size -1)
	 (data nil))
    (cond
     ((equal type "http")		; use head
      (let ((url-request-method "HEAD")
	    (url-request-data nil)
	    (url-working-buffer " *url-temp*"))
	(save-excursion
	  (url-retrieve url)
	  (setq size (or (cdr
			  (assoc "content-length" url-current-mime-headers))
			 -1))
	  (kill-buffer " *url-temp*"))))
     ((member type '("ftp" "file"))	; file-attributes
      (let ((fname (if (url-host urlobj)
		       (concat "/"
			       (if (url-user urlobj)
				   (concat (url-user urlobj) "@")
				 "")
			       (url-host urlobj) ":"
			       (url-filename urlobj))
		     (url-filename urlobj))))
	(setq data (file-attributes fname)
	      size (nth 7 data))))
     (t nil))
    (cond
     ((stringp size) (string-to-int size))
     ((integerp size) size)
     ((null size) -1)
     (t -1))))

(defun url-generate-new-buffer-name (start)
  "Create a new buffer name based on START."
  (let ((x 1)
	name)
    (if (not (get-buffer start))
	start
      (progn
	(setq name (format "%s<%d>" start x))
	(while (get-buffer name)
	  (setq x (1+ x)
		name (format "%s<%d>" start x)))
	name))))

(defun url-generate-unique-filename (&optional fmt)
  "Generate a unique filename in url-temporary-directory"
  (if (not fmt)
      (let ((base (format "url-tmp.%d" (user-real-uid)))
	    (fname "")
	    (x 0))
	(setq fname (format "%s%d" base x))
	(while (file-exists-p (expand-file-name fname url-temporary-directory))
	  (setq x (1+ x)
		fname (concat base (int-to-string x))))
	(expand-file-name fname url-temporary-directory))
    (let ((base (concat "url" (int-to-string (user-real-uid))))
	  (fname "")
	  (x 0))
      (setq fname (format fmt (concat base (int-to-string x))))
      (while (file-exists-p (expand-file-name fname url-temporary-directory))
	(setq x (1+ x)
	      fname (format fmt (concat base (int-to-string x)))))
      (expand-file-name fname url-temporary-directory))))

(defvar url-lazy-message-time 0)

(defun url-lazy-message-1 (&rest args)
  "Just like `message', but is a no-op if called more than once a second.
Will not do anything if url-show-status is nil."
  (if (or (null url-show-status)
	  (= url-lazy-message-time
	     (setq url-lazy-message-time (nth 1 (current-time)))))
      nil
    (apply 'message args)))

(defun url-lazy-message-2 (&rest args)
  "Just like `message', but will not do anything if url-show-transfer-status
is nil."
  (if url-show-status
      (apply 'message args)
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for HTTP/1.0 MIME messages
;;; ----------------------------------
;;; These functions are the guts of the HTTP/0.9 and HTTP/1.0 transfer
;;; protocol, handling access authorization, format negotiation, the
;;; whole nine yards.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-parse-viewer-types ()
  "Create a string usable for an Accept: header from mm-mime-data"
  (let ((tmp mm-mime-data)
	mjr mnr (str ""))
    (while tmp
      (setq mnr (cdr (car tmp))
	    mjr (car (car tmp))
	    tmp (cdr tmp))
      (while mnr
	(if (> (+ (% (length str) 60)
		  (length (concat ", " mjr "/" (car (car mnr))))) 60)
	    (setq str (format "%s\r\nAccept: %s/%s" str mjr
			      (if (string= ".*" (car (car mnr))) "*"
				(car (car mnr)))))
	  (setq str (format "%s, %s/%s" str mjr
			    (if (string= ".*" (car (car mnr))) "*"
			      (car (car mnr))))))
	(setq mnr (cdr mnr))))
    (substring str 2 nil)))

(defun url-create-multipart-request (file-list)
  "Create a multi-part MIME request for all files in FILE-LIST"
  (let ((separator (current-time-string))
	(content "message/http-request")		   
	(ref-url nil))
    (setq separator
	  (concat "separator-"
		  (mapconcat
		   (function
		    (lambda (char)
		      (if (memq char url-mime-separator-chars)
			  (char-to-string char) ""))) separator "")))
    (cons separator
	  (concat
	   (mapconcat
	    (function
	     (lambda (file)
	       (concat "--" separator "\nContent-type: " content "\n\n"
		       (url-create-mime-request file ref-url)))) file-list "\n")
	   "--" separator))))

(defun url-create-message-id ()
  "Generate a string suitable for the Message-ID field of a request"
  (concat "<" (url-create-unique-id) "@" (system-name) ">"))

(defun url-create-unique-id ()
  ;; Generate unique ID from user name and current time.
  (let* ((date (current-time-string))
	 (name (user-login-name))
	 (dateinfo (and date (timezone-parse-date date)))
	 (timeinfo (and date (timezone-parse-time (aref dateinfo 3)))))
    (if (and dateinfo timeinfo)
	(concat (upcase name) "."
		(aref dateinfo 0)	; Year
		(aref dateinfo 1)	; Month
		(aref dateinfo 2)	; Day
		(aref timeinfo 0)	; Hour
		(aref timeinfo 1)	; Minute 
		(aref timeinfo 2)	; Second
		)
      (error "Cannot understand current-time-string: %s." date))
    ))
  
(defun url-create-mime-request (fname ref-url)
  "Create a MIME request for fname, referred to by REF-URL."
  (let* ((extra-headers)
	 (request nil)
	 (sessionid (cdr-safe (assoc (concat url-current-server ":"
					     url-current-port)
				     url-session-id-alist)))
	 (url (url-view-url t))
	 (no-cache (cdr-safe (assoc "Pragma" url-request-extra-headers)))
	 (auth (if (cdr-safe (assoc "Authorization" url-request-extra-headers))
		   nil
		 (url-get-authentication (or
					  (and (boundp 'proxy-info)
					       proxy-info)
					  url) nil 'any nil))))
    (setq no-cache (and no-cache (string-match "no-cache" no-cache)))
    (if auth
	(setq auth (concat "Authorization: " auth "\r\n")))

    (if (and ref-url (stringp ref-url) (or (string= ref-url "file:nil")
					   (string= ref-url "")))
	(setq ref-url nil))

    (if (or (memq url-privacy-level '(low high paranoid))
	    (and (listp url-privacy-level)
		 (memq 'lastloc url-privacy-level)))
	(setq ref-url nil))

    (setq extra-headers (mapconcat
			 (function (lambda (x)
				     (concat (car x) ": " (cdr x))))
			 url-request-extra-headers "\r\n"))
    (if (not (equal extra-headers ""))
	(setq extra-headers (concat extra-headers "\r\n")))
    (setq request
	  (format
	   (concat
	    "%s %s HTTP/1.0\r\n"	; The request
	    "MIME-Version: 1.0\r\n"	; Version of MIME we speaketh
	    "Extension: Security/Digest\r\n"
	    "Session-ID: %s\r\n"	; This session's unique ID
	    "%s"			; Who its from
	    "Accept-encoding: %s\r\n"	; Encodings we understand
	    "Accept-language: %s\r\n" 	; Languages we understand
	    "Accept: %s\r\n"		; Types we understand
	    "User-Agent: %s/%s"		; User agent
	    " URL/%s (%s ; %s)\r\n"
	    "Message-ID: %s\r\n"	; Message ID #
	    "%s"			; Authorization
	    "%s"			; If-modified-since
	    "%s"			; Where we came from
	    "%s"			; Any extra headers
	    "%s"			; Any data
	    "\r\n")			; End request
	   (or url-request-method "GET")
	   fname
	   (or sessionid url-default-session-id)
	   (if url-personal-mail-address
	       (concat "From: " url-personal-mail-address "\r\n")
	     "")
	   url-mime-encoding-string
	   url-mime-language-string
	   url-mime-accept-string
	   url-package-name
	   url-package-version
	   url-version
	   url-system-type
	   url-os-type
	   (url-create-message-id)
	   (or auth "")
	   (if (and url-current-time-string-has-args
		    (not no-cache)
		    (member url-request-method '("GET" nil)))
	       (let ((tm (url-is-cached url)))
		 (if tm
		     (concat "If-modified-since: "
			     (url-get-normalized-date tm) "\r\n")
		   ""))
	     "")
	   (if ref-url (concat "Referer: " ref-url "\r\n") "")
	   extra-headers
	   (if url-request-data
	       (format "Content-length: %d\r\n\r\n%s"
		       (length url-request-data) url-request-data)
	     "")))
    request))

(defun url-setup-reload-timer (url must-be-viewing &optional time)
  ;; Set up a timer to load URL at optional TIME.  If TIME is unspecified,
  ;; default to 5 seconds.  Only loads document if MUST-BE-VIEWING is the
  ;; current URL when the timer expires."
  (or time (setq time 5))
  (let ((func
	 (` (lambda ()
	      (if (equal (url-view-url t) (, must-be-viewing))
		  (let ((w3-reuse-buffers 'no))
		    (if (equal (, url) (url-view-url t))
			(kill-buffer (current-buffer)))
		    (w3-fetch (, url))))))))
    (cond
     ((featurep 'itimer)
      (start-itimer "reloader" func time))
     ((fboundp 'run-at-time)
      (run-at-time time nil func))
     (t
      (url-warn 'url "Cannot set up timer for automatic reload, sorry!")))))

(defun url-handle-refresh-header (reload)
  (if (and reload
	   url-honor-refresh-requests
	   (or (eq url-honor-refresh-requests t)
	       (funcall url-confirmation-func "Honor refresh request? ")))
      (let ((uri (url-view-url t)))
	(if (string-match ";" reload)
	    (progn
	      (setq uri (substring reload (match-end 0) nil)
		    reload (substring reload 0 (match-beginning 0)))
	      (if (string-match
		   "ur[li][ \t]*=[ \t]*\"*\\([^ \t\"]+\\)\"*"
		   uri)
		  (setq uri (url-match uri 1)))
	      (setq uri (url-expand-file-name uri (url-view-url t)))))
	(url-setup-reload-timer uri (url-view-url t)
				(string-to-int (or reload "5"))))))

(defun url-parse-mime-headers (&optional no-delete switch-buff)
  ;; Parse mime headers and remove them from the html
  (and switch-buff (set-buffer url-working-buffer))
  (let* ((st (point-min))
	 (nd (progn
	       (goto-char (point-min))
	       (skip-chars-forward " \t\n")
	       (if (re-search-forward "^\r*$" nil t)
		   (1+ (point))
		 (point-max))))
	 save-pos
	 status
	 hname
	 hvalu
	 result
	 )
    (narrow-to-region st (min nd (point-max)))
    (goto-char (point-min))
    (skip-chars-forward " \t\n")	; Get past any blank crap
    (skip-chars-forward "^ \t")	; Skip over the HTTP/xxx
    (setq status (read (current-buffer)); Quicker than buffer-substring, etc.
	  result (cons (cons "status" status) result))
    (end-of-line)
    (while (not (eobp))
      (skip-chars-forward " \t\n\r")
      (setq save-pos (point))
      (skip-chars-forward "^:\n\r")
      (downcase-region save-pos (point))
      (setq hname (buffer-substring save-pos (point)))
      (skip-chars-forward ": \t ")
      (setq save-pos (point))
      (skip-chars-forward "^\n\r")
      (setq hvalu (buffer-substring save-pos (point))
	    result (cons (cons hname hvalu) result)))
    (or no-delete (delete-region st (min nd (point))))
    (setq url-current-mime-type (cdr (assoc "content-type" result))
	  url-current-mime-encoding (cdr (assoc "content-encoding" result))
	  url-current-mime-viewer (mm-mime-info url-current-mime-type nil t)
	  url-current-mime-headers result
	  url-current-can-be-cached
	  (not (string-match "no-cache"
			     (or (cdr-safe (assoc "pragma" result)) ""))))
    (url-handle-refresh-header (cdr-safe (assoc "refresh" result)))
    (if (and url-request-method
	     (not (string= url-request-method "GET")))
	(setq url-current-can-be-cached nil))
    (let ((sessionid (cdr-safe (assoc "session-id" result)))
	  (node (assoc (concat url-current-server ":" url-current-port)
		       url-session-id-alist)))
      (if sessionid
	  (if node
	      (setcdr node sessionid)
	    (setq url-session-id-alist
		  (cons (cons (concat url-current-server ":" url-current-port)
			      sessionid) url-session-id-alist)))))
    (let ((expires (cdr-safe (assoc "expires" result))))
      (if (and expires url-current-can-be-cached (featurep 'timezone))
	  (progn
	    (if (string-match
		 (concat "^[^,]+, +\\(..\\)-\\(...\\)-\\(..\\) +"
			 "\\(..:..:..\\) +\\[*\\([^\]]+\\)\\]*$")
			      expires)
		(setq expires (concat (url-match expires 1) " "
				      (url-match expires 2) " "
				      (url-match expires 3) " "
				      (url-match expires 4) " ["
				      (url-match expires 5) "]")))
	    (setq expires
		  (let ((d1 (mapcar
			     (function
			      (lambda (s) (and s (string-to-int s))))
			     (timezone-parse-date
			      (current-time-string))))
			(d2 (mapcar
			     (function (lambda (s) (and s (string-to-int s))))
			     (timezone-parse-date expires))))
		    (- (timezone-absolute-from-gregorian 
			(nth 1 d1) (nth 2 d1) (car d1))
		       (timezone-absolute-from-gregorian 
			(nth 1 d2) (nth 2 d2) (car d2))))
		  url-current-can-be-cached (/= 0 expires)))))
    (cond
     ((= status 500)			; Internal server error
      (setq url-current-can-be-cached nil))
     ((= status 501)			; Facility not supported
      (setq url-current-can-be-cached nil))
     ((= status 400)			; Bad request - syntax
      (setq url-current-can-be-cached nil))
     ((and (= status 401)		; Unauthorized access, retry w/auth.
	   (< url-current-passwd-count url-max-password-attempts))
      (setq url-current-passwd-count (1+ url-current-passwd-count))
      (let* ((y (cdr (assoc "www-authenticate" result)))
	     (url (url-view-url t))
	     (type (downcase (if (string-match "[ \t]" y)
				 (substring y 0 (match-beginning 0))
			       y))))
	(cond
	 ((or (equal "pem" type) (equal "pgp" type))
	  (if (string-match "entity=\"\\([^\"]+\\)\"" y)
	      (url-fetch-with-pgp url-current-file
				  (url-match y 1) (intern type))
	    (error "Could not find entity in %s!" type)))
	 ((url-auth-registered type)
	  (let ((args y)
		(ctr (1- (length y)))
		auth
		(url-request-extra-headers url-request-extra-headers))
	    (while (/= 0 ctr)
	      (if (= ?, (aref args ctr))
		  (aset args ctr ?\;))
	      (setq ctr (1- ctr)))
	    (setq args (mm-parse-args y)
		  auth (url-get-authentication url
					       (cdr-safe (assoc "realm" args))
					       type t args))
	    (if auth
		(setq url-request-extra-headers
		      (cons (cons "Authorization" auth)
			    url-request-extra-headers)))
	    (url-retrieve url t)))
	 (t
	  (widen)
	  (goto-char (point-max))
	  (setq url-current-can-be-cached nil)
	  (insert "<hr>Sorry, but I do not know how to handle " y
		  " authentication.  If you'd like to write it,"
		  " send it to " url-bug-address ".<hr>")))))
     ((= status 401) nil)		; Tried too many times
     ((= status 402) nil)		; Payment required, retry w/Chargeto:
     ((= status 403) nil)		; Access is forbidden
     ((= status 404) nil)		; Not found...
     ((or (= status 301)		; Moved - retry with Location: header
	  (= status 302)		; Found - retry with Location: header
	  (= status 303))		; Method - retry with location/method
      (let ((x (url-view-url t))
	    (redir (or (cdr (assoc "uri" result))
		       (cdr (assoc "location" result))))
	    (redirmeth (upcase (or (cdr (assoc "method" result))
				   url-request-method
				   "get"))))
	(if (and redir (string-match "\\([^ \t]+\\)[ \t]" redir))
	    (setq redir (url-match redir 1)))
	(if (and redir (string-match "^<\\(.*\\)>$" redir))
	    (setq redir (url-match redir 1)))

	;; As per Roy Fielding, 303 maps _any_ method to a 'GET'
	(if (= 303 status)
	    (setq redirmeth "GET"))

	;; As per Roy Fielding, 301, 302 use the same method as the
	;; original request, but if != GET, user interaction is
	;; required.
	(if (and (not (string= "GET" redirmeth))
		 (not (funcall
		       url-confirmation-func
		       (concat
			"Honor redirection with non-GET method "
			"(possible security risks)? "))))
	    (progn
	      (url-warn 'url
			(format
"The URL %s tried to issue a redirect to %s using a method other than
GET, which can open up various security holes.  Please see the
HTTP/1.0 specification for more details." x redir) 'error)
	      (if (funcall url-confirmation-func
			   "Continue (with method of GET)? ")
		  (setq redirmeth "GET")
		(error "Transaction aborted."))))

	(if (not (equal x redir))
	    (let ((url-request-method redirmeth))
	      (url-maybe-relative redir))
	  (progn
	    (goto-char (point-max))
	    (insert "<hr>Error!  This URL tried to redirect me to itself!<P>"
		    "Please notify the server maintainer.")))))
     ((= status 304)			; Cached document is newer
      (message "Extracting from cache...")
      (url-extract-from-cache (url-create-cached-filename (url-view-url t))))
     ((= status 204)			; No response - leave old document
      (kill-buffer url-working-buffer))
     (t nil))				; All others indicate success
    (widen)
    status))

(defun url-lf-to-crlf (str)
  ;; Convert all linefeeds to carriage-return-line-feed pairs in string STR
  (mapconcat (function
	      (lambda (x)
		(if (= x 10) "\r\n" (char-to-string x)))) str ""))	     

(defun url-mime-response-p (&optional switch-buff)
  ;; Determine if the current buffer is a MIME response
  (and switch-buff (set-buffer url-working-buffer))
  (goto-char (point-min))
  (skip-chars-forward " \t\n")
  (and (looking-at "^HTTP/.+")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UUencoding
;;; ----------
;;; These functions are needed for the (RI)PEM encoding.  PGP can
;;; handle binary data, but (RI)PEM requires that it be uuencoded
;;; first, or it will barf severely.  How rude.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-uuencode-buffer (&optional buff)
  "UUencode buffer BUFF, with a default of the current buffer."
  (setq buff (or buff (current-buffer)))
  (save-excursion
    (set-buffer buff)
    (url-lazy-message "UUencoding...")
    (call-process-region (point-min) (point-max)
			 url-uuencode-program t t nil "url-temp-file")
    (url-lazy-message "UUencoding... done.")))

(defun url-uudecode-buffer (&optional buff)
  "UUdecode buffer BUFF, with a default of the current buffer."
  (setq buff (or buff (current-buffer)))
  (let ((newname (url-generate-unique-filename)))
    (save-excursion
      (set-buffer buff)
      (goto-char (point-min))
      (re-search-forward "^begin [0-9][0-9][0-9] \\(.*\\)$" nil t)
      (replace-match (concat "begin 600 " newname))
      (url-lazy-message "UUdecoding...")
      (call-process-region (point-min) (point-max) url-uudecode-program)
      (url-lazy-message "UUdecoding...")
      (erase-buffer)
      (mm-insert-file-contents newname)
      (url-lazy-message "UUdecoding... done.")
      (condition-case ()
	  (delete-file newname)
	(error nil)))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Decoding PGP/PEM responses
;;; --------------------------
;;; A PGP/PEM encrypted/signed response contains all the real headers,
;;; so this is just a quick decrypt-then-reparse hack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-decode-pgp/pem (arg)
  "Decode a pgp/pem response from an HTTP/1.0 server.
This expects the decoded message to contain all the necessary HTTP/1.0 headers
to correctly act on the decoded message (new content-type, etc)."
  (mc-decrypt-message)
  (url-parse-mime-headers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PGP/PEM Encryption
;;; ------------------
;;; This implements the highly secure PGP/PEM encrypted requests, as
;;; specified by NCSA and CERN.
;;;
;;; The complete online spec of this scheme was done by Tony Sanders
;;; <sanders@bsdi.com>, and can be seen at
;;; http://www.bsdi.com/HTTP:TNG/ripem-http.txt
;;;
;;; This section of code makes use of the EXCELLENT mailcrypt.el
;;; package by Jin S Choi (jsc@mit.edu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun url-public-key-exists (entity scheme)
  "Return t iff a key for ENTITY exists using public key system SCHEME.
ENTITY is the username/hostname combination we are checking for.
SCHEME is a symbol representing what public key encryption program to use.
       Currently only 'pgp (Pretty Good Privacy) and 'pem (RIPEM) are
       recognized."
  (let (retval)
    (save-excursion
      (cond
       ((eq 'pgp scheme)			; PGP encryption
	(set-buffer (get-buffer-create " *keytmp*"))
	(erase-buffer)
	(call-process mc-pgp-path nil t nil "+batchmode" "-kxaf" entity)
	(goto-char (point-min))
	(setq retval (search-forward mc-pgp-key-begin-line nil t)))
       ((eq 'pem scheme)			; PEM encryption
	(set-buffer (find-file-noselect mc-ripem-pubkeyfile))
	(goto-char (point-min))
	(setq retval (search-forward entity nil t)))
       (t
	(url-warn 'security
		  (format
		   "Bad value for SCHEME in url-public-key-exists %s"
		   scheme))))
      (kill-buffer (current-buffer)))
    retval))

(defun url-get-server-keys (entity &optional scheme)
  "Make sure the key for ENTITY exists using SCHEME.
ENTITY is the username/hostname combination to get the info for.  
       This should be a string you could pass to 'finger'.
SCHEME is a symbol representing what public key encryption program to use.
       Currently only 'pgp (Pretty Good Privacy) and 'pem (RIPEM) are
       recognized."
  (or scheme (setq scheme mc-default-scheme))
  (save-excursion
    (cond
     ((url-public-key-exists entity scheme) nil)
     (t
      (string-match "\\([^@]+\\)@\\(.*\\)" entity)
      (let ((url-working-buffer " *url-get-keys*"))
	(url-retrieve (format "gopher://%s:79/0%s/w" (url-match entity 1)
			     (url-match entity 2)))
	(mc-snarf-keys)
	(kill-buffer url-working-buffer))))))
   
(defun url-fetch-with-pgp (url recipient type)
  "Retrieve a document with public-key authentication.
      URL is the url to request from the server.
RECIPIENT is the server's entity name (usually webmaster@host)
     TYPE is a symbol representing what public key encryption program to use.
          Currently only 'pgp (Pretty Good Privacy) and 'pem (RIPEM) are
          recognized."
  (or noninteractive (require 'mailcrypt))
  (let ((request (url-create-mime-request url "PGP-Redirect"))
	(url-request-data nil)
	(url-request-extra-headers nil))
    (save-excursion
      (url-get-server-keys recipient type)
      (set-buffer (get-buffer-create " *url-encryption*"))
      (erase-buffer)
      (insert "\n\n" mail-header-separator "\n" request)
      (mc-encrypt-message recipient type)
      (goto-char (point-min))
      (if (re-search-forward (concat "\n" mail-header-separator "\n") nil t)
	  (delete-region (point-min) (point)))
      (setq url-request-data (buffer-string)
	    url-request-extra-headers
	    (list (cons "Authorized" (format "%s entity=\"%s\""
					     (cond
					      ((eq type 'pgp) "PGP")
					      ((eq type 'pem) "PEM"))
					     url-pgp/pem-entity))
		  (cons "Content-type" (format "application/x-www-%s-reply"
					       (cond
						((eq type 'pgp) "pgp")
						((eq type 'pem) "pem")))))))
    (kill-buffer " *url-encryption*")
    (url-retrieve (url-expand-file-name "/") t)))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gopher and Gopher+ support
;;; --------------------------
;;; Here come a few gross hacks that I call gopher and gopher+ support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-convert-ask-to-form (ask)
  ;; Convert a Gopher+ ASK block into a form.  Returns a string to be
  ;; inserted into a buffer to create the form."
  (let ((form (concat "<form enctype=application/gopher-ask-block\n"
		      "      method=\"GOPHER-ASK\">\n"
		      " <ul plain>\n"))
	(type "")
	(x 0)
	(parms ""))
    (while (string-match "^\\([^:]+\\): +\\(.*\\)" ask)
      (setq parms (url-match ask 2)
	    type (url-strip-leading-spaces (downcase (url-match ask 1)))
	    x (1+ x)
	    ask (substring ask (if (= (length ask) (match-end 0))
				   (match-end 0) (1+ (match-end 0))) nil))
      (cond
       ((string= "note" type) (setq form (concat form parms)))
       ((or (string= "ask" type)
	    (string= "askf" type)
	    (string= "choosef" type))
	(setq parms (url-string-to-tokens parms ?\t)
	      form (format "%s\n<li>%s<input name=\"%d\" value=\"%s\">"
			   form (or (nth 0 parms) "Text:")
			   x (or (nth 1 parms) ""))))
       ((string= "askp" type)
	(setq parms (mapcar 'car (nreverse (url-split parms "\t")))
	      form (format
		    "%s\n<li>%s<input name=\"%d\" type=\"password\" value=\"%s\">"
		    form			   ; Earlier string
		    (or (nth 0 parms) "Password:") ; Prompt
		    x				   ; Name
		    (or (nth 1 parms) "") 	   ; Default value
		    )))
       ((string= "askl" type)
	(setq parms (url-string-to-tokens parms ?\t)
	      form (format "%s\n<li>%s<textarea name=\"%d\">%s</textarea>"
			   form			 ; Earlier string
			   (or (nth 0 parms) "") ; Prompt string
			   x			 ; Name
			   (or (nth 1 parms) "") ; Default value
			   )))
       ((or (string= "select" type)
	    (string= "choose" type))
	(setq parms (url-string-to-tokens parms ?\t)
	      form (format "%s\n<li>%s<select name=\"%d\">" form (car parms) x)
	      parms (cdr parms))
	(if (null parms) (setq parms (list "Yes" "No")))
	(while parms
	  (setq form (concat form "<option>" (car parms) "\n")
		parms (cdr parms)))
	(setq form (concat form "</select>")))))
    (concat form "\n<li><input type=\"SUBMIT\""
	    " value=\"Submit Gopher+ Ask Block\"></ul></form>")))

(defun url-grok-gopher-line ()
  "Return a list of link attributes from a gopher string.  Order is:
title, type, selector string, server, port, gopher-plus?"
  (let (type selector server port gopher+ st nd)
    (beginning-of-line)
    (setq st (point))
    (end-of-line)
    (setq nd (point))
    (save-excursion
      (mapcar (function
	       (lambda (var)
		 (goto-char st)
		 (skip-chars-forward "^\t\n" nd)
		 (set-variable var (buffer-substring st (point)))
		 (setq st (1+ (point)))))
	      '(type selector server port))
      (setq gopher+ (and (/= (1- st) nd) (buffer-substring st nd)))
      (list type (concat (substring type 0 1) selector) server port gopher+))))

(defun url-format-gopher-link (gophobj)
  ;; Insert a gopher link as an <A> tag
  (let ((title (nth 0 gophobj))
	(ref   (nth 1 gophobj))
	(type  (if (> (length (nth 0 gophobj)) 0)
		   (substring (nth 0 gophobj) 0 1) ""))
	(serv  (nth 2 gophobj))
	(port  (nth 3 gophobj))
	(plus  (nth 4 gophobj))
	(desc  nil))
    (if (and (equal type "")
	     (> (length title) 0))
	(setq type (substring title 0 1)))
    (setq title (and title (substring title 1 nil))
	  title (mapconcat
		 (function
		  (lambda (x)
		    (cond
		     ((= x ?&) "&amp;")
		     ((= x ?<) "&lt;");
		     ((= x ?>) "&gt;");
		     (t (char-to-string x))))) title "")
	  desc (or (cdr (assoc type url-gopher-labels)) "(UNK)"))
    (cond
     ((null ref) "")
     ((equal type "8")
      (format "<LI> %s <A HREF=\"telnet://%s:%s/\">%s</A>\n"
 	      desc serv port title))
     ((equal type "T")
      (format "<LI> %s <A HREF=\"tn3270://%s:%s/\">%s</A>\n"
	      desc serv port title))
     (t (format "<LI> %s <A METHODS=%s HREF=\"gopher://%s:%s/%s\">%s</A>\n"
		desc type serv (concat port plus)
		(url-hexify-string ref) title)))))

(defun url-gopher-clean-text (&optional buffer)
  "Decode text transmitted by gopher.
0. Delete status line.
1. Delete `^M' at end of line.
2. Delete `.' at end of buffer (end of text mark).
3. Delete `.' at beginning of line.   (does gopher want this?)"
  (set-buffer (or buffer url-working-buffer))
  ;; Insert newline at end of buffer.
  (goto-char (point-max))
  (if (not (bolp))
      (insert "\n"))
  ;; Delete `^M' at end of line.
  (goto-char (point-min))
  (while (re-search-forward "\r[^\n]*$" nil t)
    (replace-match ""))
;  (goto-char (point-min))
;  (while (not (eobp))
;    (end-of-line)
;    (if (= (preceding-char) ?\r)
;       (delete-char -1))
;    (forward-line 1)
;    )
  ;; Delete `.' at end of buffer (end of text mark).
  (goto-char (point-max))
  (forward-line -1)                     ;(beginning-of-line)
  (while (looking-at "^\\.$")
    (delete-region (point) (progn (forward-line 1) (point)))
    (forward-line -1))
  ;; Replace `..' at beginning of line with `.'.
  (goto-char (point-min))
  ;; (replace-regexp "^\\.\\." ".")
  (while (search-forward "\n.." nil t)
    (delete-char -1))
  )

(defun url-parse-gopher (&optional buffer)
  (save-excursion
    (set-buffer (or buffer url-working-buffer))
    (url-replace-regexp "^\r*$\n" "")
    (url-replace-regexp "^\\.\r*$\n" "")
    (url-gopher-clean-text (current-buffer))
    (goto-char (point-max))
    (skip-chars-backward "\n\r\t ")
    (delete-region (point-max) (point))
    (insert "\n")
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (delete-region (point-min) (point))
    (let* ((len (count-lines (point-min) (point-max)))
	   (objs nil)
	   (i 0))
      (while (not (eobp))
	(setq objs (cons (url-grok-gopher-line) objs)
	      i (1+ i))
	(url-lazy-message "Converting gopher listing... %d/%d (%d%%)"
			  i len (url-percentage i len))
						
	(forward-line 1))
      (setq objs (nreverse objs))
      (erase-buffer)
      (insert "<title>"
	      (cond
	       ((or (string= "" url-current-file)
		    (string= "1/" url-current-file)
		    (string= "1" url-current-file))
		(concat "Gopher root at " url-current-server))
	       ((string-match (format "^[%s]+/" url-gopher-types)
			      url-current-file)
		(substring url-current-file 2 nil))
	       (t url-current-file))
	      "</title><ol>"
	      (mapconcat 'url-format-gopher-link objs "")
	      "</ol>"))))

(defun url-gopher-retrieve (host port selector &optional wait-for)
  ;; Fetch a gopher object and don't mess with it at all
  (let ((proc (url-open-stream "*gopher*" url-working-buffer
			      host (if (stringp port) (string-to-int port)
				     port)))
	(len nil)
	(parsed nil))
    (url-clear-tmp-buffer)
    (setq url-current-file selector
	  url-current-port port
	  url-current-server host
	  url-current-type "gopher")
    (if (> (length selector) 0)
	(setq selector (substring selector 1 nil)))
    (if (stringp proc)
	(message "%s" proc)
      (save-excursion
	(process-send-string proc (concat selector "\r\n"))
	(while (and (or (not wait-for)
			(progn
			  (goto-char (point-min))
			  (not (re-search-forward wait-for nil t))))
		    (memq (url-process-status proc) '(run open)))
	  (if (not parsed)
	      (cond
	       ((and (eq ?+ (char-after 1))
		     (memq (char-after 2)
			   (list ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
		(setq parsed (copy-marker 2)
		      len (read parsed))
		(delete-region (point-min) parsed))
	       ((and (eq ?+ (char-after 1))
		     (eq ?- (char-after 2)))
		(setq len nil
		      parsed t)
		(goto-char (point-min))
		(delete-region (point-min) (progn
					     (end-of-line)
					     (point))))
	       ((and (eq ?- (char-after 1))
		     (eq ?- (char-after 2)))
		(setq parsed t
		      len nil)
		(goto-char (point-min))
		(delete-region (point-min) (progn
					     (end-of-line)
					     (point))))))
	  (if len (url-lazy-message "Read %d of %d bytes (%d%%)" (point-max)
				    len
				    (url-percentage (point-max) len))
	    (url-lazy-message "Read %d bytes." (point-max)))
	  (url-accept-process-output proc))
	(condition-case ()
	    (url-kill-process proc)
	  (error nil))
	(url-replace-regexp "\n*Connection closed.*\n*" "")
	(url-replace-regexp "\n*Process .*gopher.*\n*" "")
	(while (looking-at "\r") (delete-char 1))))))

(defun url-do-gopher-cso-search (descr)
  ;; Do a gopher CSO search and return a plaintext document
  (let ((host (nth 0 descr))
	(port (nth 1 descr))
	(file (nth 2 descr))
	search-type search-term)
    (string-match "search-by=\\([^&]+\\)" file)
    (setq search-type (url-match file 1))
    (string-match "search-term=\\([^&]+\\)" file)
    (setq search-term (url-match file 1))
    (url-gopher-retrieve host port (format "2query %s=%s"
					  search-type search-term) "^[2-9]")
    (goto-char (point-min))
    (url-replace-regexp "^-[0-9][0-9][0-9]:[0-9]*:" "")
    (url-replace-regexp "^[^15][0-9][0-9]:.*" "")
    (url-replace-regexp "^[15][0-9][0-9]:\\(.*\\)" "<H1>\\1</H1>&ensp;<PRE>")
    (goto-char (point-min))
    (insert "<title>Results of CSO search</title>\n"
	    "<h1>" search-type " = " search-term "</h1>\n")
    (goto-char (point-max))
    (insert "</pre>")))

(defun url-do-gopher (descr)
  ;; Fetch a gopher object
  (let ((host (nth 0 descr))
	(port (nth 1 descr))
	(file (nth 2 descr))
	(type (nth 3 descr))
	(extr (nth 4 descr))
	parse-gopher)
    (cond
     ((and				; Gopher CSO search
       (equal type "www/gopher-cso-search")
       (string-match "search-by=" file)) ; With a search term in it
      (url-do-gopher-cso-search descr)
      (setq type "text/html"))
     ((equal type "www/gopher-cso-search") ; Blank CSO search
      (url-clear-tmp-buffer)
      (insert "<html>\n"
	      " <head>\n"
	      "  <title>CSO Search</title>\n"
	      " </head>\n"
	      " <body>\n"
	      "  <div1>\n"
	      "   <h1>This is a CSO search</h1>\n"
	      "   <hr>\n"
	      "   <form>\n"
	      "    <ul>\n"
	      "     <li> Search by: <select name=\"search-by\">\n"
	      "                      <option>Name\n"
	      "                      <option>Phone\n"
	      "                      <option>Email\n"
	      "                      <option>Address\n"
	      "                     </select>\n"
	      "     <li> Search for: <input name=\"search-term\">\n"
	      "     <li> <input type=\"submit\" value=\"Submit query\">\n"
	      "    </ul>\n"
	      "   </form>\n"
	      "  </div1>\n"
	      " </body>\n"
	      "</html>\n"
	      "<!-- Automatically generated by URL v" url-version " -->\n")
      (setq type "text/html"
	    parse-gopher t))
     ((and
       (equal type "www/gopher-search")	; Ack!  Mosaic-style search href
       (string-match "\t" file))	; and its got a search term in it!
      (url-gopher-retrieve host port file)
      (setq type "www/gopher"
	    parse-gopher t))
     ((and
       (equal type "www/gopher-search")	; Ack!  Mosaic-style search href
       (string-match "\\?" file))	; and its got a search term in it!
      (setq file (concat (substring file 0 (match-beginning 0)) "\t"
			 (substring file (match-end 0) nil)))
      (url-gopher-retrieve host port file)
      (setq type "www/gopher"
	    parse-gopher t))
     ((equal type "www/gopher-search")	; Ack!  Mosaic-style search href
      (setq type "text/html"
	    parse-gopher t)
      (url-clear-tmp-buffer)
      (insert "<html>\n"
	      " <head>\n"
	      "  <title>Gopher Server</title>\n"
	      " </head>\n"
	      " <body>\n"
	      "  <div1>\n"
	      "   <h1>Searchable Gopher Index</h1>\n"
	      "   <hr>\n"
	      "   <p>\n"
	      "    Enter the search keywords below\n"
	      "   </p>"
	      "   <form enctype=\"application/x-gopher-query\">\n"
	      "    <input name=\"internal-gopher\">\n"
	      "   </form>\n"
	      "   <hr>\n"
	      "  </div1>\n"
	      " </body>\n"
	      "</html>\n"
	      "<!-- Automatically generated by URL v" url-version " -->\n"))
     ((null extr)			; Normal Gopher link
      (url-gopher-retrieve host port file)
      (setq parse-gopher t))
     ((eq extr 'gopher+)		; A gopher+ link
      (url-gopher-retrieve host port (concat file "\t+"))
      (setq parse-gopher t))
     ((eq extr 'ask-block)		; A gopher+ interactive query
      (url-gopher-retrieve host port (concat file "\t!")) ; Fetch the info
      (goto-char (point-min))
      (cond
       ((re-search-forward "^\\+ASK:[ \t\r]*" nil t) ; There is an ASK
	(let ((x (buffer-substring (1+ (point))
				   (or (re-search-forward "^\\+[^:]+:" nil t)
				       (point-max)))))
	  (erase-buffer)
	  (insert (url-convert-ask-to-form x))
	  (setq type "text/html" parse-gopher t)))
       (t (setq parse-gopher t)))))
    (if (or (equal type "www/gopher")
	    (equal type "text/plain")
	    (equal file "")
	    (equal type "text/html"))
	(url-gopher-clean-text))
    (if (and parse-gopher (or (equal type "www/gopher")
			      (equal file "")))
	(progn
	  (url-parse-gopher)
	  (setq type "text/html"
		url-current-mime-viewer (mm-mime-info type nil 5))))
    (setq url-current-mime-type (or type "text/plain")
	  url-current-mime-viewer (mm-mime-info type nil 5)
	  url-current-file file
	  url-current-port port
	  url-current-server host
	  url-current-type "gopher")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WAIS support
;;; ------------
;;; Here are even more gross hacks that I call native WAIS support.
;;; This code requires a working waisq program that is fully
;;; compatible with waisq from think.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-create-wais-source (server port dbase)
  ;; Create a temporary wais source description file.  Returns the
  ;; file name the description is in.
  (let ((x (url-generate-unique-filename))
	(y (get-buffer-create " *waisq-tmp*")))
    (save-excursion
      (set-buffer y)
      (erase-buffer)
      (insert 
       (format
	(concat "(:source\n:version 3\n"
		":ip-name \"%s\"\n:tcp-port %s\n"
		":database-name \"%s\"\n)")
	server (if (equal port "") "210" port) dbase))
      (write-region (point-min) (point-max) x nil nil)
      (kill-buffer y))
    x))

(defun url-wais-stringtoany (str)
  ;; Return a wais subelement that specifies STR in any database
  (concat "(:any :size " (length str) " :bytes #( "
	  (mapconcat 'identity str " ")
	  " ) )"))

;(defun url-retrieve-wais-docid (server port dbase local-id)
;  (call-process "waisretrieve" nil url-working-buffer nil
;		(format "%s:%s@%s:%s" (url-unhex-string local-id)
;			dbase server port)))

;(url-retrieve-wais-docid "quake.think.com" "210" "directory-of-servers"
;			"0 2608 /proj/wais/wais-sources/vpiej-l.src")
(defun url-retrieve-wais-docid (server port dbase local-id)
  ;; Retrieve a wais document.
  ;; SERVER is the server the database is on (:ip-name in source description)
  ;; PORT is the port number to contact (:tcp-port in the source description)
  ;; DBASE is the database name (:database-name in the source description)
  ;; LOCAL-ID is the document (:original-local-id in the question description)
  (let* ((dbf (url-create-wais-source server port dbase))
	 (qstr (format
		(concat "(:question :version 2\n"
			"           :result-documents\n"
			"           ( (:document-id\n"
			"              :document\n"
			"              (:document\n"
			"               :headline \"\"\n"
			"               :doc-id\n"
			"               (:doc-id :original-database %s\n"
			"                :original-local-id %s )\n"
			"               :number-of-bytes -1\n"
			"               :type \"\"\n"
			"               :source\n"
			"               (:source-id :filename \"%s\") ) ) ) )")
		(url-wais-stringtoany dbase)
		(url-wais-stringtoany (url-unhex-string local-id))
		dbf))
	 (qf (url-generate-unique-filename)))
    (set-buffer (get-buffer-create url-working-buffer))
    (insert qstr)
    (write-region (point-min) (point-max) qf nil nil)
    (erase-buffer)
    (call-process url-waisq-prog nil url-working-buffer nil "-f" qf "-v" "1")
    (save-excursion
      (set-buffer url-working-buffer)
      (setq url-current-file (url-unhex-string local-id)))
    (condition-case ()
	(delete-file dbf)
      (error nil))
    (condition-case ()
	(delete-file qf)
      (error nil))))

;(url-perform-wais-query "quake.think.com" "210" "directory-of-servers" "SGML")
(defun url-perform-wais-query (server port dbase search)
  ;; Perform a wais query.
  ;; SERVER is the server the database is on (:ip-name in source description)
  ;; PORT is the port number to contact (:tcp-port in the source description)
  ;; DBASE is the database name (:database-name in the source description)
  ;; SEARCH is the search term (:seed-words in the question description)"
  (let ((dbfname (url-create-wais-source server port dbase))
	(qfname (url-generate-unique-filename))
	(results 'url-none-gotten))
    (save-excursion
      (url-clear-tmp-buffer)
      (insert
       (format
	(concat "(:question\n"
		" :version 2\n"
		" :seed-words \"%s\"\n"
		" :sourcepath \"" url-temporary-directory "\"\n"
		" :sources\n"
		" (  (:source-id\n"
		"     :filename \"%s\"\n"
		"    )\n"
		" )\n"
		" :maximum-results 100)\n")
	search dbfname))
      (write-region (point-min) (point-max) qfname nil nil)
      (erase-buffer)
      (call-process url-waisq-prog nil url-working-buffer nil "-g" "-f" qfname)
      (set-buffer url-working-buffer)
      (erase-buffer)
      (setq url-current-server server
	    url-current-port port
	    url-current-file dbase)
      (mm-insert-file-contents qfname)
      (goto-char (point-min))
      (if (re-search-forward "(:question" nil t)
	  (delete-region (point-min) (match-beginning 0)))
      (url-replace-regexp "Process.*finished.*" "")
      (subst-char-in-region (point-min) (point-max) 35 32)
      (goto-char (point-min))
      (message "Done reading info - parsing results...")
      (if (re-search-forward ":result-documents[^(]+" nil t)
	  (progn
	    (goto-char (match-end 0))
	    (while (eq results 'url-none-gotten)
	      (condition-case ()
		  (setq results (read (current-buffer)))
		(error (progn
			 (setq results 'url-none-gotten)
			 (goto-char (match-end 0))))))
	    (erase-buffer)
	    (insert "<title>Results of WAIS search</title>\n"
		    "<h1>Searched " dbase " for " search "</h1>\n"
		    "<hr>\n"
		    "Found <b>" (int-to-string (length results))
		    "</b> matches.\n"
		    "<ol>\n<li>"
		    (mapconcat 'url-parse-wais-doc-id results "\n<li>")
		    "\n</ol>\n<hr>\n"))
	(message "No results"))
      (setq url-current-mime-type "text/html")
      (condition-case ()
	  (delete-file qfname)
	(error nil))
      (condition-case ()
	  (delete-file dbfname)
	(error nil)))))

(defun url-wais-anytostring (x)
  ;; Convert a (:any ....) wais construct back into a string.
  (mapconcat 'char-to-string (car (cdr (memq ':bytes x))) ""))

(defun url-parse-wais-doc-id (x)
  ;; Return a list item that points at the doc-id specified by X
  (let* ((document (car (cdr (memq ':document x))))
	 (doc-id (car (cdr (memq ':doc-id document))))
	 (score (car (cdr (memq ':score x)))) 
	 (title (car (cdr (memq ':headline document))))
	 (type (car (cdr (memq ':type document))))
	 (size (car (cdr (memq ':number-of-bytes document))))
	 (server (car (cdr (memq ':original-server doc-id))))
	 (dbase (car (cdr (memq ':original-database doc-id))))
	 (localid (car (cdr (memq ':original-local-id doc-id))))
	 (dist-server (car (cdr (memq ':distributor-server doc-id))))
	 (dist-dbase (car (cdr (memq ':distributor-database doc-id))))
	 (dist-id (car (cdr (memq ':distributor-local-id doc-id))))
	 (copyright (or (car (cdr (memq ':copyright-disposition doc-id))) 0)))
    (format "<a href=\"wais://%s:%s/%s/%s/%d/1=%s;2=%s;3=%s;4=%s;5=%s;6=%s;7=%d;\">%s (Score = %s)</a>"
	    url-current-server url-current-port url-current-file
	    type size
	    (url-hexify-string (url-wais-anytostring server))
	    (url-hexify-string (url-wais-anytostring dbase))
	    (url-hexify-string (url-wais-anytostring localid))
	    (url-hexify-string (url-wais-anytostring dist-server))
	    (url-hexify-string (url-wais-anytostring dist-dbase))
	    (url-hexify-string (url-wais-anytostring dist-id))
	    copyright title score)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grokking different types of URLs
;;; --------------------------------
;;; Different functions for parsing out URLs, based on the type of
;;; link (http/wais/etc).  These must be passed a fully qualified URL.
;;; All the functions do their best to handle bad/ugly URLs, but
;;; nothing is perfect.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-grok-wais-href (url)
  "Return a list of server, port, database, search-term, doc-id"
  (if (string-match "wais:/+\\([^/:]+\\):*\\([^/]*\\)/+\\(.*\\)" url)
      (let ((host (url-match url 1))
	    (port (url-match url 2))
	    (data (url-match url 3)))
	(list host port data))
    (make-list 3 nil)))

(defun url-grok-gopher-href (url)
  "Return a list of attributes from a gopher url.  List is of the
type: host port selector-string MIME-type extra-info"
  (let (host				; host name
	port				; Port #
	selector			; String to send to gopher host
	type				; MIME type
	extra				; Extra information
	x				; Temporary storage for host/port
	y				; Temporary storage for selector
	ylen
	)
    (or (string-match "gopher:/*\\([^/]+\\)\\(/*\\)" url)
	(error "Can't understand url %s" url))
    (setq x (url-match url 1)		; The host (and possible port #)
	  ylen (- (length url) (match-end 2))
	  y (if (= ylen 0)		; The selector (and possible type)
		""
		(url-unhex-string (substring url (- ylen)))))

    ;First take care of the host/port/gopher+ information from the url
    ;A + after the port # (host:70+) specifies a gopher+ link
    ;A ? after the port # (host:70?) specifies a gopher+ ask block
    (if (string-match "^\\([^:]+\\):\\([0-9]+\\)\\([?+]*\\)" x)
	(setq host (url-match x 1)
	      port (url-match x 2)
	      extra (url-match x 3))
      (setq host x
	    port "70"
	    extra nil))
    (cond
     ((equal extra "")  (setq extra nil))
     ((equal extra "?") (setq extra 'ask-block))
     ((equal extra "+") (setq extra 'gopher+)))

    ; Next, get the type/get rid of the Mosaic double-typing. Argh.
    (setq x (string-to-char y)		; Get gopher type
	  selector (if (or url-use-hypertext-gopher
			   (< 3 (length y)))
		       y		; Get the selector string
		     (substring y 1 nil))
	  type (cdr (assoc x url-gopher-to-mime)))
    (list host port (or selector "") type extra)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing/updating the user's .newsrc file
;;; ----------------------------------------
;;; Large parts of this code are based on the newsrc parsing of the
;;; lucid emacs version of GNUS, and is very fast and efficient.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-parse-newsrc (&optional newsrc-file)
  ;; Parse out a newsrc.  This was largely yanked out of gnus
  (save-excursion
    (setq newsrc-file (or newsrc-file (expand-file-name
				       (concat "~/.newsrc" url-news-server))))
    (if (and (file-exists-p newsrc-file)
	     (file-readable-p newsrc-file))
	(message "Using newsrc file %s... " newsrc-file)
      (setq newsrc-file (expand-file-name "~/.newsrc")))
    (or (file-exists-p newsrc-file)
	(file-readable-p newsrc-file)
	(error "%s could not be read." newsrc-file))
    (set-buffer (get-buffer-create " *newsrc*"))
    (erase-buffer)
    (mm-insert-file-contents newsrc-file)
    (url-replace-regexp "^[ \t]options.*\n" "")
    (let ((subscribe nil)
	  (read-list nil)
	  newsgroup
	  p p2)
      (save-restriction
	(while (not (eobp))
	  (cond
	   ((= (following-char) ?\n)
	    ;; skip blank lines
	    nil)
	   (t
	    (setq p (point))
	    (skip-chars-forward "^:!\n")
	    (if (= (following-char) ?\n)
		(error "unparsable line in %s" (buffer-name)))
	    (setq p2 (point))
	    (skip-chars-backward " \t")
	    (setq newsgroup (read (buffer-substring p (point))))
	    (goto-char p2)

	    (setq subscribe (= (following-char) ?:))
	    (setq read-list nil)

	    (forward-char 1)		; after : or !
	    (skip-chars-forward " \t")
	    (while (not (= (following-char) ?\n))
	      (skip-chars-forward " \t")
	      (or
	       (and (cond
		     ((looking-at "\\([0-9]+\\)-\\([0-9]+\\)") ; a range
		      (setq read-list
			    (cons
			     (cons
			      (progn
				;; faster than buffer-substring/string-to-int
				(narrow-to-region (point-min) (match-end 1))
				(read (current-buffer)))
			      (progn
				(narrow-to-region (point-min) (match-end 2))
				(forward-char) ; skip over "-"
				(prog1
				    (read (current-buffer))
				  (widen))))
			     read-list))
		      t)
		     ((looking-at "[0-9]+")
		      ;; faster than buffer-substring/string-to-int
		      (narrow-to-region (point-min) (match-end 0))
		      (setq p (read (current-buffer)))
		      (widen)
		      (setq read-list (cons (cons p p) read-list))
		      t)
		     (t
		      ;; bogus chars in ranges
		      nil))
		    (progn
		      (goto-char (match-end 0))
		      (skip-chars-forward " \t")
		      (cond ((= (following-char) ?,)
			     (forward-char 1)
			     t)
			    ((= (following-char) ?\n)
			     t)
			    (t
			     ;; bogus char after range
			     nil))))
	       ;; if we get here, the parse failed
	       (progn
		 (end-of-line)		; give up on this line
		 (ding)
		 (message "Ignoring bogus line for %s in %s"
			  newsgroup (buffer-name))
		 (sleep-for 1)
		 )))
	    (put 'url-newsrc newsgroup (cons subscribe (nreverse read-list)))))
	  (forward-line 1))))
    (kill-buffer (current-buffer))
    (put 'url-newsrc 'parsed t)))

(defun url-save-newsrc (&optional fname)
  ;; Save the newsrc of the user
  (set-buffer (get-buffer-create " *newsrc*"))
  (erase-buffer)
  (mm-insert-file-contents (or fname (expand-file-name "~/.newsrc")))
  (goto-char (point-min))
  (delete-non-matching-lines "^[ \t]options")	; preserve option lines
  (goto-char (point-max))
  (let ((grps (symbol-plist 'url-newsrc)) grp info)
    (while grps
      (setq grp (car grps)
	    info (car (cdr grps))
	    grps (cdr (cdr grps)))
      (if (eq grp 'parsed)
	  nil
	(insert (symbol-name grp) (if (car info) ": " "! ")
		(mapconcat
		 (function
		  (lambda (range)
		    (cond
		     ((consp range) (concat (car range) "-" (cdr range)))
		     ((numberp range) range)))) (cdr info) ",") "\n")))))
		     
(defun url-retrieve-newsgroup (group &optional show-all howmany)
  ;; Select newsgroup NEWSGROUP and return a list of headers of the remaining
  ;; articles
  (or (get 'url-newsrc 'parsed) (url-parse-newsrc))
  (if (symbolp group) (setq group (symbol-name group)))
  (let ((stat
	 (cond
	  ((string-match "flee" nntp-version)
	   (nntp/command "GROUP" group)
	   (save-excursion
	     (set-buffer nntp-server-buffer)
	     (while (progn
		      (goto-char (point-min))
		      (not (re-search-forward
			    "[0-9]+[ \t]+[0-9]+[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)" nil t)))
	       (url-accept-process-output nntp/connection))
	     (cons (string-to-int
		    (buffer-substring (match-beginning 1) (match-end 1)))
		   (string-to-int
		    (buffer-substring (match-beginning 2) (match-end 2))))))
	  (t
	   (nntp-request-group group)
	   (let ((msg (nntp-status-message)))
	     (if (string-match "[0-9]+[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)"
			       msg)
		 (cons (string-to-int (url-match msg 1))
		       (string-to-int (url-match msg 2)))
	       (cons 0 0))))))
	(info (cdr (get 'url-newsrc (read group))))
	(seqs '())
	(temp nil)
	(last nil)			; last unread article
	)
    (setq last (car stat))
    (url-lazy-message "Finding unread articles...")
    (if show-all
	(setq seqs (url-make-sequence (car stat) (cdr stat)))
      (while info
	(setq temp (car info)
	      info (cdr info))
	(cond
	 ((consp temp)			; a range of articles
	  (setq seqs (nconc seqs (url-make-sequence last (1- (car temp))))
		last (1+ (cdr temp))))
	 ((numberp temp)
	  (setq seqs (nconc seqs (url-make-sequence last (1- temp)))
		last (1+ temp))))))
    (setq seqs (nconc seqs (url-make-sequence last (cdr stat))))
    (and seqs (nntp-retrieve-headers seqs))))

(defun url-get-new-newsgroups (&optional tm)
  ;; Get a string suitable for an NTTP server to get a list of new newsgroups.
  ;; Optional argument TM is a list of three integers. The first has the
  ;; most significant 16 bits of the seconds, while the second has the
  ;; least significant 16 bits.  The third integer gives the microsecond
  ;; count.  (The format returned either by (current-time) or file-attributes
  ;; mod-time, etc.)
  (let* ((x (if url-current-time-string-has-args
		(current-time-string tm)
	      (current-time-string)))
	 (y (cdr (assoc (substring x 4 7) monthabbrev-alist)))
	 (z (substring x 9 10)))
    (concat "NEWGROUPS "
	    (substring x -2 nil)
	    (if (< y 10) "0" "")
	    y
	    (if (= (length z) 2) "" "0")
	    z " "
	    (substring x 11 13)
	    (substring x 14 16)
	    (substring x 17 19))))
	  
(defun url-format-news ()
  (url-clear-tmp-buffer)
  (insert "HTTP/1.0 200 Retrieval OK\r\n"
	  (save-excursion
	    (set-buffer nntp-server-buffer)
	    (buffer-string)))
  (url-parse-mime-headers)
  (let ((from  (cdr (assoc "from" url-current-mime-headers)))
	(subj  (cdr (assoc "subject" url-current-mime-headers)))
	(org   (cdr (assoc "organization" url-current-mime-headers)))
	(typ   (or (cdr (assoc "content-type" url-current-mime-headers))
		   "text/plain"))
	(grps  (mapcar 'car
		       (url-split
			(or (cdr (assoc "newsgroups" url-current-mime-headers))
			    "")
			"[ \t\n,]+")))
	(refs  (mapcar 'car
		       (url-split
			(or (cdr (assoc "references" url-current-mime-headers))
			    "")
			"[ \t,\n<>]+")))
	(date  (cdr (assoc "date" url-current-mime-headers))))
    (setq url-current-file ""
	  url-current-type "")
    (if (or (not (string-match "text/" typ))
	    (string-match "text/html" typ))
	nil				; Let natural content-type take over
      (insert "<html>\n"
	      " <head>\n"
	      "  <title>" subj "</title>\n"
	      "  <link rev=\"made\" href=\"mailto:" from "\">\n"
	      " </head>\n"
	      " <body>\n"
	      "  <div1>\n"
	      "   <h1 align=center>" subj "</h1>\n"
	      "   <p role=\"headers\">\n"
	      "    <b>From</b>: <address> " from "</address><br>\n"
	      "    <b>Newsgroups</b>: "
	      (mapconcat
	       (function
		(lambda (grp)
		  (concat "<a href=\"" grp "\"> " grp "</a>"))) grps ", ")
	      "<br>\n"
	      (if org
		  (concat
		   "    <b>Organization</b>: <i> " org "</i> <br>\n")
		"")
	      "    <b>Date</b>: <date> " date "</date> <br>\n"
	      "   </p> <hr>\n"
	      (if (null refs)
		  ""
		(concat
		 "   <p align=\"center\">References\n"
		 "    <ol>\n"
		 (mapconcat
		  (function
		   (lambda (ref)
		     (concat "     <li> <a href=\"" ref "\"> " 
			     ref "</a></li>\n")))
		  refs "")
		 "    </ol>\n"
		 "   <hr>\n"))
	      "   <ul plain>\n"
	      "    <li><a href=\"newspost:disfunctional\"> "
	      "Post to this group </a></li>\n"
	      "    <li><a href=\"mailto:" from "\"> Reply to " from
	      "</a></li>\n"
	      "   </ul>\n"
	      "   <hr>"
	      "   <xmp>\n")
      (goto-char (point-max))
      (setq url-current-mime-type "text/html"
	    url-current-mime-viewer (mm-mime-info url-current-mime-type nil 5))
      (let ((x (assoc "content-type" url-current-mime-headers)))
	(if x
	    (setcdr x "text/html")
	  (setq url-current-mime-headers (cons (cons "content-type"
						     "text/html")
					       url-current-mime-headers))))
      (insert "\n"
	      "   </xmp>\n"
	      "  </div1>\n"
	      " </body>\n"
	      "</html>\n"
	      "<!-- Automatically generated by URL/" url-version
	      "-->"))))

(defun url-format-whole-newsgroup (newsgroup header-list)
  (url-clear-tmp-buffer)
  (insert "<html>\n"
	  " <head>\n"
	  "  <title>" newsgroup "</title>\n"
	  " </head>\n"
	  " <body>\n"
	  "  <div1>\n"
	  "   <h1 align=center>" newsgroup "</h1>\n"
	  "   <hr>\n"
	  "   <p>\n"
	  "   <ol>\n"
	  (mapconcat
	   (function
	    (lambda (artcl)
	      (let ((id (nntp-header-id artcl))
		    (subj (nntp-header-subject artcl))
		    (from (nntp-header-from artcl)))
		(if (string-match "<\\(.*\\)>" id)
		    (setq id (url-match id 1)))
		(concat "    <li> <a href=\"" id "\"> " subj "</a> <br>\n"
			"         " from " </li>\n")))) header-list "")
	  "   </ol>\n"
	  "  </div1>\n"
	  " </body>\n"
	  "</html>\n"
	  "<!-- Automatically generated by URL/" url-version
	  "-->"))

(defun url-show-all-newsgroups ()
  (or (get 'url-newsrc 'parsed) (url-parse-newsrc))
  (let ((grps (symbol-plist 'url-newsrc))
	grp info)
    (insert "<html>\n"
	    " <head>\n"
	    "  <title> Newsgroups </title>\n"
	    " </head>\n"
	    " <body>\n"
	    "  <div1>\n"
	    "   <h1> Newsgroup listing </h1>\n"
	    "   <pre>\n")
    (while grps
      (setq grp (symbol-name (car grps))
	    info (car (cdr grps))
	    grps (cdr (cdr grps)))
      (if (eq grp 'parsed)
	  nil
	(insert (format "    <a href=\"%s\">%7d%s %s" grp
			(url-retrieve-newsgroup grp nil t)
			(if (car info) ": " "! ") grp))))
    (insert "   </pre>\n"
	    "  </div1>\n"
	    " </body>\n"
	    "</html>\n"
	    "<!-- Automatically generated by URL/" url-version
	    "-->")))    

(defun url-news-generate-reply-form (to newsgroups body &rest refs)
  (set-buffer (get-buffer-create url-working-buffer))
  (erase-buffer)
  (insert "<html>\n"
	  " <head>\n"
	  "  <title>News Post/Reply Form</title>\n"
	  "  <!-- Automatically generated by URL -->\n"
	  " </head>\n"
	  " <body>\n"
	  "  <div1>\n"
	  "   <h1>News Post/Reply Form</h1>\n"
	  "   <hr>\n"
	  "   <form method=\"GET\" action=\"news-internal://\">\n"
	  "    <ul>\n"
	  "     <li> Reply by:"
	  "<select name=\"replyby\"><option>Mail<option>News</select></li>\n"
	  "     <li> Email: <input name=\"addr\" default=\"" to "\"></li>\n"
	  "     <li> Newsgroups: <input name=\"newsg\" default=\""
	  newsgroups "\"></li>\n"
	  "     <li> <input type=\"checkbox\" name=\"include\">"
	  "Include/quote article in followup</li>\n"
	  "    </ul>\n"
	  "    <hr>\n"
	  "    <textarea \"name=body\">\n" body "\n</textarea>\n"
	  "    <hr>\n"
	  "    <input type=\"submit\" value=\"Send it\">\n"
	  "    <br>\n"
	  "    <input type=\"reset\"  value=\"Reset to default values\">\n"
	  "   </form>\n"
	  "  </div1>\n"
	  " </body>\n"
	  "</html>\n"))	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for the different types of urls
;;; ---------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-wais (url)
  ;; Retrieve a document via WAIS
  (if (and url-wais-gateway-server url-wais-gateway-port)
      (url-retrieve
       (format "http://%s:%s/%s"
	       url-wais-gateway-server
	       url-wais-gateway-port
	       (substring url (match-end 0) nil)))
    (let ((href (url-grok-wais-href url)))
      (url-clear-tmp-buffer)
      (setq url-current-type "wais"
	    url-current-server (nth 0 href)
	    url-current-port (nth 1 href)
	    url-current-file (nth 2 href))
      (cond
       ((string-match "2=\\(.*\\);3=\\([^ ;]+\\)" (nth 2 href)); full link
	(url-retrieve-wais-docid (nth 0 href) (nth 1 href)
				(url-match (nth 2 href) 1)
				(url-match (nth 2 href) 2)))
       ((string-match "\\([^\\?]+\\)\\?\\(.*\\)" (nth 2 href)) ; stored query
	(url-perform-wais-query (nth 0 href) (nth 1 href)
			       (url-match (nth 2 href) 1)
			       (url-match (nth 2 href) 2)))
       (t
	(insert "<title>WAIS search</title>\n"
		"<h1>WAIS search of " (nth 2 href) "</h1>"
		"<hr>\n"
		"<form>\n"
		"Enter search term: <input name=\"internal-wais\">\n"
		"</form>\n"
		"<hr>\n"))))))

(autoload 'Info-goto-node "info" "" t)

(defun url-info (url)
  ;; Fetch an info node
  (if (get-buffer url-working-buffer)
      (kill-buffer url-working-buffer))
  (let* ((data (url-generic-parse-url url))
	 (fname (url-filename data))
	 (node (or (url-target data) "Top")))
    (if (and fname node)
	(Info-goto-node (concat "(" fname ")" node))
      (error "Malformed url: %s" url))))

(defun url-https (url)
  ;; Retrieve a URL via SSL
  (condition-case ()
      (require 'ssl)
    (error (error "Not configured for SSL, please read the info pages.")))
  (let ((url-this-is-ssl t)
	(url-gateway-method 'ssl))
    (url-http url)))

(defun url-shttp (url)
  ;; Retrieve a URL via Secure-HTTP
  (error "Secure-HTTP not implemented yet."))

(defun url-http (url &optional proxy-info)
  ;; Retrieve URL via http.
  (let* ((urlobj (url-generic-parse-url url))
	 (ref-url (or url-current-referer (url-view-url t))))
    (url-clear-tmp-buffer)
    (setq url-current-type (if (boundp 'url-this-is-ssl)
			       "https" "http"))
    (let* ((server (url-host urlobj))
	   (port   (url-port urlobj))
	   (file   (or proxy-info (url-filename urlobj)))
	   (dest   (url-target urlobj))
	   request)
      (if (equal port "") (setq port "80"))
      (if (equal file "") (setq file "/"))
      (if proxy-info
	  (let ((x (url-generic-parse-url url)))
	    (setq url-current-server (url-host urlobj)
		  url-current-port (url-port urlobj)
		  url-current-file (url-filename urlobj)
		  url-find-this-link (url-target urlobj)
		  request (url-create-mime-request file ref-url)))
	(setq url-current-server server
	      url-current-port port
	      url-current-file file
	      url-find-this-link dest
	      request (url-create-mime-request file ref-url)))
      (if (or (not (member port url-bad-port-list))
	      (funcall url-confirmation-func
		       (concat
			"Warning!  Trying to connect to port "
			port
			" - continue? ")))
	  (progn
;	    (url-lazy-message "Fetching: %s %s %s" server port file)
	    (url-lazy-message "Contacting %s:%s" server port)
	    (let ((process
		   (url-open-stream "WWW" url-working-buffer server
				   (string-to-int port))))
	      (if (stringp process)
		  (progn
		    (set-buffer url-working-buffer)
		    (erase-buffer)
		    (setq url-current-mime-type "text/html"
			  url-current-mime-viewer 
			  (mm-mime-info "text/html" nil 5))
		    (insert "<title>ERROR</title>\n"
			    "<h1>ERROR - Could not establish connection</h1>"
			    "<p>"
			    "The browser could not establish a connection "
			    (format "to %s:%s.<P>" server port)
			    "The server is either down, or the URL"
			    (format "(%s) is malformed.<p>" (url-view-url t)))
		    (message "%s" process))
		(progn
		  (process-kill-without-query process)
		  (process-send-string process request)
		  (url-lazy-message "Request sent, waiting for response...")
		  (if (and url-show-http2-transfer
			   (boundp 'after-change-functions))
		      (progn
			(make-local-variable 'after-change-functions)
			(add-hook 'after-change-functions
				  'url-after-change-function)))
		  (if url-be-asynchronous
		      (set-process-sentinel process 'url-sentinel)
		    (unwind-protect
			(save-excursion
			  (set-buffer url-working-buffer)
			  (while (memq (url-process-status process)
				       '(run open))
			    (if (boundp 'after-change-functions)
				nil
			      (url-after-change-function nil))
			    (url-accept-process-output process)))
		      (condition-case ()
			  (url-kill-process process)
			(error nil))))
		  (if (not url-be-asynchronous)
		    (message "Retrieval complete."))
		  (if (boundp 'after-change-functions)
		      (remove-hook 'after-change-functions
				   'url-after-change-function))))))
	(progn
	  (ding)
	  (url-warn 'security "Aborting connection to bad port..."))))))

(defun url-proxy (url)
  ;; Retrieve URL from a proxy.
  ;; Expects `url-using-proxy' to be bound to the specific proxy to use."
  (let ((url-be-asynchronous nil)
	(urlobj (url-generic-parse-url url))
	(proxyobj (url-generic-parse-url url-using-proxy)))
    (url-http url-using-proxy url)
    (setq url-current-type (url-type urlobj)
	  url-current-user (url-user urlobj)
	  url-current-port (or (url-port urlobj)
			       (cdr-safe (assoc url-current-type
						url-default-ports)))
	  url-current-server (url-host urlobj)
	  url-current-file (url-filename urlobj))))

(defun url-insert-possibly-compressed-file (fname &rest args)
  ;; Insert a file into a buffer, checking for compressed versions.
  (let ((compressed nil)
	(file-coding-system-for-read
	  (if (boundp 'MULE)
	      *noconv*)))
    (setq compressed 
	  (cond
	   ((file-exists-p fname) nil)
	   ((file-exists-p (concat fname ".Z"))
	    (setq fname (concat fname ".Z")))
	   ((file-exists-p (concat fname ".gz"))
	    (setq fname (concat fname ".gz")))
	   ((file-exists-p (concat fname ".z"))
	    (setq fname (concat fname ".z")))
	   (t
	    (error "File not found %s" fname))))
    (if (or (not compressed) url-inhibit-uncompression)
	(apply 'mm-insert-file-contents (cons fname args))
      (let* ((extn (url-file-extension fname))
	     (code (cdr-safe (assoc extn url-uncompressor-alist)))
	     (decoder (cdr-safe (assoc code mm-content-transfer-encodings))))
	(cond
	 ((null decoder) 
	  (apply 'mm-insert-file-contents fname args))
	 ((stringp decoder)
	  (apply 'mm-insert-file-contents fname args)
	  (message "Decoding...")
	  (call-process-region (point-min) (point-max) decoder t t nil)
	  (message "Decoding... done."))
	 ((listp decoder)
	  (apply 'call-process-region (point-min) (point-max)
		 (car decoder) t t t (cdr decoder)))
	 ((and (symbolp decoder) (fboundp decoder))
	  (apply 'mm-insert-file-contents fname args)
	  (message "Decoding...")
	  (funcall decoder (point-min) (point-max))
	  (message "Decoding... done."))
	 (t
	  (error "Malformed entry for %s in `mm-content-transfer-encodings'"
		 code))))))
  (set-buffer-modified-p nil))

(defun url-file (url)
  ;; Find a file
  (let* ((urlobj (url-generic-parse-url url))
	 (user (url-user urlobj))
	 (site (url-host urlobj))
	 (file (url-unhex-string (url-filename urlobj)))
	 (dest (url-target urlobj))
	 (filename (if (or user (and site (not (string= site "localhost"))))
		       (concat "/" (or user "anonymous") "@" site ":" file)
		     file))
	 ;; Patch by Yamaoka to not screw up jam-zcat/jka-compr by
	 ;; uncompressing before they get a chance
	 jka-compr-compression-info-list
	 jam-zcat-filename-list)

    (if (and file (not site)
	     (memq system-type '(ms-windows ms-dos windows-nt os2)))
	(let ((x (1- (length file)))
	      (y 0))
	  (while (<= y x)
	    (if (= (aref file y) ?\\ )
		(aset file y ?/))
	    (setq y (1+ y)))))

    (url-clear-tmp-buffer)
    (cond
     ((file-directory-p filename)
      (if url-use-hypertext-dired
	  (progn
	    (if (string-match "/$" filename)
		nil
	      (setq filename (concat filename "/")))
	    (if (string-match "/$" filename)
		nil
	      (setq file (concat file "/")))
	    (url-set-filename urlobj file)
	    (url-format-directory filename))
	(progn
	  (if (get-buffer url-working-buffer)
	      (kill-buffer url-working-buffer))
	  (find-file filename))))
     ((and (boundp 'w3-dump-to-disk) (symbol-value 'w3-dump-to-disk))
      (cond
       ((file-exists-p filename) nil)
       ((file-exists-p (concat filename ".Z"))
	(setq filename (concat filename ".Z")))
       ((file-exists-p (concat filename ".gz"))
	(setq filename (concat filename ".gz")))
       ((file-exists-p (concat filename ".z"))
	(setq filename (concat filename ".z")))
       (t
	(error "File not found %s" filename)))
      (cond
       ((null site)
	(copy-file
	 filename 
	 (read-file-name "Save to: " nil (url-basepath filename t)) t))
       ((featurep 'ange-ftp)
	(ange-ftp-copy-file-internal
	 filename
	 (expand-file-name
	  (read-file-name "Save to: " nil (url-basepath filename t))) t
	 nil t nil t))
       ((or (featurep 'efs) (featurep 'efs-auto))
	(let ((new (expand-file-name
		    (read-file-name "Save to: " nil
				    (url-basepath filename t)))))
	  (efs-copy-file-internal filename (efs-ftp-path filename)
				  new (efs-ftp-path new)
				  t nil 0 nil 0 nil)))
       (t (copy-file
	   filename 
	   (read-file-name "Save to: " nil (url-basepath filename t)) t)))
      (if (get-buffer url-working-buffer)
	  (kill-buffer url-working-buffer)))
     (t
      (let ((viewer (mm-mime-info
		     (mm-extension-to-mime (url-file-extension file))))
	    (errobj nil))
	(if (or url-source		; Need it in a buffer
		(and (symbolp viewer)
		     (not (eq viewer 'w3-default-local-file)))
		(stringp viewer))
	    (condition-case errobj
		(url-insert-possibly-compressed-file filename t)
	      (error
	       (url-save-error errobj)
	       (url-retrieve (concat "www://error/nofile/" file))))))))
    (setq url-current-type (if site "ftp" "file")
	  url-current-object urlobj
	  url-find-this-link dest
	  url-current-user user
	  url-current-server site
	  url-current-mime-type (mm-extension-to-mime
				 (url-file-extension file))
	  url-current-file file)))

(defun url-finger (url)
  ;; Find a finger reference
  (setq url-current-mime-headers '(("content-type" . "text/html"))
	url-current-mime-type "text/html")
  (set-buffer (get-buffer-create url-working-buffer))
  (let* ((urlobj (if (vectorp url) url
		   (url-generic-parse-url url)))
	 (host (or (url-host urlobj) "localhost"))
	 (port (or (url-port urlobj)
		   (cdr-safe (assoc "finger" url-default-ports))))
	 (user (url-unhex-string (url-filename urlobj)))
	 (proc (url-open-stream "finger" url-working-buffer host
				(string-to-int port))))
    (if (stringp proc)
	(message "%s" proc)
      (process-kill-without-query proc)
      (if (= (string-to-char user) ?/)
	  (setq user (substring user 1 nil)))
      (goto-char (point-min))
      (insert "<html>\n"
	      " <head>\n"
	      "  <title>Finger information for " user "@" host "</title>\n"
	      " </head>\n"
	      " <body>\n"
	      "  <h1>Finger information for " user "@" host "</h1>\n"
	      "  <hr>\n"
	      "  <pre>\n")
      (process-send-string proc (concat user "\r\n"))
      (while (memq (url-process-status proc) '(run open))
	(url-after-change-function)
	(url-accept-process-output proc))
      (goto-char (point-min))
      (url-replace-regexp "^Process .* exited .*code .*$" "")
      (goto-char (point-max))
      (insert "  </pre>\n"
	      " </body>\n"
	      "</html>\n"))))

(defun url-news (article)
  ;; Find a news reference
  (or noninteractive (require 'nntp))
  (setq url-current-mime-headers '(("content-type" . "text/html"))
	url-current-mime-type "text/html")
  (let* ((urlobj (url-generic-parse-url article))
	 (host (or (url-host urlobj) url-news-server))
	 (port (or (url-port urlobj)
		   (cdr-safe (assoc "news" url-default-ports))))
	 (article-brackets nil)
	 (article (url-filename urlobj)))
    (or (nntp-server-opened)
	(nntp-open-server host (if (string-match (regexp-quote "4.0")
						 nntp-version)
				   (list (string-to-int port))
				 (string-to-int port))))
    (cond
     ((string-match "@" article)	; Its a specific article
      ;; put the message-id in article, and <message-id> in article-brackets
      (cond 
       ((eq ?> (aref article (1- (length article))))
	(setq article-brackets article)
	(setq article (substring article 1 -1)))
       (t
	(setq article-brackets (concat "<" article ">"))))
      (if (boundp 'after-change-functions)
	  (progn
	    (set-buffer nntp-server-buffer)
	    (make-local-variable 'after-change-functions)
	    (add-hook 'after-change-functions 'nntp-after-change-function)))
      (if (nntp-request-article article-brackets)
	  (progn
	    (if (boundp 'after-change-functions)
		(progn
		  (set-buffer nntp-server-buffer)
		  (remove-hook 'after-change-functions
			       'nntp-after-change-function)))
	    (url-format-news))
	  (set-buffer (get-buffer-create url-working-buffer))
	  (setq url-current-can-be-cached nil)
	  (insert "<html>\n"
		  " <head>\n"
		  "  <title>Error</title>\n"
		  " </head>\n"
		  " <body>\n"
		  "  <div1>\n"
		  "   <h1>Error requesting article...</h1>\n"
		  "   <p>\n"
		  "    The status message returned by the NNTP server was:"
		  "<br><hr>\n"
		  "    <pre>\n"
		  (nntp-status-message)
		  "    </pre>\n"
		  "   </p>\n"
		  "   <p>\n"
		  "    If you If you feel this is an error, <a href=\""
		  "mailto:" url-bug-address "\">send me mail</a>\n"
		  "   </p>\n"
		  "  </div1>\n"
		  " </body>\n"
		  "</html>\n"
		  "<!-- Automatically generated by URL v" url-version " -->\n"
		  )))
     ((string= article "")		; List all newsgroups
      (url-show-all-newsgroups))
     (t					; Whole newsgroup
      (url-format-whole-newsgroup article (url-retrieve-newsgroup article))))
    (cond
     ((boundp 'nntp-server-process)	; original nntp.el by umeda
      (process-kill-without-query nntp-server-process))
     ((boundp 'nntp/connection)		; Flee's version of nntp.el
      (process-kill-without-query nntp/connection))
     (t nil))				; Unknown version of nntp.el
    (setq url-current-type "news"
	  url-current-server host
	  url-current-port port
	  url-current-file article)))

(defun url-rlogin (url)
  ;; Open up an rlogin connection
  (if (get-buffer url-working-buffer)
      (kill-buffer url-working-buffer))
  (or (string-match "rlogin:/*\\(.*@\\)*\\([^/]*\\)/*" url)
      (error "Malformed RLOGIN URL."))
  (let* ((server (substring url (match-beginning 2) (match-end 2)))
	 (name (if (match-beginning 1)
		   (substring url (match-beginning 1) (1- (match-end 1)))
		 nil))
	 (title (format "%s%s" (if name (concat name "@") "") server))
	 (thebuf (string-match ":" server))
	 (port (if thebuf
		   (prog1
		       (substring server (1+ thebuf) nil)
		     (setq server (substring server 0 thebuf))) "23")))
    (cond
     ((not (eq (device-type) 'tty))
      (apply 'start-process
	     "htmlsub"
	     nil
	     (url-string-to-tokens
	      (format url-xterm-command title 
		      (if (and url-gateway-local-host-regexp
			       (string-match url-gateway-local-host-regexp
					     server))
			  url-local-rlogin-prog
			url-remote-rlogin-prog) server
			(concat "-l " name)) ? )))
     (url-use-transparent
      (require 'transparent)
      (sit-for 1)
      (transparent-window (get-buffer-create
			   (format "%s%s:%s" (if name (concat name "@") "")
				   server port))
			  (if (and url-gateway-local-host-regexp
				   (string-match url-gateway-local-host-regexp
						 server))
			      url-local-rlogin-prog
			    url-remote-rlogin-prog)
			  (list server "-l" name) nil
			  "Press any key to return to emacs"))
     (t
      (terminal-emulator
       (get-buffer-create (format "%s%s:%s" (if name (concat name "@") "")
				  server port))
       (if (and url-gateway-local-host-regexp
		(string-match url-gateway-local-host-regexp
			      server))
	   url-local-rlogin-prog
	 url-remote-rlogin-prog)
       (list server "-l" name))))))

(defun url-telnet (url)
  ;; Open up a telnet connection
  (if (get-buffer url-working-buffer)
      (kill-buffer url-working-buffer))
  (or (string-match "telnet:/*\\(.*@\\)*\\([^/]*\\)/*" url)
      (error "Malformed telnet URL: %s" url))
  (let* ((server (substring url (match-beginning 2) (match-end 2)))
	 (name (if (match-beginning 1)
		   (substring url (match-beginning 1) (1- (match-end 1)))
		 nil))
	 (title (format "%s%s" (if name (concat name "@") "") server))
	 (thebuf (string-match ":" server))
	 (port (if thebuf
		   (prog1
		       (substring server (1+ thebuf) nil)
		     (setq server (substring server 0 thebuf))) "23")))
    (cond
     ((not (eq (device-type) 'tty))
      (apply 'start-process
	     "htmlsub"
	     nil
	     (url-string-to-tokens
	      (format url-xterm-command title 
		      (if (and url-gateway-local-host-regexp
			       (string-match url-gateway-local-host-regexp
					     server))
			  url-local-telnet-prog
			url-remote-telnet-prog) server port) ? ))
      (if name (message "Please log in as %s" name)))
     (url-use-transparent
      (require 'transparent)
      (if name (message "Please log in as %s" name))
      (sit-for 1)
      (transparent-window (get-buffer-create
			   (format "%s%s:%s" (if name (concat name "@") "")
				   server port))
			  (if (and url-gateway-local-host-regexp
				   (string-match url-gateway-local-host-regexp
						 server))
			      url-local-telnet-prog
			    url-remote-telnet-prog)
			  (list server port) nil
			  "Press any key to return to emacs"))
     (t
      (terminal-emulator
       (get-buffer-create (format "%s%s:%s" (if name (concat name "@") "")
				  server port))
       (if (and url-gateway-local-host-regexp
		(string-match url-gateway-local-host-regexp
			      server))
	   url-local-telnet-prog
	 url-remote-telnet-prog)
       (list server port))
      (if name (message "Please log in as %s" name))))))

(defun url-tn3270 (url)
  ;; Open up a tn3270 connection
  (if (get-buffer url-working-buffer)
      (kill-buffer url-working-buffer))
  (string-match "tn3270:/*\\(.*@\\)*\\([^/]*\\)/*" url)
  (let* ((server (substring url (match-beginning 2) (match-end 2)))
	 (name (if (match-beginning 1)
		   (substring url (match-beginning 1) (1- (match-end 1)))
		 nil))
	 (thebuf (string-match ":" server))
	 (title (format "%s%s" (if name (concat name "@") "") server))
	 (port (if thebuf
		   (prog1
		       (substring server (1+ thebuf) nil)
		     (setq server (substring server 0 thebuf))) "23")))
    (cond
     ((not (eq (device-type) 'tty))
      (start-process "htmlsub" nil url-xterm-command
		     "-title" title
		     "-ut" "-e" url-tn3270-emulator server port)
      (if name (message "Please log in as %s" name)))
     (url-use-transparent
      (require 'transparent)
      (if name (message "Please log in as %s" name))
      (sit-for 1)
      (transparent-window (get-buffer-create
			   (format "%s%s:%s" (if name (concat name "@") "")
				   server port))
			  url-tn3270-emulator
			  (list server port) nil
			  "Press any key to return to emacs"))
     (t
      (terminal-emulator
       (get-buffer-create (format "%s%s:%s" (if name (concat name "@") "")
				  server port))
       url-tn3270-emulator
       (list server port))
      (if name (message "Please log in as %s" name))))))

(defun url-mailto (url)
  ;; Send mail to someone
  (if (not (string-match "mailto:/*\\(.*\\)" url))
      (error "Malformed mailto link: %s" url))
  (if (get-buffer url-working-buffer)
      (kill-buffer url-working-buffer))
  (let ((to (substring url (match-beginning 1) (match-end 1)))
	(url (url-view-url t)))
    (if (fboundp url-mail-command) (funcall url-mail-command) (mail))
    (mail-to)
    (insert (concat to "\nX-URL-From: " url))
    (mail-subject)
    (if (not url-request-data)
	nil				; Not automatic posting
      (insert "Automatic submission from "
	      url-package-name "/" url-package-version)
      (if url-request-extra-headers
	  (progn
	    (goto-char (point-min))
	    (insert
	     (mapconcat
	      (function
	       (lambda (x)
		 (concat (capitalize (car x)) ": " (cdr x) "\n")))
	      url-request-extra-headers ""))))
      (goto-char (point-max))
      (insert url-request-data)
      (mail-send-and-exit nil))))

(defvar url-mailserver-syntax-table
  (copy-syntax-table emacs-lisp-mode-syntax-table)
  "*A syntax table for parsing the mailserver URL")

(modify-syntax-entry ?' "\"" url-mailserver-syntax-table)
(modify-syntax-entry ?` "\"" url-mailserver-syntax-table)
(modify-syntax-entry ?< "(>" url-mailserver-syntax-table)
(modify-syntax-entry ?> ")<" url-mailserver-syntax-table)
(modify-syntax-entry ?/ " " url-mailserver-syntax-table)

(defmacro url-mailserver-skip-chunk ()
  (` (while (and (not (looking-at "/"))
		 (not (eobp)))
       (forward-sexp 1))))

(defun url-mailserver (url)
  ;; Send mail to someone, much cooler/functional than mailto
  (if (get-buffer url-working-buffer)
      (kill-buffer url-working-buffer))
  (set-buffer (get-buffer-create " *mailserver*"))
  (erase-buffer)
  (insert url)
  (goto-char (point-min))
  (set-syntax-table url-mailserver-syntax-table)
  (skip-chars-forward "^:")		; Get past mailserver
  (skip-chars-forward ":")		; Get past :
  ;; Handle some ugly malformed URLs, but bitch about it.
  (if (looking-at "/")
      (progn
	(url-warn 'url "Invalid mailserver URL... attempting to cope.")
	(skip-chars-forward "/")))
  
  (let ((save-pos (point))
	(url (url-view-url t))
	(rfc822-addr nil)
	(subject nil)
	(body nil))
    (url-mailserver-skip-chunk)
    (setq rfc822-addr (buffer-substring save-pos (point)))
    (forward-char 1)
    (setq save-pos (point))
    (url-mailserver-skip-chunk)
    (setq subject (buffer-substring save-pos (point)))
    (if (not (eobp))
	(progn				; There is some text to use
	  (forward-char 1)		; as the body of the message
	  (setq body (buffer-substring (point) (point-max)))))
    (if (fboundp url-mail-command) (funcall url-mail-command) (mail))
    (mail-to)
    (insert (concat rfc822-addr
		    (if (and url (not (string= url "")))
			(concat "\nX-URL-From: " url) "")
		    "\nX-User-Agent: " url-package-name "/"
		    url-package-version))
    (mail-subject)
    ;; Massage the subject from URLEncoded garbage
    ;; Note that we do not allow any newlines in the subject,
    ;; as recommended by the Internet Draft on the mailserver
    ;; URL - this means the document author cannot spoof additional
    ;; header lines, which is a 'Good Thing'
    (if subject
	(progn
	  (setq subject (url-unhex-string subject))
	  (let ((x (1- (length subject)))
		(y 0))
	    (while (<= y x)
	      (if (memq (aref subject y) '(?\r ?\n))
		  (aset subject y ? ))
	      (setq y (1+ y))))))
    (insert subject)
    (if url-request-extra-headers
	(progn
	  (goto-char (point-min))
	  (insert
	   (mapconcat
	    (function
	     (lambda (x)
	       (concat (capitalize (car x)) ": " (cdr x) "\n")))
	    url-request-extra-headers ""))))
    (goto-char (point-max))
    ;; Massage the body from URLEncoded garbage
    (if body
	(let ((x (1- (length body)))
	      (y 0))
	  (while (<= y x)
	    (if (= (aref body y) ?/)
		(aset body y ?\n))
	    (setq y (1+ y)))
	  (setq body (url-unhex-string body))))
    (and body (insert body))
    (and url-request-data (insert url-request-data))
    (if (and (or body url-request-data)
	     (funcall url-confirmation-func
		      (concat "Send message to " rfc822-addr "? ")))
	(mail-send-and-exit nil))))    

(defun url-gopher (url)
  ;; Handle gopher URLs
  (let ((descr (url-grok-gopher-href url)))
    (cond
     ((or (not (member (nth 1 descr) url-bad-port-list))
	  (funcall
	   url-confirmation-func
	   (format "Warning!  Trying to connect to port %s - continue? "
		   (nth 1 descr))))
      (if url-use-hypertext-gopher
	  (url-do-gopher descr)
	(gopher-dispatch-object (vector (if (= 0
					       (string-to-char (nth 2 descr)))
					    ?1
					  (string-to-char (nth 2 descr)))
					(nth 2 descr) (nth 2 descr)
					(nth 0 descr)
					(string-to-int (nth 1 descr)))
				(current-buffer))))
     (t
      (ding)
      (url-warn 'security "Aborting connection to bad port...")))))

(fset 'url-ftp 'url-file)

(defun url-x-exec (url)
  ;; Handle local execution of scripts.
  (set-buffer (get-buffer-create url-working-buffer))
  (erase-buffer)
  (string-match "x-exec:/+\\([^/]+\\)\\(/.*\\)" url)
  (let ((process-environment process-environment)
	(executable (url-match url 1))
	(path-info (url-match url 2))
	(query-string nil)
	(safe-paths url-local-exec-path)
	(found nil)
	(y nil)
	)
    (setq url-current-server executable
	  url-current-file path-info)
    (if (string-match "\\(.*\\)\\?\\(.*\\)" path-info)
	(setq query-string (url-match path-info 2)
	      path-info (url-match path-info 1)))
    (while (and safe-paths (not found))
      (setq y (expand-file-name executable (car safe-paths))
	    found (and (file-exists-p y) (file-executable-p y) y)
	    safe-paths (cdr safe-paths)))
    (if (not found)
	(url-retrieve (concat "www://error/nofile/" executable))
      (setq process-environment
	    (append
	     (list
	      "SERVER_SOFTWARE=x-exec/1.0"
	      (concat "SERVER_NAME=" (system-name))
	      "GATEWAY_INTERFACE=CGI/1.1"
	      "SERVER_PROTOCOL=HTTP/1.0"
	      "SERVER_PORT="
	      (concat "REQUEST_METHOD=" url-request-method)
	      (concat "HTTP_ACCEPT="
		      (mapconcat
		       (function
			(lambda (x)
			  (cond
			   ((= x ?\n) (setq y t) "")
			   ((= x ?:) (setq y nil) ",")
			   (t (char-to-string x))))) url-mime-accept-string
		       ""))
	      (concat "PATH_INFO=" (url-unhex-string path-info))
	      (concat "PATH_TRANSLATED=" (url-unhex-string path-info))
	      (concat "SCRIPT_NAME=" executable)
	      (concat "QUERY_STRING=" (url-unhex-string query-string))
	      (concat "REMOTE_HOST=" (system-name)))
	     (if (assoc "content-type" url-request-extra-headers)
		 (concat "CONTENT_TYPE=" (cdr
					  (assoc "content-type"
						 url-request-extra-headers))))
	     (if url-request-data
		 (concat "CONTENT_LENGTH=" (length url-request-data)))
	     process-environment))
      (and url-request-data (insert url-request-data))
      (setq y (call-process-region (point-min) (point-max) found t t))
      (goto-char (point-min))
      (delete-region (point) (progn (skip-chars-forward " \t\n") (point)))
      (cond
       ((url-mime-response-p) nil)	; Its already got an HTTP/1.0 header
       ((null y)			; Weird exit status, whassup?
	(insert "HTTP/1.0 404 Not Found\n"
		"Server: " url-package-name "/x-exec\n"))	
       ((= 0 y)				; The shell command was successful
	(insert "HTTP/1.0 200 Document follows\n"
		"Server: " url-package-name "/x-exec\n"))	
       (t				; Non-zero exit status is bad bad bad
	(insert "HTTP/1.0 404 Not Found\n"
		"Server: " url-package-name "/x-exec\n"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gateway Support
;;; ---------------
;;; Fairly good/complete gateway support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-kill-process (proc)
  "Kill the process PROC - knows about all the various gateway types,
and acts accordingly."
  (cond
   ((eq url-gateway-method 'native) (delete-process proc))
   ((eq url-gateway-method 'program) (kill-process proc))
   (t (error "Unknown url-gateway-method %s" url-gateway-method))))

(defun url-accept-process-output (proc)
  "Allow any pending output from subprocesses to be read by Emacs.
It is read into the process' buffers or given to their filter functions.
Where possible, this will not exit until some output is received from PROC,
or 1 second has elapsed."
  (if url-current-time-string-has-args
      (accept-process-output proc 1)
    (accept-process-output)))

(defun url-process-status (proc)
  "Return the process status of a url buffer"
  (cond
   ((memq url-gateway-method '(native ssl program)) (process-status proc))
   (t (error "Unkown url-gateway-method %s" url-gateway-method))))  

(defun url-open-stream (name buffer host service)
  "Open a stream to a host"
  (let ((tmp-gateway-method (if (and url-gateway-local-host-regexp
				     (not (eq 'ssl url-gateway-method))
				     (string-match
				      url-gateway-local-host-regexp
				      host))
				'native
			      url-gateway-method))
	(tcp-binary-process-output-services (if (stringp service)
						(list service)
					      (list service
						    (int-to-string service)))))
    (and (eq url-gateway-method 'tcp)
	 (require 'tcp)
	 (setq url-gateway-method 'native
	       tmp-gateway-method 'native))
    (cond
     ((eq tmp-gateway-method 'ssl)
      (open-ssl-stream name buffer host service))
     ((eq tmp-gateway-method 'native)
      (if url-broken-resolution
	  (setq host
		(cond
		 ((featurep 'ange-ftp) (ange-ftp-nslookup-host host))
		 ((featurep 'efs) (efs-nslookup-host host))
		 ((featurep 'efs-auto) (efs-nslookup-host host))
		 (t host))))
      (let ((retry url-connection-retries)
	    (errobj nil)
	    (conn nil))
	(while (and (not conn) retry)
	  (condition-case errobj
	      (setq conn (open-network-stream name buffer host service))
	    (error
	     (url-save-error errobj)
	     (save-window-excursion
	       (save-excursion
		 (switch-to-buffer-other-window " *url-error*")
		 (setq retry (funcall url-confirmation-func
				      (concat "Connection to " host
					      " failed, retry? "))))))))
	(if conn
 	    (progn
 	      (if (boundp 'MULE)
		  (save-excursion
		    (set-buffer (get-buffer-create buffer))
		    (setq mc-flag nil)
		    (set-process-coding-system conn *noconv* *noconv*)))
 	      conn)
	  (error "Unable to connect to %s:%s" host service))))
     ((eq tmp-gateway-method 'program)
      (let ((proc (start-process name buffer url-gateway-telnet-program host
				 (int-to-string service)))
	    (tmp nil))
	(save-excursion
	  (set-buffer buffer)
	  (setq tmp (point))
	  (while (not (progn
			(goto-char (point-min))
			(re-search-forward 
			 url-gateway-telnet-ready-regexp nil t)))
	    (url-accept-process-output proc))
	  (delete-region tmp (point))
	  (goto-char (point-min))
	  (if (re-search-forward "connect:" nil t)
	      (progn
		(condition-case ()
		    (delete-process proc)
		  (error nil))
		(url-replace-regexp ".*connect:.*" "")
		nil)
	    proc))))
     (t (error "Unknown url-gateway-method %s" url-gateway-method)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-setup-privacy-info ()
  (interactive)
  (setq url-system-type
	(cond
	 ((or (eq url-privacy-level 'paranoid)
	      (and (listp url-privacy-level)
		   (memq 'os url-privacy-level)))
	  "NoneOfYourBusiness")
	 ((eq system-type 'Apple-Macintosh) "Macintosh")
	 ((eq system-type 'next-mach) "NeXT")
	 ((eq system-type 'windows-nt) "Windows-NT ; 32bit")
	 ((eq system-type 'ms-windows) "Windows ; 16bit")
	 ((eq system-type 'ms-dos) "MS-DOS ; 32bit")
	 ((and (eq system-type 'vax-vms) (device-type))
	  "VMS ; X11")
	 ((eq system-type 'vax-vms) "VMS ; TTY")
	 ((eq (device-type) 'x) "X11")
	 ((eq (device-type) 'ns) "NeXTStep")
	 ((eq (device-type) 'pm) "OS/2")
	 ((eq (device-type) 'win32) "Windows ; 32bit")
	 ((eq (device-type) 'tty) "(Unix?) ; TTY")
	 (t "UnkownPlatform")))

  (setq url-personal-mail-address (or url-personal-mail-address
				      url-pgp/pem-entity))

  (if (or (memq url-privacy-level '(paranoid high))
	  (and (listp url-privacy-level)
	       (memq 'email url-privacy-level)))
      (setq url-personal-mail-address nil))

  (if (or (eq url-privacy-level 'paranoid)
	  (and (listp url-privacy-level)
	       (memq 'os url-privacy-level)))
      (setq url-os-type "ImNotTelling")
    (let ((vers (emacs-version)))
      (if (string-match "(\\([^, )]+\\))$" vers)
	  (setq url-os-type (url-match vers 1))
	(setq url-os-type (symbol-name system-type))))))

(defun url-setup-save-timer ()
  "Reset the history list timer."
  (interactive)
  (cond
   ((featurep 'itimer)
    (if (get-itimer "url-history-saver")
	(delete-itimer (get-itimer "url-history-saver")))
    (start-itimer "url-history-saver" 'url-write-global-history
		  url-global-history-save-interval
		  url-global-history-save-interval))
   ((fboundp 'run-at-time)
    (run-at-time url-global-history-save-interval
		 url-global-history-save-interval
		 'url-write-global-history))
   (t nil)))

(defun url-handle-no-scheme (url)
  (let ((temp url-registered-protocols)
	(found nil))
    (while (and temp (not found))
      (if (and (not (member (car (car temp)) '("auto" "www")))
	       (string-match (concat "^" (car (car temp)) "\\.")
					url))
	  (setq found t)
	(setq temp (cdr temp))))
    (cond
     (found				; Found something like ftp.spry.com
      (url-retrieve (concat (car (car temp)) "://" url)))
     ((string-match "^www\\." url)
      (url-retrieve (concat "http://" url)))
     ((string-match "\\(\\.[^\\.]+\\)\\(\\.[^\\.]+\\)" url)
      ;; Ok, we have at least two dots in the filename, just stick http on it
      (url-retrieve (concat "http://" url)))
     (t
      (url-retrieve (concat "http://www." url ".com"))))))

(defun url-do-setup ()
  "Do setup - this is to avoid conflict with user settings when URL is
dumped with emacs."
  (if url-setup-done
      nil

    ;; Register all the protocols we can handle
    (url-register-protocol 'file)
    (url-register-protocol 'ftp        nil nil "21")
    (url-register-protocol 'gopher     nil nil "70")
    (url-register-protocol 'http       nil nil "80")
    (url-register-protocol 'https      nil nil "443")
    (url-register-protocol 'info       nil 'url-identity-expander)
    (url-register-protocol 'mailserver nil 'url-identity-expander)
    (url-register-protocol 'finger     nil 'url-identity-expander "79")
    (url-register-protocol 'mailto     nil 'url-identity-expander)
    (url-register-protocol 'news       nil 'url-identity-expander "119")
    (url-register-protocol 'rlogin)
    (url-register-protocol 'shttp      nil nil "80")
    (url-register-protocol 'telnet)
    (url-register-protocol 'tn3270)
    (url-register-protocol 'wais)
    (url-register-protocol 'x-exec)
    (url-register-protocol 'proxy)
    (url-register-protocol 'auto 'url-handle-no-scheme)
			   
    ;; Register all the authentication schemes we can handle
    (url-register-auth-scheme "basic" nil 4)
    (url-register-auth-scheme "digest" nil 7)

    ;; Filename handler stuff for emacsen that support it
    (url-setup-file-name-handlers)
    (setq url-default-session-id (url-create-message-id))

    (if url-current-time-string-has-args
	(fset 'url-lazy-message 'url-lazy-message-1)
      (fset 'url-lazy-message 'url-lazy-message-2))
    (setq url-global-history-file
	  (or url-global-history-file
	      (and (memq system-type '(ms-dos ms-windows))
		   (expand-file-name "~/mosaic.hst"))
	      (and (memq system-type '(axp-vms vax-vms))
		   (expand-file-name "~/mosaic.global-history"))
	      (condition-case ()
		  (expand-file-name "~/.mosaic-global-history")
		(error nil))))
  
    ;; Parse the global history file if it exists, so that it can be used
    ;; for URL completion, etc.
    (if (and url-global-history-file
	     (file-exists-p url-global-history-file))
	(url-parse-global-history))

    ;; Setup save timer
    (and url-global-history-save-interval (url-setup-save-timer))
    
    ;; Read in proxy gateways
    (let ((noproxy (and (not (assoc "no_proxy" url-proxy-services))
			(or (getenv "NO_PROXY")
			    (getenv "no_PROXY")
			    (getenv "no_proxy")))))
      (if noproxy
	  (setq url-proxy-services
		(cons (cons "no_proxy"
			    (concat "\\("
				    (mapconcat
				     (function
				      (lambda (x)
					(cond
					 ((= x ?,) "\\|")
					 ((= x ? ) "")
					 ((= x ?.) (regexp-quote "."))
					 ((= x ?*) ".*")
					 ((= x ??) ".")
					 (t (char-to-string x)))))
				     noproxy "") "\\)"))
		      url-proxy-services))))

    ;; Set the url-use-transparent with decent defaults
    (if (not (eq (device-type) 'tty))
	(setq url-use-transparent nil))
    (and url-use-transparent (require 'transparent))
  
    ;; Set the password entry funtion based on user defaults or guess
    ;; based on which remote-file-access package they are using.
    (cond
     (url-passwd-entry-func nil)	; Already been set
     ((boundp 'read-passwd)		; Use secure password if available
      (setq url-passwd-entry-func 'read-passwd))
     ((or (featurep 'efs)		; Using EFS
	  (featurep 'efs-auto))		; or autoloading efs
      (if (not (fboundp 'read-passwd))
	  (autoload 'read-passwd "passwd" "Read in a password" nil))
      (setq url-passwd-entry-func 'read-passwd))
     ((or (featurep 'ange-ftp)		; Using ange-ftp
	  (and (boundp 'file-name-handler-alist)
	       (not (string-match "Lucid" (emacs-version)))))
      (setq url-passwd-entry-func 'ange-ftp-read-passwd))
     (t
      (url-warn 'security
		"Can't determine how to read passwords, winging it.")))
  
    ;; Set up the news service if they haven't done so
    (setq url-news-server
	  (cond
	   (url-news-server url-news-server)
	   ((and (boundp 'gnus-default-nntp-server)
		 (not (equal "" gnus-default-nntp-server)))
	    gnus-default-nntp-server)
	   ((and (boundp 'gnus-nntp-server)
		 (not (null gnus-nntp-server))
		 (not (equal "" gnus-nntp-server)))
	    gnus-nntp-server)
	   ((and (boundp 'nntp-server-name)
		 (not (null nntp-server-name))
		 (not (equal "" nntp-server-name)))
	    nntp-server-name)
	   ((getenv "NNTPSERVER") (getenv "NNTPSERVER"))
	   (t "news")))
  
    ;; Set up the MIME accept string if they haven't got it hardcoded yet
    (or url-mime-accept-string
	(setq url-mime-accept-string (url-parse-viewer-types)))
    (or url-mime-encoding-string
	(setq url-mime-encoding-string
	      (mapconcat 'car
			 mm-content-transfer-encodings
			 ", ")))
  
    ;; Set up the entity definition for PGP and PEM authentication
    (setq url-pgp/pem-entity (or url-pgp/pem-entity
				 (format "%s@%s"  (user-real-login-name)
					 (system-name))))
    (url-setup-privacy-info)
    (run-hooks 'url-load-hook)
    (setq url-setup-done t)))

(defun url-cache-file-writable-p (file)
  "Follows the documentation of file-writable-p, unlike file-writable-p."
  (and (file-writable-p file)
       (if (file-exists-p file)
           (not (file-directory-p file))
         (file-directory-p (file-name-directory file)))))
                
(defun url-prepare-cache-for-file (file)
  "Makes it possible to cache data in FILE.
Creates any necessary parent directories, deleting any non-directory files
that would stop this.  Returns nil if parent directories can not be
created.  If FILE already exists as a non-directory, it changes
permissions of FILE or deletes FILE to make it possible to write a new
version of FILE.  Returns nil if this can not be done.  Returns nil if
FILE already exists as a directory.  Otherwise, returns t, indicating that
FILE can be created or overwritten."

  ;; COMMENT: We don't delete directories because that requires
  ;; recursively deleting the directories's contents, which might
  ;; eliminate a substantial portion of the cache.

  (cond
   ((url-cache-file-writable-p file)
    t)
   ((file-directory-p file)
    nil)
   (t
    (catch 'upcff-tag
      (let ((dir (file-name-directory file))
            dir-parent dir-last-component)
        (if (string-equal dir file)
            ;; *** Should I have a warning here?
            ;; FILE must match a pattern like /foo/bar/, indicating it is a
            ;; name only suitable for a directory.  So presume we won't be
            ;; able to overwrite FILE and return nil.
            (throw 'upcff-tag nil))
        
        ;; Make sure the containing directory exists, or throw a failure
        ;; if we can't create it.
        (if (file-directory-p dir)
            nil
          (or (fboundp 'make-directory)
              (throw 'upcff-tag nil))
          (make-directory dir t)
          ;; make-directory silently fails if there is an obstacle, so
          ;; we must verify its results.
          (if (file-directory-p dir)
              nil
            ;; Look at prefixes of the path to find the obstacle that is
            ;; stopping us from making the directory.  Unfortunately, there
            ;; is no portable function in Emacs to find the parent directory
            ;; of a *directory*.  So this code may not work on VMS.
            (while (progn
                     (if (eq ?/ (aref dir (1- (length dir))))
                         (setq dir (substring dir 0 -1))
                       ;; Maybe we're on VMS where the syntax is different.
                       (throw 'upcff-tag nil))
                     (setq dir-parent (file-name-directory dir))
                     (not (file-directory-p dir-parent)))
              (setq dir dir-parent))
            ;; We have found the longest path prefix that exists as a
            ;; directory.  Deal with any obstacles in this directory.
            (if (file-exists-p dir)
                (condition-case nil
                    (delete-file dir)
                  (error (throw 'upcff-tag nil))))
            (if (file-exists-p dir)
                (throw 'upcff-tag nil))
            ;; Try making the directory again.
            (setq dir (file-name-directory file))
            (make-directory dir t)
            (or (file-directory-p dir)
                (throw 'upcff-tag nil))))

        ;; The containing directory exists.  Let's see if there is
        ;; something in the way in this directory.
        (if (url-cache-file-writable-p file)
            (throw 'upcff-tag t)
          (condition-case nil
              (delete-file file)
            (error (throw 'upcff-tag nil))))

        ;; The return value, if we get this far.
        (url-cache-file-writable-p file))))))
       
(defun url-store-in-cache (&optional buff)
  "Store buffer BUFF in the cache"
  (if (or (not (get-buffer buff))
	  (member url-current-type '("www" "about" "https" "shttp"
					 "news" "mailto"))
	  (and (member url-current-type '("file" "ftp" nil))
	       (not url-current-server))
	  )
      nil
    (save-excursion
      (and buff (set-buffer buff))
      (let* ((fname (url-create-cached-filename (url-view-url t)))
             (fname-hdr (concat (if (memq system-type '(ms-windows ms-dos os2))
                                    (url-file-extension fname t)
                                  fname) ".hdr"))
	     (info (mapcar (function (lambda (var)
				       (cons (symbol-name var)
					     (symbol-value var))))
			   '( url-current-content-length
			      url-current-file
			      url-current-isindex
			      url-current-mime-encoding
			      url-current-mime-headers
			      url-current-mime-type
			      url-current-mime-viewer
			      url-current-nntp-server
			      url-current-port
			      url-current-server
			      url-current-type
			      url-current-user
			      ))))
	(cond ((and (url-prepare-cache-for-file fname)
		    (url-prepare-cache-for-file fname-hdr))
	       (write-region (point-min) (point-max) fname nil 5)
	       (set-buffer (get-buffer-create " *cache-tmp*"))
	       (erase-buffer)
	       (insert "(setq ")
	       (mapcar
		(function
		 (lambda (x)
		   (insert (car x) " "
			   (cond ((null (setq x (cdr x))) "nil")
				 ((stringp x) (prin1-to-string x))
				 ((listp x) (concat "'" (prin1-to-string x)))
				 ((numberp x) (int-to-string x))
				 (t "'???")) "\n")))
		info)
	       (insert ")\n")
	       (write-region (point-min) (point-max) fname-hdr nil 5)))))))
	
	     
(defun url-is-cached (url)
  "Return non-nil if the URL is cached."
  (let* ((fname (url-create-cached-filename url))
	 (attribs (file-attributes fname)))
    (and fname				; got a filename
	 (file-exists-p fname)		; file exists
	 (not (eq (nth 0 attribs) t))	; Its not a directory
	 (nth 5 attribs))))		; Can get last mod-time
    
(defun url-create-cached-filename-using-md5 (url)
  (if url
      (expand-file-name (md5 url)
			(concat url-temporary-directory "/"
				(user-real-login-name)))))

(defun url-create-cached-filename (url)
  "Return a filename in the local cache for URL"
  (if url
      (let* ((url url)
	     (urlobj (if (vectorp url)
			 url
		       (url-generic-parse-url url)))
	     (protocol (url-type urlobj))
	     (hostname (url-host urlobj))
	     (host-components
	      (cons
	       (user-real-login-name)
	       (cons (or protocol "file")
		     (nreverse
		      (delq nil
			    (mm-string-to-tokens
			     (or hostname "localhost") ?.))))))
	     (fname    (url-filename urlobj)))
	(if (and fname (/= (length fname) 0) (= (aref fname 0) ?/))
	    (setq fname (substring fname 1 nil)))
	(if fname
	    (let ((slash nil))
	      (setq fname
		    (mapconcat
		     (function
		      (lambda (x)
			(cond
			 ((and (= ?/ x) slash)
			  (setq slash nil)
			  "%2F")
			 ((= ?/ x)
			  (setq slash t)
			  "/")
			 (t
			  (setq slash nil)
			  (char-to-string x))))) fname ""))))

	(if (and fname (memq system-type '(ms-windows ms-dos windows-nt))
		 (string-match "\\([A-Za-z]\\):[/\\]" fname))
	    (setq fname (concat (url-match fname 1) "/"
				(substring fname (match-end 0)))))
	
	(setq fname (and fname
			 (mapconcat
			  (function (lambda (x)
				      (if (= x ?~) "" (char-to-string x))))
			  fname ""))
	      fname (cond
		     ((null fname) nil)
		     ((or (string= "" fname) (string= "/" fname))
		      url-directory-index-file)
		     ((= (string-to-char fname) ?/)
		      (if (string= (substring fname -1 nil) "/")
			  (concat fname url-directory-index-file)
			(substring fname 1 nil)))
		     (t
		      (if (string= (substring fname -1 nil) "/")
			  (concat fname url-directory-index-file)
			fname))))

	;; Honor hideous 8.3 filename limitations on dos and windows
	;; we don't have to worry about this in Windows NT/95 (or OS/2?)
	(if (and fname (memq system-type '(ms-windows ms-dos)))
	    (let ((base (url-file-extension fname t))
		  (ext  (url-file-extension fname nil)))
	      (setq fname (concat (substring base 0 (min 8 (length base)))
				  (substring ext  0 (min 4 (length ext)))))
	      (setq host-components
		    (mapcar
		     (function
		      (lambda (x)
			(if (> (length x) 8)
			    (concat 
			     (substring x 0 8) "."
			     (substring x 8 (min (length x) 11)))
			  x)))
		     host-components))))

	(and fname
	     (expand-file-name fname
			       (expand-file-name
				(mapconcat 'identity host-components "/")
				url-temporary-directory))))))

(defun url-extract-from-cache (fnam)
  "Extract FNAM from the local disk cache"
  (set-buffer (get-buffer-create url-working-buffer))
  (erase-buffer)
  (setq url-current-mime-viewer nil)
  (cond
   ((or (null url-request-method)
	(string= url-request-method "GET"))
    (mm-insert-file-contents fnam)
    (load-file (concat
		(if (memq system-type '(ms-windows ms-dos os2))
		    (url-file-extension fnam t)
		  fnam) ".hdr")))
   ((string= url-request-method "HEAD")
    (load-file (concat (if (memq system-type '(ms-windows ms-dos os2))
			   (url-file-extension fnam t)
			 fnam) ".hdr"))
    (insert
     (mapconcat
      (function
       (lambda (hdr)
	 (if (equal (car hdr) "") ""
	   (concat (capitalize (car hdr)) ": " (cdr hdr)))))
      url-current-mime-headers "\n"))))
  (message "Extracted %s from cache" url-current-file))

;;;###autoload
(defun url-get-url-at-point (&optional pt)
  "Get the URL closest to point, but don't change your
position. Has a preference for looking backward when not
directly on a symbol."
  ;; Not at all perfect - point must be right in the name.
  (save-excursion
    (if pt (goto-char pt))
    (let ((filename-chars "%.?@a-zA-Z0-9---()_/:~=&") start url)
      (save-excursion
	;; first see if you're just past a filename
	(if (not (eobp))
	    (if (looking-at "[] \t\n[{}()]") ; whitespace or some parens
		(progn
		  (skip-chars-backward " \n\t\r({[]})")
		  (if (not (bobp))
		      (backward-char 1)))))
	(if (string-match (concat "[" filename-chars "]")
			  (char-to-string (following-char)))
	    (progn
	      (skip-chars-backward filename-chars)
	      (setq start (point))
	      (skip-chars-forward filename-chars))
	  (setq start (point)))
	(setq url (if (fboundp 'buffer-substring-no-properties)
		      (buffer-substring-no-properties start (point))
		    (buffer-substring start (point)))))
      (if (string-match "^URL:" url)
	  (setq url (substring url 4 nil)))
      (if (string-match "\\.$" url)
	  (setq url (substring url 0 -1)))
      (if (not (string-match url-nonrelative-link url))
	  (setq url nil))
      url)))

(defun url-eat-trailing-space (x)
  ;; Remove spaces/tabs at the end of a string
  (let ((y (1- (length x)))
	(skip-chars (list ?  ?\t ?\n)))
    (while (and (>= y 0) (memq (aref x y) skip-chars))
      (setq y (1- y)))
    (substring x 0 (1+ y))))

(defun url-strip-leading-spaces (x)
  ;; Remove spaces at the front of a string
  (let ((y (1- (length x)))
	(z 0)
	(skip-chars (list ?  ?\t ?\n)))
    (while (and (<= z y) (memq (aref x z) skip-chars))
      (setq z (1+ z)))
    (substring x z nil)))

(defun url-convert-newlines-to-spaces (x)
  "Convert newlines and carriage returns embedded in a string into spaces,
and swallow following whitespace.
The argument is not side-effected, but may be returned by this function."
  (if (string-match "[\n\r]+\\s-*" x)   ; [\\n\\r\\t ]
      (concat (substring x 0 (match-beginning 0)) " "
	      (url-convert-newlines-to-spaces
	       (substring x (match-end 0))))
    x))

;; Test cases
;; (url-convert-newlines-to-spaces "foo    bar")  ; nothing happens
;; (url-convert-newlines-to-spaces "foo\n  \t  bar") ; whitespace converted
;;
;; This implementation doesn't mangle the match-data, is fast, and doesn't
;; create garbage, but it leaves whitespace.
;; (defun url-convert-newlines-to-spaces (x)
;;   "Convert newlines and carriage returns embedded in a string into spaces.
;; The string is side-effected, then returned."
;;   (let ((i 0)
;;      (limit (length x)))
;;     (while (< i limit)
;;       (if (or (= ?\n (aref x i))
;;            (= ?\r (aref x i)))
;;        (aset x i ? ))
;;       (setq i (1+ i)))
;;     x))

(defun url-expand-file-name (url &optional default)
  "Convert URL to a fully specified URL, and canonicalize it.
Second arg DEFAULT is a URL to start with if URL is relative.
If DEFAULT is nil or missing, the current buffer's URL is used.
Path components that are `.' are removed, and 
path components followed by `..' are removed, along with the `..' itself."
  (if url
      (setq url (mapconcat (function (lambda (x)
				       (if (= x ?\n) "" (char-to-string x))))
			   (url-strip-leading-spaces
			    (url-eat-trailing-space url)) "")))
  (cond
   ((null url) nil)			; Something hosed!  Be graceful
   ((string-match "^#" url)		; Offset link, use it raw
    url)
   (t
    (let* ((urlobj (url-generic-parse-url url))
	   (inhibit-file-name-handlers t)
	   (defobj (cond
		    ((vectorp default) default)
		    (default (url-generic-parse-url default))
		    ((and (null default) url-current-object)
		     url-current-object)
		    (t (url-generic-parse-url (url-view-url t)))))
	   (expander (cdr-safe
		      (cdr-safe
		       (assoc (or (url-type urlobj)
				  (url-type defobj))
			      url-registered-protocols)))))
      (if (fboundp expander)
	  (funcall expander urlobj defobj)
	(message "Unknown URL scheme: %s" (or (url-type urlobj)
					     (url-type defobj)))
	(url-identity-expander urlobj defobj))
      (url-recreate-url urlobj)))))

(defun url-default-expander (urlobj defobj)
  ;; The default expansion routine - urlobj is modified by side effect!
  (url-set-type urlobj (or (url-type urlobj) (url-type defobj)))
  (url-set-port urlobj (or (url-port urlobj)
			   (and (string= (url-type urlobj)
					 (url-type defobj))
				(url-port defobj))))
  (if (not (string= "file" (url-type urlobj)))
      (url-set-host urlobj (or (url-host urlobj) (url-host defobj))))
  (if (string= "ftp"  (url-type urlobj))
      (url-set-user urlobj (or (url-user urlobj) (url-user defobj))))
  (if (string= (url-filename urlobj) "")
      (url-set-filename urlobj "/"))
  (if (string-match "^/" (url-filename urlobj))
      nil
    (url-set-filename urlobj
		      (url-remove-relative-links
		       (concat (url-basepath (url-filename defobj))
			       (url-filename urlobj))))))

(defun url-identity-expander (urlobj defobj)
  (url-set-type urlobj (or (url-type urlobj) (url-type defobj))))

(defun url-hexify-string (str)
  "Escape characters in a string"
  (if (and (boundp 'MULE) str)
      (setq str (code-convert-string 
 		 str *internal* url-mule-retrieval-coding-system)))
  (setq str (mapconcat
	     (function
	      (lambda (char)
		(if (or (> char ?z)
			(< char ?-)
			(and (< char ?a)
			     (> char ?Z))
			(and (< char ?@)
			     (> char ?:)))
		    (if (< char 16)
			(upcase (format "%%0%x" char))
		      (upcase (format "%%%x" char)))
		  (char-to-string char)))) str "")))

(defun url-make-sequence (start end)
  "Make a sequence (list) of numbers from START to END"
  (cond
   ((= start end) '())
   ((> start end) '())
   (t
    (let ((sqnc '()))
      (while (<= start end)
	(setq sqnc (cons end sqnc)
	      end (1- end)))
      sqnc))))
 
(defun url-file-extension (fname &optional x)
  "Return the filename extension of FNAME.  If optional variable X is t,
then return the basename of the file with the extension stripped off."
  (if (and fname (string-match "\\.[^./]+$" fname))
      (if x (substring fname 0 (match-beginning 0))
	(substring fname (match-beginning 0) nil))
    ;;
    ;; If fname has no extension, and x then return fname itself instead of 
    ;; nothing. When caching it allows the correct .hdr file to be produced
    ;; for filenames without extension.
    ;;
    (if x
 	fname
      "")))

(defun url-basepath (file &optional x)
  "Return the base pathname of FILE, or the actual filename if X is true"
  (cond
   ((null file) "")
   (x (file-name-nondirectory file))
   (t (file-name-directory file))))

(defun url-unhex (x)
  (if (> x ?9)
      (if (>= x ?a)
	  (+ 10 (- x ?a))
	(+ 10 (- x ?A)))
    (- x ?0)))

(defun url-unhex-string (str)
  "Remove %XXX embedded spaces, etc in a url"
  (setq str (or str ""))
  (let ((tmp ""))
    (while (string-match "%[0-9a-f][0-9a-f]" str)
      (let* ((start (match-beginning 0))
	     (ch1 (url-unhex (elt str (+ start 1))))
	     (code (+ (* 16 ch1)
		      (url-unhex (elt str (+ start 2))))))
	(setq tmp
	      (concat 
	       tmp (substring str 0 start)
	       (if (or (= code ?\n) (= code ?\r)) " " (char-to-string code)))
	      str (substring str (match-end 0)))))
    (setq tmp (concat tmp str))
    tmp))

(defun url-clean-text ()
  "Clean up a buffer, removing any excess garbage from a gateway mechanism,
and decoding any MIME content-transfer-encoding used."
  (set-buffer url-working-buffer)
  (goto-char (point-min))
  (skip-chars-forward (if (memq url-gateway-method '(host program))
			  " \t\n" "\n"))
  (delete-region (point) (point-min))
  (url-replace-regexp "Connection closed by.*" "")
  (url-replace-regexp "Process WWW.*" ""))

(defun url-uncompress ()
  "Do any necessary uncompression on `url-working-buffer'"
  (set-buffer url-working-buffer)
  (if (not url-inhibit-uncompression)
      (let* ((extn (url-file-extension url-current-file))
	     (decoder nil)
	     (code-1 (cdr-safe
		      (assoc "content-transfer-encoding"
			     url-current-mime-headers)))
	     (code-2 (cdr-safe
		      (assoc "content-encoding" url-current-mime-headers)))
	     (code-3 (and (not code-1) (not code-2)
			  (cdr-safe (assoc extn url-uncompressor-alist))))
	     (done nil)
	     (default-process-coding-system
	       (if (boundp 'MULE) (cons *noconv* *noconv*))))
	(mapcar
	 (function
	  (lambda (code)
	    (setq decoder (and (not (member code done))
			       (cdr-safe
				(assoc code mm-content-transfer-encodings)))
		  done (cons code done))
	    (cond
	     ((null decoder) nil)
	     ((stringp decoder)
	      (message "Decoding...")
	      (call-process-region (point-min) (point-max) decoder t t nil)
	      (message "Decoding... done."))
	     ((listp decoder)
	      (apply 'call-process-region (point-min) (point-max)
		     (car decoder) t t nil (cdr decoder)))
	     ((and (symbolp decoder) (fboundp decoder))
	      (message "Decoding...")
	      (funcall decoder (point-min) (point-max))
	      (message "Decoding... done."))
	     (t
	      (error "Bad entry for %s in `mm-content-transfer-encodings'"
		     code)))))
	 (list code-1 code-2 code-3))))
  (set-buffer-modified-p nil))

(defun url-filter (proc string)
  (save-excursion
    (set-buffer url-working-buffer)
    (insert string)
    (if (string-match "\nConnection closed by" string)
	(progn (set-process-filter proc nil)
	       (url-sentinel proc string))))
  string)

(defun url-sentinel (proc string)
  (set-buffer (get-buffer (process-buffer proc)))
  (if (boundp 'after-change-functions)
      (remove-hook 'after-change-functions 'url-after-change-function))
  (let ((status nil))
    (if url-be-asynchronous
	(progn
	  (url-clean-text)
	  (cond
	   ((and (null proc) (not (get-buffer url-working-buffer))) nil)
	   ((url-mime-response-p) (setq status (url-parse-mime-headers))))
	  (if (not url-current-mime-type)
	      (setq url-current-mime-type (mm-extension-to-mime
					   (url-file-extension
					    url-current-file))))))
    (if (member status '(401 301 302 303 204))
	nil
      (funcall url-default-retrieval-proc (buffer-name)))))

(defun url-remove-relative-links (name)
  ;; Strip . and .. from pathnames
  (let ((new (if (not (string-match "^/" name))
		 (concat "/" name)
	       name)))
    (while (string-match "/\\([^/]*/\\.\\./\\)" new)
      (setq new (concat (substring new 0 (match-beginning 1))
			(substring new (match-end 1)))))
    (while (string-match "/\\(\\./\\)" new)
      (setq new (concat (substring new 0 (match-beginning 1))
			(substring new (match-end 1)))))
    new))

(defun url-truncate-url-for-viewing (url)
  (let* ((fr-width (frame-width))
  	 (str-width (length url))
 	 (tail (file-name-nondirectory url))
 	 (fname nil)
 	 (modified 0)
 	 (urlobj nil))
    ;; The first thing that can go are the search strings
    (if (and (>= str-width fr-width)
  	     (string-match "?" url))
  	(setq url (substring url 0 (match-beginning 0))
  	      str-width (length url)
  	      tail (file-name-nondirectory url)))
    (if (< str-width fr-width)
 	nil				; Hey, we are done!
      (setq urlobj (url-generic-parse-url url)
 	    fname (url-filename urlobj)
 	    fr-width (- fr-width 4))
      (while (and (>= str-width fr-width)
 		  (string-match "/" fname))
 	(setq fname (substring fname (match-end 0) nil)
 	      modified (1+ modified))
 	(url-set-filename urlobj fname)
 	(setq url (url-recreate-url urlobj)
 	      str-width (length url)))
      (if (> modified 1)
 	  (setq fname (concat "/.../" fname))
 	(setq fname (concat "/" fname)))
      (url-set-filename urlobj fname)
      (setq url (url-recreate-url urlobj)))
    url))

(defun url-view-url (&optional no-show)
  "View the current document's URL.  Optional argument NO-SHOW means
just return the URL, don't show it in the minibuffer."
  (interactive)
  (let ((url ""))
    (cond
     ((equal url-current-type "gopher")
      (setq url (format "%s://%s%s/%s"
			url-current-type url-current-server
			(if (or (null url-current-port)
				(string= "70" url-current-port)) ""
			  (concat ":" url-current-port))
			url-current-file)))
     ((equal url-current-type "news")
      (setq url (concat "news:"
			(if (not (equal url-current-server
					url-news-server))
			    (concat "//" url-current-server
				    (if (or (null url-current-port)
					    (string= "119" url-current-port))
					""
				      (concat ":" url-current-port)) "/"))
			url-current-file)))
     ((equal url-current-type "about")
      (setq url (concat "about:" url-current-file)))
     ((member url-current-type '("http" "shttp" "https"))
      (setq url (format  "%s://%s%s/%s" url-current-type url-current-server
			 (if (or (null url-current-port)
				 (string= "80" url-current-port))
			     ""
			   (concat ":" url-current-port))
			 (if (and url-current-file
				  (= ?/ (string-to-char url-current-file)))
			     (substring url-current-file 1 nil)
			   url-current-file))))
     ((equal url-current-type "ftp")
      (setq url (format "%s://%s%s/%s" url-current-type
			(if (and url-current-user
				 (not (string= "anonymous" url-current-user)))
			    (concat url-current-user "@") "")
			url-current-server
			(if (and url-current-file
				 (= ?/ (string-to-char url-current-file)))
			    (substring url-current-file 1 nil)
			  url-current-file))))
     ((and (member url-current-type '("file" nil)) url-current-file)
      (setq url (format "file:%s" url-current-file)))
     ((equal url-current-type "www")
      (setq url (format "www:/%s/%s" url-current-server url-current-file))))
    (if (not no-show) (message "%s" url) url)))

(defun url-parse-Netscape-history (fname)
  ;; Parse a Netscape/X style global history list.
  (let (pos				; Position holder
	url				; The URL
	time)				; Last time accessed
    (goto-char (point-min))
    (skip-chars-forward "^\n")
    (skip-chars-forward "\n \t")	; Skip past the tag line
    (setq url-global-history-hash-table (url-make-hashtable 131))
    ;; Here we will go to the end of the line and
    ;; skip back over a token, since we might run
    ;; into spaces in URLs, depending on how much
    ;; smarter netscape is than the old XMosaic :)
    (while (not (eobp))
      (setq pos (point))
      (end-of-line)
      (skip-chars-backward "^ \t")
      (skip-chars-backward " \t")
      (setq url (buffer-substring pos (point))
	    pos (1+ (point)))
      (skip-chars-forward "^\n")
      (setq time (buffer-substring pos (point)))
      (skip-chars-forward "\n")
      (url-puthash url time url-global-history-hash-table))))

(defun url-parse-Mosaic-history (fname)
  ;; Parse an NCSA Mosaic/X style global history list
  (goto-char (point-min))
  (skip-chars-forward "^\n")
  (skip-chars-forward "\n \t")	; Skip past the tag line
  (skip-chars-forward "^\n")
  (skip-chars-forward "\n \t")	; Skip past the second tag line
  (setq url-global-history-hash-table (url-make-hashtable 131))
  (let (pos				; Temporary position holder
	bol				; Beginning-of-line
	url				; URL
	time				; Time
	last-end			; Last ending point
	)
    (while (not (eobp))
      (setq bol (point))
      (end-of-line)
      (setq pos (point)
	    last-end (point))
      (skip-chars-backward "^ \t" bol)	; Skip over year
      (skip-chars-backward " \t" bol)
      (skip-chars-backward "^ \t" bol)	; Skip over time
      (skip-chars-backward " \t" bol)
      (skip-chars-backward "^ \t" bol)	; Skip over day #
      (skip-chars-backward " \t" bol)
      (skip-chars-backward "^ \t" bol)	; Skip over month
      (skip-chars-backward " \t" bol)
      (skip-chars-backward "^ \t" bol)	; Skip over day abbrev.
      (if (bolp)
	  nil				; Malformed entry!!! Ack! Bailout!
	(setq time (buffer-substring pos (point)))
	(skip-chars-backward " \t")
	(setq pos (point)))
      (beginning-of-line)
      (setq url (buffer-substring (point) pos))
      (goto-char (min (1+ last-end) (point-max))) ; Goto next line
      (if (/= (length url) 0)
	  (url-puthash url time url-global-history-hash-table)))))

(defun url-parse-Emacs-history (&optional fname)
  ;; Parse out the Emacs-w3 global history file for completion, etc.
  (or fname (setq fname (expand-file-name url-global-history-file)))
  (cond
   ((not (file-exists-p fname))
    (message "%s does not exist." fname))
   ((not (file-readable-p fname))
    (message "%s is unreadable." fname))
   (t
    (condition-case ()
	(load fname nil t)
      (error (message "Could not load %s" fname)))
    (if (boundp 'url-global-history-completion-list)
	;; Hey!  Automatic conversion of old format!
	(progn
	  (setq url-global-history-hash-table (url-make-hashtable 131))
	  (mapcar (function
		   (lambda (x)
		     (url-puthash (car x) (cdr x)
				 url-global-history-hash-table)))
		  (symbol-value 'url-global-history-completion-list)))))))
			    

(defun url-parse-global-history (&optional fname)
  ;; Parse out the mosaic global history file for completions, etc.
  (or fname (setq fname (expand-file-name url-global-history-file)))
  (cond
   ((not (file-exists-p fname))
    (message "%s does not exist." fname))
   ((not (file-readable-p fname))
    (message "%s is unreadable." fname))
   (t
    (save-excursion
      (set-buffer (get-buffer-create " *url-tmp*"))
      (erase-buffer)
      (mm-insert-file-contents fname)
      (goto-char (point-min))
      (cond
       ((looking-at "(setq") (url-parse-Emacs-history fname))
       ((looking-at "ncsa-mosaic") (url-parse-Mosaic-history fname))
       ((or (looking-at "MCOM-") (looking-at "netscape"))
	(url-parse-Netscape-history fname))
       (t
	(url-warn 'url (format "Cannot deduce type of history file: %s"
			       fname))))))))

(defun url-write-Emacs-history (fname)
  ;; Write an Emacs-w3 style global history list into FNAME
  (erase-buffer)
  (let ((count 0))
    (url-maphash (function
		  (lambda (key value)
		    (setq count (1+ count))
		    (insert "(url-puthash " (prin1-to-string key)
			    (if (not (stringp value)) " '" "")
			    (prin1-to-string value)
			    " url-global-history-hash-table)\n")))
		 url-global-history-hash-table)
    (goto-char (point-min))
    (insert (format
	     "(setq url-global-history-hash-table (url-make-hashtable %d))\n"
	     (/ count 4)))
    (goto-char (point-max))
    (insert "\n")
    (write-file fname)))

(defun url-write-Netscape-history (fname)
  ;; Write a Netscape-style global history list into FNAME
  (erase-buffer)
  (let ((last-valid-time "785305714"))	; Picked out of thin air,
					; in case first in assoc list
					; doesn't have a valid time
    (goto-char (point-min))
    (insert "MCOM-Global-history-file-1\n")
    (url-maphash (function
		  (lambda (url time)
		    (if (or (not (stringp time)) (string-match " \t" time))
			(setq time last-valid-time)
		      (setq last-valid-time time))
		    (insert (concat url " " time "\n"))))
		 url-global-history-hash-table)
    (write-file fname)))

(defun url-write-Mosaic-history (fname)
  ;; Write a Mosaic/X-style global history list into FNAME
  (erase-buffer)
    (goto-char (point-min))
    (insert "ncsa-mosaic-history-format-1\nGlobal\n")
    (url-maphash (function
		  (lambda (url time)
		    (if (and (listp time) url-current-time-string-has-args)
			(setq time (current-time-string time)))
		    (if (or (not (stringp time))
			    (not (string-match " " time)))
			(setq time (current-time-string)))
		    (insert (concat url " " time "\n"))))
		 url-global-history-hash-table)
    (write-file fname))

(defun url-write-global-history (&optional fname)
  "Write the global history file into `url-global-history-file'.
The type of data written is determined by what is in the file to begin
with.  If the type of storage cannot be determined, then prompt the
user for what type to save as."
  (interactive)
  (or fname (setq fname (expand-file-name url-global-history-file)))
  (cond
   ((not (file-writable-p fname))
    (message "%s is unwritable." fname))
   (t
    (save-excursion
      (set-buffer (get-buffer-create " *url-tmp*"))
      (erase-buffer)
      (condition-case ()
	  (mm-insert-file-contents fname)
	(error nil))
      (goto-char (point-min))
      (cond
       ((looking-at "ncsa-mosaic") (url-write-Mosaic-history fname))
       ((looking-at "MCOM-") (url-write-Netscape-history fname))
       ((looking-at "netscape") (url-write-Netscape-history fname))
       ((looking-at "(setq") (url-write-Emacs-history fname))
       (t
	(let* ((opts '(("Netscape" . url-write-Netscape-history)
		       ("Mosaic"   . url-write-Mosaic-history)
		       ("Emacs"    . url-write-Emacs-history)))
	       (val (completing-read "Save history in what format: "
				     opts nil t (cons "Mosaic" 0) nil)))
	  (if (string= val "")
	      nil
	    (funcall (cdr (assoc val opts)) fname)))))
      (kill-buffer (current-buffer))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The main URL fetching interface
;;; -------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun url-popup-info (url)
  "Retrieve the HTTP/1.0 headers and display them in a temp buffer."
  (let* ((urlobj (url-generic-parse-url url))
	 (type (url-type urlobj))
	 data)
    (cond
     ((string= type "http")
      (let ((url-request-method "HEAD")
	    (url-automatic-caching nil)
	    (url-inhibit-mime-parsing t)
	    (url-working-buffer " *popup*"))
	(save-excursion
	  (set-buffer (get-buffer-create url-working-buffer))
	  (erase-buffer)
	  (setq url-be-asynchronous nil)
	  (url-retrieve url)
	  (subst-char-in-region (point-min) (point-max) ?\r ? )
	  (buffer-string))))
     ((or (string= type "file") (string= type "ftp"))
      (setq data (url-file-attributes url))
      (set-buffer (get-buffer-create
		   (url-generate-new-buffer-name "*Header Info*")))
      (erase-buffer)
      (if data
	  (concat (if (stringp (nth 0 data))
		      (concat "    Linked to: " (nth 0 data))
		    (concat "    Directory: " (if (nth 0 data) "Yes" "No")))
		  "\n        Links: " (int-to-string (nth 1 data))
		  "\n     File UID: " (int-to-string (nth 2 data))
		  "\n     File GID: " (int-to-string (nth 3 data))
		  (if url-current-time-string-has-args
		      (concat
		       "\n  Last Access: " (current-time-string (nth 4 data))
		       "\nLast Modified: " (current-time-string (nth 5 data))
		       "\n Last Changed: " (current-time-string (nth 6 data)))
		    "")
		  "\n Size (bytes): " (int-to-string (nth 7 data))
		  "\n    File Type: " (or (nth 8 data) "text/plain"))
	(concat "No info found for " url)))
     ((and (string= type "news") (string-match "@" url))
      (let ((art (url-filename urlobj)))
	(if (not (string= (substring art -1 nil) ">"))
	    (setq art (concat "<" art ">")))
	(url-get-headers-from-article-id art)))
     (t (concat "Don't know how to find information on " url)))))

(defun url-decode-text ()
  ;; Decode text transmitted by NNTP.
  ;; 0. Delete status line.
  ;; 1. Delete `^M' at end of line.
  ;; 2. Delete `.' at end of buffer (end of text mark).
  ;; 3. Delete `.' at beginning of line."
  (save-excursion
    (set-buffer nntp-server-buffer)
    ;; Insert newline at end of buffer.
    (goto-char (point-max))
    (if (not (bolp))
	(insert "\n"))
    ;; Delete status line.
    (goto-char (point-min))
    (delete-region (point) (progn (forward-line 1) (point)))
    ;; Delete `^M' at end of line.
    ;; (replace-regexp "\r$" "")
    (while (not (eobp))
      (end-of-line)
      (if (= (preceding-char) ?\r)
	  (delete-char -1))
      (forward-line 1)
      )
    ;; Delete `.' at end of buffer (end of text mark).
    (goto-char (point-max))
    (forward-line -1)			;(beginning-of-line)
    (if (looking-at "^\\.$")
	(delete-region (point) (progn (forward-line 1) (point))))
    ;; Replace `..' at beginning of line with `.'.
    (goto-char (point-min))
    ;; (replace-regexp "^\\.\\." ".")
    (while (search-forward "\n.." nil t)
      (delete-char -1))
    ))

(defun url-get-headers-from-article-id (art)
  ;; Return the HEAD of ART (a usenet news article)
  (cond
   ((string-match "flee" nntp-version)
    (nntp/command "HEAD" art)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (while (progn (goto-char (point-min))
		    (not (re-search-forward "^.\r*$" nil t)))
	(url-accept-process-output nntp/connection))))
   (t
    (nntp-send-command "^\\.\r$" "HEAD" art)
    (url-decode-text)))
  (save-excursion
    (set-buffer nntp-server-buffer)
    (buffer-string)))

(defvar url-external-retrieval-program "www"
  "*Name of the external executable to run to retrieve URLs.")

(defvar url-external-retrieval-args '("-source")
  "*A list of arguments to pass to `url-external-retrieval-program' to
retrieve a URL by its HTML source.")

(defun url-retrieve-externally (url &optional no-cache)
  (if (get-buffer url-working-buffer)
      (save-excursion
	(set-buffer url-working-buffer)
	(set-buffer-modified-p nil)
	(kill-buffer url-working-buffer)))
  (set-buffer (get-buffer-create url-working-buffer))
  (let* ((args (append url-external-retrieval-args (list url)))
	 (urlobj (url-generic-parse-url url))
	 (type (url-type urlobj)))
    (if (or (member type '("www" "about" "mailto" "mailserver"))
	    (url-file-directly-accessible-p urlobj))
	(url-retrieve-internally url)
      (url-lazy-message "Retrieving %s..." url)
      (apply 'call-process url-external-retrieval-program
	     nil t nil args)
      (url-lazy-message "Retrieving %s... done" url)
      (if (and type urlobj)
	  (setq url-current-server (url-host urlobj)
		url-current-type (url-type urlobj)
		url-current-port (url-port urlobj)
		url-current-file (url-filename urlobj)))
      (if (member url-current-file '("/" ""))
	  (setq url-current-mime-type "text/html")))))

(defconst weekday-alist
  '(("Sunday" . 0) ("Monday" . 1) ("Tuesday" . 2) ("Wednesday" . 3)
    ("Thursday" . 4) ("Friday" . 5) ("Saturday" . 6)
    ("Tues" . 2) ("Thurs" . 4)
    ("Sun" . 0) ("Mon" . 1) ("Tue" . 2) ("Wed" . 3)
    ("Thu" . 4) ("Fri" . 5) ("Sat" . 6)))

(defconst monthabbrev-alist
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12))
  )

(defun url-get-normalized-date (&optional specified-time)
  ;; Return a 'real' date string that most HTTP servers can understand.
  (require 'timezone)
  (let* ((raw (if specified-time (current-time-string specified-time)
		(current-time-string)))
	 (gmt (timezone-make-date-arpa-standard raw
						(nth 1 (current-time-zone))
						"GMT"))
	 (parsed (timezone-parse-date gmt))
	 (day (cdr-safe (assoc (substring raw 0 3) weekday-alist)))
	 (year nil)
	 (month (car
		 (rassoc
		  (string-to-int (aref parsed 1)) monthabbrev-alist)))
	 )
    (setq day (or (car-safe (rassoc day weekday-alist))
		  (substring raw 0 3))
	  year (aref parsed 0))
    ;; This is needed for plexus servers, or the server will hang trying to
    ;; parse the if-modified-since header.  Hopefully, I can take this out
    ;; soon.
    (if (and year (> (length year) 2))
	(setq year (substring year -2 nil)))

    (concat day ", " (aref parsed 2) "-" month "-" year " "
	    (aref parsed 3) " " (or (aref parsed 4)
				    (concat "[" (nth 1 (current-time-zone))
					    "]")))))

;;;###autoload
(defun url-cache-expired (url mod)
  "Return t iff a cached file has expired."
  (if (not (string-match url-nonrelative-link url))
      t
    (let* ((urlobj (url-generic-parse-url url))
	   (type (url-type urlobj)))
      (cond
       (url-standalone-mode
	(not (file-exists-p (url-create-cached-filename urlobj))))
       ((string= type "http")
	(if (not url-standalone-mode) t
	  (not (file-exists-p (url-create-cached-filename urlobj)))))
       ((not (fboundp 'current-time))
	t)
       ((member type '("file" "ftp"))
	(if (or (equal mod '(0 0)) (not mod))
	      (return t)
	    (or (> (nth 0 mod) (nth 0 (current-time)))
		(> (nth 1 mod) (nth 1 (current-time))))))
       (t nil)))))

(defun url-retrieve-internally (url &optional no-cache)
  (if (get-buffer url-working-buffer)
      (save-excursion
	(set-buffer url-working-buffer)
	(erase-buffer)
	(setq url-current-can-be-cached (not no-cache))
	(set-buffer-modified-p nil)))
  (let* ((urlobj (url-generic-parse-url url))
	 (type (url-type urlobj))
	 (url-using-proxy (and
			   (if (assoc "no_proxy" url-proxy-services)
			       (not (string-match
				     (cdr
				      (assoc "no_proxy" url-proxy-services))
				     url))
			     t)
			   (not
			    (and
			     (string-match "file:" url)
			     (not (string-match "file://" url))))
			   (cdr (assoc type url-proxy-services))))
	 (handler nil)
	 (original-url url)
	 (cached nil)
	 (tmp url-current-file))
    (if url-using-proxy (setq type "proxy"))
    (setq cached (url-is-cached url)
	  cached (and cached (not (url-cache-expired url cached)))
	  handler (if cached 'url-extract-from-cache
		    (car-safe
		     (cdr-safe (assoc (or type "auto")
				      url-registered-protocols))))
	  url (if cached (url-create-cached-filename url) url))
    (save-excursion
      (set-buffer (get-buffer-create url-working-buffer))
      (setq url-current-can-be-cached (not no-cache)))
    (if (and handler (fboundp handler))
	(funcall handler url)
      (set-buffer (get-buffer-create url-working-buffer))
      (setq url-current-file tmp)
      (erase-buffer)
      (insert "<title> Link Error! </title>\n"
	      "<h1> An error has occurred... </h1>\n"
	      (format "The link type <code>%s</code>" type)
	      " is unrecognized or unsupported at this time.<p>\n"
	      "If you feel this is an error, please "
	      "<a href=\"mailto://" url-bug-address "\">send me mail.</a>"
	      "<p><address>William Perry</address><br>"
	      "<address>" url-bug-address "</address>")
      (setq url-current-file "error.html"))
    (if (and
	 (not url-be-asynchronous)
	 (get-buffer url-working-buffer))
	(progn
	  (set-buffer url-working-buffer)
	  (if (not url-current-object)
	      (setq url-current-object urlobj))
	  (url-clean-text)))
    (cond
     ((equal type "wais") nil)
     ((and url-be-asynchronous (not cached) (equal type "http")) nil)
     ((not (get-buffer url-working-buffer)) nil)
     ((and (not url-inhibit-mime-parsing)
	   (or cached (url-mime-response-p t)))
      (or cached (url-parse-mime-headers nil t))))
    (if (and (or (not url-be-asynchronous)
		 (not (equal type "http")))
	     (not url-current-mime-type))
	(if (url-buffer-is-hypertext)
	    (setq url-current-mime-type "text/html")
	  (setq url-current-mime-type (mm-extension-to-mime
				      (url-file-extension
				       url-current-file)))))
    (if (and url-automatic-caching url-current-can-be-cached)
	(save-excursion
	  (url-store-in-cache url-working-buffer)))
    (if (not (url-hashtablep url-global-history-hash-table))
	(setq url-global-history-hash-table (url-make-hashtable 131)))
    (if (not (string-match "^about:" original-url))
	(url-puthash original-url (if url-current-time-string-has-args
				      (current-time)
				    (current-time-string))
		     url-global-history-hash-table))
    cached))

;;;###autoload
(defun url-retrieve (url &optional no-cache expected-md5)
  "Retrieve a document over the World Wide Web.
The document should be specified by its fully specified
Uniform Resource Locator.  No parsing is done, just return the
document as the server sent it.  The document is left in the
buffer specified by url-working-buffer.  url-working-buffer is killed
immediately before starting the transfer, so that no buffer-local
variables interfere with the retrieval.  HTTP/1.0 redirection will
be honored before this function exits."
  (url-do-setup)
  (if (and (fboundp 'set-text-properties)
	   (subrp (symbol-function 'set-text-properties)))
      (set-text-properties 0 (length url) nil url))
  (let ((status (url-retrieve-internally url no-cache)))
    (if (and expected-md5 url-check-md5s)
	(let ((cur-md5 (md5 (current-buffer))))
	  (if (not (string= cur-md5 expected-md5))
	      (and (not (funcall url-confirmation-func
				 "MD5s do not match, use anyway? "))
		   (error "MD5 error.")))))
    status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; How to register a protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-register-protocol (protocol &optional retrieve expander defport)
  "Register a protocol with the URL retrieval package.
PROTOCOL is the type of protocol being registers (http, nntp, etc),
         and is the first chunk of the URL.  ie: http:// URLs will be
         handled by the protocol registered as 'http'.  PROTOCOL can
         be either a symbol or a string - it is converted to a string,
         and lowercased before being registered.
RETRIEVE (optional) is the function to be called with a url as its
         only argument.  If this argument is omitted, then this looks
         for a function called 'url-PROTOCOL'.  A warning is shown if
         the function is undefined, but the protocol is still
         registered.
EXPANDER (optional) is the function to call to expand a relative link
         of type PROTOCOL.  If omitted, this defaults to
         `url-default-expander'

Any proxy information is read in from environment variables at this
time, so this function should only be called after dumping emacs."
  (let* ((protocol (cond
		    ((stringp protocol) (downcase protocol))
		    ((symbolp protocol) (downcase (symbol-name protocol)))
		    (t nil)))
		     
	 (retrieve (or retrieve (intern (concat "url-" protocol))))
	 (expander (or expander 'url-default-expander))
	 (cur-protocol (assoc protocol url-registered-protocols))
	 (urlobj nil)
	 (cur-proxy (assoc protocol url-proxy-services))
	 (env-proxy (or (getenv (concat protocol "_proxy"))
			(getenv (concat protocol "_PROXY"))
			(getenv (upcase (concat protocol "_PROXY"))))))

    (if (not protocol)
	(error "Invalid data to url-register-protocol."))
    
    (if (not (fboundp retrieve))
	(message "Warning: %s registered, but no function found." protocol))

    ;; Store the default port, if none previously specified and
    ;; defport given
    (if (and defport (not (assoc protocol url-default-ports)))
	(setq url-default-ports (cons (cons protocol defport)
				      url-default-ports)))
    
    ;; Store the appropriate information for later
    (if cur-protocol
	(setcdr cur-protocol (cons retrieve expander))
      (setq url-registered-protocols (cons (cons protocol
						 (cons retrieve expander))
					   url-registered-protocols)))

    ;; Store any proxying information - this will not overwrite an old
    ;; entry, so that people can still set this information in their
    ;; .emacs file
    (cond
     (cur-proxy nil)			; Keep their old settings
     ((null env-proxy) nil)		; No proxy setup
     ;; First check if its something like hostname:port
     ((string-match "^\\([^:]+\\):\\([0-9]+\\)$" env-proxy)
      (setq urlobj (url-generic-parse-url nil)) ; Get a blank object
      (url-set-type urlobj "http")
      (url-set-host urlobj (url-match env-proxy 1))
      (url-set-port urlobj (url-match env-proxy 2)))
     ;; Then check if its a fully specified URL
     ((string-match url-nonrelative-link env-proxy)
      (setq urlobj (url-generic-parse-url env-proxy))
      (url-set-type urlobj "http")
      (url-set-target urlobj nil))
     ;; Finally, fall back on the assumption that its just a hostname
     (t
      (setq urlobj (url-generic-parse-url nil)) ; Get a blank object
      (url-set-type urlobj "http")
      (url-set-host urlobj env-proxy)))

     (if (and (not cur-proxy) urlobj)
	 (progn
	   (setq url-proxy-services
		 (cons (cons protocol (url-recreate-url urlobj))
		       url-proxy-services))
	   (message "Using a proxy for %s..." protocol)))))

(require 'urlauth)
(provide 'url)
