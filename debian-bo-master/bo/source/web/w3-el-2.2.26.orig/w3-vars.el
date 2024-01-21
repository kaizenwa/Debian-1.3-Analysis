;;; w3-vars.el,v --- All variable definitions for emacs-w3
;; Author: wmperry
;; Created: 1995/10/15 19:58:42
;; Version: 1.171
;; Keywords: comm, help, hypermedia

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
;;; Variable definitions for w3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst w3-version-number
  (let ((x "p2-2-26"))
    (if (string-match "State:[ \t\n]+.\\([^ \t\n]+\\)" x)
	(setq x (substring x (match-beginning 1) (match-end 1)))
      (setq x (substring x 1)))
    (mapconcat
     (function (lambda (x) (if (= x ?-) "." (char-to-string x)))) x ""))
  "Version # of w3-mode.")

(defconst w3-version-date (let ((x "1996/03/11 06:58:42"))
			    (if (string-match "Date: \\([^ \t\n]+\\)" x)
				(substring x (match-beginning 1) (match-end 1))
			      x))
  "Date this version of w3-mode was released.")

(defconst w3-version
  (format "WWW %s %s" w3-version-number w3-version-date)
  "More descriptive version of w3-version-number.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General configuration variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-annotation-mode 'html-mode
  "*A symbol specifying the major mode to enter when doing annotations.")

(defvar w3-annotation-position 'bottom
  "*A symbol specifying where personal annotations should appear in a buffer.
Can be one of the symbols 'top or 'bottom.  If the symbol is eq to 'top, then
the annotations will appear at the top of the buffer.  If 'bottom, will appear
at the end of the buffer.")

(defvar w3-auto-image-alt t
  "*Whether emacs-w3 should create an alt attribute for an image that
is missing it.
If nil, emacs-w3 will not automatically create an ALT attribute.
If t, the alt attribute will be [IMAGE(nameofimage)]
If a string, it should be a string suitable for running through format,
   with only one %s, which will be replaced with just the filename of the
   graphic that is not loaded.")

(defvar w3-debug-html nil "*Whether to gripe about bad HTML or not.")

(defvar w3-debug-buffer "*HTML Debug*"
  "*Name of buffer to store debugging information in.")

(defvar w3-default-configuration-file nil
  "*Where per-user customizations of w3 are kept.")

(defvar w3-default-action 'w3-prepare-buffer
  "*A lisp symbol specifying what action to take for files with
extensions that are not mapped to a MIME type in `mm-mime-extensions'.
This is useful in case you ever run across files with weird extensions
\(.foo, .README, .READMEFIRST, etc).  This should not be required
anymore.

Possible values: any lisp symbol.  Should be a function that takes no
arguments.  The return value does not matter, it is ignored.  Some examples
are:

Action			Value
----------------------------------------------
Parse as HTML		'w3-prepare-buffer
View as text		'indented-text-mode")

(defvar w3-default-homepage nil
  "*The url to open at startup.  It can be any valid URL.  This will
default to the environment variable WWW_HOME if you do not set it in
your .emacs file. If WWW_HOME is undefined, then it will default to
the hypertext documentation for W3 at Indiana University.")

(defvar w3-default-stylesheet nil
  "*The filename of the users default stylesheet.")

(defvar w3-do-blinking nil
  "*Whether emacs-w3 should display blinking text.")

(defvar w3-do-incremental-display nil
  "*Whether to do incremental display of pages or not.")

(defvar w3-documents-menu-file nil
  "*Where the Mosaic documents-menu file is located.  This is a file
that has extra menus for the 'Navigate' menu.  This should be in the same
format as the Mosaic extra documents.menu.")

(defvar w3-dump-to-disk nil
  "*If non-nil, all W3 pages loaded will be dumped to disk.")

(defvar w3-emacs19-hack-faces-p nil
  "Whether emacs19 should try to emulate bold/underline faces when running
on a dumb terminal.")

(defvar w3-echo-link 'url
  "Whether to display the URL of a link when w3-forward-link or
w3-back-link is called.  Possible values are:

  url    == show the url of the target in the minibuffer
  text   == show the text of the link in the minibuffer
  nil    == show nothing")
  
(defvar w3-horizontal-rule-char ?-
  "*The character to use to create a horizontal rule.
Must be the character's code, not a string.  This character is
replicated across the screen to create a division.")

(defvar w3-hotlist-file nil
  "*Hotlist filename.
This should be the name of a file that is stored in either
NCSA's Mosaic/X or Netscape/X format.  It is used to keep a listing
of commonly accessed URL's without having to go through 20 levels of
menus to get to them.")

(defvar w3-html2latex-args "-s -"
  "*Args to pass `w3-html2latex-prog'.  This should send the LaTeX source
to standard output.")

(defvar w3-html2latex-prog "html2latex"
  "*Program to convert html to latex.")

(defvar w3-icon-directory-list
  '("http://cs.indiana.edu/elisp/w3/icons/")
  "*A list of directories to look in for the w3 standard icons...
All entries must end in a /!")

(defvar w3-indent-level 4
  "*Default # of spaces to indent instead of using TABs.  This is
necessary to preserve tabs in PRE segments yet still get smaller
indentation for lists, etc.")

(defvar w3-keep-old-buffers t
  "*Whether to keep old buffers around when following links.")

(defvar w3-latex-docstyle "[psfig]{article}"
  "*The documentstyle to use when printing/mailing converted HTML
files in LaTeX.  Good defaults are:
{article}, [psfig,twocolumn]{article}, etc.")

(defvar w3-link-delimiter-info nil
  "*A function to call to get extra information about a link and
include it in a buffer.  Will be placed after the link and any other
delimiters.")

(defvar w3-mail-command 'mail
  "*This function will be called whenever w3 needs to send mail.  It should
enter a mail-mode-like buffer in the current window.
`w3-mail-other-window-command' will be used if w3-mutable-windows is t.
The commands `mail-to' and `mail-subject' should still work in this
buffer, and it should use mail-header-separator if possible.")

(defvar w3-mail-other-window-command 'mail-other-window
  "*This function will be called whenever w3 needs to send mail in
another window.  It should enter a mail-mode-like buffer in a
different window.  The commands `mail-to' and `mail-subject' should still
work in this buffer, and it should use mail-header-separator if
possible.")

(defvar w3-max-inlined-image-size nil
  "*The maximum byte size of a file to transfer as an inlined image.
If an image is being retrieved and exceeds this size, then it will be
cancelled.  This works best on HTTP/1.0 servers that send a
Content-length header, otherwise the image is retrieved up until the
max number of bytes is retrieved, then killed.")

(defvar w3-max-menu-length 35
  "*The maximum length of a pulldown menu before it will be split into
smaller chunks, with the first part as a submenu, followed by the rest
of the menu.")

(defvar w3-max-menu-width 40 "*The maximum width of a pulldown menu choice.")

(defvar w3-modeline-format
  '("" "  %&%& "
    ("W3: "
     (40 (-40 "%b"))
     " "
     (w3-current-isindex "[Searchable]  ")
     "%p" "  " global-mode-string))
  "*The modeline format string when in w3 mode")  

(defvar w3-mule-attribute 'underline
  "*How to highlight items in Mule (Multi-Linugual Emacs).")

(defvar w3-mutable-windows nil
  "*Controls how new WWW documents are displayed.  If this is set to
non-nil and pop-up-windows is non-nil, then new buffers will be shown
in another window.  If either is nil, then it will replace the document
in the current window.")

(defvar w3-netscape-configuration-file nil
  "*A Netscape-for-X style configuration file.  This file will only be read if
and only if `w3-use-netscape-configuration-file' is non-nil.")

(defvar w3-notify 'friendly
  "*Selects the behavior when w3 page is ready.
This variable may have one of the following values:

newframe   -- put the w3 page in its own frame
bully      -- make the w3 page the current buffer and only window
aggressive -- make the w3 page the current buffer in the other window
friendly   -- display  w3page in other window but don't make current
polite     -- don't display w3 page, but prints message when ready (beeps)
quiet      -- like `polite', but don't beep
meek       -- make no indication that page is ready

Any other value of `w3-notify' is equivalent to `meek'.")

(defvar w3-personal-annotation-directory nil
  "*Directory where w3 looks for personal annotations.
This is a directory that should hold the personal annotations stored in
a Mosaic-compatible format.")

(defvar w3-ppmtoxbm-command "ppmtopgm | pgmtopbm | pbmtoxbm"
  "*The command used to convert from the portable-pixmap graphics format
to an x bitmap.  This will only ever be used if XEmacs doesn't have support
for XPM.")

(defvar w3-ppmtoxpm-command "ppmtoxpm"
  "*The command used to convert from the portable-pixmap graphics format
to XPM.  The XPM _MUST_ be in version 3 format.")

(defvar w3-print-command "lpr -h -d"
  "*Print command for dvi files.
This is usually lpr -h -d to send it to a postscript printer, but you can set
it up so that it is any command that takes a dvi file as its last argument.")

(defvar w3-reuse-buffers nil
  "What to do when following a link will re-fetch a document that has
already been fetched into a W3 buffer.  Possible values are: nil,
'yes, and 'no.  Nil means ask the user if we should reuse the buffer
(this is the default value).  A value of 'yes means assume the user
wants us to reuse the buffer.  A value of 'no means assume the user
wants us to re-fetch the document.

This will also accept:
'no 'never 'reload	==> always reload
'yes 'reuse 'always	==> always reuse
'ask nil		==> always ask")

(defvar w3-right-border 2
  "*Amount of space to leave on right margin of WWW buffers.
This amount is subtracted from (window-width) for each new WWW buffer
and used as the new fill-column.")

(defvar w3-right-justify-address t
  "*Whether to make address fields right justified, like Arena.")

(defvar w3-show-headers nil
  "*This is a list of regexps that match HTTP/1.0 headers to show at
the end of a buffer.  All the headers being matched against will be
in lowercase.  All matching headers will be inserted at the end of the
buffer in a <UL> list.")

(defvar w3-show-status t
  "*Whether to show a running total of bytes transferred.  Can cause a
large hit if using a remote X display over a slow link, or a terminal
with a slow modem.")

(defvar w3-starting-documents
  '(("Internet Starting Points"  "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/StartingPoints/NetworkStartingPoints.html")
    ("Internet Resources Meta-index"  "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/MetaIndex.html")
    ("NCSA's What's New"  "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/whats-new.html"))
  "*An assoc list of titles and URLs for quick access.  These are just
defaults so that new users have somewhere to go.")

(defvar w3-temporary-directory "/tmp" "*Where temporary files go.")

(defvar w3-track-last-buffer nil
  "*Whether to track the last w3 buffer to automatically switch to with
 M-x w3.")

(defvar w3-track-mouse t
  "*Whether to track the mouse and message the url under the mouse.")

(defvar w3-use-forms-index t
  "*Non-nil means translate <ISINDEX> tags into a hypertext form.
A single text entry box will be drawn where the ISINDEX tag appears.
If t, the isindex handling will be the same as Mosaic for X.")

(defvar w3-use-html2latex nil
  "*This controls how HTML is converted into LaTeX for printing or mailing.
If nil, the w3-convert-html-to-latex function is used instead of the
html2latex in a subprocess.  The lisp function gives slightly better
formatting in my biased opinion.")

(defvar w3-use-netscape-configuration-file nil
  "*Whether to use a netscape configuration file to determine things like
home pages, link colors, etc.  If non-nil, then `w3-netscape-configuration-file'
is read in at startup.")

(defvar w3-honor-stylesheets t
  "*Whether to let a document specify a CSS stylesheet.")

(defvar w3-user-colors-take-precedence nil
  "*Whether to let a document define certain colors about itself.
Like foreground and background colors and pixmaps, color of links and
visited links, etc.")

(defvar w3-gc-cons-threshold-multiplier 1
  "Amount to temporarily multiply gc-cons-threshold by when parsing HTML.
Setting this to a number greater than 1 will result in less frequent
garbage collections when parsing an HTML document, which may often speed
up handling of a large document with many elements.  The disadvantage is
that it allows Emacs's total memory usage to grow larger, which may result
in later garbage collections taking more time.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hook Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-load-hook nil "*Hooks to be run after loading w3.")
(defvar w3-mode-hook nil "*Hooks to be run after entering w3-mode.")
(defvar w3-file-prepare-hook nil
  "*Hooks to be run before preparing a buffer.")
(defvar w3-file-done-hook nil "*Hooks to be run after preparing a buffer.")
(defvar w3-source-file-hook nil
  "*Hooks to be run after getting document source.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Figure out what flavor of emacs we are running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-running-epoch (and (boundp 'epoch::version)
			      (symbol-value 'epoch::version))
  "*In Epoch.")

(defvar w3-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "*In XEmacs or Lucid Emacs?.")

(defvar w3-running-FSF19 (and (string-match "^19" emacs-version)
			      (not w3-running-xemacs))
  "*In FSF v19 emacs?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Link delimiting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-delimit-emphasis 'guess
  "*Whether to use characters at the start and end of each bold/italic
region.  Types of characters are specified in w3-style-chars-assoc.")

(defvar w3-link-start-delimiter '("[[" . "{{")
  "*Put this at front of link if w3-delimit-links is t.")

(defvar w3-link-end-delimiter '("]]" . "}}")
  "*Put this at end of link if w3-delimit-links is t.")

(defvar w3-delimit-links 'guess
  "*Put brackets around links?  If this variable is eq to 'linkname, then
it will put the link # in brackets after the link text.  If it is nil, then
it will not put anything.  If it is non-nil and not eq to 'linkname, then
it will put [[ & ]] around the entire text of the link.  Is initially set
to be t iff in normal emacs.  Nil if in epoch or lucid emacs, since links
should be in different colors/fonts.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; embedded document variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-mpeg-size 150 "*The height and width of an mpeg window.")
(defvar w3-mpeg-args '("-loop") "*Arguments to mpeg_play.")
(defvar w3-mpeg-program "mpeg_play" "*The mpeg_play executable.")
(defvar w3-delayed-movies nil "A list of mpeg movies for this buffer.")
  
(defvar w3-embedded-data-converters
  '(("application/eqn" . w3-embed-eqn)
    ("application/postscript" . w3-embed-postscript)
    ("text/plain". w3-embed-text)
    ("text/html" . w3-embed-text)
    ("image/.*"  . w3-embed-image))
  "An assoc list of regular expressions to match against MIME content-types
for embedded data in HTML documents.  The cdr is a function to be passed
to 'funcall', with the embedded data and content-type as the sole arguments
passed to the function.")

(if w3-running-xemacs
    (progn
      (condition-case ()
	  (require 'annotations)
	(error nil))
      (setq w3-embedded-data-converters
	    (cons (cons "video/mpeg" 'w3-embed-mpeg)
		  w3-embedded-data-converters))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphics parsing stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-graphics-always-show-entities t
  "*Set to t to always show graphic entities, regardless of the value of
w3-delay-image-loads.  Useful if you keep the entities locally and aren't
worried about the transfer time on something that small.")

(defvar w3-graphics-list nil
  "*List of graphics already read in.")

(defvar w3-delay-image-loads nil
  "*Delay loading images for w3 or not?")

(defvar w3-delayed-images nil
  "*A buffer-local variable holding positions and urls of images within
the buffer.")

(defvar w3-delay-mpeg-loads t
  "*Whether to delay loading mpegs or not.")

(defvar w3-graphic-converter-alist
  '(
    ("image/x-xbitmap"        . "cat ")
    ("image/xbitmap"          . "cat ")
    ("image/xbm"              . "cat ")
    ("image/jpeg"             . "djpeg | %s")
    ("image/gif"              . "giftopnm  | %s")
    ("image/x-fax"            . "g3topbm   | %s")
    ("image/x-raster"         . "rasttoppm | %s")
    ("image/windowdump"       . "xwdtoppm  | %s")
    ("image/x-icon"           . "icontopbm | %s")
    ("image/portable-graymap" . "pgmtoppm  | %s")
    ("image/portable-pixmap"  . "cat ")
    ("image/x-pixmap"         . "cat ")
    ("image/x-xpixmap"        . "cat ")
    ("image/pict"             . "picttoppm | %s")
    ("image/x-rgb"            . "sgitopnm | %s")
    ("image/x-sgi"            . "sgitopnm | %s")
    ("image/x-macpaint"       . "macptopbm | %s")
    ("image/x-targa"          . "tgatoppm  | %s")
    ("image/tiff"             . "tifftopgm | pgmtoppm | %s")
    ) "*How to convert graphics into xpixmaps.")

(defvar w3-color-use-reducing 'guess
  "*Whether to use ppmquant/ppmdither to do color reducing for inlined images.
If you are using a 24bit display, you should set this to nil.")

(defvar w3-color-max-red 4
  "*Max # of red cells to allocate for inlined images.")

(defvar w3-color-max-green 4
  "*Max # of green cells to allocate for inlined images.")

(defvar w3-color-max-blue 4
  "*Max # of blue cells to allocate for inlined images.")

(defvar w3-color-filter 'ppmdither
  "*How to do color reducing on inlined images.
This should be a symbol, either ppmdither or ppmquant.
This variable only has any meaning if w3-color-use-reducing is non-nil.
Possible values are:

ppmquant    :== Use the ppmquant program to reduce colors.  The product
                of w3-color-max-[red|green|blue] is used as the maximum
                number of colors.
ppmdither   :== Use the ppmdither program to reduce colors.

any string :== Use this string as the filter.  No interpretation of it
               is done at all.  Example is:
               ppmquant -fs -map ~/pixmaps/colormap.ppm")

(defvar w3-ppmdither-is-buggy t
  "*The ppmdither which comes with pbmplus/netpbm releases through 
1mar1994 at least ignores the 'maxval' in its input.  This can cause
trouble viewing black-and-white gifs.  If this variable is set, a
(harmless) 'pnmdepth 255' step is inserted to work around this bug.
You can test your ppmdither by doing 
  ppmmake white 100 100 | pnmdepth 1 | ppmdither | pnmdepth 255 | ppmhist
If the output has a single line like this:
  255 255 255     255     10000
then it's safe to set this variable to nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; How to look up styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-style-tags-assoc nil
  "A version of w3-style-chars-assoc for the new display engine.")

(defvar w3-style-chars-assoc
  '(
    ("B"       . ("*" . "*"))
    ("ADDRESS" . ("*" . "*"))
    ("BYLINE"  . ("_" . "_"))
    ("CITE"    . ("_" . "_"))
    ("CMD"     . ("*" . "*"))
    ("DFN"     . ("*" . "*"))
    ("EM"      . ("~" . "~"))
    ("I"       . ("~" . "~"))
    ("Q"       . ("\"" . "\""))
    ("REMOVED" . ("" . ""))
    ("S"       . ("" . ""))
    ("STRONG"  . ("*" . "*"))
    ("SUB"     . ("" . ""))
    ("SUP"     . ("" . ""))
    ("U"       . ("_" . "_"))
    )
  "*An assoc list of emphasis tags and their corresponding
begin and end characters.")

(defvar w3-header-chars-assoc
  '(
    (h1 . (?* ?* w3-upcase-region))
    (h2 . (?* ?* w3-upcase-region))
    (h3 . (?- ?- w3-upcase-region))
    (h4 . (nil ?= nil))
    (h5 . (nil ?= nil))
    (h6 . (nil ?: nil)))
  "*An assoc list of header tags and a list of formatting instructions.
This list consists of 3 items - the first item is no longer used.  The
second item is the character to insert after the header.  A <BR> is
inserted before and after this string. And the third is a function to
call on the region between the start and end of the header.  This will
be called with 2 arguments, the buffer positions of the start and end
of the headers.")

(defvar w3-list-chars-assoc 
  '(
    (ul . ("o" "*" "+" ">"))
    (ol . ("." ")" "]" ":"))
    (dl . ("o" "*" "+" ">")))
  "An assoc list of characters to put at the front of list items.  It is
keyed on the type of list, followed by a list of items.  Each item should
be placed in the nth position of the list, where n is the nesting level it
should be used for.  n starts at 1.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Styles for emphasis, bold, etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((or w3-running-epoch w3-running-FSF19 w3-running-xemacs)
  (defvar w3-node-style 'w3-node-style "Face used for hypertext links.")
  (defvar w3-default-style 'w3-default-style "Default face.")
  (defvar w3-visited-node-style 'w3-visited-node-style "Visited links.")
  (defvar w3-active-node-style 'w3-active-node-style "Active links."))
 (t
  (defvar w3-node-style nil)
  (defvar w3-default-style nil)
  (defvar w3-active-node-style nil)
  (defvar w3-visited-node-style nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entities table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-html-entities
  '(
    ("&AElig;"      . "Æ") ; capital AE diphthong (ligature)
    ("&Aacute;"     . "Á") ; capital A, acute accent
    ("&Acirc;"      . "Â") ; capital A, circumflex accent
    ("&Agrave;"     . "À") ; capital A, grave accent
    ("&Aring;"      . "Å") ; capital A, ring
    ("&Atilde;"     . "Ã") ; capital A, tilde
    ("&Auml;"       . "Ä") ; capital A, dieresis or umlaut mark
    ("&Ccedil;"     . "Ç") ; capital C, cedilla
    ("&ETH;"        . "Ð") ; capital Eth, Icelandic
    ("&Eacute;"     . "É") ; capital E, acute accent
    ("&Ecirc;"      . "Ê") ; capital E, circumflex accent
    ("&Egrave;"     . "È") ; capital E, grave accent
    ("&Euml;"       . "Ë") ; capital E, dieresis or umlaut mark
    ("&Iacute;"     . "Í") ; capital I, acute accent
    ("&Icirc;"      . "Î") ; capital I, circumflex accent
    ("&Igrave;"     . "Ì") ; capital I, grave accent
    ("&Iuml;"       . "Ï") ; capital I, dieresis or umlaut mark
    ("&Ntilde;"     . "Ñ") ; capital N, tilde
    ("&Oacute;"     . "Ó") ; capital O, acute accent
    ("&Ocirc;"      . "Ô") ; capital O, circumflex accent
    ("&Ograve;"     . "Ò") ; capital O, grave accent
    ("&Oslash;"     . "Ø") ; capital O, slash
    ("&Otilde;"     . "Õ") ; capital O, tilde
    ("&Ouml;"       . "Ö") ; capital O, dieresis or umlaut mark
    ("&THORN;"      . "Þ") ; capital THORN, Icelandic
    ("&Uacute;"     . "Ú") ; capital U, acute accent
    ("&Ucirc;"      . "Û") ; capital U, circumflex accent
    ("&Ugrave;"     . "Ù") ; capital U, grave accent
    ("&Uuml;"       . "Ü") ; capital U, dieresis or umlaut mark
    ("&Yacute;"     . "Ý") ; capital Y, acute accent
    ("&acute;"      . "´") ; acute accent on its own
    ("&aacute;"     . "á") ; small a, acute accent
    ("&acirc;"      . "â") ; small a, circumflex accent
    ("&aelig;"      . "æ") ; small ae diphthong (ligature)
    ("&agrave;"     . "à") ; small a, grave accent
    ("&apos;"       . "'") ; apostrophe
    ("&aring;"      . "å") ; small a, ring
    ("&ast;"        . "*") ; asterisk
    ("&atilde;"     . "ã") ; small a, tilde
    ("&auml;"       . "ä") ; small a, dieresis or umlaut mark
    ("&breve;"      . "[breve ]")  ; breve
    ("&brvbar;"     . "¦") ; broken (vertical) bar
    ("&bsol;"       . "[bsol  ]")   ; /backslash =reverse solidus
    ("&caron;"      . "[caron ]")   ; caron
    ("&cedil;"      . "¸") ; cedilla on its own
    ("&ccedil;"     . "ç") ; small c, cedilla
    ("&cent;"       . "¢") ; cent sign
    ("&circ;"       . "[circ  ]")   ; circumflex accent
    ("&colon;"      . ":") ; colon
    ("&comma;"      . ",") ; comma
    ("&commat;"     . "@") ; commercial at
    ("&copy;"       . "©") ; copyright sign
    ("&curren;"     . "¤") ; general currency sign
    ("&darr;"       . "[darr  ]")   ; /downarrow A: =downward arrow
    ("&dblac;"      . "[dblac ]")   ; double acute accent
    ("&deg;"        . "°") ; degree sign
    ("&die;"        . "¨") ; dieresis
    ("&divide;"     . "÷") ; divide sign
    ("&dollar;"     . "$") ; dollar sign
    ("&dot;"        . "[dot   ]")   ; dot above
    ("&eacute;"     . "é") ; small e, acute accent
    ("&ecirc;"      . "ê") ; small e, circumflex accent
    ("&egrave;"     . "è") ; small e, grave accent
    ("&emsp;"       . "  ") ; em space
    ("&ensp;"       . " ") ; en space
    ("&equals;"     . "=") ; equals sign
    ("&eth;"        . "ð") ; small eth, Icelandic
    ("&euml;"       . "ë") ; small e, dieresis or umlaut mark
    ("&excl;"       . "!") ; exclamation mark
    ("&frac12;"     . "½") ; fraction - one-half
    ("&frac14;"     . "¼") ; fraction - one-quarter
    ("&frac34;"     . "¾") ; fraction - three-quarters
    ("&frac18;"     . "1/8") ; fraction - one-eigth
    ("&frac38;"     . "3/8") ; fraction - three-eighths
    ("&frac58;"     . "5/8") ; fraction - five-eighths
    ("&frac78;"     . "7/8") ; fraction - seven-eighths
    ("&frac13;"     . "1/3") ; fraction - one-third
    ("&frac23;"     . "2/3") ; fraction - two-thirds
    ("&frac15;"     . "1/5") ; fraction - one-fifth
    ("&frac25;"     . "2/5") ; fraction - two-fifths
    ("&frac35;"     . "3/5") ; fraction - three-fifths
    ("&frac45;"     . "4/5") ; fraction - four-fifths
    ("&frac16;"     . "1/6") ; fraction - one-sixth
    ("&frac56;"     . "5/6") ; fraction - five-sixths
    ("&grave;"      . "`") ; grave accent
    ("&gt;"         . ">") ; greater-than sign
    ("&gt"          . ">") ; greater-than sign
    ("&half;"       . "½") ; fraction - one-half
    ("&hellip;"     . ". . .") ; ellipsis (horizontal)
    ("&hibar;"      . "¯") ; high bar
    ("&horbar;"     . "-") ; horizontal bar
    ("&hyphen;"     . "-") ; hyphen
    ("&iacute;"     . "í") ; small i, acute accent
    ("&icirc;"      . "î") ; small i, circumflex accent
    ("&iexcl;"      . "¡") ; inverted exclamation mark
    ("&igrave;"     . "ì") ; small i, grave accent
    ("&iquest;"     . "¿") ; inverted question mark
    ("&iuml;"       . "ï") ; small i, dieresis or umlaut mark
    ("&larr;"       . "<--") ; left arrow
    ("&laquo;"      . "«") ; angle quotation mark, left
    ("&lcub;"       . "{") ; left curly bracket
    ("&ldquo;"      . "``") ; double quotation mark, left
    ("&lowbar;"     . "_") ; low bar
    ("&lpar;"       . "(") ; left parenthesis
    ("&lsqb;"       . "[") ; left square bracket
    ("&lsquo;"      . "`") ; single quotation mark, left
    ("&lt;"         . "<") ; less-than sign
    ("&lt"          . "<") ; less-than sign
    ("&macr;"       . "¯") ; macron
    ("&mdash;"      . "--") ; em-dash
    ("&micro;"      . "µ") ; micro sign
    ("&middot;"     . "·") ; middle dot
    ("&nbsp;"       . " ") ; non-breaking-space
    ("&ndash;"      . "-") ; en-dash
    ("&not;"        . "¬") ; logical not
    ("&ntilde;"     . "ñ") ; small n, tilde
    ("&num;"        . "#") ; number sign
    ("&oacute;"     . "ó") ; small o, acute accent
    ("&ocirc;"      . "ô") ; small o, circumflex accent
    ("&ogon;"       . "[ogon  ]")   ; ogonek
    ("&ograve;"     . "ò") ; small o, grave accent
    ("&ohm;"        . "[ohm   ]")   ; ohm sign
    ("&ordf;"       . "ª") ; ordinal indicator, feminine
    ("&ordm;"       . "º") ; ordinal indicator, masculine
    ("&oslash;"     . "ø") ; small o, slash
    ("&otilde;"     . "õ") ; small o, tilde
    ("&ouml;"       . "ö") ; small o, dieresis or umlaut mark
    ("&para;"       . "¶") ; pilcrow (paragraph sign)
    ("&percnt;"     . "%") ; percent sign
    ("&period;"     . ".") ; full stop, period
    ("&plus;"       . "+") ; plus sign
    ("&plusmn;"     . "±") ; plus-or-minus sign
    ("&pound;"      . "£") ; pound sign
    ("&quest;"      . "?") ; question mark
    ("&quot;"       . "\"") ; quotation mark
    ("&raquo;"      . "»") ; angle quotation mark, right
    ("&rarr;"       . "-->") ; Right arrow
    ("&rcub;"       . "}") ; right curly bracket
    ("&rdquo;"      . "''") ; double quotation mark, right
    ("&reg;"        . "®") ; registered sign
    ("&ring;"       . "°") ; ring
    ("&rpar;"       . ")") ; right parenthesis
    ("&rsqb;"       . "]") ; right square bracket
    ("&rsquo;"      . "'") ; single quotation mark, right
    ("&sect;"       . "§") ; section sign
    ("&semi;"       . ";") ; semicolon
    ("&shy;"        . "") ; soft hyphen
    ("&sol;"        . "[sol   ]")   ; solidus
    ("&sung;"       . "[sung  ]")   ; music note (sung text sign)
    ("&sup1;"       . "¹") ; superscript one
    ("&sup2;"       . "²") ; superscript two
    ("&sup3;"       . "³") ; superscript three
    ("&szlig;"      . "ß") ; small sharp s, German (sz ligature)
    ("&thorn;"      . "þ") ; small thorn, Icelandic
    ("&tilde;"      . "~") ; tilde
    ("&times;"      . "*") ; multiply sign
    ("&trade;"      . "(TM)") ; trade mark sign
    ("&uacute;"     . "ú") ; small u, acute accent
    ("&uarr;"       . "^") ; Up arrow
    ("&ucirc;"      . "û") ; small u, circumflex accent
    ("&ugrave;"     . "ù") ; small u, grave accent
    ("&uml;"        . "¨") ; umlaut mark
    ("&uuml;"       . "ü") ; small u, dieresis or umlaut mark
    ("&yacute;"     . "ý") ; small y, acute accent
    ("&yen;"        . "¥") ; yen sign
    ("&yuml;"       . "ÿ") ; small y, dieresis or umlaut mark
    ("&vellip;"     . "[vellip]")   ; vertical ellipsis
    ("&verbar;"     . "|") ; vertical bar
    ("&amp;"        . "&") ; ampersand
    )
  "*An assoc list of entity names and how to actually display them.")

(defvar w3-graphics-entities-alist
  '(
    ("&archive;"             . ("archive.xbm" . "[archive]"))
    ("&audio;"               . ("audio.xbm" . "[audio]"))
    ("&binary.document;"     . ("binary.document.xbm" . "[bin]"))
    ("&binhex.document;"     . ("binhex.document.xbm" . "[binhex]"))
    ("&caution;"             . ("caution.xbm" . "[CAUTION]"))
    ("&clock;"               . ("clock.xbm" . "[clock]"))
    ("&compressed.document;" . ("compressed.document.xbm" . "[comp]"))
    ("&disk.drive;"          . ("disk.drive.xbm" . "[diskdrive]"))
    ("&diskette;"            . ("diskette.xbm" . "[diskette]"))
    ("&display;"             . ("display.xbm" . "[display]"))
    ("&document;"            . ("unknown.document.xbm" . "[unk]"))
    ("&fax;"                 . ("fax.xbm" . "[fax]"))
    ("&filing.cabinet;"      . ("filing.cabinet.xbm" . "[filing cabinet]"))
    ("&film;"                . ("film.xbm" . "[film]"))
    ("&fixed.disk;"          . ("fixed.disk.xbm" . "[fixed disk]"))
    ("&folder;"              . ("folder.xbm" . "[folder]"))
    ("&form;"                . ("form.xbm" . "[form]")) ; draw one
    ("&ftp;"                 . ("ftp.xbm" . "[ftp]"))
    ("&glossary;"            . ("glossary.xbm" . "[glossary]"))
    ("&home;"                . ("home.xbm" . "[HOME]"))
    ("&image;"               . ("image.xbm" . "[image]"))
    ("&index;"               . ("index.xbm" . "[index]"))
    ("&keyboard;"            . ("keyboard.xbm" . "[keyboard]"))
    ("&mail;"                . ("mail.xbm" . "[mail]"))
    ("&mail.in;"             . ("mail.in.xbm" . "[mail - in]"))
    ("&mail.out;"            . ("mail.out.xbm" . "[mail - out]"))
    ("&map;"                 . ("map.xbm" . "[map]"))
    ("&mouse;"               . ("mouse.xbm" . "[mouse]"))
    ("&network;"             . ("network.xbm" . "[network]"))
    ("&next;"                . ("next.xbm" . "[next]"))
    ("&notebook;"            . ("notebook.xbm" . "[notebook]"))
    ("&parent;"              . ("parent.xbm" . "[parent]")) ; draw one
    ("&previous;"            . ("previous.xbm" . "[previous]")) ; draw one
    ("&printer;"             . ("printer.xbm" . "[printer]"))
    ("&scheduler;"           . ("scheduler.xbm" . "[scheduler]"))
    ("&stop;"                . ("stop.xbm" . "[STOP]"))
    ("&summary;"             . ("summary.xbm" . "[summary]")) ; draw one
    ("&symlink;"             . ("symlink.xbm" . "[symlink]"))
    ("&telephone;"           . ("telephone.xbm" . "[telephone]"))
    ("&telnet;"              . ("telnet.xbm" . "[telnet]"))
    ("&text.document;"       . ("unknown.document.xbm" . "[text]"))
    ("&tn3270;"              . ("tn3270.xbm" . "[tn3270]"))
    ("&toc;"                 . ("toc.xbm" . "[Table Of Contents]")); Draw one!
    ("&trash;"               . ("trash.xbm" . "[trash]"))
    ("&unknown.document;"    . ("unknown.document.xbm" . "[unknown]"))
    ("&uuencoded.document;"  . ("uuencoded.document.xbm" . "[uuencoded]"))
    ("&workstation;"         . ("workstation.xbm" . "[workstation]"))
    )
    "*An assoc list of entity nams and the names of bitmaps to
display them. The car is the entity name, the cdr is a list of the
form (bitmapfile . alttag), where bitmapfile is a filename that
specifies a bitmap file - w3-icon-directory is prepended to this
automatically.  Alttag is the text to use if the bitmap is
unavailable.  If nil, no text is used.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-navigate-menu nil)
(defvar w3-popup-menu
  '("WWW Browser"
    ["Open Local File" w3-open-local t]
    ["Open URL" w3-fetch t]
    ["Copy URL to clipboard" w3-save-url t]
    "---"
    "---"
    ["Load Delayed Images" w3-load-delayed-images w3-delayed-images]
    ["Load Delayed MPEGs" w3-load-delayed-mpegs w3-delayed-movies]
    "---"
    ["Add annotation" w3-annotation-add t]
    "---"
    ["Leave & Bury Buffer" w3-leave-buffer t]
    ["Leave & Kill Buffer" w3-quit t]
    )
  "The shorter popup menu.")

(defvar w3-documentation-root "http://www.cs.indiana.edu/elisp/w3/"
  "*Where the w3 documentation lives.  This MUST end in a slash.")

(defvar w3-graphlink-menu
  '(("View headers"  . url-popup-info)
    ("Dump to disk"  . w3-download-url)
    ("Copy URL"      . w3-save-url)
    ("Get image"     . w3-fetch))
  "An assoc list of function names and labels.  These will be displayed
in a popup menu when the mouse is pressed on a hyperlink.  Format is
( (label . function)), function is called with one argument, the URL of
the link.")  

(defvar w3-hyperlink-menu
  '(("View headers"  . w3-popup-info)
    ("Dump to disk"  . w3-download-url)
    ("Mail document" . w3-mail-current-document)
    ("Copy URL"      . w3-save-url)
    ("View source"   . w3-source-document)
    ("Print"         . w3-print-this-url)
    )
  "An assoc list of function names and labels.  These will be displayed
in a popup menu when the mouse is pressed on a hyperlink.  Format is
( (label . function)), function is called with one argument, the URL of
the link.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables internal to W3, you should not change any of these
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-faces
   '(
;     W3-face        Fall-back default face
     (abbrev     .   default)
     (abstract   .   default)
     (acronym    .   default)
     (added      .   italic)
     (address    .   italic)
     (arg        .   italic)
     (b          .   bold)
     (big        .   bold)
     (blink      .   bold)
     (blink      .   italic)
     (byline     .   italic)
     (cite       .   italic)
     (cmd        .   bold)
     (code       .   default)
     (dfn        .   italic)
     (em         .   italic)
     (font0      .   default)
     (font1      .   default)
     (font2      .   default)
     (font3      .   default)
     (font4      .   default)
     (font5      .   default)
     (font6      .   default)
     (font7      .   default)
     (h1         .   bold)
     (h2         .   bold)
     (h3         .   italic)
     (h4         .   italic)
     (h5         .   underline)
     (h6         .   underline)
     (i          .   italic)
     (kbd        .   default)
     (person     .   bold)
     (q          .   italic)
     (removed    .   italic)
     (rot13      .   rot13)
     (s          .   highlight)
     (samp       .   default)
     (small      .   italic)
     (strong     .   bold)
     (sub        .   bold)
     (sup        .   italic)
     (tt         .   default)
     (u          .   underline)
     (var        .   italic)
     (wired      .   italic)
     )

   "*An assoc list of faces.  The cdr of each list item is the default
face to copy if no resources are set to define the face.
(`face-differs-from-default-p' is used to determine whether to copy
the default face or not).")

(defvar w3-all-faces
  (cons (cons 'html 'w3-default-style)
	(mapcar
	 (function
	  (lambda (x)
	    (cons (car x) (intern
			   (concat "w3-" (symbol-name (car x)) "-style")))))
	 w3-faces))
  "A list of all the faces for use by emacs-w3.")

(defvar w3-invisible-href-list nil
  "A list of 'invisible' graphic links in the current buffer.")

(defconst w3-state-locator-variable
  '(
    align
    background
    center
    depth
    figalt
    figdata
    fillcol
    form
    formnum
    header-start
    href
    image
    lists
    mpeg
    name
    needspace
    next-break
    nofill
    nowrap
    optarg
    options
    pre-start
    select
    secret
    table
    text-mangler
    title
    w3-graphic
    zone
    label-text
    seen-this-url
    )
  "A list of all the various state kept in the drawing engine.
This is used by the `w3-get-state' and `w3-put-state' macros.")

(defvar w3-state-vector
  (make-vector (1+ (length w3-state-locator-variable)) nil)
  "Various state shit kept by emacs-w3.")

(defvar w3-user-stylesheet nil
  "The global stylesheet for this user.")

(defvar w3-current-stylesheet nil
  "The stylesheet for this document.")

(defvar w3-base-alist nil
  "An assoc list of named BASE tags in the current document.")

(defvar w3-blinking-buffs nil
  "A list of buffers with blinking text in them.
This is used to optimize when we change a face so the entire display
doesn't flash every second, whether we've run into a buffer that is
displaying blinking text or not.")

(defvar w3-last-fill-pos nil
  "An internal variable for the new display engine that specifies the
last character position that was correctly filled.")

(defvar w3-last-tag nil
  "An internal variable for the new display engine that specifies the
last tag processed.")

(defvar w3-table-info nil
  "An internal variable for the new display engine for keeping table data
during the pre-pass parsing.")

(defvar w3-current-formatter nil
  "Current formatter function.")

(defvar w3-draw-buffer nil
  "Where we are currently drawing into.  This _must_ be a buffer object
when it is referenced.")

(defvar w3-active-faces nil "The list of active faces.")

(defvar w3-netscape-variable-mappings
  '(("PRINT_COLOR"	. ps-print-color-p)
    ("DITHER_IMAGES"	. w3-color-use-reducing)
    ("SOCKS_HOST"	. url-socks-host)
    ("ORGANIZATION"	. url-user-organization)
    ("EMAIL_ADDRESS"	. url-personal-mail-address)
    ("REAL_NAME"	. url-user-real-name)
    ("NEWSGROUP_DESCRIPTIONS" . url-show-newsgroup-descriptions)
    ("NNTPSERVER"	. url-news-server)
    ("AUTOLOAD_IMAGES"	. w3-delay-image-loads)
    ("HOME_DOCUMENT"	. w3-default-homepage)
    ("UNDERLINE_LINKS"	. w3-underline-links)
    ("TMPDIR"		. url-temporary-directory))
  "A mapping from netscape configuration file options to w3 variables.")
     
(defvar w3-acceptable-protocols-alist
  '(("Gopher"                           . "gopher")
    ("TN3270 (IBM Mainframe emulation)" . "tn3270")
    ("Interactive Telnet Session"       . "telnet")
    ("Local file or file over ftp"      . "file")
    ("File on an http server"           . "http")
    ("Usenet newsgroup/article"         . "news")
    ("Mail session"                     . "mailto"))
  "An assoc list of descriptive labels and the corresponding URL stub.")
(defvar w3-annotation-marker "<ncsa-annotation-format-1>")
(defvar w3-annotation-minor-mode nil "Whether we are in the minor mode.")
(defvar w3-annotation-minor-mode-map (make-keymap) "Keymap for annotation.")
(defconst w3-bug-address "wmperry@aventail.com" "Address of current maintainer.")
(defvar w3-continuation '(url-uncompress url-clean-text)
  "List of functions to call to process a document completely.")
(defvar w3-current-annotation nil "URL of document we are annotating...")
(defvar w3-current-isindex nil "Is the current document a searchable index?")
(defvar w3-current-last-buffer nil "Last W3 buffer seen before this one.")
(defvar w3-current-links nil "An assoc list of <LINK> tags for this doc.")
(defvar w3-current-source nil "Source of current document.")
(defvar w3-current-parse nil "Parsed version of current document.")
(defconst w3-default-continuation '(url-uncompress url-clean-text) 
  "Default action to start with - cleans text and uncompresses if necessary.")
(defvar w3-editing-annotation nil "Are we editing an annotation or not?")
(defvar w3-find-this-link nil "Link to go to within a document.")
(defvar w3-hidden-forms nil "List of hidden form areas and their info.")
(defvar w3-hotlist nil "Default hotlist.")
(defvar w3-icon-path-cache nil "Cache of where we found icons for entities.")
(defvar w3-last-buffer nil "The last W3 buffer visited.")
(defvar w3-mode-map (make-keymap) "*Keymap to use in w3-mode.")
(defvar w3-old-kill-emacs-hook nil "Old value of kill-emacs-hook, if any.")
(defvar w3-personal-annotations nil "Assoc list of personal annotations.")
(defvar w3-print-next nil "Should we latex & print the next doc?")
(defvar w3-roman-characters "ivxLCDMVX" "Roman numerals.")
(defvar w3-setup-done nil "Have we been through setup code yet?")
(defvar w3-source nil "Should we source the next document or not?")
(defvar w3-strict-width nil
  "*This variable will control how wide emacs thinks the current window is.
This is useful when working in batch mode, and (window-width) returns the
wrong value.  If the value is nil, it will use the value (window-width)
returns.")
(defvar w3-submit-button nil
  "A zone object specifying what button was pressed to submit a form.")
(defvar w3-zones-list nil "*List of 'zones' in a dumb emacs buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer-local variables to keep around when going into w3-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-e19-hotlist-menu nil
  "A menu for hotlists.")

(defvar w3-e19-links-menu nil
  "A buffer-local menu for links.")

(defvar w3-persistent-variables
  '(
    url-current-callback-func
    url-current-content-length
    url-current-file
    url-current-mime-encoding
    url-current-mime-headers
    url-current-mime-type
    url-current-mime-viewer
    url-current-port
    url-current-referer
    url-current-server
    url-current-type
    url-current-user
    w3-e19-links-menu
    w3-current-parse
    w3-current-annotation
    w3-current-isindex
    w3-current-last-buffer
    w3-current-links
    w3-current-source
    w3-delayed-images
    w3-delayed-movies
    w3-hidden-forms
    w3-invisible-href-list
    w3-node-style
    w3-active-node-style
    w3-state-vector
    w3-visited-node-style
    w3-form-labels
    w3-zones-list
    )
  "A list of variables that should be preserved when entering w3-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-parse-args-syntax-table
  (copy-syntax-table emacs-lisp-mode-syntax-table)
  "A syntax table for parsing sgml attributes.")

(modify-syntax-entry ?' "\"" w3-parse-args-syntax-table)
(modify-syntax-entry ?` "\"" w3-parse-args-syntax-table)
(modify-syntax-entry ?< "(>" w3-parse-args-syntax-table)
(modify-syntax-entry ?> ")<" w3-parse-args-syntax-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-form-labels nil "")
(mapcar 'make-variable-buffer-local w3-persistent-variables)
(make-variable-buffer-local 'w3-state-vector)
(make-variable-buffer-local 'w3-current-stylesheet)
(make-variable-buffer-local 'w3-base-alist)
(make-variable-buffer-local 'w3-annotation-minor-mode)
(make-variable-buffer-local 'w3-last-tag)
(make-variable-buffer-local 'w3-last-fill-pos)
(make-variable-buffer-local 'w3-table-info)
(make-variable-buffer-local 'w3-draw-buffer)
(make-variable-buffer-local 'w3-current-formatter)
(make-variable-buffer-local 'w3-active-faces)
(make-variable-buffer-local 'w3-default-style)
 
(suppress-keymap w3-mode-map)
(define-key w3-mode-map " "	   'w3-scroll-up)
(define-key w3-mode-map "<"        'beginning-of-buffer)
(define-key w3-mode-map ">"        'end-of-buffer)
(define-key w3-mode-map "?"        'w3-help)
(define-key w3-mode-map "A"        'w3-hotlist-add-document-at-point)
(define-key w3-mode-map "B"        'w3-backward-in-history)
(define-key w3-mode-map "F"        'w3-forward-in-history)
(define-key w3-mode-map "G"        'w3-show-graphics)
(define-key w3-mode-map "H"	   'w3-show-hotlist)
(define-key w3-mode-map "I"        'w3-popup-info)
(define-key w3-mode-map "K"        'w3-save-this-url)
(define-key w3-mode-map "P"        'w3-print-url-under-point)
(define-key w3-mode-map "Q"        'w3-leave-buffer)
(define-key w3-mode-map "R"        'w3-refresh-buffer)
(define-key w3-mode-map "S"        'w3-source-document-at-point)
(define-key w3-mode-map "U"        'w3-use-links)
(define-key w3-mode-map "V"        'w3-view-this-url)
(define-key w3-mode-map "\C-?"     'scroll-down)
(define-key w3-mode-map "\C-c\C-b" 'w3-show-history-list)
(define-key w3-mode-map "\C-c\C-v" 'w3-version)
(define-key w3-mode-map "\C-o"     'w3-fetch)
(define-key w3-mode-map "\M-M"     'w3-mail-document-under-point)
(define-key w3-mode-map "\M-\C-i"  'w3-insert-this-url)
(define-key w3-mode-map "\M-m"	   'w3-mail-current-document)
(define-key w3-mode-map "\M-s"	   'w3-search)
(define-key w3-mode-map "\M-\r"    'w3-follow-inlined-image)
(define-key w3-mode-map "\r"       'w3-follow-link)
(define-key w3-mode-map "\t"       'w3-forward-link)
(define-key w3-mode-map "a"	   'w3-hotlist-add-document)
(define-key w3-mode-map "b"	   'w3-back-link)
(define-key w3-mode-map "c"        'w3-mail-document-author)
(define-key w3-mode-map "d"        'w3-hotlist-delete)
(define-key w3-mode-map "f"	   'w3-forward-link)
(define-key w3-mode-map "g"        'w3-reload-document)
(define-key w3-mode-map "h"        'w3-use-hotlist)
(define-key w3-mode-map "i"        'w3-document-information)
(define-key w3-mode-map "k"        'w3-save-url)
(define-key w3-mode-map "l"        'w3-goto-last-buffer)
(define-key w3-mode-map "m"        'w3-complete-link)
(define-key w3-mode-map "n"        'w3-forward-link)
(define-key w3-mode-map "o"	   'w3-open-local)
(define-key w3-mode-map "p"        'w3-print-this-url)
(define-key w3-mode-map "q"	   'w3-quit)
(define-key w3-mode-map "r"        'w3-reload-document)
(define-key w3-mode-map "s"        'w3-source-document)
(define-key w3-mode-map "u"        'w3-leave-buffer)
(define-key w3-mode-map "v"	   'url-view-url)
(define-key w3-mode-map "w"        'w3-submit-bug)

(cond
 (w3-running-FSF19
  (define-key w3-mode-map [S-tab]    'w3-back-link))
 (w3-running-xemacs
  (define-key w3-mode-map '(shift tab) 'w3-back-link))
 (t nil))

(define-key w3-annotation-minor-mode-map "\C-c\C-c"
  'w3-personal-annotation-finish)

(provide 'w3-vars)
