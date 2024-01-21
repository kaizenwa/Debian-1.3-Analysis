# $Header$
###########################################################################
# 
# 
#    TkMail -- A Tk/Tcl interface to Mail
# 	    		by Paul Raines (raines@slac.stanford.edu)
# 
###########################################################################
#  -*- Mode: tcl-mode -*- 
global mf mfp

# TYPES OF USER SETTINGS
#	N  Number
#	S  Short string (edit in entry widget)
#	L  Long string (edit in text widget)
#	K  Key sequence for bind command
#	B  Boolean
#	T  Tcl command
#	F  File which must exist
#	f  File with does not have to exist
#	D  Directory which must exist
#	U  Unix command
#	C  Text tag configure line
#	k  Code string or keyword (will be trimmed and lowercased)
#       a  Font configurable specification
#	1  A list where each item is a single entity
#	2  A list where each item is a pair of entities
#       c  A choice of short string options
# default user settings dynamically configurable
proc mfv:default-set {} {
  global mf mfp mfd env mfch

  # MAIL SETTINGS
  set mfd(mail-deliver) {U The UNIX command to deliver mail (reads standard input for address and text) }
  set mf(mail-deliver) "[mfv:find-sendmail] -bm -odb -oi -t"
  set mfd(mail-system) {f Pathname of file your mail is spooled to by your mail handler }
  set mf(mail-system) [mfv_set mailspool]/$mfp(user)
  set mfd(mail-mbox) {f Pathname of folder you want opened by default at startup. Most often this is the same as your mail spool file.  }
  set mf(mail-mbox) $mf(mail-system)
  set mfd(mail-directory) {D Pathname to directory contain your main cache of folders. Used for creating menus and for root of <Sender> files. }
  set mf(mail-directory) $mfp(homedir)/Mail
  set mfd(mail-tmpdir) {D Pathname of directory for temporary files }
  set mf(mail-tmpdir) /usr/tmp
  set mfd(mail-interval) {N Number of milliseconds between new mail and append checks }
  set mf(mail-interval) 10000
  set mfd(mail-autosave) {N Number of milliseconds between autosaves to \#folder\# file. Use number < 1 to disable. }
  set mf(mail-autosave) 600000
  set mfd(mail-auto-incorp) {B When using mbox model, whether to incorporate mail automatically when detected }
  set mf(mail-auto-incorp) 0
  set mfd(mail-debug) {B print usually ignored error messages to stderr}
  set mf(mail-debug) 0
  set mfd(mail-read-ask) {B Whether to ask to continue when reading in large messages }
  set mf(mail-read-ask) 0
  set mfd(mail-read-max) {N Maximum number of lines before asking to how much to fetch }
  set mf(mail-read-max) 1000
  set mfd(mail-alias-file) {f Name of file to read aliases from. Do a "Reread Alias File" after changing. Also set mail-alias-type correctly. }
  set mf(mail-alias-file) $mfp(homedir)/.mailrc
  set mfd(mail-alias-type) {c Type of alias file format - "bsd" or "elm". Do a "Reread Alias File" after changing. Also set mail-alias-file correctly. }
  set mfch(mail-alias-type) {bsd elm}
  set mf(mail-alias-type) {bsd}
  set mfd(mail-alias-case) {B Whether aliases are case sensitive}
  set mf(mail-alias-case) 1
  set mfd(mail-remove-empty) {B Whether to delete zero-length folders when closing them}
  set mf(mail-remove-empty) 1
  set mfd(mail-archive-folder) {f Name of default folder for archiving messages}
  set mf(mail-archive-folder) {@mfv:sender-default-hook 1}

  # VIEWER SETTTINGS
  set mfd(viewer-print) {U Printing command where %F is a placeholder for file to print, %D the mesg date, %S the subject, %W the From: field }
  set mf(viewer-print) "lpr %F"
  set mfd(viewer-bitmap-nomail) {F Bitmap to display when there is no new mail }
  set mf(viewer-bitmap-nomail) "/usr/include/X11/bitmaps/flagdown"
  set mfd(viewer-bitmap-mail) {F Bitmap to display when there is new mail }
  set mf(viewer-bitmap-mail) "/usr/include/X11/bitmaps/flagup"
  set mfd(viewer-beep-new) {T Tcl command to eval for new mail (in case you have a better one like blt_bell) }
  set mf(viewer-beep-new) {bell}
  set mfd(viewer-beep-empty) {T Tcl command to eval for emtpy mailbox }
  set mf(viewer-beep-empty) {}
  set mfd(viewer-beep-error) {T Tcl command to eval for error notifications }
  set mf(viewer-beep-error) {bell}
  set mfd(viewer-state) {c Set to either 'normal' or 'disabled' to allow message window editing }
  set mfch(viewer-state) {normal disabled}
  set mf(viewer-state) disabled
  set mfd(viewer-pipe-dir) {D Pathname of directory to run piped UNIX commands in }
  set mf(viewer-pipe-dir) $mfp(homedir)
  set mfd(viewer-geom) {k Default geometry for mail viewers }
  set mf(viewer-geom) {}
  set mfd(viewer-start-locked) {B Whether new viewers should be created locked. }
  set mf(viewer-start-locked) 0 

  # HEADER LISTBOX
  set mfd(headlist-sort) {k Field name to sort summary list. Use 'normal' for no sort. Examples: sm-from, fullname, subject}
  set mf(headlist-sort) normal
  set mfd(headlist-reverse) {B Whether sorting should be done in reverse order }
  set mf(headlist-reverse) 0 
  set mfd(headlist-reverse-moveup) {B Whether current mesg should move up after a delete }
  set mf(headlist-reverse-moveup) 1
  set mfd(headlist-height) {N Number messages lines displayed in header listbox }
  set mf(headlist-height) 8
  set mfd(headlist-format) {S format of summary line in header listbox }
  set mf(headlist-format) {%-20.20F  %3m %2d %-5.5h %4l  %-45.45s}
  set mfd(headlist-deleted-hide) {B Whether deleted messages should be hidden from list }
  set mf(headlist-deleted-hide) 0 
  set mfd(headlist-deleted-config) {C Text properties to configure deleted message summary lines with. }
  set mf(headlist-deleted-config) "-foreground red"

  # ISPELL SETTINGS
  set mfd(ispell-present) {B Whether your system has ispell }
  set mf(ispell-present) 1
  set mfd(ispell-binary) {f Name of ispell binary. Give full path if needed. Blank uses installed default}
  set mf(ispell-binary) {}
  set mfd(ispell-main-dictionary) {f Filename of main dictionary to use. Use "default" for default. }
  set mf(ispell-main-dictionary) {default}
  set mfd(ispell-personal-dictionary) {f Filename of personal dictionary to use. Use "default" for default. }
  set mf(ispell-personal-dictionary) {default}
  set mfd(ispell-addopts) {S Addition options to pass to ispell process.}
  set mf(ispell-addopts) {}

  # COMPOSE WINDOW SETTINGS
  set mfd(compose-icon-bitmap) {F Bitmap to display as icon for compose windows }
  set mf(compose-icon-bitmap) "/usr/include/X11/bitmaps/letters"
  set mfd(compose-geom) {k Default geometry of compose window }
  set mf(compose-geom) {}
  set mfd(compose-show-cc) {B Whether to show the Cc and Bcc fields in compose even if a simple Reply or they are empty}
  set mf(compose-show-cc) 1
  set mfd(compose-save-send) {B Whether to store a copy of the last sent message for possible restore }
  set mf(compose-save-send) 1
  set mfd(compose-alt-editor) {U Alternate editor command. If it is not an X windows editor, you must use xterm (i.e. xterm  -e vi %F). %F is file name placeholder }
  set mf(compose-alt-editor) "emacs %F"
  set mfd(compose-alt-auto) {B Whether to startup alternate editor automatically }
  set mf(compose-alt-auto) 0
  set mfd(compose-alternates) {1 Alternate email addresses to strip from Cc and Bcc }
  set mf(compose-alternates) ""
  set mfd(compose-addr-postfix) {S Possible postfix to add to addresses that don't include a @machine part. You must include the '@' character in the string }
  set mf(compose-addr-postfix) ""
  set mfd(compose-require-subject) {B Whether a subject should be required or not on outgoing messages }
  set mf(compose-require-subject) 1
  set mfd(compose-from-field) {S Text to include on From: line. Your sendmail must be configured to allow this. }
  set mf(compose-from-field) {}
  set mfd(compose-fcc-folder) {f Pathname to file to record outgoing messages in with FCC }
  set mf(compose-fcc-folder) {}
  set mfd(compose-fcc-swap) {B Whether to record outgoing messages in FCC file as originating from the address you sent it to so that you see this address in the header list }
  set mf(compose-fcc-swap) 0
  set mfd(compose-fcc-forward) {B Whether forwarded mail should be recorded in FCC file }
  set mf(compose-fcc-forward) {1}

  # INSERTION OF MESSAGE SETTINGS
  set mfd(insert-prefix) {S String used to prefix included messages and files }
  set mf(insert-prefix) ">> "
  set mfd(insert-cite-format) {S Format for the cite string at top of included messages, i.e "%F says:" }
  set mf(insert-cite-format) {}
  set mfd(insert-forward-format) {S Format of line to place at top of forwarded messages }
  set mf(insert-forward-format) "---------- Forwarded message from %f on %D -----------"
  set mfd(insert-headers) {L Text to automatically put at top of every composition }
  set mf(insert-headers) {}
  set mfd(insert-signature) {f Name of .signature file to put at end of messages }
  set mf(insert-signature) $mfp(homedir)/.signature
  set mfd(insert-auto-sign) {B Whether to automatically append signature file when compose window is created}
  set mf(insert-auto-sign) 1
  set mfd(insert-always-sign) {B Whether to always append signature file before a message is sent if it hasn't been appended already and exists.}
  set mf(insert-always-sign) 1
  set mfd(insert-prefix-sig) {L Text to put before the signature file. Note that a final linefeed is important. }
  set mf(insert-prefix-sig) "--\n"
  set mfd(insert-encoder) {U Program to encode inserted files }
  set mf(insert-encoder) uuencode
  set mfd(insert-strip) {B Whether to automatically strip header of included messages }
  set mf(insert-strip) 1
  set mfd(insert-compress) {U Program to use to compress inserted files }
  set mf(insert-compress) compress
  set mfd(insert-compress-suffix) {S Suffix the compress program appends to compressed files }
  set mf(insert-compress-suffix) Z

  # MAIL HEADER SETTINGS
  set mfd(header-retain) {1 Header fields to retain in display. Overrides Headers to Strip }
  set mf(header-retain) {}
  set mfd(header-strip) {1 Header fields to strip out of viewed messages. }
  set mf(header-strip) {Received Status Message-Id}
  set mfd(header-config) {C Text properties to configure headers with in viewer display. }
  set mf(header-config) "-underline 1"

  # MENU SETTINGS
  set mfd(menu-folders-max) {N Maximum number of folders listed in menus. Do a "Rebuild Folder Menus" after changing. }
  set mf(menu-folders-max) 25
  set mfd(menu-depth-max) {N Maximum depth of pull right menus for folders. Do a "Rebuild Folder Menus" after changing. }
  set mf(menu-depth-max) 5
  set mfd(menu-folders-ignore) {1 Filenames in mf(mail-directory) directory to not put in menus. Shell glob sytax accepted.  Do a "Rebuild Folder Menus" after changing.}
  set mf(menu-folders-ignore) {}
  set mfd(menu-quick-send) {1 List of common addresses for composing to put in menu. Set to @aliases to use your alias list. }
  set mf(menu-quick-send) {}
  set mfd(menu-recent-max) {N Maximum number of folders to put in Recent menus }
  set mf(menu-recent-max) 8
  set mfd(menu-recent-exclusive) {B Whether folders in mf(mail-directory) should be excluded from Recent menus }
  set mf(menu-recent-exclusive) 0

  # BIND
  set mfd(bind-alt-key) {K Key to press in order to access menu accelarators when Alt can't be used. TkMail must be restarted. }
  set mf(bind-alt-key) <Control-c>

  # MISC
  set mfd(disp-left-scroll) {B Whether to place scrollbars on left side of scrollable windows. Will only affect new windows till restart. }
  set mf(disp-left-scroll) 1
  set mfd(disp-horiz-scroll) {B Whether to display horizontal scrollbars. }
  set mf(disp-horiz-scroll) 1
  set mfd(notify-popup) {B Whether to popup a window listing new messages when they arrive }
  set mf(notify-popup) 0
  set mfd(notify-format) {S Format of summary line in notify popup listbox }
  set mf(notify-format) {%-16.16F  %4l  %-45.45s}
  set mfd(notify-geom) {k Default geometry of notify popup window}
  set mf(notify-geom) {}
  set mfd(disp-font-default) {a X11 font use in Text widgets not showing MIME}
  set mf(disp-font-default) {-*-helvetica-medium-r-normal-*-12-*}
  set mfd(disp-font-fixed) {a X11 font to use for showing non-MIME fixed text in Text widgets}
  set mf(disp-font-fixed) {fixed}
  set mfd(disp-default-fixed) {B Whether to use fixed font by default in viewer and compose}
  set mf(disp-default-fixed) 0
  set mfd(option-editor-geom) {k Default geometry of option editor window }
  set mf(option-editor-geom) {}

  # MIME
  set mfd(mime-parse) {B Whether to do MIME parsing }
  set mf(mime-parse) 0
  set mfd(mime-compose) {B Whether message inclusion in compose should be done as MIME rfc822 attachments }
  set mf(mime-compose) 0
  set mfd(mime-external-default) {S Default command to run on mime parts not handled internally}
  set mf(mime-external-default) {metamail -b -d -q -x -f %A -s %S -m tkmail -c %T}
  set mfd(mime-external-viewers) {2 List of content-type / program pairs}
  set mf(mime-external-viewers) { \
    {Audio {cat %F > /dev/audio}} \
    {Application/PostScript ghostview} \
    {Application @prompt} }
  set mfd(mime-font-default) {a Font as {foundry family fntsize} for deriving fonts for mime}
  set mf(mime-font-default) {adobe helvetica 12}
  set mfd(mime-font-fixed) {a Font as {foundry family fntsize} for deriving fixed fonts for mime}
  set mf(mime-font-fixed) {adobe courier 12}
}

proc mfv:find-sendmail {} {
  foreach dir {/usr/lib /usr/sbin /usr/ucblib} {
    if [file executable $dir/sendmail] { return $dir/sendmail }
  }
  return /usr/lib/sendmail
}

###############################################################
# DO NOT EDIT BELOW THIS LINE

# Private program variables

set mfp(user) [mfv_util user]
set mfp(homedir) [mfv_util fullpath [mfv_util home]]
set mfp(hostname) [mfv_util host]
if $mfp(debug) { 
  puts stderr "Starting source of viewer.tcl"
  flush stderr
}

# the current toplevel widget (any type)
   set mfp(curtop) .mf0
# the current viewer widget
   set mfp(curview) .mf0
# temp text processing widget
   set mfp(tmptxt) .tmptxt
# list of toplevel viewers
   set mfp(viewlist) {}
# header list holding & displaying widget
   set mfp(head) head.list
# message holding & displaying widget
   set mfp(mesg) mesg.txt
# header status label
   set mfp(hstat) stat.folder
# message status label
   set mfp(mstat) stat.mesg
# save of text of last sent message
   set mfp(savesendtxt) .savesend
   set mfp(savesendto) ""
   set mfp(savesendsubj) ""
   set mfp(savesendcc) ""
   set mfp(savesendbcc) ""
   set mfp(savesendsign) ""
# settings for file insertion
   set mfp(ins_compress) 0
   set mfp(ins_prefix) 0		
   set mfp(ins_encode) 0
# list of folders in Recent menus
   set mfp(recentlist) ""
# index of last permanent item in Folder menu
   set mfp(fmenulast) 0
   set mfp(fmenusender) -1
# index of last permanent item in Mail menu
   set mfp(mmenulast) 0
# list of widgets to put watch in for waiton and waitoff
   set mfp(waitlist) {}
# whether mailbox is empty
   set mfp(nomail) 1
# number messages auto-incorped with no mbox opened
   set mfp(auto-incorped) 0
# prefix to Escape for cancels (need by emacs users)
   set mfp(cancel) ""
# additional alternate address to remove beyond user setting
   set mfp(add-alt) {}
# whether to skip print prompting
  set mfp(print-noprompt) 0
# whether to print messages on separated pages
  set mfp(print-separate) 0
# keeps track of last read message in each open folder
  set mfp(last-mesg) {}
# need this one mfopt variable here
  set mfopt(modified) 0
# inform procedures they should not prompt user
  set mfp(noask) 0
# inform we are in a trap exit
  set mfp(trap-exit) 0
# mbox model
  set mfp(mbox-model) 0

# MIME
# list of attachable types
set mfp(mime-attach-types) {text application image audio video}
set mfp(mime-subtypes,text) {plain enriched}
set mfp(mime-subtypes,application) {octet-stream postscript}
set mfp(mime-subtypes,image) {gif jpeg}
set mfp(mime-subtypes,audio) {basic}
set mfp(mime-subtypes,video) {mpeg}

# have a blank bitmap for spacers
image create bitmap blank -data {
#define blank_width 15
#define blank_height 15
static char blank_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
}

### CONVENIENCE ROUTINES FOR LATER REMOVAL TO LIBRARY ######

if {[catch "infox version"]} {
  proc lempty { l } { return [expr ![string length $l]] }
  proc keylget { lvar key {rvar 0} } {
    upvar $lvar klist
    set check 0
    if {$rvar != 0} {
      set check 1
      if {[string length $rvar]} {upvar $rvar ret}
    }
    foreach pair $klist {
      if {[lindex $pair 0] == $key} {
	if [catch {lindex $pair 1} ret] {
	  set ret [string trim [string range $pair [string length $key] end]]
	  if {[string index $ret 0] == "\{"} {
	    set ret [string trim $ret "{}"]
	  }
	}
	if {$check} { return 1 } else {return $ret}
      }
    }
    if {$check} { 
      return 0 
    } else { error "No key named $key in $lvar" }
  }
  proc keylset { lvar key val } {
    upvar $lvar klist
    set ndx 0
    if {[info exists klist]} {
      foreach pair $klist {
	if {[lindex $pair 0] == $key} {
	  set klist [lreplace $klist $ndx $ndx "$key {$val}"]
	  return {}
	}
	incr ndx
      }
    }
    lappend klist "$key {$val}"
    return {}
  }
}

proc llast {list} {
  return [expr [llength $list]-1]
}

proc quotespecial { str } {
  regsub -all {\"} $str {\"} str
  return $str
}

proc unquotespecial { str } {
  regsub -all {\\\"} $str {"} str
  return $str
}

# get temp file
proc tmpfile { {pfx tkmail} {dir ""} } {
  return [mfv_util tmpfile $pfx $dir]
}

# return current selection
proc selection_if_any {} {
  if {[catch {selection get} s]} {return ""} {return $s}
}

# returns 1 if arguments are the same file, 0 otherwise
proc samefile {file1 file2} {
  set err 0
  if [catch "file stat $file1 stat1"] {incr err}
  if [catch "file stat $file2 stat2"] {incr err}
  if {$err == 1} {return 0}
  if {$err == 2} {
    if {[mfv_util fullpath $file1] == [mfv_util fullpath $file2]} {
      return 1
    } else { return 0 }
  }
  if { $stat1(ino) == $stat2(ino) && $stat1(dev) == $stat2(dev)} { return 1 }
  return 0
}

proc tkButtonUp3 w {
  global tkPriv
  if {$w == $tkPriv(buttonWindow)} {
      set tkPriv(buttonWindow) ""
      $w config -relief $tkPriv(relief)
  }
}

# replace default grouping Menu commands
proc tkMbMotion {w upDown rootx rooty} {
  global tkPriv

  if {$tkPriv(inMenubutton) == $w} {
    return
  }
  set new [winfo containing $rootx $rooty]
  if {($new != $tkPriv(inMenubutton)) && (($new == "")
					  || ([winfo toplevel $new] == [winfo toplevel $w]))} {
    if {$tkPriv(inMenubutton) != ""} {
      tkMbLeave $tkPriv(inMenubutton)
    }
    if {($new != "") && ([winfo class $new] == "Menubutton")
	&& ([$new cget -indicatoron] == 0)} {
      if {[bind $new <Motion>] == "break"} return
      if {$upDown == "down"} {
	tkMbPost $new $rootx $rooty
      } else {
	tkMbEnter $new
      }
    }
  }
}

proc tkMbSingleEnter w {
  global tkPriv

  if {$tkPriv(inMenubutton) != ""} return
  set tkPriv(inMenubutton) $w
  if {[$w cget -state] != "disabled"} {
    $w configure -state active
  }
}

proc tkMbSingleLeave w {
  global tkPriv

  if {$tkPriv(inMenubutton) != $w} return
  set tkPriv(inMenubutton) {}
  if ![winfo exists $w] {
    return
  }
  if {[$w cget -state] == "active"} {
    $w configure -state normal
  }
}

proc tkMbMakeSingle w {
  global tkPriv
  bind $w <Enter> { tkMbSingleEnter %W; break }
  bind $w <Leave> { tkMbSingleLeave %W; break }
  bind $w <1> {tkMbPost %W %X %Y}
  bind $w <Motion> break
  bind $w <B1-Motion> break
  lappend tkPriv(menuSingle) $w
}

############################################################
proc mfv:noop { args } {
  # do nothing. Returns <args> as string separated by spaces
  return [join $args " "]
}

proc mfv:see-stack-trace {{w {}}} {
  global mfp errorInfo
  if [lempty $w] {
    set w [ut:simpletext -title "TkMail: Stack Trace" \
	       -text $errorInfo]
  } else {
    $w.txt delete 1.0 end
    $w.txt insert end $errorInfo
  }
  focus $w.txt
  return 0
}

proc mfv:current-top {} {
  global mfp

  if [string length [set top [focus]]] {
    return [winfo toplevel $top]
  } elseif [winfo exists $mfp(curtop)] {
    return $mfp(curtop)
  } else {
    return [lindex $mfp(viewlist) 0]
  }
}

proc mfv:error-mesg { str {master {}} } {
  # popup a error message using <str> and eval mf(viewer-beep-error.
  # if <master> given, use it to place dialog 
  global mf mfp
  
  eval $mf(viewer-beep-error)
  if {![winfo exists $master]} { set master [mfv:current-top] }
  mfv:log-mesg {} "ERROR: $str" 
  mfv:wait-off

  if {[winfo exists $master]} {
    if {[string first "\n" $str] > -1} {
      # set mfp(stack-trace) [mfv:stack-trace 1]
      set w [ut:simpletext -title "TkMail: ERROR" -text $str \
		 -buttons {{OK} {{Stack Trace} mfv:see-stack-trace %W}} \
		 -grab 1 -master $master]
      $w.txt configure -height 8
      focus $w.txt
      tkwait window $w
    } else {
      if {![ut:getok -title "TkMail: ERROR" -prompt $str -bitmap error \
		-nolabel "Stack Trace" -master $master]} {
	mfv:see-stack-trace
      }
    }
  } else {
    puts stderr "ERROR: $str"
  }
}
proc mfv:disable-hook hook {
  rename $hook 0
  return 1
}

proc mfv:run-hook { hook args } {
  global mf

  if {[info proc $hook]!=""} {
    if [catch [concat $hook $args] res] {
      eval $mf(viewer-beep-error)
      set buttons [list [list {Disable Hook} mfv:disable-hook $hook]]
      lappend buttons {{Stack Trace} mfv:see-stack-trace %W}
      lappend buttons {Ignore}
      set w [ut:simpletext -text "ERROR running $hook: $res" \
		 -buttons $buttons -title {Hook Error!} -grab 1 \
		 -master [mfv:current-top]]
      $w.txt configure -height 8
      focus $w.txt
      tkwait window $w
    }
  }
}

proc mfv:check-old-settings { } {
  global mf mfp

  set warnmesg {}
  if [info exists mf(mail-record)] {
    append warnmesg "	mf(mail-record) -> mf(compose-fcc-folder)\n"
  }
  if [info exists mf(compose-fcc-default)] {
    append warnmesg "	mf(compose-fcc-default) -> mf(compose-fcc-folder)\n"
  }
  if [info exists mf(compose-fcc-list)] {
    append warnmesg "	mf(compose-fcc-list) ~> mf(compose-fcc-folder)\n"
  }
  if [info exists mf(compose-fcc-bysender)] {
    append warnmesg "	mf(compose-fcc-bysender) ~> mf(compose-fcc-folder)\n"
  }
  if [info exists mf(compose-fcc-directory)] {
    append warnmesg "	mf(compose-fcc-directory) ~> mf(compose-fcc-folder)\n"
  }
  if [info exists mf(mail-record-swap)] {
    append warnmesg "	mf(mail-record-swap) -> mf(compose-fcc-swap)\n"
  }
  if [info exists mf(mail-record-forward)] {
    append warnmesg "	mf(mail-record-forward) -> mf(compose-fcc-forward)\n"
  }
  if [info exists mf(compose-quick-forward)] {
    append warnmesg "	mf(compose-quick-forward) is no longer supported\n"
  }
  if [info exists mf(menu-sender-full)] {
    append warnmesg "	mf(menu-sender-full) ~> mf(mail-archive-folder)\n"
  }
  if [info exists mf(menu-sender-list)] {
    append warnmesg "	mf(menu-sender-list) ~> mf(mail-archive-folder)\n"
  }
  if [info exists mf(menu-default-new)] {
    append warnmesg "	mf(menu-default-new) is no longer supported. Use Lock.\n"
  }
  if [info exists mf(bind-emacs)] {
    append warnmesg "	mf(bind-emacs) is no longer supported. Use ~/.tkbindrc.\n"
  }
  if [info exists mf(bind-use-meta)] {
    append warnmesg "	mf(bind-use-meta) is no longer supported. Use ~/.tkbindrc.\n"
  }
  if [info exists mf(bind-use-esc)] {
    append warnmesg "	mf(bind-use-esc) is no longer supported. Use ~/.tkbindrc.\n"
  }
  if [string length $warnmesg] {
    mfv:error-mesg "The following old settings need to be updated or removed from your $mfp(setfile) file:\n\n$warnmesg"
  }
}

proc mfv:reset-viewer { top } {
  global mf mfp

  keylset mfp($top) fid mfv:nofolder
  keylset mfp($top) file {}
  $top.$mfp(hstat) configure -text {No folder}
  mfv:empty-viewer $top
}

proc mfv:empty-viewer { top } {
  global mf mfp

  keylset mfp($top) curnum 0
  keylset mfp($top) mesgnum 0
  set mfp($top.$mfp(mesg)) {}

  $top.$mfp(head) delete 0 end

  $top.$mfp(mesg) configure -state normal
  $top.$mfp(mesg) delete 1.0 end
  tkTextUndoSetup $top.$mfp(mesg)
  $top.$mfp(mesg) configure -state $mf(viewer-state)

  $top.$mfp(mstat) configure -text "Message 0 out of 0"
}

proc mfv:folder-crash {fid} {
  global mfp

  puts stderr "\nFOLDER-CRASH:"
  set lvl [expr [info level]-1]
  while {$lvl > 0} {
    puts stderr [info level $lvl]
    incr lvl -1
  }


  catch "mfv_close $fid"

  if ![info exists mfp($fid)] return

  if [keylget mfp($fid) viewers vlist] {
    foreach viewer $vlist { catch "mfv:reset-viewer $viewer" }
  }
  unset mfp($fid)
  catch "unset mfp($fid,delmesg)"
}

proc mfv:checklist-add {file} {
  global mfp mf_check

  set file [mfv_util fullpath $file]

  if {![info exists mf_check($file)]} {
    if {[file exists $file] && 
	![catch "file stat $file stat"]} {
      set mf_check($file) $stat(size)
    } else {
      set mf_check($file) 0
    }
  }
}

proc mfv:checklist-remove {file} {
  global mfp mf_check

  set file [mfv_util fullpath $file]
  if {$file == $mfp(mbox) || $file == $mfp(inbox)} return

  if {[info exists mf_check($file)]} {
    unset mf_check($file)
  }
}

proc mfv:check-new-mail {} {
  global mf mfp mf_check

  foreach fid [mfv_util list] {
    set newm [mfv:check-append $fid]
    if {$newm > 0} {
      mfv:newlist-add [$fid info dir]/[$fid info name]
    }
  }

  set mfid [mfv_util folderid $mfp(mbox)]

  foreach file [array names mf_check] {
    if {[file exists $file] && 
	![catch "file stat $file stat"]} {
      set fid [mfv_util folderid $file]
      if [string length $fid] {
	# skip check, was done above
      } else {
	if {$stat(size) > $mf_check($file)} {
	  eval $mf(viewer-beep-new)
	  set name [file tail $file]
	  mfv:log-mesg {} "New mail messages have arrived in $name!" 1
	  if {$mfp(mbox-model) && $file == $mfp(inbox) &&
	      [string length $mfid]} {
	    mfv:newlist-add $mfp(mbox)
	    if $mf(mail-auto-incorp) {
	      if [catch mfv:incorp cnt] {
		mfv:error-mesg $cnt
	      }
	      if {$cnt > 0} {
		if [keylget mfp($mfid) viewers vlist] {
		  foreach viewer $vlist {mfv:iconic-new-mail $viewer}
		}
	      }
	    } else {
	      if [keylget mfp($mfid) viewers vlist] {
		foreach viewer $vlist {
		  catch {wm iconbitmap $viewer "@$mf(viewer-bitmap-mail)"}
		}
	      }
	    }
	  } else {
	    mfv:newlist-add $file
	  }
	}
      }
      set mf_check($file) $stat(size)
    } else {
      set mf_check($file) 0
    }
  }

  # check if person thought it was for seconds and not milliseconds
  if {$mf(mail-interval) < 1000} {set mf(mail-interval) [expr $mf(mail-interval)*1000]}
  after $mf(mail-interval) mfv:check-new-mail
}

# argument file should be full path
proc mfv:get-file-abbrev {file {max 30}} {
  global mf mfp

  set efile $file
  if [catch {set maildir [mfv_util fullpath $mf(mail-directory)]}] {
    set maildir XXXX
  }
  if {$file == $mfp(inbox)} {
    set efile InBox
  } elseif {$file == $mfp(mbox)} {
    set efile MBox
  } elseif {[string first $maildir $file] > -1} {
    set len [string length $maildir]
    set efile +[string range $efile [string length $maildir] end]
  } elseif {[string first $mfp(homedir) $file] > -1} {
    set len [string length $maildir]
    set efile ~[string range $efile [string length $mfp(homedir)] end]
  }

  while {[string length $efile] > $max && [string first "/" $efile] > -1} {
    set efile [string range $efile 4 end]
    set efile [string range $efile [string first "/" $efile] end]
    set efile "...$efile"
  }
  return $efile
}

proc mfv:newlist-add {file} {
  global mf mfp mf_newlist

  set file [mfv_util fullpath $file]
  if [info exists mf_newlist($file)] {return $mf_newlist($file)}

  set efile [mfv:get-file-abbrev $file]
  set mf_newlist($file) $efile

  foreach top $mfp(viewlist) {
    if [winfo exists $top.bb.newm.m] {
      if {$efile == "InBox"} {
	$top.bb.newm.m insert 1 command -label $efile -command \
	    "mfv:goto-new-mail \$mfp(inbox) $top"
      } else {
	$top.bb.newm.m add command -label $efile -command \
	    [list mfv:goto-new-mail $file $top]
      }
      $top.bb.newm configure -state normal
    }
  }

  return $efile
}

proc mfv:newlist-remove {file} {
  global mf mfp mf_newlist

  if ![info exists mf_newlist($file)] return

  foreach top $mfp(viewlist) {
    if [winfo exists $top.bb.newm.m] {
      if ![catch "$top.bb.newm.m index $mf_newlist($file)" ndx] {
	$top.bb.newm.m delete $ndx
      }
      if {[$top.bb.newm.m index end] < 1} {
	$top.bb.newm configure -state disabled
      }
    }
  }

  unset mf_newlist($file)
}

proc mfv:goto-new-mail {file top} {
  global mf mfp

  if ![string length $file] {
    set file [keylget mfp($top) file]
  }

  set func mfv:goto-newest
  if {$mfp(mbox-model) && 
      ([samefile $file $mfp(inbox)] || [samefile $file $mfp(mbox)])} {
    if {[file exists $mfp(inbox)] && [file size $mfp(inbox)]} {
      set func mfv:mbox
    }
    set file $mfp(mbox)
  }

  set fid [mfv_util folderid $file]
  if {[string length $fid] 
      && [keylget mfp($fid) viewers vlist] && [llength $vlist]} {
    if {[keylget mfp($top) fid] != $fid} {
      foreach viewer [concat $vlist [lindex $vlist 0]] {
	if [winfo ismapped $viewer] break
      }
    } else {
      set viewer $top
    }
    wm deiconify $viewer
    raise $viewer
    $func $viewer
    if $mfp($viewer,newmail) {mfv:cancel-new-mail $viewer}
  } else {
    mfv:explicit-open $top $file -1
    if {$func == "mfv:mbox"} { mfv:mbox $top }
  }
}

proc mfv:cancel-new-mail {top} {
  global mfp mf

  set fid [keylget mfp($top) fid]
  if {$mfp(mbox-model)} {
    if {$fid == [mfv_util folderid $mfp(mbox)]} {
      if {[file exists $mfp(inbox)] && [file size $mfp(inbox)]} {
	return
      }
    }
  }

  keylget mfp($fid) viewers vlist
  foreach viewer $vlist {
    catch {wm iconbitmap $viewer "@$mf(viewer-bitmap-nomail)"}
    set mfp($viewer,newmail) 0
    tkBindRemoveTag $viewer NewMail
  }
}

bind NewMail <Map> {
  # mfv:goto-new-mail {} %W
  mfv:goto-newest %W
  mfv:cancel-new-mail %W
}

proc mfv:iconic-new-mail {viewer {force 0}} {
  global mf mfp

  if {($force || ![winfo ismapped $viewer]) &&
      [file exists $mf(viewer-bitmap-mail)] &&
      [file exists $mf(viewer-bitmap-nomail)]} {
    wm iconbitmap $viewer "@$mf(viewer-bitmap-mail)"
    set mfp($viewer,newmail) 1
    if {[lsearch -exact [bindtags $viewer] NewMail] < 0} {
      bindtags $viewer [concat NewMail [bindtags $viewer]]
    }
  }
}

proc mfv:check-append {fid} {
  global mf mfp

  if {[catch {$fid info max} oldmax]} {return -1}

  if {[catch "$fid check" appm]} {
    mfv:folder-crash $fid
    mfv:error-mesg $appm
    return -1
  }
  if {$appm < 1} {return 0}

  set newlist {}
  foreach msg [$fid info new] {
    if {$msg > $oldmax} { lappend newlist $msg }
  }
  set newm [llength $newlist]
  set oldm [expr $appm-$newm]
  set name [$fid info name]

  if {$oldm > 0} {
    mfv:log-mesg {} "$oldm old mail messages have been appended to $name!"
  }

  if {$newm > 0} {
    eval $mf(viewer-beep-new)
    mfv:log-mesg {} "$newm new mail messages have arrived in $name!" 1
  }
  if {$appm > 0} {
    if [keylget mfp($fid) viewers vlist] {
      foreach viewer $vlist {
	# TODO: update viewer listing better
	mfv:reset-summary $viewer
	if {$newm > 0} {mfv:iconic-new-mail $viewer}
      }
    }
  } else {
    eval $mf(viewer-beep-empty)
  }

  return $newm
}

proc mfv:folder-lock {fid} {
  global mf mfp

  while {[set res [$fid lock]] < 0} {
    if {$res == -2} {
      if $mfp(noask) {
	set mfp(last-error) "[$fid info name] locked by another process."
	return 1
      } else {
	set res [tk_dialog .tmp_dlg "File Locked" \
	    "Folder [$fid info name] is locked by another process." \
		     error 2 Override {Try Again} Cancel]
	if {$res == 2} {return 2}
	if {$res == 0} {$fid unlock}
      }
    } else {
      set mfp(last-error) "Cannot lock folder [$fid info name]. Got error: [mfv_util lockerror]"
      return 1
    }
  }
  return 0
}

proc mfv:folder-unlock {fid} {
  global mfp
  if [$fid unlock] {
    set mfp(last-error) [mfv_util lockerror]
    return 1
  }
  return 0
}

proc mfv:file-lock {filename} {
  global mf mfp errorCode

  set tid [mfv_util folderid $filename]
  if [string length $tid] {
    return [mfv:folder-lock $tid]
  } else {
    set dotlock [mfv_set lock dotlock]
    set retries [mfv_set lock retries]
    set tout [mfv_set lock timeout]
    if {[mfv_util fullpath $filename] == $mfp(inbox)} {
      set lockcmd "exec $dotlock -s -r $retries -t $tout"
      set unlockcmd "exec $dotlock -u -s"
    } else {
      set lockcmd "exec $dotlock -r $retries -t $tout $filename"
      set unlockcmd "exec $dotlock -u $filename"
    }
    while {[catch $lockcmd res]} {
      if {[lindex $errorCode 2] != 2} { 
	set mfp(last-error) $res
	return 1
      }
      set res [tk_dialog .tmp_dlg "File Locked" \
		   "Folder [file tail $filename] is locked by another process." \
		   error 2 Override {Try Again} Cancel]
      if {$res == 2} {return 2}
      if {$res == 0} {
	if [catch $unlockcmd res] {
	  set mfp(last-error) "Could not remove dotlock on $filename\n$res"
	  return 1
	}
      }
    }
  }
  return 0
}

proc mfv:file-unlock {filename} {
  global mf mfp

  set tid [mfv_util folderid $filename]
  if [string length $tid] {
    return [mfv:folder-unlock $tid]
  } else {
    set dotlock [mfv_set lock dotlock]
    if {[mfv_util fullpath $filename] == $mfp(inbox)} {
      set unlockcmd "exec $dotlock -u -s"
    } else {
      set unlockcmd "exec $dotlock -u $filename"
    }
    if {[catch $unlockcmd res]} {
      set mfp(last-error) $res
      return 1
    }
  }
  return 0
}

proc mfv:folder-safe-save {fid args} {
  global mf mfp

  if [catch "$fid save $args" res] { 
    set mfp(last-error) $res
    if {[string first folderID $res] > -1} {
      mfv:folder-crash $fid
    }
    return 1
  }
  return 0
}

proc mfv:compose-list-tops { } {
  set clist {}
  foreach child [winfo children .] {
    if {[winfo class $child] == "MailCompose"} {
      lappend clist $child
    }
  }
  return $clist
}

proc mfv:folder-backup { fid } {
  global mf mfp env mf_autofile

  if [catch "$fid info count"] {
    error "$fid in no longer a valid open folder"
  }

  if {![$fid info readonly] && [$fid info count] &&
      [$fid info modified] != [keylget mfp($fid) modified]} {
    set bdir [$fid info directory]
    set afile "$bdir/\#[$fid info name]\#"
    if [file writable $bdir] {
      if [ catch {$fid save $afile} res] {
	if {[string first folderID $res] > -1} {
	  mfv:folder-crash $fid
	}
	error $res
      } else {
	keylset mfp($fid) modified [$fid info modified]
	set mf_autofile($fid) $afile
      }
    }
  }
}

proc mfv:autosave { } {
  global mf mfp env

  if {$mf(mail-autosave) < 1} return
  if ![llength $mfp(viewlist)] return
  if ![winfo exists $mfp(curview)] { set mfp(curview) [lindex $mfp(viewlist) 0] }

  set savetext [lindex [$mfp(curview).$mfp(mstat) configure -text] 4]
  $mfp(curview).$mfp(mstat) configure -text "Autosaving ..."
  mfv:wait-on

  set err {}
  foreach fid [mfv_util list] {
    if [catch "mfv:folder-backup $fid" res] {
      append err $res\n\n
    }
  }

  foreach mfc [mfv:compose-list-tops] {
    if [scan $mfc ".mfc%d" num] {
      catch {mfv:text-to-file $mfc.comp.txt $env(HOME)/letter$mfc 1}
    }
  }

  $mfp(curview).$mfp(mstat) configure -text $savetext
  mfv:wait-off

  if [string length $err] { mfv:error-mesg $err }

  # check if person thought it was for seconds and not milliseconds
  if {$mf(mail-autosave) < 1000} {set mf(mail-autosave) [expr $mf(mail-autosave)*1000]}
  after $mf(mail-autosave) "mfv:autosave"
}

proc mfv:reset-summary { viewer {goto 1}} {
  global mf mfp

  set fid [keylget mfp($viewer) fid]
  set lndx [$viewer.$mfp(head) cursingle]
  set msg [mfv:head-to-num $viewer $lndx]
  set force 0

  if {[$fid info count] < 1} {
    mfv:empty-viewer $viewer
    return
  }
  $viewer.$mfp(head) delete 0 end

  set sortkeys $mfp($viewer,sort)
  if {$sortkeys == "user-defined"} {
    set sortkeys $mf(headlist-sort)
  }

  if $mfp($viewer,reverse) {
    set ndx 0
  } else { set ndx end }

  if $mfp($viewer,hidedel) {
    set lastmsg -1
    foreach m [$fid sort $sortkeys] {
      if [$fid message flag $m deleted] {
	if {$msg == $m} { set msg -3; set force 1 }
      } else {
	if {$msg == -3} { set msg $m }
	$viewer.$mfp(head) insert $ndx [$fid message summary $m]
	set lastmsg $m
      }
    }
    if {$msg == -3} { set msg $lastmsg }
  } else {
    foreach m [$fid sort $sortkeys] {
      $viewer.$mfp(head) insert $ndx [$fid message summary $m]
      if {[$fid message flag $m deleted]} {
	eval "$viewer.$mfp(head) item configure $ndx $mf(headlist-deleted-config)"
      }
    }
  }

  if $goto {
    if {[string length $msg] && $msg > 0} {
      mfv:goto-mesg $viewer $msg $force
      $viewer.$mfp(head) see [$viewer.$mfp(head) cursingle]
    } elseif [$fid info count] {
      if {$lndx >= [$viewer.$mfp(head) size]} {
	set lndx [$viewer.$mfp(head) index end]
      }
      set msg [mfv:head-to-num $viewer $lndx]
      if {[string length $msg] && $msg > 0} {
	mfv:goto-mesg $viewer $msg
      } else {
	mfv:goto-mesg $viewer [mfv:head-to-num $viewer 0]
      }
    }
  }

  if ![string length [$fid info new]] {
    mfv:newlist-remove [$fid info dir]/[$fid info name] 
  }

}

proc mfv:openlist-add {file} {
  global mf mfp mf_openlist

  set file [mfv_util fullpath $file]
  if [info exists mf_openlist($file)] {return $mf_openlist($file)}

  set efile [mfv:get-file-abbrev $file 22]
  set mf_openlist($file) $efile

  set olist [array names mf_openlist]
  set max $mf(menu-recent-max)
  if {$max < 10} { set max 10 }
  if {[llength $olist] > $max} {
    set top [lindex $mfp(viewlist) 0]
    set last [$top.$mfp(hstat).m entrycget [$top.$mfp(hstat).m index end] -label]
    foreach ofile $olist {
      if {$mf_openlist($ofile) == $last} {
	unset mf_openlist($ofile)
	break
      }
    }
    set trim 1
  } else {
    set trim 0
  }

  foreach top $mfp(viewlist) {
    if [winfo exists $top.$mfp(hstat).m] {
      if {$efile == "InBox"} {
	$top.$mfp(hstat).m insert 1 command -label $efile -command \
	    "mfv:explicit-open $top \$mfp(inbox) {} 0 0"
      } else {
	$top.$mfp(hstat).m insert 2 command -label $efile -command \
	    [list mfv:explicit-open $top $file {} 0 0]
      }
      if $trim { $top.$mfp(hstat).m delete end }
    }
  }

  return $efile
}

proc mfv:close-folder { top } {
  global mfp mf_autofile

  keylget mfp($top) fid fid
  if {![string length $fid] || $fid == "mfv:nofolder"} {
    return 0
  }

  keylget mfp($fid) viewers oldlist
  set newlist {}
  foreach viewer $oldlist {
    if {$viewer != $top} { lappend newlist $viewer }
  }

  keylset mfp($top) fid mfv:nofolder
  keylset mfp($top.$mfp(mesg)) fid mfv:nofolder

  if {[llength $newlist] == 0} {
    if {[llength [$fid info modified]]} {
      if [set res [mfv:folder-safe-save $fid]] {
	if {$res != 2} {
	  mfv:error-mesg $mfp(last-error) $top
	}
	return 1
      }
    }
    if [catch "mfv_close $fid" res] {
      mfv:error-mesg $res $top
      return 1
    } else {
      unset mfp($fid)
      catch "unset mfp($fid,delmesg)"
      if [info exists mf_autofile($fid)] {
	exec rm -rf $mf_autofile($fid)
	unset mf_autofile($fid)
      }
    }
  } else {
    keylset mfp($fid) viewers $newlist
  }

  return 0
}

proc mfv:nofolder {args} {
  error "Oops. There is no folder opened in this viewer. Cannot continue."
}

proc mfv:setup-folder { top folder {ndx {}}} {
  # Open up <folder> in viewer <top> and set view to message at <ndx>
  # Close old folder associated with viewer if not shared with another
  global mf mfp
  
  pdebug "mfv:setup-folder called with $top $folder $ndx\n"
  if {[lempty $folder]} { return 1}
  mfv:wait-on
  
  # close previous folder in viewer
  if [mfv:close-folder $top] {
    mfv:wait-off
    return 1
  }

  if {[catch "mfv_open $folder" fid]} {
    mfv:reset-viewer $top
    mfv:error-mesg $fid $top
    mfv:wait-off
    return 1
  }
  set folder [$fid info directory]/[$fid info name]

  if [samefile $folder $mfp(mbox)] {
    wm iconname $top $mfp(user)
  } else {
    wm iconname $top [file tail $folder]
  }
  pdebug "Folder $folder opened\n"

  keylset mfp($top) fid $fid
  if [info exists mfp($fid)] {
    keylget mfp($fid) viewers vlist
  } else {
    keylset mfp($fid) modified {}
  }
  lappend vlist $top
  keylset mfp($fid) viewers $vlist

  mfv:reset-summary $top 0

  # set new viewer specific database items
  keylset mfp($top) file $folder
  keylset mfp($top) curnum 0
  set mesgnum [$top.$mfp(head) size]
  keylset mfp($top) mesgnum $mesgnum

  $top.$mfp(mesg) configure -state normal
  $top.$mfp(mesg) delete 1.0 end
  tkTextUndoSetup $top.$mfp(mesg)
  $top.$mfp(mesg) configure -state $mf(viewer-state)

  $top.$mfp(hstat) configure -text [mfv:openlist-add $folder]

  pdebug "$mesgnum messages\n"
  
  if {$mesgnum==0} {
    $top.$mfp(mstat) configure -text "Message 0 out of 0"
    mfv:wait-off
    return 0
  }

  set nmsgs [$fid info new]
  if {[llength $nmsgs] && [samefile $folder $mfp(mbox)]} {
    eval $mf(viewer-beep-new)
    $top.$mfp(mstat) configure -text "[file tail $folder] has New Mail"
    mfv:iconic-new-mail $top
    if [lempty $ndx] { set ndx -1 }
  }

  if {[lempty $ndx] && [keylget mfp(last-mesg) $folder lastmesg]
      && $lastmesg <= $mesgnum} {
    mfv:goto-mesg $top $lastmesg
  } else {
    if {[lempty $ndx]} { set ndx [mfv:head-to-num $top 0] }
    if { $ndx < 1 } {
      mfv:goto-newest $top
    } else {
      mfv:goto-mesg $top $ndx
    }
  }
  catch {$top.$mfp(head) see [mfv:cursingle $top]}

  mfv:wait-off
  return 0
}

proc mfv:goto-newest { viewer {S N}} {
  global mf mfp
  
  if {$mfp($viewer,reverse)} {
    set ndx [$viewer.$mfp(head)_text search -regexp -backwards \
		 "^ \[$S\] *\[0-9\]+" end 1.0]
  } else {
    set ndx [$viewer.$mfp(head)_text search -regexp \
		 "^ \[$S\] *\[0-9\]+" 1.0 end]
  }

  if [string length $ndx] {
    set ndx [expr [lindex [split $ndx .] 0]-1]
    if {$ndx > -1} {
      mfv:select-mesg $viewer from $ndx
    }
  } else {
    if {$S == "N"} {
      return [mfv:goto-newest $viewer U]
    } else {
      mfv:select-mesg $viewer from [$viewer.$mfp(head) index end]
    }
  }
}

proc mfv:goto-mesg { viewer msg {goto 1} } {
  global mf mfp

  set ndx [$viewer.$mfp(head)_text search -regexp \
	       "^>*\[NDAFU* \]*$msg " 1.0 end]

  if {$ndx == 0.0} {
    mfv:select-mesg $viewer from 0
  } else {
    set ndx [expr [lindex [split $ndx .] 0]-1]
    if {$goto} {
      mfv:select-mesg $viewer from $ndx
    } else {
      $viewer.$mfp(head) selection clear
      $viewer.$mfp(head) selection set $ndx
    }
  }
}

proc mfv:insert-mesg-contents {top tw fid msg} {
  global mf mfp

  mfv:wait-on
  $tw insert insert [$fid message headers $msg] headers
  $tw insert insert \n {}

  set type [$fid message mimepart $msg type 0]
  if {[lsearch -exact [list multipart application message image \
			   audio video text] $type] < 0} {
    set type {}
  }

  if {![string length $type] || !$mf(mime-parse)} {
    $tw insert insert [$fid message body $msg]
  } else {
    # TODO: MIME
    mfv:mime-show-part $top $tw $fid $msg [$fid message mimelist $msg]
  }
  mfv:run-hook mfv:display-mesg-hook $top $tw
  mfv:wait-off
}

proc mfv:display-mesg { top msg } {
  # Display message <msg> from folder associated with viewer <top> 
  # <button> is button associated with selection if any
  global mf mfp
  pdebug "Displaying message $msg in $top\n"

  set fid [keylget mfp($top) fid]

  set tw $top.$mfp(mesg)
  keylset mfp($tw) fid $fid
  keylset mfp($tw) msg $msg
  $tw configure -state normal
  $tw delete 1.0 end
  tkTextUndoSetup $tw
  
  if {![string length $msg] || $msg < 1} {
    $top.$mfp(mstat) configure -text "No messages"
    unset mfp($tw)
    keylset mfp($top) curnum 0
    $tw configure -state $mf(viewer-state)
    return 1
  }

  # verify msg is valid
  if [catch {$fid message lines $msg} msglines] {
    $top.$mfp(mstat) configure \
	-text "Message $msg does not exist"
    $tw configure -state $mf(viewer-state)
    unset mfp($tw)
    keylset mfp($top) curnum 0
    return 1
  }
  
  keylset mfp($top) curnum $msg
  mfv:mesg-set-sender $top

  set msglines [$fid message lines $msg]
  if {$mf(mail-read-ask) && $mf(mail-read-max) < $msglines} {
    set fromline UNKNOWN
    set subjline UNKNOWN
    set fromline [$fid message field $msg from]
    set subjline [$fid message field $msg subject]
    $tw insert 1.0 "From: $fromline\n"
    $tw insert 2.0 "Subject: $subjline\n\n"
    $tw insert 4.0 "     Message is $msglines lines long. Click here to view whole message."
    $tw tag add seelong 4.5 "4.5 lineend"
    $tw tag bind seelong <1> \
	"mfv:wait-on; mfv:simple-display-mesg $top.$mfp(mesg) $msg; mfv:wait-off"
  } else {
    $tw mark set insert 1.0
    mfv:insert-mesg-contents $top $tw $fid $msg
  }
  $tw mark set insert 1.0

  # check if no more new message in folder
  if {[llength [$fid info new]] == 0} {
    mfv:newlist-remove [$fid info dir]/[$fid info name]
  }

  # notify user of message read
  $top.$mfp(mstat) configure \
      -text "Message $msg out of [$fid info count]"
  $tw configure -state $mf(viewer-state)
  
  # remove possible unread status symbol from listbox
  set tndx [expr [mfv:cursingle $top]+1]
  if {$tndx > 0} {
    set stat [$top.$mfp(head)_text get $tndx.1]
    if {$stat == "U" || $stat == "N"} {
      $top.$mfp(head)_text insert $tndx.2 " "
      $top.$mfp(head)_text delete $tndx.1
    }
  }
  return 0
}

proc mfv:extract-address { field {num 0} } {
  set field [mfv_util simplify $field]
  if {[string first , $field] != -1} {
    set field [lindex [split $field ,] $num]
  }
  if {[regexp {<([^, <>]+)>} $field trash res]} {
    set field $res
  }
  if {[string first ":" $field] != -1} {
    set field [lrange [split $field ":"] 1 end]
  }
  return [string trim $field]
}

proc mfv:get-from { tw {strip 1}} {
  # Return the user addresss from From: field of text widget <tw>
  # Actually tries Return-Path, then Reply-To, then From, then sm-from
  global mf mfp

  set curfrom ""

  if {[info exists mfp($tw)] && [keylget mfp($tw) fid fid] 
      && [keylget mfp($tw) msg msg]} {
    foreach field { reply-to from return-path sm-from } {
      set curfrom [$fid message field $msg $field]
      if {![lempty $curfrom]} break
    }

    if $strip {
      set curfrom [mfv_util simplify $curfrom]
      if {[regexp {<([^, <>]+)>} $curfrom trash res]} {
	set curfrom $res
      }
    }
  }

  return $curfrom
}

proc mfv:viewer-get-field { top fieldlist {def {}}} {
  global mf mfp

  set res $def
  set fid [keylget mfp($top) fid]
  set msg [keylget mfp($top) curnum]

  if {![$fid message exists $msg]} return {}
  foreach field $fieldlist {
    set field [string tolower $field]
    set res [$fid message field $msg $field]
    if {![lempty $res]} break
  }

  return $res
}

proc mfv:mesg-set-sender {top} {
  global mf mfp

  if {[string range $mf(mail-archive-folder) 0 0] == "@"} {
    set archproc [string range $mf(mail-archive-folder) 1 end]
    if [catch "set mfp($top,sender) \[$archproc $top\]" res] {
      mfv:error-mesg "ERROR running $archproc: $res\n\nFeature disabled." $top
      set mfp($top,sender) {}
    }
  } else {
    set mfp($top,sender) $mf(mail-archive-folder)
  }
  if [lempty $mfp($top,sender)] {
    set mfp($top,sender) [mfv:sender-default-hook 1 $top]
  }
  set ftail [file tail $mfp($top,sender)]
  if {![string length $ftail]} { set ftail "<None>" }
  $top.menu.mesg.m.move entryconfigure 3 -label $ftail
  $top.bb.move.m entryconfigure 3 -label $ftail
  $top.menu.mesg.m.copy entryconfigure 3 -label $ftail
  if {$mfp(fmenusender) > $mfp(fmenulast)} {
    $top.menu.folder.m entryconfigure $mfp(fmenusender) -label $ftail
  }
}

proc mfv:sender-default-hook {short top} {
  global mf mfp

  set fromname [mfv:viewer-get-field $top \
		    {reply-to from return-path sm-from}]
  set fromname [string tolower [mfv:extract-address $fromname 0]]

  if {$short} {
    return [lindex [split $fromname "@"] 0]
  }
  return $fromname
}

proc mfv:show-full-headers { top } {
  global mf mfp 

  set tw $top.$mfp(mesg)
  set fid [keylget mfp($top) fid]
  set range [$tw tag nextrange headers 1.0]
  set start [lindex $range 0]
  set stop [lindex $range 1]
  keylget mfp($top) curnum msg
  $tw configure -state normal
  $tw delete $start $stop
  $tw mark set insert $start
  $tw insert $start [$fid message headers $msg full]
  $tw tag add headers $start insert
  $tw configure -state $mf(viewer-state)
  return 1
}

proc mfv:strip-comment { txt {keepq 1} } {
  if !$keepq { return [mfv_util simplify -stripq $txt]}
  return [mfv_util simplify $txt]
}

proc mfv:cursingle { top } {
  global mfp

  set ndx [$top.$mfp(head) cursingle]
  if {[lempty $ndx]} {
    set ndx [keylget mfp($top) curnum]
    incr ndx -1
    if {$ndx > [$top.$mfp(head) size]} {return -1}
  }
  return $ndx
}

proc mfv:simple-display-mesg {tw msg} {
  # Simple placement of message <msg> in viewer <top>
  global mf mfp

  set top [winfo toplevel $tw]
  set fid [keylget mfp($top) fid]
  keylset mfp($tw) fid $fid
  keylset mfp($tw) msg $msg
  $tw configure -state normal
  $tw delete 1.0 end
  mfv:insert-mesg-contents $top $tw $fid $msg
  $tw mark set insert 1.0
  $tw configure -state $mf(viewer-state)
}

proc mfv:reload-mesg { top } {
  global mf mfp
  
  set fid [keylget mfp($top) fid]
  set newcur [mfv:head-to-num $top [mfv:cursingle $top]]
  if {![$fid message exists $newcur]} { 
    set newcur [keylget mfp($top) curnum]
    if {![$fid message exists $newcur]} {return 0}
  }
  mfv:display-mesg $top $newcur
  catch "$top.$mfp(head) see [mfv:cursingle $top]"
  focus $top.$mfp(mesg)
  return 1
}

proc mfv:select-mesg { top mode tndx {force 0} {def 1.0}} {
  # Select message at index <tndx> in the header list of <top>.
  # if <mode> is "from" or "at", the actual selection is done in the listbox
  global mf mfp
  
  if {![string length $tndx]} { set tndx $def }
  if [string length $mode] {
    if {$mode == "from"} {
      $top.$mfp(head) selection clear
    }
    if $force {
      $top.$mfp(head) selection at $tndx
    } else {
      $top.$mfp(head) selection set $tndx
    }
  }

  if {![string length [$top.$mfp(head) cursingle]]} {
    $top.$mfp(head) selection clear
    $top.$mfp(head) selection set $tndx
  }
  set newcur [mfv:head-to-num $top [mfv:cursingle $top]]
  if {!$newcur} {return 0}
  
  # primary selection has changed
  if {$newcur != [keylget mfp($top) curnum] || $force} {
    mfv:display-mesg $top $newcur
    keylset mfp($top) curnum $newcur
    $top.$mfp(head) see $tndx
  }
  focus $top.$mfp(mesg)
  return 1
}

proc mfv:head-to-num { top tndx } {
  # Translate header list index <tndx> of <top> to message number
  global mf mfp
  
  if {[lempty $tndx]} {return 0}
  
  if {[regexp {[0-9][0-9]*} \
      [$top.$mfp(head) get $tndx] ndx]} {
    return [string trim $ndx]
  } else {
    return 0
  }
  
}

proc mfv:wait-on { } {
  # Set cursor to watch bitmap for windows in mfp(waitlist)
  global mf mfp
  
  set cnt 0
  foreach w $mfp(waitlist) {
    if {[winfo ismapped [winfo toplevel $w]]} {
      if {[set tcur [lindex [$w configure -cursor] 4]] != "watch"} {
	set mfp($w,cursor) $tcur
	$w configure -cursor watch
	incr cnt
      }
    }
  }
  if {$cnt} {
    update idletasks
    after idle mfv:wait-off-really
  }
}

proc mfv:wait-off { } { # dummy }

proc mfv:wait-off-really { } {
  # Set cursor to default for windows in mfp(waitlist)
  global mf mfp

  set cnt 0
  foreach w $mfp(waitlist) {
    if {[winfo ismapped [winfo toplevel $w]]} { incr cnt }
    if {$mfp($w,cursor) != ""} {
      $w configure -cursor $mfp($w,cursor)
    }
  }
  if {$cnt} {update idletasks}
}

# parse user alias file
proc mfv:parse-alias-file { } {
  # parse user's alias file mf(mail-alias-file) according to mf(mail-alias-type)
  global mf mfp env
  
  set mfp(aliasfiles) {}
  set mfp(aliasnames) {}
  set mfp(aliasdesc) {}
  set mfp(aliasaddr) {}
  
  if {[lempty $mf(mail-alias-file)]} { return 0}
  set cwd [pwd]

  foreach afile $mf(mail-alias-file) {
    if {[file exists $afile]} {
      cd $mfp(homedir)
      switch -regexp -- $mf(mail-alias-type) {
	{^bsd$} { mfv:parse-bsd-aliases $afile }
	{^elm$} { mfv:parse-elm-aliases $afile }
	default {
	  mfv:error-mesg \
	      "Unknown alias file type: $mf(mail-alias-type)!"
	  return 1
	}
      }
    }
  }

  cd $cwd
  return 0
}

proc mfv:parse-bsd-aliases { afile } {
  # parse BSD style alias file <afile>
  global mf mfp env
  
  if {![string length $afile] || ![file exists $afile]} return
  if {[mfv:file-to-var $afile filetext]} {
    mfv:error-mesg $mfp(last-error)
    return 1
  }

  file stat $afile fstat
  set key $fstat(dev)@$fstat(ino)
  if {[lsearch -exact $mfp(aliasfiles) $key] > -1} {
    puts stderr "Already parsed $afile"
    return 1
  }
  lappend mfp(aliasfiles) $key

  # concat continued lines (ones that end with backslash)
  regsub -all "\\\\\[ \t\]*\n\[ \t\]*" $filetext { } filetext
  foreach tline [split $filetext \n] {
    set line [string trim $tline]
    set tmp [lindex $line 0]
    switch -regexp -- $tmp {
      {^(a|alias|g|group)$} {
	lappend mfp(aliasnames) [lindex $line 1]
	lappend mfp(aliasdesc) {}
	set realaddr [string trim [lrange $line 2 end]]
	if [regexp {^['"](.*)['"]$} $realaddr trash stripped] {
	  lappend mfp(aliasaddr) $stripped
	} else {
	  lappend mfp(aliasaddr) $realaddr
	}
      }
      {^(alt|alternates)$} {
	# set mf(compose-alternates) [lrange $line 2 end]
      }
      {^cd$} {
	if [catch {cd [lrange $line 1 end]}] {
	  puts stderr "Cannot change directory to [lrange $line 1 end]"
	}
      }
      {^source$} {
	if [mfv:parse-bsd-aliases [lrange $line 1 end]] {
	  puts stderr "Cannot read file [lrange $line 1 end]"
	}
      }
    }
  }
  return 0
}

# parse an elm alias
proc mfv:parse-elm-aliases { afile } {
  # parse Elm style alias file <afile>
  global mf mfp env
  
  if {![file exists $afile]} return
  if {[mfv:file-to-var $afile filetext]} {
    mfv:error-mesg $mfp(last-error)
    return 1
  }
  
  # concat continued lines (ones that start with space or tab)
  regsub -all "\n\[ \t\]+" $filetext { } filetext
  foreach tline [split $filetext \n] {
    set line [string trim $tline]
    if {[string index $line 0] == "#"} continue
    # strip space around equals signs:
    if {[regsub -all { *= *} $line {=} line]} {
      set topfields [split $line {=}]
      # Position 0 -- aliases
      # Position 1 -- description
      # Position 2 -- email addresses
      regsub -all {[, ]+} [lindex $topfields 0] { } anamelist
      foreach aname $anamelist {
	lappend mfp(aliasnames) $aname
	lappend mfp(aliasdesc) [lindex $topfields 1]
	lappend mfp(aliasaddr) [join [lrange $topfields 2 end] =]
      }
    }
  }
  return 0
}

proc mfv:menu-create { m } {
  # create new menu <m> only if it doesn't already exist
  if {[winfo exists $m]} {
    $m delete 0 last
  } else {menu $m}
}

proc mfv:bind-file-complete { w } {
  bind $w <Key-Tab> {
    set f [%W get]; %W delete 0 end
    %W insert end [j:expand_filename $f]
  }
}

proc mfv:get-filename { args } {
  global mf mfp

 j:parse_args { \
   {prompt "File: "} \
   {callback ""} \
   {cbargs ""} \
   {cancelvalue ""} \
   {master ""} \
   {dir ""} }

  if {![winfo exists $master] && [winfo exists $mfp(curtop)]} { set master $mfp(curtop) }
  return [ut:fsbox -master $master -grab 1 -prompt $prompt \
	     -title "TkMail v$mfp(version) FileSelector" \
	      -quick "{Mail $mf(mail-directory)}" -cancelvalue $cancelvalue \
	      -callback $callback -cbargs $cbargs -dir $dir]
}

proc mfv:folder-menu-sender { op top } {
  # Run procedure <op> using <top> and <Sender> folder name as arguments
  global mf mfp

  if {$mfp($top,sender) != ""} {
    if {[regexp {^/} $mfp($top,sender)] || [regexp {^\./} $mfp($top,sender)]} {
      if {![file exists $mfp($top,sender)]} {mfv:add-recent $mfp($top,sender)}
      $op $top $mfp($top,sender)
    } else {
      if {![file exists $mf(mail-directory)/$mfp($top,sender)]} {
	mfv:add-recent $mf(mail-directory)/$mfp($top,sender)
      }
      $op $top $mf(mail-directory)/$mfp($top,sender)
    }
  } else {
    mfv:error-mesg "Could not determine filename based on sender for operation." $top
  }
}

proc mfv:build-folder-menus { {vlist ""} } {
  # Rebuild folder menus for viewers in <vlist>. Default is all.
  global mf mfp

  if {[lempty $vlist]} {
    set vlist $mfp(viewlist)
  }
  set keepdir [pwd]
  
  mfv:wait-on
  
  foreach top $vlist {
    # setup menus of folders in user's folder directory
    if {$mfp(fmenulast) != [$top.menu.folder.m index last]} {
      $top.menu.folder.m delete [expr $mfp(fmenulast)+1] last
    }
    mfv:menu-create $top.menu.mesg.m.copy
    mfv:menu-create $top.menu.mesg.m.move
    mfv:menu-create $top.bb.move.m

    if {$mf(menu-recent-max) > 0} {
      mfv:menu-create $top.menu.folder.m.recent
      mfv:menu-create $top.menu.mesg.m.copy.recent
      mfv:menu-create $top.menu.mesg.m.move.recent
      mfv:menu-create $top.bb.move.m.recent

      $top.menu.folder.m add cascade -label {Recent} \
	  -menu $top.menu.folder.m.recent
      $top.menu.mesg.m.copy add cascade -label {Recent} \
	  -menu $top.menu.mesg.m.copy.recent
      $top.menu.mesg.m.move add cascade -label {Recent} \
	  -menu $top.menu.mesg.m.move.recent
      $top.bb.move.m add cascade -label {Recent} \
	  -menu $top.bb.move.m.recent
    }

    $top.menu.folder.m add command -label {Other . . .} \
	-command "mfv:explicit-open $top \[mfv:get-filename -master $top\]"
    $top.menu.mesg.m.move add command -label {Other . . .} \
	-command "mfv:explicit-move $top \[mfv:get-filename -master $top\]"
    $top.bb.move.m add command -label {Other . . .} \
	-command "mfv:explicit-move $top \[mfv:get-filename -master $top\]"
    $top.menu.mesg.m.copy add command -label {Other . . .} \
	-command "mfv:explicit-copy $top \[mfv:get-filename -master $top\]"

    if {[file isdirectory $mf(mail-directory)]} {

      $top.menu.folder.m add command -label {<Sender>} \
	  -command "mfv:folder-menu-sender mfv:explicit-open $top"
      set mfp(fmenusender) [$top.menu.folder.m index last]
      if {$mf(menu-folders-max) > 0} {
	$top.menu.folder.m add separator
      }
      $top.menu.mesg.m.move add command -label {<Sender>} \
	  -command "mfv:folder-menu-sender mfv:explicit-move $top"
      $top.menu.mesg.m.move add separator
      $top.bb.move.m add command -label {<Sender>} \
	  -command "mfv:folder-menu-sender mfv:explicit-move $top"
      $top.bb.move.m add separator
      $top.menu.mesg.m.copy add command -label {<Sender>} \
	  -command "mfv:folder-menu-sender mfv:explicit-copy $top"
      $top.menu.mesg.m.copy add separator

      mfv:set-folder-menus $top $mf(mail-directory) "" -1
    } else {
      set mfp(fmenusender) -1
    }

    # append mf(menu-quick-send) contents to Mesg menu
    if {$mfp(mmenulast) != [$top.menu.mail.m index last]} {
      $top.menu.mail.m delete [expr $mfp(mmenulast)+1] last
    }
    if {[llength $mf(menu-quick-send)]} {
      $top.menu.mail.m add separator
      if {$mf(menu-quick-send) == "@aliases"} {
	foreach addr $mfp(aliasnames) {
	  $top.menu.mail.m add command -label $addr \
	      -command "mfv:compose -viewer $top -sendto {$addr}"
	}
      } else {
	foreach addr $mf(menu-quick-send) {
	  $top.menu.mail.m add command -label $addr \
	      -command "mfv:compose -viewer $top -sendto {$addr}"
	}
      }
    }
    mfv:add-recent-to-top $top
  }
  
  mfv:wait-off
  cd $keepdir
}

proc mfv:set-folder-menus { top dir extmenu depth } {
  # Build a folder menu for viewer <top> from directory <dir> as part
  # of menu <extmenu> at <depth>
  global mf mfp
  
  pdebug "Setting up menu for $dir\n"
  incr depth

  cd $dir
  set rdir [string range $dir [expr [string length $mf(mail-directory)]+1] end]
  if [string length $rdir] { append rdir / }
  set foldfiles [lsort [glob -nocomplain *]]
  set chopped 0
  set mcnt 0
  set icnt 0

  foreach mfold $foldfiles {
    
    set skipit 0
    if {[string match *.lock $mfold]} continue
    if {[string match *.bak $mfold]} continue
    if {[string match \#*\# $mfold]} continue
    foreach ifold $mf(menu-folders-ignore) {
      if {[string match $ifold ${rdir}$mfold]} { set skipit 1; break }
      if {[string match $ifold $dir/$mfold]} { set skipit 1; break }
    }
    if {$skipit} continue

    if {[string index $mfold 0] == "."} {
      continue
    }

    incr icnt
    if {$icnt > $mf(menu-folders-max)} {
      set chopped [expr $mf(menu-folders-max) > 0]
      break
    }

    if {[file isfile $dir/$mfold]} {
      $top.menu.folder.m$extmenu add command -label $mfold \
	  -command "mfv:explicit-open $top {$dir/$mfold} {} 0 0"
      $top.menu.mesg.m.copy$extmenu add command -label $mfold \
	  -command "mfv:mesg-copy $top {$dir/$mfold}"
      $top.menu.mesg.m.move$extmenu add command -label $mfold \
	  -command "mfv:mesg-move $top {$dir/$mfold}"
      $top.bb.move.m$extmenu add command -label $mfold \
	  -command "mfv:mesg-move $top {$dir/$mfold}"
      
    } elseif {[file isdirectory $dir/$mfold] && $depth < $mf(menu-depth-max) } {
      
      mfv:menu-create $top.menu.folder.m${extmenu}.f$mcnt
      $top.menu.folder.m$extmenu add cascade \
	  -label $mfold \
	  -menu $top.menu.folder.m${extmenu}.f$mcnt
      
      mfv:menu-create $top.menu.mesg.m.copy${extmenu}.f$mcnt
      $top.menu.mesg.m.copy$extmenu add cascade \
	  -label $mfold \
	  -menu $top.menu.mesg.m.copy${extmenu}.f$mcnt
      
      mfv:menu-create $top.menu.mesg.m.move${extmenu}.f$mcnt
      $top.menu.mesg.m.move$extmenu add cascade \
	  -label $mfold \
	  -menu $top.menu.mesg.m.move${extmenu}.f$mcnt
      
      mfv:menu-create $top.bb.move.m${extmenu}.f$mcnt
      $top.bb.move.m$extmenu add cascade \
	  -label $mfold \
	  -menu $top.bb.move.m${extmenu}.f$mcnt
      
      mfv:set-folder-menus $top $dir/$mfold ${extmenu}.f$mcnt $depth

      incr mcnt
    }
  }
  
  if {$chopped} {
    $top.menu.folder.m$extmenu add command -label "+++ chopped +++" \
	-command "mfv:explicit-open $top \[mfv:get-filename -master $top -dir $mf(mail-directory)\]"
    $top.menu.mesg.m.copy$extmenu add command -label "+++ chopped +++" \
	-command "mfv:explicit-copy $top \[mfv:get-filename -master $top -dir $mf(mail-directory)\]"
    $top.menu.mesg.m.move$extmenu add command -label "+++ chopped +++" \
	-command "mfv:explicit-move $top \[mfv:get-filename -master $top -dir $mf(mail-directory)\]"
    $top.bb.move.m$extmenu add command -label "+++ chopped +++" \
	-command "mfv:explicit-move $top \[mfv:get-filename -master $top -dir $mf(mail-directory)\]"
  }
  
}

proc mfv:add-recent { file } {
  # Add <file> to the Recent folder menu item for all viewers
  global mf mfp

  if [catch {mfv_util fullpath $file} file] return

  if {$mf(menu-recent-exclusive)} {
    if { [samefile $file $mfp(mbox)] || \
	     $file == $mfp(inbox)} {return 0}
    if ![catch {set maildir [mfv_util fullpath $mf(mail-directory)]}] {
      if {[string first $maildir $file] > -1} {return 0}
    }
  }

  if {[set ndx [lsearch $mfp(recentlist) $file]] > -1} {
    set mfp(recentlist) [lreplace $mfp(recentlist) $ndx $ndx]
  }
  
  set mfp(recentlist) [lrange [linsert $mfp(recentlist) 0 $file] \
      0 [expr $mf(menu-recent-max)-1]]
  
  foreach top $mfp(viewlist) {
    mfv:add-recent-to-top $top
  }
}

proc mfv:add-recent-to-top { top } {
  # Add <file> to Recent folder menu of viewer <top>
  global mf mfp
  
  if {![winfo exists $top.menu.folder.m.recent]} {return 0}

  $top.menu.folder.m.recent delete 0 last
  $top.menu.mesg.m.copy.recent delete 0 last
  $top.menu.mesg.m.move.recent delete 0 last
  $top.bb.move.m.recent delete 0 last
  
  foreach folder $mfp(recentlist) {
    set mfold [file tail $folder]
    $top.menu.folder.m.recent add command -label $mfold \
	-command "mfv:add-recent {$folder}; mfv:explicit-open $top {$folder} {} 0 0"
    $top.menu.mesg.m.copy.recent add command -label $mfold \
	-command "mfv:add-recent {$folder}; mfv:mesg-copy $top {$folder}"
    $top.menu.mesg.m.move.recent add command -label $mfold \
	-command "mfv:add-recent {$folder}; mfv:mesg-move $top {$folder}"
    $top.bb.move.m.recent add command -label $mfold \
	-command "mfv:add-recent {$folder}; mfv:mesg-move $top {$folder}"
  }
}

set mfp(logwindow) ".NONE"
proc mfv:log-mesg { top str {all 0}} {
  # Log message <str> showing pre-colon part in status label of <top>
  global mf mfp
  
  if { ![winfo exists $mfp(logwindow)] } {
    set mfp(logwindow) [ut:simpletext -title "TkMail: Message Log" \
			    -keep 1 -text $str -master $top]
    wm withdraw $mfp(logwindow)
    pdebug "Created log window $mfp(logwindow)\n"
  } else {
    $mfp(logwindow).txt configure -state normal
    $mfp(logwindow).txt insert end "$str\n"
    $mfp(logwindow).txt configure -state disabled
  }

  if $all {
    set vlist $mfp(viewlist)
  } else { set vlist $top }

  foreach viewer $vlist {
    if {[winfo exists $viewer.$mfp(mstat)]} {
      $viewer.$mfp(mstat) configure -text [lindex [split $str :] 0]
    }
  }
}

proc mfv:show-log { top } {
  global mf mfp
  if {[winfo exists $mfp(logwindow)]} {
    wm geometry $mfp(logwindow) +[winfo rootx $top]+40
    wm deiconify $mfp(logwindow)
    raise $mfp(logwindow)
  }
}

proc mfv:clear-text-mem { tw } {
  # Cleanup memory associated with text widget <tw>
  global mfp

  if {[info exists mfp($tw)]} {unset mfp($tw)}
  tkTextUndoSetup $tw
  return 1
}

proc mfv:toggle-video { args } {
  foreach tw $args {
    set backgr [lindex [$tw configure -background] 4]
    $tw configure -background \
	[lindex [$tw configure -foreground] 4]
    $tw configure -foreground $backgr
    if {[lsearch [$tw tag names] headers] > -1} {
      set backgr [lindex [$tw tag configure headers -background] 4]
      $tw tag configure headers -background \
	  [lindex [$tw tag configure headers -foreground] 4]
      $tw tag configure headers -foreground $backgr
    }
  }
}

proc mfv:toggle-fixed-font { top tw } {
  global mf mfp

  if $mfp($top,fixed) {
    if [catch {$tw configure -font $mf(disp-font-fixed)}] {
      $tw configure -font fixed
    }
  } else {
    if [catch {$tw configure -font $mf(disp-font-default)}] {
      if [catch {$tw configure -font -*-helvetica-medium-r-normal-*-12-*}] {
	$tw configure -font [lindex [$mfp(tmptxt) configure -font] 4]
      }
    }
  }
}

proc mfv:close-viewer { top } {
  # Close viewer <top> possible closing the folder associated with it if
  # no other viewers are using it
  global mf mfp

  if $mfp(trap-exit) return

  if {[set ndx [lsearch -exact $mfp(viewlist) $top]] > -1} {

    set mfp(viewlist) [lreplace $mfp(viewlist) $ndx $ndx]
    if {[llength $mfp(viewlist)] < 1} {
      mfv:quit
      lappend mfp(viewlist) $top
      return 2
    }

    if [mfv:close-folder $top] {
      return 1
    }

    set filename [keylget mfp($top) file]
    keylset mfp(last-mesg) $filename [mfv:head-to-num $top [mfv:cursingle $top]]

    unset mfp($top)
    foreach opt {fixed sort reverse sender lock newmail revvid horiz} {
      unset mfp($top,$opt)
    }
    mfv:clear-text-mem $top.$mfp(mesg)
    mfv:clear-text-mem $top.mesg2.txt
    while {[set ndx [lsearch -glob $mfp(waitlist) $top.*]] != -1} {
      set mfp(waitlist) [lreplace $mfp(waitlist) $ndx $ndx]
    }
    if {[winfo exists ${top}_search]} {
      unset mfp($top,search,case)
      unset mfp($top,search,regexp)
      unset mfp($top,search,back)
      unset mfp($top,search,where)
      destroy ${top}_search
    }
  }
  if {$mfp(curtop) == $top} {set mfp(curtop) [lindex $mfp(viewlist) 0]}
  if {$mfp(curview) == $top} {set mfp(curview) [lindex $mfp(viewlist) 0]}
  catch "destroy $top"

  return 0
}

proc mfv:new-viewer { filename {iconic 0} {msgndx {}}} {
  # Open up new viewer for folder <filename>. If <isman>, this is master viewer
  global mf mfp env tkBind mf_newlist mf_openlist

  set cnt 0
  while {[winfo exists .mf${cnt}]} {incr cnt}
  set top .mf${cnt}
  lappend mfp(viewlist) $top

  set mfp($top,fixed) $mf(disp-default-fixed)
  set mfp($top,sort) user-defined
  set mfp($top,reverse) $mf(headlist-reverse)
  set mfp($top,hidedel) $mf(headlist-deleted-hide)
  set mfp($top,sender) {}
  set mfp($top,lock) $mf(viewer-start-locked)
  set mfp($top,newmail) 0
  set mfp($top,revvid) 0
  set mfp($top,horiz) $mf(disp-horiz-scroll)
  set mfp($top) {}
  keylset mfp($top) fid mfv:nofolder
  keylset mfp($top) file {}
  keylset mfp($top) curnum 0
  keylset mfp($top) mesgnum 0

  toplevel $top -class MailView
  wm iconname $top "TkMail"
  wm title $top "TkMail v$mfp(version)"
  wm minsize $top 400 400
  wm protocol $top WM_DELETE_WINDOW "mfv:close-viewer $top"
  wm withdraw $top
  bind $top <FocusIn> {
    set mfp(curtop) [winfo toplevel %W]
    set mfp(curview) [winfo toplevel %W]
  }

  frame $top.menu -relief raised
  menubutton $top.menu.folder -text {Folder} -menu $top.menu.folder.m -underline 0
  menubutton $top.menu.edit -text {Edit} -menu $top.menu.edit.m -underline 0
  menubutton $top.menu.mesg -text {Mesg} -menu $top.menu.mesg.m -underline 3
  menubutton $top.menu.mail -text {Compose} -menu $top.menu.mail.m -underline 0
  menubutton $top.menu.view  -text {View} -menu $top.menu.view.m -underline 0
  menubutton $top.menu.opt  -text {Options} -menu $top.menu.opt.m -underline 0
  menubutton $top.menu.help -text {Help} -menu $top.menu.help.m -underline 0
  
  menu $top.menu.folder.m
  $top.menu.folder.m add command -label {Open . . .} -accelerator {[o]} -underline 0 \
      -command "mfv:explicit-open $top \[mfv:get-filename -master $top\] {} 0 1"
  $top.menu.folder.m add command -label {Save} -underline 0 \
      -command "mfv:save-folder $top 0"
  $top.menu.folder.m add command -label {Save Sorted} -underline 0 \
      -command "mfv:save-folder $top 1"
  $top.menu.folder.m add command -label {Close} -accelerator {[w]} -underline 0 \
      -command "mfv:close-viewer $top"
  $top.menu.folder.m add command -label {Quit} -accelerator {[q]} -underline 0 \
      -command "mfv:quit"

  $top.menu.folder.m add separator
  $top.menu.folder.m add command -label {Main Box} -accelerator {[b]} -underline 5 \
      -command "mfv:explicit-open $top \$mfp(mbox) {} 0 0"
  $top.menu.folder.m add command -label {Force Autosave Now} -underline 12 \
      -command "mfv:folder-backup \[keylget mfp($top) fid\]"
  $top.menu.folder.m add command -label {Force New Mail Check} -underline 12 \
      -command "mfv:check-append \[keylget mfp($top) fid\]"
  $top.menu.folder.m add separator
  set mfp(fmenulast) [$top.menu.folder.m index last]
  
  menu $top.menu.edit.m
  $top.menu.edit.m add command -label {Cut} -underline 2 \
      -command "tkTextCut $top.mesg.txt"
  $top.menu.edit.m add command -label {Copy} -underline 0 \
      -command "tkTextCopy $top.mesg.txt"
  $top.menu.edit.m add command -label {Paste} -underline 0 \
      -command "tkTextYankPop $top.mesg.txt"
  $top.menu.edit.m add command -label {Select All} -underline 2 \
      -command "tkTextMarkRegion $top.mesg.txt"
  $top.menu.edit.m add separator
  $top.menu.edit.m add command -label {Search . . .} -underline 0 \
      -command "mfv:search-prompt $top"
  $top.menu.edit.m add command -label {Search Again} -underline 7 \
      -command "mfv:search $top ${top}_search"
  $top.menu.edit.m add separator
  $top.menu.edit.m add command -label {Save X Selection . . .} -underline 2 \
      -command "mfv:write $top \[mfv:get-filename -master $top\] xsel"
  $top.menu.edit.m add command -label {Print X Selection . . .} -underline 6 \
      -command "mfv:print $top xsel"
  $top.menu.edit.m add command -label {TCL Evaluate X Sel} -underline 4 \
      -command "mfv:tcl-eval-sel"
  $top.menu.edit.m add command -label {UNIX Pipe X Sel . . .} -underline 0 \
      -command "mfv:pipe $top xsel $top.mesg.txt 0"
  
  if {!$tkBind(emacs)} {
    $top.menu.edit.m entryconfigure {Cut} -accelerator {[x]}
    $top.menu.edit.m entryconfigure {Copy} -accelerator {[c]}
    $top.menu.edit.m entryconfigure {Paste} -accelerator {[v]}
    $top.menu.edit.m entryconfigure {Search . . .} -accelerator {[f]}
    $top.menu.edit.m entryconfigure {Search Again} -accelerator {[g]}
  }

  menu $top.menu.mesg.m
  $top.menu.mesg.m add command -label {Read} \
      -command "mfv:reload-mesg $top" -underline 0
  $top.menu.mesg.m add command -label {Unread} \
      -command "mfv:mesg-mark-unread $top"
  $top.menu.mesg.m add separator
  $top.menu.mesg.m add cascade -label {Copy} \
      -menu $top.menu.mesg.m.copy -underline 0
  $top.menu.mesg.m add cascade -label {Move} \
      -menu $top.menu.mesg.m.move -underline 0
  $top.menu.mesg.m add command -label {Delete} -accelerator {[d]} \
      -command "mfv:mesg-delete $top" -underline 0
  $top.menu.mesg.m add command -label {Undelete} -accelerator {[u]} -underline 0 \
      -command "mfv:mesg-undelete $top"
  $top.menu.mesg.m add command -label {Select All} \
      -command "$top.$mfp(head) selection set 0 end" -underline 1
  $top.menu.mesg.m add separator
  $top.menu.mesg.m add command -label {Write Body . . .} -accelerator {[s]} -underline 0 \
      -command "mfv:explicit-write $top \[mfv:get-filename -master $top\] mesg"
  $top.menu.mesg.m add command -label {Print . . .} -accelerator {[p]} \
      -command "mfv:print $top mesg" -underline 4
  $top.menu.mesg.m add command -label {UNIX Pipe . . .} -underline 3 \
      -command "mfv:pipe $top mesg $top.mesg.txt 0"
  $top.menu.mesg.m add separator
  $top.menu.mesg.m add command -label {Show Full Headers} \
      -command "mfv:show-full-headers $top" -underline 1
  $top.menu.mesg.m add separator
  $top.menu.mesg.m add command -label {Detach} -command "mfv:detach-mesg $top" -underline 5
  $top.menu.mesg.m add command -label {Split/Unsplit} -underline 3 \
      -command "mfv:split-mesg-view $top"
  $top.menu.mesg.m add command -label {Quick Decode} -underline 0 \
      -command "mfv:quick-decode $top.mesg.txt"
  $top.menu.mesg.m add command -label {Alias Current} -underline 11 \
      -command "mfv:alias-current $top"

  menu $top.menu.mail.m
  $top.menu.mail.m add command -label {New . . .} -accelerator {[m]} \
      -command "mfv:compose -viewer $top" -underline 0
  $top.menu.mail.m add command -label {Reply . . .} -accelerator {[r]} \
      -command "mfv:reply $top 0 0" -underline 0
  $top.menu.mail.m add command -label {Reply All . . .} -accelerator {[t]} \
      -command "mfv:reply $top 0 1" -underline 6
  $top.menu.mail.m add command -label {Forward . . .} -accelerator {[k]} \
      -command "mfv:forward $top 3" -underline 0
  $top.menu.mail.m add separator
  $top.menu.mail.m add command -label {Restore Last . . .} -underline 8 \
      -command "mfv:restore-last \[mfv:compose -viewer $top\]"
  $top.menu.mail.m add separator
  $top.menu.mail.m add command -label {TkMail Support . . .} -underline 7 -command {
    global mf mfp
    $mfp(tmptxt) delete 1.0 end
    catch {$mfp(tmptxt) insert end "Machine/OS: [exec uname -a]\n"}
    catch {$mfp(tmptxt) insert end "Tk Version: $tk_patchLevel\n"}
    catch {$mfp(tmptxt) insert end "TkMail Version: $mfp(version)\n\n"}
    catch {$mfp(tmptxt) insert end "Mfv Version: [mfv_util version]\n\n"}
    foreach name [lsort [array names mf]] {
      $mfp(tmptxt) insert end "  mf($name) {$mf($name)}\n"
    }
    $mfp(tmptxt) insert end "------------------------------------\n"
    $mfp(tmptxt) insert end "NOTE: Please insert your $mfp(setfile) file unless you think it isn't relevant.\n\n"
    mfv:compose -sendto raines@slac.stanford.edu \
	-subject "TkMail Beta Support" -incmesg 2
    }
  set mfp(mmenulast) [$top.menu.mail.m index last]
  
  menu $top.menu.view.m

  $top.menu.view.m add radiobutton -label "Sort Normal" -underline 5 \
      -command "mfv:reset-headlist $top" -variable mfp($top,sort) -value normal
  $top.menu.view.m add radiobutton -label "Sort From Addr" -underline 5 \
      -command "mfv:reset-headlist $top" -variable mfp($top,sort) -value sm-from
  $top.menu.view.m add radiobutton -label "Sort Full Name" -underline 6 \
      -command "mfv:reset-headlist $top" -variable mfp($top,sort) -value fullname
  $top.menu.view.m add radiobutton -label "Sort Subject" -underline 7 \
      -command "mfv:reset-headlist $top" -variable mfp($top,sort) -value subject
  $top.menu.view.m add radiobutton -label "Sort Time Received" -underline 5 \
      -command "mfv:reset-headlist $top" -variable mfp($top,sort) -value received
  $top.menu.view.m add radiobutton -label "User Defined" -underline 5 \
      -command "mfv:reset-headlist $top" -variable mfp($top,sort) -value user-defined

  $top.menu.view.m add separator
  $top.menu.view.m add checkbutton -label "Hide Deleted" -underline 2 \
      -variable mfp($top,hidedel) \
      -command "mfv:reset-summary $top"
  $top.menu.view.m add checkbutton -label "Fixed-spaced font" -underline 2 \
      -variable mfp($top,fixed) \
      -command "mfv:toggle-fixed-font $top $top.mesg.txt"
  $top.menu.view.m add checkbutton -label "Reverse Order" -underline 0 \
      -variable mfp($top,reverse) -command "mfv:reset-headlist $top"
  $top.menu.view.m add checkbutton -label "Reverse Video" -underline 2 \
      -variable mfp($top,revvid) \
      -command "mfv:toggle-video $top.$mfp(head)_text $top.$mfp(mesg)"
  $top.menu.view.m add checkbutton -label "Horizontal Scrollbar" -underline 0 \
      -variable mfp($top,horiz) -command "mfv:toggle-horiz $top"

  menu $top.menu.opt.m

  $top.menu.opt.m add command -label "Edit Global Settings . . ." -underline 5 \
      -command "mfv:opt-master"
  $top.menu.opt.m add command -label "Edit Aliases . . ." -underline 5 \
      -command "mfv:edit-alias-file $top"
  
  $top.menu.opt.m add separator
  $top.menu.opt.m add command -label {Rebuild Folder Menus} -underline 8 \
      -command "mfv:build-folder-menus; mfv:mesg-set-sender $top"
  $top.menu.opt.m add command -label {Reread Alias File} -underline 7 \
      -command "mfv:parse-alias-file"

  $top.menu.opt.m add separator
  if $mfp(mbox-model) {
    $top.menu.opt.m add checkbutton -label "Auto-Incorporate" -underline 5 \
	-variable mf(mail-auto-incorp) -command "global mfopt; set mfopt(modified) 1"
  }
  $top.menu.opt.m add checkbutton -label "Popup Notice of New Mail" -underline 0 \
      -variable mf(notify-popup) -command "global mfopt; set mfopt(modified) 1" \
      -state disabled
  $top.menu.opt.m add checkbutton -label "Ask to Continue on Long Mesg" -underline 2 \
      -variable mf(mail-read-ask) -command "global mfopt; set mfopt(modified) 1"
  $top.menu.opt.m add checkbutton -label "Strip Header on Insert" -underline 6 \
      -variable mf(insert-strip) -command "global mfopt; set mfopt(modified) 1"
  $top.menu.opt.m add checkbutton -label "Parse MIME messages" -underline 6 \
      -variable mf(mime-parse) -command "mfv:reload-mesg $top"

  $top.menu.opt.m add separator
  $top.menu.opt.m add command -label "Save Settings" -underline 0 \
      -command "mfv:opt-save .mf_dummy 0"

  # SETUP HELP MENU
  menu $top.menu.help.m
  $top.menu.help.m add command -label {Intro} -command "mfv:display-help $top TOP" \
      -accelerator {[h]} -underline 0
  
  set mfp(readme) [list "GENERAL USAGE" "ALIASES" \
      "MOUSE BINDINGS" "KEY BINDINGS" "VIEWER MENU" "COMPOSE MENU" \
      "PRINTING" "HEADER FIELD STRIPPING" "SUMMARY LISTBOX FORMAT" \
      "CC:, BCC: and FCC:" "SIGNATURE" "MIME" "USER SETTINGS" "WIDGET CONFIGURATION" \
      "USEFUL METHODS" "BUGS" "FAQ" "FUTURE" "COPYRIGHT" "DISCLAIMER"]
  
  foreach topic $mfp(readme) {
    $top.menu.help.m add command -label [string tolower $topic] \
	-command "mfv:display-help $top \{$topic\}" -underline 0
  }
  $top.menu.help.m add separator
  $top.menu.help.m add command -label {Show Log} -underline 5 \
      -command "mfv:show-log $top"
  
  # PACK MENU
  pack $top.menu.folder $top.menu.edit $top.menu.mesg \
      $top.menu.mail $top.menu.view $top.menu.opt -side left
  pack $top.menu.help -side right

  if $mfp(print-noprompt) {
      $top.menu.mesg.m entryconfigure {Print*} -label Print
      $top.edit.mesg.m entryconfigure {Print X*} -label {Print X Selection}
  }

  pdebug "  Menu\n"

  # HEADLIST STATUS LINE
  frame $top.stat
  menubutton $top.stat.folder -text Unknown -menu $top.stat.folder.m \
      -relief raised -indicatoron 1 -bd 2 -padx 4p -pady 1p  \
      -highlightthickness 2 -anchor c
  tkMbMakeSingle $top.stat.folder
  # label $top.stat.folder -relief raised -anchor w -width 22
  label $top.stat.mesg -relief raised -width 36 -pady 1p
  checkbutton $top.stat.lock -text "Lock " \
	-variable mfp($top,lock) -relief raised -pady 1p
  pack $top.stat.lock -side left
  pack $top.stat.folder -side left
  pack $top.stat.mesg -side left -expand true -fill x

  # HEADLIST LISTBOX
  frame $top.head
  scrollbar $top.head.yscroll -command "$top.$mfp(head) yview" \
      -relief raised
  fancylistbox $top.$mfp(head) -yscroll "$top.head.yscroll set" \
      -relief sunken -selectmode extended
  bindtags $top.$mfp(head) "Listbox $top.$mfp(head) $top all"

  bind $top.$mfp(head) <ButtonRelease-1> \
      "mfv:select-mesg $top {} \[$top.$mfp(head) cursingle\]"
  bind $top.$mfp(head) <ButtonRelease-3> \
      "mfv:select-mesg $top at \[$top.$mfp(head) nearest %y\] 1"

  # BUTTONBAR
  frame $top.bb

  menubutton $top.bb.newm -text {New Mail} -menu $top.bb.newm.m \
      -relief raised -bd 2 -padx 8p -pady 4p -highlightthickness 2 -anchor c
  tkMbMakeSingle $top.bb.newm
  if $mfp(mbox-model) {
    button $top.bb.mbox -text "Mbox" -command "mfv:mbox $top"
  }
  menubutton $top.bb.move -text {Move} -menu $top.bb.move.m \
      -relief raised -bd 2 -padx 8p -pady 4p -highlightthickness 2 -anchor c
  tkMbMakeSingle $top.bb.move
  button $top.bb.del -text "Delete" -command "$top.menu.mesg.m invoke Delete*"
  button $top.bb.comp -text "Compose" -command "$top.menu.mail.m invoke New*"
  button $top.bb.reply -text "Reply" -command "$top.menu.mail.m invoke Reply*"
  button $top.bb.detach -text "Split" -command "$top.menu.mesg.m invoke Split*"
  button $top.bb.close -text "Close" -command "$top.menu.folder.m invoke Close*"

  bind Button <2> {tkButtonDown %W}
  bind Button <ButtonRelease-2> {tkButtonUp3 %W}
  bind Button <3> {tkButtonDown %W}
  bind Button <ButtonRelease-3> {tkButtonUp3 %W}
  
  menu $top.bb.newm.m
  foreach cfile [set newflist [array names mf_newlist]] {
    set efile $mf_newlist($cfile)
    if {$efile == "InBox"} {
      $top.bb.newm.m insert 1 command -label $efile -command \
	  "mfv:goto-new-mail \$mfp(inbox) $top"
    } else {
      $top.bb.newm.m add command -label $efile -command \
	  [list mfv:goto-new-mail $cfile $top]
    }
  }
  if {![llength $newflist]} { $top.bb.newm configure -state disabled }

  menu $top.$mfp(hstat).m
  foreach cfile [array names mf_openlist] {
    set efile $mf_openlist($cfile)
    if {$efile == "InBox"} {
      $top.$mfp(hstat).m insert 1 command -label $efile -command \
	  "mfv:explicit-open $top \$mfp(inbox) {} 0 0"
    } else {
      $top.$mfp(hstat).m add command -label $efile -command \
	  [list mfv:explicit-open $top $cfile {} 0 0]
    }
  }

  # include message with no prefix, no address
  bind $top.bb.comp <ButtonRelease-2> "mfv:forward $top 3"
  
  # include message with no prefix, with address
  bind $top.bb.reply <ButtonRelease-2> "mfv:reply $top 3"
  
  # detach current message
  bind $top.bb.detach <ButtonRelease-2> "$top.menu.mesg.m invoke Detach*"
  
  # include message with prefix, no address
  bind $top.bb.comp <ButtonRelease-3> "mfv:forward $top 1"
  
  # undelete current message
  bind $top.bb.del <ButtonRelease-3> "$top.menu.mesg.m invoke Undelete"
  
  # include message with prefix, with address
  bind $top.bb.reply <ButtonRelease-3> "mfv:reply $top 1"
  
  # unsplit current viewer
  bind $top.bb.detach <ButtonRelease-3> "$top.menu.mesg.m invoke Split*"

  # save folder
  bind $top.bb.close <ButtonRelease-3> "$top.menu.folder.m invoke Save"

  pack $top.bb.newm $top.bb.move $top.bb.del \
      $top.bb.comp $top.bb.reply $top.bb.detach $top.bb.close \
      -side left -expand true -fill x
  if [winfo exists $top.bb.mbox] {
    pack $top.bb.mbox -after $top.bb.newm -side left -expand true -fill x
  }
  pdebug "  ButtonBar\n"

  # TEXT WIDGETS

  frame $top.mesg
  scrollbar $top.mesg.yscroll -command "$top.mesg.txt yview" \
      -relief raised
  frame $top.hbar
  scrollbar $top.hbar.xscroll -command "$top.mesg.txt xview" \
      -relief raised -orient horizontal
  button $top.hbar.spacer -width [$top.mesg.yscroll cget -width] \
      -image blank -relief flat -borderwidth 0 -state disabled

  text $top.mesg.txt -yscroll "$top.mesg.yscroll set" \
      -relief sunken -bd 2 -wrap none -xscroll "$top.hbar.xscroll set"
  $top.mesg.txt tag configure seelong -borderwidth 2 -relief raised \
      -background [lindex [$top.bb.close configure -background] 4] \
      -font [lindex [$top.bb.close configure -font] 4]

  mfv:toggle-fixed-font $top $top.mesg.txt

  if {$tkBind(emacs)} {
    mfv:bind-menu-key $top.mesg.txt o "$top.menu.folder.m invoke {Open*}"
    mfv:bind-menu-key $top.mesg.txt n "$top.menu.view.m invoke {New*}"
    mfv:bind-menu-key $top.mesg.txt w "$top.menu.folder.m invoke {Close*}"
    mfv:bind-menu-key $top.mesg.txt q "$top.menu.folder.m invoke {Quit}"
    mfv:bind-menu-key $top.mesg.txt b "$top.menu.folder.m invoke {Main*}"
    mfv:bind-menu-key $top.mesg.txt i "$top.menu.folder.m invoke {Incor*}"
    mfv:bind-menu-key $top.mesg.txt Down "mfv:select-next $top"
    mfv:bind-menu-key $top.mesg.txt Up "mfv:select-prev $top"
    mfv:bind-menu-key $top.mesg.txt d "$top.menu.mesg.m invoke {Delete}"
    mfv:bind-menu-key $top.mesg.txt u "$top.menu.mesg.m invoke {Undelete}"
    mfv:bind-menu-key $top.mesg.txt s "$top.menu.mesg.m invoke {Save*}"
    mfv:bind-menu-key $top.mesg.txt p "$top.menu.mesg.m invoke {Print*}"
    mfv:bind-menu-key $top.mesg.txt m "$top.menu.mail.m invoke {New*}"
    mfv:bind-menu-key $top.mesg.txt r "$top.menu.mail.m invoke {Reply*}"
    mfv:bind-menu-key $top.mesg.txt t "$top.menu.mail.m invoke {Reply All*}"
    mfv:bind-menu-key $top.mesg.txt k "$top.menu.mail.m invoke {Forward*}"
    mfv:bind-menu-key $top.mesg.txt h "$top.menu.help.m invoke 0"
  } else {
    bind $top.mesg.txt <Control-o> "$top.menu.folder.m invoke {Open*}"
    bind $top.mesg.txt <Control-n> "$top.menu.view.m invoke {New*}"
    bind $top.mesg.txt <Control-w> "$top.menu.folder.m invoke {Close*}"
    bind $top.mesg.txt <Control-q> "$top.menu.folder.m invoke {Quit}"
    bind $top.mesg.txt <Control-b> "$top.menu.folder.m invoke {Main*}"
    bind $top.mesg.txt <Control-i> "$top.menu.folder.m invoke {Incor*}"
    bind $top.mesg.txt <Control-Down> "mfv:select-next $top"
    bind $top.mesg.txt <Control-Up> "mfv:select-prev $top"
    bind $top.mesg.txt <Control-d> "$top.menu.mesg.m invoke {Delete}"
    bind $top.mesg.txt <Control-u> "$top.menu.mesg.m invoke {Undelete}"
    bind $top.mesg.txt <Control-s> "$top.menu.mesg.m invoke {Save*}"
    bind $top.mesg.txt <Control-p> "$top.menu.mesg.m invoke {Print*}"
    bind $top.mesg.txt <Control-m> "$top.menu.mail.m invoke {New*}"
    bind $top.mesg.txt <Control-r> "$top.menu.mail.m invoke {Reply*}"
    bind $top.mesg.txt <Control-t> "$top.menu.mail.m invoke {Reply All*}"
    bind $top.mesg.txt <Control-k> "$top.menu.mail.m invoke {Forward*}"
    bind $top.mesg.txt <Control-h> "$top.menu.help.m invoke 0"
  }
  
  # configure second text widget for splits
  frame $top.mesg2
  scrollbar $top.mesg2.yscroll -command "$top.mesg2.txt yview" \
      -relief raised
  frame $top.hbar2
  scrollbar $top.hbar2.xscroll -command "$top.mesg2.txt xview" \
      -relief raised -orient horizontal
  button $top.hbar2.spacer -width [$top.mesg2.yscroll cget -width] \
      -image blank -relief flat -borderwidth 0 -state disabled

  text $top.mesg2.txt -yscroll "$top.mesg2.yscroll set" \
      -relief sunken -bd 2 -wrap none -xscroll "$top.hbar2.xscroll set"
  $top.mesg2.txt configure -font [lindex [$top.$mfp(mesg) configure -font] 4]
  
  # configure pane handle below summary list
  frame $top.sep1 -relief flat -height 11 -bd 1
  frame $top.line1 -relief sunken -height 2 -bd 1
  frame $top.handle1 -relief raised  -bd 2 -cursor crosshair \
      -width 9 -height 9

  bind $top.handle1 <ButtonPress-1> "mfv:sash-begin $top $top.$mfp(head) 1"
  bind $top.handle1 <B1-Motion> "mfv:sash-draw $top %Y 1"
  bind $top.handle1 <ButtonRelease-1> "mfv:sash-end $top"

  place configure $top.line1 -in $top.sep1 -relx 0.03 -rely 0.4 \
      -relwidth 0.95
  place configure $top.handle1 -in $top.sep1 -relx 0.8 -rely 0.4 \
      -anchor center

  pack $top.menu $top.stat $top.head $top.bb $top.sep1 -side top -fill x
  if $mfp($top,horiz) {
    pack $top.hbar -side bottom -fill x
    $top.mesg.txt configure -wrap none
  } else {
    $top.mesg.txt configure -wrap word
  }
  pack $top.mesg -side bottom -expand true -fill both

  # configure pane handle between text widgets
  frame $top.sep2 -relief flat -height 11 -bd 1
  frame $top.line2 -relief sunken -height 2 -bd 1
  frame $top.handle2 -relief raised  -bd 2 -cursor crosshair \
      -width 9 -height 9

  bind $top.handle2 <ButtonPress-1> "mfv:sash-begin $top $top.$mfp(mesg) 2"
  bind $top.handle2 <B1-Motion> "mfv:sash-draw $top %Y 2"
  bind $top.handle2 <ButtonRelease-1> "mfv:sash-split-end $top"

  # configure widgets to user settings
  $top.$mfp(head) configure -height $mf(headlist-height)
  eval "$top.$mfp(mesg) tag configure headers $mf(header-config)"
  
  if {$mf(disp-left-scroll)} { set sside left } { set sside right }

  pack $top.head.yscroll -side $sside -fill y
  pack $top.$mfp(head) -side left -expand true -fill both
  pack $top.mesg.yscroll -side $sside -fill y
  pack $top.mesg.txt -side left -expand true -fill both
  pack $top.hbar.spacer -side $sside
  pack $top.hbar.xscroll -side $sside -expand true -fill x
  pack $top.mesg2.yscroll -side $sside -fill y
  pack $top.mesg2.txt -side left -expand true -fill both
  pack $top.hbar2.spacer -side $sside
  pack $top.hbar2.xscroll -side $sside -expand true -fill x

  pdebug "  Scroll\n"
  
  
  # build folder list menus
  mfv:build-folder-menus $top
  
  # source users mfv:viewer-hook procedure if defined
  mfv:run-hook mfv:viewer-hook $top

  lappend mfp(waitlist) $top.$mfp(head) $top.$mfp(mesg)
  set mfp($top.$mfp(head),cursor) {}
  set mfp($top.$mfp(mesg),cursor) {}

  wm geometry $top [string trim $mf(viewer-geom)]

  if {[file exists $mf(viewer-bitmap-nomail)]} {
    wm iconbitmap $top "@$mf(viewer-bitmap-nomail)"
  }
  if {$iconic} {
    wm iconify $top
  } else {
    wm deiconify $top; raise $top
  }
  focus $top.mesg.txt
  update idletasks

  pdebug "   Folder setup\n"
  mfv:setup-folder $top $filename $msgndx

  return $top
}

proc mfv:init-bindings { } {
  # Initialize viewers in general. Called once.
  global mf mfp env tkBind ut_glob
  
  if {[info proc tkBindDefVar]==""} { 
    foreach key [bind Text] { bind Text $key {} }
    foreach key [bind Entry] { bind Entry $key {} }
    source $mfp(bindlib)/bindxtnd.tcl
    source $mfp(bindlib)/text.tcl 
    source $mfp(bindlib)/entry.tcl
  }

  if $tkBind(emacs) {
    if {$tkBind(useEsc)} {
      set mfp(cancel) Control-
      set ut_glob(cancel) Control-
    }
    bind Text <Control-Shift-Y> "mfv:prefix-sel %W emacs"
    bind Text <Shift-Control-S> "mfv:search-prompt %W"
    bind Text <Control-s> "mfv:search %W \[winfo toplevel %W\]_search"
  } else {
    bind Text <Control-Shift-V> "mfv:prefix-sel %W emacs"
    bind Text <Control-f> "mfv:search-prompt %W"
    bind Text <Control-g> "mfv:search %W \[winfo toplevel %W\]_search"
  }  
  bind Text <Shift-$tkBind(meta)-ButtonRelease-2> "mfv:prefix-sel %W xsel; break"
  
  proc mfv:menu-key { k } {global mf; return <Mod4-$k>}

  if {$tkBind(emacs) && ($tkBind(meta) == "Meta" || $tkBind(meta) == "Alt")} {
    if {[string range $mf(bind-alt-key) 0 0] == "<"} {
      proc mfv:bind-menu-key {w k cmd} {
	global mf
	bind $w $mf(bind-alt-key) {tkBindSetStateKey %W Menu%W M-; break}
	bind Menu$w <$k> $cmd
      }
    } else {
      proc mfv:bind-menu-key { w k cmd} {
	global mf; bind $w <$mf(bind-alt-key)-$k> $cmd
      }
    }
  } else {
    proc mfv:bind-menu-key {w k cmd} {bind $w <Meta-$k> $cmd}
  }
  set ut_glob(key-hook) mfv:bind-menu-key

  # source users mfv:bind-hook procedure if defined
  mfv:run-hook mfv:bind-hook

  # set binding options
  set tkBind(bell) $mf(viewer-beep-error)

}

proc mfv:tkmail-init { } {
  # called by server once ~/.tkmailrc is sourced to initialize WISH side
  global mf mfp
  
  set mfp(inbox) [mfv_util fullpath $mf(mail-system)]
  set mfp(mbox) [mfv_util fullpath $mf(mail-mbox)]
  if ![samefile $mfp(inbox) $mfp(mbox)] {
    set mfp(mbox-model) 1
  }

  # make default bindings
  mfv:init-bindings

  # create temp text widget for use
  text $mfp(tmptxt)
  text $mfp(savesendtxt)

  # transfer appropriate options to MFV
  mfv_set sumformat "%1S%3n $mf(headlist-format)"
  if [string length $mf(header-retain)] {
    mfv_set retain $mf(header-retain)
  } else {
    mfv_set strip $mf(header-strip)
  }
  if [catch {mfv_set tmpdir $mf(mail-tmpdir)} res] {
    puts stderr "Error setting tmpdir to $mf(mail-tmpdir). $res"
    mfv:quit
  }
  mfv_set noempty $mf(mail-remove-empty)
  mfv_set lock dotlock $mfp(dotlock)

  # Check that tmpdir is writable by user
  if {![file writable $mf(mail-tmpdir)]} {
    puts stderr "FATAL: temporary directory $mf(mail-tmpdir) not writable!"
    puts stderr "	You can change this with mf(mail-tmpdir) in your resource file."
    mfv:quit
  }

  if {![string length $mf(bind-alt-key)]} {
    puts stderr "WARNING: empty mf(bind-alt-key)! Setting to <Control-c>."
    set mf(bind-alt-key) <Control-c>
  }

  mfv:parse-alias-file

  if ![llength $mfp(firstfile)] { set mfp(firstfile) $mfp(mbox) }
  pdebug "Starting with folder $mfp(firstfile)\n"

  pdebug "Viewer initializing ...\n"
  set inboxopened 0
  foreach file $mfp(firstfile) {
    mfv:new-viewer $file $mfp(starticonic)
    mfv:add-recent $file
    set inboxopened [samefile $mfp(inbox) $file]
  }

  mfv:checklist-add $mfp(inbox)
  if $mfp(mbox-model) { mfv:checklist-add $mfp(mbox) }
  mfv:openlist-add $mfp(mbox)

  # if inbox wasn't opened, check if it has real new mail
  if !$inboxopened {
    set tfid [mfv_open $mfp(inbox)]
    if {[llength [$tfid info new]] > 0} {
      eval $mf(viewer-beep-new)
      mfv:log-mesg {} "New mail messages present in InBox!" 1
      mfv:newlist-add $mfp(inbox)
      if $mfp(mbox-model) {
	if [string length [set mfid [mfv_util folderid $mfp(mbox)]]] {
	  if [keylget mfp($mfid) viewers vlist] {
	    foreach viewer $vlist {
	      catch {wm iconbitmap $viewer "@$mf(viewer-bitmap-mail)"}
	    }
	  }
	}
      }
    }
    mfv_close $tfid
  }

  # check if person thought it was for seconds and not milliseconds
  if {$mf(mail-interval) < 1000} {set mf(mail-interval) [expr $mf(mail-interval)*1000]}
  after $mf(mail-interval) mfv:check-new-mail
  if {$mf(mail-autosave) < 1000} {set mf(mail-autosave) [expr $mf(mail-autosave)*1000]}
  if {$mf(mail-autosave) > 0} { after $mf(mail-autosave) "mfv:autosave" }

  if [file isdirectory $mf(mail-directory)] {
    cd $mf(mail-directory)
  }
  mfv:check-old-settings
}

proc mfv:close-all { } {
  global mf mfp mf_autofile

  set stat 0
  set mfp(last-error) {}

  foreach mfc [mfv:compose-list-tops] {
    if $mfp(noask) {
      if [scan $mfc ".mfc%d" num] {
	catch {mfv:text-to-file $mfc.comp.txt $env(HOME)/letter$mfc 1}
      }
    } else {
      eval $mf(viewer-beep-error)
      if ![ut:getok -prompt "Cancel composition to \"[$mfc.to.ent get]\"" \
	       -title "WARNING!" -bitmap warning -oklabel Yes \
	       -nolabel No -master [mfv:current-top]] {
	return 2
      }
    }
  }

  foreach fid [mfv_util list] {
    if {[llength [$fid info modified]]} {
      if [catch "$fid save" res] {
	append mfp(last-error) "\n$res\n"
	set stat 1
      } else {
	if [info exists mf_autofile($fid)] {
	  exec rm -rf $mf_autofile($fid)
	  unset mf_autofile($fid)
	}
      }
    }
    if [catch "mfv_close $fid" res] {
      append mfp(last-error) $res\n\n
      set stat 1
    }
  }

  return $stat
}

proc mfv:quit { {force 0} } { 
  global mfp 
  if $force { set mfp(noask) 1 }
  set stat [mfv:close-all]
  if {$stat == 2 && !$force} {return 1}
  if {$stat} {
    if $force {
      if [info exists mfp(trap-fid)] {
	puts $mfp(trap-fid) $mfp(last-error)
      } else {
	puts stderr $mfp(last-error)
      }
    } else {
      mfv:error-mesg $mfp(last-error)
    }
  } else {
    if [info exists mfp(trap-fid)] {
      puts $mfp(trap-fid) "No, should be fine."
      close $mfp(trap-fid)
      catch {exec rm -f $mfp(homedir)/tkmail.CRASH}
    }
  }
  exit 
}

proc mfv:trap-signal { signal } {
  global mfp
  
  set buttons [list Continue]
  # lappend buttons [list Error mfv:signal-error %W]
  lappend buttons [list Quit after idle mfv:quit 1]
  lappend buttons [list Kill exit]

  set w [ut:simpletext -title "Caught $signal signal" \
             -buttons $buttons -grab 1 -text "
Caught a $signal signal. You may:

        Continue        Ignore signal
        Quit            Try to cleanly quit tkmail
        Kill            Kill tkmail"]

  focus $w.txt
  tkwait window $w
}

proc mfv:term-signal { signal } {
  global mfp
  set mfp(trap-exit) 1
  set mfp(trap-fid) [open $mfp(homedir)/tkmail.CRASH w]
  if [catch {puts $mfp(trap-fid) "TkMail: caught $signal ... trying to quit cleanly, but if"}] {
    unset mfp(trap-fid)
    puts stderr "TkMail: caught $signal ... trying to quit cleanly"
  } else {
    puts $mfp(trap-fid) "this file exists, something might have gone wrong.\n\n"
  }
  after idle mfv:quit 1
}

if {[lsearch [info commands] signal] > -1} {
  signal trap SIGINT {mfv:term-signal %S}
  signal trap SIGTERM {mfv:term-signal %S}
  signal trap SIGQUIT {mfv:term-signal %S}
  signal trap SIGHUP {mfv:term-signal %S}
  signal error SIGUSR1
  signal trap SIGUSR2 {mfv:trap-signal %S}
  signal ignore SIGALRM
}


if {$mfp(debug)} {puts stderr "VIEWER: viewer.tcl has been loaded"}

# Local Variables: ***
# mode:tcl ***
# End: ***
