#
# pgp.tcl
#	PGP 2.6 support for exmh.
#	Orginally contributed by Allan Downey
#	Updated by Stefan Monnier, Anders Klemets and William Sproule
#

# future:
# - somehow arrange for "insert @" to use the decoded part...
# - rewrite PGP decrypt, to deal with keys, mime stuff, etc...
# - encrypt pgp parts, similar to the automatic quote-printable
# - split big functions
# - drop misc_displaytext
# - add a "init PGP" command
# - add some key handling commands
# - keep track of who has your private key (for revocation purposes)

# Pgp_Init is in extrasInit.tcl
# to avoid auto-loading this whole file.

# $Log: pgp.tcl,v $
# Revision 1.6  1996/03/22  18:44:34  bwelch
# Changed graphic part separator size in PGP from 5 to 6 to avoid
#     downloading a new font for this case.
#
# Revision 1.5  1995/09/28  04:11:10  bwelch
# Fixed "hasfcc" check in PGP.
#
# Revision 1.4  1995/06/30  18:32:40  bwelch
# Upcase PGP mail headers
#
# Revision 1.3  1995/06/09  20:57:06  bwelch
# Added ChoosePrivateKey
#
# Revision 1.2  1995/05/24  06:01:38  bwelch
# Added Pgp_SetMyName to choose private key name
#
# Revision 1.1  1995/05/24  01:48:03  bwelch
# Initial revision
#
# Revision 1.21  1995/04/15  18:17:01  welch
# Introduced msg(path)
#
# Revision 1.20  1995/03/22  22:17:30  welch
# Changed exmh.PGP.help to help.PGP
#
# Revision 1.19  1995/03/22  18:53:52  welch
# More new code from Stefan
#
# Revision 1.1  1994/12/17  20:18:49  monnier
# Initial revision
#

proc Pgp_Setup {  } {
    global pgp env

    if {[info exists pgp(path)] && \
	    ([string length [string trim $pgp(path)]] > 0) && \
	    ([lsearch -exact [split $env(PATH) :] $pgp(path)] < 0)} {
	set env(PATH) $pgp(path):$env(PATH)
    }

    foreach path [split $env(PATH) ":"] {
	if [file executable "$path/pgp"] {
	    set pgpexec "$path/pgp"
	}
    }
    
    if {![info exists pgpexec]} {
	Misc_DisplayText "PGP Setup" \
"The PGP executable is not in your PATH.
You'll have to find it (or install it) before
I can do anything for you."
        return
    }

    # setup the directory
    set tmpfile [Mime_TempFile "pgp"]
    if {![info exists env(PGPPATH)]} {
	set env(PGPPATH) "$env(HOME)/.pgp"
    }
    catch { exec mkdir $env(PGPPATH) }
    exec touch "$env(PGPPATH)/config.txt"

    # make the key pair and self sign it
    exec xterm -title "PGP Setup" -e sh -c {
	cd ${PGPPATH}
	rm -f pubring.bak
	pgp -kg
	rm -f pubring.bak
	pgp +verbose=0 +force=on -ks "" -u ""
    }

    if {![file exists "$env(PGPPATH)/pubring.bak"]} {
	return
    }

    # init the pgp support if necessary
    if {! $pgp(enabled)} {
	Pgp_Init
    } else {
	set pgp(secring) $pgp(pgppath)/secring.pgp
	set pgp(privatekeys) [PgpExec_KeyList "" $pgp(secring)]
    }

    # send the key to the keyservers
    PgpExec_GetKeys [lindex [lindex $pgp(privatekeys) 0] 0] $tmpfile
    Misc_Send $pgp(keyserver) ADD $tmpfile "content-type: application/pgp; format=keys-only"
    exec rm -f $tmpfile
}

proc Pgp_Help {  } {
    global exmh

    set label "Help about setting up PGP"
    if [Exwin_Toplevel .pgphelp "PGP Help" PgpHelp] {
	Widget_Label .pgphelp.but label {left fill} -text $label
	Widget_AddBut .pgphelp.but setup "Make Key" [list Pgp_Setup]
    
	set t [Widget_Text .pgphelp 30 -setgrid true]
	Ftoc_ColorConfigure $t
	$t insert insert "EXMH Version: $exmh(version)\n\n"
	if [catch {open "$exmh(library)/help.PGP" r} in] {
	    $t insert insert "Cannot find file exmh.PGP.help to display"
	    $t configure -state disabled
	} else {
	    $t insert insert [read $in]
	}
    }
}

# display the message without keeping the decrypted form
proc Pgp_View { {file {}} } {
    global exmh msg exwin pgp

    if {$file == {}} { set file $msg(path) }

    if $pgp(keeppass) {
	PgpExec [list -m $file] message $pgp(myname)
	Misc_DisplayText "PGP view $exmh(folder)/$msg(id)" $message 40
    } else {
	PgpExec [list -m $file] message $pgp(myname)
    }

    return 1
}

# decrypt the current message
proc Pgp_Decrypt {  } {
    global exmh msg mhProfile exwin pgp

    set file $msg(path)
    set pgpfile [Mime_TempFile "decrypt"]

    Exmh_Status "pgp $exmh(folder)/$msg(id)"
    PgpExec [list $file -o $pgpfile] message $pgp(myname)
    if {$message != {}} {
	Misc_DisplayText "PGP decrypt $exmh(folder)/$msg(id)" $message
    }
    set t $exwin(mtext)

    set orig [open $file r]
    if [catch {
	set mess [open $pgpfile r]
    } err] {
	return 0
    }
    set outfile [Mime_TempFile "decypted"]
    set comb [open $outfile w 0600]

    while {[gets $orig line] != -1} {
	if {[regexp {^-+BEGIN PGP MESSAGE} $line]} {
	    puts -nonewline $comb [read $mess]
	    while {[gets $orig line] != -1} {
		if {[regexp {^-+END PGP MESSAGE} $line]} { break }
	    }
	} else {
	    puts $comb $line
	}
    }
    close $orig
    close $comb
    close $mess

    Mh_Rename $outfile $file
    exec rm -f $pgpfile

    set msg(dpy) {}
    MsgChange $msg(id)

    return 1
}

# encrypts the current message with the user's key
proc Pgp_ExmhEncrypt {  } {
    global exmh msg mhProfile pgp

    set file $msg(path)
    set tmpfile [Mime_TempFile "encrypt"]

    Exmh_Status "pgp -e $exmh(folder)/$msg(id)"

    Pgp_Process $file $tmpfile "pgp-action: encrypt; recipients=[lindex $pgp(myname) 0]"
    Mh_Rename $tmpfile $file

    set msg(dpy) {}
    MsgChange $msg(id)

    return 1
}

# remove any pgp-action header and puts a brand new one
proc Pgp_SeditEncrypt { action draft t } {
    global miscRE pgp

    SeditSave $draft $t

    set linenb 1
    set line [$t get $linenb.0 $linenb.end]
    set hasfcc 0

    while {![regexp $miscRE(headerend) $line]} {
	if [regexp -nocase {^pgp-action:} $line] {
	    set line " dummy"
	    while {[regexp "^\[ \t]" $line]} {
		$t delete $linenb.0 [expr {$linenb + 1}].0
		set line [$t get $linenb.0 $linenb.end]
	    }
	} else {
	    if [regexp -nocase {^fcc:} $line] {
		set hasfcc 1
	    }
	    set linenb [expr {$linenb + 1}]
	}
	set line [$t get $linenb.0 $linenb.end]
    }
    if {! $pgp(enabled)} {
	SeditMsg $t "PGP Not enabled"
	return
    }
    set pgpaction "Pgp-Action: $action"
    if $pgp(rfc822) {
	append pgpaction "; rfc822=on"
    } else {
	append pgpaction "; rfc822=off"
    }
    if [regexp {sign} $action] {
       if {$pgp(choosekey) && [llength $pgp(privatekeys)] > 1} {
	  set signkey [Pgp_ChoosePrivateKey \
		"Please select the key to use for signing"]
       } else {
	  set signkey $pgp(myname)
       }
       append pgpaction ";\n\toriginator=\"[lindex $signkey 1]\""
    }
    if [regexp {encrypt} $action] {
	catch {
	    append pgpaction ";\n\trecipients=\"[join [Misc_Map key {lindex $key 1} [PgpMatch_Whom $draft $hasfcc]] ",\n\t\t    "]\""
	}
    }
    $t insert 1.0 "$pgpaction\n"
}

proc Pgp_ChoosePrivateKey { text } {
   global pgp

   set signkeys {}
   while {[llength $signkeys] != 1} {
      if [catch {Pgp_KeyBox $text $pgp(secring) $pgp(privatekeys)} signkeys] {
	 set signkeys [list $pgp(myname)]
      }
   }
   return [lindex $signkeys 0]
}

proc Pgp_SetMyName {} {
   global pgp

   set pgp(myname) [Pgp_ChoosePrivateKey \
	 "Please select the default key to use for signing"]
}

#
proc Pgp_Process { srcfile dstfile {pgpaction {}} } {
    global pgp env miscRE

    set orig [open $srcfile r]

    # get the header of the draft and split it into mime and non-mime headers
    set mailheaders [Misc_Segregate line \
	  {[regexp $miscRE(mimeheaders) $line]} [Misc_GetHeader $orig]]
    set mimeheaders [lindex $mailheaders 0]
    set mailheaders [lindex $mailheaders 1]
    set hasfcc [expr {[lsearch -glob $mailheaders "fcc:*"] >= 0}]

    # add the default pgp-action header and mime headers
    if {$pgpaction != {}} {
	set mailheaders [linsert $mailheaders 0 $pgpaction]
    } else {
	lappend mailheaders "pgp-action: none"
    }
    if {[lsearch -glob $mimeheaders "content-type:*"] < 0} {
	lappend mimeheaders "content-type: text/plain; charset=us-ascii"
    }

    # get the pgp-action header and parse it
    set pgpaction [split [lindex $mailheaders [lsearch -glob $mailheaders "pgp-action:*"]] ";"]
    set action [string trim [string tolower [lindex [split [lindex $pgpaction 0] ":"] 1]]]
    set actionparams [lrange $pgpaction 1 end]

    # if there is nothing to do, stop here
    if {"$action" == "none"} {
	close $orig
	error "no action"
    }

    # parse pgp-action parameters
    foreach param $actionparams {
	if [regexp {^([^=]+)=(.*)$} $param {} name value] {
	    set actionparam([string tolower [string trim $name]]) [string trim $value " \t\""]
	} else {
	    error "<PGP> incorrect syntax: $param"
	}
    }

    # setup default params
    set typeparams "; x-action=$action"
    
    # setup rfc822
    if [info exists actionparam(rfc822)] {
	set rfc822 [regexp -nocase $miscRE(true) $actionparam(rfc822)]
    } else {
	set rfc822 $pgp(rfc822)
    }

    # setup the originator (if necessary)
    if [regexp {sign} $action] {
	if [info exists actionparam(originator)] {
	    set originator [PgpMatch_Simple $actionparam(originator) $pgp(secring)]
	} else {
	    set originator $pgp(myname)
	}
	append typeparams "; x-originator=[string range [lindex $originator 0] 2 end]"
    }

    # get the ids of the recipients (if necessary)
    if [regexp {encrypt} $action] {
	if [info exists actionparam(recipients)] {
	    set ids [Misc_Map id {PgpMatch_Simple $id $pgp(pubring)} \
		    [split $actionparam(recipients) ","]]
	} else {
	    set ids [PgpMatch_Whom $srcfile $hasfcc]
	}
	ExmhLog "<Pgp_Process> Encrypting with public key(s): [join $ids ", "]"

	append typeparams ";\n\tx-recipients=\"[join [Misc_Map key {string range [lindex $key 0] 2 end} $ids] ", "]\""
    }
    
    # remove pgp-action and mime-version headers
    set mailheaders [Misc_Filter line {![regexp "^(mime-version|pgp-action):" $line]} $mailheaders]

    # setup the header of the application/pgp subpart
    if $rfc822 {
	set pgpheaders [concat \
		[list "content-type: message/rfc822" ""] \
		[Misc_Filter line {![string match {[bf]cc:*} $line]} $mailheaders] \
		[list "mime-version: 1.0"] \
		$mimeheaders]
    } else {
	set pgpheaders $mimeheaders
    }

    # write the message to be enrypted
    set msgfile [Mime_TempFile "msg"]
    set msg [open $msgfile w 0600]
    foreach line $pgpheaders { puts $msg [Pgp_FixHeader $line] }
    puts $msg ""
    puts -nonewline $msg [read $orig]
    close $orig
    close $msg

    set pgpfile [Mime_TempFile "pgp"]
    if [catch {
	switch -exact $action {
	    encrypt	{ PgpExec_Encrypt $msgfile $pgpfile $ids }
	    signbinary	{ PgpExec_Sign $msgfile $pgpfile $originator 0 }
	    signclear	{ PgpExec_Sign $msgfile $pgpfile $originator 1 }
	    encryptsign	{ PgpExec_EncryptSign $msgfile $pgpfile $originator $ids }
	    default	{ error "<PGP> unknown action $action" }
	}
    } err] {
	exec rm -f $msgfile
	error $err
    }

    # complete mailheaders with the applcation/pgp content-type
    if {[info exists actionparam(localaction)] && \
	    [regexp -nocase $miscRE(true) $actionparam(localaction)]} { 
	set mailheaders {}
    }
    lappend mailheaders \
	    "mime-version: 1.0" \
	    "content-type: application/pgp; format=mime$typeparams" \
	    "content-transfer-encoding: 7bit"

    # write out the new mail file
    set dst [open $dstfile w 0600]
    foreach line $mailheaders { puts $dst [Pgp_FixHeader $line] }
    puts $dst ""
    set msg [open $pgpfile r]
    puts -nonewline $dst [read $msg]
    close $msg
    close $dst

    exec rm -f $msgfile $pgpfile
}

proc Pgp_ShowMessage { tkw part } {
    global mimeHdr mime pgp miscRE

    set in [open $mimeHdr($part,file) r]
    gets $in firstLine
    close $in

    # let's get the format
    if {![info exists mimeHdr($part,param,format)]} {
	lappend mimeHdr($part,params) format
	if [regexp $miscRE(beginpgpkeys) $firstLine] {
	    set mimeHdr($part,param,format) keys-only
	} else {
	    set mimeHdr($part,param,format) text
	}
    }
    set format $mimeHdr($part,param,format)

    # the action pgp performed
    if {"$format" != "keys-only"} {
	if {![info exists mimeHdr($part,param,x-action)]} {
	    if [regexp $miscRE(beginpgpclear) $firstLine] {
		set action signclear
	    } else {
		set action encryptsign
	    }
	} else {
	    set action $mimeHdr($part,param,x-action)
	}
    } else {
	set action "keys-only"
    }

    # short cut if you don't have PGP at all
    if {!$pgp(enabled) && ("$action" != "signclear")} {
	Mime_ShowDefault $tkw $part
	return
    }

    # get the recipients if necessary
    if [regexp {encrypt} $action] {
	if {![info exists mimeHdr($part,param,x-recipients)]} {
	    set recipients [string range [lindex $pgp(myname) 0] 2 end]
	} else {
	    set recipients $mimeHdr($part,param,x-recipients)
	}
	set recipients [Misc_Map elem {string trim $elem} [split $recipients ","]]
    }

    # see if we should decode the thing
    if {![info exists mimeHdr($part,pgpdecode)]} {
	set mimeHdr($part,pgpdecode) [expr {$pgp(enabled) && [expr $pgp(decode,$pgp(showinline))]}]
	if $pgp(enabled) {
	    MimeMenuAdd $part checkbutton \
		    -label "$pgp(menutext,$action) with pgp..." \
		    -command [list busy MimeRedisplayPart $tkw $part] \
		    -variable mimeHdr($part,pgpdecode)
	}
    }
    
    if {($format == "mime") || ($format == "text")} {
	if $mimeHdr($part,pgpdecode) {
	    set tmpfile [Mime_TempFile "decrypt"]
	    
	    if [regexp "encrypt" $action] {
		PgpExec_Decrypt $mimeHdr($part,file) $tmpfile msg $recipients
	    } else {
		PgpExec [list $mimeHdr($part,file) -o $tmpfile] msg
	    }
	    
	    set msg [string trim $msg " \t\n"]
	    if {![file exists $tmpfile]} {
		set msg "PGP failed to decrypt the file:\n$msg"
	    }
	    if {[regexp {sign} $action] && \
		    [regexp -nocase {key id ([0-9a-f]+) } $msg {} keyid]} {
		MimeMenuAdd $part command \
			-label "Query keyserver for key $keyid" \
			-command "Misc_Send {$pgp(keyserver)} {GET 0x$keyid}"
		TextButton $tkw "Query keyserver" "Misc_Send {$pgp(keyserver)} {GET 0x$keyid}"
		$tkw insert insert "\n"
	    }
	    if {($msg != {}) && ($msg != "child process exited abnormally")} {
		$tkw insert insert "$msg\n"
		MimeInsertSeparator $tkw $part 6
	    }
	    if [catch {open $tmpfile r} fileIO] {
		return
	    }
	    exec rm -f $tmpfile
	    if {$format == "mime"} {
		if {![info exists mimeHdr($part,numParts)]} {
		    set mimeHdr($part,numParts) [MimeParseSingle $tkw $part $fileIO]
		    set mimeHdr($part=1,color) [MimeDarkerColor $tkw $mimeHdr($part,color)]
		}
		MimeShowPart $tkw $part=1 [MimeLabel $part part] 1
	    } else {
		$tkw insert insert "[read $fileIO]"
	    }
	    MimeClose $fileIO
	} else {
	    if {$action == "signclear"} {
		$tkw insert insert "  the signature hasn't been checked "
		if $pgp(enabled) {
		    TextButton $tkw "check sig" [list $mimeHdr($part,menu) invoke "$pgp(menutext,$action) with pgp..."]
		}
		$tkw insert insert "\n"
		MimeInsertSeparator $tkw $part 6
		
		if [catch {Pgp_Unsign [Misc_FileString $mimeHdr($part,file)]} msg] {
		    $tkw insert insert "  can't find the signed message.\nPlease check it out: it might be suspicious !\n"
		    return
		}
		if {$format == "mime"} {
		    set tmpfile "$mimeHdr($part,file).msg"
		    Misc_StringFile $msg $tmpfile
		    set fileIO [open $tmpfile r]
		    exec rm -f $tmpfile
		    if {![info exists mimeHdr($part,numParts)]} {
			set mimeHdr($part,numParts) [MimeParseSingle $tkw $part $fileIO]
			set mimeHdr($part=1,color) [MimeDarkerColor $tkw $mimeHdr($part,color)]
		    }
		    MimeShowPart $tkw $part=1 [MimeLabel $part part] 1
		    MimeClose $fileIO
		} else {
		    $tkw insert insert $msg
		}
	    } else {
		Mime_ShowDefault $tkw $part
	    }
	}
    } elseif {$format == "keys-only"} {
	if $pgp(autoextract) {
	    PgpExec_ExtractKeys $mimeHdr($part,file) 0
	} else {
	    MimeMenuAdd $part command \
		    -label "Extract keys into keyring..." \
		    -command "PgpExec_ExtractKeys $mimeHdr($part,file)"
	    TextButton $tkw "Extract keys" "PgpExec_ExtractKeys $mimeHdr($part,file)"
	    $tkw insert insert "\n"
	}
	if $mimeHdr($part,pgpdecode) {
	    PgpExec [list $mimeHdr($part,file)] msg
	    regexp "\n(Type.*\n(sig|pub|sec)\[^\n]*)" $msg {} msg
	    $tkw insert insert "$msg\n"
	} else {
	    Mime_ShowDefault $tkw $part
	}
    } else {
	$tkw insert insert "PGP application format '$format' unknown\n"
	return
    }
}

#
proc Pgp_InsertKeys { draft t } {
    global env pgp

    if [catch {Pgp_KeyBox "select the keys you want to send" $pgp(pubring) [Pgp_FlatKeyList "" $pgp(secring)]} keys] {
	SeditMsg $t $keys
	return
    }

    foreach key $keys {
	set keyid [lindex $key 0]
	if {![info exists done($keyid)]} {
	    set done($keyid) 1
	    set tmpfile [Mime_TempFile "insertkeys"]
	    if [catch {PgpExec_GetKeys $keyid $tmpfile} msg] {
		SeditMsg $t "Pgp refuses to generate the key message"
		ExmhLog $msg
		return
	    }
	    
	    SeditInsertFile $draft $t $tmpfile 1 7bit {application/pgp; format=keys-only} "keys of [lindex $key 1]"
	    exec rm -f $tmpfile
	}
    }
}
