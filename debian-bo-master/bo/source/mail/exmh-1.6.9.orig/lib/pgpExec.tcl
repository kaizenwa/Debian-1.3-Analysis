# pgpInterface.tcl -- 
# created by monnier@didec26.epfl.ch on Mon Dec 12 17:34:38 1994

# 
# 
# 

# $Log: pgpExec.tcl,v $
# Revision 1.3  1996/03/22  18:42:54  bwelch
# Added Mh_Rename
# .
#
# Revision 1.2  1995/05/24  05:58:04  bwelch
# Updates from Stefan
#
# Revision 1.1  1995/05/19  17:36:16  bwelch
# Initial revision
#
# Revision 1.2  1995/03/22  19:14:21  welch
# More new code from Stefan
#
# Revision 1.1  1994/12/30  21:49:00  welch
# Initial revision
#
# Revision 1.1  1994/12/17  20:19:16  monnier
# Initial revision
#

# execs pgp with the usual flags
proc PgpExec { arglist outvar {privatekey {}} {interactive 0} } {
    upvar $outvar output
    global pgp env

    #puts "<PgpExec> $arglist $outvar $privatekey $interactive"
    
    if {! $pgp(enabled)} {
	error "<PGP> pgp isn't enabled"
    }
    
    if {!$pgp(keeppass)} {
	global pgpPass
	catch { unset pgpPass }
	set pgpPass() {}
    }

    if {$interactive || !($pgp(keeppass) || ($privatekey == {}))} {
	return [PgpExec_Interactive $arglist output]
    } else {
	if {$privatekey == {}} {
	    return [PgpExec_Batch $arglist output]
	} else {
	    return [PgpExec_Batch $arglist output [Pgp_GetPass $privatekey]]
	}
    }
}

#
proc PgpExec_Batch { arglist outvar {password {}} } {
    upvar $outvar output
    global pgp env

    set tclcmd [concat \
	    [list exec pgp +armorlines=0 +keepbinary=off +batchmode=on +verbose=0 +pager=cat] \
	    $arglist]
    if {$password == {}} {
	catch { unset env(PGPPASSFD) }
    } else {
	lappend tclcmd << $password
	set env(PGPPASSFD) 0
    }
    set result [catch $tclcmd output]
    regsub -all "\x07" $output "" output

    catch { unset env(PGPPASSFD) }

    return $result
}

#
proc PgpExec_Interactive { arglist outvar } {
    upvar $outvar output
    
    set args [concat [list +armorlines=0 +keepbinary=off] $arglist]
    set shcmd "unset PGPPASSFD;
        pgp \"[join [Misc_Map x {
	    regsub {([$"\`])} $x {\\1} x
	    set dummy $x
        } $args] {" "}]\";
	echo
	echo press Return...;
        read dummy"

    set logfile [Mime_TempFile "xterm"]
    set tclcmd {exec xterm -l -lf $logfile -title PGP -e sh -c $shcmd}
    set result [catch $tclcmd]
    if [catch {open $logfile r} log] {
	set output {}
    } else {
	set output [read $log]
	close $log
    }

    # clean up the output
    regsub -all "\[\x0d\x07]" $output {} output
    regsub "^.*\nEnter pass phrase:" $output {} output
    regsub "\nPlaintext filename:.*" $output {} output
    regsub "^.*Just a moment\\.\\.+" $output {} output
    regsub "^.*Public key is required \[^\n]*\n" $output {} output
    set output [string trim $output]

    return $result
}

#
proc PgpExec_CheckPassword { password key } {

    set tmpfile [Mime_TempFile "pwdin"]
    set outfile [Mime_TempFile "pwdout"]

    set out [open $tmpfile w 0600]
    puts $out "salut"
    close $out
    
    PgpExec_Batch [list -as $tmpfile -o $outfile -u [lindex $key 0]] err $password
    exec rm -f $tmpfile

    # pgp thinks he knows better how to name files !
    if {![file exists $outfile] && [file exists "$outfile.asc"]} {
	Mh_Rename "$outfile.asc" $outfile
    }
    if {![file exists $outfile]} {
	if {![regexp "PGP" $err]} {
	    # Probably cannot find pgp to execute.
	    Exmh_Status !${err}!
	    error "<PGP> can't find pgp"
	} else {
	    if [regexp {(Error:[^\.]*)\.} $err x match] {
		Exmh_Status ?${match}?
	    }
	    ExmhLog "<Pgp_GetPass> $err"
	}
	return 0
    } else {
	exec rm -f $outfile
	return 1
    }
}

# wrapper for 'pgp -kv $pattern $keyring'
# returns a list of keys. Each "key" is a list whose first element is the keyID
# and the next ones are the corresponding userids
proc PgpExec_KeyList { pattern keyring } {

    set pattern [string trimleft $pattern "<>|2"]
    PgpExec_Batch [list -kv $pattern $keyring] keylist

    # drop the revoked keys
    regsub -all "\n(pub|sec) \[^\n]+\\*\\*\\* KEY REVOKED \\*\\*\\*(\n\[\t ]\[^\n]+)+" $keylist "" keylist

    if {! [regexp "\n((pub|sec) .*)\n\[0-9]+" $keylist {} keylist]} {
	return {}
    } else {
	set keylist [split $keylist "\n"]
	set keys {}
	set key {}
	foreach line $keylist {
	    if [regexp {^(pub|sec) +[0-9]+/([0-9A-F]+) [0-9/]+ (.*)$} $line {} {} keyid userid] {
		lappend keys $key
		set key [list "0x$keyid" [string trim $userid]]
	    } else {
		lappend key [string trim $line]
	    }
	}
	lappend keys $key
	return [lrange $keys 1 end]
    }
}

#
proc PgpExec_Init {  } {
    global pgp pgpConfig pgpPass env

    if {[info exists pgp(path)] && \
	    ([string length [string trim $pgp(path)]] > 0) && \
	    ([lsearch -exact [split $env(PATH) :] $pgp(path)] < 0)} {
	set env(PATH) $env(PATH):$pgp(path)
    }

    if {![info exists env(LOCALHOST)]} {
	if [catch {exec uname -n} env(LOCALHOST)] {
	    set env(LOCALHOST) localhost
	}
    }

    set pgpPass() {}

    PgpExec_ParseConfigTxt $pgp(pgppath)/config.txt pgpConfig
    
    set pgp(secring) $pgp(pgppath)/secring.pgp
    if {![file exists $pgp(secring)]} { set pgp(secring) {} }

    set pgp(privatekeys) [PgpExec_KeyList "" $pgp(secring)]
    
    if [info exists pgpConfig(myname)] {
	set myname [string tolower $pgpConfig(myname)]
	foreach key $pgp(privatekeys) {
	    if {[string first $myname [string tolower $key]] >= 0} {
		set pgp(myname) $key
		break
	    }
	}
	if {![info exists pgp(myname)]} {
	    if [catch {PgpMatch_Simple $pgpConfig(myname) $pgp(pubring)} key] {
		Misc_DisplayText "PGP Init" "the name specified in your config.txt file\ncouldn't be unambiguously found in your key rings !"
		set pgp(myname) {}
	    } else {
		set pgp(myname) [lindex $key 0]
	    }
	}
    } else {
	set pgp(myname) [lindex $pgp(privatekeys) 0]
    }
#    PgpMatch_Init
}

#
proc PgpExec_ParseConfigTxt { file configarray } {
    upvar $configarray config

    if [catch {open $file r} in] {
	return
    }

    for {set len [gets $in line]} {$len >= 0} {set len [gets $in line]} {
	if [regexp -nocase "^\[ \t]*(\[a-z]+)\[ \t]*=(\[^#]*)" $line {} option value] {
	    set config([string tolower $option]) [string trim $value " \t\""]
	}
    }
    close $in
}

#
proc PgpExec_Encrypt { in out tokeys } {

    PgpExec_Batch [concat [list -aet $in -o $out] [Misc_Map key {lindex $key 0} $tokeys]] output

    # pgp thinks he knows better how to name files !
    if {![file exists $out] && [file exists "$out.asc"]} {
	Mh_Rename "$out.asc" $out
    }
    if {![file exists $out]} {
	error "PGP refused to generate the encrypted text:\n$output"
    } else {
	return {}
    }
}

#
proc PgpExec_EncryptSign { in out sigkey tokeys } {

    PgpExec [concat [list -aset $in -o $out -u [lindex $sigkey 0]] [Misc_Map key {lindex $key 0} $tokeys]] output $sigkey

    # pgp thinks he knows better how to name files !
    if {![file exists $out] && [file exists "$out.asc"]} {
	Mh_Rename "$out.asc" $out
    }
    if {![file exists $out]} {
	error "PGP refused to generate the encrypted signed text:\n$output"
    } else {
	return {}
    }
}

#
proc PgpExec_Sign { in out sigkey clear } {

    if $clear {
	set clear "+clearsig=on"
    } else {
	set clear "+clearsig=off"
    }
    PgpExec [list $clear -ast $in -u [lindex $sigkey 0] -o $out] output $sigkey

    # pgp thinks he knows better how to name files !
    if {![file exists $out] && [file exists "$out.asc"]} {
	Mh_Rename "$out.asc" $out
    }
    if {![file exists $out]} {
	error "PGP refused to generate the signed text:\n$output"
    } else {
	return {}
    }
}

#
proc PgpExec_Decrypt { in out outvar recipients } {
    global pgp pgpPass
    upvar $outvar output

    set recipients [string tolower $recipients]
    set useablekeys [Misc_Filter key {[string first [string tolower [string range [lindex $key 0] 2 end]] $recipients] >= 0} $pgp(privatekeys)]
    set knownkeys [Misc_Filter key {[info exists pgpPass([lindex $key 0])]} $useablekeys]

    if {[llength $knownkeys] > 0} {
	set key [lindex $knownkeys 0]
    } elseif {[llength $useablekeys] > 0} {
	set key [lindex $useablekeys 0]
    } else {
	set key {}
    }
    PgpExec [list $in -o $out] output $key
}

#
proc Pgp_GetPass { key } {
    global pgp pgpPass

    if {[lsearch -glob $pgp(privatekeys) "[lindex $key 0]*"] < 0} {
	return {}
    }

    set keyid [lindex $key 0]
    if [info exists pgpPass($keyid)] {
	return $pgpPass($keyid)
    }
    while 1 {
	if [catch {Misc_GetPass "Enter PGP password" "password for [lindex $key 1]"} password] {
	    return {}
	} elseif [PgpExec_CheckPassword $password $key] {
	    return [set pgpPass($keyid) $password]
	}
    }
}

#
proc PgpExec_ExtractKeys { file {interactive 1} } {
    global env

    if [PgpExec [list -ka $file] err {} $interactive] {
	Exmh_Status $err
    }
}

#
proc PgpExec_GetKeys { keyid file } {
    if [PgpExec [list -akx $keyid $file] msg] {
	error $msg
    } else {
	# pgp thinks he knows better how to name files !
	if {![file exists $file] && [file exists "$file.asc"]} {
	    Mh_Rename "$file.asc" $file
	}
	if {![file exists $file]} {
	    error "PGP refused to generate the key block for $keyid"
	}
    }
}

#
proc Pgp_CheckPoint {  } {
    foreach cmd { PgpMatch_CheckPoint } {
	if {[info command $cmd] != {}} {
	    if [catch {$cmd} err] {
		puts stderr "$cmd: $err"
	    }
	}
    }
}
