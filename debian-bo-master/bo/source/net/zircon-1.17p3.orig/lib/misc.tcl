#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/misc.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
proc credits {} {
    global zircon tk_patchLevel
    mkInfoBox {} .@credits "Zircon Credits" "The Zircon IRC Client
Version $zircon(version) Patchlevel: $zircon(patchlevel)

Brought to you by Lindsay (from an original idea by Jimbles)

Thanks to:

Sorry, too many people to mention. You know who you are and I
appreciate all your help.

e-mail for problems : zircon@catless.ncl.ac.uk

To join mailing list : zircon-request@newcastle.ac.uk

Web Page : http://catless.ncl.ac.uk/Programs/Zircon

tcl Version [info patchlevel]
tk Version $tk_patchLevel
[version]" {Dismiss {}}
}
#
proc viewMode {net chan args} { $net qSend MODE :$chan }
#
proc doLimit {net chan string} {
    if [string compare $string 0] {$net MODE $chan +l $string} {unlimit $net $chan}
}
#
proc unlimit {net chan args} { $net MODE ${chan} -l }
#
proc channel_setLimit {this} {
    global ztrans
    set chan [$this name]
    set net [$this net]
    if [$this operator] {
    mkEntryBox .@l$this $ztrans(limit) \
      "Enter limit value for $chan:" "{$ztrans(limit) {}}" \
      "$ztrans(set) {doLimit $net $chan}" \
      "$ztrans(view) {viewMode $net $chan}" \
      "$ztrans(clear) {unlimit $net $chan}" "$ztrans(cancel) {}"
    } {
	viewMode $net $chan
    }
}
#
proc channel_kick {this usr} {
    global ztrans
    set chan [$this name]
    set who [$usr name]
    mkDialog {} .@k$this $ztrans(kick) "Really kick $who from channel $chan?" \
      "{$ztrans(message) {}}" "$ztrans(ok) {[$this net] KICK {$chan} {$who}}" \
      "$ztrans(cancel) {}"
}
#
proc channel_banKick {this usr} {
    global banInfo
    set banInfo([$this net]) [list $usr $this]
    [$this net] qSend USERHOST :[$usr name]
}
#
proc channel_banList {this args} { [$this net] qSend MODE [$this name] :+b }
#
proc doBan {net op chid string} { 
    if ![string match {} $string] {
	$net qSend MODE [$chid name] ${op}b :$string
    }
}
#
proc channel_setBan {this} {
    global ztrans
    set net [$this net]
    if [$this operator] {
	mkEntryBox .@ban$this $ztrans(ban) \
	  "Enter name to be banned/unbanned from [$this name]." \
	  "{$ztrans(pattern) {}}" "$ztrans(ban) {doBan $net + $this}" \
	  "$ztrans(unban) {doBan $net - $this}" \
	  "$ztrans(list) {$this banList}" "$ztrans(cancel) {}"
    } {
	$this banList
    }
}
#
proc doKey {chid string} {
    if [string match {} $string] {
	clearKey ${chid}
    } {
	global ztrans
	mkDialog SETKEY .@[newName key] {Set Key} \
	  "Really set key for channel [$chid name]?" {} \
	  "$ztrans(ok) {doSetKey $chid {$string}}" "$ztrans(cancel) {}"
    }
}
#
proc doSetKey {chid string} {
    if ![string match {} [$chid key]] { doClearKey $chid }
    $chid configure -key $string
    [$chid net] MODE [$chid name] +k $string
}
#
proc clearKey {chid args} {
    if [string match {} [$chid key]] return
    global ztrans
    mkDialog CLEARKEY .@[newName key] {Clear Key} \
      "Really clear key for channel [$chid name]?" {} \
      "$ztrans(ok) {doClearKey $chid}" "$ztrans(cancel) {}"
}
#
proc doClearKey {chid args} {
    [$chid net] qSend MODE [$chid name] -k :[$chid key]
    $chid configure -key {}
}
#
proc channel_setKey {this} {
    global ztrans
    set ch [$this name]
    set net [$this net]
    if [$this operator] {
	mkEntryBox .@k$this $ztrans(key) "Enter key for $ch:" \
	  "{$ztrans(key) [$this key]}" "$ztrans(set) {doKey $this}" \
	  "$ztrans(view) {viewMode $net $ch}" \
	  "$ztrans(clear) {clearKey $this}" "$ztrans(cancel) {}"
    } {
	viewMode $net $ch
    }
}
#
proc finger {net nk} {
    if ![string match {} $nk] {
	global fingerInfo
	$net qSend USERHOST [set fingerInfo($net) :[$net trimNick [cleanup $nk]]]
    }
}
#
proc doBanKick {net who chan msg ptr} {
    $net qSend MODE $chan +b :$ptr
    $net qSend KICK $chan $who :$msg
}
#
proc irc302 {net prefix param pargs} {
    global banInfo ignoreInfo fingerInfo signInfo ztrans
    if ![regexp {^(.*)(\*?)=([+-])(.*)$} $param match nk op away uh] {
	if [info exists fingerInfo($net)] { set nk $fingerInfo($net) } \
	elseif {[info exists banInfo($net)]} { set nk $banInfo($net) } \
	elseif {[info exists ignoreInfo($net)]} { set nk $ignoreInfo($net)} { set nk {} }
	catch {unset banInfo($net) ignoreInfo($net) fingerInfo($net)}
	mkInfoBox ERROR .@bii$net Nickerr "No such nick as $nk!"
	return
    }
    set usr [User :: find $nk]
    set frd [Friend :: find  $nk]
    if {[info exists banInfo($net)] && [lindex $banInfo($net) 0] == $usr} {
	set chan [[lindex $banInfo($net) 1] name]
	set who [$usr name]
	mkEntryBox .@[newName kick] "Ban+Kick" \
	  "Really ban and kick $who ($uh) from channel $chan?" \
	  [list "$ztrans(message) {}" [list $ztrans(pattern) "*!*$uh"]] \
	  "$ztrans(ok) {doBanKick $net {$who} {$chan}}" "$ztrans(cancel) {}"
	unset banInfo($net)
    } \
    elseif {[info exists ignoreInfo($net)]} {
	unset ignoreInfo($net)
    } \
    elseif {[info exists signInfo($net)] && [set x [lsearch $signInfo($net) $frd]] >= 0} {
	if [string match [$frd id] $uh] {
	    global friendsOn signOns
	    append signOns($net) "$nk ($uh) "
	    set frnd [$net finfo]
	    if {[$net friendsOn] && [$frd menu]} { $frnd add $frd }
	    $frnd mark $frd ison
	}
        listdel signInfo($net) $x
	if [string match {} $signInfo($net)] {
	    if [info exists signOns($net)] {
		mkInfoBox ISON .@[newName isonw] Notify \
		  "[getDate] :\nSignon by $signOns($net)" {Dismiss {}} \
		  "WHOIS {who303 $net $signOns($net)}"
	    }
	    catch {unset signInfo($net) signOns($net)}
	}
    } \
    elseif {[info exists fingerInfo($net)]} {
	unset fingerInfo($net)
	regexp {^~?([^@]*)@(.*)$} $uh match user host
	if ![catch {connect $host 79} sock] {
		set w .@[newName fng]
		fileevent $sock readable "handleFinger $net $sock $w"
		toplevel $w -class Zircon
		wm title $w "Finger [$usr name]"
		wm protocol $w WM_DELETE_WINDOW "
		    destroy $w
		    catch {close $sock}
		"

		set oft [frame $w.oFrm]
		scrollbar $oft.vscroller -command "$oft.text yview"
		text $oft.text -yscrollcommand "$oft.vscroller set"
		pack $oft.text -side left -fill both -expand 1
		pack $oft.vscroller -side right -fill y
		button $w.ok -text Dismiss -command "
		    destroy $w
		    catch {close $sock}
		"
		pack $w.oFrm -expand 1 -fill x
		pack $w.ok -fill x
		puts $sock $user@$host
	} {
	    $net errmsg "Finger Error $uh : $sock"
	}
    } {
	$net inform "$nk is $uh (${op}${away})"
    }
}
#
proc handleFinger {net conn w} {
    if {[catch {gets $conn} msg] || [string match {} $msg]} {
	catch {close $conn}
    } \
    elseif {[winfo exists $w]} {
	regsub -all "\r" $msg {} msg
	$w.oFrm.text insert end $msg\n
    }
}
#
proc irc311 {net prefix param pargs} {
    global whois
    regsub -all {\\} $pargs {\\\\} pargs
    set whois($net,info0) [lindex $pargs 1]
    set whois($net,info1) [lindex $pargs 2]
    set whois($net,info2) [lindex $pargs 3]
    set whois($net,info3) $param
}
#
proc irc312 {net prefix param pargs} {
    global whois
    regsub -all {\\} $pargs {\\\\} pargs
    set whois($net,info4) [lindex $pargs 2]
    set whois($net,info5) $param
}
#
proc irc313 {net prefix param pargs} { global whois ; set whois($net,ircop) 1 }
#
proc irc314 {net prefix param pargs} {
    global whois whowas
    if [info exists whois($net,info0)] {
	if [info exists whowas($net)] { append whowas($net) "\n\n" }
	append whowas($net) "Name: $whois($net,info1)@$whois($net,info2)\
	  ($whois($net,info3))\nServer: $whois($net,info4) ($whois($net,info5))"
	foreach x [array names whois $net,*] { unset whois($x) }
    }
    irc311 $net $prefix $param $pargs
}
#
proc irc317 {net prefix param pargs} {
    global whois
    regsub -all {\\} $pargs {\\\\} pargs
    set val [lindex $pargs 2]
    if {$val == 1} {
	set whois($net,time) "1 second"
    } {
	if {$val >= 60} {
	    if {$val < 120} {
		set whois($net,time) "1 minute"
	    } {
		set whois($net,time) "[expr {$val / 60}] minutes"
	    }
	} {
	    set whois($net,time) "$val seconds"
	}
    }
}

proc max {a b} { return [expr $a > $b ? $a : $b] }

proc irc318 {net prefix param pargs} {
    global whois
    if ![info exists whois($net,info0)] return
    set who $whois($net,info0)
    set txt "Name: $whois($net,info1)@$whois($net,info2) ($whois($net,info3))"
    set st "Server: $whois($net,info4) ($whois($net,info5))"
    set wd [max [string length $txt] [string length $st]]
    append txt "\n$st\n"
    if [info exists whois($net,time)] { append txt "Idle: $whois($net,time)\n" }
    if [info exists whois($net,ircop)] { append txt "$who is an IRC operator.\n" }
    if [info exists whois($net,away)] {
	set wd [max $wd [string length $whois(away)]]
	append txt "Away: $whois($net,away)\n"
    }
    set w .@[newName ws]
    catch {destroy $w}
    toplevel $w -class Zircon
    wm title $w "WHOIS $who"
    wm protocol $w WM_DELETE_WINDOW "destroy $w"
    frame $w.f1 -borderwidth 0
    text $w.f1.t -relief raised -height 5 -width $wd
    $w.f1.t insert end $txt
    frame $w.f1.b -relief raised
    pack $w.f1.b -fill x -side bottom
    pack $w.f1.t -expand 1 -fill both -side top
    button $w.f1.b.ok -text Dismiss -command "destroy $w"
    button $w.f1.b.msg -text Message -command "doMsg $net {$who}"
    pack $w.f1.b.ok $w.f1.b.msg -expand 1 -side left -fill x
    pack $w.f1 -fill both -expand 1 -side left
    if {[info exists whois($net,channels)] && $whois($net,channels) != {}} {
	button $w.f1.b.all -text {Join All} \
	  -command "joinAll $net $whois($net,channels) ; destroy $w"
	pack $w.f1.b.all -expand 1 -side left -fill x
	makeLB $w.f2
	foreach chn $whois($net,channels) { $w.f2.l insert end $chn }
	bind $w.f2.l <Double-Button-1> { joinAll [%W get [%W nearest %y]] }
	pack $w.f2 -side right -fill both -expand 1
    }
    foreach x [array names whois $net,*] { unset whois($x) }
}
#
proc joinAll {net args} {
    foreach ch $args { regsub {^@} $ch {} ch ; channelJoin $net $ch }
}
#
proc irc319 {net prefix param pargs} {
    global whois ; append whois($net,channels) " $param"
}
#
proc irc369 {net prefix param pargs} {
    global whois whowas
    if [info exists whois($net,err)] {
	set txt "There was no such user as $whois($net,err)."
    } {
	if [info exists whowas($net)] { append whowas($net) "\n\n" } { set whowas($net) {} }
	set txt "$whowas($net)Name: $whois($net,info1)@$whois($net,info2) ($whois($net,info3))\n\
Server: $whois($net,info4) ($whois($net,info5))"
    }
    mkInfoBox WHOWAS .@whowas Whowas "$txt" {Dismiss {}}
    foreach x [array names whois $net,*] { unset whois($x) }
    catch {unset whowas($net)}
}
#
proc irc341 {net prefix param pargs} {
    regsub -all {[\\{\"}]} $pargs {\\&&} pargs
    if ![string compare nil [set id [Channel :: find [set chan [lindex $pargs 2]]]]] {
	set id [$net info]
    }
    $id addText {} "*** Inviting [lindex $pargs 1] to channel ${chan}"
}
#
proc irc342 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    $net inform "Summoning [lindex $pargs 1] to IRC"
}
#
proc irc315 {net prefix param pargs} {
    global whoTxt
    if {[info exists whoTxt($net)] && [string match {.@who*} $whoTxt($net)] &&
      [winfo exists $whoTxt($net)]} {$whoTxt($net) yview 0}
    catch {unset whoTxt($net)}
}
#
proc irc352 {net prefix param pargs} {
    global whoTxt
    set fmt "%-9s\t%-14s\t%-3s\t%s@%s (%s)\n" 
    regsub -all {[\\{}]} $pargs {\\&} pargs
    set txt [format $fmt [lindex $pargs 1] \
      [lindex $pargs 5] [lindex $pargs 6] [lindex $pargs 2] \
      [lindex $pargs 3] $param]
    if ![info exists whoTxt($net)] {
	set whoTxt($net) [mkInfoBox WHO .@[newName who] "Who [getDate]" {} {Dismiss {}}]
	$whoTxt($net) configure -tabs {1i 2i 3i}
    }
    if ![winfo exists $whoTxt($net)] return
    $whoTxt($net) configure -state normal
    insertText [$net info] $whoTxt($net) $txt {}
    $whoTxt($net) configure -state disabled
    set ln [lindex [split [$whoTxt($net) index end] .] 0]
    if {$ln < 24 && $ln > 10} {
	$whoTxt($net) conf -height $ln
    }
    $whoTxt($net) see end
}
#
proc irc367 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    set chan [lindex $pargs 1]
    set ban [lindex $pargs 2]
    if {[string compare nil [set chn [Channel :: find $chan]]] && [$chn active]} {
	$chn addText @BAN "**> $ban is banned."
    } {
	$net display @BAN "Channel $chan bans $ban"
    }
}
#
proc irc368 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    set chan [lindex $pargs 1]
    if {[string compare nil [set chn [Channel :: find $chan]]] && [$chn active]} {
	$chn addText @BAN "**> $param"
    } {
	$net display @BAN "Channel $chan $param"
    }
}
#
proc handleURL {net url} {
    global zircon
    if [info exists zircon(cciport)] {
	if {$zircon(cciport) == {netscape}} {
	    exec netscape -remote openurl($url,newwindow) &
	} \
	elseif {[catch {connect localhost $zircon(cciport)} ccisock]} {
	    $net errmsg "Cannot connect to WWW viewer ($val)"
	} {
	    set url [string trim $url]
	    gets $ccisock res
	    puts $ccisock "GET URL <$url> OUTPUT NEW\r"
	    gets $ccisock res
	    puts $ccisock "DISCONNECT\r"
	    close $ccisock
	    return
	}
    }
    if [info exists zircon(wwwclient)] { exec $zircon(wwwclient) $url & }
}

#
proc doExec {where} {
    global ztrans
    mkDialog EXEC .@e$where {Execute command} {Enter command to be executed} \
	{{Command {}}} "$ztrans(ok) {runCmd $where}" "$ztrans(cancel) {}"
}
#
proc runCmd {where cmd} {
    if ![string match {} $cmd] {
	if [catch {open "|$cmd 2>&1" r} ip] {
	    mkInfoBox ERROR .@xe$where {Execerr} \
	      "Error executing \"$cmd\" - $ip"
	} {
	    fileevent $ip readable "execOP $where {$cmd} $ip"
	}
    }
}
#
proc net_exec {this} { doExec [$this info] }
#
proc execOP {where cmd fd} {
    if [catch {gets $fd} data] {
	mkInfoBox ERROR .@xcerr {Execerr} "Error executing \"$cmd\" - $data"
    } \
    elseif {[eof $fd]} {
	if [catch {close $fd} msg] {
	    mkInfoBox ERROR .@xcerr Execerr "Error executing \"$cmd\" - $msg"
	}
    } \
    elseif {[string match {info*} $where]} {
	$where addText EXEC $data
    } {
	$where send $data
    }
}
#
proc doScript {where} {
}
#
proc findURL {win x y net} {
    set ls [$win index "@$x,$y linestart"]
    set txt [$win get $ls "@$x,$y lineend"]
    if [regexp -nocase -indices \
      "((http|gopher|ftp|wais|telnet)://\[^ \t\)>\",;&\]+)" \
      $txt url mt] {
	$win tag remove sel 0.0 end
	set se [expr [lindex $mt 1] + 1]
	$win tag add sel "$ls +[lindex $mt 0] chars" "$ls +$se chars"
	handleURL $net [$win get sel.first sel.last]
    }
    notIdle %W
}
