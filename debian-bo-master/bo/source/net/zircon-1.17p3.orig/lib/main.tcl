#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/main.tcl,v $
# $Date: 1996/06/24 10:58:24 $
# $Revision: 1.17.1.6 $
#
proc playBell {this tag} {
    global zircon Bl TBl
    set snd $Bl($this)
    switch -glob -- $tag {
      @me {catch { set snd $TBl($this,@me) }}
      user* { set foo [$tag lname] ; catch { set snd $TBl($this,$foo) } }
    }
    if {[info exists zircon(soundcmd)] && ![string match {} $zircon(soundcmd)] 
      && ![string match {} $snd]} {
	eval exec $zircon(soundcmd) $snd &
    } {
	eval $zircon(bellcmd)
    }
}
#
proc notIdle {win} {
    [channel $win] extendTime
    uplevel #0 set zircon(idle) 0
}
#
proc doScroll {win what args} {eval $win yview $what $args}
#
proc setScroll {txt win total window} {
    set id [channel $win]
    upvar #0 $id cdata
    if {$total > $cdata(history)} {
	incr total -$cdata(history)
	$txt delete 1.0 "1.0 + $total lines"
	set total $cdata(history)
    }
    $win set $total $window
}
#
# alter menu item states - used for oping and ircoping
#
proc setState {name pick state} {
    if [string compare none [$name index last]] {
	global Ops
	foreach cmd $Ops($pick) {
	    if ![catch {set idx [$name index [trans $cmd]]}] {
		$name entryconfigure $idx -state $state
	    }
	}
    }
}
#
proc makeMB {win text} {
    menubutton $win -text [trans $text] -menu $win.menu -width 10
    return [menu $win.menu]
}
#
proc getOValue {win opt lc uc} {
    if [string match {} [set x [option get ${win} $lc $uc]]] {
	set x [$win cget -$opt]
    }
    return $x
}
#
proc getTValue {win win2 opt lc uc} {
    if {[set x [option get ${win} $lc $uc]] == {}} { set x [$win2 cget -$opt] }
    return $x
}
#
proc doHelp {net topic args} {
    set service [lindex $args 0]
    if ![string match {} $service] {$net PRIVMSG $service $topic}
}
#
proc getHelp {net} {
    global ztrans
    if ![string match {} [$net helpService]] {
	set ents "{$ztrans(topic) {zircon ?}}"
    } {
	set ents "{$ztrans(topic) {zircon ?}} {$ztrans(service) [$net helpService]}"
    }
    mkEntryBox .@help$net $ztrans(help) "Enter topic on which you need help:" \
      $ents "$ztrans(ok) {doHelp $net}" "$ztrans(cancel) {}"
}
#
proc pickvar {v1 v2} { return [expr {[uplevel info exists $v1] ? $v2 : $v1}]}
#
proc setTags {this nk} {
    global TFn TFa TFg TBg TAF TAB TBl Bl
    set w [$this text]
    set x "$this,$nk"
    set ch [$this tagWindow]
    set TFn($x) [getTValue $ch $w font ${nk}Font Font]
    set TFg($x) [getTValue $ch $w foreground ${nk}Foreground Foreground]
    set TBg($x) [getTValue $ch $w background ${nk}Background Background]
    set TFa($x) [getTValue $ch $w font ${nk}ActionFont Font]
    set TAF($x) [getTValue $ch $w foreground ${nk}ActionForeground Foreground]
    set TAB($x) [getTValue $ch $w background ${nk}ActionBackground Background]
    if {[set TBl($x) [option get $ch ${nk}Bell Bell]] == {} &&
      [info exists Bl($this)]} { set TBl($x) $Bl($this) }
}
#
proc insertText {this name text tag} {
    set taglist $tag
    if [string match "*\[\002\007\017\026\037\]*" $text] {
	while {[regexp \
	  "^(\[^\002\007\017\026\037\]*)(\[\002\007\017\026\037\])(.*)$" \
	  $text match m1 ch text]} {
	    $name insert end $m1 $taglist
	    switch -- $ch {
	    \002 { set spc @b@ }
	    \007 {
		    if ![$this quiet] { playBell $this $tag }
		    global zircon
		    $name insert end { } $taglist $zircon(beep) \
		      [concat $taglist @a@$tag] { } $taglist
		    continue
		}
	    \017 { set taglist $tag ; continue }
	    \026 { set spc @v@ }
	    \037 { set spc @u@ }
	    }
	    if {[set x [lsearch $taglist ${spc}*]] < 0} {
		lappend taglist $spc$tag
	    } {	set taglist [lreplace $taglist $x $x] }
	}
    }
    $name insert end $text $taglist
}
#
proc channelInvite {net chan args} {
   if ![string match {} $chan] { userInvite $net $chan }
}
#
proc doNotice {net chan string} {
    if ![string match {} $string] {
	if {![string compare nil [set cn [Channel :: find $chan]]] ||
	    ![$cn active]} {
	    $net display @me "$chan>- $string"
	} {
	    $cn addText @me "- $string"
	    $cn configure -hpos end
	}
	$net NOTICE $chan $string
    }
}
#
proc channelNotice {net chan args} {
    if ![string match {} $chan] {
	global ztrans
	mkEntryBox .@[newName wn] "Notice to ${chan}" \
	  {Enter your notice text:} \
	  "{$ztrans(notice) {}}" "$ztrans(ok) {doNotice $net {$chan}}" \
	  "$ztrans(cancel) {}"
    }
}
#
proc channelList {net chan} { $net channelList $chan }
#
proc channelJoin {net chan args} {
    if ![string match {} $chan] { [Channel :: make $chan] sendJoin [lindex $args 0] }
}
#
proc channelMonitor {net chan} { $net monitor $chan }
#
proc channelMode {net chan} { if ![string match {} $chan] { $net qSend MODE :$chan } }
#
proc channelWho {net chan} { if ![string match {} $chan] { $net qSend WHO :$chan } }
#
proc channelNames {net chan} {
    if [string match {} $chan] {
	global ztrans
	mkDialog {} {} $ztrans(names) {That will list *ALL* users on irc!!!} {} \
	    "$ztrans(ok) {$net q1Send NAMES}" "$ztrans(cancel) {}"
    } {
	$net qSend NAMES :$chan
    }
}
#
proc popup {win} { wm deiconify $win ; raise $win }
#
proc markButton {name which} {
    if ![winfo exists $name] return
    foreach opt {font foreground background activeForeground \
      activeBackground} {
	set uopt [capitalise $opt]
	set fopt ${which}[expr {$which != {} ? $uopt : $opt}]
	set lopt [string tolower $opt]
	if ![string match {} [set cl [option get $name $fopt $uopt]]] {
	    $name conf -$lopt $cl
	} \
	elseif {[string match {} $which]} {
	    if ![string match {} [set cl [lindex [$name conf -$lopt] 3]]] {
		$name conf -$lopt $cl
	    }
	}
    }
    set af [$name cget -activeforeground]
    set fg [$name cget -foreground]
    if ![string compare $af $fg] {
	set bg [$name cget -background]
	$name conf -activeforeground $bg -activebackground $fg
    }
}
#
proc markEntry {name index which} {
    if {![winfo exists $name] || $index == -1} return
    foreach opt {font background activeBackground} {
	set uopt [capitalise $opt]
	set fopt ${which}[expr {$which != {} ? $uopt : $opt}]
	set lopt [string tolower $opt]
	if {[set cl [option get $name $fopt $uopt]] != {}} {
	    $name entryconfigure $index -$lopt $cl
	} \
	elseif {[string match {} $which]} {
	    if {[set cl [lindex [$name conf -$lopt] 3]] != {}} {
		$name entryconfigure $index -$lopt $cl
	    }
	}
    }
    set af [$name cget -activeforeground]
    set fg [$name cget -foreground]
    if ![string compare $af $fg] {
	set bg [$name cget -background]
	$name entryconfigure $index -activeforeground $bg -activebackground $fg
    }
}
#
array set userFlags {
	o	ircop
	O	ircop
	w	wallops
	s	srvmsg
	i	invisible
}
#
# First message from the server....
#
proc irc001 {net prefix param pargs} {
    global zircon defMsg defChan
    catch {destroy .@cl$net}
    $net flagControl normal
    $net fast
    set nk [lindex $pargs 0]
    if ![string compare nil [set unk [User :: find $nk]]] {
	set unk [User $nk -net $net]
    }
    $net configure -myid $unk
    $net deIRCOp
    set me [[set myid [$net myid]] name]
    set srv [$net hostid]
    set opStuff [list [$srv oper] [$srv operpw]]
    if ![string match {} [set nk [lindex $opStuff 0]]] {
	if [string match {} [set pw [lindex $opStuff 1]]] {
	    global ztrans
	    mkEntryBox .@opw$net {IRC Op Password} \
	      {Enter your operator password:} "{$ztrans(password) {}}" \
	      "$ztrans(ok) { doOper $net $nk }" "$ztrans(cancel) {}"
	} {
	    $net send OPER $nk $pw
	}
    }
    if [$net invisible] { $net setFlag invisible }
    if [$net wallops]	{ $net setFlag wallops }
    if [$net srvmsg] { $net setFlag srvmsg }
    if !$zircon(j) {
	foreach id [$net channels] {
	    if [string compare $id $defChan] {
		if {[$id join] || [$id active]} {
		    $id sendJoin {}
		    if [$id active] {
			$id setTopic {}
			$net qSend MODE :[$id name]
			$net qSend TOPIC :[$id name]
			$id flag normal
			$id unmarkV $myid
			$id unmarkOp $myid
		    }
		} \
		elseif {[$id monitor]} { channelMonitor $net [$id name] }
	    }
	}
	foreach id [$net messages] {
	    if [string compare $id $defMsg] {
		if [string compare nil [User :: find [$id name]]] {
		    $id show
		    $id flag normal
		}
	    }
	}
    }
    $net inform $param
    $net setupTests
    if $zircon(register) {
	$net send PRIVMSG ZirconBot "!zstartup $zircon(version) $zircon(patchlevel)"
	set zircon(register) 0
    }
    handleOn STARTUP [list [$srv host] [$srv port]]
    $net configure -startup 0
}
#
proc irc004 {net prefix param pargs} {
    set serverInfo [lrange $pargs 1 4]
    $net inform \
      "[string range $prefix 1 end]: umodes available [lindex $serverInfo 2],\
channel modes available [lindex $serverInfo 3]"
}
#
proc irc381 {net prefix param pargs} {
    $net configure -ircop 1
    $net inform $param
}
#
proc irc301 {net prefix param pargs} {
    global whois
    if [info exists whois($net,info)] {
	set whois($net,away) $param
    } {
	regsub -all {[\\{\"}]} $pargs {\\&} pargs
	switch [set x [Message :: find [set who [lindex $pargs 1]]]] {
	nil { $net inform "$who is away: $param" }
	default { $x awayMsg $param }
	}
    }
}
#
proc irc303 {net prefix param pargs} {
    global signInfo ztrans
    set frnd [$net finfo]
    set signons {}
    set signoffs {}
    set msg {}
    set lpar {}
    set signInfo($net) {}
    foreach who $param {
	set frd [Friend :: find $who]
	lappend lpar $frd
	if {![$frd ison] || [$frd limbo]} {
	    $frd configure -ison 1 -limbo 0
 	    if {[$frnd absent $frd] && [string compare nil [$frd usr]]} {
		[$frd usr] heal
	    } {
		if ![string match {} [$frd id]] {
		    lappend signInfo($net) $frd
		} {
		    lappend signons $who
		    if {[$net friendsOn] && [$frd menu]} { $frnd add $frd }
		    $frnd mark $frd ison
		}
	    }
	}
    }
    if ![string match {} $signons] { set msg "Signon by $signons detected.\n" }
    foreach frd [$net friends] {
	if {[$frd ison] && ![$frd limbo] && [lsearch $lpar $frd] < 0} {
	    if [string compare nil [set usr [$frd usr]]] { $usr off }
	    $frd configure -usr nil -ison 0
	    $frnd remove $frd
	    lappend signoffs [$frd name]
	}
    }
    if ![string match {} $signoffs] {
	set msg "${msg}Signoff by $signoffs detected.\n"
    }
    if ![string match {} $msg] {
	set cmd "mkInfoBox ISON .@isn$net Notify {[getDate] :\n$msg} {$ztrans(dismiss) {}}"
	if ![string match {} $signons] {
	    append cmd " {$ztrans(whois) {who303 $net $signons}}"
	}
	if ![string match {} $signoffs] {
	    append cmd " {$ztrans(whowas) {was303 $net $signoffs}}"
	}
	eval $cmd
    }
    foreach x $signInfo($net) {$net send USERHOST [$x name]}
}
#
proc who303 {net args} { foreach x $args { $net send WHOIS $x } }
#
proc was303 {net args} { foreach x $args { $net send WHOWAS $x } }
#
proc irc305 {net prefix param pargs} { $net irc305 }
#
proc irc306 {net prefix param pargs} { $net irc306 }
#
proc irc321 {net prefix param pargs} { $net irc321 }
#
proc irc322 {net prefix param pargs} { $net irc322 $prefix $param $pargs }
#
proc irc323 {net prefix param pargs} { $net irc323 $prefix $param $pargs }
#
proc irc324 {net prefix param pargs} {
    regsub -all {[\\{}\"]} $pargs {\\&} pargs
    set chan [lindex $pargs 1]
    set mode [lrange $pargs 2 end]
    if [string compare nil [set chn [Channel :: find $chan]]] {
	$chn mode $mode
    } {
	$net inform "Mode for $chan : $mode"
    }
}
#
proc irc328 {net prefix param pargs} {
    regsub -all {[\\{}\"]} $pargs {\\&} pargs
    set chan [lindex $pargs 1]
    set url $param
    if {[string compare nil [set chn [Channel :: find $chan]]] && \
      [$chn active]} {
	$chn showInfo url $url
    } {
	$net inform "Url for $chan : $url"
    }
}
#
proc irc329 {net prefix param pargs} {
    regsub -all {[\\{}\"]} $pargs {\\&} pargs
    set chan [lindex $pargs 1]
    set crt [convTime [lindex $pargs 2]]
    if {[string compare nil [set chn [Channel :: find $chan]]] && \
      [$chn active]} {
	$chn showInfo create "Created at $crt"
    } {
	$net inform "$chan created at $crt"
    }
}
#
proc irc333 {net prefix param pargs} {
    regsub -all {[\\{}\"]} $pargs {\\&} pargs
    set chan [lindex $pargs 1]
    set who [lindex $pargs 2]
    set crt [convTime [lindex $pargs 3]]
    if {[string compare nil [set chn [Channel :: find $chan]]] && \
      [$chn active]} {
	$chn showInfo topic "Topic set by $who at $crt"
    } {
	$net inform "$chan topic set at $crt by $who"
    }
}
#
proc irc353 {net prefix param pargs} {
    regsub -all {[\\{\"}]} $pargs {\\&} pargs
    if ![[set chid [Channel :: make [lindex $pargs 2]]] active] {
	global namesTxt namesChan
	if [string compare $namesChan($net) $chid] {
	    set namesChan($net) $chid
	    set namesTxt($net) $param
	} {
	    append namesTxt($net) "\n$param"
	}
    } { 
	$chid doNames $param
    }
}
#
proc updateMon {net chid names} {
    set w .@mon$chid
    if ![winfo exists $w] {makeMon $net $chid $names ; return}
    set win $w.users.userList
    set xist {}
    foreach n [winfo children $win] { lappend xist [winfo name $n] }
    foreach n $names {
	if [string match {} $n] continue
	set mrk {}
	set sp 0
	while {[string match {[@+]*} $n]} {
	    if [string match @* $n] { set mrk operator } { set mrk speaker}
	    set n [string range $n 1 end] ;
	}
	set usr [User :: make $n]
	if ![winfo exists $win.$usr] {
	    wsortIns nil $win $n $usr
	    $usr ref
	    bind $winu <Destroy> "catch {$usr deref}"
	} {
	    if ![normal $win.$usr] {
		$win.$usr configure -state normal
		$usr heal
	    }
	    listkill xist $usr
	}
	markButton $win.$usr $mrk
    }
    foreach n $xist { if [normal $win.$n] {destroy $win.$n}}
}
#
proc makeMon {net chid names} {
    global ztrans
    set w .@mon$chid
    toplevel $w -class Zircon
    set chan [$chid name]
    wm title $w "$chan $ztrans(monitor)"
    wm resizable $w 0 1
    wm protocol $w WM_DELETE_PROTOCOL "deMonitor $net $chid 1"
 
    pack [frame $w.btns -relief raised] -side bottom -fill x
    pack [set wu [frame $w.users -relief raised]] -fill y
    scrollbar $wu.vscroller -command "$wu.userList yview" 
    set win [text $wu.userList -yscrollcommand "bsSet $wu.vscroller" \
      -relief flat -borderwidth 0 -width 14]
    pack $wu.userList $wu.vscroller -side left -fill y -expand 1
    button $w.btns.cancel -text $ztrans(dismiss) \
      -command "deMonitor $net $chid 1" -width 5
    button $w.btns.join -text $ztrans(join) \
      -command " $chid sendJoin {} " -width 5
    bind $w <Destroy> "deMonitor $net $chid 1"
    pack $w.btns.cancel $w.btns.join -side left -fill x -expand 1
    foreach n $names {
	if [string match {} $n] continue
	set mrk {}
	while {[string match {[@+]*} $n]} {
	    if [string match @* $n] { set mrk operator } { set mrk speaker}
	    set n [string range $n 1 end]
	}
	set usr [User :: make $n]
	set winu $win.$usr
	if ![winfo exists $winu] {
	    wsortIns nil $win $n $usr
	    $usr ref
	    markButton $winu $mrk
	    bind $winu <Destroy> "catch {$usr deref}"
	}
    }
}
#
proc irc366 {net prefix param pargs} {
    regsub -all {[\\\"{}]} $pargs {\\&} pargs
    set chan [lindex $pargs 1]
    set chid [Channel :: make $chan]
    global namesChan namesTxt ztrans
    if ![string compare $namesChan($net) $chid] {
	if {[lsearch [$net monitorlst] [$chid lname]] >= 0 } {
	    updateMon $net $chid [split $namesTxt($net)]
	} {
	    mkInfoBox NAMES .@names$chid "$ztrans(names) $chan" $namesTxt($net) "$ztrans(dismiss) {}"
	}
    }
    set namesChan($net) {}
    set namesTxt($net) {}
}
#
proc irc376 {net prefix param pargs} {}
#
proc irc394 {net prefix param pargs} {}
#
proc irc251 {net prefix param pargs} {
    $net inform "[string range $prefix 1 end]: $param"
}
#
proc irc252 {net prefix param pargs} {
    set nm [lindex "$pargs" 1]
    $net inform \
      "[string range $prefix 1 end]: There [expr {$nm == 1 ? {is 1 operator} : "are $nm operators"}] online."
}
#
proc irc253 {net prefix param pargs} {
    set nm [lindex "$pargs" 1]
    $net inform "[string range $prefix 1 end]: There [expr {$nm == 1 ? {is 1 unknown connection} : "are $nm connections"}]."
}
#
proc irc254 {net prefix param pargs} {
    set nm [lindex "$pargs" 1]
    $net inform \
      "[string range $prefix 1 end]: There [expr {$nm == 1 ? {is 1 channel} : "are $nm channels"}] formed."
}
#
proc irc255 {net prefix param pargs} {
    $net inform "[string range $prefix 1 end]: $param"
}
#
proc zUnknown {args} {
    set cmd [lindex $args 0]
    if [string match {irc*} $cmd] {
	set net [lindex $args 1]
	if [$net startup] {$net fast}
	if ![auto_load $cmd] { ircNUM $net $cmd $args } { return [uplevel $args] }
    } {
	return [uplevel sys_unknown $args]
    }
}
#
proc ircNUM {net number pargs} {
    global ztrans
    set txt {}
    regsub -all {\\} $pargs {\\&} pargs
    foreach arg [lrange [lindex $pargs 4] 1 end] {
	if ![string match {} $arg] { append txt " $arg" }
    }
    append txt " [lindex $pargs 3]"
    switch -glob $number {
    irc[45]* { mkInfoBox ERROR .@e$net$number "$ztrans(error) $number" $txt}
    default { $net inform $txt }
    }
}
#
set inData {}
#
proc deMonitor {net chid dflag} {
    set w .@mon$chid
    catch {
	foreach x [winfo children $w.users.userList] {
	    catch {[winfo name $x] deref}
	}
	bind $w <Destroy> {}
	destroy $w
    }
    $net deMonitor [$chid lname]
    $chid configure -monitor 0
    if {$dflag && ![$chid keep] && ![$chid active]} { $chid delete }
}
#
proc find {name} {
    foreach class {Channel Message Notice} {
	if [string compare nil [set handle [$class :: find $name]]] { return $handle }
    }
    return nil
}
#
proc trans {text} {
    global ztrans
    catch {set text $ztrans([string tolower $text])}
    return $text
}
#
rename unknown sys_unknown
rename zUnknown unknown
