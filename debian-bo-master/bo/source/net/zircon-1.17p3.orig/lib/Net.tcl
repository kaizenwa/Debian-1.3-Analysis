#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Net.tcl,v $
# $Date: 1996/07/01 09:29:47 $
# $Revision: 1.17.1.8 $
#
#
set netCount 0
#
proc Netspace {name args} {
    global netCount current zircon ztrans
    if ![string match {} $current(net)] {
	if [string compare [$current(net) name] _theNet_] {
	    mkDialog {} .@neterr {Netspace error} \
	      {Nested Netspace directive detected!!} {}
	    return
	}
    }
    if ![string match {} $args] {
	set body [lindex $args 0]
	if ![string compare [llength $body] 1] {
	    if [file exists $zircon(prefdir)/$body] {
		set fd [open $zircon(prefdir)/$body]
		set body [read $fd]
		close $fd
	    }
	}
    } {
	set name _theNet_
	set body $name
    }
    Net [set current(net) net$netcount] -name $name
    incr netCount

    set current(net) {}
}
#
class Net {
    name	{}
    sock	{}
    startup	1
    hostid	nil
    away	0
    ircop	0
    info	{}
    control	{}
    nickname	{}
    nicks	{}
    ircname	{}
    ircnames	{}
    userinfo	{}
    myid	nil
    helpService	{}
    wallops	0
    srvmsg	0
    invisible	0
    showPublic	0
    showLocal	0
    showPrivate	0
    topicOnly	0
    minMembers	0
    sorted	0
    listPattern .*
    topicPattern	.*
    friendsStyle	window
    aways	{}
    signoffs	{}
    actions	{}
    friendsOn	0
    monitorlst	{}
    monitorTime	60000
    testTime	30000
    closeTime	0
    notifyInterval	30000
    verboseCTCP	0
    noRefresh	1
    killPath	1
    ons		{}
    bindings	{}
    noConfirm	{}
    toInfo	{}
    ignores	{}
    leaves	{}
    noPopup	0
    popInfo	0
    allChannels {}
    channels	{}
    messages	{}
    notices	{}
    chats	{}
    users	{}
    friends	{}
    servers	{}
    services	{}
    busy	0
    ping	0
    pinged	0
    reconnect	0
    nicksize	9
    listFile	{}
    splits	{}
}
#
proc Net {name args} {
    if [string match {::} $name] {
	return [eval Net_[lindex $args 0] [lrange $args 1 end] ]
    }
    set this [objName Net]
    initObj $this Net
    proc $this {args} "eval net_call $this \$args"
    set OType($this) Net
    upvar #0 $this ndata
    set ndata(name) $name
    if ![string match {} $args] { eval $this configure $args }
    return $this
}
proc net_trimNick {this nk} {
    upvar #0 $this ndata
    if $ndata(nicksize) {
	return [string range $nk 0 [expr $ndata(nicksize) - 1]]
    }
    return $nk
}
#
proc net_me {this usr} {return [expr ![string compare $usr [$this myid]]]}
#
proc net_configure {this args} {
    upvar #0 $this ndata
    while {![string match {} $args]} {
	set opt [lindex $args 0]
	set val [lindex $args 1]
	switch -glob -- $opt {
	-nickname { $this setNickname $val }
	-ircname { 
		catch {entrySet [[$this control] window].nSFrm.ircname.entry $val}
		set ndata(ircname) $val
		if [string match {} $ndata(userinfo)] { set ndata(userinfo) $val }
	    }
	-ircop {
		set ndata(ircop) $val
		[$this control] ircItems [expr {$val ? {normal} : {disabled}}]
		if $val {
		    set ndata(wallops) 1
		    set ndata(srvmsg) 1
		}
	    }
	-myid {
		if [string compare $val $ndata(myid)] {
		    catch {$ndata(myid) deref}
		    set ndata(myid) $val
		    if [string compare nil $val] {
			$val ref
			$this setNickname [$val name]
		    }
		}
	    }
	-popInfo {
		set ndata(popInfo) $val
		if ![string match {} $ndata(info)] {
		    $ndata(info) configure -open $val
		}
	    }
	+* {
	    if ![string match {} $val] {
		set var [string range $opt 1 end]
		if {[lsearch $ndata($var) $val] < 0} {lappend ndata($var) $val}
	    }
	}
	default { set ndata([string range $opt 1 end]) $val }
	}
	set args [lrange $args 2 end]
    }
}
#
proc net_register {this what name} {
    upvar #0 $this ndata
    lappend ndata($what) $name
}
#
proc net_deregister {this what name} {
    upvar #0 $this ndata
    listkill ndata($what) $name
}
#
proc net_setFlag {this flag} {
    $this MODE [$this nickname] \
      [expr {[$this $flag] ? {+} : {-}}][string index $flag 0]
}
#
proc net_flagControl {this state} {
    set ctl [[$this control] window]
    if ![string match {} [$this helpService]] {
	$ctl.helpFrm.help.menu entryconfigure end -state $state
    }
    foreach w {cr.invis cr.wallop cr.srvmsg 
      bf2.servers bf2.users bf2.channels bf2.services
      bf1.away bf1.brb bf1.friends cmdLine.channel } {
	$ctl.$w configure -state $state
    }
}
#
proc net_finfo {this} {
    upvar #0 $this ndata
    return [$ndata(control) friends]
}
#
proc net_setupUsers {this} {
    set frnd [$this finfo]
    foreach frd [$this friends] {
	if [$this friendsOn] { $frd configure -notify 1 }
	if {![$this friendsOn] || [$frd ison]} { $frnd add $frd}
    }
}
#
proc net_fast {this} {
    set txt [[$this info] text]
    $txt configure -cursor arrow
    catch {grab release $txt}
}
#
proc net_slow {this} {
    set txt [[$this info] text]
    catch {grab set $txt}
    $txt configure -cursor watch
}
#
proc net_display {this tag txt} { [$this info] addText $tag $txt }
#
proc net_inform {this txt} { [$this info] addText {} "*** $txt" }
#
proc net_warn {this txt} { [$this info] addText @WARN "*** $txt" }
#
proc net_errmsg {this txt} { [$this info] addText @ERROR "*** $txt" }
#
proc net_call {this op args} {
    upvar #0 $this ndata
    switch $op {
    active { return [expr {![string match {} $ndata(sock)]}]}
    host { return [$ndata(hostid) host] }
    }
    if [info exists ndata($op)] { return $ndata($op) }
    uplevel #0 set current(net) $this
    return [eval net_$op $this $args]
}
#
proc net_closeSock {this msg} {
    upvar #0 $this ndata
    catch {foreach x [after info] { after cancel $x } }
    if ![string match {} [set sock $ndata(sock)]] {
	catch {atclose $sock clear}
	if ![string match {} $msg] { catch {ircsend $sock "QUIT :$msg" }}
	catch {shutdown $sock all}
	catch {close $sock}
	$this inform "Connection to [$this host] closed"
	set ndata(sock) {}
	set ndata(pinged) 0
	set ndata(hostid) nil
    }
}
#
proc net_doQuit {this msg} {
    global confChange
    $this closeSock $msg
    if $confChange {
	set w .@[newName Save]
	mkDialog SAVECONF $w {Save Configuration} \
	  {You have made changes to your configuration. Do you wish to \
save them?} {} {No exit} {Yes {saverc}}
	tkwait window $w
    }
    exit
}
#
proc net_quit {this} {
    global ztrans
    if [$this active] {
	mkDialog QUIT .@q$this "Quit IRC" {Really quit?} \
	  "{$ztrans(message) {[lindex [$this signoffs] 0]}}" \
	  "$ztrans(ok) {$this doQuit}" "$ztrans(cancel) {}"
    } {
	$this doQuit {}
    }
}
#
proc net_show {this} {
    global zlayout
    upvar #0 $this ndata
    if [string match {} $ndata(info)] {
	set ndata(info) [Info info$this -net $this]
	catch {wm geometry [$ndata(info) window] $zlayout([$this name],info)}
	$this slow
	set ndata(control) [Control ctl$this -net $this]
	catch {wm geometry [$ndata(control) window] $zlayout([$this name],control)}
	$this fast
    }
    $this flagControl disabled
    $this setupUsers
    update
}
#
proc net_startIRC {this args} {
    global connected zircon ztrans
    upvar #0 $this ndata
    if [string match {} $args] { set srv $ndata(hostid) } {
	set srv [lindex $args 0]
    }
    if ![string compare nil $srv] { return 0 }
    $this show
    set port [$srv port]
    set server [$srv host]
    $this configure -ircop 0 -reconnect $zircon(reconnect)
    set sname $server
    if [string compare 6667 $port] { append sname :$port }
    [$this control] showServer $sname
    $this inform "Connecting to port $port of server $server"
    update idletasks
    $this slow
    set ndata(sock) {}
    set ndata(hostid) $srv
    if [string match {} $port] {
	if [catch {dp_connect $server} sock] {
	    $this errmsg "Cannot connect to UNIX domain server $server ($sock)"
	    $this fast
	    return 0
	}
    } \
    elseif {[catch {aconnect $server $port} sock]} {
	$this errmsg "Cannot connect to server $server ($sock)"
	$this fast
	return 0
    }
    if $zircon(async) {
	global ztrans
	fileevent $sock writable \
	  "sconf $sock ; $this cCheck $sock ; $this afterCon $sock"
	$this fast
	set ctl [[$this control] window]
	set cmd [$ctl.bf1.quit cget -command]
	$ctl.bf1.quit configure -text $ztrans(abort) -command "$this abort $sock"
	set connected($this) 0
	vwait connected($this)
	$ctl.bf1.quit configure -text $ztrans(quit) -command $cmd
	return [expr {$connected($this) != 2}]
    } {
	return [$this afterCon $sock]
    }
}
#
proc net_abort {this sock} {
    close $sock
    uplevel #0 set connected($this) 2
}
#
proc net_cCheck {this sock} {
    uplevel #0 set connected($this) 1
    fileevent $sock writable {}
}    
#
proc net_afterCon {this sock} {
    global user host STN Icon
    upvar #0 $this ndata
    set STN($sock) $this
    set ndata(sock) $sock
    set srv $ndata(hostid)
    set server [$srv host]
    set passwd [$srv passwd]
    foreach ln [$srv script] { lowsend $sock "$ln\n" }
    handler $sock re ircInput
    socketOption $sock recvBuffer 8192
    atclose $sock append "$this close"
    if ![string match {} $passwd] { $this qSend PASS :$passwd }
    $this qSend USER $user $host $server :[$this ircname]
    $this qSend NICK :$ndata(nickname)
    if !$ndata(noRefresh) { $this channelList { } }
    set w [[$this info] window]
    wm title $w "Zircon Information Window - $server" 
    wm iconname $w [set Icon($w) "Info $server"]
    return 1
}
#
proc net_changeServerPort {this srv prt} {
    foreach x [$this servers] {
	if {![string compare [$x host] $srv] && \
	  ![string compare [$x port] $prt]} {
	    $this changeServer $x
	    return
	}
    }
    $this changeServer [Server $srv -port $prt]
}
#
proc net_changeServer {this srv args} {
    global zircon connected
    if {[info exists connected($this)] && $connected($this) == 0} {
	bell
	entrySet [[$this control] window].nSFrm.server.entry [$this host]
	return
    }
    set ctl [[$this control] window]
    if [$this active] {
	$this inform "Closing connection to [$this host]"
	$this closeSock {Changing Servers}
	$this flagControl disabled
	foreach x {channels messages notices} {
	    foreach ch [$this $x] { $ch flag disabled }
	}
	$this irc305
	set zircon(j) 0
	$this inform "About to connect to [$srv host]"
	after 3000
    }
    $this configure -startup 1
    $this startIRC $srv
}
#
proc net_channelList {this doit} {
    global showList ztrans
    upvar #0 $this ndata
    set ndata(allChannels) {}
    set w .@l$this
    if ![winfo exists $w] {
	toplevel $w -class Zircon
	wm title $w {IRC Channel List}
	wm iconname $w {IRC Channel List}
	frame $w.filter -relief raised
	checkbutton $w.filter.public -variable ${this}(showPublic) -text Public
	checkbutton $w.filter.local -variable ${this}(showLocal) -text Local
	checkbutton $w.filter.private -variable ${this}(showPrivate) \
	  -text Private
	checkbutton $w.filter.topic -variable ${this}(topicOnly) \
	  -text {With Topic}
	checkbutton $w.filter.sorted -variable ${this}(sorted) -text Sorted

	scale $w.filter.members \
	  -from 1 -to 25 -label {Minimum Number of Members} \
	  -showvalue 1 -orient horizontal \
	  -command "set ${this}(minMembers)"

	$w.filter.members set $ndata(minMembers)

	pack $w.filter.members -fill x
	pack $w.filter.public $w.filter.local $w.filter.private \
	  $w.filter.topic $w.filter.sorted -side left -fill x
	labelEntry 0 $w.filter2 {-text Channel} [$this listPattern] {}
	labelEntry 0 $w.filter3 {-text Topic} [$this topicPattern] {}

	makeLB $w.chn -width 20 -height 8 -setgrid 1
	frame $w.btn
	button $w.btn.ok -text $ztrans(dismiss) -command "destroy $w" -relief raised
	wm protocol $w WM_DELETE_PROTOCOL "destroy $w"
	button $w.btn.clear -text $ztrans(clear) -relief raised \
	  -command "$w.chn.l delete 0 end ; set ${this}(allChannels) {}"
	button $w.btn.list -text $ztrans(list) -relief raised -command "
	    $w.chn.l delete 0 end
	    catch {grab set $w}
	    $w configure -cursor watch
	    $this q1Send LIST
	    $w.btn.list configure -state disabled
	    set ${this}(allChannels) {}
	  "
	pack $w.btn.list $w.btn.clear $w.btn.ok -side left -expand 1 -fill x
	pack $w.btn -fill x -side bottom
	pack $w.filter $w.filter2 $w.filter3 -fill x
	pack $w.chn -expand 1 -fill both
	bind $w.chn.l <Double-Button-1> "
	    channelJoin $this \[lindex \$${this}(allChannels) \[%W nearest %y\]\] {}
	    break
	"
	bind $w.chn.l <Double-Button-2> "
	    whoAction \[lindex \$${this}(allChannels) \[%W nearest %y\]\]
	    break
	"
	bind $w.chn.l <Button-1> "
	    entrySet [[$this control] window].cmdLine.channel \
	      \[lindex \$${this}(allChannels) \[%W nearest %y\]\]
	    break
	"
    } {
	popup $w
	if ![string match {} $doit] {$w.chn.l delete 0 end}
    }
    set showList 0
    if ![string match {} $doit] { $this qSend LIST :$doit ; set showList 1 }
}
#
proc net_irc321 {this args} {
    if ![winfo exists .@l$this] return
    global zircon
    upvar #0 $this ndata
    if [catch {set ndata(listFile) [open "$zircon(tmp)/list[pid]" w+]} msg] {
	set ndata(listFile) {}
    }
}
#
proc net_irc322 {this prefix param pargs} {
    if ![winfo exists .@l$this] return
    upvar #0 $this ndata
    if ![string match {} $ndata(listFile)] {
	puts $ndata(listFile) $pargs
	puts $ndata(listFile) $param
    } {
	$this listline $param $pargs
    }
}
#
proc net_listline {this param pargs} {
    set w .@l$this
    upvar #0 ${this}(listPattern) listPattern
    upvar #0 ${this}(topicPattern) topicPattern
    if [string match {} [set listPattern [$w.filter2.entry get]]] {
	set listPattern {.*}
    } \
    elseif {[catch {regexp $listPattern test} msg]} {
	set listPattern {.*}
	$w.filter2.entry delete 0 end
	$w.filter2.entry insert insert $listPattern
	mkInfoBox {} .@lpt$this Error "Bad regexp for list pattern:\n$msg"
    }
    if [string match {} [set topicPattern [$w.filter3.entry get]]] {
	set topicPattern {.*}
    } \
    elseif {[catch {regexp $topicPattern test} msg]} {
	set topicPattern {.*}
	$w.filter3.entry delete 0 end
	$w.filter3.entry insert insert $topicPattern
	mkInfoBox {} .@tpt$this Error "Bad regexp for topic pattern:\n$msg"
    }
    net_listline2 $this $param $pargs
}
#
proc net_listline2 {this param pargs} {
    global showList
    upvar #0 ${this}(listPattern) listPattern \
      ${this}(topicPattern) topicPattern $this ndata
    regsub -all {\\} $pargs {\\\\} pargs
    regsub -all "\t" $pargs "\\\t" pargs
    set chan [lindex $pargs 1]
    set w .@l$this
    if !$showList {
	switch -glob $chan {
	{\*}  { if !$ndata(showPrivate) { return } {set chan Prv } }
	&*  { if !$ndata(showLocal)   { return } }
	#*  { if !$ndata(showPublic)  { return } }
	}
    }
    set memb [lindex $pargs 2]
    if {$showList  || ((![string match {} $param] || !$ndata(topicOnly)) && \
      $memb >= $ndata(minMembers) && [regexp -nocase $listPattern $chan] && \
      [regexp $topicPattern $param])} {
	$this configure +allChannels $chan
	if ![catch {set lln \
	  "[format {%-12s %3d %s} [string range $chan 0 11] $memb $param]"}] {
	    $w.chn.l insert end $lln
	}
    }
}
#
proc net_irc323 {this prefix param pargs} {
    global showList zircon
    upvar #0 ${this}(listPattern) listPattern \
      ${this}(topicPattern) topicPattern $this ndata
    set showList 0
    set w .@l$this
    catch {grab release $w}
    catch {$w configure -cursor arrow}
    if ![string match {} [set fd $ndata(listFile)]] {
	update
	if [winfo exists $w] {
	    seek $fd 0 start
	    set lcount 0
	    if [string match {} [set listPattern [$w.filter2.entry get]]] {
		set listPattern {.*}
	    } \
	    elseif {[catch {regexp $listPattern test} msg]} {
		set listPattern {.*}
		$w.filter2.entry delete 0 end
		$w.filter2.entry insert insert $listPattern
		mkInfoBox {} .@lpt$this Error "Bad regexp for list pattern:\n$msg"
	    }
	    if [string match {} [set topicPattern [$w.filter3.entry get]]] {
		set topicPattern {.*}
	    } \
	    elseif {[catch {regexp $topicPattern test} msg]} {
		set topicPattern {.*}
		$w.filter3.entry delete 0 end
		$w.filter3.entry insert insert $topicPattern
		mkInfoBox {} .@tpt$this Error "Bad regexp for topic pattern:\n$msg"
	    }
	    if $ndata(sorted) {
		while {![eof $fd]} {
		    gets $fd pg
		    gets $fd top
		    lappend lst [list $pg $top]
		    if {[incr lcount] > 100} {
			update
			set lcount 0
			if ![winfo exists $w] break
		    }
		}
		set lcount 0
		foreach x [lsort $lst] {
		    $this listline2 [lindex $x 1] [lindex $x 0]
		    if {[incr lcount] > 50} {
			update
			set lcount 0
			if ![winfo exists $w] break
		    }
		}
	    } {
		while {![eof $fd]} {
		    gets $fd pg
		    gets $fd top
		    $this listline2 $top $pg
		    if {[incr lcount] > 50} {
			update
			set lcount 0
			if ![winfo exists $w] break
		    }
		}
	    }
	}
	catch {exec rm $zircon(tmp)/list[pid] }
	close $fd
	set ndata(listFile) {}
    }
    catch {$w.btn.list configure -state normal}
}
#
proc net_deIRCOp {this} {
    $this configure -ircop 0
    $this MODE [$this nickname] -O
}
#
proc net_keepAway {this value} {
    $this AWAY $value
    [[$this control] window].bf1.away.menu add command \
      -label "[prune $value 15]" -command "$this AWAY {$value}"
    upvar #0 $this ndata
    lappend ndata(aways) $value
    uplevel #0 set confChange 1
}
#
proc net_getAway {this} {
    global ztrans
    mkEntryBox .@away$this {Away Message} {Enter your away message:} \
      "{$ztrans(away) {}}" \
      "$ztrans(ok) {$this AWAY}" "$ztrans(keep) {$this keepAway}" \
      "$ztrans(back) {$this AWAY}" "$ztrans(cancel) {}"
}
#
proc net_doBRB {this args} {
    upvar #0 $this ndata
    set ctl [[$this control] window]
    if $ndata(away) {
	$ctl.bf1.brb conf -text BRB
	foreach x {channels messages chats} {
	    foreach id $ndata($x) {if [$id active] { $id send back -nopop }}
	}
	$this AWAY
    } {
	$ctl.bf1.brb conf -text Back
	foreach x {channels messages chats} {
	    foreach id $ndata($x) {if [$id active] { $id send brb -nopop }}
	}
	$this AWAY {Back soon.}
    }
}
#
proc net_setNickname {this nk} {
    set nk [$this trimNick $nk]
    upvar #0 $this ndata
    if ![string match {} $ndata(control)] { $ndata(control) showNick $nk }
    set ndata(nickname) $nk
    if {[string compare nil [set myid [$this myid]]] &&
      [string compare [$myid name] $nk]} {
	foreach x {channels messages notices chats} {
	    foreach id [$this $x] { $id nickChange $myid $nk }
	}
	$myid rename $nk
    }
}
#
proc net_changeNickname {this nk} {
    if [$this active] { $this NICK $nk } { $this setNickname $nk }
}
#
proc net_changeIRCName {this name} {
    $this configure -ircname $name
    if [$this active] {
	mkDialog {} .@warn Warning \
	  "Change will not take effect until next server change." {}
    }
}
#
proc net_irc305 {this} {
    upvar #0 $this ndata
    if $ndata(away) {invert [[$this control] window].bf1.away}
    set ndata(away) 0
}
#
proc net_irc306 {this} {
    upvar #0 $this ndata
    if !$ndata(away) {invert [[$this control] window].bf1.away}
    set ndata(away) 1
}
#
proc net_close {this args} {
    global zircon ztrans connected
    set connected($this) 2
    if ![$this active] return
    foreach x {monitorTest pingTest isonTest ircTests} {
	catch {after cancel "$this $x"}
    }
    foreach x [$this splits] { $this cleanSplit $x }
    set srv [$this hostid]
    $this closeSock {}
    $this flagControl disabled
    foreach x {channels messages notices} {
	foreach id [$this $x] { $id flag disabled }
    }
    $this irc305
    set zircon(j) 0
    if [string compare nil $srv] {
	set host [$srv host]
	set port [$srv port]
    } {
	set host {}
	set port 6667
    }
    if [string match {} $args] {
	set msg "Server $host has closed the connection."
    } {
	set msg [lindex $args 0]
    }
    bell
    handleOn CLOSE [list $host $port]
    if [$this reconnect] {startReconn $this $srv $msg} {
	mkDialog {} {} $ztrans(shutdown) $msg {} \
	  "$ztrans(dismiss) {}" \
	  "$ztrans(connect) {after 0 $this doReconnect $srv}"
#	  "{Keep Trying} {$this configure -reconnect 1 ; after 0 startReconn $this $host {{$msg}}}"
    }
}
#
proc startReconn {net srv msg} {
    global ztrans
    if ![winfo exists .@cl$net] {
	mkDialog {} .@cl$net $ztrans(shutdown) \
	  "$msg - $ztrans(reconnecting)." {} \
	  "{Stop trying} {stopReconn $net}"
    }
    after 1000 $net doReconnect $srv
}
#
proc stopReconn {net} {$net configure -reconnect 0}
#
proc net_doReconnect {this srv} {
    if {![$this startIRC $srv] && [$this reconnect]} {
	after 0 startReconn $this $srv {{}}
    }
}
#
proc net_deMonitor {this chan} {
    upvar #0 $this ndata
    listkill ndata(monitorlst) $chan
    if [string match {} $ndata(monitorlst)] {after cancel "$this monitorTest"}
}
#
proc net_monitor {this chan} {
    if ![string match {} $chan] {
	if ![[set chid [Channel :: make $chan]] active] {
	    upvar #0 $this ndata
	    set chan [$chid lname]
	    if {[lsearch $ndata(monitorlst) $chan] < 0} { lappend ndata(monitorlst) $chan }
	    $chid configure -monitor 1
	    $this monitorTest
	}
    }
}
#
proc net_monitorTest {this} {
    upvar #0 $this ndata
    if ![string match {} $ndata(monitorlst)] {
	$this NAMES [join [split $ndata(monitorlst)] ,]
	after $ndata(monitorTime) "$this monitorTest"
    }
}
#
proc net_send {this op args} {
    upvar #0 $this ndata
    if ![string match {} $ndata(sock)] {
	set msg $op
 	if [string compare : [set last :[lindex $args end]]] {
	    if ![catch {set foo [lreplace $args end end]}] {
		append msg " $foo $last"
	    }
	}
	if [catch {ircsend $ndata(sock) $msg}] { $this close }
    }
}
#
proc net_qSend {this op args} {
    upvar #0 $this ndata
    if [catch {ircsend $ndata(sock) "$op [join $args]"} msg] {
	$this close $msg
    }
}
#
proc net_q1Send {this op} {
    upvar #0 $this ndata
    if [catch {ircsend $ndata(sock) $op} msg] {	$this close $msg }
}
#
proc net_setupTests {this} {
    upvar #0 $this ndata
    set ndata(testTime) $ndata(notifyInterval)
    if {$ndata(closeTime) > 0 && $ndata(closeTime) < $ndata(notifyInterval)} { 
	set ndata(testTime) $ndata(closeTime)
    }
    $this ISON
    upvar #0 $this ndata
    if $ndata(ping) {
	set ndata(pinged) 0
	after $ndata(ping) "$this pingTest"
    }
    $this monitorTest
    after $ndata(notifyInterval) "$this isonTest"
    after $ndata(testTime) "$this ircTests"
}
#
proc net_ircTests {this} {
    global zircon MkOp
    upvar #0 $this ndata
    if {$ndata(closeTime) > 0} {
	foreach x {channels notices messages chats} {
	    foreach id [$this $x] {$id inactive}
	}
	[$this info] inactive
    }
    incr zircon(idle) [expr {$ndata(testTime) / 1000}]
    foreach id [array names MkOp] {
	if {![string match {} [info procs $id]] && [$id operator]} {
	    set flag +
	    set who {}
	    foreach n $MkOp($id) {
		if ![$id isOp $n] {
		    append flag o
		    lappend who [$n name]
		    $n deref
		}
	    }
	    if ![string match {} $who] { $this MODE [$id name] $flag $who }
	}
	unset MkOp($id)
    }
    after $ndata(testTime) "$this ircTests"
}
#
proc net_isonTest {this} {
    $this ISON
    after [$this notifyInterval] "$this isonTest"
}
#
proc net_pingTest {this} {
    global zircon
    upvar #0 $this ndata
    set nm [$this host]
    if $ndata(pinged) {
	$this close "Server $nm is not responding - closing the connection"
    } {
	$this PING $nm
	set ndata(pinged) 1
	after $ndata(ping) "$this pingTest"
    }
}
#
proc Net_list {} { return [info globals net\[0-9\]*] }
#
proc net_newSplit {this split} {
    global Split TSplit Heal
    upvar #0 $this ndata
    $ndata(info) optText SPLIT "*** Netsplit - $split"
    set TSplit($split) [after 600000 $this cleanSplit "{$split}"]
    catch {after cancel $Heal($split) ; unset Heal($split)}
    handleOn SPLIT $split
    lappend ndata(splits) [list $split]
}
#
proc net_cleanSplit {this h} {
    global Split Heal TSplit
    upvar #0 $this ndata
    if [info exists Split($h)] {
	set frnd [$this finfo]
	foreach user $Split($h) {
	    if [catch {set nk [$user name]}] continue
	    foreach x {Chat Message Notice} {
		if [string compare nil [set msg [$x :: find $nk]]] {
		    if [$msg active] {
			$msg flag normal
			$msg addText {} \
			  "*** netsplit : $nk may have left IRC."
		    }
		}
	    }
	    foreach id [$this channels] {
		if {[$id isJoined $user] &&
		  ![normal [$id window].cFrm.uFrm.userBtn.$user]} {
		    $id killUser $user
		} elseif {[$id monitor]} {
		    catch {destroy .@mon$id.users.userList.$user}
		}
	    }
	    $frnd remove [$user fobj]
	    $user deref
	}
	unset Split($h)
	listkill ndata(splits) [list $h]
    }
    catch { after cancel $TSplit($h) ; unset TSplit($h) }
    catch { after cancel $Heal($h) ; unset Heal($h) }
}
#
proc net_setMode {this chan mode args} {
    $this MODE $chan $mode [lindex $args 0]
}
#
# IRC Command procs
#
#
proc net_WHOIS {this nk args} {
    if ![string match {} $nk] {
	if [string match {} $args] {
	    $this qSend WHOIS :$nk
	} {
	    $this qSend WHOIS [lindex $args 0] :$nk
	}
    }
}
#
proc net_WHOWAS {this nk args} {
    if ![string match {} $nk] {
	if [string match {} $args] {
	    $this qSend WHOWAS :$nk
	} {
	    $this qSend WHOWAS $nk :[lindex $args 0]
	}
    }
}
#
proc net_INFO {this args} {
    if [string match {} $args] {$this q1Send INFO} {$this qSend INFO :[lindex $args 0]}
}
#
proc net_ISON {this} {
    foreach x [$this friends] {	if [$x notify] { append ns " [$x name]"}}
    if [info exists ns] { $this qSend ISON :$ns }
}
#
proc net_SQUIT {this srv} { $this qSend SQUIT :$srv }
#
proc net_TIME {this nk} { $this qSend TIME :$nk }
#
proc net_PRIVMSG {this where what} { $this qSend PRIVMSG $where :$what }
#
proc net_NOTICE {this where what} {
    if {$where != {} && $what != {}} { $this qSend NOTICE $where :$what }
}
#
proc net_INVITE {this who where} {
    if {$who != {} && $where != {}} { $this qSend INVITE $who :$where }
}
#
proc net_KILL {this who why} { $this qSend KILL $who :$why }
#
proc net_KICK {this where who msg} { $this qSend KICK $where $who :$msg}
#
proc net_STATS {this p1 p2} { $this qSend STATS $p1 :$p2 }
#
proc net_USERHOST {this nk} { $this qSend USERHOST :$nk }
#
proc net_NICK {this name} {
    if [$this startup] {
	$this setNickname $name
    }
    $this qSend NICK :$name
}
proc net_MODE {this who mode args} {
    switch [llength $args] {
    0 {	$this qSend MODE $who :$mode }
    1 {	$this qSend MODE $who $mode :[lindex $args 0] }
    * { error "MODE Called with too many parameters" }
    }
}
#
proc net_CONNECT {this srv port remote} {
    if ![string match {} $srv] {$this qSend CONNECT $srv $port :$remote }
}
#
proc net_LINKS {this srv mask} {
    if [string match {} $srv] {
	if [string match {} $mask] {$this q1Send LINKS} {
	  $this qSend LINKS [$this host] :$mask
	}
    } {
	if [string match {} $mask] {$this qSend LINKS :$srv} {
	  $this qSend LINKS $srv :$mask
	}
    }
}
#
proc net_nsend {this cmd par} {
    if [string match {} $par] {$this q1Send $cmd} {$this qSend $cmd :$par}
}
#
proc net_AWAY {this args} {
    if [string match {} $args] {$this q1Send AWAY} {$this qSend AWAY :[join $args]}
}
#
proc net_TOPIC {this chan args} {
    if [string match {} $args] {$this qSend TOPIC :$chan} {
	$this qSend TOPIC $chan :[lindex $args 0]
    }
}
#
proc net_CTCP {this cmd nk str} {$this qSend PRIVMSG $nk ":\001$cmd $str\001"}
#
proc net_PART {this chan args} {
    if [string match {} $args] {$this qSend PART :$chan} {
	$this qSend PART $chan :[lindex $args 0]
    }
}
#
proc net_OPER {this nk str} {
    if ![string match {} $str] { $this qSend OPER  $nk :$str }
}
#
proc net_NAMES {this chan} { $this qSend NAMES :$chan }
#
proc net_PING {this srv} { $this qSend PING :$srv}
#
proc net_error {this prefix param pargs} {
    set hst [$this host]
    if [$this startup] {
	set msg "Cannot connect to $hst : $param"
    } {
	set msg "Closing connection to $hst, ERROR : $param"
    }
    $this close $msg
}

