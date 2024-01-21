#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/dcc.tcl,v $
# $Date: 1996/07/01 09:28:18 $
# $Revision: 1.17.1.3 $
#
global dccInfo
set dccInfo {}
#
proc DCCSend {usr file} {
    if [string match {} $file] return
    if [file exists $file] {
	if ![file readable $file] {
	    mkDialog ERROR .@fe {File error} "Cannot read file $file." {}
	    return
	}
	set file [glob $file]
	set xfile [file tail $file]
	global zircon Offer monitorOut
	if $monitorOut { zOut "! dccsend $file 00 $usr" }
	if [catch {set fd [open "|$zircon(lib)/dccsend $file 00 $usr" r]}] {
	    mkDialog ERROR .@fe {Prog error} "Cannot run DCC \
	      helper program dccsend. Check your installation" {}
	    return
	}
	gets $fd port
	[$usr net] CTCP DCC [$usr name] \
	  "SEND $xfile [ipPack [ipAddress]] [lindex $port 0] [file size $file]"
	if ![info exists Offer($usr)] { $usr ref }
	lappend Offer($usr) [list [lindex $port 1] $file $fd]
	set net [$usr net]
	fileevent $fd readable "handleInfo $net $fd"
	if [winfo exists .@dls$net] { buildDCCList $net }
    } {
	mkDialog ERROR .@fe {File error} "File $file does not exist." {}
    }
}
#
proc doDCC {cmd nk} {
    if {![string match {} $nk] && ![string match {[#&]*} $nk]} {[User :: make $nk] dcc $cmd}
}
#
proc closeChat {cht who conn} {
    catch {clearHandler $conn ; close $conn}
    catch {uplevel #0 unset $conn}
    if ![string match {} [info proc $cht]] {
	$cht addText $who "*** $who has closed the connection"
    }
}
#
proc dccChat {mode conn} {
    global monitorIn
    upvar #0 $conn chdata
    set who [$chdata(who) name]
    set cht $chdata(obj)
    switch $mode {
    r   {
	    if {[catch {gets $conn} buffer] || 
	      ([string match {} $buffer] && [eof $conn])} {
		closeChat $cht $who $conn
	    } {
		if $monitorIn { zIn "= $buffer" }
		regsub -all "\r" $buffer {} buffer
		$cht addText $who "=$who= $buffer"
	    }
	}
    e   {  [$chData(who) net] errmsg "Error on DCC Chat connection with $who" }
    }
}

proc handleInfo {net conn} {
    if {[catch {gets $conn} msg] || ([string match {} $msg] && [eof $conn])} {
	catch {close $conn}
    } {
	global Offer Send Get monitorIn
	if $monitorIn {zIn "! $msg"}
	set sp [split $msg { }]
	set who [lindex $sp 5]
	set pid [lindex $sp 0]
	set msg [join [lreplace [lrange $sp 1 end] 4 4 [$who name]]]
	switch -glob -- $msg {
	{DCC Get conn*} { dccGetWin $conn $who ; return }
	{DCC Send acc*} { return }
	{DCC Send conn*} {
		set x [lsearch $Offer($who) "$pid *"]
		lappend Send($who) [lindex $Offer($who) $x]
		listdel Offer($who) $x
		if [string match {} $Offer($who)] { unset Offer($who) ; $who deref }
		if [winfo exists .@dls$net] { buildDCCList $net }
		dccWindow $conn $who
		return
	    }
	{DCC Get prog*} {dccGetProg $conn [lindex $sp 6] ; return}
	{DCC Send prog*} {dccProgress $conn [lindex $sp 6] ; return}
	{DCC Send*} -
	{DCCError Send*} {
		set x [lsearch $Send($who) "$pid *"]
		listdel Send($who) $x
		if [string match {} $Send($who)] { unset Send($who) ; $who deref} 
		if [winfo exists .@dls$net] { buildDCCList $current(net) }
		catch {destroy .@$conn}
	    }
	{DCCError Get*} -
	{DCC Get*} {
		set x [lsearch $Get($who) "$pid *"]
		listdel Get($who) $x
		if [string match {} $Get($who)] { unset Get($who) ; $who deref}
		if [winfo exists .@dls$net] { buildDCCList $current(net) }
	    }
	default {
		set msg "WEIRD ERROR : $msg"
	    }
	}
	mkInfoBox DCCINFO .@dcc$conn {DCC Info} $msg
    }
}
#
proc doGetDCC {net wh usr addr port args} {
    if [catch {set host [dectonet $addr]}] return
    if ![string compare Chat $wh] {
	if [catch {connect $host $port} sok] {
	    $net display {} "*** Cannot connect to host $host ($sok)"
	    return 0
	}
	catch {chatBuffer $sok}
	upvar #0 $sok chdata
	set chdata(who) $usr
	[set chdata(obj) [set this [Chat [$usr name] -caller $usr]]] show
	$this addUser $chdata(who) 0 0
	$this configure -sock $sok
	handler $sok re dcc${wh}
    } {
	if {[file exists [set file [lindex $args 0]]] && 
	  ![file writable $file]} {
	    mkDialog {} .@fe {File error} "Cannot write file $file." \
	      {} {Dismiss {}}
	    return 0
	}
	global zircon Get monitorOut
	set file [file dirname $file]/[file tail $file]
	if $monitorOut { zOut "! dccget $host $port $file 00 $usr" }
	if [catch {open "|$zircon(lib)/dccget $host $port $file \
	  00 $usr" r} fd] {
	    mkDialog ERROR .@fe {Prog error} "Cannot run DCC \
	      helper program dccget - $fd. Check your installation" {} {Dismiss {}}
	    return 0
	} {
	    gets $fd pid
	    fileevent $fd readable "handleInfo $net $fd"
	    lappend Get($usr) [list $pid $file $fd]
	}
    }
    return 1
}
#
proc dccPick {net win y} {
    global DCCList ztrans
    notIdle {}
    if [string match {} [set t [$win curselection]]] { set t [$win nearest $y] }
    foreach x $t {lappend calls [lindex $DCCList($net) $t]}
    $win delete [lindex $t 0] [lindex $t end]
    foreach l $calls {
	set usr [lindex $l 1]
	set addr [lindex $l 2]
	set port [lindex $l 3]
	set fln [lindex $l 4]
	switch [set op [lindex $l 0]] {
	Send {
		set msg "DCC Send request ($fln) from [$usr name]"
		tkwait window [mkFileBox {} .* "DCC Send $fln" $msg $fln \
		  "$ztrans(accept) {doGetDCC $net Get $usr $addr $port}" \
		  "$ztrans(reject) {}"]
	    }
	default { doGetDCC $net $op $usr $addr $port {} }
	}
    }
    foreach x $t {
	[lindex [lindex $DCCList($net) $t] 1] deref
	listdel DCCList($net) $t
    }
    if [string match {} $DCCList($net)] { destroy .@drq$net }
}
#
proc dccDel {net win} {
    notIdle {}
    if ![string match {} [set t [$win curselection]]] {
	global DCCList
	$win delete [lindex $t 0] [lindex $t end]
	foreach x $t {
	    [lindex [lindex $DCCList($net) $t] 1] deref
	    listdel DCCList($net) $t
	}
	if [string match {} $DCCList($net)] { destroy .@drq$net }
    }
}
#
proc addDCCRequest {net op usr fln addr port} {
     global DCCList
     if ![winfo exists [set w .@drq$net]] {
	set DCCList($net) {}
	toplevel $w -class Zircon
	wm title $w {Incoming DCC Requests}
	makeLB $w.lb -setgrid 1 -width 40 -height 8 
	pack $w.lb
	bind $w.lb.l <Double-Button-1> "dccPick $net %W %y ; break"
	bind $w.lb.l <Delete> "dccDel  $net %W ; break"
	bind $w.lb.l <BackSpace> [bind %W <Delete>]
	bind $w.lb.l <Control-h> [bind %W <Delete>]
	bind $w.lb <Any-Enter> {focus %W.l ; notIdle {} ; break}
     }
     $w.lb.l insert end "[$usr name] : $op $fln"
     popup $w
     lappend DCCList($net) [list $op $usr $addr $port $fln]
     $usr ref
}
#
proc handleDCC {net usr param} {
    set pars [split $param]
    switch -exact -- [lindex $pars 1] {
    SEND {
	    if [string match ".*" [set fln [lindex $pars 2]]] {
		set fln _[string range $fln 1 end]
	    }
	    set addr [lindex $pars 3]
	    set port [lindex $pars 4]
	    addDCCRequest $net Send $usr $fln $addr $port
	}  
    CHAT { addDCCRequest $net Chat $usr {} [lindex $pars 3] [lindex $pars 4]	}
    }
}
#
proc ipPack {ip} {
    set val 0
    foreach x [split $ip "."] {	set val [expr {($val << 8) + $x}] }
    return [format %u $val]
}
#
proc dectonet {dec} {
    if {[string length $dec] == 10 && [set first [string index $dec 0]] > 1} {
	switch -exact -- $first {
	    2 {set overflow "0 148 53 119"}
	    3 {set overflow "0 94 208 178"}
	    4 {set overflow "0 40 107 238"}
	}
	set dec [string range $dec 1 end]
    } else {
	set overflow {0 0 0 0}
    }   

    scan [format "%08x" $dec] "%2x%2x%2x%2x" net(3) net(2) net(1) net(0)

    for {set part 0; set carry 0} {$part < 4} {incr part} {
	set sum [expr {$net($part) + [lindex $overflow $part] + $carry}]
	set internet($part) [expr {$sum % 256}]
	set carry [expr {$sum / 256}]
    }

    return "$internet(3).$internet(2).$internet(1).$internet(0)"
}
#
proc killDel {arr usr file} {
    global $arr
    set i 0
    foreach p [set ${arr}($usr)] {
	if ![string compare [lindex $p 1] $file] {
	    catch {exec kill [lindex $p 0]}
	    listdel ${arr}($usr) $i
	    if [string match {} [set ${arr}($usr)]] {
		unset ${arr}($usr)
		$usr deref
	    }
	    return
	}
	incr i
    }
}
#
proc dccClose {win} {
    foreach t [$win curselection] {
	set x [split [$win get $t]]
	set usr [User :: find [set who [lindex $x 2]]]
	set file [lindex $x 4]
	switch -glob -- $x {
	{Call -*} {$usr unChat }
	{Call from*} { }
	Chat* {catch {[Chat :: find $who] leave}}
	Offer* { killDel Offer $usr $file }
	Request* { }
	Send* { killDel Send $usr $file }
	Get* { killDel Get $usr $file}
	}
    }
    foreach t [$win curselection] { $win delete $t }
}
#
proc buildDCCList {net args} {
    global ztrans
    set w .@dls$net
    if [winfo exists $w] {
	popup $w
	if [string match {} $args] { $w.dcc.l delete 0 end }
    } {
	toplevel $w -class Zircon -relief raised -borderwidth 2
	wm title $w {DCC Connections}
	wm protocol $w WM_DELETE_WINDOW "destroy $w"
	makeLB $w.dcc -setgrid 1
	frame $w.btn
	button $w.btn.ok -text $ztrans(dismiss) -command "destroy $w" -relief raised
	button $w.btn.clear -text $ztrans(close) -relief raised \
	  -command "dccClose $w.dcc.l"
	pack $w.btn.ok $w.btn.clear -side left -expand 1 -fill x
	pack $w.dcc -fill both
	pack $w.btn -fill x
    }
    global AChat Offer Send Get
    foreach nn [array names AChat] { $w.dcc.l insert end "$ztrans(call) - [$nn name]" }
    foreach nn [$net chats] { $w.dcc.l insert end "$ztrans(chat) - [$nn name]" }

    foreach arr {Offer Send Get} {
	foreach nn [array names $arr] {
	   foreach fl [set ${arr}($nn)] {
		$w.dcc.l insert end "[trans $arr] - [$nn name] : [lindex $fl 1]"
	   }
	}
    }
}
#
proc usersDCC {net cmd} {
    global ztrans
    switch $cmd {
    List -
    Close { buildDCCList $net }
    default {
	    mkEntryBox .@$cmd $cmd "Enter user name for DCC $cmd:" \
	      "{$ztrans(user) {}}" \
	      "$ztrans(ok) {doDCC [string toupper $cmd]}" "$ztrans(cancel) {}"
	}
    }
}
#
proc dccWindow {conn usr} {
    global Send ztrans
    set w .@$conn
    toplevel $w -class Zircon
    set file [lindex $Send($usr) 1]
    wm title $w "DCC send $file to [$usr name]"
    wm protocol $w WM_DELETE_WINDOW {}
    frame $w.meter -relief raised
    scale $w.meter.slide -from 0 -to 100 -tickinterval 10 \
       -state disabled -length 400 -orient horizontal
    pack $w.meter.slide -padx 10 -pady 10 -fill x
    pack $w.meter -fill x 
    button $w.cancel -text $ztrans(cancel) \
      -command "killDel Send {$usr} {$file} ; destroy .@$conn"
    pack $w.cancel -fill x
}
#
proc dccGetWin {conn usr} {
    global Get ztrans
    set w .@$conn
    toplevel $w -class Zircon
    set file [lindex $Get($usr) 1]
    wm title $w "DCC get $file from [$usr name]"
    wm protocol $w WM_DELETE_WINDOW {}
    label $w.meter -relief raised -text {0 bytes received} -borderwidth 2
    pack $w.meter -fill x  -padx 10 -pady 10
    button $w.cancel -text $ztrans(cancel) \
      -command "killDel Get {$usr} {$file} ; destroy .@$conn"
    pack $w.cancel -fill x
}
#
proc dccProgress {conn sent} {
    set w .@$conn.meter.slide
    $w configure -state normal
    $w set $sent
    $w configure -state disabled
    update idletasks
}
#
proc dccGetProg {conn got} {
    set w .@$conn.meter
    $w configure -text "$got bytes received"
    update idletasks
}
