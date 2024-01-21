#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/debug.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
# ----------------------------------------------------------------------
#   AUTHOR:  Lindsay Marshall <lindsay.marshall@newcastle.ac.uk>
# ----------------------------------------------------------------------
# Copyright 1995 The University of Newcastle upon Tyne (see COPYRIGHT)
# ======================================================================
#
proc zWind {ctl title icon dest save} {
    toplevel $ctl -class Zircon -borderwidth 2
    set oFrm $ctl
    wm title $ctl $title
    wm iconname $ctl $icon
    wm resizable $ctl 1 1
    wm protocol $ctl WM_DELETE_WINDOW "destroy $ctl"
    set f [frame $ctl.btn]
    button $f.close -text Dismiss -command $dest -width 8
    button $f.save -text Save -state disabled -width 8 -command $save
    button $f.clear -text Clear -width 8 -command "zWClear $ctl.t.txt"
    button $f.dump -text Dump -width 8 -command "zDump /tmp/zircon.dump"
    button $f.kill -text Kill -width 8 -command  zKill
    pack $f.close $f.save $f.clear $f.dump $f.kill -expand 1 -side left
    pack $f -fill x
    set f [frame $ctl.t]
    scrollbar $f.vs -command "$f.txt yview"
    text $f.txt -yscrollcommand "$f.vs set" -height 10 -width 40 \
      -takefocus 0 -state disabled
    bindtags $f.txt ROText
    $f.txt tag configure input -foreground red
    $f.txt tag configure output -foreground black
    pack $f.vs -side right -fill y
    pack $f.txt -side left -expand 1 -fill both
    pack $f -fill both -expand 1
    tkwait visibility $ctl
}
#
proc zshow {line} {
    if [winfo exists .@zdbg] {
	.@zdbg.t.txt configure -state normal
	.@zdbg.t.txt insert end ">> $line\n"
	.@zdbg.t.txt configure -state disabled
    }
}
#
proc zKill {} { exit }
#
proc zWClear {txt} {
    $txt configure -state normal
    $txt delete 1.0 end
    $txt configure -state disabled
}
#
proc zdebug {} {
    global DBGPos
    set ctl .@zdbg
    if ![winfo exists $ctl] {
	zWind $ctl {Zircon Debug Window} {Zircon Debug} "destroy $ctl" {}
	emacsEntry $ctl.entry -relief sunken
	pack $ctl.entry -fill x
	bind $ctl.entry <Return> zDbgDo
	bind $ctl.entry <Control-p> "zDbgHist 2 %W"
	bind $ctl.entry <Up> [bind $ctl.entry <Control-p>]
	bind $ctl.entry <Control-n> "zDbgHist -2 %W"
	bind $ctl.entry <Down> [bind $ctl.entry <Control-n>]
	bind $ctl <Enter> "focus $ctl.entry"
	set DBGPos end
    } {
	raise $ctl
	wm deiconify $ctl
    }
    focus $ctl.entry
    catch {wm geometry .@zdbg $zlayout(default,debug)}
}
#
proc zDbgHist {inc ent} {
    global DBGPos
    $ent delete 0 end
    set rng [.@zdbg.t.txt tag ranges input]
    set foo [lindex $rng $DBGPos]
    if ![string compare $DBGPos end] {
	if [string match {} $foo] return
	set DBGPos [expr [llength $rng] - 1]
    }
    incr DBGPos $inc
    if {$DBGPos <= 0} { set DBGPos end }
    if {$DBGPos >= [llength $rng]} { set DBGPos 1 }
    set idx [lindex [split $foo .] 0]
    $ent insert insert [.@zdbg.t.txt get $idx.2 "$idx.0 lineend"]
}
#
proc zDbgDo {} {
    global DBGPos
    set cmd [.@zdbg.entry get]
    catch {uplevel #0 $cmd} msg
    .@zdbg.t.txt configure -state normal
    .@zdbg.t.txt insert end {% }
    .@zdbg.t.txt insert end $cmd input
    .@zdbg.t.txt insert end "\n"
     if ![string match {} $msg] { .@zdbg.t.txt insert end "$msg\n" output }
    .@zdbg.t.txt configure -state disabled
    .@zdbg.t.txt see end
    .@zdbg.entry delete 0 end
    set DBGPos end
}
#
proc zError {msg cmd prefix param rest} {
    global errorInfo tk_patchLevel zircon
    set file /tmp/zirc[pid]
    set foo [open $file a]
    puts $foo "------Error: zircon $zircon(version) $zircon(patchlevel) tcl [info patchlevel] tk $tk_patchLevel [version]"
    puts $foo "Message: $msg"
    puts $foo "Processing: $prefix $cmd $rest :$param"
    puts $foo $errorInfo
    close $foo
    mkInfoBox ZERROR .@zrun {Internal Error} \
      "Zircon has detected an internal error \"$msg\" when processing\
      \"$cmd\" from \"$prefix\" ($param $rest). The stack trace has\
      been saved in file $file. Please send this information to\
      zircon@catless.ncl.ac.uk."
}
#
proc tkerror {err} { zError $err INTERNAL {} {} {} }
#
proc bgerror {err} { zError $err INTERNAL {} {} {} }
#
proc zTrace {} {
    global monitorIn monitorOut dbgProcs zlayout
    set ctl .@ztrace
    if [winfo exists $ctl] {
	if {!$monitorIn && !$monitorOut} {destroy $ctl}
    } elseif {$monitorIn || $monitorOut} {
	zWind $ctl {Zircon Trace Window} {Zircon trace} zNoTrace {}
	foreach x [info procs dbg_*] {
	    set n [string range $x 4 end]
	    rename $n nrm_$n
	    rename $x $n
	}
	bind $ctl <Destroy> {
	    foreach x [info procs nrm_*] {
		set n [string range $x 4 end]
		rename $n dbg_$n
		rename $x $n
	    }
	}
    }
    catch {wm geometry .@ztrace $zlayout(default,trace)}
}
#
proc zNoTrace {} {
    global monitorIn monitorOut
    set monitorIn 0
    set monitorOut 0
    destroy .@ztrace
}
#
proc zIn {line} {
    global current
    .@ztrace.t.txt configure -state normal
    .@ztrace.t.txt insert end "$current(net)>$line\n" input
    .@ztrace.t.txt configure -state disabled
    .@ztrace.t.txt see end
}
#
proc zOut {line} {
     global current
    .@ztrace.t.txt configure -state normal
    .@ztrace.t.txt insert end "$current(net)<$line\n" output
    .@ztrace.t.txt configure -state disabled
    .@ztrace.t.txt see end
}
#
proc zDump {file} {
    set fd [open $file w]
    foreach _x [lsort [info globals]] {
	global $_x
	if [string match auto_* $_x] continue
	if [array exists $_x] {
	    foreach _v [lsort [array names $_x]] {
		puts $fd "${_x}($_v) : {[set ${_x}($_v)]}"
	    }
	} {
	    puts $fd "$_x : {[set $_x]}"
	}
    }
    close $fd
}
#
proc dbg_net_send {this op args} {
    global monitorOut
    upvar #0 $this ndata
    if ![string match {} $ndata(sock)] {
	set msg $op
	if ![string match {} [set last :[lindex $args end]]] {
	    if ![catch {set foo [lreplace $args end end]}] {
		append msg " $foo $last"
	    }
	}
	if $monitorOut { zOut $msg }
	if [catch {ircsend $ndata(sock) $msg}] { $this close }
    }
}
#
proc dbg_net_qSend {this op args} {
    global monitorOut
    upvar #0 $this ndata
    set msg "$op [join $args]"
    if $monitorOut { zOut $msg }
    if [catch {ircsend $ndata(sock) $msg}] { $this close }
}
#
proc dbg_net_q1Send {this op} {
    global monitorOut
    upvar #0 $this ndata
    if $monitorOut { zOut $op }
    if [catch {ircsend $ndata(sock) $op}] { $this close }
}
#
proc dbg_connect {host port} {
    global monitorOut
    if $monitorOut { zOut "**** Connecting to $host:$port" }
    if [catch {nrm_connect $host $port} msg] {
	if $monitorOut { zOut "**** Connect error - $msg" }
	error $msg
    }
    return $msg
}
