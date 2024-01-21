#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Info.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
# Build a Zircon Information Window.
#
class Info {
    net		{}
    icon	{}
}
#
proc clSet {fr f l} {
    if {![string compare $f 0.0] && ![string compare $l 1.0]} {
	catch "pack forget $fr"
    } {
	catch "pack $fr -side bottom -fill x"
	$fr set $f $l
    }
}	
#
proc Info {name args} {
    if [string match {::} $name] {
	return [eval Info_[lindex $args 0] [lrange $args 1 end] ]
    }
    global Icon BF Fg Bg Ft Bl OType Name defChan zircon ztrans
    initObj $name Channel Info
    proc $name {args} "eval info_call $name \$args"
    set w .$name
    set OType($name) Info
    upvar #0 $name idata
    set idata(net) [lindex $args 1]
    set Name($w) $name
    $name configure -hpos end -open [$idata(net) popInfo] \
      -close [$defChan close] -jump 1 -quiet 0 -draw 0 \
      -msg [$defChan msg] -actionmode 0 \
      -actions 0 -patterns {} -logfile {} -history [$defChan history] \
      -closetime [$defChan closetime] -log {} -menu 0 -join 0 \
      -ops {} -keep 1 -monitor 0
    toplevel $w -borderwidth 0 -class Zircon
    wm title $w {Zircon Information Window}
    wm iconname $w [set idata(icon) {Zircon Info}]
    wm protocol $w WM_DELETE_WINDOW "$idata(net) quit"
    set f [frame $w.cmdLine -borderwidth 0]
    scrollbar $f.cscroller -orient horizontal -command "$f.commandLine xview"
    emacsEntry $f.commandLine -xscrollcommand "clSet $f.cscroller" \
      -relief sunken
    button $f.clear -text $ztrans(clear) -command "$name clear" -width 5 \
      -borderwidth 2 -highlightthickness 2 -pady 0
    pack $f.clear -side right  -anchor ne
    pack $f.commandLine -side top -fill x
    if $zircon(command) { pack $f -fill x -side bottom}
    doInfoBindings $f.commandLine $name
    bind $f.commandLine <Return> "$name doCmd %W"
    set fr [frame $w.cFrm -borderwidth 0]
    frame $fr.cFrm -relief flat -borderwidth 0
    set oft $fr.info
    scrollbar $fr.vscroller -command "doScroll $oft"
    text $oft -height 10 -width 80 \
      -yscrollcommand "setScroll $oft $fr.vscroller" -borderwidth 2
    rebind $oft $idata(net)
    bind $oft <Configure> {%W see end ; notIdle %W}
    bind $fr <Visibility> {notIdle %W}
    pack $fr.vscroller -side right -fill y -in $fr.cFrm
    pack $oft -side left -expand 1 -fill both -in $fr.cFrm
    pack $fr.cFrm -expand 1 -fill both
    pack $w.cFrm -expand 1 -fill both
    set BF($name) [getOValue $oft font boldFont Font]
    set Fg($name) [getOValue $oft foreground foreground Foreground]
    set Bg($name) [getOValue $oft background background Background]
    set Ft($name) [getOValue $oft font font Font]
    set Bl($name) [option get $oft bell Bell]
    confTag $oft {} $Fg($name) $Bg($name) $Ft($name) $BF($name)
    $name configure -window $w
    tkwait visibility $oft
    return $name
}
#
proc info_clear {this} {
   [$this text] delete 1.0 end
   update idletasks
}
#
proc info_doCmd {this w} {
    global zircon monitorOut 
    set net [$this net]
    if $zircon(raw) {
	if ![string match {} [set sock [$net sock]]] {
	    set line [$w get]
	    $this configure -hpos end
	    $this addText {} ":>$line"
	    if $monitorOut { zOut $line }
	    if [catch {ircsend $sock $line} msg] { $net close $msg }
	    $w delete 0 end
	}
    } {
	$net doMisc2 $this $w
    }
}
#
proc info_call {this op args} {
    upvar #0 $this idata
    switch $op {
    lname -
    name { return $this }
    crypt { return {} }
    control { return [$idata(net) control] }
    text { return [$this window].cFrm.info }
    display {return [eval channel_call $this addText $args] }
    command {
	    if [lindex $args 0] {
		pack $idata(window).cmdLine
	    } {
		pack $idata(window).cmdLine -forget
	    }
	    return
	}
    window { return $idata(window) }
    }
    if [info exists idata($op)] { return $idata($op) }
    if [string match {} [info procs info_$op]] {
	return [eval channel_call $this $op $args]
    }
    return [eval info_$op $this $args]
}
#
proc info_popup {this} {
    popup [$this window]
    $this extendTime
    handleOn POPINFO [list [$this lname]]
    return $this
}
#
proc info_insert {this text} {
    if {$text != {}} {
	set ent [$this window].cmdLine.commandLine
	while {[regexp "(\[^\n\]*)\n(.*)" $text d line text]} {
	    if [string match {} $line] continue
	    tkEntryInsert $ent $line
	    $this doCmd $ent
	    $ent delete 0 end
	}
	if ![string match {} $text] {
	    $ent insert insert $text
	    tkEntrySeeInsert $ent
	}
	$this configure -hpos end
    }
}
