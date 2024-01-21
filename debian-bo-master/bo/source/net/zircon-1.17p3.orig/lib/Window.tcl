#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Window.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
class Window {
    name	{}
    icon	{}
    iconbm	{}
}
#
proc Window {name args} {
    global WTO
    set ln [string tolower $name]
    set this [objName Window]
    initObj $this Window
    set WTO($ln) $this
    upvar #0 $this wdata
    set wdata(name) $ln
    proc $this {args} "eval window_call $this \$args"
    toplevel $ln -class Zircon
    return $this
}
#
proc window_call {this op args} {
    upvar #0 $this wdata
    if [info exists wdata($op)] { return $wdata($op) }
    return [eval window_$op $this $args]
}
#
proc window_setIcon {this chan title} {
    global Icon
    set win [$this name]
    wm iconname $win $title
    set Icon($win) $title
    if ![string match {} [set icn [$chan icon]]] {
	global IconBM
	set IconBM($win) $icn
	wm iconbitmap $win [lindex $icn 0]
    }
}
#
proc window_popup {this} {
   wm [set w [$this name]] deiconify
   raise $w
}
#
proc window_delete {this} {
    global WTO Icon IconBM Otype
    set w [$this name]
    bind $w.cFrm <Destroy> {}
    catch {unset WTO($w) Icon($w) IconBM($w)}
    foreach v {OType } { catch {unset ${v}($this)} }
    upvar #0 $this wdata
    unset wdata
    rename $this {}
    destroy $w
}
#
proc window_iconify {this} { wm iconify [$this name] }
#
proc window_configure {this args} {
    upvar #0 $this wdata
    while {![string match {} $args]} {
	set val [lindex $args 1]
	set name [lindex $args 0]
	set opt [string range $name 1 end]
	switch -glob -- $name {
	-title -
	-iconname -
	-iconbitmap { wm $opt [$this name] $val }
	-geometry -
	-grid -
	-resizable -
	-maxsize -
	-minsize { eval wm $opt [$this name] $val }
	-*  { set wdata($opt) $val }
	}
	set args [lrange $args 2 end]
    }
}
#
proc window_build {this args} {
    upvar #0 $this wdata
    set w $wdata(name)
    frame $w.f1
    set f [frame $w.f2 -borderwidth 0]
    label $f.label -text {} -width 10
    pack $f.label -side left
    set f [frame $f.cmdLine -borderwidth 0]
    scrollbar $f.cscroller -orient horizontal -command "$f.commandLine xview"
    emacsEntry $f.commandLine -xscrollcommand "$f.cscroller set"
    pack $f.commandLine $f.cscroller -expand 1 -fill x
    pack $w.f2.cmdLine -side left -fill x -expand 1
    pack $w.f1 -fill both -expand 1
    pack $w.cmdLine -fill x -expand 1
    
}
#
proc Window_find {name} {
    global WTO
    set ln [string tolower name]
    return [expr {[info exists WTO($ln)] ? $WTO($ln) : {nil}}]
}
#
proc Window_make {name} {
    global WTO
    set ln [string tolower name]
    return [expr {[info exists WTO($ln)] ? $WTO($ln) : [Window $name]}]
}







