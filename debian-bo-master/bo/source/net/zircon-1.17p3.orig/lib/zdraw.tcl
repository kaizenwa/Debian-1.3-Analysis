#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/zdraw.tcl,v $
# $Date: 1996/07/01 13:13:39 $
# $Revision: 1.17.1.2 $
#
proc zdraw {usr cmd} {
    if ![string compare nil [set this [find [lindex $cmd 0]]]] {
	[$usr net] inform "Bad draw command - {$cmd}"
	return
    }
    if [$this draw] {
	$this makeZDraw
	set w .@zd$this.pic.canvas
	switch [set lnm [$usr lname]] {
	all -
	current { set lnm ,$v }
	}
	switch [lindex $cmd 1] {
	delete {
		switch -exact -- [set v [string tolower [lindex $cmd 2]]] {
		*all* {	if [$this isOp $usr] { set v all } { set v $lnm } }
		all -
		current { set v ,$v }
		}
		if {![string compare $v all] || ![string compare $v $lnm]} { $w delete $v }
	    }
	default {
		regsub -all {[][$;]} [lrange $cmd 1 end] {} dc
		if ![catch {set tid [eval $w $dc]} msg] {
		    $w itemconfigure $tid -tags $lnm
		}
	    }
	}
    }
}

proc zdDestroy {win} {
    destroy $win
    catch {uplevel #0 unset Z$win}
}

proc zdModeChange {type win} {
    upvar #0 Z$win zdata
    if [info exists zdata(mode)] {
	$win.plt.$zdata(mode) configure -relief raised
    }
    set zdata(mode) $type
    foreach v {start last points} { set zdata($v) {} }
    $win.pic.canvas delete @r
    $win.pic.canvas delete @p
    $win.plt.$type configure -relief sunken
}

proc clButtons win {
    global zircon
    button $win.oln.none -bitmap @$zircon(lib)/bitmaps/none.xbm \
      -command "zdColChange oln $win oln none" -relief raised \
      -height 24 -width 24
    button $win.fill.none -bitmap @$zircon(lib)/bitmaps/none.xbm \
      -command "zdColChange fill $win fill none" -relief raised \
      -height 24 -width 24
    pack $win.oln.none -side left -expand 1 -fill x
    pack $win.fill.none -side left -expand 1 -fill x
    foreach cl {black white red orange yellow green blue violet} {
	button $win.oln.$cl -background $cl \
	  -command "zdColChange oln $win oln $cl" \
	  -relief raised
	button $win.fill.$cl \
	  -background $cl -command "zdColChange fill $win fill $cl" \
	  -relief raised 
	pack $win.oln.$cl -side left -expand 1 -fill both
	pack $win.fill.$cl -side left -expand 1 -fill both
    }
}
#
proc zdColChange {var win frame cl} {
    upvar #0 Z$win zdata
    if [info exists zdata($var)] {
	$win.$frame.$zdata($var) configure -relief raised
    }
    set zdata($var) $cl
    $win.$frame.$cl configure -relief sunken
}
#
proc channel_zClearAll {this} {
    if [$this operator] { $this zdDo delete *ALL*  } { $this zdClear }
}
#
proc channel_makeZDraw {this} {
    set win .@zd$this
    if [winfo exists $win] { popup $win ; return }
    set chan [$this name]
    toplevel $win -class Zircon
    wm title $win "${chan} Sketch Pad"
    wm resizable $win 1 1
    wm protocol $win WM_DELETE_WINDOW "zdDestroy $win"
    set f0 [frame $win.btn -relief raised]
    button $f0.save -text Save
    button $f0.print -text Print -command "$this zdPrint"
    button $f0.clear -text Clear -command "$this zdClear"
    bind $f0.clear <Shift-ButtonRelease-1> "
	set sc \[lindex \[%W configure -command\] 4\]
	%W configure -command {}
	tkButtonUp %W
	uplevel #0 $this zClearAll
	%W configure -command \$sc
    "
    button $f0.quit -text Quit -command "zdDestroy $win"
    pack $f0.save $f0.print  $f0.clear  $f0.quit -side left -expand 1 -fill x
    set fp [frame $win.plt -relief raised]
    global zircon
    foreach t {line arc polygon rectangle oval text} {
	button $fp.$t -bitmap @$zircon(lib)/bitmaps/${t}.xbm \
	  -command "zdModeChange $t ${win}" -height 48 -width 48 \
	  -relief raised
	pack $fp.$t -side left -expand 1 -fill x
    }
    frame $win.oln -relief raised
    label $win.oln.label -text Outline -width 10
    frame $win.fill -relief raised
    label $win.fill.label -text Fill -width 10
    pack $win.oln.label $win.fill.label -side left
    clButtons $win
    set f1 [frame $win.pic -relief raised]
    set f2 [frame $win.hsFrm]
    scrollbar $f1.vscroller -command "$f1.canvas yview" 
    canvas $f1.canvas -yscrollcommand "$f1.vscroller set" \
      -xscrollcommand "$f2.hscroller set" -background white
    pack $f1.canvas -side left -expand 1 -fill both
    pack $f1.vscroller -side right -fill y
    scrollbar $f2.hscroller -command "$f1.canvas xview" -orient horizontal
    frame $f2.pf0
    pack $f2.hscroller -side left -expand 1 -fill x
    pack $f2.pf0 -side right -padx 20
    pack $win.btn $win.plt $win.oln $win.fill -fill x
    pack $win.pic -expand 1 -fill both
    pack $win.hsFrm -fill x
    zdModeChange line $win
    zdColChange oln $win oln black
    zdColChange fill $win fill black
    bind $f1.canvas <1> " $this zdPress 1 %x %y "
    bind $f1.canvas <Double-1> "$this zdDouble 1 %x %y "
    bind $f1.canvas <B1-Motion> { zdMove %W 1 %x %y }
    bind $f1.canvas <ButtonRelease-1> "$this zdUp 1 %x %y "
}

proc channel_zdPrint {this} {
    .@zd$this.pic.canvas postscript -file /tmp/[$this name].ps
}

proc channel_zdSave {this chan} {
    global ztrans
    mkFileBox .@zs$this .* "Save Sketch ${chan}" {}\
      "Save ${chan} sketch pad to:" \
      "$ztrans(ok) {.@zd$this.pic.canvas postscript -file }" \
      "$ztrans(cancel) {}"
}

proc channel_zdDo {this args} {
    set myid [[$this net] myid]
    set w .@zd$this.pic.canvas
    switch [lindex $args 0] {
    delete {
	    if {[set v [string tolower [lindex $args 1]]] == {*all*}} {
		set v all
	    }
	    $w delete $v
	}
    default {
	    set tid [eval $w [join $args]]
	    $w itemconfigure $tid -tags [$myid lname]
	}
    }
    set chan2 [expr {[$this isa Channel] ? [$this name] : [$myid name]}]
    [$this net] CTCP ZIRCON [$this name] "DRAW ${chan2} [join $args]"
}
#
proc channel_zdClear {this} {
    $this zdDo delete [[$this net] nickname]
}
#
proc checkNone {val} { return [expr {$val == {none} ? {{}} : $val}] }
#
proc channel_zdPress {this btn x y} {
    set w .@zd$this
    upvar #0 Z$w zdata
    set can $w.pic.canvas
    set x [$can canvasx $x]
    set y [$can canvasy $y]
    if [string match {} $zdata(start)] {
	set zdata(last) [set zdata(start) [list $x $y]]
	return
    }
    set fill [checkNone $zdata(fill)]
    set oln [checkNone $zdata(oln)]
    switch -exact -- $zdata(mode) {
    arc -
    oval -
    rectangle {
	    $this zdDo create $zdata(mode) [join $zdata(start)] $x $y \
	    -fill $fill -outline $oln
	    set zdata(last) [set zdata(start) {}]
	}
    line {
	    $this zdDo create $zdata(mode) [join $zdata(start)] $x $y \
	      -fill $oln
	    set zdata(last) [set zdata(start) {}]
	}
    polygon {
	    eval ${w}.pic.canvas create line [join $zdata(last)] $x $y \
	      -tags @p -fill $oln
	    lappend zdata(points) "$x $y"
	    set zdata(last) [list $x $y]
	}
    text {
	    global ztrans
	    mkEntryBox .@[newName text] Text {Enter your text:} \
	      "{$ztrans(text) {}}" "$ztrans(ok) {zAddText $this $w $x $y}" \
	      "$ztrans(cancel) {}"
	}
    }
}
#
proc zAddText {this w x y txt} {
    upvar #0 Z$w zdata
    set can .@zd$this.pic.canvas
    set x [$can canvasx $x]
    set y [$can canvasy $y]
#    if [string match {none} [set oln $zdata(oln)]] {set oln {{}} }
    if ![string match {none} [set fill $zdata(fill)]] {
	$this zdDo create text $x $y -text "{$txt}" -fill $fill
    }
}
#
proc zdMove {win btn x y} {
    set w [winfo toplevel $win]
    upvar #0 Z$w zdata
    set can $w.pic.canvas
    set x [$can canvasx $x]
    set y [$can canvasy $y]
    if {[string match {} $zdata(start)] || $zdata(start) == [list $x $y]} return
    set fill [checkNone $zdata(fill)]
    set oln [checkNone $zdata(oln)]
    $win delete @r
    switch -exact -- $zdata(mode) {
    arc -
    oval -
    rectangle {
	    eval ${win} create $zdata(mode) [join $zdata(start)] $x $y \
	      -tags @r -fill $fill -outline $oln
	}
    line {
	    eval ${win} create line [join $zdata(start)] $x $y \
	      -tags @r -fill $oln
	}
    polygon {
	    eval ${win} create line [join $zdata(last)] $x $y -tags @r \
	      -fill $oln
	}
    text {
	}   
    }
}
#
proc channel_zdUp {this btn x y} {
    set w .@zd$this
    upvar #0 Z$w zdata
    set can .@zd$this.pic.canvas
    set x [$can canvasx $x]
    set y [$can canvasy $y]
    if {![string match {} $zdata(start)] && $zdata(start) != [list $x $y]} {
	set fill [checkNone $zdata(fill)]
	set oln [checkNone $zdata(oln)]
	set win $w.pic.canvas
	$win delete @r
	switch -exact -- $zdata(mode) {
	arc -
	oval -
	rectangle {
		$this zdDo create $zdata(mode) [join $zdata(start)] $x $y \
		  -fill $fill -outline $oln
		set zdata(last) [set zdata(start) {}]
	    }
	line {
		$this zdDo create $zdata(mode) [join $zdata(start)] $x $y \
		  -fill $oln
		set zdata(last) [set zdata(start) {}]
	    }
	polygon {
		eval ${win} create line [join $zdata(last)] $x $y -tags @r \
		  -fill $oln
		set zdata(last) [list $x $y]
	    }
	text {
	    }    
	}
    }
}

proc channel_zdDouble {this btn x y} {
    set w .@zd$this
    upvar #0 Z$w zdata
    set can $w.pic.canvas
    set x [$can canvasx $x]
    set y [$can canvasy $y]
    if [string match {} $zdata(start)] {
	set zdata(last) [set zdata(start) [list $x $y]]
	return
    }
    set fill [checkNone $zdata(fill)]
    set oln [checkNone $zdata(oln)]
    switch -exact -- $zdata(mode) {
    arc -
    oval -
    rectangle {
	    $this zdDo create $zdata(mode) [join $zdata(start)] $x $y \
	      -fill $fill -outline $oln
	    set zdata(last) [set zdata(start) {}]
	}
    line {
	    $this zdDo create $zdata(mode) [join $zdata(start)] $x $y \
	      -fill $oln
	    set zdata(last) [set zdata(start) {}]
	}
    polygon {
	    $this zdDo create polygon [join $zdata(start)] \
	      [join $zdata(points)] $x $y -fill $fill
	    ${w}.pic.canvas delete @p
	    if {$oln != {{}}} {
		$this zdDo create line [join $zdata(start)] \
		  [join $zdata(points)] $x $y [join $zdata(start)] \
		  -fill $oln
	    }
	    set zdata(points) {}
	    set zdata(last) [set zdata(start) {}]
	}
    text {
	}
    }
}
