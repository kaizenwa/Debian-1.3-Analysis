#
# Configuration program for zircon created from code
# ----------------------------------------------------------------------
#   AUTHOR:  Lindsay Marshall <lindsay.marshall@newcastle.ac.uk>
# ----------------------------------------------------------------------
# Copyright (c) 1996 The University of Newcastle upon Tyne
#======================================================================
#
# This script created Fri May 17 16:19:44 BST 1996
#
proc ob_EClick {w x lab} {
    global refs config lastX ob_pat3 ob_pat2
    set lastX $x
    set x [$w index @$x]
    if $refs($lab) {
	set v $config($lab)
	while {[regexp -indices $ob_pat3 $v m sm]} {
	    if {$x >= [lindex $sm 0] && $x <= [lindex $sm 1]} {
		if {[set x [lindex $sm 0]] != 0} {incr x -1}
		break
	    }
	    regsub $ob_pat2 $v {@@\1@} v
	}
    }
    $w icursor $x
    $w select from $x
    if ![string compare [$w cget -state] normal] {focus $w}
}
#
proc ob_EDel {w lab} {
    global refs config ob_pat3 ob_pat2
    if $refs($lab) {
	set x [expr [$w index insert] - 1]
	if ![string compare [string index $config($lab) $x] "\}"] {
	    set v $config($lab)
	    while {[regexp -indices $ob_pat3 $v m sm]} {
		if ![string compare [lindex $sm 1] $x] {
		    $w delete [lindex $sm 0] $x
		    tkEntrySeeInsert $w
		    ob_changed
		    incr refs($lab) -1
		    return
		}
		regsub $ob_pat2 $v {@@\1@} v
	    }
	}
    }
    tkEntryBackspace $w
    tkEntrySeeInsert $w
    ob_changed
}
#
proc ob_EDelSel {w lab} {
    global refs config ob_pat3 ob_pat2
    if $refs($lab) {
	set sf [$w index sel.first]
	set sl [$w index sel.last]
	set v $config($lab)
	while {[regexp -indices $ob_pat3 $v m sm]} {
	    if {[lindex $sm 0] >= $sf && [lindex $sm 0] <= $sl} {incr refs($lab) -1}
	    regsub $ob_pat2 $v {@@\1@} v
	}
    }
    $w delete sel.first sel.last
    tkEntrySeeInsert $w
    ob_changed
}
#
proc ob_ERel {w x lab} {
    global refs config lastX ob_pat3 ob_pat2
    if ![string compare $x $lastX] { set lastX {} ; return }
    set lastX {}
    set x [$w index @$x]
    if $refs($lab) {
	set v $config($lab)
	while {[regexp -indices $ob_pat3 $v m sm]} {
	    if {$x >= [lindex $sm 0] && $x <= [lindex $sm 1]} {
		if {[set ix [$w index insert]] > $x} {
		    $w select to [lindex $sm 0]
		} {
		    $w select to [lindex $sm 1]
		}
		return
	    }
	    regsub $ob_pat2 $v {@@\1@} v
	}
    }
    $w select to $x
}
#
proc ob_FindLabel {lab} {
    global label
    foreach x [array names label] {
	if {![string match {!*} $x] && $label($x) == $lab} { return $x }
    }
    return {}
}
#
proc ob_LabelEntry {lab name args} {
    global config default label refs wlab window
    if ![info exists config($lab)] {
	if [info exists default($lab)] {
	    set config($lab) $default($lab)
	} {
	    set config($lab) {}
	    set default($lab) {}
	}
    }  elseif {![info exists default($lab)]} {
	set default($lab) $config($lab)
    }
    set refs($lab) [ob_RefCount $config($lab)]
    set wlab($name.entry) $lab
    set window($lab) $name
    frame $name
    label $name.label -text $label($lab)
    eval $name.label configure $args
    entry $name.entry -relief sunken -width 40 -textvariable config($lab)
    bind $name.label <Any-Enter> "ob_showHelp $lab"
    bind $name <Any-Enter> "ob_showHelp $lab"
    bind $name.entry <Delete> "ob_EDel %W $lab"
    bind $name.entry <BackSpace> "ob_EDel %W $lab"
    bind $name.entry <Control-h> "ob_EDel %W $lab"
    bind $name.entry <1> "ob_EClick %W %x $lab"
    bind $name.entry <ButtonRelease-1> "ob_ERel %W %x $lab"
    bind $name.entry <Shift-ButtonRelease-1> "ob_ERel %W %x $lab"
    bind $name.entry <Control-d> "ob_EDelSel %W $lab"
    bind $name.entry <Control-u> "%W delete 0 end ; set refs($lab) 0"
    
    bind $name.entry <Escape> "
	global config default
	set config($lab) \$default($lab)
	ob_changed
    "
    bind $name.entry <Shift-Escape> "
	global config
	set config($lab) {}
	ob_changed
    "
    pack $name.label -side left -padx 5
    pack $name.entry -side left -fill x -expand 1
}
#
proc ob_RefCount {str} {
    global ob_pat1
    set res 0
    while {[regsub $ob_pat1 $str {} str]} {incr res}
    return $res
}
#
proc ob_Subst {which} {
    global ob_pat2 ob_pat1 config subst label
    if [info exists subst($which)] {
	ob_tellError "Recursive reference in string expansion!!! - $label($which)"
	unset subst
	return {}
    }
    set v $config($which)
    set subst($which) 1
    while {[regexp $ob_pat2 $v m lab]} {
	if [catch {regsub $ob_pat1 $v [ob_Subst [ob_FindLabel $lab]] v}] {
	    return {}
	}
    }
    unset subst($which)
    return $v
}
#
proc ob_ask {title msg} {
    return [tk_dialog .ask $title $msg question 1 No Yes]
}
#
proc ob_changed {} {
    global obeah
    set obeah(altered) 1
    .ctl.save configure -state normal
    if $obeah(cMode) {catch {.create.ctl.save configure -state normal}}
}
#
proc ob_clear {} {
    global config refs
    foreach x [array names config] {
	if ![string match {!*} $x] {
	    set config($x) {}
	    set refs($x) 0
	}
    }
    ob_changed
}
#
proc ob_configWindow {} {
    global config obeah help action window label delete
    wm minsize . 1 1
    wm title . "$obeah(program) configuration"
    frame .conf -relief raised
    set w 0
    foreach x [array names label] {
	if {[string match {[if]*} $x] && [string length $label($x)] > $w} {
	    set w [string length $label($x)]
	}
    }
#
    ob_vWindow f Configurable {f*} obc_field $w
#
    if $obeah(cMode) {ob_vWindow i Invisible {i*} obc_invisible $w}
#
    pack .conf -fill x
    message .msg -relief raised -aspect 800
    pack .msg -fill x
    bind .msg <Any-Enter> {ob_showHelp !.msg}
    set help(!.msg) {Messages are displayed here.}
    text .help -borderwidth 2 -state disabled -height 4 -wrap word -width 50
    bind .help <Any-Enter> {ob_showHelp !.help}
    set help(!.help) {Help information is displayed here.}
    frame .ctl -relief raised
    set help(!quit) {Clicking the Quit button will exit the configuration program.}
    set help(!clear) {Clicking the Clear button will clear all the fields.}
    set help(!reset) {Clicking the Reset button will restore the default configuration values.}
    set help(!verify) {Clicking the Verify button will validate the values you have provided for the fields.}
    set help(!save) {Clicking the Save button will save your configuration info.}
    set help(!install) {Clicking the Install button will attempt to install %p and will save your configuration.}
    foreach x {Quit Reset Clear Verify Save Install} {
	set lx [string tolower $x]
	if [info exists delete(!$lx)] { continue }
	button .ctl.$lx -text $x -command ob_$lx
	bind .ctl.$lx <Any-Enter> "[bind Button <Any-Enter>]
	  ob_showHelp !$lx"
	if $obeah(cMode) { bind .ctl.$lx <Shift-1> "obc_control !$lx" }
	pack .ctl.$lx -side left -fill x -expand 1
    }
    .ctl.save configure -state disabled
    if $obeah(cMode) {.ctl.quit configure -state disabled}
    foreach x [lsort [array names action]] {
	button .ctl.$x -text $label($x) -command $action($x)
	bind .ctl.$x <Any-Enter> "[bind Button <Any-Enter>]
	  ob_showHelp $x"
	if $obeah(cMode) { bind .ctl.$x <Shift-1> "obc_control $x" }
	pack .ctl.$x -side left -fill x -expand 1
	set window($x) .ctl.$x
    }
    checkbutton .ctl.help -text Help -variable obeah(helpOn)  -command ob_setHelp
    set help(!help) {Checking the Help flag will cause help information to be displayed.}
    bind .ctl.help <Any-Enter> "[bind Button <Any-Enter>]
      ob_showHelp !help"
#
    pack .ctl.help -side left
    pack .ctl -fill x -side bottom
}
#
proc ob_dirCheck {dir} {
    set ans 1
    if ![file exists $dir] {
	if ![file writable [file dirname $dir]] {
	    ob_flagError  "$dir does not exist and you have no write permission for its parent."
	    set ans 0
	}
    }  elseif {![file isdirectory $dir]} {
	ob_flagError "$dir is not a directory."
	set ans 0
    }  elseif {![file writable $dir]} {
	ob_flagError "You have no write permission for $dir."
	set ans 0
    }
    return $ans
}
#
proc ob_dirExists {dir} {
    set ans 1
    if ![file exists $dir] {
	ob_flagError "$dir does not exist."
	set ans 0
    }  elseif {![file isdirectory $dir]} {
	ob_flagError "$dir is not a directory."
	set ans 0
    }
    return $ans
}
#
proc ob_fBind {frm} {
    set l [pack slaves $frm]
    set fst [lindex $l 0]
    set prv $fst
    foreach x $l {ob_nextBind $prv $x ; set prv $x}
    if {$fst != {}} { ob_nextBind $x $fst }
}
#
proc ob_flagError {msg} {
    if ![winfo exists .error] {
	toplevel .error -class Obeah
	wm minsize .error 1 1
	wm title .error {Verify Errors}
	frame .error.txt
	scrollbar .error.sb -command {.error.msg yview}
	text .error.msg -relief raised -state disabled -wrap word  -yscrollcommand {.error.sb set} -height 16
	button .error.quit -text Dismiss -command "destroy .error"
	pack .error.msg -fill both -expand 1 -side left -in .error.txt
	pack .error.sb -fill y -side right -in .error.txt
	pack .error.txt -fill both -expand 1
	pack .error.quit -fill x
    }
    .error.msg configure -state normal
    .error.msg insert end "$msg\n"
    .error.msg configure -state disabled
    .error.msg see end
}
#
proc ob_inform {msg} {
    tk_dialog .info Warning $msg warning 0 OK
}
#
proc ob_init {} {
    global config help check makeVar action default label delete obeah  ob_pat1 ob_pat2 ob_pat3 
#
    ob_makeArray window config help check makeVar action default  label delete obeah refs
#
    set obeah(creator) {}
    set obeah(version) 1.4
    set obeah(cMode) 0
    set obeah(findex) 0
    set obeah(iindex) 0
    set obeah(helpOn) 0
    set obeah(cfFile) configure.in
    set obeah(altered) 0
#
#  Bindings to detect need for saves.
#
    foreach x {Delete BackSpace KeyPress Insert Control-d
      Control-h Control-k Control-t Meta-d Meta-BackSpace
      ButtonRelease-2} {
	  bind Entry <$x>	{+ ob_changed}
    }
#
# Reference patterns
#
    set ob_pat1 "\\\${\[^\}\]+}"
    set ob_pat2 "\\\${(\[^\}\]+)}"
    set ob_pat3 "(\\\${\[^\}\]+})"
}
#
proc ob_install {} { ob_runMake install }
#
proc ob_interpCheck {actions wish} {
    set ans 1
    if ![file exists $wish] {
	ob_flagError "$wish does not exist."
	set ans 0
    }  elseif {![file executable $wish]} {
	ob_flagError "You cannot execute $wish."
	set ans 0
    }  elseif {![ob_wishCheck $wish {puts stdout check} {^check.*}  {is not a tcl interpreter}]} {
	set ans 0
    } {
	foreach x $actions {
	    set xa [ob_wishCheck $wish [lindex $x 0] [lindex $x 1] [lindex $x 2]]
	    set ans [expr $ans & $xa]
	}
    }
    return $ans
}
#
proc ob_label {w x width prc} {
    global obeah
    ob_LabelEntry $x $w.$x -width $width
    pack $w.$x -fill x
    bind $w.$x.label <1> "obc_fRef \$wlab($w.$x.entry)"
    if $obeah(cMode) {
	bind $w.$x.label <Shift-1> "$prc \$wlab($w.$x.entry)"
	bind $w.$x.label <2> "obc_pick %W %y"
	bind $w.$x.label <B2-Motion> "obc_move %W %y"
	bind $w.$x.label <ButtonRelease-2> "obc_place %W %x %y"
    }
}
#
proc ob_load {} {
    global config help check action makeVar default label window obeah  delete
    wm withdraw .
    catch {destroy .conf .msg .help .ctl}
    foreach x {config help check action makeVar default label window} {
	foreach v [array names $x] {
	    if ![string match {!*} $v] { unset ${x}($v) }
	}
    }
    unset delete
    ob_makeArray delete
    set obeah(program) {}
    set obeah(makefile) {}
    set obeah(cfProg) configure
    set prcs [info procs]
    if ![file exist $obeah(cfFile)] {
	ob_tellError "File \"$obeah(cfFile)\" does not exist."
	if !$obeah(cMode) { exit 1 }
    } {
	if ![string match {*.in} $obeah(cfFile)] {
	    if [file exists $obeah(cfFile).in] {
		if [ob_ask File? "Do you mean file \"$obeah(cfFile).in\"?"] {
		    set obeah(cfFile) $obeah(cfFile).in
		}
#
#  The next command avoids a problem where you end up saving tk code....
#
		set prcs [info procs]
	    }
	}
	if [catch {source $obeah(cfFile)} msg] {
	    ob_tellError "Error in config file \"$obeah(cfFile)\" : $msg"
	    if !$obeah(cMode) { exit 1 }
	}
    }
    set obeah(procs) {}
    foreach x [info procs] { 
	if [string match tk_* $x] continue
	if [string match auto_* $x] continue
	if {[lsearch $prcs $x] < 0} { lappend obeah(procs) $x }
    }
    if [string match {} $obeah(program)] {
	if !$obeah(cMode) {
	    ob_tellError {No program name specified in configuration file.}
	    exit 1
	}
	set obeah(program) [string tolower [file tail [pwd]]]
    }
    if [string match {} $obeah(makefile)] {
	set obeah(makefile) $obeah(program).mk
    }
    set obeah(altered) 0
    ob_configWindow
    wm deiconify .
    if {$obeah(creator) != $obeah(version)} {
	ob_changed
	ob_inform "\"$obeah(cfFile)\" was created by a different version of obeah. Please resave it."
    }
}
#
proc ob_log {cmd} {
    global obeah
    ob_tellUser "Executing \"$cmd\""
    if [catch {set desc [open "|$cmd" r]} msg] {
	ob_tellUser "Cannot execute \"$cmd\" : $msg"
	return
    }
    ob_putLog "% $cmd"
    while {![eof $desc]} { ob_putLog "[gets $desc]" }
    if {[catch {close $desc} msg] && $msg != {}} {
	ob_tellUser "\"[lindex $cmd 0]\" terminated : $msg"
	ob_putLog $msg
    } {
	ob_tellUser "\"[lindex $cmd 0]\" terminated"
    }
    update
}
#
proc ob_main {argv} {
    global obeah

    ob_init
#
    if ![string compare [lindex $argv 0] -C] {
	set obeah(cMode) 1
	set argv [lrange $argv 1 end]
    }
    if ![string match {} $argv] {
	set obeah(cfFile) [lindex $argv 0]
	set argv [lrange $argv 1 end]
    }
    ob_load
    if $obeah(cMode) { obc_makeCreate }
}
#
proc ob_makeArray {args} {
    foreach x $args {
	global $x
	set ${x}(1) 1
	unset ${x}(1)
    }
}
#
proc ob_makeMF {} {
    global config makeVar obeah
    set desc [open Makefile w]
    foreach x [lsort [array names config]] {
	if {![string match {!*} $x] && [info exists makeVar($x)] && 
	  $makeVar($x) != {}} {
	    puts $desc "$makeVar($x)	= $config($x)"
	}
    }
    close $desc
    exec cat $obeah(makefile) >>Makefile
}
#
proc ob_makeVars {} {
    global makeVar config
    set res {}
    foreach x [array names makeVar] {
	regsub -all {([ 	])} [ob_Subst $x] {\\&} v
	if {$v != {}} { append res " $makeVar($x)=$v" }
    }
    return $res
}
#
proc ob_mkDirs {args} {
    foreach x $args {
	ob_tellUser "Making directory \"$x\""
	if [string match {/*} $x] {
	    set x [string range $x 1 end]
	    set dir {/}
	} {
	    set dir [pwd]/
	}
	foreach d [split $x /] {
	    append dir $d
	    if [file exists $d] {
		if ![file isdirectory $d] {
		    ob_putLog "Error making \"$dir\" : $d is not a directory"
		    break
		}
	    } {
		ob_log "mkdir $dir"
	    }
	    append dir {/}
	}
    }
}
#
proc ob_nextBind {from to} {
    bind $from.entry <Return> "focus $to.entry"
    catch {bind $from.entry <Key-KP_Enter> "focus $to.entry"}
    bind $from.entry <Key-Tab> "focus $to.entry"
    bind $from.entry <Control-i> "focus $to.entry"
    bind $from.entry <Down> "focus $to.entry"
    bind $to.entry <Up> "focus $from.entry"
}
#
proc ob_order {frm} {
    if [winfo exists .conf.$frm] {
	set v {}
	foreach x [pack slaves .conf.$frm] {
	    lappend v [string range [file extension $x] 1 end]
	}
	if {$v != {none}} { return $v }
    }
    return {}
}
#
proc ob_putLog {msg} {
    global obeah
    if ![winfo exists .log] {
	toplevel .log -class Obeah
	wm minsize .log 1 1
	wm title .log "$obeah(program) execution log"
	frame .log.txt
	scrollbar .log.sb -command {.log.msg yview}
	text .log.msg -relief raised -state disabled -wrap word  -yscrollcommand {.log.sb set} -height 16
	button .log.quit -text Dismiss -command {destroy .log}
	pack .log.msg -fill both -expand 1 -side left -in .log.txt
	pack .log.sb -fill y -side right -in .log.txt
	pack .log.txt -fill both -expand 1
	pack .log.quit -fill x -side bottom
    }
    .log.msg configure -state normal
    .log.msg insert end "$msg\n"
    .log.msg configure -state disabled
}
#
proc ob_quit {} {
    global obeah
    if {$obeah(altered) &&
      [ob_ask Save? "You have made changes. Do you wish to save them?"]} {
	ob_save
    }
    exit
}
#
proc ob_reset {} {
    global config default refs
    foreach x [array names config] {
	if ![string match {!*} $x] {
	    if [info exists default($x)] {
		set config($x) $default($x)
		set refs($x) [ob_RefCount $config($x)]
	    } {
		set config($x) {}
		set refs($x) 0
	    }
	}
    }
    ob_changed
}
#
proc ob_run {args} {
    global env makeVar config
    foreach x [array names makeVar] {
	regsub -all {([ 	])} [ob_Subst $x] {\\&} v
	if {$v != {}} {
	    set nm $makeVar($x)
	    if [info exists env($nm)] { set save($nm) $env($nm) }
	    set env($nm) $v
	}
    }
    ob_log $args
    foreach x [array names makeVar] {
	if {[set nm $makeVar($x)] != {}} {
	    if [info exists save($nm)] {set env($nm) $save($nm)} {unset env($nm)}
	}
    }
    catch {unset save}
}
#
proc ob_runMake {args} {
    global obeah
    ob_log "make -f $obeah(makefile) [ob_makeVars] $args"
}
#
proc ob_save {} {
    global config help check action makeVar default cMode label delete obeah

    if [string match {} $obeah(cfFile)] {
	ob_tellError "No config file specified."
	return 0
    }
    ob_tellUser "Saving configuration in \"$obeah(cfFile)\""
    if [file exists $obeah(cfFile)] {
	if [catch {exec mv -f $obeah(cfFile) $obeah(cfFile).bak} msg] {
	    ob_tellError "Cannot create backup : $msg"
	}
    }
    if [catch {set desc [open $obeah(cfFile) w]} msg] {
	ob_tellError "Error opening $obeah(cfFile) for write : $msg"
	return 0
    }
    puts $desc {# ************ Use the configuration system to change this file ****}
    foreach x [lsort $obeah(procs)] {
	puts $desc "#\nproc $x {[info args $x]} {[info body $x]}"
    }
    puts $desc "#\nset obeah(program) {$obeah(program)}"
    puts $desc "set obeah(cfFile) {$obeah(cfFile)}"
    puts $desc "set obeah(cfProg) {$obeah(cfProg)}"
    puts $desc "set obeah(makefile) {$obeah(makefile)}"
    puts $desc "set obeah(creator) {$obeah(version)}"
    puts $desc "set obeah(forder) {[ob_order f]}"
    puts $desc "set obeah(findex) $obeah(findex)"
    puts $desc "set obeah(iorder) {[ob_order i]}"
    puts $desc "set obeah(iindex) $obeah(iindex)"
    foreach v [expr {$obeah(cMode) ? {config} : {config default}}] {
	puts $desc {#}
	foreach x [lsort [array names $v]] {
	    if ![string match {!*} $x] {
		if [regexp {^=(.*)} [set ${v}($x)] m xpr] {
		    puts $desc "set ${v}($x) $xpr"
		} {
		    puts $desc "set ${v}($x) {[set ${v}($x)]}"
		}
	    }
	}
    }
    foreach v {label help check makeVar action} {
	puts $desc {#}
	foreach x [lsort [array names $v]] {
	    if ![string match {!*} $x] {
		puts $desc "set ${v}($x) {[set ${v}($x)]}"
	    }
	}
    }
    puts $desc {#}
    foreach x [lsort [array names delete]] { puts $desc "set delete($x) 1" }
    close $desc
    set obeah(altered) 0
    .ctl.save configure -state disabled
    if $obeah(cMode) { .create.ctl.save configure -state disabled }
    return 1
}
#
proc ob_setHelp {} {
    global obeah
    if $obeah(helpOn) {pack .help -after .msg -fill x -pady 5} {pack forget .help}
}
#
proc ob_showHelp {what} {
    global obeah help
    if $obeah(helpOn) {
	.help configure -state normal
	.help delete 1.0 end
	if [info exists help($what)] {
	    regsub -all {%n} $help($what) $what val
	    regsub -all {%p} $help($what) $obeah(program) val
	    .help insert end $val
	}
	.help configure -state disabled
    }
}
#
proc ob_tellError {msg} {
    tk_dialog .derr Error $msg error 0 OK
}
#
proc ob_tellUser {what} {
    .msg configure -text $what
    update
}
#
proc ob_vWindow {frm txt ptn prc width} {
    global window label obeah
    set w [frame .conf.$frm]
    if [info exists obeah(${frm}order)] {
	foreach x $obeah(${frm}order) { ob_label $w $x $width $prc }
    } {
	set obeah(${frm}index) 0
	foreach x [lsort [array names label]] {
	    if [string match $ptn $x] {
		ob_label $w $x $width $prc
		incr obeah(${frm}index)
	    }
	}
    }
    label $w.none -text "No $txt Values"
    if {$obeah(${frm}index) == 0} {pack $w.none -fill x -padx 10} {ob_fBind $w}
    pack $w -fill x
}
#
proc ob_valCheck {kind possible val} {
    if {[lsearch $possible $val] < 0} {
	ob_flagError "\"$val\" is not a valid $kind."
	return 0
    }
    return 1
}
#
proc ob_verify {} {
    global check obeah config label
    if [winfo exists .error] {
	.error.msg configure -state normal
	.error.msg delete 1.0 end
	.error.msg configure -state disabled
    }
    set ans 1
    foreach x [lsort [array names check]] {
	if ![string match {} $check($x)] {
	    ob_tellUser "Checking $label($x)"
	    if ![eval $check($x) [ob_Subst $x]] { set ans 0 }
	}
    }
    if $ans {ob_tellUser {Verify succeeded}} {ob_tellUser {Verify failed}}
}
#
proc ob_wishCheck {wish cmd patrn msg} {
    set ans 1
    set desc [open "|$wish" r+]
    puts $desc "puts stdout \[$cmd\] ; exit"
    flush $desc
    set res [read $desc]
    if ![regexp -- $patrn $res] {ob_flagError "$wish $msg" ; set ans 0}
    catch {close $desc}
    return $ans
}
#
ob_main {zconf.in}
