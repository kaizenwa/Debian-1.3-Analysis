#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/confInfo.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
set zircon(idata1) {
    { {Verbose CTCP} verboseCTCP }
    { {Pop Up Info} popInfo }
    { {Flag Pop Up} noPopup }
    { {No Channel List} noRefresh }
    { {Kill Path} killPath }
}
#
proc confInfo {net} {
    global confChange
    set win .@cfInfo$net
    if [winfo exists $win] { popup $win ; return }
    global zircon confData
    upvar #0 new$net newd
    toplevel $win -class Zircon
    wm title $win {General Configuration}
    wm protocol $win WM_DELETE_WINDOW "confDismiss $net Info"
    confInit $net Info
    frame $win.misc0 -relief raised
    set i 0
    foreach d $zircon(idata1) {
	checkbutton $win.misc0.$i -text [lindex $d 0] \
	  -variable new${net}([lindex $d 1]) -command "confDirty $win"
	pack $win.misc0.$i -side left
	incr i
    }
    frame $win.misc1
    labelEntry 0 $win.misc1.help {-text {Help Service}} $newd(helpService) \
      " set new${net}(helpService) \[%W get\] ; confDirty $win"
    pack $win.misc1.help -side left -fill x
    labelEntry 0 $win.misc1.ison {-text {ISON interval}} \
      [expr $newd(notifyInterval) / 1000] \
      " set new${net}(notifyInterval) \[expr \[%W get\]*1000\] ; confDirty $win"
    pack $win.misc1.ison -side left -fill x
    frame $win.misc2 -relief raised
    global confData toInfo
    label $win.misc2.label -text "Send to Info :"
    pack $win.misc2.label -side left
    set i 0
    foreach ci $confData(info) {
	set uci [string toupper $ci]
	checkbutton $win.misc2.inf${i} -text $ci \
	  -variable confI($net,$i) -command "doCInfo $net $i $uci"
	global confI
	set confI($net,$i) [expr {[lsearch $toInfo $uci] >= 0}]
	pack $win.misc2.inf${i} -side left
	incr i
    }
    frame $win.misc3 -relief raised
    label $win.misc3.label -text "No Confirm :"
    pack $win.misc3.label -side left
    global confData
    global noConfirm
    set i 0
    foreach ci $confData(nconf) {
	set uci [string toupper $ci]
	checkbutton $win.misc3.inf${i} -text $ci \
	  -variable confNC${i} -command "doCNConf $net $i $uci"
	global confNC${i}
	set confNC${i} [expr {[lsearch $noConfirm $uci] >= 0}]
	pack $win.misc3.inf${i} -side left
	incr i
    }
    frame $win.filter -relief raised

    checkbutton $win.filter.public -variable new${net}(showPublic) \
      -text "Public" -command "confDirty $win"

    checkbutton $win.filter.local -variable new${net}(showLocal) \
      -text "Local" -command "confDirty $win"
    checkbutton $win.filter.private -variable new${net}(showPrivate) \
      -text "Private" -command "confDirty $win"
    checkbutton $win.filter.topic -variable new${net}(topicOnly) \
      -text "With Topic" -command "confDirty $win"

    scale $win.filter.members \
      -from 1 -to 25 -label {Minimum Number of Members} \
      -showvalue 1 -orient horizontal
    $win.filter.members set [$net minMembers]
    $win.filter.members configure \
      -command "$win.filter.members configure -command {confDirty $win ; set new${net}(minMembers)} ; set x "
    pack $win.filter.members -side left -expand 1 -fill x
    pack $win.filter.public $win.filter.local $win.filter.private \
      $win.filter.topic -side left

    labelEntry 0 $win.filter2 {-text {Channel Pattern} -width 16} \
      $newd(listPattern) "set new${net}(listPattern) \[%W get\] ; confDirty $win"
    labelEntry 0 $win.filter3 {-text {Topic Pattern} -width 16} \
      $newd(topicPattern) "set new${net}(topicPattern) \[%W get\] ; confDirty $win"
    pack $win.misc0  $win.misc1  $win.misc2 $win.misc3 $win.filter \
      $win.filter $win.filter2 $win.filter3 -fill x
    confMkBtn $net $win Info
    pack $win.btn -fill x
}
#
proc doCNConf {net indx val} {
    global confNC$indx
    upvar #0 new$net newd
    if [set confNC$indx] {
	lappend newd(noConfirm) $val
    } {
	listdel newd(noConfirm) [lsearch $newd(noConfirm) $val]
    }
    confDirty .@cfInfo$net
}
#
proc doCInfo {net indx val} {
    global confI
    upvar #0 new$net newd
    if $confI($net,$indx) {
	lappend newd(toInfo) $val
    } {
	listdel newd(toInfo) [lsearch $newd(toInfo) $val]
    }
    confDirty .@cfInfo$net
}













