#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Topic.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
# Topic handling procs
#
proc keepTopic {this value} {
    if {$value != {}} {
	set chan [$this name]
	$this configure -topic $value
	set t [$this topics]
	if {[lsearch $t $value] < 0} {
	    lappend t $value
	    $this configure -topics [lsort $t]
	    [$this window].topic.label.menu add command \
	      -label "[prune $value 15]" \
	      -command "$this configure -topic {$value}"
	    if [$this keep] { global confChange ; set confChange 1 }	
	}
    }
}
#
proc getTopic {this} {
    global ztrans
    set chan [$this name]
    mkEntryBox .@tp$this "$chan Topic" "Enter your new topic for ${chan}:" \
      "{$ztrans(topic) {}}" "$ztrans(ok) {$this configure -topic}" \
      "$ztrans(keep) {keepTopic $this}" "$ztrans(cancel) {}"
}
#
proc sendTopic {win} {
    if [normal $win] {[channel $win] configure -topic [$win get 1.0 end]}
}
#
proc irc331 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    if {[set chn [Channel :: find [lindex $pargs 1]]] != {nil}} {
	$chn setTopic {}
    }
}
#
proc irc332 {net prefix param pargs} {
    regsub -all {\\} $pargs {\\\\} pargs
    if {[set chn [Channel :: find [lindex $pargs 1]]] != {nil}} {
	 $chn setTopic $param
    }
}
