#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/On.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
# ON condition support code
#
proc on {action pattern code} {
    global OnCode
    set pat {}
    foreach p $pattern { lappend pat [string tolower $p] }
    lappend OnCode([string toupper $action]) [list $pat $code]
}
#
proc handleOn {action pattern} {
    global OnCode zircon
    if {!$zircon(o) && [info exists OnCode($action)]} {
	foreach act $OnCode($action) {
	    set re [lindex $act 0]
	    set i 0
	    set match 1
	    foreach pat $pattern {
		set up [lindex $re $i]
		if {![string match $up {}] && ![regexp -nocase -- $up $pat]} {
		    set match 0
		    break
		}
		incr i
	    }
	    if $match {
		set i 0
		foreach pat $pattern {
		    global onPar${i}
		    set onPar${i} $pat
		    incr i
		}
		if [catch {uplevel #0 [lindex $act 1]} msg] {
		    mkDialog ON .@on "On Command Error" \
		  "Error when executing on command \"[lindex $act 1]\" : $msg" \
		  {}
		}
		catch {eval unset [info globals onPar*]}
		if !$zircon(multion) return
	    }
	}
    }
}
#
proc operator {chan} { return [[Channel :: find $chan] operator] }
