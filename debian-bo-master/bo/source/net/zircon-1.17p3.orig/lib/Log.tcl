#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Log.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
proc logOpen {this mode save file} {
    if [string match {} $file] {return 0}
    if ![string match {/*} $file] { set file "[pwd]/$file" }
    upvar #0 $this cdata
    if [catch {open $file $mode} cdata(logfd)] {
	[$this net] errmsg \
	  "Cannot open log file for channel [$this name] : $cdata(logfd)"
	set cdata(logfd) {}
	return 0
    }
    set cdata(logfile) $file
    set w [$this window].channel.menu.log
    $w entryconfigure 0 -state normal
    $w entryconfigure 1 -state disabled
    $w entryconfigure 2 -state normal
    $w entryconfigure 3 -state normal
    puts $cdata(logfd) "**** Logging Started : [getDate]"
    if $save {
	puts $cdata(logfd) "**** Saving Window"
	puts $cdata(logfd) "[[$this text] get 1.0 end]"
	puts $cdata(logfd) "**** End of Window Text"
    }
    return 1
}

proc channel_doLog {this op} {
    global ztrans
    upvar #0 $this cdata
    set w [$this window].channel.menu.log
    switch $op {
    Close {
	    if ![string match {} $cdata(logfd)] {
		close $cdata(logfd)
		set cdata(logfd) {}
	    }
	    $w entryconfigure 0 -state disabled
	    $w entryconfigure 1 -state normal
	    $w entryconfigure 2 -state disabled
	    $w entryconfigure 3 -state disabled
	}
    Empty {
	    if ![string match {} $cdata(logfd)] {
		close $cdata(logfd)
		set cdata(logfd) [open $cdata(logfile) w]
	    }
	}
    Flush {
	    if ![string match {} cdata(logfd)] {
		if [catch {flush $cdata(logfd)} msg] {
		   [$this net] errmsg \
		     "Error flushing log file for channel [$this name] : $msg"
		   catch {close $cdata(logfd)}
		   set cdata(logfd) [open $cdata(logfile) a]
		}
	    }
	}
    Open {
	    set chan [$this name]
	    set fl [expr {[string match {} $cdata(logfile)] ? \
	      $cdata(logfile) : "$chan.log"}]
	    mkFileBox .@log$this .* "Log $chan" \
	      "Log file for channel $chan:" {} \
	      "$ztrans(append) {logOpen $this a 0 }"\
	      "{Append All} {logOpen $this a 1 }"\
	      "$ztrans(open) {logOpen $this w 0 }" \
	      "{Open from Start} {logOpen $this w 1 }" "$ztrans(cancel) {}"
	}
    }
}
