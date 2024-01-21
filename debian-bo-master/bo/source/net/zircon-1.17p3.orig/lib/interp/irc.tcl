proc dp_address {args} { }
proc dp_connect {args} {
    global myid
    set srv [lindex $args 0]
    if {[set port [lindex $args 1]] == {}} { set port 6667 }
    if {[catch {set desc [open "|irc -d [$myid name] $srv:$port" r+]} msg]} {
	error $msg
    }
    return [list $desc 1234]
}
proc dp_filehandler {sock what prc} {
    fileevent $sock readable "$prc r $sock"
}
proc dp_shutdown {args} { }
proc dp_atclose {args} { }
proc dp_socketOption {args} { }
set dp_version "No tcl-dp"
proc dp_receive {sock} { return [gets $sock] }
proc dp_send {sock what} {
    puts $sock $what
    flush $sock
}
set USINGIRC 1
