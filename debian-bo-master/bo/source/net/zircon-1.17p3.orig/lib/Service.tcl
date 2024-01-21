#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Service.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
class Service {
    name	{}
    host	{}
    nick	{}
    ops		{}
    addr	{}
    net		{}
    sys		0
}
#
proc svid {name} {
    global VTO
    set name [string tolower $name]
    if [info exists VTO($name)] { return $VTO($name) } { return nil }
}
#
proc Service {name args} {
    if ![string compare :: $name] {
	return [eval Service_[lindex $args 0] [lrange $args 1 end]]
    }
    set this [objName Service]
    initObj $this Service
    global VTO current
    set VTO([string tolower $name]) $this
    upvar #0 $this sdata
    set sdata(name) $name
    set sdata(nick) $name
    set sdata(addr) $name
    set sdata(net) $current(net)
    $current(net) register services $this
    proc $this {args} "eval service_call $this \$args"
    if ![string match {} $args] { eval $this configure $args }
    if ![string match {} $sdata(host)] {
	global Secure
	set sdata(addr) $sdata(nick)@$sdata(host)
	set Secure([string tolower $sdata(nick)]) $sdata(addr)
    }
    return $this
}
#
proc service_configure {this args} {
    upvar #0 $this sdata
    while {![string match {} $args]} {
	set sdata([string range [lindex $args 0] 1 end]) [lindex $args 1]
	set args [lrange $args 2 end]
    }
}
#
proc service_call {this op args} {
    upvar #0 $this sdata
    if [info exists sdata($op)] { return $sdata($op) }
    return [eval service_$op $this $args]
}
#
proc service_delete {this} {
    [$this net] deregister services $this
    global VTO $this
    unset VTO([string tolower [$this name]]) $this
    rename $this {}
}
#
proc service_send {this op par} { [$this net] PRIVMSG [$this addr] "$op $par" }
#
proc service_do {this op} {
    global ztrans
    set sv [$this name]
    mkEntryBox .@sv$this $sv "Enter any parameters needed for $sv:" \
      [list [list $op {}]] "$ztrans(ok) {$this send $op }" "$ztrans(cancel) {}"
}
#
proc Service_save {desc net} {
    foreach sv [$net services] {
	if [$sv sys] continue
	regsub -all {[][\\{\"}]} [$sv name] {\\&} nm
	set ln "Service $nm"
	foreach p {nick host ops} {
	    if ![string match {} [set n [$sv $p]]] {append ln " -$p {$n}"}
	}
	puts $desc $ln
    }
}
