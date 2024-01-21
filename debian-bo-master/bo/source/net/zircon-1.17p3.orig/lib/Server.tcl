#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Server.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
class Server {
    name	{}
    host	{}
    port	6667
    oper	{}
    operpw	{}
    script	{}
    sys		0
    passwd	{}
    net		{}
    sys		0
}
#
proc Server {name args} {
    if ![string compare :: $name] {
	return [eval Server_[lindex $args 0] [lrange $args 1 end]]
    }
    set this [objName Server]
    initObj $this Server
    global current
    upvar #0 STO$current(net) STO $this sdata
    set sdata(name) $name
    set sdata(host) $name
    set sdata(net) $current(net)
    $current(net) register servers $this
    proc $this {args} "eval server_call $this \$args"
    if ![string match {} $args] { eval $this configure $args }
    if ![string compare $sdata(name) $sdata(host)] {
	set sdata(name) [newName _srv]
    }
    set STO([string tolower $sdata(name)]) $this
    return $this
}
#
proc server_configure {this args} {
    upvar #0 $this sdata
    while {![string match {} $args]} {
	set sdata([string range [lindex $args 0] 1 end]) [lindex $args 1]
	set args [lrange $args 2 end]
    }
}
#
proc server_call {this op args} {
   upvar #0 $this sdata
   if [info exists sdata($op)] { return $sdata($op) }
   return [eval server_$op $this $args ]
}
#
proc server_delete {this} {
    uplevel #0 unset STO\$current(net)([string tolower [$this name]]) $this
    uplevel #0 \$current(net) deregister servers $this
    rename $this {}
}
#
proc Server_select {host} {
    global zircon current
    if ![string compare nil [set hst [Server :: find $host]]] {
	mkInfoBox ERROR .@host {Server Host Error} \
	  "Cannot find host - $host"
    } {
	$current(net) configure -hostid $hst
    }
}
#
proc Server_save {desc net} {
    set sid 1
    foreach id [$net servers] {
	if [$id sys] continue
	set n [$id name]
	set nmap($n) $sid
	if ![string compare default $n] { continue }
	set ln "Server srv$sid -host [$id host]"
	if [string compare 6667 [$id port]] {append ln " -port [$id port]"}
	foreach x {oper operpw script} {
	    if ![string match {} [set y [$id $x]]] { append ln " -$x $y" }
	}
	puts $desc $ln
	incr sid
    }
    if [string compare [set hst [$net hostid]] nil] {
	if {![$hst sys] && [string compare default [$hst name]]} {
	    puts $desc "Server :: select srv$nmap([$hst name])"
	}
    } {
	mkDialog WARNING .@nodef {Warning} \
	  {You have no default server selected!} {}
    }
}
#
proc Server_make {host} {
    if ![string compare nil [set s [Server :: find $host]]] {
	set s [Server $host]
    }
    return $s
}
#
proc server_pack {this net} {
    upvar #0 new$this new
    foreach v {host port oper operpw script name} {
	set new($v) [$this $v]
    }
    global ${net}STO
    set ${net}STO([string tolower [$this name]]) $this
}
#
proc server_unpack {this net} {
    upvar #0 new$this new ${net}STO STO
    foreach v {host port oper operpw script} {
	$this configure -$v $new($v)
    }
    unset STO([string tolower $new(name)]) new
}
#
proc Server_pack {net} {
    foreach s [$net servers] { if ![$s sys] { $s pack $net} }
}
#
proc Server_unpack {net} {
    upvar #0 ${net}STO newst
    foreach s [array names newst] { $newst($s) unpack $net}
    Server :: cleanup new
}
#
proc Server_cleanup {where} { }
#
proc Server_find {name} {
    global current
    upvar #0 STO$current(net) STO
    set name [string tolower $name]
    if [info exists STO($name)] { return $STO($name) }
    foreach x [array names STO] {
	if ![string compare [$STO($x) host] $name] { return $STO($x) }
    }
    return nil
}
