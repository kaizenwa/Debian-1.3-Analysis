##
## init.tcl
##
## Scotty initialization file. At the end of this file, we source
## the file init.tcl in the site subdirectory of the scotty library 
## directory. This is the preferred way to do site specific 
## initializations because this will work even after updating scotty 
## sources.
##
## Copyright (c) 1994, 1995
##
## J. Schoenwaelder
## TU Braunschweig, Germany
## Institute for Operating Systems and Computer Networks
##
## Permission to use, copy, modify, and distribute this
## software and its documentation for any purpose and without
## fee is hereby granted, provided that this copyright
## notice appears in all copies.  The University of Braunschweig
## makes no representations about the suitability of this
## software for any purpose.  It is provided "as is" without
## express or implied warranty.

if {![info exists scotty_version]} {
    error "scotty init.tcl file loaded without a scotty extension"
}
scan $scotty_version "%d.%d.%d" major minor patchlevel
if {$major != 2 || $minor < 0} {
    error "wrong version of init.tcl file loaded for scotty ($scotty_version)"
}

##
## Alias definitions for SNMP sessions. You can add more local 
## definitions in the site/init.tcl file.
##

snmp alias localhost "-address 127.0.0.1"
snmp alias mcasttrap "-address 234.0.0.1 -port 162"

##
## The global variable scotty_mibs is used to hold a list of mibs that
## are auto-loaded by a scotty interpreter. You can extend or
## redefine this list in your site/init.tcl file, which gets 
## sourced at the end of this script.
##

lappend scotty_mibs rfc1315.mib
lappend scotty_mibs rfc1406.mib
lappend scotty_mibs rfc1407.mib
lappend scotty_mibs rfc1414.mib
lappend scotty_mibs rfc1451.mib
lappend scotty_mibs rfc1471.mib
lappend scotty_mibs rfc1493.mib

lappend scotty_mibs rfc1757.mib

lappend scotty_mibs rfc1512.mib
lappend scotty_mibs rfc1513.mib 
lappend scotty_mibs rfc1514.mib
lappend scotty_mibs rfc1515.mib
lappend scotty_mibs rfc1516.mib
lappend scotty_mibs rfc1525.mib
lappend scotty_mibs rfc1565.mib
lappend scotty_mibs rfc1566.mib
lappend scotty_mibs rfc1567.mib
lappend scotty_mibs rfc1573.smi
lappend scotty_mibs rfc1573.mib
lappend scotty_mibs rfc1595.mib
lappend scotty_mibs rfc1596.mib

lappend scotty_mibs rfc1611.mib
lappend scotty_mibs rfc1612.mib
lappend scotty_mibs rfc1650.mib
lappend scotty_mibs rfc1658.mib
lappend scotty_mibs rfc1659.mib
lappend scotty_mibs rfc1660.mib
lappend scotty_mibs rfc1694.mib
lappend scotty_mibs rfc1695.mib
lappend scotty_mibs rfc1696.mib
lappend scotty_mibs rfc1697.mib

lappend scotty_mibs rfc1724.mib
lappend scotty_mibs rfc1747.mib
lappend scotty_mibs rfc1748.mib
lappend scotty_mibs rfc1749.mib
lappend scotty_mibs rfc1759.mib
lappend scotty_mibs rfc1792.mib

# Vendor MIB definitions - you might want to copy these lines to
# your $scotty_lib/site/init.tcl file.

# lappend scotty_mibs cisco.mib
# lappend scotty_mibs hp-unix.mib
# lappend scotty_mibs ncd.mib
# lappend scotty_mibs synoptics.mib
# lappend scotty_mibs wellfleet.mib
# lappend scotty_mibs unix.mib

# Some local MIB fun for experimentation

lappend scotty_mibs tubs.mib
lappend scotty_mibs mlm.mib

##
## Redefine the mib command. This is a little bit of magic, but it
## makes sure that the mib definitions above get loaded by the first 
## mib command. However, we do not autoload all the mibs above if the
## first mib command is a load command. Instead, we expect that the
## application knows what it needs.
##

if {[info commands mib] == "mib"} {

    rename mib _mib

    proc mib {args} {

	global scotty_lib scotty_mibs

	# always load the .smi and .tc and MIB-II definitions

	_mib load rfc1155.smi
	_mib load rfc1442.smi
	_mib load rfc1443.tc
	_mib load rfc1447.mib
	_mib load rfc1450.mib
	_mib load rfc1213.mib
	_mib load usec.mib

	rename mib ""
	rename _mib mib

	# don't autoload if the first command is mib load

	if {[lindex $args 0] == "load"} {
	    return [eval mib $args]
	}

	foreach mib $scotty_mibs {
	    mib load $mib
	}

	return [eval mib $args]
    }

}

##
## Define a proc to handle background errors.
##

if {[info commands tkerror] == ""} {
    proc tkerror {msg} {
	global errorInfo
	puts stderr $errorInfo
    }
}

##
## This nice procedure allows us to use static variables. It was
## posted on the net by Karl Lehenbauer. There was another one
## which does not pollute the name space, but it fails on proc
## names or variable names with spaces in it...
##

proc static {args} {
    set procName [lindex [info level [expr [info level]-1]] 0]
    foreach varName $args {
        uplevel 1 "upvar #0 {$procName:$varName} $varName"
    }
}

##
## Emulate the sleep command using the after command.
##

proc sleep secs {
    after [expr $secs * 1000]
}

##
## Emulate the nslook command using the netdb command.
##

proc nslook {arg} {
    if {[catch {netdb hosts name $arg} result]} {
	set result [netdb hosts address $arg]
    }
    return $result
}

##
## Extend the auto_path to include $scotty_lib/library.
##

lappend auto_path $scotty_lib/library

##
## Allow for site specific initializations.
##

if [file exists $scotty_lib/site/init.tcl] {
    source $scotty_lib/site/init.tcl
}

