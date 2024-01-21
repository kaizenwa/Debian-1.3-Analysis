#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Object.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
# ----------------------------------------------------------------------
#   AUTHOR:  Lindsay Marshall <lindsay.marshall@newcastle.ac.uk>
# ----------------------------------------------------------------------
# Copyright 1995 The University of Newcastle upon Tyne (see COPYRIGHT)
# ======================================================================
#
proc objName {class} {
    global OType
    set n [newName $class]
    set OType($n) $class
    return $n
}
#
proc newName {thing} {
    global zircon
    return [string tolower $thing][incr zircon(nameCount)]
}
#
proc class {name vars} {
    global $name
    set $name $vars
}
#
proc initObj {name args} {
    foreach x $args { uplevel #0 "array set $name \[set $x\]" }
}
