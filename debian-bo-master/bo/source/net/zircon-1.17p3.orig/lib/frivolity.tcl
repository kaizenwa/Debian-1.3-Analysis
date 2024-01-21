#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/frivolity.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
proc strrev {t} {
    set r {}
    set l [string length $t]
    for {set i 0} {$i < $l} {incr i} {
	set r "[string index $t $i]$r"
    }
    return $r
}
#

proc rot13 {t} {
    scan A %c Aval
    scan M %c Mval
    scan Z %c Zval
    scan a %c aVal
    scan m %c mVal
    scan z %c zVal
    set r {}
    set l [string length $t]
    for {set i 0} {$i < $l} {incr i} {
	set c [string index $t $i]
	scan $c %c v
	if {($v >= $Aval && $v <= $Mval ) \
	  || ($v >= $aVal && $v <= $mVal)} {
	    incr v 13
	    set c [format %c $v]
	} elseif {($v > $Mval && $v <= $Zval ) \
	  || ($v > $mVal && $v <= $zVal)} {
	    incr v -13
	    set c [format %c $v]
	}
	append r $c
    }
    return $r
}
#
set zircon(morseIndex) \
   {ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789?.,}
#
set zircon(morse) {
    { A .- }
    { B -... }
    { C -.-. }
    { D -.. }
    { E . }
    { F ..-. }
    { G --. }
    { H .... }
    { I .. }
    { J .--- }
    { K -.- }
    { L .-.. }
    { M -- }
    { N -. }
    { O --- }
    { P .--. }
    { Q --.- }
    { R .-. }
    { S ... }
    { T - }
    { U ..- }
    { V ...- }
    { W .-- }
    { X -..- }
    { Y -.-- }
    { Z --.. }
    { a .- }
    { b -... }
    { c -.-. }
    { d -.. }
    { e . }
    { f ..-. }
    { g --. }
    { h .... }
    { i .. }
    { j .--- }
    { k -.- }
    { l .-.. }
    { m -- }
    { n -. }
    { o --- }
    { p .--. }
    { q --.- }
    { r .-. }
    { s ... }
    { t - }
    { u ..- }
    { v ...- }
    { w .-- }
    { x -..- }
    { y -.-- }
    { z --.. }
    { 1 .----}
    { 2 ..---}
    { 3 ...--}
    { 4 ....-}
    { 5 .....}
    { 6 -....}
    { 7 --...}
    { 8 ---..}
    { 9 ----.}
    { 0 -----}
    { ? ..--..}
    { . .-.-.-}
    { , --..-- }
}
#
proc toMorse {str} {
    global zircon

    set res {}
    foreach ch [split $str {}] {
	if {[set x [string first $ch $zircon(morseIndex)]] >= 0} {
	    append res [lindex [lindex $zircon(morse) $x] 1] {  }
	} {
	    append res $ch
	}
    }
    return $res
}
