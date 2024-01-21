#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/decrypt.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
#
proc decrypt {string key} {
    if [string match {} $key] { return {<ENCRYPTED TEXT>} }
    global zircon
    return [exec $zircon(lib)/crypt D "$string" "$key"]
}
#
proc encrypt {string key} {
    if [string match {} $key] { return $string }
    global zircon
    return "\001SED [exec $zircon(lib)/crypt E "$string" "$key"]\001"
}
