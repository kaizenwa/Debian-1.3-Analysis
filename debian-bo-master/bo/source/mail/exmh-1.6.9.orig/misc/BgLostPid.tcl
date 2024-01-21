#
# From:	Scott Hammond <scott@sctc.com>
#
# On Solaris 2.2 and 2.3, `/usr/ucb/ps pid' says,
#       "can't find controlling terminal", though `/bin/ps -p pid' works fine.
# Modded to detect the controlling terminal error, and
# modded to home in on the right ps, and the right option, so that
# it doesn't do wasteful fork/execs after the first time through.
#
set ps ps
set ps_opt ""
proc BgLostPid { pid {name notused} } {
    global ps ps_opt
    #puts stderr "ps: $ps, ps_opt: $ps_opt, pid: $pid"
    if [catch "exec $ps $ps_opt $pid" err] {
        #puts stderr "ps error: $err"
        if [string match {[Uu]sage:*} $err] {
            # got usage, so ps must be right, -p should also be right
            set ps_opt "-p"
            return [catch {exec $ps -p $pid}]
        } elseif [string match {*can't find controlling terminal} $err] {
            if {"$ps" == "ps"} {
                set ps "/bin/ps"
            } elseif {"$ps" == "/bin/ps"} {
                set ps "/usr/ucb/ps"
            } else {
                return 1
            }
            return [BgLostPid $pid $name]
        } else {
            return 1
        }
    } else {
        foreach line [split $err \n] {
            if {[string compare [lindex $line 0] $pid] == 0} {
                return 0
            }
        }
        return 1
    }
}
