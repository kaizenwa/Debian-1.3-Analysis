

set _gk_notifier(id) 1

proc gk_notifier name {
  global _gk_notifier
  if {$name=="-anonymous"} {
    set name "gk_notifier$_gk_notifier(id)"
    incr _gk_notifier(id)
  }
  if {[lsearch [info commands] $name]!=-1} {
    error "could not create command"
  }
  eval "proc $name \{args\} \{return \[_gk_notifier_cmd $name \$args\]\}"
  return $name
}


proc _gk_notifier_cmd {name cmds} {
  global _gk_notifier
  set cmd [lindex $cmds 0]
  switch -exact $cmd {
    bind {
      if {[llength $cmds]!=3} {error "wrong # args"}
      set id $_gk_notifier(id); incr _gk_notifier(id)
      set _gk_notifier(bind$id::script) [lindex $cmds 2]
      set _gk_notifier(bind$id::event) [lindex $cmds 1]
      lappend _gk_notifier($name::[lindex $cmds 1]) bind$id
      return bind$id
    }
    destroy {
      if {[llength $cmds]!=1} {error "wrong # args"}
      rename $name ""
      return
    }
    delete {
      if {[llength $cmds]!=2} {error "wrong # args"}
      set binding [lindex $cmds 1]; 
      if {[info exists _gk_notifier($binding::script)]==1} {
	set event $_gk_notifier($binding::event)
	unset _gk_notifier($binding::script)
	unset _gk_notifier($binding::event)
        set bindlist $_gk_notifier($name::$event)
        set posn [lsearch $bindlist $binding]
        if {$posn!=-1} {
	  set _gk_notifier($name::$event) [lreplace $bindlist $posn $posn]
	}
        return 
      } else {
	error "no such binding"
      }
    }
    notify {
      if {[llength $cmds]!=3} {error "wrong # args"}
      set event [lindex $cmds 1]; set subst [lindex $cmds 2]
      if {[info exists _gk_notifier($name::$event)]==1} {
	foreach i $_gk_notifier($name::$event) {
	  set cmd $_gk_notifier($i::script)
	  set newcmd ""
	  while {$cmd!=""} {
	    set idx [string first % $cmd]   
            if {$idx==-1} {
	      set newcmd "$newcmd$cmd"
	      set cmd ""
	    } else {
	      set newcmd "$newcmd[string range $cmd 0 [expr $idx-1]]"
	      set cmd [string range $cmd [expr $idx] end]
	      set char [string range $cmd 1 1]
              switch $char {
		E {set newcmd "$newcmd$event"}
		% {set newcmd "$newcmd%"}
		default {
		  foreach j $subst {
		    if {[lindex $j 0]==$char} {
		      set newcmd "$newcmd[list [lindex $j 1]]"		    
		    }
		  }
		}
	      }
              set cmd [string range $cmd 2 end]
            }
          }
          catch {uplevel #0 $newcmd}
	}
      }
    }
    default {
      error "invalid gk_notifier subcommand"
    }
  } 
}
