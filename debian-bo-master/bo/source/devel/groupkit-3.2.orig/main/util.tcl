# miscellaneous stuff


# insert an item into a list
#
proc Insert {ListName Item} {
    upvar 1 $ListName List
    lappend List $Item
}


# remove an item from a list
#
proc Remove {ListName Item} {
    upvar 1 $ListName List
    set Pos [lsearch $List $Item]
    Assert "$Pos >= 0"
    set List [lreplace $List $Pos $Pos]
}

# reverse a list
#
proc Reverse List {
    set Ans {}
    while {[llength $List] > 0} {
        set Item [lindex $List [expr [llength $List]-1]]
        Insert Ans $Item
        Remove List $Item
    }
    return $Ans
}

# Convert a list of window pathnames to a list of components
#     example lowToLol {.1.a.A} right 2   returns    {a A}
# side refers whether to read the pathnames from right to left or
# from left to right.
# number is how many "children" or "parents" you want.
#
proc lowToLol {winList side number} {
    set list {}
    
    foreach window $winList {
        set window [split $window .]
        set max [llength $window]
        if { [expr $max -1] < $number } {
            error "Bad number, $number, supplied to lowToLol"
        }
        if { [string first $side right] != -1} {
            set newItem {}
            for {set count [expr $max - $number]} {$count < $max} \
                    {incr count} {
                lappend newItem [lindex $window $count]
            }
        } elseif { [string first $side left] != -1 } {
            set newItem {}
            for {set count 1} {$count < [expr $number+1] && $count < $max} \
                    {incr count} {
                lappend newItem [lindex $window $count]
            }
        } else {
            error "Incorrect side, $side, for lowToLol: must be left or right."
        }
        lappend list $newItem
    }
    return $list
}

# check if item is a member of the list
#
proc member {item list} {
  return [expr [lsearch $list $item]!=-1]
}


# assert a condition
#
proc Assert Cond {
    if "$Cond" {
    } else {
	tkerror "Assertion failed"
    }	
}


###########   Routines to make an application busy or unbusy  #########

# Busy changes cursor to a watch cursor for all windows
proc busy {} {
  # Commented out: MUCH too slow
  #forAllWindows setBusy
  foreach w [winfo children .] {
    setBusy $w
  }
}

proc setBusy {w} {
  global oldCursor
  if {$w != "."} {
    set oldCursor($w) [lindex [$w config -cursor] 4]
    $w config -cursor watch
    if ![catch {set oldState($w) [$w cget -state]}] {
       catch {$w config -state disabled}
    }
  }
}

# Unbusy changes cursor back to the original cursor cursor for all windows
proc unbusy {} {
  #forAllWindows setUnbusy
  foreach w [winfo children .] {
    setUnbusy $w
  }
}

proc setUnbusy {w} {
  global oldCursor oldState
  if {$w != "."} {
    if [info exists oldCursor($w)] {
      $w config -cursor $oldCursor($w)
    }
    if [info exists oldState($w)] {
	catch {$w config -state $oldState($w)}
    }
  }
}


########  Bolean Stuff

# if the variable is "yes|true|1" or any abbreviation of yes or true
# then 1 is return - 0 is returned otherwise
#
proc isTrue {var} {
    if { [string first $var yes] == 0 } { return 1  
    } elseif { [string first $var true] == 0 } { return 1  
    } elseif { $var == 1 } { return 1  
    } else { return 0
    }
}

# if the variable is "no|false|0" or any abbreviation of no or false
# then 1 is return - 0 is returned otherwise
#
proc isFalse {var} {
    if { [string first $var no] == 0 } { return 1 
    } elseif { [string first $var false] == 0 } { return 1
    } elseif { $var == 0 } { return 1
    } else { return 0
    }
}

# if the variable is "no|yes|true|false|0|1" or any abbreviation of 
# no, yes, true or false then 1 is return - 0 is returned otherwise
#
proc isBoolean {var} {
    if [string first $var yes] { return 1 
    } elseif [string first $var true] { return 1
    } elseif { $var == 1 } { return 1
    } elseif [string first $var no] { return 1 
    } elseif [string first $var false] { return 1
    } elseif { $var == 0 } { return 1
    } else { return 0
    }
}   


proc gk_debug msg {
    if {[userprefs debugevents]==1} {
	puts $msg
    }
}

##### debugging console

proc gk_debug_console {} {
        toplevel .debug
        catch {wm title .debug "Debug Console for [tk appname]"}
        pack [frame .debug.cmd] -side top -fill x
        pack [label .debug.cmd.lbl -font 6x10 -text Cmd:] -side left
        pack [entry .debug.cmd.entry -font 6x10] -side left -fill x \
                        -expand yes
        bind .debug.cmd.entry <Return> {
                set res [eval [.debug.cmd.entry get]]
                .debug.t insert end "$res\n"
                .debug.cmd.entry delete 0 end
        }
        pack [text .debug.t -yscrollcommand ".debug.s set" -font 6x10 \
                        -width 60] -side left -fill both -expand yes
        pack [scrollbar .debug.s -command ".debug.t yview"] -side left \
                        -fill y
}
