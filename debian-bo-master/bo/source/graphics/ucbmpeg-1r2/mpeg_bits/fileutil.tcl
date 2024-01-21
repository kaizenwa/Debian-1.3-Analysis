#
# util.tcl -- Procedures that are used in a bunch of different files.
#

#--------------------------------------------------------------------
# Fullname --
#
#	Given an absolute, relative, or ~user directory path, or a
#	path that includes symbolic links, return the full absolute
#	path name.
#
# dir:	The name of the directory to expand.
#--------------------------------------------------------------------
proc Fullname {dir} {
  set x [pwd]
  if [catch {cd $dir} msg] {
    tkerror $msg
    return $x
  }
  set y [pwd]
  cd $x
  return $y
}

#--------------------------------------------------------------------
# SaveBindings --
#
# 	Delete all the bindings for a window, and save them in global
# 	variables that all begin with the characters in the specified
#	name. Returns the number of bindings.  
#
# w	The window.
# name	The name to prepend to all the global variables created
#--------------------------------------------------------------------
proc SaveBindings {w name} {
  set i 0
  foreach bd [bind $w] {
    global $name.savebd$i
    set $name.savebd$i "$bd {[bind $w $bd]}"
    bind $w $bd ""
    incr i
  }
  return $i
}

#--------------------------------------------------------------------
# RestoreBindings --
#
#	Restore all the bindings that were saved and unbound by
#	SaveBindings.  The name passed in must be the same as the
#	name passed in to SaveBindings.
#
# w	The window to rebind.
# i	The number of bindings.
# name	The name to prepend to all the global variables read.
#--------------------------------------------------------------------
proc RestoreBindings {w i name} {
  for {set num 0} {$num<$i} {incr num} {
    global $name.savebd$num
    
    eval bind $w [set $name.savebd$num]
  }
}  

#--------------------------------------------------------------------
# Grid --
#
#       Align the input coordinate in ATUs to the nearest grid point
#       for the current spot widget.
#
# arg   The input coordinate, in the form "x y".
#--------------------------------------------------------------------
proc Grid {arg} {
  global talk

  set grid $talk(grid)
  set x [expr int([lindex $arg 0]/$grid+.5)*$grid]
  set y [expr int([lindex $arg 1]/$grid+.5)*$grid]
  return "$x $y"
}
	  
#--------------------------------------------------------------------
# Listselected --
#
# 	Given a listbox, return a number from 0 to N if a single line
#	is selected, or -1 if no lines or more than 1 line are
#	selected.
#
# list	The name of the listbox widget.
#--------------------------------------------------------------------
proc Listselected {list} {
  set found [$list curselection]
  set first [lindex $found 0]
  if {$first != "" && $first == [lindex $found 1]} {
    return $first
  }
  return -1
}

#--------------------------------------------------------------------
# Grab --
#
#	Grab the mouse and give focus to the window, recording
#	information about who used to own the grab (if anyone).
#
# w	The toplevel window to grab.
#--------------------------------------------------------------------
proc Grab {w} {
  global widget

  set place [lsearch $widget(grab) $w]
  if $place!=-1 {
    set widget(grab) [lreplace $widget(grab) $place $place]
  } 
  if [llength $widget(grab)]==0 {
    if {$widget(focus) != ""} {
      error "Problem in ``Grab $w''"
    }
    set widget(focus) [focus]
  }
  lappend widget(grab) $w
  Focus $w
  bind $w <Any-Destroy> "Ungrab $w"
  bind $w <Any-Visibility> "MakeVisible $w \"%W\""
  grab set $w
}

proc MakeVisible {win eventWin} {
  set x [grab current $win]
  if {$x == $eventWin} {raise $eventWin}
}

#--------------------------------------------------------------------
# Ungrab --
#
#	When a modal dialog box is going away, give the grab and focus
#	back to the previous owners (if any).
#	The catch on "focus $widget(focus)" is needed for file load;
#	widget(focus) will be the name of the window that invoked the
#	file load, but the process of loading will destroy that window.
#
# w	The modal dialog box that is going away.
#--------------------------------------------------------------------
proc Ungrab {w} {
  global widget focus 

  set place [lsearch $widget(grab) $w]
  if $place==-1 {
    error "Problem removing $w from grab list ``$widget(grab)''"
  }
  set widget(grab) [lreplace $widget(grab) $place $place]
  set l [llength $widget(grab)]
  if $l>0 {
    set t [lindex $widget(grab) [expr $l-1]]
    focus $focus($t)
    grab $t
    raise $t
  } else {
    grab release .
    catch {focus $widget(focus)}
    set widget(focus) ""
  }
  bind $w <Any-Destroy> ""
  bind $w <Any-Visibility> ""
}

proc Focus {w} {
  global focus oldfocus
  
  set t [winfo toplevel $w]
  if [info exists oldfocus($t)] {
    QuickUnfocus
  }
  set focus($t) $w
  focus $w
}

#--------------------------------------------------------------------
# Wait
#
#	Wait for a toplevel window to become mapped.
#
# w	The window.
#--------------------------------------------------------------------
proc Wait {w} {
  global waiting

  if [winfo ismapped $w] {
    return
  }
  set waiting 0
  bind $w <Any-Map> {set waiting 1}
  tkwait variable waiting
  update
  bind $w <Any-Map> ""
}


#--------------------------------------------------------------------
# Nothing --
#
#	Do nothing.  Used when want to ignore arguments.
#--------------------------------------------------------------------
proc Nothing {args} {
  return 0
}

#--------------------------------------------------------------------
# UniqueName --
#
#	Search through all the elements in a listbox and find a name
#	that is not in use in the listbox.  The name is constructed
#	from a base string plus a number.  Used in the various menu
#	editing dialogs to find a name for a newly created color,
#	look, etc.  The newly created name is inserted at the end
#	of the listbox.
#
#	Returns the name that was created from the base string, or
#	"" if no name could be created.
#
# list	The listbox that needs a new element.
# base	The base string for the new element.
#--------------------------------------------------------------------
proc UniqueName {list base} {
  set s [string length $base]
  set l [$list size]
  set nums {}
  for {set i 0} {$i<$l} {incr i} {
    set t [$list get $i]
    if [string match ${base}* $t] {
      set n [string range $t $s end]
      if [catch {expr $n}]==0 {
        lappend nums $n
      }
    }
  }
  for {set i 1} {$i<1000} {incr i} {
    case $i $nums {} default {
      return $base$i
    }
  }
  return ""
}

#--------------------------------------------------------------------
# Geometry --
#
#	Both "winfo geometry" and "wm geometry" give the wrong answer
#	for windows.  The width and height returned by "wm" are
#	usuallly wrong, and the x, y location returned by "winfo"
#	is usually wrong.  Try to deal with it, and return the
#	correct geometry for a window.
#
# w	The window.
#--------------------------------------------------------------------
proc Geometry {w} {
  set winfo [winfo geometry $w]
  set winfo [string range $winfo 0 [expr [string first + $winfo]-1]]
  set wm [wm geometry $w]
  if {$winfo == "1x1"} {
    set winfo ""
  } else {
    set wm [string range $wm [string first + $wm] end]
  }
  return $winfo$wm
}

#--------------------------------------------------------------------
# print --
#
#	Print the string only if debugging is turned on.
#
# str	The string to print.
#--------------------------------------------------------------------
proc print {str} {
  global debugTool

  if $debugTool {
    puts stdout $str
  }
}

proc parray a {
    global $a
    set maxl 0
    foreach name [lsort [array names $a]] {
	if {[string length $name] > $maxl} {
	    set maxl [string length $name]
	}
    }
    set maxl [expr {$maxl + [string length $a] + 2}]
    foreach name [lsort [array names $a]] {
	set nameString [format %s(%s) $a $name]
	puts stdout [format "%-*s = %s" $maxl $nameString [set ${a}($name)]]
    }
}

proc Center {w} {
  set x [expr int(([winfo screenwidth $w]-[winfo width $w])/2)]
  set y [expr int(([winfo screenheight $w]-[winfo height $w])/2)]
  wm geometry $w +[set x]+[set y]
  print [winfo geometry $w]
  print [wm geometry $w]
}

#--------------------------------------------------------------------
# CountCmd --
#
#	Counts the number of Tcl commands executed.
#--------------------------------------------------------------------
proc CountCmd {cmd} {
    set a [info cmdcount]
    eval $cmd
    set b [info cmdcount]
    return [expr $b-$a-3]
}

proc List {} {
  set w .command
  if {[info command $w] != ""} {
    wm deiconify $w
    raise $w
    return
  }
  toplevel $w
  for {set i 1} {$i<6} {incr i} {
    frame $w.$i
    label $w.$i.l -text $i -width 2
    entry $w.$i.e -relief sunken -width 40 
    bind $w.$i.e <Any-Return> "puts stdout \"Result $i: \[eval \[%W get]]\""
    pack append $w.$i $w.$i.l left $w.$i.e left
    pack append $w $w.$i "top pady 6 padx 6"
  }
}
