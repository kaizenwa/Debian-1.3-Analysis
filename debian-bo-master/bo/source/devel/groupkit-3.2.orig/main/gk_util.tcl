#
#
# MODULE: gk_util.tcl  (Miscellaneous goodies)
#
#


# initialization code


proc gk_initApplication {} {
  global argv0
  gk_newenv userprefs
  gk_newenv registrar
  if {![file exists ~/.groupkitrc]} {
    if [member frame [info commands]] {
	_gk_setupNewUser
	tkwait window .newuser
    }
  }
  if [file exists ~/.groupkitrc] {
      uplevel #0 {source ~/.groupkitrc}
  }
  userprefs scriptpath [string range $argv0 0 [expr [string last / $argv0]-1]]
}

proc _gk_setupNewUser {} {
  global _gk_setupinfo gk_library
  catch {source $gk_library/main/dot_groupkitrc}  
  frame .newuser
  label .newuser.title -text "Welcome to GroupKit!"
  message .newuser.msg -aspect 400 -text "GroupKit needs a file called .groupkitrc in your home directory to store your preferences for GroupKit programs.  Enter the information requested below and click on \"Create Preferences\" to proceed.  You'll also want to take a look at the ~/.groupkitrc file that is created, and add in extra information about yourself that will be displayed to other users via the GroupKit \"participants\" box."

  frame .newuser.name
  pack [label .newuser.name.lbl -text "Your name:" -width 25 -anchor w] -side left -padx 10
  pack [entry .newuser.name.ent -relief sunken -borderwidth 2 \
	  -textvariable _gk_setupinfo(name) -width 30] -side left
  .newuser.name.ent insert 0 [userprefs name]

  frame .newuser.domain
  pack [label .newuser.domain.lbl -text "Internet Domain:" -width 25 -anchor w] -side left -padx 10
  pack [entry .newuser.domain.ent -relief sunken -borderwidth 2 \
	  -textvariable _gk_setupinfo(domain) -width 30] -side left
  .newuser.domain.ent insert 0 [userprefs internetdomain]

  frame .newuser.reghost
  pack [label .newuser.reghost.lbl -text "Machine running the registrar:" -width 25 -anchor w] -side left -padx 10
  pack [entry .newuser.reghost.ent -relief sunken -borderwidth 2 \
	  -textvariable _gk_setupinfo(reghost) -width 30] -side left
  .newuser.reghost.ent insert 0 [registrar host]

  frame .newuser.buttons
  pack [button .newuser.buttons.ok -text "Create Preferences" \
	  -command {_gk_makePrefsFile}] -side left
  pack [button .newuser.buttons.cancel -text "Quit Program" \
	  -command "destroy ."] -side left

  pack append .newuser .newuser.title top .newuser.msg {top fillx pady 30} \
	  .newuser.name {top pady 20} \
	  .newuser.domain {top pady 20} \
	  .newuser.reghost {top pady 20} \
          .newuser.buttons {top pady 10}
  pack .newuser
}

proc _gk_makePrefsFile {} {  global gk_library _gk_setupinfo
  set fin [open $gk_library/main/dot_groupkitrc r] 
  set fout [open ~/.groupkitrc w]
  while {[gets $fin line] >= 0} {
    puts $fout $line
  }
  puts $fout "# the next four lines added by the user setup dialog"
  puts $fout "userprefs name \"$_gk_setupinfo(name)\""
  puts $fout "userprefs internetdomain \"$_gk_setupinfo(domain)\""
  puts $fout "registrar host \"$_gk_setupinfo(reghost)\""
  puts $fout "userprefs rooms.host \"$_gk_setupinfo(reghost)\""
  close $fin
  close $fout
  destroy .newuser
  }


# Error management

proc _gk_initGkErrorHandling {} {}

proc _gk_properQuit {} { 
    if {[userprefs isConference]=="yes"} {
      _gk_QuitConference
    } else {
      gk_shutdownServer
      after 1 "destroy ."
    }
}


# get the fully qualified host name of the current machine
#
proc _gk_getHostname {} {
    if {[userprefs host_override]!=""} {
	return [userprefs host_override]
    }
    set hostprefix [gk_info host]
    if {[string index [userprefs internetdomain] 0] == "."} {
        set host $hostprefix[userprefs internetdomain]
    } else {
        if {[userprefs internetdomain] == ""} {
            set host $hostprefix
        } else {
            set host $hostprefix.[userprefs internetdomain]
        }
    }
    return $host
}


##### color stuff

set _gk_colorlist [list PapayaWhip NavajoWhite MintCream AliceBlue \
			     DarkSlateGrey NavyBlue MediumTurquoise LightCyan \
			     DarkOliveGreen chartreuse OliveDrab \
			     LightGoldenrodYellow IndianRed chocolate \
			     LightSalmon VioletRed MediumPurple burlywood4 \
			     wheat3 plum3 ]
proc gk_getMyColour {} { global _gk_colorlist
  if {[userprefs color] == ""} {
    userprefs color [lindex $_gk_colorlist [expr [users local.usernum]%20]]
  }
  return [userprefs color]
}


proc gk_colorPicker {cmd} {  global _gk_colorlist
    set w .colordlg
    toplevel $w
    wm title $w "Color Picker"
    pack [frame $w.left] -side left -padx 5 -pady 5
    pack [entry $w.left.color] -side bottom -fill x -pady 5
    pack [listbox $w.left.list -yscrollcommand "$w.left.scroll set"] \
	    -side left
    pack [scrollbar $w.left.scroll -command "$w.left.list yview"] \
	    -side right -fill y
    foreach i $_gk_colorlist {$w.left.list insert end $i}
    bind $w.left.list <ButtonRelease-1> _gk_color_picker_select
    pack [frame $w.right] -side left
    pack [message $w.right.msg -text "Choose a color from the list at the \
left or type a color name in the entry below the list.  To make the change \
permanent, you'll need to edit your ~/.groupkitrc file."] -side top
    pack [frame $w.right.sample -height 30 -width 50 -borderwidth 3 -relief sunken] -side top -pady 4
    pack [button $w.right.ok -text "Set Color" -width 10 -command "_gk_color_picker_do [list $cmd]"] -pady 5
    pack [button $w.right.cancel -text Cancel -width 10 -command "destroy $w"]
}

proc _gk_color_picker_select {} {
    set w .colordlg
    set sel [$w.left.list get [lindex [$w.left.list curselection] 0]]
    $w.right.sample configure -bg $sel
    $w.left.color delete 0 end
    $w.left.color insert 0 $sel
}

proc _gk_color_picker_do {cmd} {
    set w .colordlg
    set color [$w.left.color get]
    if {$color!=""} {
	uplevel #0 "eval $cmd $color"
    }
    destroy $w
}
# Convert float to int
proc toint {num} {
  return [format "%.0f" $num]
}


##########################################################################
##
## SUPPORT FOR SCHEDULING COMMAND EXECUTION
##  
##   This package allows you to deal with executing repetitive
##   commands (such as move object messages) so that commands do
##   not cause a backlog.  Commands (such as an update) are added
##   to a queue without duplicates, so only a single command will
##   be executed within a given time interval (default 1/10 second). 
##
##########################################################################

gk_newenv scheduler

### schedule a command to be executed sometime in the future
###   - if a command with the same tag exists, don't schedule this command
proc gk_schedule {tag command {timer 100}} {
    if ![scheduler exists schedule.$tag] {
        scheduler set schedule.$tag $command
        after $timer _gk_runScheduled $tag
    }   
}

### cancel a previously scheduled command if not already run
proc gk_cancelScheduled tag {
    scheduler delete schedule.$tag
}

### run a scheduled command if it has not been cancelled
proc _gk_runScheduled tag {
    set cmd [scheduler get schedule.$tag]
    if {$cmd!=""} {
        scheduler delete schedule.$tag
        uplevel #0 $cmd
    }
}

