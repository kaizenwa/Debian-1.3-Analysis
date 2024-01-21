#
# fileio.tcl - a simple file save/load utility.
#
# Routines:
#	FileDialog
#	

set file(retry) 0

#--------------------------------------------------------------------
# SaveDialog --
#
#	Create a Save dialog box, which consists of a file/directory
#	chooser and a panel to specify the file creation permissions.
#	This procedure is invoked to save the internal database.
#
# dialogTitle	The title for the dialog.
# saveCmd       Command to execute to save.
#--------------------------------------------------------------------
proc SaveDialog {{dialogTitle "Save Dialog"} \
                 {saveCmd "puts -nonewline stderr {Save in: }; puts stderr"}} {
  global file 

  set name $file(name)
  set w .save
  IOBox $w $dialogTitle Save $saveCmd $name
  
  label $w.p.right.per -text " Permissions"
  set q $w.p.right.1
  frame $q -relief sunken -bd 1
  frame $q.per -relief sunken
  label $q.per.empty -text "" -padx 0 -pady 0
  label $q.per.read -text Read -padx 0 -pady 0
  label $q.per.write -text Write -padx 0 -pady 0
  pack $q.per.empty $q.per.read $q.per.write -side top -expand yes
  frame $q.group
  label $q.group.label -text Group -padx 0
  checkbutton $q.group.read -var file(gr) -com {SetMask $file(gr) 040} \
  	-text "" -padx 0 -pady 0 -relief flat -anchor center
  checkbutton $q.group.write -var file(gw) -com {SetMask $file(gw) 020} \
  	-text "" -padx 0 -pady 0 -relief flat -anchor center
  pack $q.group.label -side top -anchor w
  pack $q.group.read $q.group.write -side top 
  frame $q.world
  label $q.world.label -text World -padx 0
  checkbutton $q.world.read -var file(wr) -com {SetMask $file(wr) 004} \
  	-text "" -padx 0 -pady 0 -relief flat -anchor center
  checkbutton $q.world.write -var file(ww) -com {SetMask $file(ww) 002} \
  	-text "" -padx 0 -pady 0 -relief flat -anchor center
  pack $q.world.label $q.world.read $q.world.write -side top
  pack $q.per $q.group $q.world -side left

  pack $w.p.right.1 -side bottom
  pack $w.p.right.per -side bottom -anchor w

  if $file(mask)&040 {$q.group.read select}
  if $file(mask)&020 {$q.group.write select}
  if $file(mask)&004 {$q.world.read select}
  if $file(mask)&002 {$q.world.write select}

  tkwait window $w
}

#--------------------------------------------------------------------
# LoadDialog --
#
# 	Create a Load dialog box, which consists of a file/directory
#	chooser.  Uses the global variable $file to identify default file
#   and permissions.
#
# dialogTitle   The title for the dialog.
# LoadCmd       Command to execute to load.
#--------------------------------------------------------------------
proc LoadDialog {{dialogTitle "Load Dialog"} \
		{loadCmd "puts -nonewline stderr {Load from: }; puts stderr"}} {
  global file

  set w .load
  IOBox $w $dialogTitle Open $loadCmd $file(name)

}

#--------------------------------------------------------------------
# SetMask --
#
#	A utility procedure used by SaveBox to set or clear various
#	bits in the file creation permission mask.
#
# add 	0 means remove permission, 1 means add permission.
# prec 	The bits of permission to add or remove.
#--------------------------------------------------------------------
proc SetMask {add prec} {
  global file
  if $add {
    set file(mask) [expr $file(mask)|$prec]
  } else {
    set file(mask) [expr $file(mask)&~$prec]
  }
}

#--------------------------------------------------------------------
# IOBox --
#
#	Create an iobox, which consists of a title, entry, scrollbox,
#	and two buttons.  The iobox allows the user to select a
#	directory of filename by typing in the entry widget and
#	pressing return, or by double clicking in the listbox.  When
#	a directory is selected, the listbox changes to display that
#	directory.  When a file is selected, the command passed in
#	by the user is called with the name of the file as an
#	argument.  
#
# 	If the user specified command returns an error, then the
#	window remains posted.  Otherwise, when the user specified
#	command sucessfully finishes, this window goes away.
#
# w 	  Name for the iobox.
# title   Title for the iobox.
# button  String that goes in the first button.
# cmd 	  The user specified command to execute when a file is selected.
# name 	  The initial string that goes in the entry in the filebox.
#--------------------------------------------------------------------
proc IOBox {w title button cmd name} {
  global file
  
  Dialog $w 1 $title
  frame $w.p -bd 1 -relief raised 
  label $w.p.cwd -anchor w

  set file(working) $name
  set file(cancel) 0
  frame $w.p.left
  label $w.p.left.label -text Filename
  entry $w.p.left.entry -relief sunken
  ScrollBox $w.p.left.list 20x15
  pack $w.p.left.label -side top -anchor w
  pack $w.p.left.entry -side top -expand yes -fill x -padx 3
  pack $w.p.left.list -side top -fill x -pady 7

  set cmd "IOFinished {$cmd} $w"
  set list $w.p.left.list.list
  bind $list <Any-Double-Button> \
    "FilePress $w \"\[%W get \[%W nearest %y]]\" {$cmd}"
  bind $list <Any-ButtonRelease> \
    "if %x>0&&%x<\[winfo width $list] {Editname $w}"
  Editname $w
  $w.p.left.entry select from 0
  $w.p.left.entry select to end
  Browse $w
  Focus $w.p.left.entry  

  frame $w.p.right 
  set b [DefaultButton $w.p.right.def -text $button -padx 0 \
  	-com "FileReturn $w {$cmd}"]
  button $w.p.right.can -text Cancel -pady 5 -padx 0 -wid 7 \
  	-com "destroy $w; set file(cancel) 1 "

  pack $w.p.right.def -side top -pady 5 -padx 40
  pack $w.p.right.can -side top -pady 5
  
  pack $w.p.cwd -side bottom -fill x
  pack $w.p.left -side left -padx 4
  pack $w.p.right -side right -fill y -pady 8
  pack $w.p -side top -expand yes -fill both

  bind $w.p.left.entry <Any-Return> "$b flash; $b invoke"
}

#--------------------------------------------------------------------
# IOFinished --
#
# 	A utility procedure used by IOBox.  If the user command
#	succeeds, then destroy the window, else continue.
#
# cmd 	The command to execute.
# w 	The window to destroy.
# file 	The file to run the command on.
#--------------------------------------------------------------------
proc IOFinished {cmd w name} {
  global file
  
  if [catch {eval $cmd $name} msg] {
    error $msg "" WARN
  }
  if $file(retry)==0 {
    destroy $w
  }
  set file(retry) 0
}

#--------------------------------------------------------------------
# FileReturn --
#
# 	Utility procedure invoked when the Return key is pressed or
#	the default button is invoked.
#
#	  If the entry widget contains the name of a directory, open
#	that directory.
#	  Otherwise, if the listbox contains a selected element, open
#	the file or directory named by that element.
#	  Otherwise, open the file name in the entry widget.
#
# w	The iobox
# cmd	The command to execute when a file is selected.
#--------------------------------------------------------------------
proc FileReturn {w cmd} {
  global file

  set goto [$w.p.left.entry get]
  if {$goto == "" || [file isdirectory [FilePath $goto]] == 0} {
    set sel [$w.p.left.list.list cursel]
    if {$sel != ""} {
      set new [$w.p.left.list.list get $sel]
      if [file isdirectory [FilePath $new]] {
        set goto $new
      }
    }
  }
  FilePress $w $goto $cmd
  catch {
    $w.p.left.entry delete 0 end
    $w.p.left.entry insert 0 $file(working)
  }
}

#--------------------------------------------------------------------
# FilePress --
#
#	Utility procedure invoked when a mouse button is pressed in
#	the listbox, or when the return key has been pressed when the
#	entry doesn't contain a directory.
#	
# 	Given the name of a file, execute the user command.
#	Given the name of a directory, change the CWD to that
#	directory and redisplay the filebox.
#	The filename and directory name may be specified relative to
#	the CWD, or may be an absolute path name.
#
# w	The iobox.
# sel	The name of either a file or a directory.
# cmd	The command to execute when a file is selected.
#--------------------------------------------------------------------
proc FilePress {w sel cmd} {
  global file
  
  if [string compare $sel ""]==0 {
    return
  }
  set path [FilePath $sel]

  if [file isdirectory $path] {
    set file(cwd) [Fullname $path]
    Browse $w
    return
  }
  set dir [file dirname $path]
  if [file isdirectory $dir] {
    set file(cwd) [Fullname $dir]
    eval $cmd "{$path}"
    return
  }
  error "Unknown file or directory: $sel" "" WARN
  
}

#--------------------------------------------------------------------
# Editname --
#
# 	Utility procedure used by Filebox.  If a single line in the
#	listbox is selected, then display that line in the entry.
#	Otherwise, if 0 or more than 1 line is selected, display
# 	the default string in the entry.
#
# w	The filebox.
#--------------------------------------------------------------------
proc Editname {w} {
  global file

  set sel [$w.p.left.list.list cursel]
  if {$sel != ""} {
    set goto [$w.p.left.list.list get $sel]
    if [file isdirectory $file(cwd)/$goto] {
      set file(working) [$w.p.left.entry get]
      return
    } else {
      set file(working) $goto
    }
  }
  $w.p.left.entry delete 0 end
  $w.p.left.entry insert 0 $file(working)
}
  
#--------------------------------------------------------------------
# Browse --
#
#	Insert the contents of the current working directory (CWD)
#	into a filebox.  Also copy the name of the CWD into the label.
#
# w	The filebox to be filled in with the contents of the cwd.
#--------------------------------------------------------------------
proc Browse {w} {
  global file
  
  set t [winfo toplevel $w]
  set cursor [lindex [$t config -cursor] 4]
  $t config -cursor watch
  if [winfo ismapped $t] {
    update idletasks
  }
  $w.p.cwd configure -text $file(cwd)
  set files [lsort [exec ls $file(cwd)]]
  $w.p.left.list.list delete 0 end
  $w.p.left.list.list yview 0
  $w.p.left.list.list insert end ../
  foreach i $files {
    if [string match .* $i]==0 {
      if [file isdirectory $file(cwd)/$i] {
        append i /
      }
      $w.p.left.list.list insert end $i
    }
  }
  $t config -cursor $cursor
}    


#
# Given a relative path, fill in the absolute part of the path.
# Given an absolute path, return the path.
#
proc FilePath {path} {
  global file

  case [string index $path 0] \
  ~ {return "[file rootname $path][file extension $path]"} \
  / {return $path} \
  default {return $file(cwd)/$path}
}


#--------------------------------------------------------------------
# Dialog --
#
#   Create a new dialog box.
#
# w The name for the new dialog box.
# trans If non-zero, then this is a transient dialog box.
# args  Additional arguments (only for non-transient dialogs).
#   args[0]: The title for the window.
#   args[1]: The icon title for the window.
#   args[2]: The initial geometry of the window.
#--------------------------------------------------------------------
proc Dialog {w trans args} {
  global oldfocus focus

  catch "destroy $w"
  catch "unset oldfocus($w)"
  toplevel $w -class Dialog -bd 0
  wm title $w [lindex $args 0]
#
#  bind $w <Any-FocusIn> "if {\$widget(grab) == \"\"} {focus \$focus($w)}"
#
  if $trans {
    wm protocol $w WM_DELETE_WINDOW Nothing
    wm geometry $w +425+300
    Grab $w
    bind $w <Any-Unmap> {wm deiconify %W; update}
  } else {
    wm iconbitmap $w perspecta
    wm iconname $w [lindex $args 1]
    wm protocol $w WM_DELETE_WINDOW "$w.but.cancel invoke"
    if {[lindex $args 2] != ""} {
      wm geometry $w [lindex $args 2]
    }
    set focus($w) $w
  }
  bind $w <Any-Help> "Help $w"
  bind $w <Any-F1> "Help $w"
}

#--------------------------------------------------------------------
# ScrollBox --
#
#   Make a new paired listbox and vertical scrollbar.
#
# w The name of the "scrollbox".  The listbox is called $w.list,
#   and the scrollbar is called $w.scroll
# geo   The geometry for the listbox.
#--------------------------------------------------------------------
proc ScrollBox {w geo} {
  frame $w
  scrollbar $w.scroll -relief sunken -com "$w.list yview"
  listbox $w.list -relief sunken -yscroll "$w.scroll set" -geometry $geo
  pack $w.scroll -side right -fill y -expand yes -padx 4
  pack $w.list -side left -expand yes -fill x
}

#--------------------------------------------------------------------
# DefaultButton --
#
#   Create a default-style button.  Returns the actual name of
#   the button, which is a button widget buried inside 2 frames
#   to provide the default-style look.
#
# w The name for the default-style button.
# args  Configuration strings for the button widget.
#--------------------------------------------------------------------
proc DefaultButton {w args} {
  frame $w -relief raised -bd 1
  frame $w.0 -relief sunken -bd 1
  eval button $w.0.0 -relief raised -wid 7 -padx 2 -pady 5 $args
  pack append $w $w.0 {left expand padx 4 pady 4}
  pack append $w.0 $w.0.0 {left expand padx 3 pady 3}
  return $w.0.0
}

