#
#  gk_defaultMenu - create a standard GroupKit menubar
#
#      See the manual pages for more information.
#

proc gk_defaultMenu {w args} {
    eval gkInt_CreateWidget $w gkDefaultMenu GkDefaultMenu $args
    return $w
}

proc gkDefaultMenu_CreateClassRec {} {
    global gkDefaultMenu

    set gkDefaultMenu(inherit) {menubutton}
    set gkDefaultMenu(methods) { add delete itemconfigure itemcommand \
				   itemlist }
}

proc gkDefaultMenu_InitWidgetRec {w class className args} {
    upvar #0 $w gk_menu

    set gk_menu(bar) $w

    # Cosmetics
    set gk_menu(-borderwidth)     2

    # Default Text for the Default Menubuttons
    set gk_menu(about_text) "About Groupkit..."
    set gk_menu(collab_text) "Collaboration"
    set gk_menu(file_text)  "File"
    set gk_menu(help_text) "Help"
    set gk_menu(quit_text) "Quit"
    if {[userprefs isConference]=="yes"} {
      set gk_menu(participant_text) "Show Participants"
    } else {
      set gk_menu(participant_text) "Show Logged-in Users"
    }

    # ItemList Initailly nothint
    set gk_menu(itemList) { }
}

proc gkDefaultMenu_ConstructWidget {w} {
    upvar #0 $w gk_menu

    # A file menu containing a quit button
     _gk_defaultMenu_add $w file 0 \
		       -menu "$gk_menu(bar).file.menu" \
		       -text $gk_menu(file_text) 

     $gk_menu(file_menu) add command \
	-label $gk_menu(quit_text) \
	-underline 0 \
	-command "_gk_properQuit"

    # A help menu containing the default groupkit help
    _gk_defaultMenu_add $w help 1 \
		  -menu "$gk_menu(bar).help.menu" \
		  -text $gk_menu(help_text) 
    $gk_menu(help_menu) add command \
	-label $gk_menu(about_text) \
	-command "gk_make_about_box"

    $gk_menu(help_menu) add command \
	-label "Debug..." \
	-command gk_debug_console

    # A Collaboration menu containing the default particpant button
    _gk_defaultMenu_add $w collaboration 1 \
			 -menu "$gk_menu(bar).collaboration.menu" \
			 -text $gk_menu(collab_text) 
     if {[userprefs isConference]=="yes"} {
       $gk_menu(collaboration_menu) add command \
           -label $gk_menu(participant_text) \
	   -command "gk_toplevelConfParticipants"
     } else {
       $gk_menu(collaboration_menu) add command \
           -label $gk_menu(participant_text) \
	   -command "gk_toplevelRegParticipants"
     }

    $gk_menu(rootCmd) config -relief ridge  -borderwidth 2
    _gk_defaultMenu_repack $w 
}

proc gkDefaultMenu_Config {w option args} {
     upvar #0 $w gk_menu

     set args [lindex $args 0]
     set whichMenu [lindex $args 0]

     if { [lsearch $gk_menu(itemList) $whichMenu] >= 0 } {
        set args [lindex $args 1]
	eval $gk_menu($whichMenu) config $option $args
     } else {
         $gk_menu(rootCmd) config $option $args
     }
}

proc gkDefaultMenu_Methods {w command args} {
     upvar #0 $w gk_menu
     set args [lindex $args 0]

     # Determine if the command is an index; either a number value
     # or a element in the itemList
     if { [string match {[0-9]} $command] } {
        set whichMenu [lindex $gk_menu(itemList) $command]
        set command index
     }
     if { [set x [lsearch $gk_menu(itemList) $command]] >= 0 } {
        set whichMenu $command
        set command index
     }

     # Now execute the appropriate command
     switch -regexp $command {
        add    {
	  eval _gk_defaultMenu_add $w $args 
	  _gk_defaultMenu_repack $w
	}
        delete {
          # Check if it is a numeric or a character string
          if { [string match {[0-9]} [lindex $args 0]] } {
             set whichMenu [lindex $args 0]
          } else {
             set whichMenu [lsearch $gk_menu(itemList) [lindex $args 0]]
          }
       
          set args [lreplace $args 0 0]
          eval _gk_defaultMenu_delete $w $whichMenu $args
	  _gk_defaultMenu_repack $w
          
        }
        index { eval $gk_menu($whichMenu) $args }
        itemco* {
           # Check if it is a numeric or a character string
           if { [string match {[0-9]} [lindex $args 0]] } {
              set whichMenu [lindex $gk_menu(itemList) [lindex $args 0]]
           } else {
              set whichMenu [lindex $args 0]
           }

	   set args [lreplace $args 0 0]

           if { $command == "itemconfigure" } {
	      eval $gk_menu(${whichMenu}_menu) configure [eval list $args]
           } elseif { $command == "itemcommand" } {
	      eval $gk_menu(${whichMenu}_menu) $args
           } else {
              error "invalid command $command.  Must be one of $gk_menu(methods) configure"
           }
        }
        itemlist { return $gk_menu(itemList) }
        default { eval $gk_menu(rootCmd) $command $args }
    }
}

proc _gk_defaultMenu_add {w name index args} {
    global gkDefaultMenu
    upvar #0 $w gk_menu

    set gk_menu($name) [eval menubutton "$gk_menu(bar).$name" \
			-menu "$gk_menu(bar).$name.menu" \
			-underline 0 $args]
    set gk_menu(${name}_menu) [menu "$gk_menu(bar).$name.menu"]
    set gk_menu(itemList) [linsert $gk_menu(itemList) $index $name]

    # Add commands to the command list so that "indexed" commands work
    lappend gkDefaultMenu(methods) $name [expr [llength $gk_menu(itemList)] - 1]
}    

proc _gk_defaultMenu_delete {w index} {
      global gkDefaultMenu
      upvar #0 $w gk_menu

      # Check if it is a numeric or a character string
      if [catch "expr $index + 1"] {
         set whichMenu $index
      } else {
         set whichMenu [lindex $gk_menu(itemList) $index]
      }
      destroy $gk_menu(${whichMenu}_menu)
      destroy $gk_menu($whichMenu)
      set gk_menu(itemList) [lreplace $gk_menu(itemList) $index $index]
  
      # Remove commands from the command list
      set index [lsearch $gkDefaultMenu(methods) $whichMenu]
      set gkDefaultMenu(methods) [lreplace $gkDefaultMenu(methods) $index $index]
      set index [lsearch $gkDefaultMenu(methods) [llength $gk_menu(itemList)]]
      set gkDefaultMenu(methods) [lreplace $gkDefaultMenu(methods) $index $index]

}

proc _gk_defaultMenu_repack {w} {
    upvar #0 $w gk_menu

    # Undo any packing that has already been done
    foreach menu $gk_menu(itemList) {
          pack forget $gk_menu($menu)
    }   

    # Put it all together

#
# NOTE (MR) - the following is a bit of a hack to pack menus correctly;
#             i.e. all on left except help and collaboration on right.
#             the "add" widget subcommand needs to be generalized to take
#             a "-right" operand for menus to be packed from the right
#
    catch {pack $gk_menu(help) -side right -padx 5}
    catch {pack $gk_menu(collaboration) -side right -padx 5}
    foreach menu $gk_menu(itemList) {
#        pack $gk_menu($menu) -side left -expand t -anchor center -fill x
        if {($menu!="help") && ($menu!="collaboration")} {
          pack $gk_menu($menu) -side left -padx 5
        }
    }
}





