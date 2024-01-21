## simple object drawing program
##

## initialize conference
##
gk_initConf $argv

## load up model, view and controller code
##
source [userprefs scriptpath]/model.tcl
source [userprefs scriptpath]/view.tcl
source [userprefs scriptpath]/obj_line.tcl
source [userprefs scriptpath]/obj_rect.tcl
source [userprefs scriptpath]/obj_oval.tcl

## create menubar, canvas
##
pack [gk_defaultMenu .menu] -side top -fill x
pack [canvas .canvas] -expand yes -fill both

## create the model, view and controller
##
gk_objmodel model
gk_objview view .canvas model

## add item types we want
##
view addtype line stdline
view addtype rectangle stdrect
view addtype oval stdoval

## add an objects menu for selecting object types; start with oval
##
.menu add objects 1 -text Objects
.menu itemcommand 1 add radiobutton -label Oval -variable objtype \
	-value oval -command "view newobjtype oval"
.menu itemcommand 1 add radiobutton -label Line -variable objtype \
	-value line -command "view newobjtype line"
.menu itemcommand 1 add radiobutton -label Rectangle -variable objtype \
	-value rectangle -command "view newobjtype rectangle"
set objtype line; view newobjtype line

## add telepointers
##
gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer .canvas

