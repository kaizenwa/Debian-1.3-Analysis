##############################################################################
# myLabel is an example of how to give default values to a standard
# tcl widget by using the gkClassBuilder.  In this case the default
# value for the class myLabel are:
#        foreground color 	red and 
#        background color  	ligthgray
#
# The option values of a new myLabel widget are initialized to are:
#	anchor		3
#	font		"-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-*-*"
#	relief		raised
#	text		Hello World
#	textvariable	textVarWidget (where widget if the name of the widget)
#	width		30
#
# To create a fancyLabel just do the following:
# 	myLabel .l
#	pack .l
#
# Note that this new widget .l can be used in the same manner as the
# standard tcl widgets are.  For example try ".l config" and see what
# happens.
##############################################################################

proc myLabel {w args} { 
	# This procedure constructs a new widget of the class "myLabel"
        # This is the only place where any of the gkClassBuilder procedures
        # get called.

        eval gkInt_CreateWidget $w myLabel myLabelClass $args
	return $w
}

proc myLabel_CreateClassRec {} {
	global myLabel

        # This procedure defines the default values for class of "myLabel"
        # widgets. Note that it inherits all of the commands/options from
        # the standard tcl widget "label".  It doesn't have any new 
        # options or commands of its own.
	
	set myLabel(inherit) {label}
	set myLabel(rootWindow) label

	set myLabel(-foreground) {-foreground foreground Foreground red}
	set myLabel(-background) {-background background Background lightgray}
}

proc myLabel_InitWidgetRec {w class className} {
	upvar #0 $w data

	# This procedure defines the inital values of the various label
        # options.  These will be the values given to the options when
        # the widget is first created.

	set data(-anchor)	e
	set data(-font)		"-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-*-*"
	set data(-relief)	raised
        set data(-text)		"Hello World"
	set data(-textvariable)	textVar$w
	set data(-width)	30
}
