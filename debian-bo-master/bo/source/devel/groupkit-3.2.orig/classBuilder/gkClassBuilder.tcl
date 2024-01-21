##########################################################################
# The Gk Class Builder
#
# This is the Gk Class Builder which allows the creation of new widget
# classes.  The intention was to make building combination widgets 
# relatively easy to do.
#
# The only procedure that is used to create a widget/class is:
# "gkInt_CreateWidget"  all of the other procedures are internal.
# For more information on how to create new widgets see the REAME
# file as well as the examples.
#
###########################################################################

############# CREATE A WIDGET OF A GIVEN CLASS #############

proc gkInt_CreateWidget {w class className args} {  
    upvar #0 $w data
    upvar #0 $class classRec
    global _cbWidgets

    # This statment is to avoid infinite procedure calls.  This is
    # requiered because _tkWidgets is used to determine the standard
    # tk widgets.  If the list of widgets is hard-coded there is no
    # need for this line.
    if { $w == "." } { error "Not allowed to use dot as the window path" }

    # Initialize the list of class builder widgets.
    if ![info exists _cbWidgets] {
	set _cbWidgets {}
    }

    # Note that this is the only procedure you really need to use when
    # building widgets.  The calls to commands that start with 
    # ${class}_ are external commands and are part of the widgets definition.
    # The other calls starting with _ are internal procedures to the 
    # widget builder.

    # CREATE CLASS RECORD IF NECESSARY

    if {[info globals $class] == ""} {
	# This little flag prevents allow classRec(__widgets) to be
	# defined if it is set to 0, and not redefined if it is set 
	# to 1.
	set classRec(_exists) 0
	_gkInt_CreateClassRec $class
    } 

    # Create the widget's root window 
    _gkInt_CreateWidgetRec      	$w $class $className

    eval ${class}_CreateWidgetRoot  	$w

    # Make the corresponding procedure and add the default
    # option values to the widget record to.  Store the command-line
    # option values in data(tmp)
    _gkInt_MkWidgetCommand          	$w
    eval _gkInt_LoadOptions       	$w $class $className
    _gkInt_WidgetRec                    $w $class $className
    eval _gkInt_LoadRootOptions       	$w $class $className
    _gkInt_EvaluateOptions 		$w $class $className $args \
	                                    {set data(tmp$option) $arg}

    # define the widget record, construct the widget and set
    # the widget's bindings these are all external commands.
    eval ${class}_ConstructWidget      	$w
    eval ${class}_SetWidgetBindings    	$w

    # apply widget defaults to the widget
    _gkInt_EvaluateOptions 		$w $class $className \
	                                    $classRec(__widgetDefaults) \
					    {$w config $option $arg}

    # Now evaluate the comand-line options
    _gkInt_EvaluateOptions 		$w $class $className $args \
						{$w configure $option $arg}

    # make sure the widget is desroyed correctly
    bind $w <Destroy> "+${class}_DestroyWidget $w"

    _cleanuptmp $w	

    set classRec(_exists) 1

    # Change winfo command so that it reports the correct class
    if { [info commands gkWinfo] == "" } {
	rename winfo gkWinfo
	proc winfo {args} {
	    if {[lindex $args 0] == "class" } {
		upvar #0 [lindex $args 1] data
		if [info exists data(className)] { return $data(className)
	        } else { return [eval gkWinfo $args] }
	    } else {
		return [eval gkWinfo $args]
	    }
	}
    }
}

############# CREATE THE CLASS  #############

proc _gkInt_CreateClassRec {class} {
    global _cbWidgets 

    # get the class record as specified
    ${class}_CreateClassRec

    # inherit the commads and options of other classes.
    _gkInt_Inherit                      $class

    # construct the default methods    
    _gkInt_CreateDefMethods              $class

    # Add the class to the global list of class builder classes
    lappend _cbWidgets $class
}


proc _gkInt_Inherit  {class} { 
    upvar #0 $class classRec
    global _cbWidgets

#   Inherit options and commands from other widgets.  Commands and Options
#   are only inherited once.  Where there values are inherited from is based
#   on which classes appear first in the inheritance list.    ie. if the
#   inheritance list is {frame button} then the background color inherited
#   will be that of the frame.  Another note is that the values are not
#   inherited if they are already specified in the definition of the class.
#   ie. if you have classname(-background) { ... } in the class then the
#   background color is not inherited from any of the inheritance classes.
#
#
#   Algorithm  
#   1.	  Make sure all of the necessary lists exists.
#   2. 	  If there is nothing to inherit return.
#   3.    Otherwise get the list of options and commands.  The method
#         for doing this is different depending on whether or not the
#         inherited widget is a standard Tk widget or not.
 
    if {![info exists classRec(options)]} { set classRec(options) {} }
    if {![info exists classRec(rootOptions)]} { set classRec(rootOptions) {} }
    if {![info exists classRec(methods)]} { set classRec(methods) {} }
    if {![info exists classRec(frame)]} { set classRec(frame) true }
    if {(![info exists classRec(inherit)]) || ($classRec(inherit) == "")} {
	return
    }

    if {![info exists classRec(__widgetDefuaults)]} { 
	set classRec(__widgetDefaults) {}
    }

    for {set idx 0} {$idx < [llength $classRec(inherit)]} {incr idx} {
	set inheritClassName [lindex $classRec(inherit) $idx]

	if { [lsearch -exact $_cbWidgets $inheritClassName] == -1 } { 
	    _getNonClassBuilderInfo $class $inheritClassName $idx
	} else { 
	    _getClassBuilderInfo $class $inheritClassName $idx
	}

	if [info exists inheritClass(__widgetDefaults)] {
	    set classRec(__widgetDefaults) [concat $classRec(__widgetDefaults)\
		    $inheritClass(__widgetDefaults)]
	}
    }
}

proc _getClassBuilderInfo {class inheritClassName idx} {
    upvar #0 $class classRec
    upvar #0 [lindex $classRec(inherit) $idx] inheritClass

    # A non standard class is one that is created with this widget builder.

    # create the class record of the inherited class if it doesn't already
    # exist.  Note that the class record is the only information we need,
    # thus, the minimum amount of information is created.
    if {[info globals $inheritClassName] == ""} {
	set classRec(_exists) 0
	_gkInt_CreateClassRec $inheritClassName
    }

    # inherit the root options
    foreach option [join "$inheritClass(options) $inheritClass(rootOptions)"] {
	if { [lsearch -exact $classRec(rootOptions) $option] == -1 } {
	    lappend classRec(rootOptions) $option
	    if { ![info exists classRec($option)] } {
		set classRec($option) $inheritClass($option)
	    }
	}
	if { [lindex $inheritClass($option) 3] != "" } { 
	    set classRec(__widgetDefaults) [concat \
		    $classRec(__widgetDefaults) "$option \
		    [list [lindex $inheritClass($option) 3]]"]
	}
    }

    # inherit the methods (commands)
    if { [info exists inheritClass(methods) ] } { 
	foreach method $inheritClass(methods) { 
	    if { [lsearch -exact $classRec(methods) $method] == -1 } {
		lappend classRec(methods) $method
	    }
	}
    }
}

proc _getNonClassBuilderInfo {class inheritClassName idx} {
    upvar #0 $class classRec
    upvar #0 [lindex $classRec(inherit) $idx] inheritClass

    # find the options and command of a standard Tk widget
    # and add them to the class record.

    eval $inheritClassName .${class}dummy

    # inhert the options
    foreach optionDescr [.${class}dummy configure] {
	set option [lindex $optionDescr 0]
	if { [lsearch -exact $classRec(rootOptions) $option] == -1 } {
	    lappend classRec(rootOptions) $option
	    if { [info exists classRec($option)] == 0 } {
		if { [llength $optionDescr] > 4} { 
		    set classRec($option) [lreplace $optionDescr 4 4]
		} else {
		    set classRec($option) $optionDescr
		}
	    } else { 
		set classRec(__widgetDefaults) [concat \
			$classRec(__widgetDefaults) "$option \
			[list [lindex $classRec($option) 3]]"]
	    }
	}
    }

    # inherit the methods (commands)
    if { [info exists inheritClass(methods) ] } {  
	foreach method $inheritClass(methods) { 
	    if { [lsearch -exact $classRec(methods) $method] == -1 } {
		lappend classRec(methods) $method
	    }
	}
    } elseif [catch {.${class}dummy " "} err_msg] {
	if { [string match "bad option*" $err_msg] } {
	    regsub -nocase -all ", " [string range $err_msg 25 end] " " \
		    methods
	    if { [set len [llength $methods]] > 2 } {
		set methods [lreplace $methods [expr $len-2] [expr $len-2]]
	    }
	    foreach method $methods {
		if { [lsearch -exact $classRec(methods) $method] == -1} {
		    lappend classRec(methods) $method
		}
	    }
	} else { error $err_msg }
    }

    destroy .${class}dummy
}

proc _gkInt_CreateDefMethods {class} {

    # Create the necessary methods for the proper operation of the 
    # class if they aren't already defined.  NOTE: that many of these
    # are just "NULL" procedures, therefore, they may not react as
    # expected.

    # The default procedure for cget - returns the value of an option
    if {[info procs ${class}_Cget] == ""} {
        proc ${class}_Cget {w args} {
	    return eval _getOptValue $w $args
	}
    }

    # Default config procedure applies all of the options to the root
    # widget.
    if {[info procs ${class}_Config] == ""} {
        proc ${class}_Config {w option args} {
	    upvar #0 $w data
	    $data(rootCmd) configure $option [lindex $args 0]
	}
    }

    # The default Create Class Rec is a null procedure
    if {[info procs ${class}_CreateClassRec] == ""} {
        proc ${class}_CreateClassRec {w} {}
    }

    # The default create widget root procedure creates the root widget.
    # The default root widget is a frame.  If the variable 
    # classname(rootWindow) is specified this is the type of the new widget.
    if {[info procs ${class}_CreateWidgetRoot] == ""} {
        proc ${class}_CreateWidgetRoot {w} {
            _gkInt_DefCreateWidgetRoot $w
        }
    }

    # The default construct widget procedure is NULL
    if {[info procs ${class}_ConstructWidget] == ""} {
        proc ${class}_ConstructWidget {w} {}
    }

    # The default destroy widget renames the window pathname to { }
    if {[info procs ${class}_DestroyWidget] == ""} {
        proc ${class}_DestroyWidget {w} {
	    _gkDeleteWidget $w
        }
    }

    # Default init widget rec procedure is NULL
    if {[info procs ${class}_InitWidgetRec] == ""} {
        proc ${class}_InitWidgetRec {w class className args} {}
    }

    # Defualt methods procedure causes an error which will cause
    # the method be applied to the root window
    if {[info procs ${class}_Methods] == ""} {
        proc ${class}_Methods {w command args} { 
	    error
	}
    }

    # Default set widget bindings is a NULL procedure.
    if {[info procs ${class}_SetWidgetBindings] == ""} {
        proc ${class}_SetWidgetBindings {w} {}
    }
}

#############  Create the window ###################

proc _gkInt_DefCreateWidgetRoot {w} {
    upvar #0 $w data
    upvar #0 $data(class) classRec

    # The widget is always defined to be a frame of the supplied class.
    if { ( ![info exists classRec(rootWindow)] ) ||  \
	    ( $classRec(rootWindow) == "" ) } { 
	set classRec(rootWindow) frame
        frame $w -class $data(class)
    } else {
	# Make the root window the first standard Tk widget which is
	# encountered.  
	set rootWindow [_gkFindRootWindow $classRec(rootWindow)]
	if { $rootWindow == "frame" }  {
	    $rootWindow $w -class $data(class)
	} else {
	    $rootWindow $w
	}
    }
}

proc _gkFindRootWindow {class} { 
    upvar #0 $class classRec
    global _cbWidgets


    if  { [lsearch -exact $_cbWidgets $class] == -1 } { 
	# If the class is not a class builder widget then return it. 
	return $class
    } elseif { ![info exists classRec(rootWindow)] } { 
	# if the rootWindow is not defined for the class then
	# use the default widget, frame.
	return frame 
    } else { 
	# otherwise try and find a non classbuilder widget for the
	# root window.
	return [_gkFindRootWindow $classRec(rootWindow)]
    }
}

#############  INTIALIZE THE WIDGET  ###############

proc _gkInt_CreateWidgetRec {w class className} {
    upvar #0 $w data

    # Set the class className and rootWidget for the given widget,
    # so that they can be used by other procedures.

    set data(class)     $class
    set data(className) $className
    set data(root)      $w
}

proc _gkInt_MkWidgetCommand {w} {
    upvar #0 $w data
    upvar #0 $data(class) classRec

    set data(rootCmd)  $w:root
    _gkRenameWidgetCmd $w $data(rootCmd)
     
     proc $w {method args} { 
	#------------------------------------------------------------
	# the name of the data structure is the same as the procedure
	#------------------------------------------------------------
	set w [lindex [info level 0] 0]
	upvar #0 $w data
	upvar #0 $data(class) classRec

	set class $data(class)
	switch -glob $method  {
	    #------------------------------------------------------------
	    # Config is a special method that needs special attention
	    #------------------------------------------------------------
	    config* { 
		if {$args == {}} {
		    return [_gkInt_QueryOptions $w]
		}
                _gkInt_EvaluateOptions $w $data(class) $data(className) \
			$args {_gk_ApplyOptions $w $class $option $arg} 
	    }
	    #------------------------------------------------------------
	    # Cget is a special method that needs special attention
	    #------------------------------------------------------------
	    cget { 
		if { $args == "" } {
		    error "wrong # args: should be \"$w cget option\""
                } else {
		    if [catch {set value [${class}_Cget $w $args]} err_msg1] {
			if [catch {set value [eval _getOptValue $w $args]}] {
			    error $err_msg1
			}
		    } 
		    return $value
		}
	    }   
	    default { 
                # Algorithm
	        # 1.  If the method (command) is notin the list of methods
                #     then an error has occured, give appropriate error message
		#     and list all of the valid methods.
                # 2.  Otherwise Execute the method and return it's value

                if [catch {set value [${class}_Methods $w $method $args]} \
			err_msg1] {
		    if { [lsearch $classRec(methods) $method] == -1} {
                       error "unknown command $method. Should be one of: \
				$classRec(methods)"
		    } elseif [catch {set value [eval $data(rootCmd) $method \
			    $args]} err_msg2] {
			if { [lsearch $classRec(methods) $method] == -1} {
			    error $err_msg2
			} else {
			    error "$err_msg1"
			}
		    }
		}
	        if { [info exists value] && ($value != "") } { return $value }
	    }
	}
    }
}

proc _gkInt_WidgetRec {w class className} {
    upvar #0 $w data
    upvar #0 $data(class) classRec

    # Not only should the class information should be inherited but the
    # widget record should be as well, for the given class.  The
    # rootWindow information is applied first and then the current
    # widget record is applied. Note that the widget records are 
    # applied in reverse order, see _inherit_WidgetRec for more details.

    _inherit_WidgetRec $w $classRec(inherit) $class $className
    eval ${class}_InitWidgetRec        	$w $class $className
}


proc _inherit_WidgetRec {w inheritClass class className} {
    upvar #0 $inheritClass classRec
    global _cbWidgets

    # Apply the widget records of inherited classes, make sure
    # that the first one in the list is applied last.

    for {set idx [expr [llength $inheritClass] - 1]} {$idx >=0} \
	    {incr idx -1} {
	
	set newClass [lindex $inheritClass $idx]
	if { [lsearch $_cbWidgets $newClass] != -1 } {
	    upvar #0 $newClass newClassRec

	    if {[info globals $newClass] == ""} {
		set newClassRec(_exists) 0
		_gkInt_CreateClassRec $newClass
	    } 

	    if [info exists newClassRec(inherit)] {
		# First find out if there are anymore classes to be evaluated
		# due to inheritance and apply them.  Then apply the current 
		# widget record.

		_inherit_WidgetRec $w $newClassRec(inherit) $class $className
		eval ${newClass}_InitWidgetRec  $w $class $className
            }
	}
    } 
}

proc _gkInt_LoadOptions {w class className} {
    upvar #0 $w data
    upvar #0 $class classRec

    # set-up the widget according to the default values
    # it ensures that there is a data(option) defined
    # for each of the allowable options for the class.

    foreach option $classRec(options) {
	if {[lindex $classRec($option) 2] == ""} {
	    continue
	}

	set o_name    [lindex $classRec($option) 1]
	set o_class   [lindex $classRec($option) 2]
	set o_default [lindex $classRec($option) 3]

	set userDef [option get $w $o_name $o_class]
        if { $userDef != "" } { 
	   set data($option) $userDef
  	} else {
	   set data($option) $o_default
        }
    }
}

proc _gkInt_LoadRootOptions {w class className} {
    upvar #0 $w data
    upvar #0 $class classRec

    # This procedure loads the root options and adds them to the
    # list of default options to be applied later.

    foreach option $classRec(rootOptions) { 
        if  { [info exists classRec($option)] } { 
            if { [info exists data($option)] == 0 } { 
		set data($option) [lindex $classRec($option) 3]
            } else { 
		if { $classRec(_exists) == 0 } {
		    set classRec(__widgetDefaults) \
			    [concat $classRec(__widgetDefaults) \
			    "$option [list $data($option)]"]
		}
	    }

	} else { destroy $w ; error "Widget record is incomplete" }
    }
}

proc _gkInt_EvaluateOptions {w class className args command} {
    upvar #0 $w data
    upvar #0 $class classRec

    # Evaluate the arguments given to the widget
    # This routine parses the argument list into pairs of options and
    # values.  It is intended that these pairs of values are passed to
    # the "command" one at a time.  
    # This procedure is used by _gk_CreateWidge as well as _gk_ApplyOptions
    # and is useful since it allows for code re-use.
    #
    #  	Algorithm
    #	1.	If there are no arguments then return and do nothing
    #	2.	If a single option (length of "args" is 1)  then a query 
    #		this option ane return it's value.
    #   3.	If there is more than one option (length is > 1) then pair
    #		up the options and values and apply the pairs to the command.
    #   4.   	If the length of "args" is odd then the last option is 
    #		missing a value so an error has occured.

    set len  [llength $args]
    set len2 [expr {$len - 2}]
    set i    0

    if { $args == {} } { return }

    if {$len == 1} {
	return [eval _gkInt_QueryOption $w $args]
    }

    while { $i <= $len2 } {
	set option [lindex $args $i] ; incr i
	set arg [lindex $args $i] ; incr i

        if {[lindex $classRec($option) 2] == ""} {
	   set option -[string tolower [lindex $classRec($option) 1]]
        }
	eval $command
     }

    if {$i != $len} {
	error "value for \"$option\" missing"
    }
}

#########  OPERATIONS FOR AN EXISTING WIDGET #################

proc _gkInt_ListOptions {w} {
    upvar #0 $w data
    upvar #0 $data(class) classRec

    return [lsort "$classRec(rootOptions) $classRec(options)"]
}


proc _gkInt_QueryOptions {w} {
    upvar #0 $w data
    upvar #0 $data(class) classRec

    # returns the list of options and there values

    set options {}

    foreach option [join "$classRec(options) $classRec(rootOptions)" ] {
        if { [lsearch -glob $options $option*] == -1 } {
            lappend options "$classRec($option) $data($option)"
        }
    }
    return [lsort $options]
}


proc _gkInt_QueryOption {w option} {
    upvar #0 $w data
    upvar #0 $data(class) classRec

    # return the value of a particular option

    if {[lindex $classRec($option) 2] == ""} {
	set option -[string tolower [lindex $classRec($option) 1]]
    }
    if [catch {set dataOption "$classRec($option) $data($option)"} err_msg] {
        if {[string range $err_msg 0 13] == "unknown option"} {
           error "unknown option $option. Should be: [_gkInt_ListOptions $w]"
         } else {
            error $err_msg
         }
    } else { return $dataOption }
}

proc _gk_ApplyOptions {w class option arg} {
    upvar #0 $w data
    upvar #0 $class classRec

    # Attempt to do "$w config $option $arg"  where
    #	w 	is a window
    #	option	is an option, and
    #	art	is a value for an option
    #
    # Algorithm
    #	1. 	Attempt to execute the class_Config method
    #	2. 	If this doesn't succeed then check to see if the given option
    #	   	is a valid option.
    #	2.1 	If it isn't a valid option then give an appropriate error
    #		message with the list of valid options.
    #	2.2	If it is a valid option then try and apply it to the root
    #           widget.
    #	3.	If everything went okay then update the widgets record.

    if [catch {${class}_Config $w $option $arg} err_msg1] { 
        set optionList [_gkInt_ListOptions $w]
        if { [lsearch $optionList $option] == -1} { 
            error "unknown option $option. Should be: $optionList"

	} elseif [catch {$data(rootCmd) configure $option $arg} err_msg2] {
	    set found 0
            eval $classRec(rootWindow) ._${class}dummy
	    foreach validOption [._${class}dummy configure] {
	        if { [lindex $validOption 1] == $option }  {
		    set found 1 
		}
	    }

	    destroy ._${class}dummy

            if  $found {
		error $err_msg2
            } else {
		error $err_msg1
	    }
	}

    }
    set data($option) $arg
}


proc _gkRenameWidgetCmd {w new_w} {
    if {[info commands $new_w] != {}} {
	rename $new_w {}
    }
    rename $w $new_w
}

proc _gkDeleteWidget {w} {
    if {[info commands $w] != {}} {
	rename $w {}
    }
    if {[info commands $w:root] != {}} {
	rename $w:root {}
    }
}

proc _getOptValue {w Option} {
    upvar #0 $w data
    upvar #0 $data(class) classRec
    global _cbWidgets

#   Algorithm
#   1.     See if option value is stored in the data record
#   1.1    If it isn't then try applying cget directly to the widget if it
#          it is a standard Tk widget.
#   1.2    If it is in the data record see if it is an abbreviated option
#   1.2.1  If it is abbreviated then find the real option and evaluate it.
#   1.2.2  If it isn't abbreviated return the option's value

   if { [info exists data($Option)] == 0 } { 
      if { [lsearch -exact _cbWidgets $data(class)] != -1 } {
	   puts "unknown option \"$Option\""
      } else {
	   return [eval $w cget $args]
      }
#   } elseif { $data($Option) == "" } {
#       set Option -[string tolower [lindex $classRec($Option) 1]]
#       return [eval _getOptValue $w $Option]
   } else {
       return $data($Option)
   }
}

proc _cleanuptmp {w} {
    upvar #0 $w data
    upvar #0 $data(class) classRec

    # Remove all of the temporary options used to make the list of options,
    # given at the time the widget was created, available to the widget
    # builder.
    foreach name [array names data] {
	if [regexp tmp-* $name] { unset data($name) }
    }
}
