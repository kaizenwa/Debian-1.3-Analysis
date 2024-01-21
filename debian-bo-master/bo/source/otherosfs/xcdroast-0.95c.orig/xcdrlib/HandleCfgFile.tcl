# Module: HandleCfgFile.tcl
# 26.1.96 T.Niederreiter
#
# External called funktions:
# - loadcfgfile{}: loads all global variables from config-file
#
# - savecfgfile{}: saves all global variables in config-file
#

set CFGFILE "../xcdroast.conf"

# Opens the configuration file $CFGFILE and reads it line by line.
# When we find a line starting with "XCDR" we treat this line as a line
# containing a variablename and a variablevalue seperated by a "=".
# All lines not starting with "XCDR" are ignored.
# We extract the variablename and the value (the value can be set in 
# double-quotes (")). Then we set this variable to that value in the
# global context. (So you can access it from the whole program.) 

proc loadcfgfile {} {
global CFGFILE
global str1 str2

	set opnflg [catch { set fileid [open $CFGFILE r] }]

	if { $opnflg != 0 } {
	#	puts "Error opening $CFGFILE!"
		return -1
	}

	set str1 ""
	set str2 ""

	while {	[gets $fileid cfgline] >= 0 } {
		if { [string range $cfgline 0 3] == "XCDR" } {
			set mrk1 [string first "=" $cfgline]
			incr mrk1 -1
			# Set str1 to variablename 
			set str1 [string trim [string range $cfgline 0 $mrk1]]
			incr mrk1 2
			# Set str2 to variablevalue
			set str2 [string trim [string range $cfgline $mrk1 end]]
		 	set str2 [string trim $str2 \"]	

			# set the variables read from file in global context
			uplevel #0 { eval set $str1 \"$str2\" }
		}
	}
	unset str1 str2
	close $fileid
	return 0
}	

# Creates (or overwrites) the configuration file.
# We write each global variable starting with XCDR together with its
# value in a file. Each line is written in a syntax readable by 
# "loadcfgfile". The variables are alphabetically sorted.
 
proc savecfgfile {} {
eval global [info globals XCDR*]
global CFGFILE

	set varlist [lsort [info globals XCDR*]]

	set fileid [open $CFGFILE w]
	puts $fileid "# X-CD-Roast V$XCDR_VERSION Configuration-File"
	puts $fileid "# Automatically created by the X-CD-Roast-Setup"
	puts $fileid "# Don't edit! (Unless you really know what you are doing)"
	puts $fileid ""

	foreach varname $varlist {
		eval set varvalue \$$varname
		puts $fileid "$varname = \"$varvalue\""
	}

	close $fileid
}
