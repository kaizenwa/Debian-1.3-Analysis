# Module: Init.tcl
# 24.1.96 T.Niederreiter
# 
# Init-routines
#

if { [getscsidevices] == -1 } {
	puts "-----------------------------------------------------------------"
   	puts "Error: No SCSI-Devices have been found."
	puts ""
	puts " - Perhaps you don´t have a supported SCSI-Controller or"
	puts " - you have not enabled \"Generic SCSI-Support\" in the kernel or"
	puts "   (check with \"cat /proc/devices\", whether there is a "
	puts "   line \"21 sg\" -> then you have kernel support.)  "
	puts " - your SCSI-Devices are not powered or connected or"
	puts " - the /dev/sga - /dev/sgh device-files are missing." 
	puts "-----------------------------------------------------------------"
	exit	
}
getpartinfo

set VERSION $XCDR_VERSION

# Check Tix-Version and enable compatiblity-mode if needed
if { $tix_patchLevel > "4.0.3" && [string index $tix_patchLevel 3] == "." } {
	set OPTMENUFIX 1
} else {
	set OPTMENUFIX 0
}
# Versions greater than 4.0 automatically need this fix
if { [string range $tix_patchLevel 0 2] > "4.0" } {
	set OPTMENUFIX 1
}

# puts "Found Tix-Version $tix_patchLevel, Fix = $OPTMENUFIX"

# Load configuration-file
if { [loadcfgfile] != 0 } then {
	Msg_ConfFileNotFound .dlg .main
	set forcesetup 1		
}

# Check if loaded configuration-file was from a different 
# program-version.
if { $VERSION != $XCDR_VERSION } {
	Msg_ConfFileWrongVersion .dlg .main $VERSION $XCDR_VERSION
	set XCDR_VERSION $VERSION
	set forcesetup 1
}

# Check if scsisetup has changed
set scsistatus [checkscsisetup]
if { $forcesetup !=1 && $scsistatus != 0 } then {
	Msg_ScsiSetupWarning .dlg .main $scsistatus
	set forcesetup 1
}

# Set nice defaults for the write-menus
set DUMMY 0
set EJECT 1
set PADDATA 0

