# Module: GetHardwareInfo.tcl 
# 24.1.96 T.Niederreiter
# 
# External called funktions:
# - getscsidevices{}: fills the array scsidevices()
#   Elements of scsidevices(0):
#     gendevice,blockdevice,type,rmb,vendor,model,rev,date,typename,#
#
# - getpartinfo{}   : fills the array partinfo()
#   Elements of partinfo():
#     partition,size(blocks),id,id-string,mountpoint
#
# - updatemountinfo { }: update internal mount-table 
#
# - getcdinfo { }: gets the TOC and label of a CD 
#

set typearray() ""

# Call the inq program, that outputs information about all connected
# SCSI-devices and format that information in the global array 
# scsidevices(i,j)
# Also calculates the names of the corresponding block-devices 
# (e.g. /dev/sda) from the generic devices.
# returns -1 if no scsi-devices are found.

proc getscsidevices {} {
global scsidevices
global INQ

	set scsihdnum 0
	set scsicdnum 0
	set charlist { a b c d e f g h }
	set scsidevtypes { 
		"Direct-Access    " "Sequential-Access" "Printer          "
		"Processor        " "WORM             " "CD-ROM           "
		"Scanner          " "Optical Device   " "Medium Changer   "
		"Communications   "
	} 

	set inqout [exec $INQ]			;# Call inquire
	set inqlist [split $inqout "\n"] 	;# Convert to list 

	# if the first char of the output is an "x" then no scsi-devices
	# were found.
	if { [lindex $inqout 0] == "x" } {
		return -1
	}

	for { set i 0 } { $i < 8 } { incr i } {
		set loopdev [lindex $inqlist $i]	;# Get a line of inq.
		set devtype [string index $loopdev 0]

		if { $devtype == "0" || $devtype == "7" } {
			set auxdev "/dev/sd[lindex $charlist $scsihdnum]"
			incr scsihdnum
		} \
		elseif { $devtype == "4" || $devtype == "5" } {
			set auxdev "/dev/sr$scsicdnum"
			incr scsicdnum
		} \
		else {
			set auxdev "x"		;# Not a special block-dev
		}

		# generic-device name
		set scsidevices($i,0) "/dev/sg[lindex $charlist $i]" 
		# block-device name or x, if none
		set scsidevices($i,1) $auxdev
		# device-type-number
		set scsidevices($i,2) $devtype
		# device removeable? (0 or 1)
		set scsidevices($i,3) [string index $loopdev 1]   
		# Vendor-string
		set scsidevices($i,4) [string range $loopdev 2 9]
		# Model-string
		set scsidevices($i,5) [string range $loopdev 10 25]
		# Revision-string
		set scsidevices($i,6) [string range $loopdev 26 29]
		# Date-string
		set scsidevices($i,7) [string range $loopdev 30 37]
		# device-type-name
		if { $devtype != "x" } {	
			set scsidevices($i,8) [lindex $scsidevtypes $devtype]
		} \
		else {
			set scsidevices($i,8) ""
		}
		# incr number if two (or more) exact same devices are connected
		set scsidevices($i,9) "1"
		for { set k 0 } { $k < $i } { incr k } {
			if { $scsidevices($k,4) == $scsidevices($i,4) 
			  && $scsidevices($k,5) == $scsidevices($i,5) } {
				incr scsidevices($i,9)
			}	
		}
	}

	# Now convert the temporary number into a nicer format
	# A "1" will become the empty string, any other number will get
	# a hash in front of it.

	for { set i 0 } { $i < 8 } { incr i } {
		if { $scsidevices($i,9) == "1" } {
			set scsidevices($i,9) ""
		} \
		else {
			set scsidevices($i,9) "#$scsidevices($i,9)"
		}
	}
}

# Calls fdisk -T to get a list of all known partitiontypes and their
# symbolic names. This list is converted into an associative array.

proc getpartitiontypenames {} {
global typearray
global FDISK

	set typeout [exec $FDISK -T]
	set typelist [split $typeout "\n"]

	foreach i $typelist {
		set typearray([string range $i 0 1]) [string range $i 4 30]
	}
}

# Takes a blockdevice as argument and returns its partition-id
# Calls fdisk --id <device> <#partition>

proc getpartitionid { devname } {
global FDISK

	set namepart1 [string range $devname 0 7]
	set namepart2 [string range $devname 8 9]
	set fpartid [exec $FDISK --id $namepart1 $namepart2]
	set partid [format "%2s" $fpartid]
	return $partid
}

# Calls "mount" to look if the partition devname is mounted at the moment.
# The output of mount is parsed and if the partition is found, its
# mountpoint is extracted and returned.

proc getmountpoint { devname } {
global MOUNT

	set point ""
	set mountout [exec $MOUNT]
	set mountlist [split $mountout "\n"]

	set devname [string trim $devname]

	foreach i $mountlist {
		set tmpdev [string trim [string range $i 0 9]]
		if { $devname == $tmpdev } {
			# The variable $i looks like this:
			# DEVICE on MOUNTPOINT type ext2 (rw)
			# we extract MOUNTPOINT
			set tmpindex1 [string first "on " $i]
		        incr tmpindex1 3 	;# index of 1st char of mount-path
			set tmpindex2 [string last " type" $i]
			incr tmpindex2 -1	;# index of last char

			set point [string range $i $tmpindex1 $tmpindex2]
		}
	}
	return $point
}

# Get the partitiontable for each scsi-disk (scsi-type 0) and fill
# the global array partinfo with that information
 
proc getpartinfo {} {
global scsidevices
global partinfo
global typearray
global GETPARTSIZE

	getpartitiontypenames

	set partindex 0
	for { set i 0 } { $i < 8 } { incr i } {
		if { $scsidevices($i,2) == "0" } {
			set auxdev $scsidevices($i,1)	;# e.g /dev/sda
			set partout [exec $GETPARTSIZE -a $auxdev]
			set partlist [split $partout "\n"]
	
			foreach j $partlist {
				# partition-device (e.g. /dev/sda1)
				set partinfo($partindex,0) [string trim [string range $j 0 9]]
				# partition-size in blocks	
				set partinfo($partindex,1) [string range $j 11 15]
				# partition-id
				set partinfo($partindex,2) [getpartitionid [string range $j 0 9]]
				# partition-id-string
				set tmpid "- Unknown -"
				catch { set tmpid $typearray($partinfo($partindex,2)) }
				set partinfo($partindex,3) $tmpid

				# mountpoint of partition (if mounted)
				set partinfo($partindex,4) [getmountpoint [string range $j 0 9]]
	
				incr partindex
			}
 		} 
	}
	set partinfo($partindex,0) "x"		;# Mark end of array with "x"
}


# Updates the information in the internal mount-table.
# Syncs with real system mounttable

proc updatemountinfo { } {
global partinfo

	set i 0
	while { $partinfo($i,0) != "x" } {
		set partinfo($i,4) [getmountpoint $partinfo($i,0)]
		incr i
	}
}

# Call the getcdtoc-programm and return a formated list containing the
# information 

proc getcdinfo { } {
global GETCDTOC
global XCDR_SOURCE_CDROM

	set blkname [ convertnametoblkdevice $XCDR_SOURCE_CDROM ]
	set genname [ convertnametogendevice $XCDR_SOURCE_CDROM ]

	resetcd $blkname
	set cdinfo [exec $GETCDTOC $blkname]

	set cdinfolist [split $cdinfo "\n"]

	set cdtoclist {}

	set tmpidx [lsearch -glob $cdinfolist "Type*"]
	lappend cdtoclist [string range [lindex $cdinfolist $tmpidx] 7 end]

	set tmpidx [lsearch -glob $cdinfolist "Label*"]
	lappend cdtoclist [string range [lindex $cdinfolist $tmpidx] 7 end]

	set tmpidx [lsearch -glob $cdinfolist "Time*"]
	lappend cdtoclist [string range [lindex $cdinfolist $tmpidx] 7 end]

	set tmpidx [lsearch -glob $cdinfolist "Tracks*"]
	lappend cdtoclist [string range [lindex $cdinfolist $tmpidx] 7 end]

	# Set a marker to show start of track-lists.     
	lappend cdtoclist "|"

	set tmpidx [lsearch -glob $cdinfolist "#track*"]
	incr tmpidx		; # tmpidx now points to first line of tracks

	# loop all tracks
	while { [set trkline [lindex $cdinfolist $tmpidx]] != "#" } {
		set trklinelist [split $trkline]
		set newtrklinelist {}

		# delete all empty list-elements
		foreach i $trklinelist {
			if { $i != {} } {
				lappend newtrklinelist $i
			}
		} 

		# preemphasis
		if { [lindex $newtrklinelist 1] == "no" } {
			lappend cdtoclist 0
		} else {
			lappend cdtoclist 1
		} 

		# copy-protected
		if { [lindex $newtrklinelist 2] == "no" } {
			lappend cdtoclist 0
		} else {
			lappend cdtoclist 1
		} 

		# track-type
		lappend cdtoclist [lindex $newtrklinelist 3]
		# track-length
		lappend cdtoclist [lindex $newtrklinelist 6]
		# track-frames
		lappend cdtoclist [lindex $newtrklinelist 7]

		incr tmpidx
	}

	return $cdtoclist
}


