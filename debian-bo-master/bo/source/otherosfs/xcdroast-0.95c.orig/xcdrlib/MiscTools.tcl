# Module: MiscTools.tcl
# 11.2.96 T.Niederreiter
#
# External called funktions:
# - createglobalvars {}: Initialize all global variables
# 
# - spawnsubwindow { call new old }: creates a new toplevel window
#
# - backupglobals{}: backup all XCDR* variables in BAK_XCDR* variables 
#
# - restoreglobals{}: restores the XCDR* values
#
# - convertarray2string { name }: Convert array to string 
#
# - checkscsisetup {}: Check if SCSI-Setup changed since last time 
#
# - isiso{ dev }: Test if ISO9660 
#
# - domount { mntdev mntdir }: Mount a device 
#
# - domountext2 { mntdev mntdir }: Mount a ext2-device 
#
# - doumount { mntdev }: Umount a device
# 
# - checkcd {}: Check if cd is in drive
#
# - getisolabel { dev }: Get ISO9660-Volume-Label 
#
# - getisosize { dev }: Get size of an ISO9660-Image
# 
# - getaudiosize { file }: Get length of an audio-file
#
# - getmountpnt { dev }: Get the mountpoint of a device
#
# - checkmountpnt { dir }: check if this dir is busy 
# 
# - getfreeimgspace { }: Get the free space on the Image-Partition 
#
# - doformat { dev}: Format a partition
#
# - loadfile2list { fname }: Load a text-file to list 
#
# - extractcolonstr { str }: Extract substr after colon 
#
# - getdircontents { dir }: Get directory-contents 
#
# - resetcd { dev }: Reset CD-Rom
#
# - calcdu { dir }: Calculates Disk-Usage
#
# - getimagecontents { file }: Get root-dir of an iso-image
#
# - checkifready { w }: Prompt user for CD
# 
# - checkifyamaha { }: Check if writer-type is yamaha 
#
# - checkifsony { }: Check if writer-type is sony 
#
# - setEndianorder {}: Set endian-order 
#
# - checkifiso { dev }: Check if valid ISO 
#
# - checkifaudio { }: Check if audio-cd 
#
# - checkifmixedmode { }: Check if mixed-mode-cd 
#
# - getnonisosize { }: Get size of image on non-iso-cd
# 
# - createbeep { num }: Create a beeping noise from the speaker
#
# - sound { type }: Sound-Handler 
#
# - log { w str }: Logfile-Handler 
#

# Defines all global variables needed
# This is done by setting all variables in the "varlist" to "" in global
# context.

proc createglobalvars {} {

	set varlist "scsidevices(0,0) partinfo(0,0) XCDR_scsidevices \
		XCDR_partinfo XCDR_AUDIOREAD_SPEED XCDR_BEEP \
		XCDR_DEST_WRITER_DEV XCDR_DEST_WRITER_MODE \
		XCDR_DEST_WRITER_SPEED \
		XCDR_DSP_DEV XCDR_IMAGE_MNTPNT XCDR_IMAGE_PART \
		XCDR_LOCKPART XCDR_LOGFILE XCDR_LOGNAME XCDR_SOURCE_CDROM \
		forcesetup ENDIANSWAP NONISO OPTMENUFIX \
		DUMMY EJECT PADDATA percent"
		

	global i
	foreach i $varlist {
		uplevel #0 { eval set $i \"\" }
	} 
	unset i
}

# Creates a new toplevel window, sets the grab on it, calls the
# function that fills the new new window, waits until it terminates
# and returns. Claims also the focus.
# Also sets a "watch"-cursor in the window of the caller
#
# Parameters: call = window-procedure to call. The procedure must
#		     have one argument, that is the window-path.
#	      new  = path of new window
#             old  = path of old window (the window of the caller)

proc spawnsubwindow { call new old } {
global CDRICO

	$old configure -cursor watch
	catch { destroy $new }
	toplevel $new
	wm title $new "X-CD-Roast"
	wm iconbitmap $new @$CDRICO
	grab $new
	set oldFocus [focus]
	focus $new
	$call $new
	# Here we need a catch, in the case a window is spawned and 
	# destroyed before we can wait for user input. Without
	# catch this results in an error.
	catch { tkwait window $new }
	catch { focus $oldFocus }
	$old configure -cursor ""
}

# Copies all XCDR* global variables in global BAK_XCDR* variables.
# Used by the Setup-Menu to save all variables before the user changes
# them. (and wants to reactive the old settings afterwards)
 
proc backupglobals {} {
eval global [info globals XCDR*]
global varname varvalue

	set varlist [lsort [info globals XCDR*]]

	foreach varname $varlist {
		eval set varvalue \$$varname
		uplevel #0 { eval set BAK_$varname \"$varvalue\" }
	}
	unset varname varvalue
}

# Restores the values from the BAK_XCDR* variables back to the XCDR* 
# variables. 

proc restoreglobals {} {
eval global [info globals BAK_XCDR*]
global newvarname varvalue

	set varlist [lsort [info globals BAK_XCDR*]]

	foreach varname $varlist {
		eval set varvalue \$$varname
		set newvarname [string range $varname 4 end] 
		catch { uplevel #0 { eval set $newvarname \"$varvalue\" } }
		# Explaination for the catch here:
		# When a variable holds an illegal value (e.g. not in option-
		# list of an tixOptionMenu) then we simply ignore the error,
		# which results automatically in a valid value.
	}
	unset newvarname varvalue
}

# Convert any array to a string. This makes it easier to save arrays in
# textfiles or to compare arrays. But it is not allowed to have the 
# pipe-char "|" inside any array-element, because it is used for internal
# seperation.

proc convertarray2string { name } {

	# Call-by-Reference: Get the array via the upvar-command
	upvar $name arr

	set elelist [lsort [array names arr]]
	set outstr ""

	foreach i $elelist {
		append outstr "|$i|$arr($i)"		
	}

	return $outstr
}

# Check if the SCSI-Bus and Partitions have been changed since last
# time in the setup-menu.
# Return-values: 0 = No Changes, setup-values are still valid
#                1 = SCSI-Bus changed only, better check setup-menu
#		 2 = Partitions changed only, could be quite dangerous
#                    when your image-partition is now wrong. setup new!
#		 3 = Both SCSI and Partition changed
 
proc checkscsisetup { } {
global XCDR_scsidevices
global XCDR_partinfo
global scsidevices
global partinfo

	# Convert scsidevices and partinfo-arrays to strings
	set NEW_scsidevices [convertarray2string scsidevices]
 	set NEW_partinfo [convertarray2string partinfo]

	set status 0

	# We have now the NEW_* variables, which contain the hardware
	# info scanned now and the XCDR_variables which contain the
	# hardware info saved last time in the setup-menu.

	if { $NEW_scsidevices != $XCDR_scsidevices } {
		incr status
	}
	if { $NEW_partinfo != $XCDR_partinfo } {
		incr status 2
	}
	
	# Now copy the new values into the XCDR-variables, so that they
	# will saved when the user clicks on "save" in the setup-menu.
	set XCDR_scsidevices $NEW_scsidevices
	set XCDR_partinfo $NEW_partinfo

	return $status
}


# Checks if the device "dev" contains an ISO9660-FS.
# Return 1 if ISO, 0 if not.

proc isiso { dev } {
global ISODETECT
	
	resetcd $dev
	set isoout [exec $ISODETECT -d $dev]
	if { $isoout == "ISO9660" } {
		return 1
	} else {
		return 0
	}
}

# Mounts a device to a directory.
# Updates also the partinfo-array.
# Return 1 for success, 0 for failure.

proc domount { mntdev mntdir } {
global MOUNT
global partinfo

	set mntout [catch { exec $MOUNT $mntdev $mntdir }]

	set i 0
	# Mount was ok?
	if { $mntout == 0 } {
		# Update partinfo-array
		while { $partinfo($i,0) != "x" } {
			if { $partinfo($i,0) == $mntdev } {
				set partinfo($i,4) $mntdir
			}
			incr i
		}
		return 1
	} else {
		# Mount failed
		return 0
	}
}
 
# Mounts a device to a directory. It must be a ext2-fs on it.
# Updates also the partinfo-array.
# Return 1 for success, 0 for failure.

proc domountext2 { mntdev mntdir } {
global MOUNT
global partinfo

	set mntout [catch { exec $MOUNT -t ext2 $mntdev $mntdir }]

	set i 0
	# Mount was ok?
	if { $mntout == 0 } {
		# Update partinfo-array
		while { $partinfo($i,0) != "x" } {
			if { $partinfo($i,0) == $mntdev } {
				set partinfo($i,4) $mntdir
			}
			incr i
		}
		return 1
	} else {
		# Mount failed
		return 0
	}
}
 

# Umounts a device .
# Updates also the partinfo-array.
# Return 1 for success, 0 for failure.

proc doumount { mntdev } {
global UMOUNT
global partinfo

	set mntout [catch { exec $UMOUNT $mntdev }]

	set i 0
	# umount was ok?
	if { $mntout == 0 } {
		# Update partinfo-array
		while { $partinfo($i,0) != "x" } {
			if { $partinfo($i,0) == $mntdev } {
				set partinfo($i,4) "" 
			}
			incr i
		}
		return 1
	} else {
		# umount failed
		return 0
	}
}
 
# Checks if the CD is loaded
# Return 1 if loaded, 0 if not

proc checkcd {} {
global CHECKCD
global XCDR_SOURCE_CDROM

 	set blkname [ convertnametoblkdevice $XCDR_SOURCE_CDROM ]
	set chkout [catch { exec $CHECKCD $blkname }]

	if { $chkout == 0 } {
		# Medium is loaded and ready
		return 1
	} else {
		# Medium not loaded
		return 0
	}
}


# Read the ISO9660-Volume-Label from device

proc getisolabel { dev } {
global ISODETECT

	return [ exec $ISODETECT -V -d $dev ]
}


# Get the Size of an ISO9660-Image
# Return 0 if it is not iso9660.

proc getisosize { dev } {
global ISODETECT
global ISOSIZE

	resetcd $dev
	set isoout [exec $ISODETECT -d $dev]
	if { $isoout == "ISO9660" } {
		return [ exec $ISOSIZE $dev ]
	}
	return 0
}


# Gets the size (in min:sec.centi_sec-format) of a file

proc getaudiosize { filename } {

	set filesize [ file size $filename ]
	set frames [expr $filesize/2352]

	return [convertframes2time $frames]
}


# Get the mountpoint of the device $dev. If it is not mounted return
# the empty string.

proc getmountpnt { dev } {
global partinfo

	set i 0
	set mntpnt ""

       	# Check if dev is mounted
      	while { $partinfo($i,0) != "x" } {
        	if { $partinfo($i,0) == $dev } {
                	set mntpnt $partinfo($i,4)
             	}
            	incr i
	}

	return $mntpnt
}


# Checks if this directory is already an active mountpoint
# returns the device that is mounted there or the empty string

proc checkmountpnt { dir } {
global partinfo

	set i 0
	set dev ""

       	# Check if dev is mounted
      	while { $partinfo($i,0) != "x" } {
        	if { $partinfo($i,4) == $dir } {
			set dev $partinfo($i,0)
             	}
            	incr i
	}

	return $dev;
}


# Gets the free space on the Image-Partition. If the partition is mounted
# we parse the output of the df-command, and if it is not mounted we 
# return the size of the partition (from partition-table)

proc getfreeimgspace { } {
global DF
global XCDR_IMAGE_PART
global partinfo

	set i 0
	set freespace 0

	set mntpnt [ getmountpnt $XCDR_IMAGE_PART ] 

	# Partition is mounted
	if { $mntpnt != "" } {
		# Get output of df-command
		set out [ exec $DF -k $XCDR_IMAGE_PART ]
		# Split the output to lines
		set out2 [ split $out "\n" ]
		# Get the second line of that output and split it in words
		set out3 [ split [lindex $out2 1] ] 
		set out4 {}
		
		# Delete all empty list-elements
		foreach i $out3 {
			if { $i != {} } {
				lappend out4 $i
			}	
		}

		set freespace [ expr [lindex $out4 3]/1024 ]

	} else {
	# Partition is not mounted

		set i 0
		# Search in Partition-table 
		while { $partinfo($i,0) != "x" } {
			if { $partinfo($i,0) == $XCDR_IMAGE_PART } {
				set freespace $partinfo($i,1)
			}
			incr i
		}
	}

	return $freespace
}

# Create an ext2-fs on device $dev
# Return 1 if ok, 0 on failure

proc doformat { dev } {
global MKE2FS

	set mkout [ catch { exec $MKE2FS -q $dev 2>/dev/null } ]

	if { $mkout == 0 } {
		return 1
	} else {
		return 0
	}
}


# Loads a text-file line-by-line into a list. 
# Returns empty list if file does not exist.

proc loadfile2list { fname } {

 	set opnflg [catch { set fileid [open $fname r] }]

        if { $opnflg != 0 } {
		# Error opening file, return empty list
                return {} 
        }

	set outlist {}

	while { [gets $fileid line] >= 0 } {
		lappend outlist "$line"
	}

	return $outlist
}


# Extract from a str all non-white-space-chars after the first colon
# and returns it

proc extractcolonstr { str } {

	# Get substr after the colon
	set str2 "[string range $str [expr [string first : $str]+1] end]"

	return "[string trim $str2]"
}


# get the contents of a directory and returns them as list, return -1 if
# there is no such directory.

proc getdircontents { dir } {
global LS

	set stat [catch {set out [exec $LS -aF $dir]}]

	# No such directory?
	if { $stat != 0 } {
		return -1
	}
	# Split the output to lines
	set out2 [ split $out "\n" ]

	return $out2
}


# Reset CD-Rom

proc resetcd { dev } {
global RESETCD

	# If we are getting a cd-rom-device,
	# reset it first. (This is to get around a kernel-bug)

	set tmp [string range $dev 0 6]
	if { $tmp == "/dev/sr" } {
		catch { exec $RESETCD $dev }
	}
}


# Calculate disk-usage in MB, return -1 if directory was invalid

proc calcdu { dir } {
global DU
global DU2

	set stat [catch { set out [exec $DU -ks $dir]}]

	# If that doesn't worked, try again with other du 
	if { $stat != 0 } {
		set stat [catch { set out [exec $DU2 -ks $dir]}]
	}

	if { $stat == 0 } {
		scan $out "%d %s" kb tmp
		set mb [expr ($kb/1024)]
		return $mb 
	}	
	return -1
}


# get the contents of the root of an image and returns them as list, 
# return -1 if there is no such file.

proc getimagecontents { file } {
global ISOINFO

	if { [file exists $file] == 0 } {
		return -1
	}

	set cmd "$ISOINFO -l -R -i $file"
	set pipe [open "|$cmd" r]

	set count 0
	set out { }
	while { [gets $pipe str] >= 0 } {

		# Get only the root-dir, skip all other directories
        	if { $count == 2 } {
        	        catch { close $pipe }
        	        break
        	}

		# Found we a new Directory-entry?
        	if { [string index $str 0 ] == "D" } {
        	        incr count
			continue
        	}

		set type [string index $str 0] 
		if { $type == "-" } {
			set type [string index $str 3]
		}
		if { $type == "-" } {
			set type " " 
		}

		set name [string range $str 64 end]
		set first [string first " " $name]
		set name [string range $name 0 [expr $first-1]]

		switch $type {
			"d" { append name "/" }
			"l" { append name "@" }
			"x" { append name "*" } 
			"p" { append name "|" }
			"s" { append name "=" }
			" " { }
		}

		lappend out $name	
	}

	return $out
}


# Check if CD is in drive, if not ask user to insert CD.
# return 0 when cd is ready now, 1 when user aborted

proc checkifready { w } {

	# Endless loop until cd ready or aborted
	while { 1 } {
	
		# CD loaded?
		if { [checkcd] == 0 } {
			set stat [Msg_NoCD .nocd $w]	
			if { $stat == 1 } { #; Abort
				return 1 
			}
		} else {
			# CD ready now
			return 0
		}
	}
}	


# Checks if the connected writer is from Yamaha, or if the writer-mode
# is set to yamaha.
# This is needed because the yamaha needs another audio-byte-order.
 
proc checkifyamaha { } {
global XCDR_DEST_WRITER_DEV 
global XCDR_DEST_WRITER_MODE

	set dev $XCDR_DEST_WRITER_DEV
	set mode $XCDR_DEST_WRITER_MODE

	set found 0
	if { [string first "yamaha" $dev] != -1 || 
     	     [string first "Yamaha" $dev] != -1 ||
	     [string first "YAMAHA" $dev] != -1 } {
		
		set found 1
	}

	if { [string first "yamaha" $mode] != -1 || 
     	     [string first "Yamaha" $mode] != -1 ||
	     [string first "YAMAHA" $mode] != -1 } {
		
		set found 1
	}

	return $found
}


# Checks if the connected writer is from Sony, or if the writer-mode
# is set to sony.
# This is needed because the sony needs another audio-byte-order.
 
proc checkifsony { } {
global XCDR_DEST_WRITER_DEV 
global XCDR_DEST_WRITER_MODE

	set dev $XCDR_DEST_WRITER_DEV
	set mode $XCDR_DEST_WRITER_MODE

	set found 0
	if { [string first "sony" $dev] != -1 || 
     	     [string first "Sony" $dev] != -1 ||
	     [string first "SONY" $dev] != -1 } {
		
		set found 1
	}

	if { [string first "sony" $mode] != -1 || 
     	     [string first "Sony" $mode] != -1 ||
	     [string first "SONY" $mode] != -1 } {
		
		set found 1
	}

	return $found
}


# Set the ENDIANSWAP-variable to value needed for this hardware

proc setEndianorder {} {
global ENDIANSWAP

	if { [checkifyamaha] || [checkifsony] } {
	        set ENDIANSWAP 1
	} else {
	        set ENDIANSWAP 0
	}
}


# Check if dev contains an ISO9660-FS. Check also if isosize
# reports a sane value...
# Returns 1 if iso, 0 if not.

proc checkifiso { dev } {
global ISODETECT
global ISOSIZE

	resetcd $dev
	set isoout [exec $ISODETECT -d $dev]
	if { $isoout == "ISO9660" } {
		set size [exec $ISOSIZE $dev]
		if { $size > 0 && $size < 734003200 } {
			return 1
		}
	
	}
	return 0
}	


# Check if the CD is a pure audio-cd

proc checkifaudio { } {

	set cdtoclist [getcdinfo]
	set type [lindex $cdtoclist 0]
	if { $type == "Audio" } {
		return 1
	} 
	return 0
}


# Check if the CD is a mixed-mode-cd

proc checkifmixedmode { } {

	set cdtoclist [getcdinfo]
	set type [lindex $cdtoclist 0]
	if { [string first "Mixed" $type] != -1} {
		return 1
	} 
	return 0
}


# Get the size of the first track of an CD. This is useful for
# determining the image-size of an non-iso-CD.
# Returns size in bytes or 0 if size is impossible.

proc getnonisosize { } {

	set cdtoclist [getcdinfo]
	# Substract 2 blocks from toc size. This is needed because
	# some blocks at the end of track are not readable. 
	# In some cases more than 2 blocks at the end are not readable,
	# if this happens we have to rely on the kernel to handle that...
	# All kernels up to pre2.0.8 and perhaps more CRASH in that case.

	set frames [expr [lindex $cdtoclist 9] - 2]

	if { $frames > 0 && $frames < 360000 } {	
		return [expr $frames*2048] 
	}
	return 0
}


# Creates num beeps from the speaker.

proc createbeep { num } {

	puts -nonewline "\a"
	flush stdout 
	if { $num > 1 } {
		for { set i 1} { $i < $num} { incr i } {
		 	after 250 
			puts -nonewline "\a"
			flush stdout 
		}
	}	
}


# Handles the beeps. 
# Type: 1 = Completed task
#       2 = Warning

proc sound { type } {
global XCDR_BEEP

	set doit 0

	switch $XCDR_BEEP {

	"on" 	{ set doit 1 }
	"off" 	{ set doit 0 }
	"oncompl" { if { $type == 1 } { set doit 1 } }
	"onwarn"  { if { $type == 2 } { set doit 1 } }

	}	

	if { $doit == 1 } {
		createbeep 1 
	}
}


# Append a string to the logfile

proc log { w str } {
global XCDR_VERSION
global XCDR_LOGFILE
global XCDR_LOGNAME
global DATE

	if { $XCDR_LOGFILE == "on" } {
		set opnflg [ catch { set fileid [ open $XCDR_LOGNAME a+] } ]

		if { $opnflg != 0 } {
			Msg_ErrorOpenLogfile .eolf $w 		
			set XCDR_LOGFILE "off"
			return
		}

		set date [exec $DATE "+%b %d %T"]
		set putflg [ catch { puts $fileid "$date XCDR $XCDR_VERSION: $str" } ]

		if { $putflg != 0 } {
			Msg_ErrorWriteLogfile .ewlf $w
			set XCDR_LOGFILE "off"
		}

		close $fileid
	}
}

