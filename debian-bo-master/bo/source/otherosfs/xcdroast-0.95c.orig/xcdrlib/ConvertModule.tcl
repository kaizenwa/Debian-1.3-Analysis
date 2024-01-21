# Module: ConvertModule.tcl
# 26.1.95 T. Niederreiter
# 
# External called functions:
# - convertnametoblkdevice{name}: returns corresponding block-device
#
# - convertnametogendevice{name}: returns corresponding generic-device
#
# - createnamelist {id1 id2}: creates a list of devices of type id1 or id2 
#
# - convertblkdevicetoname { blkdev }: returns name
# 
# - convertMB2time { mb }: return printable time 
#
# - converttime2frames { time }: returns frames 
#
# - convertframes2time { frames }: return printable time 
#

# Gets a name of a SCSI-Device as argument and returns its
# block-device-name.
# e.g. convertnametoblkdevice { "TOSHIBA XM-3601TA" }
# returns "/dev/sr0". 
# The array scsidevices must be definded.
 
proc convertnametoblkdevice { name } {
global scsidevices

	set blkdevice 0

	for { set i 0 } { $i < 8 } { incr i } {
		set tmpstr1 $scsidevices($i,4)
		set tmpstr2 $scsidevices($i,5)
		set tmpstr3 $scsidevices($i,9)
		set tmpstr4 "$tmpstr1 $tmpstr2$tmpstr3"

		if { $name == $tmpstr4 } {
			set blkdevice $scsidevices($i,1)
		}
	}
	return $blkdevice
}

# Gets a name of a SCSI-Device as argument and returns its
# generic-device-name.
# e.g. convertnametogendevice { "TOSHIBA XM-3601TA" }
# returns "/dev/sgc". 
# The array scsidevices must be definded.

proc convertnametogendevice { name } {
global scsidevices

	set gendevice 0

	for { set i 0 } { $i < 8 } { incr i } {
		set tmpstr1 $scsidevices($i,4)
		set tmpstr2 $scsidevices($i,5)
		set tmpstr3 $scsidevices($i,9)
		set tmpstr4 "$tmpstr1 $tmpstr2$tmpstr3"

		if { $name == $tmpstr4 } {
			set gendevice $scsidevices($i,0)
		}
	}
	return $gendevice
}

# Creates a list of all connected SCSI-Devices that are from type
# id1 or id2.
# The list contains the names in the "Vendor Model Number"-Form
 
proc createnamelist { id1 id2 } {
global scsidevices

        set namelist {}

        for { set i 0 } { $i < 8 } { incr i } {
                if { $scsidevices($i,2) == $id1 || $scsidevices($i,2) == $id2 } {
                        lappend namelist "$scsidevices($i,4) $scsidevices($i,5)$scsidevices($i,9)"
                }
        }
        return $namelist
}

# Converts a blkdevice (e.g. /dev/sda) to its symbolic name
# A symbolic name looks like that: "TOSHIBA XM3601TA"
 
proc convertblkdevicetoname { blkdev } {
global scsidevices

	set name "" 

	for { set i 0 } { $i < 8 } { incr i } {
		if { $blkdev == $scsidevices($i,1) } {
			set tmpstr1 $scsidevices($i,4)
			set tmpstr2 $scsidevices($i,5)
			set tmpstr3 $scsidevices($i,9)
			set name "$tmpstr1 $tmpstr2$tmpstr3"
		}
	}
	return $name
}


# Coverts MegaBytes into printable time 

proc convertMB2time { mb } {

	set frames [expr $mb*1024*1024/2352]

        set mins [expr $frames/(60*75)]
        set sec [expr ($frames%(60*75))/75]
        set centi_sec [expr (4*($frames%75)+1)/3]

        return [format "%2u:%02u.%02u" $mins $sec $centi_sec]
}


# Converts printable time to frames 

proc converttime2frames { time } {

	if { [scan $time "%u:%u.%u" mins sec centi_sec] != 3 } {
		# We got no valid string, so we assume we got
		# only MB..so convert the MB to frames
		return [expr $time*1024*1024/2352]
	}
	return [expr (($centi_sec*3/4)+($sec*75)+($mins*60*75))]
}


# Converts frames to printable time

proc convertframes2time { frames } {

        set mins [expr $frames/(60*75)]
        set sec [expr ($frames%(60*75))/75]
        set centi_sec [expr (4*($frames%75)+1)/3]

        return [format "%2u:%02u.%02u" $mins $sec $centi_sec]
}

