# Module: Messages.tcl
# 12.3.95 T. Niederreiter
# 
# External called functions:
#
# - Msg_ConfFileNotFound { new old } 
#
# - Msg_ConfFileWrongVersion { new old progver filever } 
#
# - Msg_CfgSaved { new old }
#
# - Msg_ScsiSetupWarning { new old status } 
#
# - Msg_MustUmountImage { new old } 
#
# - Msg_MustMountImage { new old } 
#
# - Msg_UmountImageFail { new old } 
#
# - Msg_MountImageFail { new old }
#
# - Msg_CantVerifyPartition { new old } 
#
# - Msg_CantVerifyFile { new old } 
#
# - Msg_CantWritePartition { new old } 
#
# - Msg_CantWriteFile { new old } 
#
# - Msg_AudioMustFormatImage { new old }
#
# - Msg_NoAudioImage { new old }
#
# - Msg_MountDataFail { new old } 
#
# - Msg_DataWrongMntpnt { new old } 
#
# - Msg_Mounted { new old part dir } 
#
# - Msg_Umounted { new old part } 
#
# - Msg_ClearISOstrings { new old } 
#
# - Msg_ReadISOstrings { new old } 
#
# - Msg_NoMasterDir { new old } 
#
# - Msg_MasterToBig { new old size }
#
# - Msg_FormatFailed { new old dev } 
#
# - Msg_ImgPartWrongMnt { new old } 
#
# - Msg_MountedISO { new old } 
#
# - Msg_FatalUmntFail { new old } 
#
# - Msg_SetupNotCompl1 { new old } 
#
# - Msg_SetupNotCompl2 { new old } 
#
# - Msg_NoCD { new old } 
#
# - Msg_NotEnoughSpace { new old } 
#
# - Msg_CDoverrun { new old } 
#
# - Msg_InsertCDR { new old } 
#
# - Msg_TruncatedImage { new old } 
#
# - Msg_NoTracksSel { new old } 
#
# - Msg_InsertBurnedCDR { new old } 
#
# - Msg_DataNotFirst { new old } 
#
# - Msg_NoDataCD { new old } 
#
# - Msg_NoISOCD { new old } 
#
# - Msg_NoNonIso { new old } 
#
# - Msg_ImpossibleNonIso { new old } 
#
# - Msg_ImageNotFound { new old } 
#
# - Msg_NotPureIso { new old } 
#
# - Msg_QuickCopyWarning { new old } 
#
# - Msg_Need2CDRoms { new old } 
#
# - Msg_NoRoot { new old } 
#
# - Msg_ErrorOpenLogfile { new old } 
#
# - Msg_ErrorWriteLogfile { new old } 
#

proc Msg_ConfFileNotFound { new old } {
global WARNICO

	showdialog $new $old "Configuration-File not found" \
	"Configuration-File xcdroast.conf not found! Entering Setup ..." \
	$WARNICO -1 "Ok"
}

proc Msg_ConfFileWrongVersion { new old progver filever } {
global WARNICO

	showdialog $new $old "Configuration-File wrong version" \
	"Configuration-File for version $filever found, while \
	program is version $progver. Entering Setup..." \
	$WARNICO -1 "Ok"
}

proc Msg_CfgSaved { new old } {
global INFOICO

	showdialog $new $old "Configuration saved" \
	"Configuration saved." \
	$INFOICO -1 "Ok"
}

proc Msg_ScsiSetupWarning { new old status } {
global WARNICO

	switch $status {
		1 
	{ showdialog $new $old "SCSI-Bus changed" \
	"The SCSI-Bus has changed since last \"save\" in the Setup-Menu. \
	Please check if all settings are still valid! Entering Setup ..." \
	$WARNICO -1 "Ok" }
		2 
	{ showdialog $new $old "Partition-table changed" \
	"The Partition-tables or any Mountpoints of the connected hard \
	drives have changed since last \"save\" in the Setup-Menu. \
	Please check if all settings are still valid! Entering Setup ..." \
	$WARNICO -1 "Ok" }
		3 
	{ showdialog $new $old "SCSI-Bus and Partitions changed" \
	"The SCSI-Bus and the Partition-tables of the connected hard drives \
	have changed since last \"save\" in the Setup-Menu. \
	Please check if all settings are still valid! Entering Setup ..." \
	$WARNICO -1 "Ok" }
	}
}

proc Msg_MustUmountImage { new old } {
global INFOICO

	return [ showdialog $new $old "Umounting Image-Partition" \
	"The Image-Partition must be umounted to write an image to a \
	partition. I will do this now..."  \
	$INFOICO -1 "Ok" "Cancel" ] 
}

proc Msg_MustMountImage { new old } {
global INFOICO

	return [ showdialog $new $old "Mounting Image-Partition" \
	"To write the image to a file, the Image-Partition has to \
	be mounted. I will do this now..." \
	$INFOICO -1 "Ok" "Cancel" ] 
}

proc Msg_UmountImageFail { new old } {
global WARNICO

	showdialog $new $old "Umounting of Image failed" \
	"The umounting of the Image-Partition failed! \
	Please check if the partition is busy and try again." \
	$WARNICO -1 "Ok"  
}


proc Msg_MountImageFail { new old } {
global WARNICO

	return [ showdialog $new $old "Mounting of Image failed" \
	"The mounting of the Image-Partition failed! \
	Formatting the partition is recommended." \
	$WARNICO -1 "Format" "Cancel" ]
}


proc Msg_CantVerifyPartition { new old } {
global WARNICO

	showdialog $new $old "Can't verify Partition" \
	"Can't verify an image on a partition, because the partition \
	contains an mounted filesystem." \
	$WARNICO -1 "Ok"
}

proc Msg_CantVerifyFile { new old } {
global WARNICO

	showdialog $new $old "Can't verify File" \
	"Can't verify an image stored in a file, because the Image-\
	Partition does not contain an ext2-filesystem." \
	$WARNICO -1 "Ok"
}

proc Msg_CantWritePartition { new old } {
global WARNICO

	showdialog $new $old "Can't write Partition" \
	"Can't write an image on a partition, because the partition \
	contains an mounted filesystem." \
	$WARNICO -1 "Ok"
}

proc Msg_CantWriteFile { new old } {
global WARNICO

	showdialog $new $old "Can't write File" \
	"Can't write an image stored in a file, because the Image-\
	Partition does not contain an ext2-filesystem." \
	$WARNICO -1 "Ok"
}

proc Msg_AudioMustFormatImage { new old } {
global INFOICO

	showdialog $new $old "Need formatted partition" \
	"Can't read audio-tracks to an unformatted partition,\
 	formatting of the Image-Partition is necessary." \
	$INFOICO -1 "Format" "Cancel"
}

proc Msg_NoAudioImage { new old } {
global INFOICO

	showdialog $new $old "No Tracks on unformatted partition" \
	"The Image-Partition is not formatted and does therefore \
	not contain any Audio-Tracks. Returning to Copy-Dialog..." \
	$INFOICO -1 "Ok"
}

proc Msg_MountDataFail { new old } {
global WARNICO

	showdialog $new $old "Mount of Data-Partition failed" \
	"The mounting of the Data-Partition failed! Falling back \
	to case \"Data premounted\"..." \
	$WARNICO -1 "Ok"
}

proc Msg_DataWrongMntpnt { new old } {
global INFOICO

	showdialog $new $old "Data-Partition on wrong mountpoint" \
	"The Data-Partition is already mounted on a different \
	mountpoint as you specified. Adjust your mountpoint to \
	existing mount or fall back to case \"Data premounted\"...?" \
	$INFOICO -1 "Adjust" "Fall back"
}

proc Msg_Mounted { new old part dir } {
global INFOICO

	showdialog $new $old "Mount" \
	"Mounted $part on $dir." \
	$INFOICO -1 "Ok"
}

proc Msg_Umounted { new old part } {
global INFOICO

	showdialog $new $old "Umount" \
	"Umounted $part." \
	$INFOICO -1 "Ok"
}

proc Msg_ClearISOstrings { new old } {
global INFOICO

	return [ showdialog $new $old "Really clear ISO-Headers?" \
	"Are you sure you want to clear the ISO-Headers?" \
	$INFOICO -1 "Yes" "No" ] 
}

proc Msg_ReadISOstrings { new old } {
global INFOICO

	return [ showdialog $new $old "Read ISO-Headers from CD?" \
	"Are you sure you want to read the ISO-Headers from a \
	CD in the Source-CD-Rom-Device? All entries will be \
	deleted." \
	$INFOICO -1 "Yes" "No" ] 
}

proc Msg_NoMasterDir { new old } {
global WARNICO

	return [ showdialog $new $old "Invalid Master-Directory!" \
	"Invalid Master-Directory. Please pick another one." \
	$WARNICO -1 "Ok" ] 
}

proc Msg_MasterToBig { new old size} {
global WARNICO

	return [ showdialog $new $old "Not enough space" \
	"Your data to master is $size MB greater than the \
	available space on the Image-Partition. But consider \
	that in most cases the generated image is smaller than \
	the data itself, so do you want to continue or abort?" \
	$WARNICO -1 "Continue" "Abort" ] 
}

proc Msg_FormatFailed { new old dev } {
global WARNICO

	return [ showdialog $new $old "Format failed" \
	"The formatting of the Image-Partition $dev failed! \
	Please exit the program and check manually the problem." \
	$WARNICO -1 "Bail out" ] 
}

proc Msg_ImgPartWrongMnt { new old } {
global WARNICO

	return [ showdialog $new $old "Image-Partition wrong" \
	"The Image-Partition is already mounted, but on a \
	different mountpoint as you specified in the Setup-Menu. \
	Use this mountpoint or exit program?" \
	$WARNICO -1 "Use Mountpoint" "Exit" ] 
}

proc Msg_MountedISO { new old } {
global WARNICO

	return [ showdialog $new $old "Mounted Iso-Filesystem" \
	"Your Image-Partition contains a mounted ISO9660-Filesystem, \
	I must umount it to continue." \
	$WARNICO -1 "Ok" ] 
}

proc Msg_FatalUmntFail { new old } {
global WARNICO

	showdialog $new $old "Umounting of Image failed" \
	"The umounting of the Image-Partition failed! \
	Its safer to leave the program now." \
	$WARNICO -1 "Bail out"  
}

proc Msg_SetupNotCompl1 { new old } {
global WARNICO

	showdialog $new $old "Setup not completed" \
	"The Setup is not completed, please specify all \
	settings, or exit the program." \
	$WARNICO -1 "Continue" "Bail out"  
}

proc Msg_SetupNotCompl2 { new old } {
global WARNICO

	showdialog $new $old "Setup not completed" \
	"The Setup is not completed, please specify a logfile-name \
	or disable logging." \
	$WARNICO -1 "Continue" 
}

proc Msg_NoCD { new old } {
global WARNICO

	showdialog $new $old "No CD in drive" \
	"Please insert a CD in the CD-ROM-Device." \
	$WARNICO -1 "Ok" "Abort"
}

proc Msg_NotEnoughSpace { new old } {
global WARNICO

	showdialog $new $old "Not enough diskspace" \
	"Not enough diskspace for this operation. \
	You can continue at own risk, but expect corrupt \
	data in this case." \
	$WARNICO -1 "Abort" "Continue Anyway"
}

proc Msg_CDoverrun { new old } {
global WARNICO

	showdialog $new $old "Not enough space on CD" \
	"Not enough space on the CD-Recordable. You can \
	continue at own risk, but the resulting CD may be \
	unreadable." \
	$WARNICO -1 "Abort" "Continue Anyway"
}

proc Msg_InsertCDR { new old } {
global INFOICO

	showdialog $new $old "Please insert CD-Recordable" \
	"Please insert the CD-Recordable in the CD-Writer." \
	$INFOICO -1 "OK" "Cancel"
}

proc Msg_TruncatedImage { new old } {
global WARNICO

	showdialog $new $old "Image seems to be truncated" \
	"The image seems to be smaller than it should be, it \
	is not recommended to proceed." \
	$WARNICO -1 "Abort" "Continue Anyway"
}

proc Msg_NoTracksSel { new old } {
global WARNICO

	showdialog $new $old "No tracks selected" \
	"No tracks have been selected." \
	$WARNICO -1 "Ok"
}

proc Msg_InsertBurnedCDR { new old } {
global INFOICO

	showdialog $new $old "Insert CDR into CD-ROM-Drive" \
	"Please insert the burned CD-Recordable in your \
	CD-ROM-Device." \
	$INFOICO -1 "Ok" "Cancel"
}

proc Msg_DataNotFirst { new old } {
global WARNICO

	showdialog $new $old "Data-Track not first track" \
	"You are going to burn a Mixed-Mode-CD with the data-track \
	not being the first track. It is not recommended to continue." \
	$WARNICO -1 "Abort" "Continue Anyway"
}

proc Msg_NoDataCD { new old } {
global WARNICO

	showdialog $new $old "No Data-CD" \
	"This is no Data-CD. Use the Read-Audio-Menu \
	to copy this. " \
	$WARNICO -1 "Ok"
}

proc Msg_NoISOCD { new old } {
global INFOICO

	showdialog $new $old "Non-ISO9660-CD" \
	"This is a non-ISO9660-CD. I am trying to read \
	such a CD, but please note that you MUST read the data \
	into a file." \
	$INFOICO -1 "Ok"
}

proc Msg_NoNonIso { new old } {
global INFOICO

	showdialog $new $old "Non-ISO9660-CD" \
	"This is a non-ISO9660-CD. You can't read such a CD \
	in this menu. Please try the Read-Image-option in the \
	data-menu." \
	$INFOICO -1 "Ok"
}

proc Msg_ImpossibleNonIso { new old } {
global WARNICO

	showdialog $new $old "Non-ISO9660-CD" \
	"The TOC (Table Of Contents) of this CD reports an \
	impossible track-size. This can be a bug in the CD-Rom \
	firmware or a defect or non-standard CD. Please try \
	another CD-Rom-Drive, or forget your idea to copy \
	this CD. Sorry." \
	$WARNICO -1 "Ok"
}

proc Msg_ImageNotFound { new old } {
global WARNICO

	showdialog $new $old "Image not found" \
	"Can't find the Image. Aborting..." \
	$WARNICO -1 "Ok"
}

proc Msg_NotPureIso { new old } {
global WARNICO

	showdialog $new $old "Not ISO9660-Data-CD" \
	"Only pure ISO9660-Data-CD's can be copied with \
	this option. For Mixed-Mode, Audio or Non-ISO-CD's \
	use the Copy-CD-option." \
	$WARNICO -1 "Ok"
}

proc Msg_QuickCopyWarning { new old } {
global INFOICO

	showdialog $new $old "Quick Copy only at own risk" \
	"Warning: Use the Quick-Copy-Option only at own risk. \
	Please consider that you need a CD-Writer and an extra \
	CD-Rom-Drive, but no Image-Partition. The CD-Rom-Drive \
	should be at least twice \
	as fast as the CD-Writer. A read-error or any other \
	interruption of the copy-process results in a wasted CD-R.\
	SO DO DUMMY-WRITE FIRST!" \
	$INFOICO -1 "Ok" "Cancel"
}

proc Msg_Need2CDRoms { new old } {
global WARNICO

	showdialog $new $old "Need 2 CD-Rom-Devices" \
	"The CD-Read-Device must be different from the \
	CD-Writer-Device to quick-copy. Use the normal \
	CD-Copy-Option." \
	$WARNICO -1 "Ok"
}

proc Msg_NoRoot { new old } {
global WARNICO

	showdialog $new $old "No Root-Permissions" \
	"You must run this program with root-permissions!" \
	$WARNICO -1 "Ok"
}

proc Msg_ErrorOpenLogfile { new old } {
global WARNICO

	showdialog $new $old "Error opening logfile" \
	"Can't open the logfile! Logging disabled." \
	$WARNICO -1 "Ok"
}

proc Msg_ErrorWriteLogfile { new old } {
global WARNICO

	showdialog $new $old "Error writing to logfile" \
	"Can't write to the logfile! Logging disabled." \
	$WARNICO -1 "Ok"
}

