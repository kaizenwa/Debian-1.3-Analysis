
# This procedure was kindly suggested to me by Brent Welch as a
# refile-and-reply procedure.  The Fcc will slow down the scan
# cache a bit, but if you need threading, like I do, it's still
# faster than doing refile, commit, wait for message to appear
# in new folder's ftoc, then reply.
# The current folder's scan cache isn't correctly updated, and
# so a warning is displayed in red that "Cannot find xxx" afterwards.
# This is normal and can be ignored.
proc Folder_RefileReply { folder } {
	Msg_Reply -nocc to -nocc cc;	# Should be configurable
	Folder_TargetMove $folder;	# Marks current
	Folder_CommitType "Stay here";	# Don't change folder, but commit
					# needed to effect proper threading
	global sedit;			# danger, internals alert!
	SeditSetHeader $sedit(t) Fcc $folder; # Set Fcc header in draft
}

# I applied the following diff to a personal copy of fdisp.tcl to
# create the control-button-3 binding:
#:r !sed 's/^/\#/g' fdisp.diff
#--- /usr/local/lib/exmh-1.6.1/fdisp.tcl	Thu Jul  6 20:00:58 1995
#+++ fdisp.tcl	Wed Jul 12 00:00:56 1995
#@@ -497,6 +497,8 @@
# 		    [list Folder_TargetMove $f]
#     $canvas bind $id <Shift-$fdisp(tarbutton)> \
# 		    [list Folder_TargetCopy $f]
#+    $canvas bind $id <Control-$fdisp(tarbutton)> \
#+		    [list Folder_RefileReply $f]
# 
#     if {[string compare $ftype goParent] == 0} {
# 	$canvas bind $id <$fdisp(navbutton)> \


