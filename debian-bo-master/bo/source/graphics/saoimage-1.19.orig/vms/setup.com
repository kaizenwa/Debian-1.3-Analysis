$!---------------------------------------------------------------------
$!
$! SETUP.COM -- Setup for running SAOimage on VMS  [user/site-specific]
$!
$!---------------------------------------------------------------------
$
$ define saodir  j:[iraf.saoimage]
$
$ saoimage   :== $saodir:saoimage.exe   -g +500+100
$ d_saoimage :== $saodir:d_saoimage.exe -g +500+100
$
$ make       :== @make.com
$ n          :== 'emacs' -b saodir:vms.notes
$
$ r_dispose  :== -
write sys$output """*** Postscript file "", ""<%s>"", "" can be printed ***"""
$
$ if f$logical("DECW$DISPLAY") .eqs. ""
$ then
$	if p1 .eqs. "" then p1 = "ZIPPIE"
$	set display/create/node='p1'
$ endif
$ show display
$
$ exit
                 
                     