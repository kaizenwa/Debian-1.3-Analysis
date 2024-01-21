$!=============================================================================
$! INSTALL.COM -- Install SAOimage and configuration files.  This file
$! should be run from [...saoimage.vms]; it can find the files it needs!
$!=============================================================================
$ if p1 .eqs. "" then inquire/nopunc p1 "Installation directory: "
$ if p1 .eqs. "" then exit
$ saodestdir = p1
$!
$ thisproc = f$environ("procedure")
$ saovmsdir = f$parse(thisproc,,,"device") + f$parse(thisproc,,,"directory")
$ saoimgdir = saovmsdir - ".VMS]" + "]"
$!
$ create/direc/log 'saodestdir'
$ copy/log 'saoimgdir'saoimage.exe 'saodestdir'
$ copy/log 'saovmsdir'saosetup.com 'saodestdir'
$ copy/log 'saoimgdir'imtoolrc.    'saodestdir'
$ copy/log 'saovmsdir'saoimage.hlp 'saodestdir'
$ exit
