$ set noverify
$ if p1.eqs."" then goto usage
$ ! setlocal
$ doscript=""
$ parms=""
$ k8option=""
$ ! Scan through the options, checking for validity.
$ ! We don't really care about capitalization on these switches, and VMS
$ ! DCL does funny things, so just check for both cases.
$ if f$locate("-",p1).eq.0
$ then
$   if p1.eqs."-k".or.p1.eqs."-K" then k8options="-k8"
$   if p1.eqs."-r".or.p1.eqs."-R" then parms=parms+" "+"-r"
$   if p1.eqs."-s".or.p1.eqs."-S"
$   then
$     parms=parms+" "+"-s"
$     doscript="y"
$   endif
$   if p1.eqs."-w".or.p1.eqs."-W" then parms=parms+" "+"-w"
$ else
$   filename=p1
$   dirname=p2
$   goto GO
$ endif
$ if f$locate("-",p2).eq.0
$ then
$   if p2.eqs."-k".or.p2.eqs."-K" then k8options="-k8"
$   if p2.eqs."-r".or.p2.eqs."-R" then parms=parms+" "+"-r"
$   if p2.eqs."-s".or.p2.eqs."-S"
$   then
$     parms=parms+" "+"-s"
$     doscript="y"
$   endif
$   if p2.eqs."-w".or.p2.eqs."-W" then parms=parms+" "+"-w"
$ else
$   filename=p2
$   dirname=p3
$   goto GO
$ endif
$ if f$locate("-",p3).eq.0
$ then
$   if p3.eqs."-k".or.p3.eqs."-K" then k8options="-k8"
$   if p3.eqs."-r".or.p3.eqs."-R" then parms=parms+" "+"-r"
$   if p3.eqs."-s".or.p3.eqs."-S"
$   then
$     parms=parms+" "+"-s"
$     doscript="y"
$   endif
$   if p3.eqs."-w".or.p3.eqs."-W" then parms=parms+" "+"-w"
$ else
$   filename=p3
$   dirname=p4
$   goto GO
$ endif
$ if f$locate("-",p4).eq.0
$ then
$   if p4.eqs."-k".or.p4.eqs."-K" then k8options="-k8"
$   if p4.eqs."-r".or.p4.eqs."-R" then parms=parms+" "+"-r"
$   if p4.eqs."-s".or.p4.eqs."-S"
$   then
$     parms=parms+" "+"-s"
$     doscript="y"
$   endif
$   if p4.eqs."-w".or.p4.eqs."-W" then parms=parms+" "+"-w"
$   filename=p5
$   dirname=p6
$ else
$   filename=p4
$   dirname=p5
$   goto GO
$ endif
$ GO:
$ ! Check that there is a filename argument after the option list, and that the
$ ! file actually exists.
$ if filename.eqs."" 
$ then
$   write sys$output missing filename
$   goto usage
$ endif
$ if f$search(filename).eqs."" 
$ then
$   write sys$output "file not found"
$   goto END
$ endif
$ ! Call gnatf on the source filename argument with special options to generate
$ ! offset information. If this special compilation completes succesfully call
$ ! the gnatchp program to actually split the source file in one file per
$ ! compilation unit in the optional directory if given otherwise in the current
$ ! directory.
$ mypid=f$getjpi(0,"PID")
$ on error then goto FERR
$ define/user sys$output gnatchop.'mypid'
$ gnatf="$gnu:[000000]gnatf.exe"
$ gnatf -s -u 'k8options' 'filename'
$ CONTINUE:
$ if f$search("gnatchop.''mypid'").eqs."" then goto CERR
$ on error then goto CERR
$ define/user sys$input gnatchop.'mypid'
$ gnatchp="$gnu:[000000]gnatchp.exe"
$ gnatchp 'parms' 'filename' 'dirname'
$ delete gnatchop.'mypid';*
$ goto END
$ FERR:
$ write sys$output "Warning: parse errors detected, chop might not be successful"
$ goto CONTINUE
$ CERR:
$ write sys$output "Error: chop failed"
$ if doscript.eqs."y"
$ then
$   comname=f$extract(0,f$length(filename)-(f$length(filename)-f$locate(".",filename)),filename)+".COM"
$   open/write scriptout 'comname'
$   write scriptout "gcc -c -gnats 'p1' 'p2' 'p3' 'p4' 'p5' 'p6' 'p7' 'p8' ",filename
$   close scriptout
$ endif
$ goto END
$ USAGE:
$ write sys$output "Usage : gnatchop [-k] [-r] [-s] [-w] filename [directory]"
$ write sys$output
$ write sys$output "  k         limit filenames to 8 characters"
$ write sys$output "  r         generate Source_Reference pragmas"
$ write sys$output "  s         generate a compilation com file"
$ write sys$output "  w         overwrite existing filenames"
$ write sys$output "  filename  source file"
$ write sys$output "  directory directory to place split files (default is the current directory)"
$ END:
$ set verify
$ exit
