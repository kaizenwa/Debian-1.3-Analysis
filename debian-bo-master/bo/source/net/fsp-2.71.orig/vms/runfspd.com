$! RUNFSPD.COM
$!
$! Sample DCL script to start the fsp-deamon as a detached process.
$! The server reads it's configuration from the file SYS$LOGIN:fspd.conf.
$! Be sure this file is present!
$! You might also need to change the path to the fspd.exe file.
$! 
$! 29-DEC-92 FSP V2.6.5jt.9 version <S.A.Pechler@bdk.tue.nl>
$! 19-MAY-93 FSP V2.7.0 version     <S.A.Pechler@bdk.tue.nl>
$!
$ run user2:[bdaasp.fsp]fspd.exe /input=nl: -
                                 /output=pool:[bdaasp]fspd.out -
                                 /error=pool:[bdaasp]fspd.out -
                                 /detached -
                                 /process_name="FSPD"
