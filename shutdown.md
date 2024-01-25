## Walkthrough of Debian 1.3's _shutdown_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (sysvinit-2.71/src/shutdown.c:246)

```txt
Control Flow:
main <-- Here

269: Calls sys_getuid and sys_setuid.

271: Calls sys_getuid.

275: Calls strcpy to copy "1" to the global variable down_level.

278: Calls tcgetpgrp.

328-369: Calls getopt to handle program arguments.

371: Calls fopen on "/var/run/shutdown.pid".

424: Calls strcpy to copy "for reboot" to global variable newstate.

435: Calls sys_unlink.

436: Calls umask.

446-447: Calls signal to set SIG_IGN as the signal handler for
         every signal except SIGINT.

449-451: Sets stopit as the signal handler for SIGINT.

454: Calls sys_chdir.

458: Calls strcpy to copy "0" to local variable when.

471: Calls localtime.

476: Calls shutdown.

482: Calls donologin.

483: Increments the local variable didnolog.

489: Calls sys_shutdown.
```

#### tcgetpgrp (bash-2.0/jobs.c:260)

```txt
Control Flow:
main
    tcgetpgrp <-- Here

266: Calls ioctl with the TIOCGPGRP command.

268: Returns the local variable pgrp.
```


#### sys\_unlink (linux/fs/namei.c:702)

```txt
Control Flow:
main
    tcgetpgrp
    sys_unlink <-- Here
```


#### sys\_umask (linux/kernel/sys.c:929)

```txt
Control Flow:
main
    tcgetpgrp
    sys_unlink
    sys_umask <-- Here
```


#### sys\_chdir (linux/fs/open.c:268)

```txt
Control Flow:
main
    tcgetpgrp
    sys_unlink
    sys_umask
    sys_chdir <-- Here
```


#### localtime (timezone-7.55/localtime.c:1035)

```txt
Control Flow:
main
    tcgetpgrp
    sys_unlink
    sys_umask
    sys_chdir
    localtime <-- Here
```

#### shutdown (sysvinit-2.71/src/shutdown.c:209)

```txt
Control Flow:
main
    tcgetpgrp
    sys_unlink
    sys_umask
    sys_chdir
    localtime
    shutdown <-- Here (wt == 0)

215: Calls warn.

222-228: Initializes init's arguments.

230: Calls sys_unlink.

233: Calls execv.
```

#### donologin (sysvinit-2.71/src/shutdown.c:111)

```txt
Control Flow:
main
    tcgetpgrp
    sys_unlink
    sys_umask
    sys_chdir
    localtime
    shutdown
    donologin <-- Here

116: Calls sys_time.

     Note: We can see how time routes to sys_time in the
           libc-5.4.33/sysdeps/linux/time.S file.

117: Increments local variable t by 300.

119: Calls sys_unlink.

120: Calls fopen on "/etc/nologin".
```
