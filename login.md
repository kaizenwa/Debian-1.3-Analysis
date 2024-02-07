## Walkthrough of Debian 1.3's _login_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (util-linux-2.5/login-utils/login.c:223)

```txt
Control Flow:
main <-- Here

249: Calls signal to assign timedout as the signal handler
     for SIGALRM.

250: Calls alarm.

251-252: Calls signal to assign SIG_IGN as the signal handler
         for SIGQUIT and SIGINT.

254: Calls sys_setpriority.

265: Calls sys_gethostname.

270: Assigns zero to local variables fflag, hflag, and pflag.

271: Assigns one to the local variable passwd_req.

272-307: Calls getopt to handle program arguments.

313-314: Assigns one to the local variable ask.

337: Calls getdtablesize.

340: Calls ttyname.

346: Calls setpgrp.

351: Calls __tcgetattr.

356: Calls tcsetattr.

358: Calls sys_vhangup.

362: Calls sys_setsid.

366: Calls opentty.

384: Assigns zero to fflag.

385: Calls getloginname.

402: Calls getpwnam.

407: Calls checktty.

428: Calls rootterm.

452: Calls getpass.

453: Calls crypt.

482-483: Breaks out of the for loop if the password
         was correct.

499: Calls alarm to turn off timeout.

520: Calls endpwent.

532: Calls sys_getuid.

534: Calls sys_getegid.

538: Calls sys_setregid.

539: Calls sys_setreuid.

540: Calls sys_access.

594: Calls sys_getpid.

596: Calls utmpname.

597: Calls setutent.

598: Calls getutent.

607: Calls endutent.

624: Calls pututline.

625: Calls endutent.

627: Calls open.

636: Calls dolastlog.

645: Calls sys_chown.

649: Calls sys_chmod.

654: Calls setgid.

655: Calls initgroups.

661-662: Assigns "/bin/sh" to pwd->pw_shell.

675: Calls getenv.

695-710: Calls setenv to initialize the user's environment.

726: Calls motd.

728: Calls sys_stat.

733-735: Calls signal to assign SIG_DFL as the signal handler
         for SIGALRM, SIGQUIT, and SIGINT.

736: Calls signal to assign SIG_IGN as the signal handler
     for SIGTSTP.

737: Calls signal to assign SIG_DFL as the signal handler
     for SIGHUP.736: Calls signal to assign SIG_IGN as the signal handler
     for SIGTSTP.

737: Calls signal to assign SIG_DFL as the signal handler
     for SIGHUP.736: Calls signal to assign SIG_IGN as the signal handler
     for SIGTSTP.

737: Calls signal to assign SIG_DFL as the signal handler
     for SIGHUP.736: Calls signal to assign SIG_IGN as the signal handler
     for SIGTSTP.

737: Calls signal to assign SIG_DFL as the signal handler
     for SIGHUP.736: Calls signal to assign SIG_IGN as the signal handler
     for SIGTSTP.

737: Calls signal to assign SIG_DFL as the signal handler
     for SIGHUP.736: Calls signal to assign SIG_IGN as the signal handler
     for SIGTSTP.

737: Calls signal to assign SIG_DFL as the signal handler
     for SIGHUP.736: Calls signal to assign SIG_IGN as the signal handler
     for SIGTSTP.

737: Calls signal to assign SIG_DFL as the signal handler
     for SIGHUP.

740: Calls sys_setuid.

746: Calls sys_chdir.

773: Calls execlp.

774: Calls fprintf if the execlp call failed.

775: Calls exit(0) if the execlp call failed.
```

#### sys\_setpriority (linux/kernel/sys.c:65)

```txt
Control Flow:
main
    sys_setpriority <-- Here
```

#### getdtablesize (libc-5.4.33/login/param.h:8)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize <-- Here

8: #define getdtablesize() (NR_OPEN-1)
```


#### ttyname (libc-5.4.33/sysdeps/linux/ttyname.c:35)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname <-- Here
```

#### setpgrp (libc-5.4.33/sysdeps/linux/setpgrp.c:12)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp <-- Here

14: return setpgid(0,0);
```

#### sys\_setpgid (linux/kernel/sys.c:563)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
        sys_setpgid <-- Here
```

#### \_\_tcgetattr (libc-5.4.33/sysdeps/linux/\_\_tcgetatr.c:5)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr <-- Here

7: return __ioctl(fildes, TCGETS, termios_p);
```

#### sys\_ioctl (linux/fs/ioctl.c:58)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
        sys_ioctl <-- Here (TCGETS)
```

#### tcsetattr (libc-5.4.33/posix/tcsetattr.c:7)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr <-- Here
```

#### sys\_vhangup (linux/fs/open.c:652)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup <-- Here
```

#### opentty (util-linux-2.5/login-utils/login.c:209)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty <-- Here
```

#### getloginname (util-linux-2.5/login-utils/login.c:779)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname <-- Here
```

#### getpwnam (shadow-961025.orig/lib/pwent.c:673)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam <-- Here
```

#### checktty (util-linux-2.5/login-utils/checktty.c:320)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty <-- Here
```

#### rootterm (util-linux-2.5/login-utils/login.c:838)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm <-- Here
```

#### getpass (shadow-961025/lib/getpass.c:100)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass <-- Here
```

#### crypt (shadow-961025/contrib/pwdauth.c:102)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt <-- Here

102: #define crypt pw_encrypt
```

#### pw\_encrypt (shadow-961025/lib/encrypt.c:46)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
        pw_encrypt <-- Here
```

#### endpwent (shadow-961025/lib/pwent.c:362)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent <-- Here
```

#### sys\_getegid (linux/kernel/sched.c:1298)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid <-- Here
```

#### setregid (linux/kernel/sys.c:236)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid <-- Here
```

#### setreuid (linux/kernel/sys.c:445)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid <-- Here
```

#### sys\_access (linux/fs/open.c:246)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access <-- Here
```

#### utmpname (libc-5.4.33/login/utmp2.c:59)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access
    utmpname <-- Here
```

#### setutent (libc-5.4.33/login/utmp2.c:53)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access
    utmpname
    setutent <-- Here
```

#### getutent (libc-5.4.33/login/utmp2.c:67)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access
    utmpname
    setutent
    getutent <-- Here
```

#### endutent (libc-5.4.33/login/utmp2.c:157)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access
    utmpname
    setutent
    getutent
    endutent <-- Here
```

#### pututline (libc-5.4.33/login/utmp2.c:152)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access
    utmpname
    setutent
    getutent
    endutent
    pututline <-- Here
```

#### dolastlog (util-linux-2.5/login-utils/login.c:914)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access
    utmpname
    setutent
    getutent
    endutent
    pututline
    dolastlog <-- Here
```

#### sys\_setgid (linux/kernel/sys.c:272)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access
    utmpname
    setutent
    getutent
    endutent
    pututline
    dolastlog
    sys_setgid <-- Here
```

#### initgroups (libc-5.4.33/nys/nsw/src/misc/initgroups.c:66)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access
    utmpname
    setutent
    getutent
    endutent
    pututline
    dolastlog
    sys_setgid
    initgroups <-- Here
```

#### getenv (libc-5.4.33/posix/getenv.c:31)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access
    utmpname
    setutent
    getutent
    endutent
    pututline
    dolastlog
    sys_setgid
    initgroups
    getenv <-- Here
```

#### motd (util-linux-2.5/login-utils/login.c:879)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access
    utmpname
    setutent
    getutent
    endutent
    pututline
    dolastlog
    sys_setgid
    initgroups
    getenv
    motd <-- Here
```

#### sys\_stat (linux/fs/stat.c:110)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access
    utmpname
    setutent
    getutent
    endutent
    pututline
    dolastlog
    sys_setgid
    initgroups
    getenv
    motd
    sys_stat <-- Here
```

#### sys\_chdir (linux/fs/open.c:268)

```txt
Control Flow:
main
    sys_setpriority
    getdtablesize
    ttyname
    setpgrp
    __tcgetattr
    tcsetattr
    sys_vhangup
    open_tty
    getloginname
    getpwnam
    checktty
    rootterm
    getpass
    crypt
    endpwent
    sys_getegid
    sys_setregid
    sys_setreuid
    sys_access
    utmpname
    setutent
    getutent
    endutent
    pututline
    dolastlog
    sys_setgid
    initgroups
    getenv
    motd
    sys_stat
    sys_chdir <-- Here
```
