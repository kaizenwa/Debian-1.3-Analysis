## Walkthrough of Debian 1.3's _agetty_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (util-linux-2.5/login-utils/agetty.c:195)

```txt
Control Source:
main <-- Here

214: Assigns "agetty" to the global variable progname.

230: Calls parse_args.

233: Calls sys_setsid.

243: Calls open_tty.

249: Calls sys_getpid.

250: Calls sys_ioctl with the TIOCSPGRP command.

255: Calls termio_init.

285: Calls get_logname.

286: Calls next_speed.

295: Calls termio_final.

303: Calls execl.

304: Calls error if execl failed.

305: Calls exit(0) if execl failed.
```

#### parse\_args (util-linux-2.5/login-utils/agetty.c:311)

```txt
Control Source:
main
    parse_args <-- Here

320-387: Calls getopt to process command line arguments.

395: Calls parse_speeds.

396: Assigns the third command line argument to op->tty.
```

#### parse\_speeds (util-linux-2.5/login-utils/agetty.c:412)

```txt
Control Source:
main
    parse_args
        parse_speeds <-- Here

421: Calls bcode and assigns it's return value to op->speeds.
```

#### bcode (util-linux-2.5/login-utils/agetty.c:1033)

```txt
Control Source:
main
    parse_args
        parse_speeds
            bcode <-- Here

1069: Calls atol to convert command line argument to a long.

1072-1073: Searches the static speedtab array for a matching
           entry and returns it.

1074: Returns zero if there was no maching entry.
```

#### sys\_setsid (linux/kernel/sys.c:635)

```txt
Control Source:
main
    parse_args
    sys_setsid <-- Here
```

#### open\_tty (util-linux-2.5/login-utils/agetty.c:510)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty <-- Here

517: Closes STDOUT.

518: Closes STDERR.

530: Calls sys_stat.

537: Closes STDIN.

540: Calls sys_open.

556: Calls sys_dup to create STDOUT and STDERR.

568: Calls sys_ioctl with the TCGETA command.

578: Calls sys_chown.

579: Calls sys_chmod.

580: Assign zero to errno.
```

#### sys\_close (linux/fs/open.c:631)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
        sys_close <-- Here
```

#### sys\_stat (linux/fs/stat.c:110)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
        sys_close
        sys_stat <-- Here
```

#### sys\_dup (linux/fs/fcntl.c:50)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
        sys_close
        sys_stat
        sys_dup <-- Here
```

#### sys\_ioctl (linux/fs/ioctl.c:58)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
        sys_close
        sys_stat
        sys_dup
        sys_ioctl <-- Here
```

#### sys\_chown (linux/fs/open.c:432)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
        sys_close
        sys_stat
        sys_dup
        sys_ioctl
        sys_chown <-- Here
```

#### sys\_chmod (linux/fs/open.c:352)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
        sys_close
        sys_stat
        sys_dup
        sys_ioctl
        sys_chown
        sys_chmod <-- Here
```

#### sys\_getpid (linux/kernel/sched.c:1273)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid <-- Here

1275: return current->pid;
```

#### sys\_ioctl (linux/fs/ioctl.c:58)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl <-- Here
```

#### termio\_init (util-linux-2.5/login-utils/agetty.c:589)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl
    termio_init <-- Here

603: Calls sys_ioctl with the TCFLSH command.

614: Calls sys_ioctl with the TCSETA command.

616: Calls sys_fcntl.
```

#### sys\_ioctl (linux/fs/ioctl.c:58)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl
    termio_init
        sys_ioctl <-- Here (TCFLSH)
```


#### sys\_ioctl (linux/fs/ioctl.c:58)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl
    termio_init
        sys_ioctl
        sys_ioctl <-- Here (TCSETA)
```


#### sys\_fcntl (linux/fs/fcntl.c:55)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl
    termio_init
        sys_ioctl
        sys_ioctl
        sys_fcntl <-- Here
```

#### get\_logname (util-linux-2.5/login-utils/agetty.c:836)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl
    termio_init
    get_logname <-- Here

860: Calls sleep.

861: Calls ioctl with the TCFLSH command.

869: Calls do_prompt.

...

942: Returns static local variable logname.
```

#### sleep (libc-5.4.33/posix/sleep.c:49)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl
    termio_init
    get_logname
        sleep <-- Here
```

#### do\_prompt (util-linux-2.5/login-utils/agetty.c:686)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl
    termio_init
    get_logname
        sleep
        ioctl
        do_prompt <-- Here

...

813: Calls sys_gethostname.

814: Calls sys_write.
```

#### sys\_gethostname (linux/kernel/sys.c:797)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl
    termio_init
    get_logname
        sleep
        ioctl
        do_prompt
            sys_gethostname <-- Here

803: Calls verify_area.

809: Calls memcpy_tofs.

810: Returns zero.
```

#### verify\_area (linux/mm/memory.c:677)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl
    termio_init
    get_logname
        sleep
        ioctl
        do_prompt
            sys_gethostname
                verify_area <-- Here
```


#### memcpy\_tofs (linux/include/asm-i386/segment.h:258)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl
    termio_init
    get_logname
        sleep
        ioctl
        do_prompt
            sys_gethostname
                verify_area
                memcpy_tofs <-- Here
```

#### next\_speed (util-linux-2.5/login-utils/agetty.c:822)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl
    termio_init
    get_logname
    next_speed <-- Here
```

#### termio\_final (util-linux-2.5/login-utils/agetty.c:947)

```txt
Control Source:
main
    parse_args
    sys_setsid
    open_tty
    sys_getpid
    sys_ioctl
    termio_init
    get_logname
    next_speed termio_final <-- Here
```

