## Walkthrough of Debian 1.3's _sulogin_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

####  (sysvinit-2.71/src/sulogin.c:319)

```txt
Control Flow:
main <-- Here

331-339: Calls getopt to handle program arguments.

341: Calls signal to set alrm_handler as the signal handler
     for SIGARLM.

348: Calls open.

354: Calls getpid.

355: Calls getpgid.

356: Calls getppid and getpgid.

357: Calls ioctl with the TIOCGPGRP command.

377-378: Calls close.

386: Calls getrootpwent.

392: Calls getpasswd.

393-395: Calls sushell.

402: Returns zero if the user pressed ctrl-D.
```

#### getpid (libc-5.4.33/sysdeps/linux/\_\_getpid.S:21)

```txt
Control Flow:
main
    open
    getpid <-- Here

21-22: SYSCALL__ (getpid, 0)
           ret
```

#### SYSCALL\_\_ (libc-5.4.33/sysdeps/linux/

```txt
Control Flow:
main
    open
    getpid
        SYSCALL__ <-- Here

40-46: #define	SYSCALL__(name,args)	PSEUDO (__machdep_sys_##name, name, args) \
       .weak machdep_sys_##name; \
         machdep_sys_##name = __machdep_sys_##name; \
       .type __machdep_sys_##name,@function; \
       .type machdep_sys_##name,@function; \
       .L__machdep_sys_##name##end: .size __machdep_sys_##name,\
       .L__machdep_sys_##name##end - __machdep_sys_##name
```

#### PSEUDO (libc-5.4.33/sysdeps/linux/i386/sysdep.h:70)

```txt
Control Flow:
main
    open
    getpid
        SYSCALL__
            PSEUDO <-- Here

70-81: #define	PSEUDO(name, syscall_name, args)				      \
         .text;								      \
         ENTRY (name)							  \
           pushl %ebp;						      \
           movl %esp,%ebp;						  \
           PUSH_##args							  \
           movl $(SYS_##syscall_name), %eax;	  \
           MOVE_##args						      \
           int $0x80;						      \
           POP_##args						      \
           movl %ebp,%esp;					      \
           popl %ebp;
```

#### sys\_getpid (linux/kernel/sched.c:2173)

```txt
Control Flow:
main
    open
    getpid
        SYSCALL__
            PSEUDO
                sys_getpid <-- Here
```


#### sys\_getpgid (linux/kernel/sys.c:604)

```txt
Control Flow:
main
    open
    sys_getpid
    sys_getpgid <-- Here
```


#### sys\_getppid (linux/kernel/sched.c:1278)

```txt
Control Flow:
main
    open
    sys_getpid
    sys_getpgid
    sys_getppid <-- Here
```


#### sys\_ioctl (linux/fs/ioctl.c:58)

```txt
Control Flow:
main
    open
    sys_getpid
    sys_getpgid
    sys_getppid
    sys_getpgid
    sys_ioctl <-- Here (TIOCGPGRP)
```


#### getrootpwent (sysvinit-2.71/src/sulogin.c:141)

```txt
Control Flow:
main
    open
    sys_getpid
    sys_getpgid
    sys_getppid
    sys_getpgid
    sys_ioctl
    sys_close
    getrootpwent <-- Here
```


#### getpasswd (sysvinit-2.71/src/sulogin.c:229)

```txt
Control Flow:
main
    open
    sys_getpid
    sys_getpgid
    sys_getppid
    sys_getpgid
    sys_ioctl
    sys_close
    getrootpwent
    getpasswd <-- Here
```


#### sushell (sysvinit-2.71/src/sulogin.c:267)

```txt
Control Flow:
main
    open
    sys_getpid
    sys_getpgid
    sys_getppid
    sys_getpgid
    sys_ioctl
    sys_close
    getrootpwent
    getpasswd
    sushell <-- Here
```

