## Walkthrough of Debian 1.3's _init_ Program 

#### main (sysvinit-2.71/src/init.c:1731)

```txt
Linux 2.4.22 calls execve on System V's init program with the following
arguments:

static char * argv_init[MAX_INIT_ARGS+2] = { "init", NULL, };
char * envp_init[MAX_INIT_ENVS+2] = { "HOME=/", "TERM=linux", NULL, };

2809: Assigns "init" to the p stack variable.

2823: Assigns one to the isinit global variable.

2891: Assigns "init" to the global argv0 variable.

2893: Calls setproctitle to set argv0 to "init boot".

2894: Calls init_main.
```

#### setproctitle (sysvinit-2.71/src/init.c:120)

```txt
Control Flow:
main
    setproctitle <-- Here

496-498: Calls vsnprintf to write "init boot" into the buf variable.

501: Calls memset to bzero the argv0 global variable.

502: Calls strncpy to copy "init boot" from the buf variable into
     the argv0 global variable.
```

#### init\_main (sysvinit-2.71/src/init.c:2517)

```txt
Control Flow:
main
    setproctitle
    init_main <-- Here

2547: Calls init_reboot with RB_DISABLE_CAD as the
      argument.
       
2548: Calls open(sysvinit-2.71/src/dev/console, O_RDWR | O_NOCTTY).

2558-2559: Calls SETSIG to ignore all signals.

2562-2572: Initializes signals with the appropriate
           signal handlers.

2574: Calls console_init.

2580-2582: Calls close on STDIN, STDOUT, and STDERR.

2583: Calls console_stty.

2584: Calls setsid.

2589: Calls setenv to set the default PATH variable to
      "/sbin:/usr/sbin:/bin:/usr/bin".

2595: Calls open(sysvinit-2.71/src/var/run/utmp, O_WRONLY|O_CREAT|O_TRUNC, 0644).

2601: Calls initlog.

2620: Assigns '#' to the runlevel global variable.

2621: Calls read_inittab.

2636: Calls start_if_needed.

2641: Calls boot_transitions.

2656: Calls check_init_fifo.

2663: Calls fail_check.

2666: Calls process_signals.

2669: Calls start_if_needed.
```

#### init\_reboot (sysvinit-2.71/src/reboot.h:50)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot <-- Here

50: #define init_reboot(magic)	reboot(magic)

    Note: This calls the Linux kernel's reboot
          system call with LINUX_REBOOT_CMD_CAD_OFF
          as the command argument. 

          This results in the C_A_D global variable
          being set to zero.

          LINUX_REBOOT_CMD_CAD_OFF is defined on line 28
          of /include/linux/reboot.h.
```

#### SETSIG (sysvinit-2.71/src/init.c:94)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG <-- Here

94-100:

#define SETSIG(sa, sig, fun, flags) \
		do { \
			sa.sa_handler = fun; \
			sa.sa_flags = flags; \
			sigemptyset(&sa.sa_mask); \
			sigaction(sig, &sa, NULL); \
		} while(0)
```

#### console\_init (sysvinit-2.71/src/init.c:512)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init <-- Here
```

#### console\_stty (sysvinit-2.71/src/init.c:741)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty <-- Here

746: Calls console_open.
```

#### console\_open (sysvinit-2.71/src/init.c:550)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
            console_open <-- Here
```

#### initlog (sysvinit-2.71/src/init.c:832)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog <-- Here
```

#### read\_inittab (sysvinit-2.71/src/init.c:1233)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab <-- Here (first call)

We will use the description of old Linux inittabs from this webpage:

https://www.cyberciti.biz/howto/question/man/inittab-man-page.php

1266: Calls fopen to open /etc/inittab as a read-only file.

1273: Checks if fp is null and calls fgets to read a line of the inittab.

1274: Sets the done local variable to one when we've reached the
      end of the file.

1280-1281: Calls snprintf to write "~~:S:wait:/sbin/sulogin\n" into buf.

1285: Increments the local variable lineNo.

1330-1334: Assigns the appropriate action to the local variable actionNo.

           Note: The actions array is defined on lines 152-172 of this
                 source file.

1351: Continues if the current line from the inittab is a duplicate.

1356: Calls imalloc to allocate a CHILD structure to ch.

      Note: The CHILD structure is defined on lines 83-95 of this
            source file.

1361: Assigns actionNo to ch->action. 

1362: Calls strncpy to copy id into ch->id.

1363: Calls strncpy to copy process into ch->process.

1364-1368: Copies the run level from rlevel to ch->rlevel.

1367: Adjusts the 's' run level in the CHILD structure to 'S'.

1369: Calls strncpy to copy the run levels from rlevel to
      ch->rlevel.

      Note: Why do we call strncpy if it's just going to undo
            the 'S' adjustment on line 1367?

            Wouldn't it make more sense to adjust rlevel[f] instead
            of ch->rlevel[f]?

1371: Calls strcpy to copy "0123456789" into ch->rlevel.

1372-1373: Calls strcpy to copy "S0123456789" into ch->rlevel if
           the CHILD strcutre has a special action.

           Note: ISPOWER is defined on line 147 of this source file.

1380-1381: Calls strcpy to copy "*" into ch->rlevel.

1418: Assigns NULL to ch->next.

1419-1423: Inserts the CHILD structure at the end of the list.

           Note: The first time we iterate through this while
                 loop, we assign ch to the newFamily global
                 variable.

1438: Calls fclose to close /etc/inittab.

1564: Calls sigprocmask to set the signals for the child
      process's to ignore.

1569: Assigns newFamily to the family global variable.

1570: Iterates through the list of CHILD structures and
      initializes their new field to NULL.

1571: Sets newFamily to NULL.

1572: I will look into this later.
```

#### imalloc (sysvinit-2.71/src/init.c:241)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
            imalloc <-- Here

245: Calls malloc to allocate memory.

246: Calls initlog to state that the system is out of memory.

247: Calls do_sleep.

249: Calls memset to bzero the allocated memory.

250: Returns the pointer to the allocated memory.
```

#### do\_sleep (sysvinit-2.71/src/init.c:225)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
            imalloc
                initlog
                do_sleep <-- Here

232: Calls select in a while loop.

     Note: I look at this in more detail later.
```

#### start\_if\_needed (sysvinit-2.71/src/init.c:1614)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
        start_if_needed <-- Here (first call)

1636: Initializes the delete local variable to one.

1645: Clears the RUNNING and WAITING bits in the current
      CHILD structure's flags field.

1646-1647: Clears the XECUTED flag in the current
           CHILD structure's flags field.

1648: Sets the current CHILD structure's pid field to zero.
```

#### boot\_transitions (sysvinit-2.71/src/init.c:2311)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
        start_if_needed
        boot_transitions <-- Here (first call)

2320-2321: Iterates through the list of CHILD structures to
           find any running processes that do NOT have the
           BOOT action.

2325: Initializes the loglevel local variable to -1.

2326: Initializes the oldlevel local variable to 'N'.

2332: Assigns zero to the wrote_utmp_reboot global variable.

2333: Assigns zero to the wrote_wtmp_reboot global variable.

2334: Calls write_utmp_wtmp.

2337: Calls get_init_default to obtain the default run level
      and assigns it to the newlevel local variable.

2343: Assigns '*' to the runlevel global variable.
```

#### write\_utmp\_wtmp (sysvinit-2.71/src/utmp.c:244)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
        start_if_needed
        boot_transitions
            write_utmp_wtmp <-- Here

```

#### get\_init\_default (sysvinit-2.71/src/init.c:1689)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
        start_if_needed
        boot_transitions
            write_utmp_wtmp
            get_init_default <-- Here

1700: Assigns ch->rlevel to to the p local variable.

1701-1704: Obtains the maximum run level from ch->rlevel
           and assigns it to the lvl local variable.

1726: Returns lvl. 
```

#### get\_init\_default (sysvinit-2.71/src/init.c:1689)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
        start_if_needed
        boot_transitions
            get_init_default <-- Here
```

#### check\_init\_fifo (sysvinit-2.71/src/init.c:2168)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
        start_if_needed
        boot_transitions
        check_init_fifo <-- Here
```

#### fail\_check (sysvinit-2.71/src/init.c:1853)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
        start_if_needed
        boot_transitions
        check_init_fifo
        fail_check <-- Here

```

#### process\_signals (sysvinit-2.71/src/init.c:2397)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
        start_if_needed
        boot_transitions
        check_init_fifo
        fail_check
        process_signals <-- Here
```

#### start\_if\_needed (sysvinit-2.71/src/init.c:1614)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
        start_if_needed
        boot_transitions
        check_init_fifo
        fail_check
        process_signals
        start_if_needed <-- Here (second call)

1639: Calls startup to execute the /etc/rc script.

1640: Assigns zero to delete.
```

#### startup (sysvinit-2.71/src/init.c:1198)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
        start_if_needed
        boot_transitions
        check_init_fifo
        fail_check
        process_signals
        start_if_needed
            startup <-- Here

1214: Sets the WAITING bit in ch->flags.

1222: Sets the RUNNING bit in ch->flags.

1223: Calls spawn.
```

#### spawn (sysvinit-2.71/src/init.c:927)

```txt
Control Flow:
main
    setproctitle
    init_main
        init_reboot
        open
        SETSIG
        console_init
        console_stty
        setsid
        setenv
        initlog
        read_inittab
        start_if_needed
        boot_transitions
        check_init_fifo
        fail_check
        process_signals
        start_if_needed
            startup
                spawn <-- Here

```
