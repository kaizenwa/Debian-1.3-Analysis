## Walkthrough of Debian 1.3's _hostname_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (hostname-2.01/hostname.c:252)

```txt
Control Flow:
main <-- Here

281: Calls catopen.

287: Assigns the first program argument to the global
     static variable program_name.

296-325: Calls getop_long to handle program arguments.

312-314: Assigns optarg to the local variable file
         and breaks for the --file case.

344: Calls setfilename.

377: Calls NLS_CATCLOSE.

378: Calls exit(0).
```

#### catopen (libc4-4.6.27/nls/msgcat.c:118)

```txt
Control Flow:
main
    catopen <-- Here
```

#### setfilename (hostname-2.01/hostname.c:171)

```txt
Control Flow:
main
    catopen
    setfilename <-- Here

177: Calls fopen.

178: Calls fgets.

184-185: Calls sethname.
```

#### sethname (hostname-2.01/hostname.c:71)

```txt
Control Flow:
main
    catopen
    setfilename
        fopen
        fgets
        sethname <-- Here

76: Calls sys_sethostname.
```

#### sys\_sethostname (linux/kernel/sys.c:781)

```txt
Control Flow:
main
    catopen
    setfilename
        fopen
        fgets
        sethname
            sys_sethostname <-- Here
```

#### NLS\_CATCLOSE (hostname-2.01/net-locale.h:37)

```txt
Control Flow:
main
    catopen
    setfilename
    NLS_CATCLOSE <-- Here

37: #  define NLS_CATCLOSE(catfd) catclose (catfd);
```

#### catclose (libc4-4.6.27/nls/msgcat.c:315)

```txt
Control Flow:
main
    catopen
    setfilename
    NLS_CATCLOSE
        catclose <-- Here
```
