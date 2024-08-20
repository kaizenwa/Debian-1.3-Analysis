## Walkthrough of Debian 1.3's _xfs_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (xfree86-3.3/programs/xfs/difs/main.c:88)

```txt
Control Flow:
main <-- Here

100: Calls ProcessCmdLine.

101: Calls InitErrors.

106: Calls ReadConfigFile.

112: Increments global variable serverGeneration, which is defined
     on line 46 of xfree86-3.3/lib/font/util/miscutil.c.

113: Calls OsInit.

116: Calls CacheInit.

117: Calls CreateSockets.

118: Calls InitProcVectors.

119: Calls fsalloc.

125: Calls fsalloc.

129: Calls ResetSockets.

132: Calls InitClient.

138: Calls InitClientResources.

141: Calls InitExtensions.

142: Calls InitAtoms.

143: Calls InitFonts.

144: Calls SetCOnfigValues.

145: Calls create_connection_block.

152: Calls Dispatch.

159: Calls CacheReset.

160: Calls CloseDownExtensions.

163: Calls fsfree.

165: Calls ReadConfigFile.

169: Calls CloseErrors.

170: Calls exit.
```

#### ProcessCmdLine (xfree86-3.3/programs/xfs/os/utils.c:296)

```txt
Control Flow:
main
    ProcessCmdLine <-- Here
```

#### InitErrors (xfree86-3.3/programs/xfs/os/error.c:95)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors <-- Here

108: Calls sys_creat.

110: Calls sys_dup2.

111: Calls sys_close.
```

#### sys\_creat (linux/fs/open.c:599)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
        sys_creat <-- Here

601: return sys_open(pathname, O_CREAT | O_WRONLY | O_TRUNC, mode);
```

#### sys\_open (linux/fs/open.c:574)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
        sys_creat
            sys_open <-- Here
```

#### sys\_dup2 (linux/fs/fcntl.c:37)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
        sys_creat
        sys_dup2 <-- Here
```

#### sys\_close (linux/fs/open.c:631)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
        sys_creat
        sys_dup2
        sys_close <-- Here
```

#### ReadConfigFile (xfree86-3.3/programs/xfs/os/config.c:307)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile <-- Here

315: Calls fsalloc.

323: Calls fopen.

328: Calls fread.

335: Calls ftell.

339: Calls fclose.

341: Calls strip_comments.

342: Calls parse_config.

344: Calls fsfree.
```

#### fsalloc (xfree86-3.3/programs/xfs/include/os.h:68)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc <-- Here

68: #define fsalloc(size)    FSalloc((unsigned long)size)
```

#### FSalloc (xfree86-3.3/programs/xfs/os/utils.c:359)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
            FSalloc <-- Here
```

#### fread (libc4-4.6.27/libio/iostdio.h:71)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread <-- Here

71: #define fread _IO_fread
```

#### \_IO\_fread (libc4-4.6.27/libio/iofread.c:28)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread
            _IO_fread <-- Here
```

#### ftell (libc4-4.6.27/libio/iostdio.h:76)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread
        ftell <-- Here

76: #define ftell _IO_ftell
```

#### \_IO\_ftell (libc4-4.6.27/libio/ioftell.c:30)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread
        ftell
            _IO_ftell <-- Here
```

#### fclose (libc4-4.6.27/libio/iostdio.h:60)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread
        ftell
        fclose <-- Here

60: #define fclose _IO_fclose
```

#### \_IO\_fclose (libc4-4.6.27/libio/iofclose.c:31)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread
        ftell
        fclose
            _IO_fclose <-- Here
```

#### strip\_comments (xfree86-3.3/programs/xfs/os/config.c:135)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread
        ftell
        fclose
        strip_comments <-- Here

142-143: Calls blank_comment.
```

#### blank\_comment (xfree86-3.3/programs/xfs/os/config.c:113)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread
        ftell
        fclose
        strip_comments
            blank_comment <-- Here

113-114: #define blank_comment(c)    while (!iseol(*(c)) && *(c) != '\0')    \
                        *(c)++= ' ';
```

#### parse\_config (xfree86-3.3/programs/xfs/os/config.c:178)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread
        ftell
        fclose
        strip_comments
        parse_config <-- Here

I might do this later.
```

#### fsfree (xfree86-3.3/programs/xfs/include/os.h:70)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread
        ftell
        fclose
        strip_comments
        parse_config
        fsfree <-- Here

70: #define fsfree(ptr)    FSfree((pointer)ptr)
```

#### FSfree (xfree86-3.3/programs/xfs/os/utils.c:443)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread
        ftell
        fclose
        strip_comments
        parse_config
        fsfree
            FSfree <-- Here

450-451: Calls free.
```

#### free (libc-5.4.33/dl-malloc/malloc.c:858)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread
        ftell
        fclose
        strip_comments
        parse_config
        fsfree
            FSfree
                free <-- Here

858: #pragma weak free = __libc_free
```

#### \_\_libc\_free (libc-5.4.33/malloc/free.c:202)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
        fread
        ftell
        fclose
        strip_comments
        parse_config
        fsfree
            FSfree
                free
                    __libc_free <-- Here
```

#### OsInit (xfree86-3.3/programs/xfs/os/osinit.c:59)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
    OsInit <-- Here

61: Calls GetTimeinMillis.

62: Calls OsInitAllocator.
```

#### GetTimeInMillis (xfree86-3.3/programs/xfs/os/utils.c:195)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
    OsInit
        GetTimeInMillis <-- Here

199: Calls X_GETTIMEOFDAY.
```

#### X\_GETTIMEOFDAY (xfree86-3.3/include/Xos.h:250)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
    OsInit
        GetTimeInMillis
            X_GETTIMEOFDAY <-- Here

250: #define X_GETTIMEOFDAY(t) gettimeofday(t, (struct timezone*)0)
```

#### sys\_gettimeofday (linux/kernel/time.c:87)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
    OsInit
        GetTimeInMillis
            X_GETTIMEOFDAY
                sys_gettimeofday <-- Here
```

#### OsInitAllocator (xfree86-3.3/programs/xfs/os/utils.c:211)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
    OsInit
        GetTimeinMillis
        OsInitAllocator <-- Here
```

#### CacheInit (xfree86-3.3/programs/xfs/difs/cache.c:83)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit <-- Here
```

#### CreateSockets (xfree86-3.3/programs/xfs/os/connection.c:201)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets <-- Here

228: Calls getdtablesize.

243-244: Calls malloc.

260: Calls _FontTransReopenCOTSServer.

278: Calls _FontTransMakeAllCOTSServerListeners.

286: Calls _FontTransGetConnectionNumber.
```

#### malloc (libc-5.4.33/malloc/malloc.c:29)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc <-- Here

29: #pragma weak malloc = __libc_malloc
```

#### \_\_libc\_malloc (libc-5.4.33/malloc/malloc.c:158)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
            __libc_malloc <-- Here
```

#### \_FontTransReopenCOTSServer (xfree86-3.3/lib/xtrans/Xtrans.c:631)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer <-- Here

639: return TRANS(Reopen) (XTRANS_OPEN_COTS_SERVER, trans_id, fd, port);
```

#### \_FontTransReopen (xfree86-3.3/lib/xtrans/Xtrans.c:492)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
            _FontTransReopen <-- Here

524: Calls xalloc.

538: Calls _FontTransSocketReopenCOTSServer.
```

#### \_FontTransSocketReopenCOTSServer (xfree86-3.3/lib/xtrans/Xtranssock.c:653)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
            _FontTransReopen
                _FontTransSocketReopenCOTSServer <-- Here

666: This is a no-op for Debian.

668: Calls _FontTransSocketSelectFamily.

676: Calls _FontTransSocketReopen.
```

#### \_FontTransSocketSelectFamily (xfree86-3.3/lib/xtrans/Xtranssock.c:267)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
            _FontTransReopen
                _FontTransSocketReopenCOTSServer
                    _FontTransSocketSelectFamily <-- Here
```

#### \_FontTransSocketReopen (xfree86-3.3/lib/xtrans/Xtranssock.c:424)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
            _FontTransReopen
                _FontTransSocketReopenCOTSServer
                    _FontTransSocketSelectFamily
                    _FontTransSocketReopen <-- Here

436-437: Calls xcalloc.
```

#### xcalloc (xfree86-3.3/lib/xtrans/transport.c:63)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
            _FontTransReopen
                _FontTransSocketReopenCOTSServer
                    _FontTransSocketSelectFamily
                    _FontTransSocketReopen
                        xcalloc <-- Here

63: #define xcalloc(_num,_size) calloc(_num,_size)
```

#### calloc (libc-5.4.33/dl-malloc/malloc.c:857)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
            _FontTransReopen
                _FontTransSocketReopenCOTSServer
                    _FontTransSocketSelectFamily
                    _FontTransSocketReopen
                        xcalloc
                            calloc <-- Here

857: #pragma weak calloc = __libc_calloc
```

#### \_\_libc\_calloc (libc-5.4.33/malloc/calloc.c:33)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
            _FontTransReopen
                _FontTransSocketReopenCOTSServer
                    _FontTransSocketSelectFamily
                    _FontTransSocketReopen
                        xcalloc
                            calloc
                                __libc_calloc <-- Here
```

#### \_FontTransMakeAllCOTSServerListeners (xfree86-3.3/lib/xtrans/Xtrans.c:1087)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners <-- Here

1116: Calls _FontTransOpenCOTSServer.

1127: Calls _FontTransCreateListener.
```

#### \_FontTransOpenCOTSServer (xfree86-3.3/lib/xtrans/Xtrans.c:587)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
            _FontTransOpenCOTSServer <-- Here

593: return TRANS(Open) (XTRANS_OPEN_COTS_SERVER, address);
```

#### \_FontTransOpen (xfree86-3.3/lib/xtrans/Xtrans.c:393)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
            _FontTransOpenCOTSServer
                _FontTransOpen <-- Here

415: Calls _FontTransParseAddress.

423: Calls _FontTransSelectTransport.

445: Calls _FontTransSocketOpenCOTSServer.
```

#### \_FontTransParseAddress (xfree86-3.3/lib/xtrans/Xtrans.c:200)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
            _FontTransOpenCOTSServer
                _FontTransOpen
                    _FontTransParseAddress <-- Here
```

#### \_FontTransSelectTransport (xfree86-3.3/lib/xtrans/Xtrans.c:164)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
            _FontTransOpenCOTSServer
                _FontTransOpen
                    _FontTransParseAddress
                    _FontTransSelectTransport <-- Here
```

#### \_FontTransSocketOpenCOTSServer (xfree86-3.3/lib/xtrans/Xtranssock.c:503)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
            _FontTransOpenCOTSServer
                _FontTransOpen
                    _FontTransParseAddress
                    _FontTransSelectTransport
                    _FontTransSocketOpenCOTSServer <-- Here

516: This is a no-op for Debian.

518: Calls _FontTransSocketSelectFamily.

526-527: Calls _FontTransSocketOpen.
```

#### \_FontTransSocketOpen (xfree86-3.3/lib/xtrans/Xtranssock.c:372)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
            _FontTransOpenCOTSServer
                _FontTransOpen
                    _FontTransParseAddress
                    _FontTransSelectTransport
                    _FontTransSocketOpenCOTSServer
                        _FontTransSocketOpen <-- Here

382-383: Calls xcalloc.

389-390: Calls socket.

412-413: Calls setsockopt.
```

#### setsockopt (libc-5.4.33/sysdeps/linux/setsockopt.c:23)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
            _FontTransOpenCOTSServer
                _FontTransOpen
                    _FontTransParseAddress
                    _FontTransSelectTransport
                    _FontTransSocketOpenCOTSServer
                        _FontTransSocketOpen
                            setsockopt <-- Here

32: return (socketcall (SYS_SETSOCKOPT, args));
```

#### \_FontTransCreateListener (xfree86-3.3/lib/xtrans/Xtrans.c:779)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
            _FontTransOpenCOTSServer
            _FontTransCreateListener <-- Here

785: return ciptr->transptr->CreateListener (ciptr, port);
```

#### \_FontTransSocketCreateListener (xfree86-3.3/lib/xtrans/Xtranssock.c:752)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
            _FontTransOpenCOTSServer
            _FontTransCreateListener
                _FontTransSocketCreateListener <-- Here

770: Calls bind.

802: Calls listen.
```

#### bind (libc-5.4.33/sysdeps/linux/bind.c:22)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
            _FontTransOpenCOTSServer
            _FontTransCreateListener
                _FontTransSocketCreateListener
                    bind <-- Here

29: return socketcall(SYS_BIND, args);
```

#### socketcall (libc-5.4.33/sysdeps/linux/\_\_socketcall.S:21)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
            _FontTransOpenCOTSServer
            _FontTransCreateListener
                _FontTransSocketCreateListener
                    bind
                        socketcall <-- Here
```
```asm
SYSCALL__ (socketcall, 2)
	ret
```

#### listen (libc-5.4.33/sysdeps/linux/listen.c:21)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
            _FontTransOpenCOTSServer
            _FontTransCreateListener
                _FontTransSocketCreateListener
                    bind
                    listen <-- Here

27: return socketcall(SYS_LISTEN, args);
```

#### \_FontTransGetConnectionNumber (xfree86-3.3/lib/xtrans/Xtrans.c:1041)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        malloc
        _FontTransReopenCOTSServer
        _FontTransMakeAllCOTSServerListeners
        _FontTransGetConnectionNumber <-- Here
```

#### getdtablesize (libc-5.4.33/login/param.h:8)

```txt
Control Flow:
main
    ...
    InitErrors
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
        getdtablesize <-- Here

8: #define getdtablesize() (NR_OPEN-1)

   Note: NR_OPEN is defined as 256 on line 30 of
         linux/include/linux/fs.h.
```

#### InitProcVectors (xfree86-3.3/programs/xfs/difs/dispatch.c:1021)

```txt
Control Flow:
main
    ...
    ReadConfigFile
    OsInit
    CacheInit
    CreateSockets
    InitProcVectors <-- Here
```

#### ResetSockets (xfree86-3.3/programs/xfs/os/connection.c:313)

```txt
Control Flow:
main
    ...
    OsInit
    CacheInit
    CreateSockets
    InitProcVectors
    ResetSockets <-- Here
```

#### InitClient (xfree86-3.3/programs/xfs/difs/dispatch.c:1036)

```txt
Control Flow:
main
    ...
    CacheInit
    CreateSockets
    InitProcVectors
    ResetSockets
    InitClient <-- Here

1043: Calls GetTimeInMillis.
```

#### InitClientResources (xfree86-3.3/programs/xfs/difs/resource.c:157)

```txt
Control Flow:
main
    ...
    CreateSockets
    InitProcVectors
    ResetSockets
    InitClient
    InitClientResources <-- Here
```

#### InitExtensions (xfree86-3.3/programs/xfs/difs/extensions.c:202)

```txt
Control Flow:
main
    ...
    InitProcVectors
    ResetSockets
    InitClient
    InitClientResources
    InitExtensions <-- Here
```

#### InitAtoms (xfree86-3.3/programs/xfs/difs/atom.c:198)

```txt
Control Flow:
main
    ...
    ResetSockets
    InitClient
    InitClientResources
    InitExtensions
    InitAtoms <-- Here
```

#### InitFonts (xfree86-3.3/xfs/difs/initfonts.c:57)

```txt
Control Flow:
main
    ...
    InitClient
    InitClientResources
    InitExtensions
    InitAtoms
    InitFonts <-- Here
```

#### SetConfigValues (xfree86-3.3/programs/xfs/os/config.c:271)

```txt
Control Flow:
main
    ...
    InitClientResources
    InitExtensions
    InitAtoms
    InitFonts
    SetConfigValues <-- Here
```

#### create\_connection\_block (xfree86-3.3/programs/xfs/difs/main.c:181)

```txt
Control Flow:
main
    ...
    InitExtensions
    InitAtoms
    InitFonts
    SetConfigValues
    create_connection_block <-- Here
```

#### Dispatch (xfree86-3.3/programs/xfs/difs/dispatch.c:108)

```txt
Control Flow:
main
    ...
    InitAtoms
    InitFonts
    SetConfigValues
    create_connection_block
    Dispatch <-- Here
```

#### CacheReset (xfree86-3.3/programs/xfs/difs/cache.c:190)

```txt
Control Flow:
main
    ...
    InitFonts
    SetConfigValues
    create_connection_block
    Dispatch
    CacheReset <-- Here
```

#### CloseDownExtensions (xfree86-3.3/programs/Xserver/dix/extension.c:259)

```txt
Control Flow:
main
    ...
    SetConfigValues
    create_connection_block
    Dispatch
    CacheReset
    CloseDownExtensions <-- Here
```

#### CloseErrors (xfree86-3.3/programs/xfs/os/error.c:119)

```txt
Control Flow:
main
    ...
    create_connection_block
    Dispatch
    CacheReset
    CloseDownExtensions
    CloseErrors <-- Here
```

#### exit (libc-5.4.33/gcc/libgcc2.c:2104)

```txt
Control Flow:
main
    ...
    Dispatch
    CacheReset
    CloseDownExtensions
    CloseErrors
    exit <-- Here
```
