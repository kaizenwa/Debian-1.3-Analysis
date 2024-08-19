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

108: Calls creat.

110: Calls dup2.

111: Calls close.
```

#### creat ()

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
        creat <-- Here
```

#### dup2 ()

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
        creat
        dup2 <-- Here
```

#### close ()

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
        creat
        dup2
        close <-- Here
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

#### strip\_comments (xfree86-3.3/programs/xfs/os/config.c:135)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
        fsalloc
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
        strip_comments
        parse_config <-- Here

I might do this later.
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

214: Calls CheckMemory.
```

#### CheckMemory (xfree86-3.3/util/memleak/fmalloc.c:476)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
    OsInit
        GetTimeinMillis
        OsInitAllocator
            CheckMemory <-- Here
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
