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

#### OsInit (xfree86-3.3/programs/Xserver/os/osinit.c:87)

```txt
Control Flow:
main
    ProcessCmdLine
    InitErrors
    ReadConfigFile
    OsInit <-- Here

103: Calls write.

106: Calls sprintf to write "/usr/adm/X%smsgs" into local character
     array fname.

     Note: the external character pointer display is used to fill in
           the %s in the format string.

112: Calls fopen on fname.

114: Calls fileno.

...
```

#### CacheInit (xfree86-3.3/programs/lbxproxy/di/cache.c:101)

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

#### InitProcVectors (xfree86-3.3/programs/Xserver/dix/dispatch.c:3433)

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

#### InitClient (xfree86-3.3/)

```txt
Control Flow:
main
    ...
    CacheInit
    CreateSockets
    InitProcVectors
    ResetSockets
    InitClient <-- Here
```

#### InitClientResources (xfree86-3.3/programs/Xserver/dix/resource.c:169)

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

#### InitExtensions (xfree86-3.3/programs/Xserver/mi/miinittext.c:171)

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

#### InitAtoms (xfree86-3.3/programs/Xserver/dix/atom.c:201)

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

#### InitFonts (xfree86-3.3/)

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

#### CacheReset (xfree86-3.3/)

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

#### CloseErrors (xfree86-3.3/)

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

#### exit ()

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
