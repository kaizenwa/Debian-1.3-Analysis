## Walkthrough of Debian 1.3's _xserver_ Program

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

253: Calls ExpandCommandLine.

261: Calls ProcessCommandLine.

267: Increments global variable serverGeneration, which is defined
     on line 92 of xfree86-3.3/programs/Xserver/dix/globals.c.

279: Calls InitBlockAndWakeupHandlers.

281: Calls OsInit.

284: Calls CreateWellKnownSockets.

285: Calls InitProcVectors.

286: Calls xalloc.

294: Calls InitClient.

301: Calls InitClientResources.

304: Calls SetInputCheck.

334: Calls InitAtoms.

335: Calls InitEvents.

336: Calls InitGlyphCaching.

337: Calls ResetClientPrivates.

338: Calls ResetScreenPrivates.

339: Calls ResetWindowPrivates.

340: Calls ResetGCPrivates.

344: Calls ResetColormapPrivates.

345: Calls ResetFontPrivateIndex.

346: Calls InitCallbackManager.

347: Calls InitOutput.

353: Calls PrinterInitOutput.

355: Calls InitExtensions.

356: Calls InitClientPrivates.

361: Calls CreateScratchPixmapsForScreen.

366: Calls CreateGCperDepth.

368: Calls CreateDefaultStipple.

370: Calls CreateRootWindow.

373: Calls InitInput.

374: Calls InitandStartDevices.

377: Calls InitFonts.

378: Calls SetDefaultFontPath.

380: Calls SetDefaultFont.

382: Calls CreateRootCursor.

392: Calls InitRootWindow.

393: Calls DefineInitialRootWIndow.

395: Calls CreateConnectionBlock.

398: Calls Dispatch.

401-402: Calls SaveScreens.

403: Calls CloseDownExtensions.

404: Calls FreeAllResources.

405: Calls CloseDownDevices.

408: Calls FreeScratchPixmapsForScreen.

409: Calls FreeGCperDepth.

410: Calls FreeDefaultStipple.

412: Calls FreeScreen.

415: Calls xfree.

416: Calls FreeFonts.

417: Calls xfree.

421: Calls OsCleanup.

422: Calls ddxGiveUp.

426: Calls xfree.

428: Returns zero.
```

#### ExpandCommandLine (xfree86-3.3/programs/Xserver/os/utils.c:1041)

```txt
Control Flow:
main
    ExpandCommandLine <-- Here
```

#### ProcessCommandLine (xfree86-3.3/programs/Xserver/os/utils.c:603)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine <-- Here
```

#### InitBlockAndWakeupHandlers (xfree86-3.3/programs/Xserver/dix/dixutils.c:548)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers <-- Here
```

#### OsInit (xfree86-3.3/programs/Xserver/os/osinit.c:87)

```txt
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit <-- Here

97: Calls fclose on stdin.

98: Calls fclose on stdout.

103: Calls sys_write.

106: Calls sprintf to write "/usr/adm/X%smsgs" into local character
     array fname.

     Note: the external character pointer display is used to fill in
           the %s in the format string.

110: Calls fopen on fname.

112: Calls fileno.

122: Calls setlinebuf.

127: Calls sys_getpgrp.

128: Calls sys_setpgid.

183: Assigns TRUE to static variable been_here.

186: Calls OsInitAllocator.
```

#### fclose (libc-5.4.33/libio/iostdio.h:63)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit
        fclose <-- Here

63: #define fclose _IO_fclose
```

#### \_IO\_fclose (lib-5.4.33/libio/iofclose.c:35)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit
        fclose
            _IO_fclose <-- Here
```

#### fileno (libc-5.4.33/libio/stdio/fileno.c:5)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit
        fclose
        fileno <-- Here
```

#### dup2 (libc-5.4.33/sysdeps/pthreads/mit/fd.c:734)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit
        fclose
        fileno
        dup2 <-- Here
```

#### setlinebuf (libc-5.4.33/libio/iostdio.h:111)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit
        fclose
        fileno
        dup2
        setlinebuf <-- Here

111: #define setlinebuf _IO_setlinebuf
```

#### \_IO\_setlinebuf (libc-5.4.33/libio/iolibio.h:49)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit
        fclose
        fileno
        dup2
        setlinebuf
            _IO_setlinebuf <-- Here

49: #define _IO_setlinebuf(_FP) _IO_setvbuf(_FP, NULL, 1, 0)
```

#### \_IO\_setvbuf (libc4-4.6.27/libio-4.6.26/iosetvbuf.c:32)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit
        fclose
        fileno
        dup2
        setlinebuf
            _IO_setlinebuf
                _IO_setvbuf <-- Here
```

#### sys\_getpgrp (linux/kernel/sys.c:617)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit
        fclose
        fileno
        dup2
        setlinebuf
        sys_getpgrp <-- Here
```

#### sys\_setpgid (linux/kernel/sys.c:563)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit
        fclose
        fileno
        dup2
        setlinebuf
        sys_getpgrp
        sys_setpgid <-- Here
```

#### OsInitAllocator (xfree86-3.3/programs/xfs/os/utils.c:211)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit
        fclose
        fileno
        dup2
        setlinebuf
        sys_getpgrp
        sys_setpgid
        OsInitAllocator <-- Here
```

#### CreateWellKnownSockets (xfree86-3.3/programs/Xserver/os/connection.c:265)

```txt
Control Flow:
main
    ExpandCommandLine
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit
    CreateWellKnownSockets <-- Here
```

#### InitProcVectors (xfree86-3.3/programs/Xserver/dix/dispatch.c:3433)

```txt
Control Flow:
main
    ...
    ProcessCommandLine
    InitBlockAndWakeupHandlers
    OsInit
    CreateWellKnownSockets
    InitProcVectors <-- Here
```

#### xalloc (xfree86-3.3/programs/Xserver/include/os.h:80)

```txt
Control Flow:
main
    ...
    InitBlockAndWakeupHandlers
    OsInit
    CreateWellKnownSockets
    InitProcVectors
    xalloc <-- Here

80: #define xalloc(size) Xalloc((unsigned long)(size))
```

#### Xalloc (libc-5.4.33/nys/nis/src/xalloc.h:38)

```txt
Control Flow:
main
    ...
    InitBlockAndWakeupHandlers
    OsInit
    CreateWellKnownSockets
    InitProcVectors
    xalloc
        Xalloc <-- Here

38: #define Xalloc(bufp, len) xalloc((void **) (bufp), (len), sizeof(**(bufp)))
```

#### xalloc (libc-5.4.33/nys/nis/src/nis\_xalloc.c:31)

```txt
Control Flow:
main
    ...
    InitBlockAndWakeupHandlers
    OsInit
    CreateWellKnownSockets
    InitProcVectors
    xalloc
        Xalloc
            xalloc <-- Here
```

#### InitClient (xfree86-3.3/programs/Xserver/dix/dispatch.c:3588)

```txt
Control Flow:
main
    ...
    OsInit
    CreateWellKnownSockets
    InitProcVectors
    xalloc
    InitClient <-- Here
```

#### InitClientResources (xfree86-3.3/programs/Xserver/dix/resource.c:169)

```txt
Control Flow:
main
    ...
    CreateWellKnownSockets
    InitProcVectors
    xalloc
    InitClient
    InitClientResources <-- Here
```

#### SetInputCheck (xfree86-3.3/programs/Xserver/dix/dispatch.c:144)

```txt
Control Flow:
main
    ...
    InitProcVectors
    xalloc
    InitClient
    InitClientResources
    SetInputCheck <-- Here
```

#### InitAtoms (xfree86-3.3/programs/Xserver/dix/atom.c:201)

```txt
Control Flow:
main
    ...
    xalloc
    InitClient
    InitClientResources
    SetInputCheck
    InitAtoms <-- Here
```

#### InitEvents (xfree86-3.3/programs/Xserver/dix/events.c:3294)

```txt
Control Flow:
main
    ...
    InitClient
    InitClientResources
    SetInputCheck
    InitAtoms
    InitEvents <-- Here
```

#### InitGlyphCaching (xfree86-3.3/lib/font/util/fontutil.c:207)

```txt
Control Flow:
main
    ...
    InitClientResources
    SetInputCheck
    InitAtoms
    InitEvents
    InitGlyphCaching <-- Here
```

#### ResetClientPrivates (xfree86-3.3/programs/Xserver/dix/privates.c:61)

```txt
Control Flow:
main
    ...
    SetInputCheck
    InitAtoms
    InitEvents
    InitGlyphCaching
    ResetClientPrivates <-- Here
```

#### ResetScreenPrivates (xfree86-3.3/programs/Xserver/dix/rpviates.c:114)

```txt
Control Flow:
main
    ...
    InitAtoms
    InitEvents
    InitGlyphCaching
    ResetClientPrivates
    ResetScreenPrivates <-- Here
```

#### ResetWindowPrivates (xfree86-3.3/programs/Xserver/dix/privates.c:154)

```txt
Control Flow:
main
    ...
    InitEvents
    InitGlyphCaching
    ResetClientPrivates
    ResetScreenPrivates
    ResetWindowPrivates <-- Here
```

#### ResetGCPrivates (xfree86-3.3/programs/Xserver/dix/privates.c:204)

```txt
Control Flow:
main
    ...
    InitGlyphCaching
    ResetClientPrivates
    ResetScreenPrivates
    ResetWindowPrivates
    ResetGCPrivates <-- Here
```

#### ResetColormapPrivates (xfree86-3.3/programs/Xserver/dix/privates.c:306)

```txt
Control Flow:
main
    ...
    ResetClientPrivates
    ResetScreenPrivates
    ResetWindowPrivates
    ResetGCPrivates
    ResetColormapPrivates <-- Here
```

#### ResetFontPrivateIndex (xfree86-3.3/lib/font/util/private.c:46)

```txt
Control Flow:
main
    ...
    ResetScreenPrivates
    ResetWindowPrivates
    ResetGCPrivates
    ResetColormapPrivates
    ResetFontPrivateIndex <-- Here
```

#### InitCallbackManager (xfree86-3.3/programs/Xserver/dix/dixutils.c:999)

```txt
Control Flow:
main
    ...
    ResetWindowPrivates
    ResetGCPrivates
    ResetColormapPrivates
    ResetFontPrivateIndex
    InitCallbackManager <-- Here
```

#### InitOutput (xfree86-3.3/programs/Xserver/Xprint/ddxInit.c:76)

```txt
Control Flow:
main
    ...
    ResetGCPrivates
    ResetColormapPrivates
    ResetFontPrivateIndex
    InitCallbackManager
    InitOutput <-- Here
```

#### PrinterInitOutput (xfree86-3.3/programs/Xserver/Xprint/Init.c:1277)

```txt
Control Flow:
main
    ...
    ResetColormapPrivates
    ResetFontPrivateIndex
    InitCallbackManager
    InitOutput
    PrinterInitOutput <-- Here
```

#### InitExtensions (xfree86-3.3/programs/Xserver/mi/miinittext.c:171)

```txt
Control Flow:
main
    ...
    ResetFontPrivateIndex
    InitCallbackManager
    InitOutput
    PrinterInitOutput
    InitExtensions <-- Here
```

#### InitClientPrivates (xfree86-3.3/programs/Xserver/dix/dispatch.c:3650)

```txt
Control Flow:
main
    ...
    InitCallbackManager
    InitOutput
    PrinterInitOutput
    InitExtensions
    InitClientPrivates <-- Here
```

#### CreateScratchPixmapsForScreen (xfree86-3.3/programs/Xserver/dix/pixmap.c:98)

```txt
Control Flow:
main
    ...
    InitOutput
    PrinterInitOutput
    InitExtensions
    InitClientPrivates
    CreateScratchPixmapsForScreen <-- Here
```

#### CreateGCperDepth (xfree86-3.3/programs/Xserver/dix/gc.c:1038)

```txt
Control Flow:
main
    ...
    PrinterInitOutput
    InitExtensions
    InitClientPrivates
    CreateScratchPixmapsForScreen
    CreateGCperDepth <-- Here
```

#### CreateDefaultStipple (xfree86-3.3/programs/Xserver/dix/gc.c:1069)

```txt
Control Flow:
main
    ...
    InitExtensions
    InitClientPrivates
    CreateScratchPixmapsForScreen
    CreateGCperDepth
    CreateDefaultStipple <-- Here
```

#### CreateRootWindow (xfree86-3.3/programs/Xserver/dix/window.c:359)

```txt
Control Flow:
main
    ...
    InitClientPrivates
    CreateScratchPixmapsForScreen
    CreateGCperDepth
    CreateDefaultStipple
    CreateRootWindow <-- Here
```

#### InitInput (xfree86-3.3/programs/Xserver/Xprint/ddxInit.c:178)

```txt
Control Flow:
main
    ...
    CreateScratchPixmapsForScreen
    CreateGCperDepth
    CreateDefaultStipple
    CreateRootWindow
    InitInput <-- Here
```

#### InitAndStartDevices (xfree86-3.3/programs/Xserver/dix/devices.c:166)

```txt
Control Flow:
main
    ...
    CreateGCperDepth
    CreateDefaultStipple
    CreateRootWindow
    InitInput
    InitAndStartDevices <-- Here
```

#### InitFonts (xfree86-3.3/programs/Xserver/dix/dixfonts.c:1911)

```txt
Control Flow:
main
    ...
    CreateDefaultStipple
    CreateRootWindow
    InitInput
    InitAndStartDevices
    InitFonts <-- Here
```

#### SetDefaultFontPath (xfree86-3.3/programs/Xserver/dix/dixfonts.c:1807)

```txt
Control Flow:
main
    ...
    CreateRootWindow
    InitInput
    InitAndStartDevices
    InitFonts
    SetDefaultFontPath <-- Here
```

#### SetDefaultFont (xfree86-3.3/programs/Xserver/dix/dixfonts.c:119)

```txt
Control Flow:
main
    ...
    InitInput
    InitAndStartDevices
    InitFonts
    SetDefaultFontPath
    SetDefaultFont <-- Here
```

#### CreateRootCursor (xfree86-3.3/programs/Xserver/dix/cursor.c:379)

```txt
Control Flow:
main
    ...
    InitAndStartDevices
    InitFonts
    SetDefaultFontPath
    SetDefaultFont
    CreateRootCursor <-- Here
```

#### InitRootWindow (xfree86-3.3/programs/Xserver/dix/window.c:464)

```txt
Control Flow:
main
    ...
    InitFonts
    SetDefaultFontPath
    SetDefaultFont
    CreateRootCursor
    InitRootWindow <-- Here
```

#### DefineInitialRootWindow (xfree86-3.3/programs/Xserver/dix/events/c:1599)

```txt
Control Flow:
main
    ...
    SetDefaultFontPath
    SetDefaultFont
    CreateRootCursor
    InitRootWindow
    DefineInitialRootWindow <-- Here
```

#### CreateConnectionBlock (xfree86-3.3/programs/Xserver/dix/main.c:434)

```txt
Control Flow:
main
    ...
    SetDefaultFont
    CreateRootCursor
    InitRootWindow
    DefineInitialRootWindow
    CreateConnectionBlock <-- Here
```

#### Dispatch (xfree86-3.3/programs/Xserver/dix/dispatch.c:224)

```txt
Control Flow:
main
    ...
    CreateRootCursor
    InitRootWindow
    DefineInitialRootWindow
    CreateConnectionBlock
    Dispatch <-- Here
```

#### SaveScreens (xfree86-3.3/programs/Xserver/dix/window.c:3260)

```txt
Control Flow:
main
    ...
    InitRootWindow
    DefineInitialRootWindow
    CreateConnectionBlock
    Dispatch
    SaveScreens <-- Here
```

#### CloseDownExtensions (xfree86-3.3/programs/Xserver/dix/extension.c:259)

```txt
Control Flow:
main
    ...
    DefineInitialRootWindow
    CreateConnectionBlock
    Dispatch
    SaveScreens
    CloseDownExtensions <-- Here
```

#### FreeAllResources (xfree86-3.3/programs/Xserver/dix/resource.c:704)

```txt
Control Flow:
main
    ...
    CreateConnectionBlock
    Dispatch
    SaveScreens
    CloseDownExtensions
    FreeAllResources <-- Here
```

#### CloseDownDevices (xfree86-3.3/programs/Xserver/dix/devices.c:283)

```txt
Control Flow:
main
    ...
    Dispatch
    SaveScreens
    CloseDownExtensions
    FreeAllResources
    CloseDownDevices <-- Here
```

#### FreeScratchPixmapsForScreen (xfree86-3.3/programs/Xserver/dix/pixmap.c:108)

```txt
Control Flow:
main
    ...
    SaveScreens
    CloseDownExtensions
    FreeAllResources
    CloseDownDevices
    FreeScratchPixmapsForScreen <-- Here

111: Calls FreeScratchPixmapHeader.
```

#### FreeScratchPixmapHeader (xfree86-3.3/programs/Xserver/dix/pixmap.c:81)

```txt
Control Flow:
main
    ...
    CloseDownExtensions
    FreeAllResources
    CloseDownDevices
    FreeScratchPixmapsForScreen
        FreeScratchPixmapHeader <-- Here
```

#### FreeGCperDepth (xfree86-3.3/programs/Xserver/dix/gc.c:1021)

```txt
Control Flow:
main
    ...
    CloseDownExtensions
    FreeAllResources
    CloseDownDevices
    FreeScratchPixmapsForScreen
    FreeGCperDepth <-- Here
```

#### FreeDefaultStipple (xfree86-3.3/programs/Xserver/dix/gc.c:1107)

```txt
Control Flow:
main
    ...
    FreeAllResources
    CloseDownDevices
    FreeScratchPixmapsForScreen
    FreeGCperDepth
    FreeDefaultStipple <-- Here
```

#### FreeScreen (xfree86-3.3/programs/Xserver/dix/main.c:737)

```txt
Control Flow:
main
    ...
    CloseDownDevices
    FreeScratchPixmapsForScreen
    FreeGCperDepth
    FreeDefaultStipple
    FreeScreen <-- Here
```

#### xfree (xfree86-3.3/programs/Xserver/include/os.h:85)

```txt
Control Flow:
main
    ...
    FreeScratchPixmapsForScreen
    FreeGCperDepth
    FreeDefaultStipple
    FreeScreen
    xfree <-- Here

85: #define xfree(ptr) Xfree((pointer)(ptr))
```

#### Xfree (xfree86-3.3/programs/Xserver/os/util.c:1252)

```txt
Control Flow:
main
    ...
    FreeScratchPixmapsForScreen
    FreeGCperDepth
    FreeDefaultStipple
    FreeScreen
    xfree
        Xfree <-- Here

1255-1256: Calls free.
```

#### free (libc-5.4.33/dl-malloc/malloc.c:858)

```txt
Control Flow:
main
    ...
    FreeScratchPixmapsForScreen
    FreeGCperDepth
    FreeDefaultStipple
    FreeScreen
    xfree
        Xfree
            free <-- Here

858: #pragma weak free = __libc_free
```

#### \_\_libc\_free (libc-5.4.33/malloc/free.c:202)

```txt
Control Flow:
main
    ...
    FreeScratchPixmapsForScreen
    FreeGCperDepth
    FreeDefaultStipple
    FreeScreen
    xfree
        Xfree
            free
                __libc_free <-- Here
```

#### FreeFonts (xfree86-3.3/programs/Xserver/dix/dixfonts.c:2122)

```txt
Control Flow:
main
    ...
    FreeGCperDepth
    FreeDefaultStipple
    FreeScreen
    xfree
    FreeFonts <-- Here
```

#### OsCleanup (xfree86-3.3/programs/Xserver/os/osinit.c:200)

```txt
Control Flow:
main
    ...
    FreeDefaultStipple
    FreeScreen
    xfree
    FreeFonts
    OsCleanup <-- Here
```

#### ddxGiveup (xfree86-3.3/programs/Xserver/Xprint/ddxinit.c:252)

```txt
Control Flow:
main
    ...
    FreeScreen
    xfree
    FreeFonts
    OsCleanup
    ddxGiveUp <-- Here
```
