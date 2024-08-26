## Walkthrough of XFree86 3.3's _XOpenDisplay_ Function

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### XOpenDisplay (xfree86-3.3/lib/X11/OpenDis.c:86)

```txt
Control Flow:
XOpenDisplay <-- Here

119: Calls _XSendClientPrefix.

120: Calls _X11TransConnectDisplay.

121: Calls _XAllocID.

122: Calls _XAllocIDs.

151: Calls Xcalloc.

161-165: Calls _X11TransConnectDisplay.

170: Calls _X11TransGetConnectionNumber.

245: Calls InitDisplayLock.

250: Calls _XPollfdCacheInit.

304: Calls LockDisplay.

336: Calls _Xread.

492: Calls _XVIDtoVisual.

500: Calls Xfree.

513: Calls UnlockDisplay.

523-525: Calls XCreateGC.

534: Calls XSynchronize.

540: Calls LockDisplay.
```

#### \_XSendClientPrefix (xfree86-3.3/lib/X11/ConnDis.c:469)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix <-- Here
```

#### \_X11TransConnectDisplay (xfree86-3.3/lib/X11/ConnDis.c:98)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay <-- Here
```

#### \_XAllocID (xfree86-3.3/lib/X11/XlibInt.c:1491)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID <-- Here
```

#### \_XAllocIDS (xfree86-3.3/lib/X11/XlibInt.c:1521)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS <-- Here
```

#### Xcalloc (xfree86-3.3/lib/X11/Xlibint.h:324)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc <-- Here

324: # define Xcalloc(nelem, elsize) calloc(((nelem) == 0 ? 1 : (nelem)), (elsize))
```

#### \_X11TransConnectDisplay (xfree86-3.3/lib/X11/ConnDis.c:98)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay <-- Here
```

#### \_X11TransGetConnectionNumber (xfree86-3.3/lib/xtrans/Xtrans.c:1041)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
    _X11TransGetConnectionNumber <-- Here
```

#### InitDisplayLock (xfree86-3.3/lib/X11/OpenDis.c:64)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
    _X11TransGetConnectionNumber
    InitDisplayLock <-- Here

NOTE: This function pointer is assigned on line 625 of
      xfree86-3.3/lib/X11/locking.c.

      This assignment takes place inside the XInitThreads
      function. I don't know where this is called, so I
      need to keep an eye out for it.

64: #define InitDisplayLock(d) (_XInitDisplayLock_fn ? (*_XInitDisplayLock_fn)(d) : Success)
```

#### \_XInitDisplayLock (xfree86-3.3/lib/X11/locking.c:546)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
    _X11TransGetConnectionNumber
    InitDisplayLock
    _XInitDisplayLock <-- Here
```

#### \_XPollfdCacheInit (xfree86-3.3/lib/X11/Xlibint.c:353)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
    _X11TransGetConnectionNumber
    InitDisplayLock
    _XInitDisplayLock
    _XPollfdCacheInit <-- Here
```

#### LockDisplay (xfree86-3.3/lib/X11/Xlibint.h:295)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
    _X11TransGetConnectionNumber
    InitDisplayLock
    _XInitDisplayLock
    _XPollfdCacheInit
    LockDisplay <-- Here
```

#### \_Xread (xfree86-3.3/lib/X11/XlibInt.c:1015)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
    _X11TransGetConnectionNumber
    InitDisplayLock
    _XInitDisplayLock
    _XPollfdCacheInit
    LockDisplay
    _Xread <-- Here
```

#### \_XVIDtoVisual (xfree86-3.3/lib/X11/XlibInt.c:2952)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
    _X11TransGetConnectionNumber
    InitDisplayLock
    _XInitDisplayLock
    _XPollfdCacheInit
    LockDisplay
    _Xread
    _XVIDtoVisual <-- Here
```

#### UnlockDisplay (xfree86-3.3/lib/X11/Xlibint.h:296)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
    _X11TransGetConnectionNumber
    InitDisplayLock
    _XInitDisplayLock
    _XPollfdCacheInit
    LockDisplay
    _Xread
    _XVIDtoVisual
    UnlockDisplay <-- Here
```

#### XCreateGC (xfree86-3.3/lib/X11/CrGc.c:65)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
    _X11TransGetConnectionNumber
    InitDisplayLock
    _XInitDisplayLock
    _XPollfdCacheInit
    LockDisplay
    _Xread
    _XVIDtoVisual
    UnlockDisplay
    XCreateGC <-- Here
```

#### XSynchronize (xfree86-3.3/lib/X11/Synchro.c:40)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
    _X11TransGetConnectionNumber
    InitDisplayLock
    _XInitDisplayLock
    _XPollfdCacheInit
    LockDisplay
    _Xread
    _XVIDtoVisual
    UnlockDisplay
    XCreateGC
    XSynchronize <-- Here
```

