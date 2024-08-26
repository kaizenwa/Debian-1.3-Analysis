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

316: Calls _X11TransOpenCOTSClient.

320: Calls _X11TransConnect.

334: Calls _X11TransGetPeerAddr.

342: Calls _X11TransConvertAddress.

367: Calls _X11TransSetOption.

391-392: Calls GetAuthorization.
```

#### \_X11TransOpenCOTSClient (xfree86-3.3/lib/xtrans/Xtrans.c:572)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay <-- Here

578: return TRANS(Open) (XTRANS_OPEN_COTS_CLIENT, address);
```

#### \_X11TransOpen (xfree86-3.3/lib/xtrans/Xtrans.c:200)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen <-- Here

415: Calls _X11TransParseAddress.

423: Calls _X11TransSelectTransport.

445: Calls _X11TransSocketOpenCOTSServer.
```

#### \_X11TransParseAddress (xfree86-3.3/lib/xtrans/Xtrans.c:200)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
            _X11TransParseAddress <-- Here
```

#### \_X11TransSelectTransport (xfree86-3.3/lib/xtrans/Xtrans.c:164)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
            _X11TransParseAddress
            _X11TransSelectTransport <-- Here
```

#### \_X11TransSocketOpenCOTSServer (xfree86-3.3/lib/xtrans/Xtranssock.c:503)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
            _X11TransParseAddress
            _X11TransSelectTransport
            _X11TransSocketOpenCOTSServer <-- Here

516: This is a no-op for Debian.

518: Calls _X11TransSocketSelectFamily.

526-527: Calls _X11TransSocketOpen.
```

#### \_X11TransSocketOpen (xfree86-3.3/lib/xtrans/Xtranssock.c:372)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
            _X11TransParseAddress
            _X11TransSelectTransport
            _X11TransSocketOpenCOTSServer
                _X11TransSocketOpen <-- Here

382-383: Calls xcalloc.

389-390: Calls socket.

412-413: Calls setsockopt.
```

#### socket ()

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
            _X11TransParseAddress
            _X11TransSelectTransport
            _X11TransSocketOpenCOTSServer
                _X11TransSocketOpen
                    socket <-- Here
```

#### setsockopt (libc-5.4.33/sysdeps/linux/setsockopt.c:23)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
            _X11TransParseAddress
            _X11TransSelectTransport
            _X11TransSocketOpenCOTSServer
                _X11TransSocketOpen
                    socket
                    setsockopt <-- Here

32: return (socketcall (SYS_SETSOCKOPT, args));
```

#### \_X11TransConnect (xfree86-3.3/lib/xtrans/Xtrans.c:844)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
        _X11TransConnect <-- Here

857: Calls _X11TransParseAddress.

873: Calls _X11TransSocketUNIXConnect.
```

#### \_X11TransSocketUNIXConnect (xfree86-3.3/lib/xtrans/Xtranssock.c:1514)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
        _X11TransConnect
            _X11TransSocketUNIXConnect <-- Here

1599: Calls connect.
```


#### connect ()

```txt
```

#### \_X11TransGetPeerAddr (xfree86-3.3/lib/xtrans/Xtrans.c:1016)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
        _X11TransConnect
        _X11TransGetPeerAddr <-- Here
```

#### \_X11TransConvertAddress (xfree86-3.3/lib/xtrans/Xtransutil.c:91)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
        _X11TransConnect
        _X11TransGetPeerAddr
        _X11TransConvertAddress <-- Here

192: Calls _X11TransGetHostname
```

#### \_X11TransGetHostname (xfree86-3.3/xtrans/Xtrans.c:1431)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
        _X11TransConnect
        _X11TransGetPeerAddr
        _X11TransConvertAddress
            _X11TransGetHostname <-- Here

1449: Calls gethostname.
```

#### gethostname ()

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
        _X11TransConnect
        _X11TransGetPeerAddr
        _X11TransConvertAddress
            _X11TransGetHostname
                gethostname <-- Here
```

#### \_X11TransSetOption (xfree86-3.3/lib/xtrans/Xtrans.c:688)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
        _X11TransConnect
        _X11TransGetPeerAddr
        _X11TransConvertAddress
        _X11TransSetOption <-- Here
```

#### GetAuthorization (xfree86-3.3/lib/X11/ConnDis.c:944)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
        _X11TransConnect
        _X11TransGetPeerAddr
        _X11TransConvertAddress
        _X11TransSetOption
        GetAuthorization <-- Here

1014: Calls _X11TransGetMyAddr.

1050: Calls getpid.

1088-1089: Calls XdmcpWrap.

1144: Calls XauDisposeAuth.
```

#### \_X11TransGetMyAddr (xfree86-3.3/lib/xtrans/Xtrans.c:992)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
        _X11TransConnect
        _X11TransGetPeerAddr
        _X11TransConvertAddress
        _X11TransSetOption
        GetAuthorization
            _X11TransGetMyAddr <-- Here
```

#### XdmcWrap (xfree86-3.3/lib/Xdmcp/Wrap.c:50)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
        _X11TransConnect
        _X11TransGetPeerAddr
        _X11TransConvertAddress
        _X11TransSetOption
        GetAuthorization
            _X11TransGetMyAddr
            XdmcWrap <-- Here
```

#### XauDisposeAuth (xfree86-3.3/lib/Xau/AuDispose.c:33)

```txt
Control Flow:
XOpenDisplay
    _XSendClientPrefix
    _X11TransConnectDisplay
    _XAllocID
    _XAllocIDS
    Xcalloc
    _X11TransConnectDisplay
        _X11TransOpen
        _X11TransConnect
        _X11TransGetPeerAddr
        _X11TransConvertAddress
        _X11TransSetOption
        GetAuthorization
            _X11TransGetMyAddr
            XdmcWrap
            XauDisposeAuth <-- Here
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

