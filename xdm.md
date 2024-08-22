## Walkthrough of Debian 1.3's _xdm_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (xfree86-3.3/programs/xdm/dm.c:107)

```txt
Control Flow:
main <-- Here

115: Calls umask.

125: Calls InitResources.

126: Calls SetConfigFileTime.

127: Calls LoadDMResources.

131: Calls getuid.

137: Calls BecomeOrphan.

141: Calls BecomeDaemon.

143: Calls StorePid.

153: Calls InitErrorLog.

162: Calls system.

166: Calls init_session_id.

167: Calls CreateWellKnownSockets.

180: Calls SetAccessFileTime.

182: Calls ScanAccessDatabase.

184: Calls ScanServers.

185: Calls StartDisplays.

192: Calls AnyWellKnownSockets.

194: Calls AnyDisplaysLeft.

198: Calls RescanServers.

204: Calls WaitForSomething.

208: Calls exit.
```

#### umask (libc-5.4.33/sysdeps/linux/\_\_umask.S:21)

```txt
Control Flow:
main
    umask <-- Here

21: SYSCALL__ (umask, 1)
```

#### sys\_umask (linux/kernel/sys.c:929)

```txt
Control Flow:
main
    umask
        sys_umask <-- Here
```

#### InitResources (xfree86-3.3/programs/xdm/resource.c:414)

```txt
Control Flow:
main
    umask
    InitResources <-- Here

418: Calls XrmInitialize.

421: Calls ReinitResources.
```

#### XrmInitialize (xfree86-3.3/lib/X11/Xrm.c:324)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize <-- Here

326: Calls XrmPermStringToQuark.
```

#### XrmPermStringToQuark (xfree86-3.3/lib/X11/Quarks.c:382)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
            XrmPermStringToQuark <-- Here

398: return _XrmInternalStringToQuark(name, tname-(char *)name-1, sig, True);
```

#### \_XrmInternalStringToQuark (xfree86-3.3/lib/X11/Quarks.c:247)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
            XrmPermStringToQuark
                _XrmInternalStringToQuark <-- Here
```

#### ReinitResources (xfree86-3.3/programs/xdm/resource.c:424)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources <-- Here

437-438: Calls XrmDestroyDatabase.

439: Calls XrmGetStringDatabase.

441-443: Calls XrmParseCommand.
```

#### SetConfigFileTime ()

```txt
```

#### LoadDMResources ()

```txt
```

#### getuid (libc-5.4.33/sysdeps/linux/\_\_getuid.S:21)

```txt

21: SYSCALL__ (getuid, 0)
```

#### sys\_getuid (linux/kernel/sched.c:1283)

```txt
```

#### BecomeOrphan ()

```txt
```

#### BecomeDaemon ()

```txt
```

#### StorePid ()

```txt
```

#### InitErrorLog ()

```txt
```

#### system (tclx74-7.4a-p2/osSupport/system.c:35)

```txt
```

#### init\_session\_id ()

```txt
```

#### CreateWellKnownSockets ()

```txt
```

#### SetAccessFileTime ()

```txt
```

#### ScanAccessDatabase ()

```txt
```

#### ScanServers ()

```txt
```

#### StartDisplays ()

```txt
```

#### AnyWellKnownSockets ()

```txt
```

#### AnyDisplaysLeft ()

```txt
```

#### RescanServers ()

```txt
```

#### WaitForSomething ()

```txt
```

#### exit ()

```txt
```
