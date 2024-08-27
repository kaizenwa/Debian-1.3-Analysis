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

444-445: Calls GetResource.

446: Calls XrmGetFileDatabase.
```

#### XrmDestroyDatabase (xfree86-3.3/lib/X11/Xrm.c:2640)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase <-- Here
```

#### XrmGetStringDatabase (xfree86-3.3/lib/X11/Xrm.c:1555)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase <-- Here
```

#### XrmParseCommand (xfree86-3.3/lib/X11/ParseCmd.c:75)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand <-- Here
```

#### GetResource (xfree86-3.3/programs/xdm/resource.c:316)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource <-- Here

329: Calls XrmGetResource.
```

#### XrmGetResource (xfree86-3.3/lib/X11/Xrm.c:2563)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
                XrmGetResource <-- Here

2583: Calls XrmStringToNameList.

2584: Calls XrmStringToClassList.

2585: Calls XrmQGetResource.

2586: Calls XrmQuarkToString.
```

#### XrmStringToNameList (xfree86-3.3/lib/X11/Xresource.h:155)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
                XrmGetResource
                    XrmStringToNameList <-- Here

155: #define XrmStringToNameList(str, name) XrmStringToQuarkList(str, name)
```

#### XrmStringToQuarkList (xfree86-3.3/lib/X11/Xrm.c:350)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
                XrmGetResource
                    XrmStringToNameList
                        XrmStringToQuarkList <-- Here

370: Calls _XrmInternalStringToQuark.
```

#### \_XrmInternalStringToQuark (xfree86-3.3/lib/X11/Quarks.c:247)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
                XrmGetResource
                    XrmStringToNameList
                        XrmStringToQuarkList
                            _XrmInternalStringToQuark <-- Here
```

#### XrmStringToClassList (xfree86-3.3/lib/X11/Xresource.h:161)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
                XrmGetResource
                    XrmStringToNameList
                        XrmStringToQuarkList
                        XrmStringToClassList <-- Here

161: #define XrmStringToClassList(str,c_class) XrmStringToQuarkList(str, c_class)
```

#### XrmQGetResource (xfree86-3.3/lib/X11/xrm.c:2520)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
                XrmGetResource
                    XrmStringToNameList
                        XrmStringToQuarkList
                        XrmStringToClassList
                        XrmQGetResource <-- Here
```

#### XrmQuarkToString (xfree86-3.3/lib/X11/Quarks.c:414)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
                XrmGetResource
                    XrmStringToNameList
                        XrmStringToQuarkList
                        XrmStringToClassList
                        XrmQGetResource
                            XrmQuarkToString <-- Here
```

#### XrmGetFileDatabase (xfree86-3.3/lib/X11/Xrm.c:1649)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase <-- Here

1659: Calls ReadInFile.

1662: Calls NewDatabase.

1664: Calls GetDatabase.
```

#### ReadInFile (xfree86-3.3/lib/X11/Xrm.c:1578)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile <-- Here

1586: Calls OpenFile.

1589: Calls GetSizeOfFile.

1596: Calls ReadFile.

1612: Calls CloseFile.
```

#### OpenFile (xfree86-3.3/lib/X11/XrmI.h:45)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                    OpenFile <-- Here

45: #define OpenFile(name) open((name), O_RDONLY)
```

#### open (libc-5.4.33/sysdeps/linux/\_\_open.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                    OpenFile
                        open <-- Here

21: SYSCALL__ (open, 3)
```

#### sys\_open (linux/fs/open.c:574)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                    OpenFile
                        open
                            sys_open <-- Here
```

#### GetSizeOfFile (xfree86-3.3/lib/X11/XrmI.h:49)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                    OpenFile
                    GetSizeOfFile <-- Here
```
```asm
#define GetSizeOfFile(name,size)                    \
{                                                   \
    struct stat status_buffer;                      \
    if ( (stat((name), &status_buffer)) == -1 )     \
	size = -1;                                  \
    else                                            \
	size = status_buffer.st_size;               \
}
```

#### stat (libc-5.4.33/sbin/lib/\_\_stat.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                    OpenFile
                    GetSizeOfFile
                        stat <-- Here

21: SYSCALL__ (stat, 2)
```

#### sys\_stat (linux/fs/stat.c:110)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                    OpenFile
                    GetSizeOfFile
                        stat
                            sys_stat <-- Here
```

#### ReadFile (xfree86-3.3/lib/X11/XrmI.h:48)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                    OpenFile
                    GetSizeOfFile
                    ReadFile <-- Here

48: #define ReadFile(fd,buf,size) read((fd), (buf), (size))
```

#### read (libc-5.4.33/sysdeps/linux/\_\_read.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                    OpenFile
                    GetSizeOfFile
                    ReadFile
                        read <-- Here

21: SYSCALL__ (read, 3)
```

#### sys\_read (linux/fs/read\_write.c:104)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                    OpenFile
                    GetSizeOfFile
                    ReadFile
                        read
                            sys_read <-- Here
```

#### CloseFile (xfree86-3.3/lib/X11/XrmI.h:47)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                    OpenFile
                    GetSizeOfFile
                    ReadFile
                    CloseFile <-- Here

47: #define CloseFile(fd) close((fd))
```

#### close (libc-5.4.33/sysdeps/linux/\_\_close.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                    OpenFile
                    GetSizeOfFile
                    ReadFile
                    CloseFile
                        close <-- Here

21: SYSCALL__ (close, 1)
```

#### sys\_close (linux/fs/open.c:631)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                    OpenFile
                    GetSizeOfFile
                    ReadFile
                    CloseFile
                        close
                            sys_close <-- Here
```

#### NewDatabase (xfree86-3.3/lib/X11/Xrm.c:486)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                NewDatabase <-- Here
```

#### GetDatabase (xfree86-3.3/lib/X11/Xrm.c:1088)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
            XrmDestroyDatabase
            XrmGetStringDatabase
            XrmParseCommand
            GetResource
            XrmGetFileDatabase
                ReadInFile
                NewDatabase
                GetDatabase <-- Here
```

#### SetConfigFileTime (xfree86-3.3/programs/xdm/dm.c:290)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime <-- Here
```

#### LoadDMResources (xfree86-3.3/programs/xdm/resource.c:468)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources <-- Here
```

#### getuid (libc-5.4.33/sysdeps/linux/\_\_getuid.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid <-- Here

21: SYSCALL__ (getuid, 0)
```

#### sys\_getuid (linux/kernel/sched.c:1283)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
            sys_getuid <-- Here
```

#### BecomeOrphan (xfree86-3.3/programs/xdm/daemon.c:64)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan <-- Here

78: Calls fork.

99: Parent calls setpgrp.

108: Parent calls exit.
```

#### setpgrp (xfree86-3.3/programs/xdm/daemon.c:46)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
            setpgrp <-- Here (parent)

46: #define setpgrp setpgid
```

#### setpgid (libc-5.4.33/sysdeps/linux/setpgid.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
            setpgrp
                setpgid <-- Here (parent)

21: SYSCALL__ (setpgid, 2)
```

#### sys\_setpgid (linux/kernel/sys.c:563)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
            setpgrp
                setpgid
                    sys_setpgid <-- Here (parent)
```

#### exit (libc-5.4.33/gcc/libgcc2.c:2104)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
            setpgrp
            exit <-- Here (parent)
```

#### BecomeDaemon (xfree86-3.3/programs/xdm/daemon.c:112)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon <-- Here

126: Calls getpid and setpgrp.

129-131: Calls close on STDIN, STDOUT, and STDERR.

145: Calls open on "/dev/tty".

154: Calls ioctl with the TIOCNOTTY argument.

166: Calls open.

167-168: Calls dup2 to create STDOUT and STDERR.
```

#### getpid (libc-5.4.33/sysdeps/linux/\_\_getpid.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
            getpid <-- Here

21: SYSCALL__ (getpid, 0)
```

#### sys\_getpid (linux/kernel/sched.c:1273)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
            getpid
                sys_getpid <-- Here
```

#### ioctl (libc-5.4.33/sysdeps/linux/\_\_ioctl.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
            getpid
            ioctl <-- Here

21: SYSCALL__ (ioctl, 3)
```

#### sys\_ioctl (linux/fs/ioctl.c:58)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
            getpid
            ioctl
                sys_ioctl <-- Here

103-104: Calls tty_ioctl with TIOCNOTTY argument.
```

#### tty\_ioctl (linux/drivers/char/tty\_io.c:1403)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
            getpid
            ioctl
                sys_ioctl
                    tty_ioctl <-- Here (TIONOCTTY)

1490-1491: Calls disassociate_ctty.
```

#### disassociate\_ctty (linux/drivers/char/tty\_io.c:475)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
            getpid
            ioctl
                sys_ioctl
                    tty_ioctl
                        disassociate_ctty <-- Here
```

#### StorePid (xfree86-3.3/programs/xdm/dm.c:738)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid <-- Here
```

#### InitErrorLog (xfree86-3.3/programs/xdm/error.c:184)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog <-- Here

188: Calls creat.

191: Calls dup2.

192: Calls close.
```

#### system (tclx74-7.4a-p2/osSupport/system.c:35)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system <-- Here
```

#### init\_session\_id (xfree86-3.3/programs/xdm/xdmcp.c:730)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id <-- Here
```

#### CreateWellKnownSockets (xfree86-3.3/programs/xdm/socket.c:84)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets <-- Here

95: Calls localHostname.

106: Calls socket.

112: Calls localHostname.

113: Calls registerHostname.

114: Calls RegisterCloseOnFork.

153: Calls bind.

170: Calls socket.

207: Calls listen.
```

#### localHostname (xfree86-3.3/programs/xdm/util.c:256)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname <-- Here

260: Calls XmuGetHostname.
```

#### XmuGetHostname (xfree86-3.3/lib/Xmu/GetHost.c:54)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
                XmuGetHostname <-- Here

72: Calls uname.
```

#### uname (libc-5.4.33/sysdeps/linux/\_\_uname.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
                XmuGetHostname
                    uname <-- Here
```

#### sys\_uname (linux/kernel/sys.c:737)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
                XmuGetHostname
                    uname
                        sys_uname <-- Here
```

#### socket (libc-5.4.33/sysdeps/linux/socket.c:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
            socket <-- Here

28: return socketcall(SYS_SOCKET, args);
```

#### socketcall (libc-5.4.33/sysdeps/linux/\_\_socketcall.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
            socket
                socketcall <-- Here

21: SYSCALL__ (socketcall, 2)
```

#### sys\_socketcall (linux/net/socket.c:1237)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
            socket
                socketcall
                    sys_socketcall <-- Here
```

#### registerHostname (xfree86-3.3/programs/xdm/xdmcp.c:416)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
            socket
            registerHostname <-- Here

422: Calls XdmcpReallocARRAYS.
```

#### XdmcpReallocARRAYS (xfree86-3.3/lib/Xdmcp/RaA8.c:37)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
            socket
            registerHostname
                XdmcpReallocARRAYS <-- Here

43: Calls Xrealloc.
```

#### Xrealloc (xfree86-3.3/Xdmcp/Alloc.c:46)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
            socket
            registerHostname
                XdmcpReallocARRAYS
                    Xrealloc <-- Here

54: return (unsigned long *) realloc ((char *) old, amount);
```

#### realloc (libc-5.4.33/malloc/realloc.c:29)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
            socket
            registerHostname
                XdmcpReallocARRAYS
                    Xrealloc
                        realloc <-- Here

29: #pragma weak realloc = __libc_realloc
```

#### \_\_libc\_realloc (libc-5.4.33/malloc/realloc.c:44)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
            socket
            registerHostname
                XdmcpReallocARRAYS
                    Xrealloc
                        realloc
                            __libc_realloc <-- Here
```

#### RegisterCloseOnFork (xfree86-3.3/programs/xdm/dm.c:696)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
            socket
            registerHostname
            RegisterCloseOnFork <-- Here
```

#### bind (libc-5.4.33/sysdeps/linux/bind.c:22)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
            socket
            registerHostname
            RegisterCloseOnFork
            bind <-- Here

29: return socketcall(SYS_BIND, args);
```

#### listen (libc-5.4.33/sysdeps/linux/listen.c:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
            localHostname
            socket
            registerHostname
            RegisterCloseOnFork
            bind
            listen <-- Here

27: return socketcall(SYS_LISTEN, args);
```

#### SetAccessFileTime (xfree86-3.3/programs/xdm/dm.c:298)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime <-- Here
```

#### ScanAccessDatabase (xfree86-3.3/programs/xdm/access.c:427)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase <-- Here

431: Calls FreeAccessDatabase.

434: Calls fopen.

440: Calls ReadAccessDatabase.

441: Calls fclose.
```

#### FreeAccessDatabase (xfree86-3.3/programs/xdm/access.c:160)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
            FreeAccessDatabase <-- Here

167: Calls FreeDisplayEntry.
```

#### FreeDisplayEntry (xfree86-3.3/programs/xdm/access.c:137)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
            FreeAccessDatabase
                FreeDisplayEntry <-- Here

148-149: Calls XdmcpDisposeARRAYS.

154: Calls FreeHostEntry.

156: Calls free.
```

#### XdmcpDisposeARRAY8 (xfree86-3.3/lib/Xdmcp/DA8.c:37)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
            FreeAccessDatabase
                FreeDisplayEntry
                    XdmcpDisposeARRAY8 <-- Here

40: Calls Xfree.
```

#### Xfree (xfree86-3.3/lib/Xdmcp/Alloc.c:58)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
            FreeAccessDatabase
                FreeDisplayEntry
                    XdmcpDisposeARRAY8
                        Xfree <-- Here

61-62: Calls free.
```

#### free (libc-5.4.33/dl-malloc/malloc.c:858)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
            FreeAccessDatabase
                FreeDisplayEntry
                    XdmcpDisposeARRAY8
                        Xfree
                            free <-- Here

858: #pragma weak free = __libc_free
```

#### \_\_libc\_free (libc-5.4.33/malloc/free.c:202)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
            FreeAccessDatabase
                FreeDisplayEntry
                    XdmcpDisposeARRAYS
                        Xfree
                            free
                                __libc_free <-- Here
```

#### FreeHostEntry (xfree86-3.3/programs/xdm/access.c:160)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
            FreeAccessDatabase
                FreeDisplayEntry
                    XdmcpDisposeARRAY8
                    FreeHostEntry <-- Here

133: Calls free.
```

#### ReadAccessDatabase (xfree86-3.3/programs/xdm/access.c:427)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
            FreeAccessDatabase
            ReadAccessDatabase <-- Here

419: Calls ReadDisplayEntry.
```

#### ReadDisplayEntry (xfree86-3.3/programs/xdm/access.c:309)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
            FreeAccessDatabase
            ReadAccessDatabase
                ReadDisplayEntry <-- Here
```

#### ScanServers (xfree86-3.3/programs/xdm/dm.c:225)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers <-- Here

240: Calls fopen.

248: Calls fstat.

251: Calls fgets.

256: Calls ParseDisplay.

258: Calls fclose.
```

#### fstat (libc4-4.6.27/sysdeps/linux/\_\_fstat.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
            fstat <-- Here

21: SYSCALL__ (fstat, 2)
```

#### sys\_fstat (linux/fs/stat.c:189)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
            fstat
                sys_fstat <-- Here
```

#### ParseDisplays (xfree86-3.3/programs/xdm/file.c:144)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
            fstat
            ParseDisplays <-- Here
```

#### StartDisplays (xfree86-3.3/programs/xdm/dm.c:569)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays <-- Here

571: Calls ForEachDisplay with CheckDisplayStatus as the argument.
```

#### ForEachDisplay (xfree86-3.3/programs/xdm/dm.c:48)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay <-- Here

55: Calls CheckDisplayStatus.
```

#### CheckDisplayStatus (xfree86-3.3/programs/xdm/dm.c:549)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus <-- Here

562: Calls StartDisplay.
```

#### StartDisplay (xfree86-3.3/programs/xdm/dm.c:575)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay <-- Here

581: Calls LoadServerResources.

592: Calls SetLocalAuthorizatoin.

602: Calls StartServer.

616: Calls fork.

626: Child calls LoadSessionResources.

627: Child calls SetAuthorization.

628: Child calls WaitForServer.

632: Child calls RunChooser?

635: Child calls ManageSession?
```

#### LoadServerResources (xfree86-3.3/programs/xdm/resource.c:526)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources <-- Here
```

#### SetLocalAuthorization (xfree86-3.3/programs/xdm/auth.c:424)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization <-- Here
```

#### StartServer (xfree86-3.3/programs/xdm/server.c:136)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer <-- Here
```

#### LoadSessionResources (xfree86-3.3/programs/xdm/resource.c:532)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources <-- Here

535: Calls LoadDisplayResources.
```

#### LoadDisplayResources (xfree86-3.3/programs/xdm/resource.c:504)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                            LoadDisplayResources <-- Here
```

#### SetAuthorization (xfree86-3.3/programs/xdm/dm.c:482)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        SetAuthorization <-- Here
```

#### WaitForServer (xfree86-3.3/programs/xdm/server.c:331)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer <-- Here

337: Calls Signal.

338: Calls alarm.

339: Calls Setjmp.

342: Calls XSetIOErrorHandler with openErrorHandler as the argument.

343: Calls XOpenDisplay.

355: Calls alarm.

356: Calls Signal.

357: Calls XSetIOErrorHandler with NULL as the argument.

362: Calls GetRemoteAddress.

364: Calls RegisterCloseOnFork.

365: Calls fcntl.
```

#### Signal (xfree86-3.3/util/patch/common.h:22)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal <-- Here

22: #define Signal (void)signal
```

#### signal (libc-5.4.33/sysdeps/linux/signal.c:4)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                                signal <-- Here

12: Calls __sigaction.
```

#### \_\_sigaction (libc-5.4.33/sysdeps/linux/i386/\_\_sigact.c:46)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                                signal
                                    __sigaction <-- Here
```
```c
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%edi,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
		:"=a" (sig)
		:"0" (SYS_sigaction),"D" (sig),"c" (new),"d" (old));
```

#### sys\_sigaction (linux/kernel/signal.c:150)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                                signal
                                    __sigaction
                                        sigaction <-- Here
```

#### alarm (libc-5.4.33/sysdeps/linux/alarm.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                            alarm <-- Here

21: SYSCALL__ (alarm, 1)
```

#### sys\_alarm (linux/kernel/sched.c:1252)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                            alarm
                                sys_alarm <-- Here
```

#### Setjmp (xfree86-3.3/programs/xdm/dm.h:372)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                            alarm
                            Setjmp <-- Here

#define Setjmp(e)   sigsetjmp(e,1)
```

#### sigsetjmp (libc-5.4.33/sysdeps/i386/setjmp/sigsetjmp.c:27)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                            alarm
                            Setjmp
                                sigsetjmp <-- Here

30-31: Calls sigprocmask.
```

#### sigprocmask (libc-5.4.33/sysdeps/linux/\_\_sigproc.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                            alarm
                            Setjmp
                                sigsetjmp
                                    sigprocmask <-- Here

21: SYSCALL__ (sigprocmask, 3)
```

#### sys\_sigprocmask (linux/kernel/signal.c:29)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                            alarm
                            Setjmp
                                sigsetjmp
                                    sigprocmask
                                        sys_sigprocmask <-- Here
```

#### XSetIOErrorHandler (xfree86-3.3/lib/X11/ErrHndlr.c:73)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                            alarm
                            Setjmp
                            XSetIoErrorHandler <-- Here
```

#### XOpenDisplay (xfree86-3.3/lib/X11/OpenDis.c:86)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                            alarm
                            Setjmp
                            XSetIoErrorHandler
                            XOpenDisplay <-- Here
```

#### GetRemoteAddress (xfree86-3.3/programs/xdm/server.c:265)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                            alarm
                            Setjmp
                            XSetIoErrorHandler
                            XOpenDisplay
                            GetRemoteAddress <-- Here
```

#### RegisterCloseOnFork (xfree86-3.3/programs/xdm/dm.c:696)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                            alarm
                            Setjmp
                            XSetIoErrorHandler
                            XOpenDisplay
                            GetRemoteAddress
                            RegisterCloseOnFork <-- Here
```

#### fcntl (libc-5.4.33/sysdeps/linux/\_\_fcntl.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                            alarm
                            Setjmp
                            XSetIoErrorHandler
                            XOpenDisplay
                            GetRemoteAddress
                            RegisterCloseOnFork
                            fcntl <-- Here

21: SYSCALL__ (fcntl, 3)
```

#### sys\_fcntl (linux/fs/fcntl.c:55)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                            Signal
                            alarm
                            Setjmp
                            XSetIoErrorHandler
                            XOpenDisplay
                            GetRemoteAddress
                            RegisterCloseOnFork
                            fcntl
                                sys_fcntl <-- Here
```

#### RunChooser (xfree86-3.3/programs/xdm/choose.c:578)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        RunChooser <-- Here?
```

#### ManageSession (xfree86-3.3/programs/xdm/sessions.c:246)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession <-- Here?

256: Calls XSetIOErrorHandler.

257: Calls XSetErrorHandler.

258: Calls SetTitle.

262: Calls LoadXloginResources.

283: Calls GreetUser.

288: Calls Setjmp.

289: Calls Signal.

294: Calls StartClient.

302: Calls Setjmp.

304: Calls Signal.

305: Calls alarm.

306: Calls wait.

331: Calls AbortClient.

338: Calls Source.

339: Calls SessionExit.
```

#### XSetErrorHandler (xfree86-3.3/lib/X11/ErrHndlr.c:40)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler <-- Here
```

#### SetTitle (xfree86-3.3/programs/xdm/dm.c:822)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle <-- Here
```

#### LoadXloginResources (xfree86-3.3/programs/xdm/sessions.c:342)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle
                            LoadXloginResources <-- Here 
```

#### GreetUser (xfree86-3.3/programs/xdm/greeter/greet.c:279)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle
                            LoadXloginResources
                            GreetUser <-- Here

321: Calls InitGreet.

...
```

### InitGreet (xfree86-3.3/programs/xdm/greeter/greet.c:153)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle
                            LoadXloginResources
                            GreetUser
                                InitGreet <-- Here

165: Calls XtToolkitInitialize.

...
```

#### XToolkitInitialize (xfree86-3.3/lib/Xt/Initialize.c:203)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle
                            LoadXloginResources
                            GreetUser
                                InitGreet
                                    XtToolkitInitialize <-- Here
```

#### StartClient (xfree86-3.3/programs/xdm/session.c:487)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle
                            LoadXloginResources
                            GreetUser
                            StartClient <-- Here
```

#### wait (libc-5.4.33/sysdeps/linux/\_\_wait.c:20)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle
                            LoadXloginResources
                            GreetUser
                            StartClient
                                wait <-- Here

20: weak_alias (__wait, wait);
```

#### \_\_wait (libc-5.4.33/sysdeps/linux/\_\_wait.c:13)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle
                            LoadXloginResources
                            GreetUser
                            StartClient
                                wait
                                    __wait <-- Here

15: return wait4(WAIT_ANY, wait_stat, 0, NULL);
```

#### wait4 (libc-5.4.33/sysdeps/linux/\_\_wait4.S:21)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle
                            LoadXloginResources
                            GreetUser
                            StartClient
                                wait
                                    __wait
                                        wait4 <-- Here

21: SYSCALL__ (wait4, 4)
```

#### sys\_wait4 (linux/kernel/exit.c:612)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle
                            LoadXloginResources
                            GreetUser
                            StartClient
                                wait
                                    __wait
                                        wait4
                                            sys_wait4 <-- Here
```

#### AbortClient (xfree86-3.3/programs/xdm/sessions.c:173)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle
                            LoadXloginResources
                            GreetUser
                            StartClient
                            wait
                            AbortClient <-- Here
```

#### source (xfree86-3.3/programs/xdm/session.c:675)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle
                            LoadXloginResources
                            GreetUser
                            StartClient
                            wait
                            AbortClient
                            source <-- Here
```

#### SessionExit (xfree86-3.3/programs/xdm/sessions.c:444)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
            ForEachDisplay
                CheckDisplayStatus
                    StartDisplay
                        LoadServerResources
                        SetLocalAuthorization
                        StartServer
                        LoadSessionResources
                        WaitForServer
                        ManageSession
                            XSetErrorHandler
                            SetTitle
                            LoadXloginResources
                            GreetUser
                            StartClient
                            wait
                            AbortClient
                            source
                            SessionExit <-- Here
```

#### AnyWellKnownSockets (xfree86-3.3/programs/xdm/xdmcp.c:122)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
        AnyWellKnownSockets <-- Here
```

#### AnyDisplaysLeft (xfree86-3.3/programs/xdm/dpylist.c:43)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
        AnyWellKnownSockets
        AnyDisplaysLeft <-- Here
```

#### RescanServers (xfree86-3.3/programs/xdm/dm.c:274)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
        AnyWellKnownSockets
        AnyDisplaysLeft
        RescanServers <-- Here
```

#### WaitForSomething (xfree86-3.3/programs/xdm/xdmcp.c:367)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
        AnyWellKnownSockets
        AnyDisplaysLeft
        RescanServers
        WaitForSomething <-- Here

374: Calls AnyWellKnownSockets.

381: Calls select.

392: Calls ProcessRequestSocket.

...
```

#### select (libc-5.4.33/sysdeps/linux/i386/\_\_select.c:60)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
        AnyWellKnownSockets
        AnyDisplaysLeft
        RescanServers
        WaitForSomething
            select <-- Here

60: weak_alias (__select, select);
```

#### \_\_select (libc-5.4.33/sysdeps/linux/i386/\_\_select.c:36)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
        AnyWellKnownSockets
        AnyDisplaysLeft
        RescanServers
        WaitForSomething
            select
                __select <-- Here
```
```c
    long __res;
#if defined(__PIC__) || defined (__pic__)
    __asm__ volatile ("pushl %%ebx\n\t"
              "movl %%ecx,%%ebx\n\t"
              "int $0x80\n\t"
              "popl %%ebx"
        : "=a" (__res)
        : "0" (SYS_select),"c" ((long) &nd));
```

#### sys\_select (linux/fs/select.c:240)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
        AnyWellKnownSockets
        AnyDisplaysLeft
        RescanServers
        WaitForSomething
            select
                __select
                    sys_select <-- Here
```

#### ProcessRequestSocket (xfree86-3.3/programs/xdm/xdmcp.c:287)

```txt
Control Flow:
main
    umask
    InitResources
        XrmInitialize
        ReinitResources
        SetConfigFileTime
        LoadDMResources
        getuid
        BecomeOrphan
        BecomeDaemon
        StorePid
        InitErrorLog
        system
        init_session_id
        CreateWellKnownSockets
        SetAccessFileTime
        ScanAccessDatabase
        ScanServers
        StartDisplays
        AnyWellKnownSockets
        AnyDisplaysLeft
        RescanServers
        WaitForSomething
            select
            ProcessRequestSocket <-- Here
```

