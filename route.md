## Walkthrough of Debian 1.3's _route_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (netbase-2.13/net-tools-1.2.0/route.c:463)

```txt
Control Flow:
main <-- Here

469: Calls setlocale.

470: Calls catopen.

475-492: Handles program arguments.

505: Calls sys_socket.

511-512: Calls rt_add.

517: Calls sys_close.

519: Calls NLS_CATCLOSE.
```

#### rt\_add (netbase-2.13/net-tools-1.2.0/route.c:271)

```txt
Control Flow:
main
    sys_socket
    rt_add <-- Here

277: Initializes local variable xflag to zero.

280-282: Assigns one to xflag for -net option and increments
         args.

289: Calls strcpy to copy the argument to local character array
     target.

293: Calls resolve.

298-300: Assigns one to local variable isnet.

402: Assigns the program argument to the rt.rt_dev field.

403: Increments args.

426: Calls sys_ioctl with the SIOCADDRT command.
```

#### resolve (netbase-2.13/net-tools-1.2.0/route.c:82)

```txt
Control Flow:
main
    sys_socket
    rt_add
        resolve <-- Here

99: Calls getnetbyname.

109: Calls gethostbyname.

115: Returns zero.
```

#### getnetbyname (lib-5.4.33/inet/getnetbynm.c:27)

```txt
Control Flow:
main
    sys_socket
    rt_add
        resolve
            getnetbyname <-- Here

32: Calls setnetent.

33: Calls getnetent.

43: Returns local variable p.
```

#### setnetent (libc-5.4.33/inet/getnetent.c:)

```txt
Control Flow:
main
    sys_socket
    rt_add
        resolve
            getnetbyname
                setnetent <-- Here

42-43: Calls fopen on "/etc/networks" if the global
       variable netf is NULL.

44-45: Calls rewind.

46: ORs the f argument into global variable _net_stayopen.
```

#### getnetent (libc-5.4.33/inet/getnetent.c:60)

```txt
Control Flow:
main
    sys_socket
    rt_add
        resolve
            getnetbyname
                setnetent
                getnetent <-- Here

87: Calls inet_network.

104: Returns the address of the global variable net.
```

#### inet\_network (libc-5.4.33./inet/inet\_net.c:47)

```txt
Control Flow:
main
    sys_socket
    rt_add
        resolve
            getnetbyname
                setnetent
                getnetent
                    inet_network <-- Here
```

#### gethostbyname (libc-5.4.33/inet/gethstnmad.c:972)

```txt
Control Flow:
main
    sys_socket
    rt_add
        resolve
            getnetbyname
            gethostbyname <-- Here
```

#### sys\_ioctl (linux/fs/ioctl.c:58)

```txt
Control Flow:
main
    sys_socket
    rt_add
        resolve
        sys_ioctl <-- Here (SIOCADDRT)

103-104: Calls sock_ioctl.
```

#### sock\_ioctl (linux/net/socket.c:396)

```txt
Control Flow:
main
    sys_socket
    rt_add
        resolve
        sys_ioctl
            sock_ioctl <-- Here

400: Calls socki_lookup.

401: return(sock->ops->ioctl(sock, cmd, arg));
```

#### socki\_lookup (linux/net/socket.c:210)

```txt
Control Flow:
main
    sys_socket
    rt_add
        resolve
        sys_ioctl
            sock_ioctl
                socki_lookup <-- Here

212: return &inode->u.socket_i;
```


#### ip\_rt\_ioctl (linux/net/ipv4/route.c:1691)

```txt
Control Flow:
main
    sys_socket
    rt_add
        resolve
        sys_ioctl
            sock_ioctl
                socki_lookup
                ip_rt_ioctl <-- Here

1700: Calls suser.

1702: Calls verify_area.

1705: Calls memcpy_fromfs.

1706: Calls ip_rt_new and returns.
```

#### ip\_rt\_new (linux/net/ipv4/route.c:1537)

```txt
Control Flow:
main
    sys_socket
    rt_add
        resolve
        sys_ioctl
            sock_ioctl
                socki_lookup
                ip_rt_ioctl
                    ip_rt_new <-- Here

1552: Calls getname.

1555: Calls dev_get.

1556: Calls putname.

1647: Calls rt_add.

1648: Returns zero.
```

#### dev\_get (linux/net/core/dev.c:208)

```txt
Control Flow:
main
    sys_socket
    rt_add
        resolve
        sys_ioctl
            sock_ioctl
                socki_lookup
                ip_rt_ioctl
                    ip_rt_new
                        dev_get <-- Here
```
