## Walkthrough of Debian 1.3's _ifconfig_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (net-tools-1.2.0/ifconfig.c:498)

```txt
Control Flow:
main <-- Here

509: Calls setlocale.

510: Calls catopen.

514: Calls sockets_open.

521-526: Handles program arguments.

537: Assigns the current program argument to local
     variable spp.

538: Calls strncpy to copy the spp to ifr.ifr_name.

547: Calls get_aftype.

811: Calls strcpy to copy the current program argument
     into local variable host.

812: Calls the address family's input method.

817: Calls memcpy to copy the socket's address into
     ifr.ifr_addr.

818: Calls sys_ioctl with the SIOCSIFADDR command.

827: Calls sys_close.

829: Calls NLS_CATCLOSE.

830: Returns local variable goterr.
```

#### sockets\_open (net-tools-1.2.0/ifconfig.c:475)

```txt
Control Flow:
main
    sockets_open <-- Here

477-480: Calls socket for multiple address families.

484-485: Returns global variable inet_sock.

494: Returns ddp_sock if none of the socket calls were
     successful.
```

#### get\_aftype (net-tools-1.2.0/lib/af.c:83)

```txt
Control Flow:
main
    sockets_open
    get_aftype <-- Here

87-88: Calls afinit if global static variable sVafinit
       is equal to zero.

91-94: Searches the aftypes array for an entry whose
       name field is equal to the name argument and
       returns it.
```

#### INET\_input (net-tools-1.2.0/lib/inet.c:192)

```txt
Control Flow:
main
    sockets_open
    get_aftype
    INET_input <-- Here

194: return(INET_resolve(bufp, (struct sockaddr_in *) sap));
```

#### INET\_resolve (net-tools-1.2.0/lib/inet.c:49)

```txt
Control Flow:
main
    sockets_open
    get_aftype
    INET_input
        INET_resolve <-- Here

65: Calls getnetbyname.

76: Calls gethostbyname.

82: Returns zero.
```

#### getnetbyname (lib-5.4.33/inet/getnetbynm.c:27)

```txt
Control Flow:
main
    sockets_open
    get_aftype
    INET_input
        INET_resolve
            getnetbyname <-- Here

87: Calls inet_network.

104: Returns the address of the global variable net.
```

#### inet\_network (libc-5.4.33./inet/inet\_net.c:47)

```txt
Control Flow:
main
    sockets_open
    get_aftype
    INET_input
        INET_resolve
            getnetbyname
                inet_network <-- Here
```

#### gethostbyname (libc-5.4.33/inet/gethstnmad.c:972)

```txt
Control Flow:
main
    sockets_open
    get_aftype
    INET_input
        INET_resolve
            getnetbyname
            gethostbyname <-- Here
```

#### sys\_ioctl (linux/fs/ioctl.c:58)

```txt
Control Flow:
main
    sockets_open
    get_aftype
    INET_input
    sys_ioctl <-- Here (SIOCSIFADDR)
```
