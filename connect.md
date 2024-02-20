## Walkthrough of Linux 2.0.30's _connect_ System Call

### Code Walkthrough

#### sys\_socketcall (linux/net/socket.c:1237)

```txt
Control Flow:
sys_socketcall <-- Here

1248: Calls verify_area.

1256-1326: Routes the system call to the appropriate case via the
           call argument.

1264-1265: return(sys_connect(a0, (struct sockaddr *)a1,
                      get_user(args+2)));
```

#### verify\_area (linux/mm/memory.c:677)

```txt
Control Flow:
sys_socketcall
    verify_area <-- Here
```

#### sys\_connect (linux/net/socket.c:639)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect <-- Here

802: Calls sockfd_lookup.

805: Calls move_addr_to_kernel.

830: Calls the socket's connect method.

835: Returns zero.
```

#### sockfd\_lookup (linux/net/socket.c:219)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup <-- Here

227: Assigns the file structure's inode to the local
     variable inode.

234: return socki_lookup(inode);
```

#### socki\_lookup (linux/net/socket.c:210)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
            socki_lookup <-- Here

212: return &inode->u.socket_i;
```

#### move\_addr\_to\_kernel (linux/net/socket.c:132)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel <-- Here

139: Calls verify_area.

141: Calls memcpy_fromfs.

142: Returns zero.
```

#### inet\_connect (linux/net/ipv4/af\_inet.c:669)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel
        inet_connect <-- Here

690: Calls inet_autobind.

692: Calls the inet socket protocol's connect method.

697: Assigns SS_CONNECTING to sock->state.

708: Calls cli.

710: Calls interruptible_sleep_on.

723: Calls sti.

724: Assigns SS_CONNECTED to sock->state.

730: Returns zero.
```

#### inet\_autobind (linux/net/ipv4/af\_inet.c:329)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel
        inet_connect
            inet_autobind <-- Here

333: Calls the inet socket protocol's good_socknum method.

337: Calls the inet socket protocol's rehash method.

338: Calls add_to_prot_sklist.

340: Returns zero.
```

#### tcp\_connect (linux/net/ipv4/tcp.c:2141)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel
        inet_connect
            inet_autobind
            tcp_connect <-- Here

I will do this after I've read Understanding Linux Network Internals.
```

#### udp\_connect (linux/net/ipv4/udp.c:839)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel
        inet_connect
            inet_autobind
            udp_connect <-- Here

848: Calls ip_my_addr.

850: Calls ip_chk_addr.

853: Calls ip_rt_route.

864: Returns zero.
```

#### ip\_my\_addr (linux/net/ipv4/devinet.c:181)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel
        inet_connect
            inet_autobind
            udp_connect
                ip_my_addr <-- Here
```

#### ip\_chk\_addr (linux/net/ipv4/devinet.c:78)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel
        inet_connect
            inet_autobind
            udp_connect
                ip_my_addr
                ip_chk_addr <-- Here
```

#### ip\_rt\_route (linux/include/net/route.h:136)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel
        inet_connect
            inet_autobind
            udp_connect
                ip_my_addr
                ip_chk_addr
                ip_rt_route <-- Here

141: Calls ip_rt_fast_lock.

143-153: I will analyze this in-depth later.

154: return ip_rt_slow_route (daddr, local);
```

#### ip\_rt\_slow\_route (linux/net/ipv4/route.c:1398)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel
        inet_connect
            inet_autobind
            udp_connect
                ip_my_addr
                ip_chk_addr
                ip_rt_route
                    ip_rt_slow_route <-- Here
```

#### cli (linux/include/asm-i386/system.h:225)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel
        inet_connect
            inet_autobind
            udp_connect
            cli <-- Here

225: #define cli() __asm__ __volatile__ ("cli": : :"memory")
```

#### interruptible\_sleep\_on (linux/kernel/sched.c:638)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel
        inet_connect
            inet_autobind
            udp_connect
            cli
            interruptible_sleep_on <-- Here

640: __sleep_on(p,TASK_INTERRUPTIBLE);
```

#### \_\_sleep\_on (linux/kernel/sched.c:618)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel
        inet_connect
            inet_autobind
            udp_connect
            cli
            interruptible_sleep_on
                __sleep_on <-- Here
```

#### sti (linux/include/asm-i386/system.h:224)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_connect
        sockfd_lookup
        move_addr_to_kernel
        inet_connect
            inet_autobind
            udp_connect
            cli
            interruptible_sleep_on
            sti <-- Here

224: #define sti() __asm__ __volatile__ ("sti": : :"memory")
```
