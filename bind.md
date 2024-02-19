## Walkthrough of Linux 2.0.30's _bind_ System Call

### Code Walkthrough

#### sys\_socketcall (linux/net/socket.c:1237)

```txt
Control Flow:
sys_socketcall <-- Here

1248: Calls verify_area.

1256-1326: Routes the system call to the appropriate case via the
           call argument.

1261-1262: return(sys_bind(a0,(struct sockaddr *)a1,
                   get_user(args+2)));
```

#### verify\_area (linux/mm/memory.c:677)

```txt
Control Flow:
sys_socketcall
    verify_area <-- Here
```

#### sys\_bind (linux/net/socket.c:668)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_bind <-- Here

678: Calls sockfd_lookup.

681: Calls move_addr_to_kernel.

684: Calls the socket's bind method.

688: Returns zero.
```

#### sockfd\_lookup (linux/net/socket.c:219)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_bind
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
    sys_bind
        sockfd_lookup
            socki_lookup <-- Here

212: return &inode->u.socket_i;
```

#### move\_addr\_to\_kernel (linux/net/socket.c:132)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_bind
        sockfd_lookup
        move_addr_to_kernel <-- Here

139: Calls verify_area.

141: Calls memcpy_fromfs.

142: Returns zero.
```

#### inet\_bind (linux/net/ipv4/af\_inet.c:598)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_bind
        sockfd_lookup
        move_addr_to_kernel
        inet_bind <-- Here

621-622: Calls the inet socket protocol's good_socknum
         method.

626: Calls ip_chk_addr.

649: Calls the inet socket protocol's verify_bind
     method.

656: Calls the inet socket protocol's rehash
     method.

657: Calls add_to_prot_sklist.

659: Calls ip_rt_put.

661: Returns zero.
```

#### udp\_good\_socknum (linux/net/ipv4/udp.c:162)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_bind
        sockfd_lookup
        move_addr_to_kernel
        inet_bind
            udp_good_socknum <-- Here
```

#### ip\_chk\_addr (linux/net/ipv4/devinet.c:78)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_bind
        sockfd_lookup
        move_addr_to_kernel
        inet_bind
            udp_good_socknum
            ip_chk_addr <-- Here
```

#### udp\_v4\_verify\_bind (linux/net/ipv4/udp.c:123)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_bind
        sockfd_lookup
        move_addr_to_kernel
        inet_bind
            udp_good_socknum
            ip_chk_addr
            udp_v4_verify_bind <-- Here
```

#### udp\_v4\_rehash (linux/net/ipv4/udp.c:232)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_bind
        sockfd_lookup
        move_addr_to_kernel
        inet_bind
            udp_good_socknum
            ip_chk_addr
            udp_v4_verify_bind
            udp_v4_rehash <-- Here
```

#### add\_to\_prot\_sklist (linux/include/net/sock.h:431)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_bind
        sockfd_lookup
        move_addr_to_kernel
        inet_bind
            udp_good_socknum
            ip_chk_addr
            udp_v4_verify_bind
            udp_v4_rehash
            add_to_prot_sklist <-- Here
```

#### ip\_rt\_put (linux/include/net/route.h:123)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_bind
        sockfd_lookup
        move_addr_to_kernel
        inet_bind
            udp_good_socknum
            ip_chk_addr
            udp_v4_verify_bind
            udp_v4_rehash
            add_to_prot_sklist
            ip_rt_put <-- Here
```
