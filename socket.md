## Walkthrough of Linux 2.0.30's _socket_ System Call

### Code Walkthrough

#### sys\_socketcall (linux/net/socket.c:1237)

```txt
Control Flow:
sys_socketcall <-- Here

1248: Calls verify_area.

1256-1326: Routes the system call to the appropriate case via the
           call argument.

1259: return(sys_socket(a0,a1,get_user(args+2)));
```

#### verify\_area (linux/mm/memory.c:677)

```txt
Control Flow:
sys_socketcall
    verify_area <-- Here
```

#### sys\_socket (linux/net/socket.c:529)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket <-- Here

536: Calls find_protocol_family.

554: Assigns the appropriate entry of the static array pops to
     the local variable ops.

     NOTE: pops is defined on line 119 of this source file.

573: Calls sock_alloc.

582: Calls the socket's create method.

588: Calls get_fd.

594: Assigns the socket's file structure to the sock->file field.

595: Returns local variable fd.
```

#### find\_protocol\_family (linux/net/socket.c:516)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket
        find_protocol_family <-- Here

519-525: Searches the static array pops for an entry
         with a family field that matches the family
         argument.

526: Returns negative one if we could not find a
     matching entry.
```


#### sock\_alloc (linux/net/socket.c:241)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket
        find_protocol_family
        sock_alloc <-- Here

246: Calls get_empty_inode.

250-253: Initializes the inode.

255-267: Initializes the inode's socket field.

268: Returns the local variable sock.
```

#### get\_empty\_inode (linux/fs/inode.c:480)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket
        find_protocol_family
        sock_alloc
            get_empty_inode <-- Here
```

#### inet\_create (linux/net/ipv4/af\_inet.c:411)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket
        find_protocol_family
        sock_alloc
        inet_create <-- Here

416: Calls sk_alloc.

429: Assigns TCP_NO_CHECK to sk->no_check.

430: Assigns tcp_prot to local variable prot.

     NOTE: tcp_prot is defined on lines 2370-2401
           of linux/net/ipv4/tcp.c.

435: Assigns UDP_NO_CHECK to sk->no_check.

436: Assigns udp_prot to local variable prot.

     NOTE: udp_prot is defined on lines 1093-1124
           of linux/net/ipv4/udp.c.

450: Assigns the sock argument to sk->socket.

454-517: Initialize the sk structure.

480: Assigns sk to sock->data.

519-520: Calls the inet socket's protocol init method.

         NOTE: This method is NULL for TCP and UDP.

526: Returns zero.
```

#### get\_fd (linux/net/socket.c:172)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket
        find_protocol_family
        sock_alloc
        inet_create
        get_fd <-- Here

180: Calls get_unused_fd.

182: Calls get_empty_filp.

189: Inserts the empty file structure into the appropriate
     entry of the files structure.

190-197: Initializes the file structure.

195-196: Increments the inode argument's i_count field.

199: Returns local variable fd.
```

#### get\_unused\_fd (linux/fs/open.c:555)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket
        find_protocol_family
        sock_alloc
        inet_create
        get_fd
            get_unused_fd <-- Here

560: Calls find_first_zero_bit on the files structure's
     open_fds bitmap.

562: Calls FD_SET on the files structure's open_fds bitmap.

563: Calls FD_CLR on the files structure's close_on_exec
     bitmap.

564: Returns local variable fd.

566: Returns -EMFILE if we could not obtain an empty
     file descriptor.
```

#### find\_first\_zero\_bit (linux/include/asm-i386/bitops.h:75)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket
        find_protocol_family
        sock_alloc
        inet_create
        get_fd
            get_unused_fd
                find_first_zero_bit <-- Here
```

#### FD\_SET (linux/include/linux/time.h:30)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket
        find_protocol_family
        sock_alloc
        inet_create
        get_fd
            get_unused_fd
                find_first_zero_bit
                    FD_SET <-- Here

30: #define FD_SET(fd,fdsetp)	__FD_SET(fd,fdsetp)
```

#### \_\_FD\_SET (linux/include/asm-i386/posix\_types.h:39)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket
        find_protocol_family
        sock_alloc
        inet_create
        get_fd
            get_unused_fd
                find_first_zero_bit
                    FD_SET
                        __FD_SET <-- Here

39-41: #define __FD_SET(fd,fdsetp) \
               __asm__ __volatile__("btsl %1,%0": \
                   "=m" (*(__kernel_fd_set *) (fdsetp)):"r" ((int) (fd)))
```

#### FD\_CLR (linux/include/linux/time.h:31)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket
        find_protocol_family
        sock_alloc
        inet_create
        get_fd
            get_unused_fd
                find_first_zero_bit
                    FD_SET
                    FD_CLR <-- Here

31: #define FD_CLR(fd,fdsetp)	__FD_CLR(fd,fdsetp)
```

#### \_\_FD\_CLR (linux/include/asm-i386/posix\_types.h:44)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket
        find_protocol_family
        sock_alloc
        inet_create
        get_fd
            get_unused_fd
                find_first_zero_bit
                    FD_SET
                    FD_CLR
                        __FD_CLR <-- Here

44-46: #define __FD_CLR(fd,fdsetp) \
               __asm__ __volatile__("btrl %1,%0": \
                   "=m" (*(__kernel_fd_set *) (fdsetp)):"r" ((int) (fd)))
```

#### get\_empty\_filp (linux/fs/file\_table.c:113)

```txt
Control Flow:
sys_socketcall
    verify_area
    sys_socket
        find_protocol_family
        sock_alloc
        inet_create
        get_fd
            get_unused_fd
            get_empty_filp <-- Here
```
