## Walkthrough of Linux 2.0.30's _bind_ System Call

### Code Walkthrough

#### sys\_socketcall (linux/net/socket.c:1237)

```txt
Control Flow:
sys_socketcall <-- Here

854: Routes the system call to the appropriate case via the
     call argument.
```

#### verify\_area (linux/mm/memory.c:677)

```txt
Control Flow:
sys_socketcall
    verify_area <-- Here

16-19: Returns -EFAULT if the addr or addr+size is greater
       than TASK_SIZE, which is equal to 3GiB.

20-21: Returns 0 here if we are verifying a read operation
       or if the architecture supports write protection.

22: Verifies the area manually for write operations on
    architectures that do NOT support write protection.
```

#### sock\_bind (linux/net/socket.c:537)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_bind <-- Here

545: Obtains a pointer to the socket from the fd argument.

546: Calls the socket's bind method and returns its retval
     if it was unsuccessful.

550: Returns 0 on success.
```

#### sockfd\_lookup (linux/net/socket.c:157)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_bind
        sockfd_lookup <-- Here

161: Returns NULL if the file descriptor is invalid.
     Assigns the file structure indexed by fd argument
     to the stack allocated file argument.

162: Points the pass-through return variable to stack
     allocated file structure.

163: Calls socki_lookup and returns its retval.
```

#### socki\_lookup (linux/net/socket.c:138)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_bind
        sockfd_lookup
            socki_lookup <-- Here

142-144: Returns the socket obtained from the inode if the
         socket is not marked free and points to the inode.

147-151: Manually searches the sockets array to find the
         socket that points to the inode.

152: Returns NULL if there are no sockets that point to
     the inode.
```

#### inet\_bind (linux/net/inet/sock.c:1013)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_bind
        sockfd_lookup
        inet_bind <-- (inet socket case)


```

#### unix\_proto\_bind (linux/net/unix/sock.c:381)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_bind
        sockfd_lookup
        unix_proto_bind <-- (unix socket case)


```
