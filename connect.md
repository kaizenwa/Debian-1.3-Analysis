## Walkthrough of Linux 2.0.30's _connect_ System Call

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

#### sock\_connect (linux/net/socket.c:639)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_connect <-- Here

```

#### inet\_connect (linux/net/inet/sock.c:1095)

```txt
```

#### unix\_proto\_connect (linux/net/unix/sock.c:437)

```txt
```
