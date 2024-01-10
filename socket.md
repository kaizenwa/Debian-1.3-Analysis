## Walkthrough of Linux 2.0.30's _socket_ System Call

### Code Walkthrough

#### sys\_socketcall (linux/net/socket.c:1237)

```txt
Control Flow:
sys_socketcall <-- Here

854: Routes the system call to the appropriate case via the
     call argument.
```

#### verify\_area (linux/include/linux/mm.h:14)

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

#### sock\_socket (linux/net/socket.c:428)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_socket <-- Here

447: Assigns the protocol ops structure to the ops
     argument.

464-467: Allocates a socket structure and assigns it to
         the sock argument.

468-469: Assigns the socket's type and protocol
         ops.

         inet sockets -> inet_proto_ops (linux/net/inet/sock.c:1828)

         unix sockets -> unix_proto_ops (linux/net/unix/sock.c:902)

470: Invokes the socket's create method to create the
     underlying specific socket type (unix or inet).

475: Calls get_fd to find and assign a file descriptor
     to the socket.
```

#### sock\_alloc (linux/net/socket.c:168)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_socket
        sock_alloc <-- Here

173: Disables interrupts.

174-206: Iterates through the socket array to obtain
         the first unused socket structure.

175-182: Enables interrupts and initializes the socket's
         fields to 0.

191: Assigns an inode structure to the socket by calling
     get_empty_inode.

196-199: Initializes the inode's fields.

201: Initializes the socket's waitqueue.

205: Returns the socket on success.

208: Enables interrupts if the for loop fails to find
     an unused socket.

211: Sleeps on &socket_wait_free to wait for an unused
     socket.

212-215: Returns NULL if the process' sleep was
         interrupted by a signal.
```

#### get\_empty\_inode (linux/fs/inode.c:343)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_socket
        sock_alloc
            get_empty_inode <-- Here

348-349: Calls grow_inodes if there are twice as many inodes
         in use as there are inodes available for use.

351-352: Initializes inode to first_inode and best to NULL.

353-362: Iterates through the inode list and finds the first
         clean unused inode.

363-367: Calls grow_inodes and loops back to repeat if we
         were unable to find an clean unused inode.

384: Bzeroes the inode with the clear_inode function.

385-386: Initializes the inode's refcount, link count, and
         semaphore count to one.

388: Decrements nr_free_inodes.

393: Returns the inode.
```

#### clear\_inode (linux/fs/inode.c:147)

```
Control Flow:
sys_socketcall
    verify_area
    sock_socket
        sock_alloc
            get_empty_inode
                clear_inode <-- Here

152-153: Removes the inode from the inode hashlist and
         free list.

155-156: Increments nr_free_inodes if the inode we are
         clearing has a nonzero refcount.

157: Bzeroes the inode structure.

159: Inserts the inode into the free list.
```

#### inet\_create (linux/net/inet/sock.c:755)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_socket
        sock_alloc
        inet_create <-- Here (remote IPC case)

761: Allocates a sock structure and assigns it to
     sk argument.

764-765: Initializes the inet socket structure's num and
         reuse field.

766: Switches on the inet socket's type. We will only
     consider datagram sockets in these notes.

783: Sets protocol to IPROTO_UDP.

785: Assigns &udp_prot (linux/net/inet/udp.c:616) to the prot
     argument.

827-923: Initializes the inet socket's fields.

925-934: sk->num is 0 for the DGRAM case, so we ignore
         this if statement.

936-942: udp_prot does not have an init method so we
         ignore this if statement.

943: Returns 0 on success.
```

#### unix\_proto\_create (linux/net/unix/sock.c:311)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_socket
        sock_alloc
        unix_proto_create <-- Here (local IPC case)

320: Assigns a unix_proto_data structure to the unix socket.

324: Allocates and assigns a buffer to the unix_proto_data
     structure.

329-330: Assigns a protocol and socket to the unix_proto_data
         structure.

331: Assigns the unix_proto_data structure to the unix socket's
     data field.

332-334: Initializes the unix_proto_data structure's refcount
         to 1 and returns 0.
```

#### unix\_data\_alloc (linux/net/unix/sock.c:251)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_socket
        sock_alloc
        unix_proto_create
            unix_data_alloc <-- Here

255: Disables interrupts.

256-269: Iterates through the unix_datas array (linux/net/unix/sock.c:55)
         and finds the first unused entry.

259: Enables interrupts inside the for loop.

260-267: Initializes the unix_proto_data structure fields to 0
         and returns it.

270-271: Enables interrupts if there are no free
         unix_proto_data structures and returns NULL.
```

#### get\_fd (linux/net/socket.c:98)

```txt
Control Flow:
sys_socketcall
    verify_area
    sock_socket
        sock_alloc
        inet_create
        get_fd <-- Here

104: Calls get_empty_filp to obtain a pointer to an
     empty file structure.

106-107: Iterates through the process' file descriptor table
         to find the first unused descriptor.

108-110: Returns -1 if the process has no free file descriptors.

112: Clears the file's close_on_exec flag.

113: Assigns the file structure to the file descriptor.

114: Assigns &socket_file_ops (linux/net/socket.c:60) as the file's
     operation structure.

115-118: Initializes the file's fields.

119: Increments the inode's refcount.

120-121: Initializes the file offset to 0 and returns the
         file descriptor.
```
