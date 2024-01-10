## Walkthrough of Linux 2.0.30's _close_ System Call

### Code Walkthrough

#### sys\_close (linux/fs/open.c:631)

```txt
Control Flow:
sys_close <-- Here

474: Sets the file's entry in the open file table to
     NULL.
```

#### close\_fp (linux/fs/open.c:442)

```txt
Control Flow:
sys_close
    close_fp <-- Here

446-449: Returns early if attempting to close a file
         with a refcount equal to zero.

451-452: Clears all locks owned by the file.

453-456: Decrements refcount and returns 0 if this
         is NOT the last reference to the file.

457-458: Invokes the inode's release function to
         close the file.

460: Sets the file structure's inode pointer to NULL.
```

#### fcntl\_remove\_locks (linux/fs/locks.c:173)

```txt
Control Flow:
sys_close
    close_fp
        fcntl_remove_locks <-- Here


```

#### free\_lock (linux/fs/locks.c:439)

```txt
Control Flow:
sys_close
    close_fp
        fcntl_remove_locks
            free_lock <-- Here

```

#### wake\_up (linux/kernel/sched.c:311)

```txt
Control Flow:
sys_close
    close_fp
        fcntl_remove_locks
            free_lock
                wake_up <-- Here

```

#### ext2\_release\_file (linux/fs/ext2/file.c:297)

```txt
Control Flow:
sys_close
    close_fp
        fcntl_remove_locks
        ext2_release_file <-- Here

```
