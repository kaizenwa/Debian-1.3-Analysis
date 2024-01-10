## Walkthrough of Linux 2.0.33's _open_ System Call

### Code Walkthrough

#### sys\_open (linux/fs/open.c:574)

```txt
Control Flow:
sys_open <-- Here

429: Copies the pathname from user space into the kernel
     space buffer tmp.

432: Calls do_open to complete the open operation.

433: Frees the kernel space buffer tmp.
```

#### getname (linux/fs/namei.c:29)

```txt
Control Flow:
sys_open
    getname <-- Here

35-37: Checks if the address of the filename exists and is
       within userspace (< 3GB).

38-43: Sets variable i to PAGE_SIZE and preemptively assigns
       -ENAMETOOLONG to the error variable if the remaining
       amount of memory in the task exceeds PAGE_SIZE.

44-46: Calls get_fs_byte to obtain the first character of the
       filename and returns -ENOENT if the character is '\0'.

47-49: Obtains a free page of memory and assigns it to the
       tmp variable to create a 4096 character array.

50-57: Copies the filename to tmp, null terminates it, and
       returns 0.

58-59: Frees the character array if the filename exceeds
       PAGE_SIZE in length and returns -ENAMETOOLONG
       (recall that this was preemptively set on line 42).
```

#### do\_open (linux/fs/open.c:376)

```txt
Control Flow:
sys_open
    getname
    do_open <-- Here

382-384: Assigns the first unused file descriptor index
         to fd.

387: Clears the close_on_exec bit for the file descriptor.

388: Obtains a pointer to an empty file structure.

391: Inserts the empty file structure into the current
     task's open file table.

392-397: Builds the flag argument to pass to open_namei.

405: Assigns the inode obtained from open_namei to the
     new file structure's inode field.

409-410: Assigns default_file_ops structure to the new
         file's f_op field.

412: Invokes inode's open file method. Note that there is
     no open method for ext2 files.

420: Clears the open flags from the file structure's flag
     field.
```

#### get\_empty\_filp (linux/fs/file\_table.c:68)

```txt
Control Flow:
sys_open
    getname
    do_open
        get_empty_filp

73-74: Calls grow_files for the system's first request
       for a new file structure.

76-83: Finds the first file structure with a zero refcount,
       removes it from the file table, bzeroes it, puts it
       at the end of the file table, increments its refcount,
       and returns it.

84-87: Calls grow_files if there are no free file structures
       in the file table and there are less than 1024 files
       in total.

88: Returns NULL if there are no free file structures and
    we cannot grow the file table any further.
```

#### grow\_files (linux/fs/file\_table.c:43)

```txt
Control Flow:
sys_open
    getname
    do_open
        get_empty_filp
            grow_files <-- Here

48: Obtains a free page of memory with get_free_page.

53: Increments nr_files by the amount of file structures
    in a single page of memory.

55-56: If this is the system's first call to grow_files
       it sets first_file as the first file structure in
       the file table and decrements i.

58-59: Adds all the newly allocated file structures to the
       file table.
```

#### insert\_file\_free (linux/fs/file\_table.c:14)

```txt
Control Flow:
sys_open
    getname
    do_open
        get_empty_filp
            grow_files
                insert_file_free <-- Here

16-20: Inserts the new file structure to the beginning of
       the file table using standard linked list methods.
```

#### remove\_file\_free (linux/fs/file\_table.c:23)

```txt
Control Flow:
sys_open
    getname
    do_open
        get_empty_filp
            grow_files
            remove_file_free <-- Here

25-31: Removes the file structure from the file table
       using standard linked list methods and assigns
       its f_next and f_prev fields to NULL.
```

#### put\_last\_free (linux/fs/file\_table.c:35)

```txt
Control Flow:
sys_open
    getname
    do_open
        get_empty_filp
            grow_files
            remove_file_free
            put_last_free <-- Here

36: Calls remove_file_free to remove the file structure
    from the file table, which is a redundant procedure
    if the put_last_free call follows the memset call
    on line 79.

37-40: Inserts the file structure at the end of the file table.
```

#### open\_namei (linux/fs/namei.c:274)

```txt
Control Flow:
sys_open
    getname
    do_open
        get_empty_filp
        open_namei <-- Here

284: Calls dir_namei to obtain the name, name length, and base
     directory of the pathname's leaf node.

287-299: Handles the "/usr/" special case.

300: Increments the base directory's refcount.

301-322: Handles the O_CREAT case.

304-307: If the file we wanted to create already exists for the
         O_EXCL case, we decrement the refcount of the inode
         and return -EEXIST at line 327.

309-314: Handles the case where lookup returned any error other
         than -ENOENT.

315-321: Calls the base directory's create method to create the
         file. 

324: Calls lookup for the !O_CREAT case.

329: Calls follow_link in case the leaf node was a symbolic link.

332-335: Returns -EISDIR if the leaf node is a directory and the
         current process does not have write permission.

336-339: Returns -EACCESS if the current process does not have
         permissions >= rw--w-r--.

340-344: Returns -EACCES if the inode represents a device file
         with the MS_NODEV flag set. This flag is defined in
         /include/linux/fs.h on line 68 as:

         #define MS_NODEV  4 /* disallow access to device special files */ 

346-350: Returns -EROFS if the inode has the MS_RDONLY flag set,
         which corresponds to a read-only file system.

         #define MS_RDONLY 1 /* mount read-only */

351-369: If the current process wants to open the file for writing
         and other processes have it open, it searches the process
         list for any processes using the file as an executable
         or as an inode. Returns -ETXTBSY if any such process is found.

370-379: Handles the O_TRUNC case.

380-381: Assigns the file's inode to *res_inode and returns 0.
```

#### dir\_namei (linux/fs/namei.c:156)

```txt
Control Flow:
sys_open
    getname
    do_open
    get_empty_filp
    open_namei
        dir_namei <-- Here

165: Sets base to the current working directory if a
     base directory is not specified.

169-174: Decrements the previous base directory's refcount,
         sets base to the current process's root directory,
         and increments its refcount.

177: Assigns the length of the next pathname node to len.

179-180: Breaks out of the while loop if we reached the
         leaf node of the pathname.

182: Calls lookup to obtain the inode of the directory entry
     thisname.

187: Calls follow_link to handle symbolic links if applicable.
     If the directory entry is not a symbolic link, the follow_link
     routine simply returns early.

191-194: Returns -ENOTDIR is the base directory does not have a
         lookup method. We check this condition here because we
         break out of the loop upon reaching the leaf node, hence
         we do not call lookup and check this condition at lines
         118-121.

195-198: Assign the leaf node's name and length, along with the
         base directory's inode to the pass-through return values
         and return 0.
```

#### lookup (linux/fs/namei.c:94)

```txt
Control Flow:
sys_open
    getname
    do_open
    get_empty_filp
    open_namei
        dir_namei
            lookup <-- Here

100: Initializes result variable to NULL.

104: Checks if the caller's acl has execute permission to
     traverse the directory.

105-108: Handles the ".." case for the process's root directory
         by returning the root directory.         

109-116: Handles the ".." case for when the dir variable is the
         root of a mounted filesystem by decrementing dir's
         refcount, assigning the mount point directory to dir,
         and incrementing the mount point's directory.

126-129: Handles the empty name case by returning dir variable
         as the result.

130: Calls the inode's lookup method.
```

#### permission (linux/fs/namei.c:74)

```txt
Control Flow:
sys_open
    getname
    do_open
    get_empty_filp
    open_namei
        dir_namei
            lookup
                permission <-- Here

78-79: Calls the inode's permission method if it exists.
```

#### ext2\_permission (linux/fs/ext2/acl.c:25)

```txt
Control Flow:
sys_open
    ...
    open_namei
        dir_namei
            lookup
                permission
                    ext2_permission <-- Here

27: Initializes mode variable to the inode's access rights.

32-33: Returns 1 for the super user. For thsoe interested
       in the suser routine, it is defined as follows:

       #define suser() (current->euid == 0)

37-38: Right-shifts mode by 6 if the current process's
       effective user ID is the owner of the file.

       mode = XXXXXXX 000      000     000
                       ^        ^       ^
                      owner   group   other

       By right-shifted by 6, we shift the owner's bits
       to the three least-significant bits for the
       bitmask.

39-40: Right-shifts mode by 3 if the current process
       is within the inode's group. This shifts the
       group's bits to the three least-significant
       bits for the bitmask.

41-44: Returns 1 if the S_IRWXO flag is set in mode and
       returns 0 otherwise.
```

#### ext2\_lookup (linux/fs/ext2/namei.c:170)

```txt
Control Flow:
sys_open
    ...
    open_namei
        dir_namei
            lookup
                permission
                ext2_lookup <-- Here

177: Initializes result to NULL.

185: Searches the ext2 directory cache for the directory.

187: Calls ext2_find_entry if the directory was not cached.

191-198: Adds the directory entry's inode number to the ext2
         dcache and releases the buffer head.

200: Assigns the directory entry's inode pointer to *result.

204-205: Decrements the directory's refcount and return 0.
```

#### ext2\_find\_entry (linux/fs/ext2/namei.c:75)

```txt
Control Flow:
sys_open
    ...
    open_namei
        dir_namei
            lookup
                permission
                ext2_lookup
                    ext2_find_entry <-- Here

Ill do this later
```

#### follow\_link (linux/fs/namei.c:133)

```txt
Control Flow:
sys_open
    ...
    open_namei
        dir_namei
            lookup
            follow_link <-- Here

147: Calls the inode's follow_link method.
```

#### ext2\_follow\_link (linux/fs/ext2/symlink.c:50)

```txt
Control Flow:
sys_open
    ...
    open_namei
        dir_namei
            lookup
            follow_link
                ext2_follow_link <-- Here

66-70: If the inode's file is not a symbolic link, we
       decrement the directory's refcount and return 0.

71-75: Returns -ELOOP if we have traversed five symbolic
       links in the overall namei procedure. Or in other
       words, if we have made five recursive calls to
       open_namei we return -ELOOP.

76-82: If the inode has buffer heads, we use one of them
       to read in the contents of the file if necessary and
       point the link variable to it.

83-84: If the inode does not have buffer heads, we cast its
       u.ext2_i.i_data field to a character pointer and assign
       it to the link variable.

85: Increment the current process's link_count field.

86: Makes a recursive call to open_namei, passing link as its
    pathname argument.

87: Decrement the current process's link_count field upon
    completing the recursive call.

89-91: Releases the buffer head if we used one and returns
       error variable.
```

#### ext2\_bread (linux/fs/ext2/inode.c:392)

```txt
Control Flow:
sys_open
    ...
    open_namei
        dir_namei
            lookup
            follow_link
                ext2_follow_link
                    ext2_bread <-- Here

397: Calls ext2_getblk to obtain a buffer head for the
     contents of the symbolic link's file data.

398-399: Returns bh if the buffer head is NULL or the
         buffer head already contains up to date file
         information.

400-401: Calls ll_rw_block to read the symbolic link's
         updated file contents into the buffer head.

402-403: Returns the buffer head if the read was successful.

404-406: Frees the buffer head, sets err to -EIO, and returns
         NULL if the updated file contents could not be read.
```

#### putname (linux/fs/namei.c:62)

```txt
Control Flow:
sys_open
    getname
    do_open
    putname <-- Here

64: Frees the filename from kernel space.
```
