## Walkthrough of Linux 2.0.33's _open_ System Call

### Code Walkthrough

#### sys\_open (linux/fs/open.c:574)

```txt
Control Flow:
sys_open <-- Here

579: Calls get_unused_fd.

582: Calls getname.

584: Calls do_open.

585: Calls putname.

586-587: Returns local variable fd.
```

#### get\_unused\_fd (linux/fs/open.c:555)

```txt
Control Flow:
sys_open
    get_unused_fd <-- Here

560: Calls find_first_zero_bit.

562: Calls FD_SET to set to the file descriptor's bit
     in the open_fds bitmap.

563: Calls FD_CLR to clear the file descritpor's bit
     in the close_on_exec bitmap.

564: Returns local variable fd.
```

#### find\_first\_zero\_bit (linux/include/asm-i386/bitops.h:75)

```txt
Control Flow:
sys_open
    get_unused_fd
        find_first_zero_bit <-- Here
```

#### FD\_SET (linux/include/linux/time.h:30)

```txt
Control Flow:
sys_open
    get_unused_fd
        find_first_zero_bit
            FD_SET <-- Here

30: #define FD_SET(fd,fdsetp)	__FD_SET(fd,fdsetp)
```

#### \_\_FD\_SET (linux/include/asm-i386/posix\_types.h:39)

```txt
Control Flow:
sys_open
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
sys_open
    get_unused_fd
        find_first_zero_bit
            FD_SET
            FD_CLR <-- Here

31: #define FD_CLR(fd,fdsetp)	__FD_CLR(fd,fdsetp)
```

#### \_\_FD\_CLR (linux/include/asm-i386/posix\_types.h:44)

```txt
Control Flow:
sys_open
    get_unused_fd
        find_first_zero_bit
            FD_SET
            FD_CLR
                __FD_CLR <-- Here

44-46: #define __FD_CLR(fd,fdsetp) \
               __asm__ __volatile__("btrl %1,%0": \
                   "=m" (*(__kernel_fd_set *) (fdsetp)):"r" ((int) (fd)))
```

#### getname (linux/fs/namei.c:29)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname <-- Here

60: Calls get_max_filename.

68: Calls get_user.

71: Calls __get_free_page.

74-81: Copies the filename from user space into the
       kernel buffer.

77-79: Null terminates the string and returns zero.
```

#### get\_max\_filename (linux/fs/namei.c:29)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
        get_max_filename <-- Here
```

#### get\_user (linux/include/asm-i386/segment.h:18)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
        get_max_filename
            get_user <-- Here

18: #define get_user(ptr) ((__typeof__(*(ptr)))__get_user((ptr),sizeof(*(ptr))))
```

#### \_\_get\_user (linux/include/asm-i386/segment.h:58)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
        get_max_filename
            get_user
                __get_user <-- Here
```

#### do\_open (linux/fs/open.c:502)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open <-- Here

508: Calls get_empty_filp.

517: Calls open_namei.

532-533: Calls the file's open method if it exists.

539: Inserts file structure into the current process's
     file descriptor table.

540: Returns zero.
```

#### get\_empty\_filp (linux/fs/file\_table.c:68)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp <-- Here

130-139: Searches the file list for a free file structure.

132: Calls remove_file_free.

133: Calls memset to bzero the file structure.

134: Calls put_last_free.

135: Assigns one to the file structure's f_count field.

137: Returns the file structure.
```

#### remove\_file\_free (linux/fs/file\_table.c:41)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
            remove_file_free <-- Here
```

#### put\_last\_free (linux/fs/file\_table.c:57)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
            remove_file_free
            put_last_free <-- Here
```

#### open\_namei (linux/fs/namei.c:335)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei <-- Here

344: Calls dir_namei.

386: Calls lookup.

391: Calls follow_link.

398: Calls permission

458: Assigns inode to res_inode argument.

459: Returns zero.
```

#### dir\_namei (linux/fs/namei.c:217)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei <-- Here

240: Breaks out of loop if we reached the end of
     the pathname.

243: Calls lookup.

248: Calls follow_link.

256: Assigns thisname to the name argument.

257: Assigns len to namelen argument.

258: Assigns bse to res_inode argument.

259: Returns zero.
```

#### lookup (linux/fs/namei.c:156)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup <-- Here

166: Calls permission.

191: return dir->i_op->lookup(dir, name, len, result);
```

#### permission (linux/fs/namei.c:99)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                    permission <-- Here
```

#### ext2\_lookup (linux/fs/ext2/namei.c:154)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                    permission
                    ext2_lookup <-- Here

172: Calls dcache_lookup.

177: Calls iget.

181: Calls iput.

182: Returns zero.

185: Calls ext2_find_entry.

192: Calls dcache_add.

193: Calls brelse.

194: Calls iget.

198: Calls iput.

199: Returns zero.
```

#### dcache\_lookup (linux/fs/dcache.c:183)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                    permission
                    ext2_lookup
                        dcache_lookup <-- Here
```

#### iget (linux/include/linux/fs.h:684)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                    permission
                    ext2_lookup
                        dcache_lookup
                        iget <-- Here

686: return __iget(sb, nr, 1);
```

#### \_\_iget (linux/fs/inode.c:569)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                    permission
                    ext2_lookup
                        dcache_lookup
                        iget
                            __iget <-- Here
```

#### iput (linux/fs/inode.c:418)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                    permission
                    ext2_lookup
                        dcache_lookup
                        iget
                        iput <-- Here
```

#### ext2\_find\_entry (linux/fs/ext2/namei.c:62)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                    permission
                    ext2_lookup
                        dcache_lookup
                        ext2_find_entry <-- Here
```

#### dcache\_add (linux/fs/dcache.c:183)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                    permission
                    ext2_lookup
                        dcache_lookup
                        ext2_find_entry
                        dcache_add <-- Here
```

#### brelse (linux/include/linux/fs.h:635)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                    permission
                    ext2_lookup
                        dcache_lookup
                        ext2_find_entry
                        dcache_add
                        brelse <-- Here

647-648: Calls __brelse.
```

#### \_\_brelse (linux/fs/buffer.c:790)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                    permission
                    ext2_lookup
                        dcache_lookup
                        ext2_find_entry
                        dcache_add
                        brelse
                            __brelse <-- Here
```

#### follow\_link (linux/fs/namei.c:194)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                follow_link <-- Here

208: return inode->i_op->follow_link(dir,inode,flag,mode,res_inode);
```

#### ext2\_follow\_link (linux/fs/ext2/symlink.c:54)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                follow_link <-- Here

81: Calls ext2_bread.

94: Calls open_namei.
```

#### ext2\_bread (linux/fs/ext2/inode.c:392)

```txt
Control Flow:
sys_open
    get_unused_fd
    getname
    do_open
        get_empty_filp
        open_namei
            dir_namei
                lookup
                follow_link
                    ext2_bread <-- Here
```
