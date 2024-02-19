## Walkthrough of Linux 2.0.30's _write_ System Call

### Code Walkthrough

#### sys\_write (linux/fs/read\_write.c:139)

```txt
Control Flow:
sys_write <-- Here

146: Calls fget.

160: Calls locks_verify_area.

163: Calls verify_area.

173: Calls suser.

185: Calls down.

186: Calls the file's write method.

189: Calls fput.

191: Returns local variable error.
```

#### fget (linux/include/linux/file.h:4)

```txt
Control Flow:
sys_write
    fget <-- Here
```

#### locks\_verify\_area (linux/fs/locks.c:395)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area <-- Here

403-404: return (locks_mandatory_area(read_write, inode, filp, offset,
                          count));
```

#### locks\_mandatory\_area (linux/fs/locks.c:428)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
        locks_mandatory_area <-- Here
```

#### verify\_area (linux/mm/memory.c:677)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area <-- Here
```

#### suser (linux/include/linux/sched.h:363)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser <-- Here
```

#### down (linux/include/asm-i386/semaphore.h:44)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down <-- Here
```

#### ext2\_file\_write (linux/fs/ext2/file.c:83)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write <-- Here

137: Calls ext2_getblk.

156: Calls memcpy_fromfs.

157: Calls update_vm_cache.

162: Calls mark_buffer_uptodate.

163: Calls mark_buffer_dirty.

169: Calls ll_rw_block.

171: Calls wait_on_buffer.

174: Calls brelse.

185: Calls ll_rw_block.

187: Calls wait_on_buffer.

190: Calls brelse.

198: Assigns local variable position to the file structure's
     f_pos field.

199: Assigns one to the inode's i_dirt field.

200: Returns local variable written.
```

#### ext2\_getblk (linux/fs/ext2/inode.c:330)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk <-- Here
```

#### memcpy\_fromfs (linux/include/asm-i386/segment.h:253)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk
        memcpy_fromfs <-- Here
```

#### update\_vm\_cache (linux/mm/filemap.c:231)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk
        memcpy_fromfs
        update_vm_cache <-- Here

243: Calls find_page.

245: Calls wait_on_page.

247: Calls release_page.
```

#### find\_page (linux/include/linux/pagemap.h:67)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk
        memcpy_fromfs
        update_vm_cache
            find_page <-- Here

69: return __find_page(inode, offset, *page_hash(inode, offset));
```

#### \_\_find\_page (linux/include/linux/pagemap.h:47)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk
        memcpy_fromfs
        update_vm_cache
            find_page
                __find_page <-- Here
```

#### wait\_on\_page (linux/include/linux/pagemap.h:137)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk
        memcpy_fromfs
        update_vm_cache
            find_page
            wait_on_page <-- Here

139-140: Calls __wait_on_page.
```

#### \_\_wait\_on\_page (linux/mm/filemap.c:310)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk
        memcpy_fromfs
        update_vm_cache
            find_page
            wait_on_page
                __wait_on_page <-- Here
```

#### release\_page (linux/mm/filemap.c:52)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk
        memcpy_fromfs
        update_vm_cache
            find_page
            wait_on_page
            release_page <-- Here

54: Calls atomic_dec.
```

#### atomic\_dec (linux/include/asm-i386/atomic.h:48)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk
        memcpy_fromfs
        update_vm_cache
            find_page
            wait_on_page
            release_page
                atomic_dec <-- Here
```

#### mark\_buffer\_uptodate (linux/fs/buffer.c:1169)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk
        memcpy_fromfs
        update_vm_cache
        mark_buffer_uptodate <-- Here
```

#### mark\_buffer\_dirty (linux/include/linux/fs.h:589)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk
        memcpy_fromfs
        update_vm_cache
        mark_buffer_uptodate
        mark_buffer_dirty <-- Here

592: Calls set_writetime.

594: Calls refile_buffer.
```

#### set\_writetime (linux/fs/buffer.c:728)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk
        memcpy_fromfs
        update_vm_cache
        mark_buffer_uptodate
        mark_buffer_dirty
            set_writetime <-- Here
```

#### refile\_buffer (linux/fs/buffer.c:748)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
        ext2_getblk
        memcpy_fromfs
        update_vm_cache
        mark_buffer_uptodate
        mark_buffer_dirty
            set_writetime
            refile_buffer <-- Here
```

#### ll\_rw\_block (linux/drivers/block/ll\_rw\_blk.c:450)

```txt

515: Calls make_request.
```

#### make\_request (linux/drivers/blocks/ll\_rw\_blk.c:283)

```txt

...

443: Calls add_request.
```

#### add\_request (linux/drivers/block/ll\_rw\_blk.c:234)

```txt
```

#### wait\_on\_buffer (linux/include/linux/locks.h:23)

```txt

25-26: Calls __wait_on_buffer.
```

#### \_\_wait\_on\_buffer (linux/fs/buffer.c:113)

```txt

```

#### brelse (linux/include/linux/fs.h:635)

```txt

647-648: Calls __brelse.
```

#### \_\_brelse (linux/fs/buffer.c:790)

```txt

```

#### up (linux/include/asm-i386/semaphore.h:111)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
    up <-- Here 
```

#### fput (linux/include/linux/file.h:17)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
    up
    fput <-- Here

20-21: Calls __fput.
```

#### \_\_fput (linux/fs/open.c:606)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
    up
    fput
        __fput <-- Here

608-609: Calls the file's release method.

611-612: Calls put_write_access.
```

#### ext2\_release\_file (linux/fs/ext2/file.c:208)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
    up
    fput
        __fput
            ext2_release_file <-- Here

210-211: Calls ext2_discard_prealloc.
```

#### ext2\_discard\_prealloc (linux/fs/ext2/inode.c:67)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
    up
    fput
        __fput
            ext2_release_file
                ext2_discard_prealloc <-- Here

75: Calls ext2_free_blocks.
```

#### ext2\_free\_blocks (linux/fs/ext2/balloc.c:168)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
    up
    fput
        __fput
            ext2_release_file
                ext2_discard_prealloc
                    ext2_free_blocks <-- Here
```

#### put\_write\_access (linux/fs/namei.c:146)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
    up
    fput
        __fput
            ext2_release_file
            put_write_access <-- Here

148: Decrements the inode's i_writecount field.
```

#### iput (linux/fs/inode.c:418)

```txt
Control Flow:
sys_write
    fget
    locks_verify_area
    verify_area
    suser
    down
    ext2_file_write
    up
    fput
        __fput
            ext2_release_file
            put_write_access
            iput <-- Here
```
