## Walkthrough of Linux 2.0.30's _read_ System Call

### Code Walkthrough

#### sys\_read (linux/fs/read\_write.c:104)

```txt
Control Flow:
sys_read <-- Here

111: Calls fget.

126: Calls locks_verirfy_area.

129: Calls verify_area.

132: Calls the file's read method.
```

#### fget (linux/include/linux/file.h:4)

```txt
Control Flow:
sys_read
    fget <-- Here
```

#### locks\_verify\_area (linux/fs/locks.c:395)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area <-- Here

403-404: return (locks_mandatory_area(read_write, inode, filp, offset,
                          count));
```

#### locks\_mandatory\_area (linux/fs/locks.c:428)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
        locks_mandatory_area <-- Here
```

#### verify\_area (linux/include/linux/mm.h:14)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area <-- Here
```

#### generic\_file\_read (linux/mm/filemap.c:569)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read <-- Here

629: Calls page_hash.

630: Calls __find_page.

643: Calls generic_file_readahead.

647: Calls wait_on_page.

666: Calls memcpy_tofs.

667: Calls release_page.

683: Calls __get_free_page.

699: Calls add_to_page_cache.

719: Calls generic_readpage.

743-744: Calls free_page.

752: Returns local variable read.
```

#### page\_hash (linux/include/linux/pagemap.h:45)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash <-- Here

45: #define page_hash(inode,offset) (page_hash_table+_page_hashfn(inode,offset))
```

#### \_\_find\_page (linux/include/linux/pagemap.h:47)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page <-- Here
```

#### add\_to\_page\_cache (linux/mm/filemap.c:257)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache <-- Here
```

#### generic\_readpage (linux/fs/buffer.c:1262)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage <-- Here

1276: Calls ext2_bmap.

1283: Calls brw_page.
```

#### ext2\_bmap (linux/fs/ext2/inode.c:132)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap <-- Here
```

#### brw\_page (linux/fs/buffer.c:1077)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page <-- Here

1091: Calls create_buffers.

1122: Calls get_hash_table.

1126: Calls ll_rw_block.

1127: Calls wait_on_buffer.

1135: Calls brelse.

1148: Calls ll_rw_block.

1155: Calsl wake_up.

1156: Calls save_flags.

1157: Calls cli.

1158: Calls free_async_buffers.

1159: Calls restore_flags.

1160: Calls after_unlock_page.

1162: Increments the current process's maj_flt field.

1163: Returns zero.
```

#### create\_buffers (linux/fs/buffer.c:1001)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers <-- Here
```

#### get\_hash\_table (linux/fs/buffer.c:472)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table <-- Here
```

#### ll\_rw\_block (linux/drivers/block/ll\_rw\_blk.c:450)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block <-- Here

515: Calls make_request.
```

#### make\_request (linux/drivers/blocks/ll\_rw\_blk.c:283)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block
                    make_request <-- Here

...

443: Calls add_request.
```

#### add\_request (linux/drivers/block/ll\_rw\_blk.c:234)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block
                    make_request
                        add_request <-- Here
```

#### wait\_on\_buffer (linux/include/linux/locks.h:23)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block
                wait_on_buffer <-- Here

25-26: Calls __wait_on_buffer.
```

#### \_\_wait\_on\_buffer (linux/fs/buffer.c:113)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block
                wait_on_buffer
                    __wait_on_buffer <-- Here
```

#### brelse (linux/include/linux/fs.h:635)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block
                wait_on_buffer
                brelse <-- Here

647-638: Calls __brelse.
```

#### \_\_brelse (linux/fs/buffer.c:790)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block
                wait_on_buffer
                brelse
                    __brelse <-- Here
```

#### wake\_up (linux/kernel/sched.c:435)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block
                wait_on_buffer
                brelse
                wake_up <-- Here
```

#### save\_flags (linux/include/asm-i386/system.h:227)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block
                wait_on_buffer
                brelse
                wake_up
                save_flags <-- Here

227-228: #define save_flags(x) \
 __asm__ __volatile__("pushfl ; popl %0":"=g" (x): /* no input */ :"memory")
```

#### cli (linux/include/asm-i386/system.h:225)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block
                wait_on_buffer
                brelse
                wake_up
                save_flags
                cli <-- Here

225: #define cli() __asm__ __volatile__ ("cli": : :"memory")
```

#### free\_async\_buffers (linux/fs/buffer.c:1054)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block
                wait_on_buffer
                brelse
                wake_up
                save_flags
                cli
                free_async_buffers <-- Here
```

#### restore\_flags (linux/include/asm-i386/system.h:230)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block
                wait_on_buffer
                brelse
                wake_up
                save_flags
                cli
                free_async_buffers
                restore_flags <-- Here

230-231: #define restore_flags(x) \
 __asm__ __volatile__("pushl %0 ; popfl": /* no output */ :"g" (x):"memory")
```

#### after\_unlock\_page (linux/fs/buffer.c:1040)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
            ext2_bmap
            brw_page
                create_buffers
                get_hash_table
                ll_rw_block
                wait_on_buffer
                brelse
                wake_up
                save_flags
                cli
                free_async_buffers
                restore_flags
                after_unlock_page <-- Here
```

#### generic\_file\_readahead (linux/mm/filemap.c:459)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
        generic_file_readahead <-- Here
```

#### wait\_on\_page (linux/include/linux/pagemap.h:137)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
        generic_file_readahead
            wait_on_page <-- Here

139-140: Calls __wait_on_page.
```

#### \_\_wait\_on\_page (linux/mm/filemap.c:310)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
        generic_file_readahead
            wait_on_page
                __wait_on_page <-- Here
```

#### memcpy\_tofs (linux/include/asm-i386/segment.h:258)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
        generic_file_readahead
            wait_on_page
            memcpy_tofs <-- Here
```

#### release\_page (linux/mm/filemap.c:52)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
        generic_file_readahead
            wait_on_page
            memcpy_tofs
            release_page <-- Here

54: Calls atomic_dec.
```

#### atomic\_dec (linux/include/asm-i386/atomic.h:48)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
        generic_file_readahead
            wait_on_page
            memcpy_tofs
            release_page
                atomic_dec <-- Here
```

#### free\_page (linux/include/linux/mm.h:249)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
        generic_file_readahead
            wait_on_page
            memcpy_tofs
            release_page
            free_page <-- Here

249: #define free_page(addr) free_pages((addr),0)
```

#### free\_pages (/linux/mm/page\_alloc.c:134)

```txt
Control Flow:
sys_read
    fget
    locks_verify_area
    verify_area
    generic_file_read
        page_hash
        __find_page
        add_to_page_cache
        generic_readpage
        generic_file_readahead
            wait_on_page
            memcpy_tofs
            release_page
            free_page
                free_pages <-- Here
```
