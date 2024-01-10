## Walkthrough of Linux 2.0.30's _write_ System Call

### Code Walkthrough

#### sys\_write (linux/fs/read\_write.c:139)

```txt
Control Flow:
sys_write <-- Here

104: Verifies that the write operation is valid.

107: Calls the file's write method.
```

#### verify\_area (linux/include/linux/mm.h:14)

```txt
Control Flow:
sys_write
    verify_area <-- Here

16-19: Returns -EFAULT if the addr or addr+size is greater
       than TASK_SIZE, which is equal to 3GiB.

20-21: Returns 0 here if we are verifying a read operation
       or if the architecture supports write protection.

22: Verifies the area manually for write operations on
    architectures that do NOT support write protection.
```

#### ext2\_file\_write (linux/fs/ext2/file.c:218)

```txt
Control Flow:
sys_write
    verify_area
    ext2_file_write <-- Here

233-237: Returns -ENOSPC if the file is marked read-only.

239-242: Determines if the inode we are trying to read
         represents a regular file.

248-251: Assigns the position of the write to pos.

         I will finish this later.

263-264: Writes the block to disk via ll_rw_block and waits
         until write operation is complete.
```

#### ext2\_getblk (linux/fs/ext2/inode.c:331)

```txt

```

#### inode\_getblk (linux/fs/ext2/inode.c:189)

```txt

```

#### block\_getblk (linux/fs/ext2/inode.c:254)

```txt

```

#### ll\_rw\_block (linux/drivers/block/ll\_rw\_blk.c:343)

```txt
353-356: Ensures that the first block to write exists and
         is valid.

359-367: Assigns the file's block device to dev.

377-384: Ensures that the block size is correct for each
         disk block in the read request.

387-390: Checks for write operations on read-only block
         devices.

406-415: Inserts each disk block into the queue via
         make_request. Increments the pgpgin and pgpgout
         kernel statistics.

425-428: Clears each buffers dirty and uptodate flags.

```
