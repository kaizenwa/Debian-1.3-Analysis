## Walkthrough of Linux 2.0.33's _mmap_ System Call

#### old\_mmap (linux/arch/i386/kernel/sys\_i386.c:46)

```txt
Control Flow:
sys_mmap <-- Here

143: Verifies that buffer, whose length is 24 bytes, is valid.

146: Obtains the system call's flags using get_user_long.

147-150: Obtains the fd for file mappings using get_user_long.
```

#### get\_fs\_long (linux/include/asm/segment.h:24)

```txt
Control Flow:
sys_mmap
    verify_area
    get_fs_long <-- Here

static inline unsigned long get_user_long(const int *addr)
{
	unsigned long _v;

	__asm__ ("movl %%fs:%1,%0":"=r" (_v):"m" (*addr)); \
	return _v;
}
```

#### do\_mmap (linux/mm/mmap.c:42)

```txt
Control Flow:
sys_mmap
    verify_area
    get_fs_long
    do_mmap <-- Here

47-48: Returns early if the page aligned length is 0.
       In other words, memory mappings must be at least
       a page in size.

50-51: Returns -EINVAL if the address, len, or address+len
       exceeds TASK_SIZE, which is 3GiB.

59-71: Ensures that the file's mode is compatible with the
       type of mapping.

78-80: Returns -EINVAL if the address for fixed mappings is
       not page aligned.

84-101: Searches for a free space to insert the mapping between
        SHM_RANGE_START and SHM_RANGE_END, which are equal to
        0x40000000 and 0x60000000 respectively.

109-110: Returns -ENODEV is the file does not have a mmap method.

112-118: Sets up the mask argument to pass to either anon_map or
         generic_mmap.
```

#### anon\_map (linux/mm/mmap.c:445)

```txt
Control Flow:
sys_mmap
    verify_area
    get_fs_long
    do_mmap
        do_munmap
        anon_map <-- Here  (!file case)

454: Allocates a new vm_area_struct structure.

458-465: Initializes the new vm_area_struct.

466: Calls insert_vm_struct to insert the vm_area_struct into
     the current task's address space.

467: Calls merge_segments to merge any contiguous vm_area_structs
     in the current task's address space.
```

#### insert\_vm\_struct (linux/mm/mmap.c:360)

```txt
Control Flow:
sys_mmap
    verify_area
    get_fs_long
    do_mmap
        do_munmap
        anon_map
            insert_vm_struct <-- Here

366: Initializes nxtpp to &t->mmap.

368-369: Breaks out of the loop when we find an vm structure
         in the list whose start address is greater than the
         start address of the vm structure we want to insert.

         Why don't we test for overlap in this case? In other
         words, why don't we check vmp->end >= mpnt->vm_start?

370: Assigns the next vm structure in the list to nxtpp.

372-379: Checks if the vm structure we want to insert overlaps
         with a pre-existing entry.

381-383: Inserts the vm structure before the entry we found
         in the loop.
```

#### generic\_mmap (linux/mm/mmap.c:311)

```txt
Control Flow:
sys_mmap
    verify_area
    get_fs_long
    do_mmap
        do_munmap
        generic_mmap <-- Here  (file case)

318-325: Returns -EINVAL for read/write memory mappings, offsets
         that are NOT block aligned, non regular files, and
         inodes with no bmap method.

326-327: Checks if the inode can be read by reading in the first
         block.

328-330: Updates the inode's atime and marks it dirty if it is
         not read-only.

334: Allocates a new vm_area_struct for the mapping.

338: Clears all the pages currently mapped in the region we want
     create the memory mapping at.

339-346: Fills in the new vm_area_struct.

347: Assigns file_mmap as the vm_area_struct's vm ops structure.

348-349: Inserts the new vm_area_struct and merges any contiguous
         vm_area_structs in the task.
```

#### unmap\_page\_range (linux/mm/memory.c:272)

```txt
Control Flow:
sys_mmap
    verify_area
    get_fs_long
    do_mmap
        do_munmap
        generic_mmap
            unmap_page_range <-- Here

282: Rounds up size to the nearest page.

283: Assigns the virtual address of the pde to dir.

284: Assigns the pte offset to poff.

285-286: Assigns pcnt to MIN(1024-poff,size).

290-292: Sets poff to 0 if the current pde is invalid.

298-301: Assigns the virtual address of the current page
         table to page_table and increments it by poff to
         obtain the virtual address of the pte.

303-313: Clears the remaining ptes in the current page
         table and frees their associated pages if they
         are present and not reserved.

315-317: Clears the current pde if we just cleared an
         entire page table.
```

#### insert\_vm\_struct (linux/mm/mmap.c:360)

```txt
Control Flow:
sys_mmap
    verify_area
    get_fs_long
    do_mmap
        do_munmap
        generic_mmap
            unmap_page_range
            insert_vm_struct <-- Here

366: Initializes nxtpp to &t->mmap.

368-369: Breaks out of the loop when we find an vm structure
         in the list whose start address is greater than the
         start address of the vm structure we want to insert.

         Why don't we test for overlap in this case? In other
         words, why don't we check vmp->end >= mpnt->vm_start?

370: Assigns the next vm structure in the list to nxtpp.

372-379: Checks if the vm structure we want to insert overlaps
         with a pre-existing entry.

381-383: Inserts the vm structure before the entry we found
         in the loop.
```

#### merge\_segments (linux/mm/mmap.c:391)

```txt
Control Flow:
sys_mmap
    verify_area
    get_fs_long
    do_mmap
        do_munmap
        generic_mmap
            unmap_page_range
            insert_vm_struct
            merge_segments <-- Here

```
