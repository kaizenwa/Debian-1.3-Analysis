## Walkthrough of Linux 2.0.33's Page Fault Code

#### do\_page\_fault (linux/mm/fault.c:34)

```txt
Control Flow:
do_page_fault <-- Here
```

### do\_no\_page (linux/mm/memory.c:872)

```txt
Control Flow:
do_page_fault
    do_no_page <-- Here

829: Obtains the faulting address's pde and assigns
     it to page.

832-834: Assigns the value of the faulting address's
         pte to tmp.

835-836: Returns early if the pte's page is present.

837: Increment resident set size.

838-841: Calls swap_in to handle the swap in case.

845: Searches the task's vm_area_structs for a node that
     contains the address.

852-855: Calls get_empty_page if the vm_area_struct that
         contains the address does not have a nopage
         method.

857: Calls the vm_area_structs nopage method to handle
     the page fault.

862-863: If the page fault occurred in the task's data
         region and does not exceed the brk, it increment
         minor faults and calls get_emtpy_page.

864-869: If the page fault was a stack fault and will not
         cause the process to exceed its stack rlim, it
         increments minor faults and calls get_empty_page.

870-873: Assigns the error code and sends the SIGSEGV signal
         to the task.
```

### get\_empty\_pgtable (linux/mm/memory.c:791)

```txt
Control Flow:
do_page_fault
    do_no_page
        get_emtpy_pgtable <-- Here

803: Allocates a new page of memory if the faulting address'
     pde does not map to a page table.

805-808: This is a race condition check that ensures the pagetable
         wasn't allocated before we completed the page fault.

813-815: Assigns the new page of memory as the page table the
         faulting address' pde points to.
```

### file\_mmap\_nopage (linux/mm/memory.c:1145)

```txt
Control Flow:
do_page_fault
    do_no_page
        get_empty_pgtable
        file_mmap_nopage <-- Here

1154-1156: Calculates the block number of the faulting address.

1159-1162: Checks if there is another task that can share the
           the faulting page with the current task.

1164: Increments major fault statistic.

1170-1171: Fills the nr array with the physical block numbers of
           the faulting page.

1172-1173: Marks the page read/write and dirty if the page fault
           was a write fault.

1176-1179: If the page is not read/write, tries to share the page
           with another process.
```

### share\_page (linux/mm/memory.c:752)

```txt
Control Flow:
do_page_fault
    do_no_page
        get_empty_pgtable
        file_mmap_nopage
            share_page <-- Here

760-764: Searches the task array for a non-null process that is
         NOT the current process.

765-775: If the process is not executing the file that contains
         the faulting page, it searches the process' vm_area_structs
         for a structure with the same vm_ops, inode, and device as
         the faulting vm_area_struct. If such a structure is found, its
         share method is invoked.

782: If the process is executing the file that contains the faulting
     page, it calls try_to_share to obtain a clean copy of the page.
```

### try\_to\_share (linux/mm/memory.c:693)

```txt
Control Flow:
do_page_fault
    do_no_page
        get_empty_pgtable
        file_mmap_nopage
            share_page
                try_to_share <-- Here

704-706: Returns early if process p does not have the page
         containing address.

708-709: Assigns the pte of process p's page to from.

711-716: Returns early if the page is dirty, if the page is in
         high memory, or the page is reserved.

726-730: Copies the read/write page with the copy_page function.

731-735: Increments the refcount of the COW page and assigns p's
         pte to the current task's pte.

738-740: Updates both pte's and flushes the TLB.
```

#### put\_page (linux/mm/memory.c:470)

```txt
Control Flow:
do_page_fault
    do_no_page
        get_empty_pgtable
        file_mmap_nopage
            share_page
            bmap
            bread_page
            put_page <-- Here

481: Assigns the virtual address of the faulting address' pde
     to page_table.

482-483: Assigns the virtual address of the faulting address'
         page table to page_table.

490: Calculates the virtual address of the faulting address'
     pte.

491-495: Sets the pte to 0 and flushes to TLB if the page is
         already present.

496: Assigns the physical address of the page and its protections
     to its pte.
```
