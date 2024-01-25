## Walkthrough of Linux Kernel 2.0.30's Kernel Initialization Code

#### start\_kernel (linux/init/main.c:768)

```txt
Control Flow:
start_kernel <-- Here

781: Assigns zero to the static variable first_cpu.

788: Calls setup_arch.

790: Calls trap_init.

791: Calls init_IRQ.

792: Calls sched_init.

793: Calls time_init.

794: Calls parse_options.

814: Calls console_init.

816: Calls pci_init.

818: Calls kmalloc_init.

819: Calls sti.

820: Calls calibrate_delay.

821: Calls inode_init.

822: Calls file_table_init.

823: Calls name_cache_init.

831: Calls mem_init.

832: Calls buffer_init.

833: Calls sock_init.

837: Calls dquot_init.

838: Calls arch_syms_export.

839: Calls sti.

840: Calls check_bugs.

844: Calls smp_init.

846: Calls sysctl_init.

852: Calls kernel_thread.

862: Calls cpu_idle.
```

#### setup\_arch (linux/arch/i386/kernel/setup.c:108)

```txt
Control Flow:
start_kernel
    setup_arch <-- Here

120: Assigns one to the local static variable smptrap.

122-150: Copies values from the real-mode boot block into
         the kernel's global data structures/variables. 

200-204: Calls request_region to reserve IO space for i386
         devices.
```

#### request\_region (linux/kernel/resource.c:72)

```txt
Control Flow:
start_kernel
    setup_arch
        request_region <-- Here

77-79: Iterates through the global static iotable to find the
       first unused entry.

83: Calls find_gap.

86-90: Initializes the iotable entry and inserts it at the head
       of iolist in front of root.
```

#### find\_gap (linux/kernel/resource.c:47)

```txt
Control Flow:
start_kernel
    setup_arch
        request_region
            find_gap <-- Here

53-54: Returns NULL if there is an overflow.

55: Calls save_flags.

56: Calls cli to clear the interrupt flag.

57-64: Searches the global static iolist to find out
       where to place the new node.

65: Calls restore_flags.

66: Returns local variable p.
```

#### save\_flags (linux/include/asm-i386/system.h:227)

```txt
Control Flow:
start_kernel
    setup_arch
        request_region
            find_gap
                save_flags <-- Here

227-228: #define save_flags(x) \
__asm__ __volatile__("pushfl ; popl %0":"=g" (x): /* no input */ :"memory")
```

#### restore\_flags (linux/include/asm-i386/system.h:230)

```txt
Control Flow:
start_kernel
    setup_arch
        request_region
            find_gap
                save_flags <-- Here
                restore_flags <-- Here

230: #define restore_flags(x) \
__asm__ __volatile__("pushl %0 ; popfl": /* no output */ :"g" (x):"memory")
```

#### paging\_init (linux/arch/i386/mm/init.c:115)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init <-- Here

133: Calls smp_scan_config.

156-202: Creates an identity map and a virtual map starting at
         0xC0000000.

203: Calls local_flush_tlb.

204: return free_area_init(start_mem, end_mem);
```

#### local\_flush\_tlb (linux/include/asm-i386/pgtable.h:56)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
        local_flush_tlb <-- Here

56: #define local_flush_tlb() __flush_tlb()
```

#### \_\_flush\_tlb (linux/include/asm-i386/pgtable.h:42)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
        local_flush_tlb
            __flush_tlb <-- Here

42-43: #define __flush_tlb() \
do { unsigned long tmpreg; __asm__ __volatile__ \
("movl %%cr3,%0\n\tmovl %0,%%cr3":"=r" (tmpreg) : :"memory"); } while (0)
```

#### free\_area\_init (linux/mm/page\_alloc.c:259)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
        local_flush_tlb
        free_area_init <-- Here

275: Calls init_swap_cache.

288: Calls init_mem_queue.

298: Returns the modified start_mem argument.
```

#### init\_swap\_cache (linux/mm/swap\_state.c:72)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
        local_flush_tlb
        free_area_init
            init_swap_cache <-- Here
```

#### init\_mem\_queue (linux/mm/page\_alloc.c:50)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
        local_flush_tlb
        free_area_init
            init_swap_cache
            init_mem_queue <-- Here
```

#### trap\_init (linux/arch/i386/kernel/traps.c:336)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init <-- Here

348: Increments the static local variable smptrap.

351-371: This is self-explanatory.

...

387: Calls load_TR.

388: Calls load_ldt.
```

#### load\_TR (linux/include/asm-i386/system.h:24)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
        load_TR <-- Here

24: #define load_TR(n) __asm__("ltr %%ax": /* no output */ :"a" (_TSS(n)))
```

#### load\_ldt (linux/include/asm-i386/system.h:25)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
        load_TR
        load_ldt <-- Here
25: #define load_ldt(n) __asm__("lldt %%ax": /* no output */ :"a" (_LDT(n)))
```

#### init\_IRQ (linux/arch/i386/kernel/irq.c:537)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ <-- Here

543: Assigns one to static local variable smptrap.

554: Calls set_intr_gate to initialize IRQ 16.

556: Calls request_region for pic1.

557: Calls request_region for pic2.

558: Calls setup_x86_irq for irq2.

559: Calls setup_x86_irq for irq13.
```

#### set\_intr\_gate (linux/include/asm-i386/system.h:246)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
        set_intr_gate <-- Here

246-247: #define set_intr_gate(n,addr) \
             _set_gate(&idt[n],14,0,addr)
```

#### \_set\_gate (linux/include/asm-i386/system.h:235)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
        set_intr_gate
            _set_gate <-- Here

235-244: #define _set_gate(gate_addr,type,dpl,addr) \
         __asm__ __volatile__ ("movw %%dx,%%ax\n\t" \
             "movw %2,%%dx\n\t" \
             "movl %%eax,%0\n\t" \
             "movl %%edx,%1" \
             :"=m" (*((long *) (gate_addr))), \
              "=m" (*(1+(long *) (gate_addr))) \
             :"i" ((short) (0x8000+(dpl<<13)+(type<<8))), \
              "d" ((char *) (addr)),"a" (KERNEL_CS << 16) \
             :"ax","dx")
```

#### setup\_x86\_irq (linux/arch/i386/kernel/irq.c:396)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
        set_intr_gate
        setup_x86_irq <-- Here
```

#### sched\_init (linux/kernel/sched.c:1639)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init <-- Here
```

#### time\_init (linux/arch/i386/kernel/time.c:456)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init <-- Here
```

#### parse\_options (linux/init/main.c:590)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options <-- Here
```

#### init\_modules (linux/kernel/module.c:63)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules <-- Here
```

#### console\_init (linux/drivers/char/tty\_io.c:1849)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init <-- Here
```

#### pci\_init (linux/drivers/pci/pci.c:955)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init <-- Here
```

#### kmalloc\_init (linux/mm/kmalloc.c:204)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init <-- Here
```

#### sti (linux/include/asm-i386/system.h:224)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti <-- Here

224: #define sti() __asm__ __volatile__ ("sti": : :"memory")
```

#### calibrate\_delay (linux/init/main.c:488)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay <-- Here
```

#### inode\_init (linux/fs/inode.c:133)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init <-- Here
```

#### file\_table\_init (linux/fs/file\_table.c:103)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init
    file_table_init <-- Here

105: return start;
```

#### name\_cache\_init (linux/fs/dcache.c:224)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init
    file_table_init
    name_cache_init <-- Here
```

#### mem\_init (linux/arch/i386/mm/init.c:207)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init
    file_table_init
    name_cache_init
    mem_init <-- Here
```

#### buffer\_init (linux/fs/buffer.c:1436)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init
    file_table_init
    name_cache_init
    mem_init
    buffer_init <-- Here
```

#### sock\_init (linux/net/socket.c:1396)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init
    file_table_init
    name_cache_init
    mem_init
    buffer_init
    sock_init <-- Here
```

#### dquot\_init (linux/fs/dquot.c:905)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init
    file_table_init
    name_cache_init
    mem_init
    buffer_init
    sock_init
    dqout_init <-- Here
```

#### arch\_syms\_export (linux/arch/i386/kernel/ksyms.c:31)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init
    file_table_init
    name_cache_init
    mem_init
    buffer_init
    sock_init
    dqout_init
    arch_syms_export <-- Here
```

#### check\_bugs (linux/include/asm-i386/bugs.h:128)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init
    file_table_init
    name_cache_init
    mem_init
    buffer_init
    sock_init
    dqout_init
    arch_syms_export
    check_bugs <-- Here
```

#### smp\_init (linux/init/main.c:712)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init
    file_table_init
    name_cache_init
    mem_init
    buffer_init
    sock_init
    dqout_init
    arch_syms_export
    check_bugs
    smp_init <-- Here
```

#### sysctl\_init (linux/kernel/sysctl.c:167)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init
    file_table_init
    name_cache_init
    mem_init
    buffer_init
    sock_init
    dqout_init
    arch_syms_export
    check_bugs
    smp_init
    sysctl_init <-- Here
```

#### kernel\_thread (linux/include/asm-i386/unistd.h:299)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init
    file_table_init
    name_cache_init
    mem_init
    buffer_init
    sock_init
    dqout_init
    arch_syms_export
    check_bugs
    smp_init
    sysctl_init
    kernel_thread <-- Here
```

#### cpu\_idle (linux/arch/i386/kernel/process.c:146)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
    time_init
    parse_options
    init_modules
    console_init
    pci_init
    kmalloc_init
    sti
    calibrate_delay
    inode_init
    file_table_init
    name_cache_init
    mem_init
    buffer_init
    sock_init
    dqout_init
    arch_syms_export
    check_bugs
    smp_init
    sysctl_init
    kernel_thread
    cpu_idle <-- Here
```
