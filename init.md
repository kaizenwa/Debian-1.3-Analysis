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

1653: Calls init_bh for timer_bh.

1654: Calls init_bh for tqueue_bh.

1655: Calls init_bh for immediate_bh.
```

#### init\_bh (linux/include/linux/interrupt.h:44)

```txt
Control Flow:
start_kernel
    setup_arch
    paging_init
    trap_init
    init_IRQ
    sched_init
        init_bh <-- Here
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

1853: Calls tty_register_ldisc.

1859-1864: Initializes the global tty_std_termios structure.

1871: return con_init(kmem_start);
```

#### tty\_register\_ldisc (linux/drivers/char/tty\_io.c:199)

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
        tty_register_ldisc <-- Here
```

#### con\_init (linux/drivers/char/console.c:1995)

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
        tty_register_ldisc
        con_init <-- Here
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

957: Calls pcibios_init.

959: Calls pcibios_present.

970: Calls pcibios_fixup.

981: return mem_start;
```

#### pcibios\_init (linux/arch/i386/kernel/bios32.c:872)

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
        pcibios_init <-- Here
```

#### pcibios\_present (linux/arch/i386/kernel/bios32.c:755)

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
        pcibios_init
        pcibios_present <-- Here

757: return access_pci ? 1 : 0;
```

#### pcibios\_fixup (linux/arch/i386/kernel/bios32.c:865)

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
        pcibios_init
        pcibios_present
        pcibios_fixup <-- Here

867: return mem_start;
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

1413: Calls init_netlink.

1420: Calls netlink_attach.

1428: Calls fwchain_init.

1435: Calls proto_init.

1442: Calls export_net_symbols.
```

#### init\_netlink (linux/net/netlink.c:231)

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
        init_netlink <-- Here
```

#### netlink\_attach (linux/net/netlink.c:192)

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
        init_netlink
        netlink_attach <-- Here
```

#### fwchain\_init (linux/net/core/firewall.c:159)

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
        init_netlink
        netlink_attach
        fwchain_init <-- Here
```

#### proto\_init (linux/net/socket.c:1380)

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
        init_netlink
        netlink_attach
        fwchain_init
        proto_init <-- Here
```

#### export\_net\_symbols (linux/net/netsyms.c:190)

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
        init_netlink
        netlink_attach
        fwchain_init
        proto_init
        export_net_symbols <-- Here
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

715: Calls smp_boot_cpus.

...
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

170: Calls register_proc_table.
```

#### register\_proc\_table (linux/kernel/sysctl.c:396)

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
        register_proc_table <-- Here
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

#### init (linux/init/main.c:913)

```txt
Control Flow:
init <-- Here

921: Calls kernel_thread to create the bdflush daemon.

923: Calls kernel_thread to create the kswapd daemon.

931: Calls sys_setup.

939: Calls smp_begin.

980: Calls open on "/dev/tty1".

984-985: Calls sys_dup to create STDOUT and STDERR.

988-991: Calls execve at multiple file locations to create
         the init process. We assume this always succeeds.
```

#### sys\_setup (linux/fs/filesystems.c:33)

```txt
Control Flow:
init
    sys_setup <-- Here

39: Assigns zero to local static variable callable.

41: Calls device_setup.

42: Calls binfmt_setup.

50: Calls init_ext2_fs.

82: Calls init_nfs_fs.

113: Calls mount_root.

114: Returns zero.
```

#### device\_setup (linux/drivers/block/genhd.c:723)

```txt
Control Flow:
init
    sys_setup
        device_setup <-- Here

738: Calls chr_dev_init.

739: Calls blk_dev_init.

740: Calls sti.

742: Calls scsi_dev_init.

745: Calls net_dev_init.

747: Calls console_map_init.

750: Calls setup_dev.
```

#### chr\_dev\_init (linux/drivers/char/mem.c:387)

```txt
Control Flow:
init
    sys_setup
        device_setup
            chr_dev_init <-- Here
```

#### blk\_dev\_init (linux/drivers/block/ll\_rw\_blk.c:610)

```txt
Control Flow:
init
    sys_setup
        device_setup
            chr_dev_init
            blk_dev_init <-- Here
```

#### scsi\_dev\_init (linux/drivers/scsi/scsi.c:2561)

```txt
Control Flow:
init
    sys_setup
        device_setup
            chr_dev_init
            blk_dev_init
            scsi_dev_init <-- Here
```

#### net\_dev\_init (linux/net/core/dev.c:1492)

```txt
Control Flow:
init
    sys_setup
        device_setup
            chr_dev_init
            blk_dev_init
            scsi_dev_init
            net_dev_init <-- Here
```

#### console\_map\_init (linux/drivers/char/consolemap.c:479)

```txt
Control Flow:
init
    sys_setup
        device_setup
            chr_dev_init
            blk_dev_init
            scsi_dev_init
            net_dev_init
            console_map_init <-- Here
```

#### setup\_dev (linux/drivers/block/genhd.c:709)

```txt
Control Flow:
init
    sys_setup
        device_setup
            chr_dev_init
            blk_dev_init
            scsi_dev_init
            net_dev_init
            console_map_init
            setup_dev <-- Here

719: Calls hd_geninit.
```

#### hd\_geninit (linux/drivers/block/hd.c:954)

```txt
Control Flow:
init
    sys_setup
        device_setup
            chr_dev_init
            blk_dev_init
            scsi_dev_init
            net_dev_init
            console_map_init
            setup_dev
                hd_geninit <-- Here
```

#### binfmt\_setup (linux/fs/exec.c:64)

```txt
Control Flow:
init
    sys_setup
        device_setup
        binfmt_setup <-- Here

67: Calls init_elf_binfmt.

78: Calls init_script_binfmt.
```

#### init\_elf\_binfmt (linux/fs/binfmt\_elf.c:1266)

```txt
Control Flow:
init
    sys_setup
        device_setup
        binfmt_setup
            init_elf_binfmt <-- Here

1268: return register_binfmt(&elf_format);
```

#### register\_binfmt (linux/fs/exec.c:81)

```txt
Control Flow:
init
    sys_setup
        device_setup
        binfmt_setup
            init_elf_binfmt
                register_binfmt <-- Here
```

#### init\_script\_binfmt (linux/fs/binfmt\_script.c:106)

```txt
Control Flow:
init
    sys_setup
        device_setup
        binfmt_setup
            init_elf_binfmt
            init_script_binfmt <-- Here

107: return register_binfmt(&script_format);
```

#### init\_ext2\_fs (fs/ext2/super.c:686)

```txt
Control Flow:
init
    sys_setup
        device_setup
        binfmt_setup
        init_ext2_fs <-- Here

688: return register_filesystem(&ext2_fs_type);
```

#### register\_filesystem (linux/fs/super.c:156)

```txt
Control Flow:
init
    sys_setup
        device_setup
        binfmt_setup
        init_ext2_fs
            register_filesystem <-- Here
```

#### init\_nfs\_fs (linux/fs/nfs/inode.c:352)

```txt
Control Flow:
init
    sys_setup
        device_setup
        binfmt_setup
        init_ext2_fs
        init_nfs_fs <-- Here

355-358: Calls kernel_thread to create four run_nfsiod threads.

359: return register_filesystem(&nfs_fs_type);
```

#### mount\_root (linux/fs/super.c:1031)

```txt
Control Flow:
init
    sys_setup
        device_setup
        binfmt_setup
        init_ext2_fs
        init_nfs_fs
        mount_root <-- Here

1033: Calls memset to bzero the global super_blocks array.

1034: Calls do_mount_root.

114: Returns zero.
```

#### do\_mount\_root (linux/fs/super.c:922)

```txt
Control Flow:
init
    sys_setup
        device_setup
        binfmt_setup
        init_ext2_fs
        init_nfs_fs
        mount_root
            do_mount_root <-- Here
```

#### smp\_begin (linux/init/main.c:756)

```txt
Control Flow:
init
    sys_setup
    smp_begin <-- Here

758: Assigns one to global variable smp_threads_ready.

759: Calls smp_commence.
```

#### smp\_commence (linu/arch/i386/kernel/smp.c:527)

```txt
Control Flow:
init
    sys_setup
    smp_begin
        smp_commence <-- Here

532: Assigns one to the global variable smp_commenced.
```

#### sys\_dup (linux/fs/fcntl.c:50)

```txt
Control Flow:
init
    sys_setup
    smp_begin
    sys_dup <-- Here

52: return dupfd(fildes,0);
```

#### dupfd (linux/fs/fcntl.c:20)

```txt
Control Flow:
init
    sys_setup
    smp_begin
    sys_dup
        dupfd <-- Here
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
