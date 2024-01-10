## Walkthrough of Linux Kernel 2.0.30's Kernel Initialization Code

#### start\_kernel (linux/init/main.c:768)

```txt
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
```

#### kernel\_thread (linux/include/asm-i386/unistd.h:299)

```txt
```

#### cpu\_idle (linux/arch/i386/kernel/process.c:146)

```txt
```
