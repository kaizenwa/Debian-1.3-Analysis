/*======================================================================

    Device driver for Intel 82365 and compatible PCMCIA controllers

    Written by David Hinds, dhinds@allegro.stanford.edu
    
======================================================================*/

#include <pcmcia/config.h>
#include <pcmcia/k_compat.h>

#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/string.h>

#include <asm/io.h>
#include <asm/bitops.h>
#include <asm/segment.h>
#include <asm/system.h>

#include <linux/kernel.h>
#include <linux/errno.h>
#include <linux/timer.h>
#include <linux/sched.h>
#include <linux/malloc.h>
#include <linux/pci.h>
#include <linux/bios32.h>
#include <linux/ioport.h>

#include <pcmcia/version.h>
#include <pcmcia/ss.h>

/* ISA-to-PCMCIA controllers */
#include "i82365.h"
#include "pd67xx.h"
#include "vg468.h"
#include "ricoh.h"

/* PCI-to-CardBus controllers */
#include "yenta.h"
#include "rl5c466.h"
#include "ti113x.h"
#include "smc34c90.h"
#include "pd6832.h"

#include <pcmcia/cs_types.h>
#include <pcmcia/cs.h>

#ifdef PCMCIA_DEBUG
static int pc_debug = PCMCIA_DEBUG;
static const char *version =
"i82365.c 1.111 1997/04/21 03:08:31 (David Hinds)";
#endif

/*====================================================================*/

/* Parameters that can be set with 'insmod' */

/* Default base address for i82365sl and other ISA chips */
static int i365_base = 0x3e0;

/* Default base address for CardBus controllers */
static int cardbus_base = 0x3e0;

/* Interrupt mode for CardBus controllers */
static int irq_mode = -1;

/* Specify a socket number to ignore */
static int ignore = -1;

/* Should we probe at 0x3e2 for an extra controller? */
static int extra_sockets = 0;

/* Probe for safe interrupts? */
static int do_scan = 1;

/* Bit map of interrupts to choose from */
static u_long irq_mask = 0xffff;

/* The card status change interrupt -- 0 means autoselect */
static int cs_irq = 0;

/* Poll status interval -- 0 means default to interrupt */
static int poll_interval = 0;

/* External clock time, in nanoseconds.  120 ns = 8.33 MHz */
static int cycle_time = 120;

/* These features may or may not be implemented */
static int has_dma = 0;
static int has_led = 1;
static int has_ring = 1;

/* Timing parameters for Vadem VG468 chips */
static int async_clock = 0;

/* For the Cirrus PD6729 PCI chip -- set if bus > 25 MHz */
#ifdef CONFIG_PCI
static int fast_pci = 0;
#endif

/* Timing parameters for Cirrus PD67XX ISA chips */
static int freq_bypass = 0;
static int setup_time = 1;
static int cmd_time = 6;
static int recov_time = 0;
static int wakeup = 0;

MODULE_PARM(i365_base, "i");
MODULE_PARM(cardbus_base, "i");
MODULE_PARM(ignore, "i");
MODULE_PARM(extra_sockets, "i");
MODULE_PARM(do_scan, "i");
MODULE_PARM(irq_mask, "i");
MODULE_PARM(cs_irq, "i");
MODULE_PARM(poll_interval, "i");
MODULE_PARM(cycle_time, "i");
MODULE_PARM(has_dma, "i");
MODULE_PARM(has_led, "i");
MODULE_PARM(has_ring, "i");
MODULE_PARM(async_clock, "i");
#ifdef CONFIG_PCI
MODULE_PARM(fast_pci, "i");
#endif
MODULE_PARM(freq_bypass, "i");
MODULE_PARM(setup_time, "i");
MODULE_PARM(cmd_time, "i");
MODULE_PARM(recov_time, "i");
MODULE_PARM(wakeup, "i");

/*====================================================================*/

static void i365_interrupt IRQ(int irq, void *dev, struct pt_regs *regs);
static int i365_service(u_int sock, u_int cmd, void *arg);

static int grab_irq;
static struct timer_list poll_timer;

typedef struct socket_info_t {
    int		type;
    socket_cap_t cap;
    u_short	ioaddr;
    u_short	psock;
    u_short	cs_irq;
    void	(*handler)(void *info, u_long events);
    void	*info;
} socket_info_t;

/* Controller types */
#define IS_I82365A	0x10
#define IS_I82365B	0x11
#define IS_I82365DF	0x12
#define IS_VLSI		0x20
#define IS_IBM		0x30
#define IS_PD6710	0x40
#define IS_PD672X	0x41
#define IS_PD6729	0x42
#define IS_PD6832	0x43
#define IS_VG468	0x50
#define IS_VG469	0x51
#define IS_RF5Cx96	0x60
#define IS_RL5C466	0x61
#define IS_SMC34C90	0x70
#define IS_TI113X	0x80
#define IS_OM82C092G	0x90

/* Where we keep track of our sockets... */
static int sockets = 0;
static socket_info_t socket_table[8] = {
    { 0, { 0 }, 0, 0, 0, NULL, NULL }, /* ... */
};

/* Socket capabilities */
static socket_cap_t i365_cap = {
    0xdeb8,	/* irq 15, 14, 12, 11, 10, 9, 7, 5, 4, 3 */
};

/*====================================================================*/

static u_char i365_get(u_short sock, u_short reg)
{
    u_short port = socket_table[sock].ioaddr;
    u_char val = I365_REG(socket_table[sock].psock, reg);
    cli();
    outb_p(val, port); val = inb_p(port+1);
    sti();
    return val;
}

static void i365_set(u_short sock, u_short reg, u_char data)
{
    u_short port = socket_table[sock].ioaddr;
    u_char val = I365_REG(socket_table[sock].psock, reg);
    cli();
    outb_p(val, port); outb_p(data, port+1);
    sti();
}

static void i365_bset(u_short sock, u_short reg, u_char mask)
{
    u_char d = i365_get(sock, reg);
    d |= mask;
    i365_set(sock, reg, d);
}

static void i365_bclr(u_short sock, u_short reg, u_char mask)
{
    u_char d = i365_get(sock, reg);
    d &= ~mask;
    i365_set(sock, reg, d);
}

static void i365_bflip(u_short sock, u_short reg, u_char mask, int b)
{
    u_char d = i365_get(sock, reg);
    if (b)
	d |= mask;
    else
	d &= ~mask;
    i365_set(sock, reg, d);
}

static u_short i365_get_pair(u_short sock, u_short reg)
{
    u_short a, b;
    a = i365_get(sock, reg);
    b = i365_get(sock, reg+1);
    return (a + (b<<8));
}

static void i365_set_pair(u_short sock, u_short reg, u_short data)
{
    i365_set(sock, reg, data & 0xff);
    i365_set(sock, reg+1, data >> 8);
}

/*====================================================================*/

static void busy_loop(u_long len)
{
    u_long flags, timeout = jiffies + len;
    save_flags(flags);
    sti();
    while (timeout >= jiffies)
	;
    restore_flags(flags);
} /* busy_loop */

/*====================================================================*/

static volatile u_long irq_hits;

static void irq_count IRQ(int irq, void *dev, struct pt_regs *regs)
{
    irq_hits++;
}

static u_long try_irq(u_short psock, int irq)
{
    if (REQUEST_IRQ(irq, irq_count, 0, "irq scan", NULL) != 0)
	return -1;
    irq_hits = 0;
    busy_loop(HZ/50);
    if (irq_hits) {
	FREE_IRQ(irq, NULL);
	return -1;
    }

    /* Generate one interrupt */
    i365_set(psock, I365_CSCINT, I365_CSC_DETECT | (irq << 4));
    i365_bclr(psock, I365_INTCTL, I365_INTR_ENA);
    i365_bset(psock, I365_GENCTL, I365_CTL_SW_IRQ);

    busy_loop(HZ/50);
    FREE_IRQ(irq, NULL);

    /* Turn off interrupts */
    i365_set(psock, I365_CSCINT, 0);
    
    return (irq_hits != 1);
}

static u_long irq_scan(u_short lsock, u_long mask0)
{
    u_short psock;
    u_long mask1, mask2;
    int i;

#ifdef __alpha__
#define PIC 0x4d0
    /* Don't probe level-triggered interrupts -- reserved for PCI */
    int level_mask = inb_p(PIC) | (inb_p(PIC+1) << 8);
    if (level_mask)
	mask0 &= ~level_mask;
#endif
    
    psock = socket_table[lsock].psock;
    mask1 = mask2 = 0;
    i365_set(psock, I365_CSCINT, 0);
    for (i = 0; i < 16; i++)
	if ((mask0 & (1 << i)) && (try_irq(psock, i) == 0))
	    mask1 |= (1 << i);
    for (i = 0; i < 16; i++)
	if ((mask0 & (1 << i)) && (try_irq(psock, i) == 0)) {
	    mask2 |= (1 << i);
	}
    return (mask1 & mask2);
}

/*====================================================================*/

/* Time conversion functions */

static int to_cycles(int ns)
{
    return ns/cycle_time;
} /* speed_convert */

static int to_ns(int cycles)
{
    return cycle_time*cycles;
}

/*====================================================================*/

static int identify(u_short port, u_short sock)
{
    u_char val;
    int type = -1;

    /* Use the next free entry in the socket table */
    socket_table[sockets].ioaddr = port;
    socket_table[sockets].psock = sock;
    
    /* Wake up a sleepy Cirrus controller */
    if (wakeup) {
	i365_bclr(sockets, PD67_MISC_CTL_2, PD67_MC2_SUSPEND);
	/* Pause at least 50 ms */
	busy_loop(HZ/20);
    }
    
    if ((val = i365_get(sockets, I365_IDENT)) & 0x70)
	return -1;
    switch (val) {
    case 0x82:
	type = IS_I82365A; break;
    case 0x83:
	type = IS_I82365B; break;
    case 0x84:
	type = IS_I82365DF; break;
    case 0x88: case 0x89: case 0x8a:
	type = IS_IBM; break;
    }
    
    /* Check for Vadem VG-468 chips */
    outb_p(0x0e, port);
    outb_p(0x37, port);
    i365_bset(sockets, VG468_MISC, VG468_MISC_VADEMREV);
    val = i365_get(sockets, I365_IDENT);
    if (val & I365_IDENT_VADEM) {
	i365_bclr(sockets, VG468_MISC, VG468_MISC_VADEMREV);
	type = ((val & 7) >= 4) ? IS_VG469 : IS_VG468;
    }

    /* Check for Ricoh chips */
    val = i365_get(sockets, RF5C_CHIP_ID);
    if ((val == RF5C_CHIP_RF5C296) || (val == RF5C_CHIP_RF5C396))
	type = IS_RF5Cx96;
    
    /* Check for Cirrus CL-PD67xx chips */
    i365_set(sockets, PD67_CHIP_INFO, 0);
    val = i365_get(sockets, PD67_CHIP_INFO);
    if ((val & PD67_INFO_CHIP_ID) == PD67_INFO_CHIP_ID) {
	val = i365_get(sockets, PD67_CHIP_INFO);
	if ((val & PD67_INFO_CHIP_ID) == 0)
	    type = (val & PD67_INFO_SLOTS) ? IS_PD672X : IS_PD6710;
    }
    return type;
} /* identify */

/*====================================================================*/

static void add_socket(u_short port, int psock, int type)
{
    socket_table[sockets].ioaddr = port;
    socket_table[sockets].psock = psock;
    socket_table[sockets].type = type;
    sockets++;
}

static void add_pcic(u_short port, int ns, int type)
{
    int i, irq, base;
    u_long mask, scan;

    request_region(port, 2, "i82365");
    if (sockets == ns) printk("\n" KERN_INFO "  ");
    switch (type) {
    case IS_VLSI:
	printk("VLSI 82C146"); break;
    case IS_I82365A:
	printk("Intel i82365sl A step"); break;
    case IS_I82365B:
	printk("Intel i82365sl B step"); break;
    case IS_I82365DF:
	printk("Intel i82365sl DF"); break;
    case IS_PD6710:
	printk("Cirrus PD6710"); break;
    case IS_PD672X:
	printk("Cirrus PD672x");
	if (has_dma) printk(", DMA");
	if (has_led) printk(", LED");
	if (has_ring) printk(", RING");
	break;
    case IS_PD6729:
	printk("Cirrus PD6729 PCI"); break;
    case IS_PD6832:
	printk("Cirrus PD6832 CardBus"); break;
    case IS_IBM:
	printk("IBM Clone"); break;
    case IS_VG468:
	printk("Vadem VG-468"); break;
    case IS_VG469:
	printk("Vadem VG-469"); break;
    case IS_RF5Cx96:
	printk("Ricoh RF5C296/396"); break;
    case IS_RL5C466:
	printk("Ricoh RL5C466 CardBus"); break;
    case IS_SMC34C90:
	printk("SMC 34C90 CardBus"); break;
    case IS_TI113X:
	printk("TI 113X CardBus"); break;
    case IS_OM82C092G:
	printk("Omega Micro 82C092G PCI"); break;
    }
    printk(" at %#x", port);
    printk(", %d socket%s\n", ns, ((ns > 1) ? "s" : ""));

    /* Build interrupt mask; scan if possible */
    base = sockets-ns;
    printk(KERN_INFO "  irq mask (");
    mask = i365_cap.irq_mask & irq_mask;
    switch (type) {
    case IS_PD6710:
    case IS_PD6729:
    case IS_PD672X:
	if (has_dma)  mask &= ~0x0600; /* disable irq 9,10 */
	if (has_led)  mask &= ~0x1000; /* disable irq 12 */
	if (has_ring) mask &= ~0x8000; /* disable irq 15 */
	printk("Cirrus");
	break;
    default:
	if (do_scan && (scan = irq_scan(base, mask))) {
	    mask = scan;
	    printk("scanned");
	} else 
	    printk("default");
	break;
    }
    printk(") = 0x%04lx, ", mask);
    
    /* Poll if only two interrupts available */
    if (poll_interval == 0) {
	scan = (mask & (mask-1));
	if ((scan & (scan-1)) == 0)
	    poll_interval = HZ;
    }

    /* Pick a card status change interrupt */
    irq = 0;
    if (poll_interval == 0) {
	if (cs_irq == 0) {
	    for (cs_irq = 15; cs_irq > 0; cs_irq--)
		if ((cs_irq != 12) && (mask & (1 << cs_irq)) &&
		    (REQUEST_IRQ(cs_irq, irq_count, 0, "i", NULL) == 0))
		    break;
	    irq = cs_irq;
	} else {
	    /* Make sure the selected interrupt is in the mask. */
	    if ((mask & (1 << cs_irq)) &&
		(REQUEST_IRQ(cs_irq, irq_count, 0, "i", NULL) == 0))
		irq = cs_irq;
	}
	if (irq == 0)
	    poll_interval = HZ;
	else {
	    FREE_IRQ(irq, NULL);
	    grab_irq = 1;
	}
    }
    
    if (irq)
	printk("status change on irq %d\n", irq);
    else
	printk("polled status, interval = %d\n", poll_interval);
    
    /* Turn on interrupts, but with all events masked */
    for (i = base; i < base+ns; i++) {
	socket_table[i].cap.irq_mask = mask;
	socket_table[i].cs_irq = irq;
	if (irq) {
	    i365_set(i, I365_CSCINT, (irq << 4));
	    i365_bclr(i, I365_INTCTL, I365_INTR_ENA);
	    i365_get(i, I365_CSC);
	}
    }

} /* add_pcic */

/*======================================================================

    See if a card is present, powered up, in IO mode, and already
    bound to a (non-PCMCIA) Linux driver.

    We make an exception for cards that seem to be serial devices.
    
======================================================================*/

static int is_active(u_short port, u_short sock)
{
    u_char stat;
    u_short start, stop;
    
    /* Use the next free entry in the socket table */
    socket_table[sockets].ioaddr = port;
    socket_table[sockets].psock = sock;
    
    stat = i365_get(sockets, I365_STATUS);
    start = i365_get_pair(sockets, I365_IO(0)+I365_W_START);
    stop = i365_get_pair(sockets, I365_IO(0)+I365_W_STOP);
    if ((stat & I365_CS_DETECT) && (stat & I365_CS_POWERON) &&
	(i365_get(sockets, I365_INTCTL) & I365_PC_IOCARD) &&
	(i365_get(sockets, I365_ADDRWIN) & I365_ENA_IO(0)) &&
	(check_region(start, stop-start+1) != 0) &&
	((start & 0xfeef) != 0x02e8))
	return 1;
    else
	return 0;
}

/*====================================================================*/

#ifdef CONFIG_PCI

#ifndef PCI_VENDOR_ID_OMEGA
#define PCI_VENDOR_ID_OMEGA 0x119b
#define PCI_DEVICE_ID_OMEGA_82C092G 0x1221
#endif

#ifndef PCI_FUNC
#define PCI_FUNC(devfn) ((devfn)&7)
#endif

#define pci_readb pcibios_read_config_byte
#define pci_writeb pcibios_write_config_byte
#define pci_readw pcibios_read_config_word
#define pci_writew pcibios_write_config_word
#define pci_readl pcibios_read_config_dword
#define pci_writel pcibios_write_config_dword

typedef struct {
    u_short	vendor;
    u_short	device;
    u_short	type;
} pci_id_t;

static pci_id_t pci_id[] = {
    { PCI_VENDOR_ID_RICOH, PCI_DEVICE_ID_RICOH_RL5C466, IS_RL5C466 },
    { PCI_VENDOR_ID_CIRRUS, PCI_DEVICE_ID_CIRRUS_6832, IS_PD6832 },
    { PCI_VENDOR_ID_SMC, PCI_DEVICE_ID_SMC_34C90, IS_SMC34C90 },
    { PCI_VENDOR_ID_TI, PCI_DEVICE_ID_TI_1130, IS_TI113X },
    { PCI_VENDOR_ID_TI, PCI_DEVICE_ID_TI_1131, IS_TI113X }
};
#define PCI_COUNT (sizeof(pci_id)/sizeof(pci_id_t))

/* Default settings for PCI command configuration register */
#define CMD_DFLT (PCI_COMMAND_IO|PCI_COMMAND_MEMORY| \
		  PCI_COMMAND_MASTER|PCI_COMMAND_WAIT)

static void pci_setup(int type, u_char bus, u_char devfn,
		      u_short addr, u_short index)
{
    u_short s, bcr;

    pci_writew(bus, devfn, PCI_COMMAND, CMD_DFLT);
    /* Enable ISA interrupt steering */
    pci_readw(bus, devfn, CB_BRIDGE_CONTROL, &bcr);
    bcr |= CB_BCR_ISA_IRQ;
    
    switch (type) {
    case IS_TI113X:
	pci_readw(bus, devfn, TI113X_DEVICE_CONTROL, &s);
	if ((s & TI113X_DCR_IMODE_MASK) == 0)
	    s |= TI113X_DCR_IMODE_ISA;
	pci_writew(bus, devfn, TI113X_DEVICE_CONTROL, s);
	break;

    case IS_RL5C466:
	s = RL5C466_16CTL_LEVEL_1 | RL5C466_16CTL_LEVEL_2 |
	    RL5C466_16CTL_IO_TIMING | RL5C466_16CTL_MEM_TIMING;
	if (index & 2) s |= RL5C466_16CTL_INDEX_SEL;
	pci_writew(bus, devfn, RL5C466_16BIT_CTL, s);
	bcr |= (index & 4) ? RL5C466_BCR_3E2_ENA : RL5C466_BCR_3E0_ENA;
	break;
	
    case IS_PD6832:
	pci_writew(bus, devfn, PD6832_SOCKET_NUMBER, index);
	bcr |= PD6832_BCR_MGMT_IRQ_ENA;
	break;
    }
    
    pci_writew(bus, devfn, CB_BRIDGE_CONTROL, bcr);
    /* Enable 16-bit legacy mode */
    addr += (index>>2) * 2;
    pci_writel(bus, devfn, CB_LEGACY_MODE_BASE, addr | 0x01);
    add_socket(addr, index & 3, type);
}

static void pci_probe(void)
{
    u_short i, index;
    u_char bus, devfn;
    u_int addr;
    
    /* Check for Cirrus PD6729 PCI controller(s) */
    for (index = 0; ; index++) {
	if (pcibios_find_device(PCI_VENDOR_ID_CIRRUS,
				PCI_DEVICE_ID_CIRRUS_6729,
				index, &bus, &devfn) != 0)
	    break;
	pci_readl(bus, devfn, PCI_BASE_ADDRESS_0, &addr);
	addr &= ~0x1;
	pci_writew(bus, devfn, PCI_COMMAND, CMD_DFLT);
	add_socket(addr, 0, IS_PD6729);
	add_socket(addr, 1, IS_PD6729);
	add_pcic(addr, 2, IS_PD6729);
    }
    
    /* Check for Omega Micro 82C092G controller(s) */
    for (index = 0; ; index++) {
	if (pcibios_find_device(PCI_VENDOR_ID_OMEGA,
				PCI_DEVICE_ID_OMEGA_82C092G,
				index, &bus, &devfn) != 0)
	    break;
	pci_readl(bus, devfn, PCI_BASE_ADDRESS_0, &addr);
        addr &= ~0x1;
	pci_writew(bus, devfn, PCI_COMMAND, CMD_DFLT);
	add_socket(addr, 0, IS_OM82C092G);
	add_socket(addr, 1, IS_OM82C092G);
	add_pcic(addr, 2, IS_OM82C092G);
    }

    /* Check for CardBus controllers */
    addr = cardbus_base;
    for (i = 0; i < PCI_COUNT; i++) {
	for (index = 0; index < 8; index++) {
	    if (pcibios_find_device(pci_id[i].vendor,
				    pci_id[i].device,
				    index, &bus, &devfn) != 0)
		break;
	    if (PCI_FUNC(devfn) == 0) {
		pci_setup(pci_id[i].type, bus, devfn, addr,
			  (index & (~0x0001)));
		pci_setup(pci_id[i].type, bus, devfn+1, addr,
			  (index | 0x0001));
	    }
	}
	if (index > 4) {
	    add_pcic(addr, 4, pci_id[i].type);
	    index -= 4; addr += 2;
	}
	if (index > 0) {
	    add_pcic(addr, index, pci_id[i].type);
	    addr += 2;
	}
    }

}
#endif /* CONFIG_PCI */

/*====================================================================*/

static void isa_probe(void)
{
    int i, j, sock, k;
    int id, type;
    u_short port;
    
    if (check_region(i365_base, 2) != 0) {
	if (sockets == 0)
	    printk("port conflict at %#x\n", i365_base);
	return;
    }

    type = identify(i365_base, 0);
    if ((type == IS_I82365DF) && (identify(i365_base, 1) != type)) {
	for (i = 0; i < 4; i++) {
	    if (i == ignore) continue;
	    port = i365_base + ((i & 1) << 2) + ((i & 2) << 1);
	    sock = (i & 1) << 1;
	    if ((identify(port, sock) == IS_I82365DF) &&
		!is_active(port, sock)) {
		add_socket(port, sock, IS_VLSI);
		add_pcic(port, 1, IS_VLSI);
	    }
	}
    } else {
	for (i = 0; i < (extra_sockets ? 8 : 4); i += 2) {
	    port = i365_base + 2*(i>>2);
	    sock = (i & 3);
	    id = identify(port, sock);
	    if (id < 0) continue;

	    for (j = 0; j < 2; j++) {
		/* Does the socket exist? */
		if ((ignore == i+j) || (identify(port, sock+j) < 0) ||
		    is_active(port, sock+j))
		    continue;
		/* Check for bad socket decode */
		i365_set(sockets, I365_MEM(0)+I365_W_OFF, sockets);
		for (k = 0; k < sockets; k++) {
		    if (i365_get(k, I365_MEM(0)+I365_W_OFF) != k) {
			i365_set(k, I365_MEM(0)+I365_W_OFF, k);
			break;
		    }
		}
		if ((i365_get(sockets, I365_MEM(0)+I365_W_OFF) !=
		     sockets) || (k != sockets))
		    break;
		add_socket(port, j, type);
	    }
	    if (j != 0) add_pcic(port, j, type);
	}
    }
}

/*====================================================================*/

static int i365_init(void)
{
    int i;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_INFO "%s\n", version);
#endif
    printk(KERN_INFO "Intel PCIC probe: ");
    sockets = 0;

#ifdef CONFIG_PCI
    if (pcibios_present())
	pci_probe();
#endif

    isa_probe();
	
    if (sockets == 0) {
	printk("not found.\n");
	return -ENODEV;
    }
    
    /* Set up interrupt handler, and/or polling */
    if (grab_irq != 0)
	REQUEST_IRQ(cs_irq, i365_interrupt, 0, "i82365", NULL);
    if (poll_interval != 0) {
	poll_timer.function = (void (*)(u_long))i365_interrupt;
	poll_timer.data = 0;
	poll_timer.prev = poll_timer.next = NULL;
    }
    
    if (register_ss_entry(sockets, &i365_service) != 0) {
	printk(KERN_NOTICE "i82365: register_ss_entry() failed\n");
	if (grab_irq != 0)
	    FREE_IRQ(cs_irq, NULL);
	for (i = 0; i < sockets; i++)
	    release_region(socket_table[i].ioaddr, 2);
	return -ENODEV;
    }

    /* Finally, schedule a polling interrupt */
    if (poll_interval != 0) {
    	poll_timer.expires = RUN_AT(poll_interval);
	add_timer(&poll_timer);
    }
    
    return 0;
    
} /* i365_init */
  
/*====================================================================*/

static void i365_finish(void)
{
    int i;
    unregister_ss_entry(&i365_service);
    if (poll_interval != 0)
	del_timer(&poll_timer);
    if (grab_irq != 0)
	FREE_IRQ(cs_irq, NULL);
    for (i = 0; i < sockets; i++) {
	i365_set(socket_table[i].psock, I365_CSCINT, 0);
	release_region(socket_table[i].ioaddr, 2);
    }
} /* i365_finish */

/*====================================================================*/

static void i365_interrupt IRQ(int irq, void *dev, struct pt_regs *regs)
{
    int i, j, csc;
    u_long events, active;
    
#ifdef PCMCIA_DEBUG
    static volatile int active = 0;
    if (active) {
	printk(KERN_NOTICE "i82365: reentered interrupt handler!\n");
	return;
    }
    else
	active = 1;
    if (pc_debug > 1)
	printk(KERN_DEBUG "i82365: i365_interrupt()\n");
#endif

    for (j = 0; j < 20; j++) {
	active = 0;
	for (i = 0; i < sockets; i++) {
	    if (socket_table[i].handler == NULL) continue;
	    csc = i365_get(i, I365_CSC);
	    if (csc == 0) continue;
	    events = (csc & I365_CSC_DETECT) ? SS_DETECT : 0;
	    if (i365_get(i, I365_INTCTL) & I365_PC_IOCARD)
		events |= (csc & I365_CSC_STSCHG) ? SS_STSCHG : 0;
	    else {
		events |= (csc & I365_CSC_BVD1) ? SS_BATDEAD : 0;
		events |= (csc & I365_CSC_BVD2) ? SS_BATWARN : 0;
		events |= (csc & I365_CSC_READY) ? SS_READY : 0;
	    }
	    if (events)
		socket_table[i].handler(socket_table[i].info, events);
	    active |= events;
	}
	if (!active) break;
    }
    if (j == 20)
	printk(KERN_NOTICE "i82365: infinite loop in interrupt handler\n");

    /* Schedule next poll, if needed */
    if (poll_interval > 0) {
	poll_timer.expires = RUN_AT(poll_interval);
	add_timer(&poll_timer);
    }
    
#ifdef PCMCIA_DEBUG
    active = 0;
    if (pc_debug > 1)
	printk(KERN_DEBUG "i82365: done\n");
#endif

} /* i365_interrupt */

/*====================================================================*/

static int i365_register_callback(u_short sock, ss_callback_t *call)
{
    if (call == NULL) {
	socket_table[sock].handler = NULL;
	MOD_DEC_USE_COUNT;
    }
    else {
	MOD_INC_USE_COUNT;
	socket_table[sock].handler = call->handler;
	socket_table[sock].info = call->info;
    }
    return 0;
} /* i365_register_callback */

/*====================================================================*/

static int i365_get_status(u_short sock, u_int *value)
{
    u_char status;
    
    status = i365_get(sock, I365_STATUS);
    *value = ((status & I365_CS_DETECT) == I365_CS_DETECT)
	? SS_DETECT : 0;
    if (i365_get(sock, I365_INTCTL) & I365_PC_IOCARD)
	*value |= (status & I365_CS_STSCHG) ? 0 : SS_STSCHG;
    else {
	*value |= (status & I365_CS_BVD1) ? 0 : SS_BATDEAD;
	*value |= (status & I365_CS_BVD2) ? 0 : SS_BATWARN;
    }
    *value |= (status & I365_CS_WRPROT) ? SS_WRPROT : 0;
    *value |= (status & I365_CS_READY) ? SS_READY : 0;
    *value |= (status & I365_CS_POWERON) ? SS_POWERON : 0;
#ifdef PCMCIA_DEBUG
    if (pc_debug > 1)
	printk(KERN_DEBUG "i82365: GetStatus(%d) = %#2.2x\n",
	       sock, *value);
#endif
    return 0;
} /* i365_get_status */

/*====================================================================*/

static int i365_inquire_socket(u_short sock, socket_cap_t *cap)
{
    *cap = socket_table[sock].cap;
    return 0;
} /* i365_inquire_socket */

/*====================================================================*/

static int i365_get_socket(u_short sock, socket_state_t *state)
{
    u_char reg, vcc, vpp;
    
    reg = i365_get(sock, I365_POWER);
    state->flags = (reg & I365_PWR_AUTO) ? SS_PWR_AUTO : 0;
    state->flags |= (reg & I365_PWR_OUT) ? SS_OUTPUT_ENA : 0;
    vcc = reg & I365_VCC_MASK; vpp = reg & I365_VPP_MASK;
    state->Vcc = state->Vpp = 0;
    switch (socket_table[sock].type) {
    case IS_PD672X:
    case IS_PD6710:
    case IS_PD6729:
	if (i365_get(sock, PD67_MISC_CTL_1) & PD67_MC1_VCC_3V) {
	    if (reg & I365_VCC_5V) state->Vcc = 33;
	    if (vpp == I365_VPP_5V) state->Vpp = 33;
	    if (vpp == I365_VPP_12V) state->Vpp = 120;
	}
	else {
	    if (reg & I365_VCC_5V) state->Vcc = 50;
	    if (vpp == I365_VPP_5V) state->Vpp = 50;
	    if (vpp == I365_VPP_12V) state->Vpp = 120;
	}
	break;
    case IS_VG469:
	if (i365_get(sock, VG469_VSELECT) & VG469_VSEL_VCC) {
	    if (reg & I365_VCC_5V) state->Vcc = 33;
	    if (vpp == I365_VPP_5V) state->Vpp = 33;
	    if (vpp == I365_VPP_12V) state->Vpp = 120;
	}
	else {
	    if (reg & I365_VCC_5V) state->Vcc = 50;
	    if (vpp == I365_VPP_5V) state->Vpp = 50;
	    if (vpp == I365_VPP_12V) state->Vpp = 120;
	}
	break;
    case IS_I82365DF:
	if (vcc == I365_VCC_3V) state->Vcc = 33;
	if (vcc == I365_VCC_5V) state->Vcc = 50;
	if (vpp == I365_VPP_5V) state->Vpp = 50;
	if (vpp == I365_VPP_12V) state->Vpp = 120;
	break;
    default:
	if (reg & I365_VCC_5V) {
	    state->Vcc = 50;
	    if (vpp == I365_VPP_5V) state->Vpp = 50;
	    if (vpp == I365_VPP_12V) state->Vpp = 120;
	}
	break;
    }
    reg = i365_get(sock, I365_INTCTL);
    state->flags |= (reg & I365_PC_IOCARD) ? SS_IOCARD : 0;
    state->flags |= (reg & I365_PC_RESET) ? 0 : SS_RESET;

    /* IO card, RESET flags, IO interrupt */
    reg = i365_get(sock, I365_INTCTL);
    state->flags |= (reg & I365_PC_RESET) ? 0 : SS_RESET;
    if (reg & I365_PC_IOCARD) state->flags |= SS_IOCARD;
    state->io_irq = reg & I365_IRQ_MASK;
    
    /* DMA mode, speaker control */
    switch (socket_table[sock].type) {
    case IS_PD672X:
    case IS_PD6710:
    case IS_PD6729:
	if (i365_get(sock, PD67_MISC_CTL_1) & PD67_MC1_SPKR_ENA)
	    state->flags |= SS_SPKR_ENA;
	if (i365_get(sock, PD67_MISC_CTL_2) & PD67_MC2_DMA_MODE)
	    state->flags |= SS_DMA_MODE;
	break;
    default:
    }
    
    /* Card status change mask */
    reg = i365_get(sock, I365_CSCINT);
    state->csc_mask = (reg & I365_CSC_DETECT) ? SS_DETECT : 0;
    if (state->flags & SS_IOCARD)
	state->csc_mask |= (reg & I365_CSC_STSCHG) ? SS_STSCHG : 0;
    else {
	state->csc_mask |= (reg & I365_CSC_BVD1) ? SS_BATDEAD : 0;
	state->csc_mask |= (reg & I365_CSC_BVD2) ? SS_BATWARN : 0;
	state->csc_mask |= (reg & I365_CSC_READY) ? SS_READY : 0;
    }
    
#ifdef PCMCIA_DEBUG
    if (pc_debug > 1)
	printk(KERN_DEBUG "i82365: GetSocket(%d) = flags %#3.3lx, "
	       "Vcc %d, Vpp %d, io_irq %d, csc_mask %#2.2lx\n", sock,
	       state->flags, state->Vcc, state->Vpp, state->io_irq,
	       state->csc_mask);
#endif
    return 0;
} /* i365_get_socket */

/*====================================================================*/

static int i365_set_socket(u_short sock, socket_state_t *state)
{
    int type = socket_table[sock].type;
    u_char reg;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug > 1)
	printk(KERN_DEBUG "i82365: SetSocket(%d, flags %#3.3lx, "
	       "Vcc %d, Vpp %d, io_irq %d, csc_mask %#2.2lx)\n",
	       sock, state->flags, state->Vcc, state->Vpp,
	       state->io_irq, state->csc_mask);
#endif

    /* Did something put the controller to sleep? */
    if ((type == IS_PD6710) || (type == IS_PD672X) ||
	(type == IS_PD6729)) {
	if (i365_get(sock, PD67_MISC_CTL_2) & PD67_MC2_SUSPEND) {
	    i365_bclr(sock, PD67_MISC_CTL_2, PD67_MC2_SUSPEND);
	    /* Pause at least 50 ms */
	    busy_loop(HZ/20);
	}
    }
    
    /* IO card, RESET flags, IO interrupt */
    reg = i365_get(sock, I365_INTCTL);
    reg &= ~(I365_PC_IOCARD | I365_PC_RESET | I365_IRQ_MASK);
    /* Note that the reset signal is inverted */
    reg |= (state->flags & SS_RESET) ? 0 : I365_PC_RESET;
    reg |= (state->flags & SS_IOCARD) ? I365_PC_IOCARD : 0;
    reg |= state->io_irq;
    i365_set(sock, I365_INTCTL, reg);

    reg = I365_PWR_NORESET;
    if (state->flags & SS_PWR_AUTO) reg |= I365_PWR_AUTO;
    if (state->flags & SS_OUTPUT_ENA) reg |= I365_PWR_OUT;
    
    switch (type) {

    case IS_PD6729:
    case IS_PD672X:
    case IS_PD6710:
    case IS_VG469:
	if (state->Vpp != 0) {
	    if (state->Vpp == 120)
		reg |= I365_VPP_12V;
	    else if (state->Vpp == state->Vcc)
		reg |= I365_VPP_5V;
	    else return -EINVAL;
	}
	if (state->Vcc != 0) {
	    if (state->Vcc == 33) {
		reg |= I365_VCC_5V;
		if (type == IS_VG469)
		    i365_bset(sock, VG469_VSELECT, VG469_VSEL_VCC);
		else /* Cirrus */
		    i365_bset(sock, PD67_MISC_CTL_1, PD67_MC1_VCC_3V);
	    }
	    else if (state->Vcc == 50) {
		reg |= I365_VCC_5V;
		if (type == IS_VG469)
		    i365_bclr(sock, VG469_VSELECT, VG469_VSEL_VCC);
		else /* Cirrus */
		    i365_bclr(sock, PD67_MISC_CTL_1, PD67_MC1_VCC_3V);
	    }
	    else return -EINVAL;
	}
	break;

    case IS_I82365DF:
	switch (state->Vcc) {
	case 0:		break;
	case 33:   	reg |= I365_VCC_3V; break;
	case 50:	reg |= I365_VCC_5V; break;
	default:	return -EINVAL;
	}
	switch (state->Vpp) {
	case 0:		break;
	case 50:   	reg |= I365_VPP_5V; break;
	case 120:	reg |= I365_VPP_12V; break;
	default:	return -EINVAL;
	}
	break;
	
    default:
	switch (state->Vcc) {
	case 0:		break;
	case 50:	reg |= I365_VCC_5V; break;
	default:	return -EINVAL;
	}
	switch (state->Vpp) {
	case 0:		break;
	case 50:	reg |= I365_VPP_5V; break;
	case 120:	reg |= I365_VPP_12V; break;
	default:	return -EINVAL;
	}
	break;
	
    }
    
    if (reg != i365_get(sock, I365_POWER))
	i365_set(sock, I365_POWER, reg);

    /* Chipset-specific functions */
    switch (type) {
    case IS_VG468:
    case IS_VG469:
	i365_bflip(sock, VG468_CTL, VG468_CTL_ASYNC, async_clock);
	break;
#ifdef CONFIG_PCI
    case IS_PD6729:
	i365_bflip(sock, PD67_MISC_CTL_2, PD67_MC2_FAST_PCI, fast_pci);
	/* Fall through... */
#endif
    case IS_PD6710:
    case IS_PD672X:
	/* Special pin functions */
	i365_bclr(sock, PD67_MISC_CTL_2, PD67_MC2_DYNAMIC_MODE);
	i365_bflip(sock, PD67_MISC_CTL_2, PD67_MC2_LED_ENA, has_led);
	i365_bflip(sock, PD67_MISC_CTL_2, PD67_MC2_IRQ15_RI, has_ring);
	/* Speaker control */
	i365_bflip(sock, PD67_MISC_CTL_1, PD67_MC1_SPKR_ENA,
		   state->flags & SS_SPKR_ENA);
	/* DMA control */
	i365_set(sock, PD67_EXT_INDEX, PD67_DMA_CTL);
	reg = i365_get(sock, PD67_EXT_DATA) & ~PD67_DMA_MODE;
	if (state->flags & SS_DMA_MODE) {
	    reg |= PD67_DMA_DREQ_BVD2;
	    i365_set(sock, PD67_EXT_DATA, reg);
	    i365_bset(sock, PD67_MISC_CTL_2, PD67_MC2_DMA_MODE);
	}
	else {
	    i365_bclr(sock, PD67_MISC_CTL_2, PD67_MC2_DMA_MODE);
	    i365_set(sock, PD67_EXT_DATA, reg);
	}
	/* Timing parameters */
	i365_bflip(sock, PD67_MISC_CTL_2, PD67_MC2_FREQ_BYPASS,
		   freq_bypass);
	i365_set(sock, PD67_TIME_SETUP(0), setup_time);
	i365_set(sock, PD67_TIME_SETUP(1), setup_time);
	i365_set(sock, PD67_TIME_CMD(0), cmd_time);
	i365_set(sock, PD67_TIME_CMD(1), cmd_time + 9);
	i365_set(sock, PD67_TIME_RECOV(0), recov_time);
	i365_set(sock, PD67_TIME_RECOV(1), recov_time);
	break;
    default:
	break;
    }
    
    /* Card status change interrupt mask */
    reg = (socket_table[sock].cs_irq << 4);
    if (state->csc_mask & SS_DETECT) reg |= I365_CSC_DETECT;
    if (state->flags & SS_IOCARD) {
	if (state->csc_mask & SS_STSCHG) reg |= I365_CSC_STSCHG;
    }
    else {
	if (state->csc_mask & SS_BATDEAD) reg |= I365_CSC_BVD1;
	if (state->csc_mask & SS_BATWARN) reg |= I365_CSC_BVD2;
	if (state->csc_mask & SS_READY) reg |= I365_CSC_READY;
    }
    i365_set(sock, I365_CSCINT, reg);
    i365_get(sock, I365_CSC);
    
    return 0;
} /* i365_set_socket */

/*====================================================================*/

static int i365_get_io_map(u_short sock, struct pcmcia_io_map *io)
{
    u_char map, ioctl, addr;
    
    map = io->map;
    if (map > 1) return -EINVAL;
    io->start = i365_get_pair(sock, I365_IO(map)+I365_W_START);
    io->stop = i365_get_pair(sock, I365_IO(map)+I365_W_STOP);
    ioctl = i365_get(sock, I365_IOCTL);
    addr = i365_get(sock, I365_ADDRWIN);
    io->speed = to_ns(ioctl & I365_IOCTL_WAIT(map)) ? 1 : 0;
    io->flags  = (addr & I365_ENA_IO(map)) ? MAP_ACTIVE : 0;
    io->flags |= (ioctl & I365_IOCTL_0WS(map)) ? MAP_0WS : 0;
    io->flags |= (ioctl & I365_IOCTL_16BIT(map)) ? MAP_16BIT : 0;
    io->flags |= (ioctl & I365_IOCTL_IOCS16(map)) ? MAP_AUTOSZ : 0;
#ifdef PCMCIA_DEBUG
    if (pc_debug > 1)
	printk(KERN_DEBUG "i82365: GetIOMap(%d, %d) = %#2.2x, %d ns, "
	       "%#4.4x-%#4.4x\n", sock, map, io->flags,
	       io->speed, io->start, io->stop);
#endif
    return 0;
} /* i365_get_io_map */

/*====================================================================*/

static int i365_set_io_map(u_short sock, struct pcmcia_io_map *io)
{
    u_char map, ioctl;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug > 1)
	printk(KERN_DEBUG "i82365: SetIOMap(%d, %d, %#2.2x, %d ns, "
	       "%#4.4x-%#4.4x)\n", sock, io->map, io->flags,
	       io->speed, io->start, io->stop);
#endif
    map = io->map;
    if ((map > 1) || (io->start > 0xffff) || (io->stop > 0xffff) ||
	(io->stop < io->start)) return -EINVAL;
    /* Turn off the window before changing anything */
    if (i365_get(sock, I365_ADDRWIN) & I365_ENA_IO(map))
	i365_bclr(sock, I365_ADDRWIN, I365_ENA_IO(map));
    i365_set_pair(sock, I365_IO(map)+I365_W_START, io->start);
    i365_set_pair(sock, I365_IO(map)+I365_W_STOP, io->stop);
    ioctl = i365_get(sock, I365_IOCTL) & ~I365_IOCTL_MASK(map);
    if (io->speed) ioctl |= I365_IOCTL_WAIT(map);
    if (io->flags & MAP_0WS) ioctl |= I365_IOCTL_0WS(map);
    if (io->flags & MAP_16BIT) ioctl |= I365_IOCTL_16BIT(map);
    if (io->flags & MAP_AUTOSZ) ioctl |= I365_IOCTL_IOCS16(map);
    i365_set(sock, I365_IOCTL, ioctl);
    /* Turn on the window if necessary */
    if (io->flags & MAP_ACTIVE)
	i365_bset(sock, I365_ADDRWIN, I365_ENA_IO(map));
    return 0;
} /* i365_set_io_map */

/*====================================================================*/

static int i365_get_mem_map(u_short sock, struct pcmcia_mem_map *mem)
{
    u_short base, i;
    u_char map, addr;
    
    map = mem->map;
    if (map > 4) return -EINVAL;
    addr = i365_get(sock, I365_ADDRWIN);
    mem->flags = (addr & I365_ENA_MEM(map)) ? MAP_ACTIVE : 0;
    base = I365_MEM(map);
    
    i = i365_get_pair(sock, base+I365_W_START);
    mem->flags |= (i & I365_MEM_16BIT) ? MAP_16BIT : 0;
    mem->flags |= (i & I365_MEM_0WS) ? MAP_0WS : 0;
    mem->sys_start = ((long)(i & 0x0fff) << 12);
    
    i = i365_get_pair(sock, base+I365_W_STOP);
    mem->speed  = (i & I365_MEM_WS0) ? 1 : 0;
    mem->speed += (i & I365_MEM_WS1) ? 2 : 0;
    mem->speed = to_ns(mem->speed);
    mem->sys_stop = ((long)(i & 0x0fff) << 12) + 0x0fff;
    
    i = i365_get_pair(sock, base+I365_W_OFF);
    mem->flags |= (i & I365_MEM_WRPROT) ? MAP_WRPROT : 0;
    mem->flags |= (i & I365_MEM_REG) ? MAP_ATTRIB : 0;
    mem->card_start = ((long)(i & 0x3fff) << 12) + mem->sys_start;
    mem->card_start &= 0x3ffffff;
#ifdef PCMCIA_DEBUG
    if (pc_debug > 1)
	printk(KERN_DEBUG "i82365: GetMemMap(%d, %d) = %#2.2x, %d ns, "
	       "%#5.5lx-%#5.5lx, %#5.5lx\n", sock, mem->map,
	       mem->flags, mem->speed, mem->sys_start, mem->sys_stop,
	       mem->card_start);
#endif
    return 0;
} /* i365_get_mem_map */

/*====================================================================*/
  
static int i365_set_mem_map(u_short sock, struct pcmcia_mem_map *mem)
{
    u_short base, i;
    u_char map;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug > 1)
	printk(KERN_DEBUG "i82365: SetMemMap(%d, %d, %#2.2x, %d ns, "
	       "%#5.5lx-%#5.5lx, %#5.5lx)\n", sock, mem->map,
	       mem->flags, mem->speed, mem->sys_start, mem->sys_stop,
	       mem->card_start);
#endif
    map = mem->map;
    if ((map > 4) || (mem->card_start > 0x3ffffff) ||
	(mem->sys_start > 0xffffff) || (mem->sys_stop > 0xffffff) ||
	(mem->sys_start > mem->sys_stop) || (mem->speed > 1000))
	return -EINVAL;
    /* Turn off the window before changing anything */
    if (i365_get(sock, I365_ADDRWIN) & I365_ENA_MEM(map))
	i365_bclr(sock, I365_ADDRWIN, I365_ENA_MEM(map));
    base = I365_MEM(map);
    
    i = mem->sys_start >> 12;
    if (mem->flags & MAP_16BIT) i |= I365_MEM_16BIT;
    if (mem->flags & MAP_0WS) i |= I365_MEM_0WS;
    i365_set_pair(sock, base+I365_W_START, i);
    
    i = mem->sys_stop >> 12;
    switch (to_cycles(mem->speed)) {
    case 0:	break;
    case 1:	i |= I365_MEM_WS0; break;
    case 2:	i |= I365_MEM_WS1; break;
    default:	i |= I365_MEM_WS1 | I365_MEM_WS0; break;
    }
    i365_set_pair(sock, base+I365_W_STOP, i);
    
    i = ((mem->card_start - mem->sys_start) >> 12) & 0x3fff;
    if (mem->flags & MAP_WRPROT) i |= I365_MEM_WRPROT;
    if (mem->flags & MAP_ATTRIB) i |= I365_MEM_REG;
    i365_set_pair(sock, base+I365_W_OFF, i);
    
    /* Turn on the window if necessary */
    if (mem->flags & MAP_ACTIVE)
	i365_bset(sock, I365_ADDRWIN, I365_ENA_MEM(map));
    return 0;
} /* i365_set_mem_map */

/*====================================================================*/

typedef int (*subfn_t)(u_short, void *);

static subfn_t service_table[] = {
    (subfn_t)&i365_register_callback,
    (subfn_t)&i365_get_status,
    (subfn_t)&i365_inquire_socket,
    (subfn_t)&i365_get_socket,
    (subfn_t)&i365_set_socket,
    (subfn_t)&i365_get_io_map,
    (subfn_t)&i365_set_io_map,
    (subfn_t)&i365_get_mem_map,
    (subfn_t)&i365_set_mem_map,
};

#define NFUNC (sizeof(service_table)/sizeof(subfn_t))

static int i365_service(u_int sock, u_int cmd, void *arg)
{
    int err;

#ifdef PCMCIA_DEBUG
    if (pc_debug > 1) 
	printk(KERN_DEBUG "i365_ioctl(%d, %d, 0x%p)\n",
	       sock, cmd, arg);
#endif

    if (cmd < NFUNC)
	err = service_table[cmd](sock, arg);
    else
	err = -EINVAL;

    return err;
} /* i365_ioctl */

/*====================================================================*/

int init_module(void)
{
    servinfo_t serv;
    CardServices(GetCardServicesInfo, &serv);
    if (serv.Revision != CS_RELEASE_CODE) {
	printk(KERN_NOTICE "i82365: Card Services release "
	       "does not match!\n");
	return -1;
    }
    return i365_init();
}

void cleanup_module(void)
{
    i365_finish();
}
