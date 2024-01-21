/* 
 * Xircom Creditcard Netwave Adapter driver v 0.2.0
 * 
 * Copyright:
 *   John Markus Bjørndalen
 *   Department of Computer Science
 *   University of Tromsø
 *   Norway             
 *   johnm@staff.cs.uit.no, http://www.cs.uit.no/~johnm/
 */

/* To have statistics (just packets sent) define this */
#undef XIRCNW_STATS
#define PCMCIA_DEBUG 0

#include <pcmcia/config.h>
#include <pcmcia/k_compat.h>

#include <assert.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/interrupt.h>
#include <linux/ptrace.h>
#include <linux/ioport.h>
#include <linux/in.h>
#include <linux/malloc.h>
#include <linux/string.h>
#include <linux/timer.h>
#include <asm/system.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/dma.h>
#include <linux/errno.h>

#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>

#include <pcmcia/version.h>
#include <pcmcia/cs_types.h>
#include <pcmcia/cs.h>
#include <pcmcia/cistpl.h>
#include <pcmcia/cisreg.h>
#include <pcmcia/ds.h>
#include <pcmcia/mem_op.h>

#define XIRCOM_REGOFF         0x8000
/* The Xircom IO registers, offsets to iobase */
#define XIRCOM_REG_COR        0x0
#define XIRCOM_REG_CCSR       0x2
#define XIRCOM_REG_ASR        0x4
#define XIRCOM_REG_IMR        0xa
#define XIRCOM_REG_PMR        0xc
#define XIRCOM_REG_IOLOW      0x6
#define XIRCOM_REG_IOHI       0x7
#define XIRCOM_REG_IOCONTROL  0x8
#define XIRCOM_REG_DATA       0xf
/* The Xircom Extended IO registers, offsets to RamBase */
#define XIRCOM_EREG_ASCC      0x114
#define XIRCOM_EREG_RSER      0x120
#define XIRCOM_EREG_RSERW     0x124
#define XIRCOM_EREG_TSER      0x130
#define XIRCOM_EREG_CB        0x100
#define XIRCOM_EREG_SPCQ      0x154
#define XIRCOM_EREG_SPU       0x155
#define XIRCOM_EREG_LIF       0x14e
#define XIRCOM_EREG_ISPLQ     0x156
#define XIRCOM_EREG_HHC       0x158
#define XIRCOM_EREG_NI        0x16e
#define XIRCOM_EREG_MHS       0x16b
#define XIRCOM_EREG_TDP       0x140
#define XIRCOM_EREG_RDP       0x150
#define XIRCOM_EREG_PA        0x160
#define XIRCOM_EREG_EC        0x180
#define XIRCOM_EREG_CRP       0x17a
#define XIRCOM_EREG_ARW       0x166

/*
 * Commands used in the extended command buffer
 * XIRCOM_EREG_CB (0x100-0x10F) 
 */
#define XIRCOM_CMD_NOP        0x00
#define XIRCOM_CMD_SRC        0x01
#define XIRCOM_CMD_STC        0x02
#define XIRCOM_CMD_AMA        0x03
#define XIRCOM_CMD_DMA        0x04
#define XIRCOM_CMD_SAMA       0x05
#define XIRCOM_CMD_ER         0x06
#define XIRCOM_CMD_DR         0x07
#define XIRCOM_CMD_TL         0x08
#define XIRCOM_CMD_SRP        0x09
#define XIRCOM_CMD_SSK        0x0a
#define XIRCOM_CMD_SMD        0x0b
#define XIRCOM_CMD_SAPD       0x0c
#define XIRCOM_CMD_SSS        0x11
/* End of Command marker */
#define XIRCOM_CMD_EOC        0x00

#define TX_TIMEOUT  20

static const unsigned int imrConfRFU1 = 0x10; /* RFU interrupt mask, keep high */
static const unsigned int imrConfIENA = 0x02; /* Interrupt enable */

static const unsigned int corConfIENA   = 0x01; /* Interrupt enable */
static const unsigned int corConfLVLREQ = 0x40; /* Keep high */

static const unsigned int rxConfRxEna  = 0x80; /* Receive Enable */
static const unsigned int rxConfMAC    = 0x20; /* MAC host receive mode*/ 
static const unsigned int rxConfPro    = 0x10; /* Promiscuous */
static const unsigned int rxConfAMP    = 0x08; /* Accept Multicast Packets */
static const unsigned int rxConfBcast  = 0x04; /* Accept Broadcast Packets */

static const unsigned int txConfTxEna  = 0x80; /* Transmit Enable */
static const unsigned int txConfMAC    = 0x20; /* Host sends MAC mode */
static const unsigned int txConfEUD    = 0x10; /* Enable Uni-Data packets */
static const unsigned int txConfKey    = 0x02; /* Scramble data packets */
static const unsigned int txConfLoop   = 0x01; /* Loopback mode */

int xinw_debug = 0;

/*
   All the PCMCIA modules use PCMCIA_DEBUG to control debugging.  If
   you do not define PCMCIA_DEBUG at all, all the debug code will be
   left out.  If you compile with PCMCIA_DEBUG=0, the debug code will
   be present but disabled -- but it can then be enabled for specific
   modules at load time with a 'pc_debug=#' option to insmod.
*/
#ifdef PCMCIA_DEBUG
static int pc_debug = PCMCIA_DEBUG;
MODULE_PARM(pc_debug, "i");
static char *version =
"xircnw_cs.c 0.2.0 1996/05/22 15:05:51 (John Markus Bjørndalen)\n";
#endif

static dev_info_t dev_info = "xircnw_cs";

/*====================================================================*/

/* Parameters that can be set with 'insmod' */

/* Choose the domain, default is 0x100 */
static u_int  domain = 0x100;

/* Scramble key, range from 0x0 to 0xffff.  
 * 0x0 is no scrambling. 
 */
static u_int  scramble_key = 0x0;

/* Shared memory speed, in ns. The documentation states that 
 * the card should not be read faster than every 400ns. 
 * This timing should be provided by the HBA. If it becomes a 
 * problem, try setting mem_speed to 400. 
 */
static int mem_speed = 0;

/* Bit map of interrupts to choose from */
/* This means pick from 15, 14, 12, 11, 10, 9, 7, 5, 4, and 3 */
static u_long irq_mask = 0xdeb8;

MODULE_PARM(domain, "i");
MODULE_PARM(scramble_key, "i");
MODULE_PARM(mem_speed, "i");
MODULE_PARM(irq_mask, "i");

/*====================================================================*/

/*
   The event() function is this driver's Card Services event handler.
   It will be called by Card Services when an appropriate card status
   event is received.  The config() and release() entry points are
   used to configure or release a socket, in response to card insertion
   and ejection events.  They are invoked from the skeleton event
   handler.
   You'll also need to prototype all the functions that will actually
   be used to talk to your device.  See 'pcmem_cs' for a good example
   of a fully self-sufficient driver; the other drivers rely more or
   less on other parts of the kernel.
*/

static void xircnw_config(dev_link_t *arg);
static void xircnw_release(u_long arg);
static int xircnw_event(event_t event, int priority,
			  event_callback_args_t *args);
/* static ushort read_eeprom(short iobase, int index); */
static void xinw_doreset(unsigned long iobase, u_char* ramBase);
static void xircnw_reset(struct device *dev);
/* static void check_if_port(struct device *dev); */
static int xinw_config(struct device *dev, struct ifmap *map);
static int xinw_open(struct device *dev);
static int xinw_start_xmit(struct sk_buff *skb, struct device *dev);
static void xinw_interrupt IRQ(int irq, void *dev_id, struct pt_regs *regs);
static void update_stats(struct device *dev);
static struct net_device_stats *xinw_get_stats(struct device *dev);
static int xinw_rx(struct device *dev);
static int xinw_close(struct device *dev);

#ifdef NEW_MULTICAST
static void set_multicast_list(struct device *dev);
#else
static void set_multicast_list(struct device *dev, int num_addrs, void *addrs);
#endif

/*
   The attach() and detach() entry points are used to create and destroy
   "instances" of the driver, where each instance represents everything
   needed to manage one actual PCMCIA card.
*/

static dev_link_t *xircnw_attach(void);
static void xircnw_detach(dev_link_t *);

/*
   A linked list of "instances" of the skeleton device.  Each actual
   PCMCIA card corresponds to one device instance, and is described
   by one dev_link_t structure (defined in ds.h).

   You may not want to use a linked list for this -- for example, the
   memory card driver uses an array of dev_link_t pointers, where minor
   device numbers are used to derive the corresponding array index.
*/

static dev_link_t *dev_list = NULL;

/*
   A dev_link_t structure has fields for most things that are needed
   to keep track of a socket, but there will usually be some device
   specific information that also needs to be kept track of.  The
   'priv' pointer in a dev_link_t structure can be used to point to
   a device-specific private data structure, like this.

   A driver needs to provide a dev_node_t structure for each device
   on a card.  In some cases, there is only one device per card (for
   example, ethernet cards, modems).  In other cases, there may be
   many actual or logical devices (SCSI adapters, memory cards with
   multiple partitions).  The dev_node_t structures need to be kept
   in a linked list starting at the 'dev' field of a dev_link_t
   structure.  We allocate them in the card's private data structure,
   because they generally can't be allocated dynamically.
*/
   
typedef struct xircom_private {
    dev_node_t	node;
    u_char      *ramBase;
    int        	timeoutCounter;
    struct net_device_stats stats;
} xircom_private;

#ifdef XIRCNW_STATS
static struct net_device_stats *xircnw_get_stats(struct device *dev);
#endif

/*
 * The Xircom card is little-endian, so won't work for big endian
 * systems.
 */

inline unsigned short get_uint16(u_char* staddr)
{
    return readw(staddr); /* Return only 16 bits */
}

inline short get_int16(u_char* staddr)
{
    assert(2 == sizeof(short));
    return readw(staddr);
}

/*====================================================================*/

static void cs_error(int func, int ret)
{
    CardServices(ReportError, dev_info, (void *)func, (void *)ret);
}



/* 
 * Wait until the WOC bit in the ASR is asserted. This should have
 * aborted if it takes too long time. 
 */
inline void wait_WOC(unsigned int iobase)
{
    while ((inb(iobase + XIRCOM_REG_ASR) & 0x8) != 0x8) ; 
}

int xircnw_init(struct device *dev)
{
    /* We do all the initialization of this in xircnw_attach instead */
    
    return 0;
}



/*======================================================================

    xircnw_attach() creates an "instance" of the driver, allocating
    local data structures for one device.  The device is registered
    with Card Services.

    The dev_link structure is initialized, but we don't actually
    configure the card at this point -- we wait until we receive a
    card insertion event.
    
======================================================================*/

static dev_link_t *xircnw_attach(void)
{
    client_reg_t client_reg;
    dev_link_t *link;
    struct device *dev;
    int ret;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "xircnw_attach()\n");
#endif

    /* Initialize the dev_link_t structure */
    link = kmalloc(sizeof(struct dev_link_t), GFP_KERNEL);
    memset(link, 0, sizeof(struct dev_link_t));
    link->release.function = &xircnw_release;
    link->release.data = (u_long)link;

    /* The io structure describes IO port mapping */
    link->io.NumPorts1 = 16;
    link->io.Attributes1 = IO_DATA_PATH_WIDTH_16;
/*    link->io.NumPorts2 = 16; 
    link->io.Attributes2 = IO_DATA_PATH_WIDTH_16; */
    link->io.IOAddrLines = 5;

    /* Interrupt setup */
    link->irq.Attributes = IRQ_TYPE_EXCLUSIVE | IRQ_HANDLE_PRESENT;
    link->irq.IRQInfo1 = IRQ_INFO2_VALID|IRQ_LEVEL_ID;
    link->irq.IRQInfo2 = irq_mask;
    link->irq.Handler = &xinw_interrupt;
    
    /* General socket configuration */
    link->conf.Attributes = CONF_ENABLE_IRQ;
    link->conf.Vcc = 50;
    link->conf.IntType = INT_MEMORY_AND_IO;
    link->conf.ConfigIndex = 1;
    link->conf.Present = PRESENT_OPTION;

    /* Allocate space for private device-specific data */
    dev = kmalloc(sizeof(struct device), GFP_KERNEL);
    memset(dev, 0, sizeof(struct device));

    dev->priv = kmalloc(sizeof(xircom_private), GFP_KERNEL);
    memset(dev->priv, 0, sizeof(xircom_private));

    /* Xircnw specific entries in the device structure */
    dev->hard_start_xmit = &xinw_start_xmit;
    dev->set_config = &xinw_config;
    dev->get_stats  = &xinw_get_stats;
    dev->set_multicast_list = &set_multicast_list;
    ether_setup(dev);
    dev->name = ((struct xircom_private *)dev->priv)->node.dev_name;
    dev->init = &xircnw_init;
    dev->open = &xinw_open;
    dev->stop = &xinw_close;
    dev->tbusy = 1;
    link->priv = dev;
    
    /* Register with Card Services */
    link->next = dev_list;
    dev_list = link;
    client_reg.dev_info = &dev_info;
    client_reg.Attributes = INFO_IO_CLIENT | INFO_CARD_SHARE;
    client_reg.EventMask =
	CS_EVENT_CARD_INSERTION | CS_EVENT_CARD_REMOVAL |
	CS_EVENT_RESET_PHYSICAL | CS_EVENT_CARD_RESET |
	CS_EVENT_PM_SUSPEND | CS_EVENT_PM_RESUME;
    client_reg.event_handler = &xircnw_event;
    client_reg.Version = 0x0210;
    client_reg.event_callback_args.client_data = link;
    ret = CardServices(RegisterClient, &link->handle, &client_reg);
    if (ret != 0) {
	cs_error(RegisterClient, ret);
	xircnw_detach(link);
	return NULL;
    }

    return link;
} /* xircnw_attach */

/*======================================================================

    This deletes a driver "instance".  The device is de-registered
    with Card Services.  If it has been released, all local data
    structures are freed.  Otherwise, the structures will be freed
    when the device is released.

======================================================================*/

static void xircnw_detach(dev_link_t *link)
{
    dev_link_t **linkp;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "xircnw_detach(0x%p)\n", link);
#endif
    
    /* Locate device structure */
    for (linkp = &dev_list; *linkp; linkp = &(*linkp)->next)
	if (*linkp == link) break;
    if (*linkp == NULL)
	return;

    /*
       If the device is currently configured and active, we won't
       actually delete it yet.  Instead, it is marked so that when
       the release() function is called, that will trigger a proper
       detach().
    */
    if (link->state & DEV_CONFIG) {
	xircnw_release((u_long) link);
	if (link->state & DEV_STALE_CONFIG) {
	    if (xinw_debug)
	        printk(KERN_DEBUG "xircnw_cs: detach postponed, '%s' still locked\n",
	               link->dev->dev_name);
	    link->state |= DEV_STALE_LINK;
	    return;
	}
    }

    /* Break the link with Card Services */
    if (link->handle)
	CardServices(DeregisterClient, link->handle);
    
    /* Unlink device structure, free pieces */
    *linkp = link->next;
    if (link->priv) {
	struct device *dev = link->priv;
	if (dev->priv)
	    kfree_s(dev->priv, sizeof(xircom_private));
	kfree_s(link->priv, sizeof(struct device));
    }
    kfree_s(link, sizeof(struct dev_link_t));
    
} /* xircnw_detach */


/*======================================================================

    xircnw_config() is scheduled to run after a CARD_INSERTION event
    is received, to configure the PCMCIA socket, and to make the
    ethernet device available to the system.
    
======================================================================*/

static void xircnw_config(dev_link_t *link)
{
    client_handle_t handle;
    tuple_t tuple;
    cisparse_t parse;
    struct device *dev;
    int i, j;
    u_char buf[64];
    win_req_t req;
    memreq_t mem;
    u_char *ramBase = NULL;
/*    modwin_t mod;
    short iobase, *phys_addr;
    */  
    handle = link->handle;
    dev = link->priv;

#ifdef PCMCIA_DEBUG
    if (pc_debug) 
	printk(KERN_DEBUG "xircnw_config(0x%p)\n", link);
#endif

    /*
       This reads the card's CONFIG tuple to find its configuration
       registers.
    */
    do {
	tuple.Attributes = 0;
	tuple.DesiredTuple = CISTPL_CONFIG;
	i = CardServices(GetFirstTuple, handle, &tuple);
	if (i != CS_SUCCESS) break;
	tuple.TupleData = (cisdata_t *) buf;
	tuple.TupleDataMax = 64;
	tuple.TupleOffset = 0;
	i = CardServices(GetTupleData, handle, &tuple);
	if (i != CS_SUCCESS) break;
	i = CardServices(ParseTuple, handle, &tuple, &parse);
	if (i != CS_SUCCESS) break;
	link->conf.ConfigBase = parse.config.base;
	link->conf.Present = parse.config.rmask[0];
    } while (0);
    if (i != CS_SUCCESS) {
	cs_error(ParseTuple, i);
	link->state &= ~DEV_CONFIG_PENDING;
	return;
    }
    
    /* Configure card */
    link->state |= DEV_CONFIG;

    do {
	
	/*
	 *  Try allocating IO ports.  This tries a few fixed addresses.
	 *  If you want, you can also read the card's config table to
	 *  pick addresses -- see the serial driver for an example.
	 */
	for (j = 0x300; j < 0x400; j += 0x20) {
	    link->io.BasePort1 = j;
/*	    link->io.BasePort2 = j+0x10; */
	    i = CardServices(RequestIO, link->handle, &link->io);
	    if (i == CS_SUCCESS) break;
	}
	if (i != CS_SUCCESS) {
	    cs_error(RequestIO, i);
	    break;
	}
	
	/*
	 *  Now allocate an interrupt line.  Note that this does not
	 *  actually assign a handler to the interrupt.
	 */
	i = CardServices(RequestIRQ, link->handle, &link->irq);
	if (i != CS_SUCCESS) {
	    cs_error(RequestIRQ, i);
	    break;
	}
	
	/*
	 *  This actually configures the PCMCIA socket -- setting up
	 *  the I/O windows and the interrupt mapping.
	 */
	i = CardServices(RequestConfiguration, link->handle, &link->conf);
	if (i != CS_SUCCESS) {
	    cs_error(RequestConfiguration, i);
	    break;
	}

	/*
	 *  Allocate a 32K memory window.  Note that the dev_link_t
	 *  structure provides space for one window handle -- if your
	 *  device needs several windows, you'll need to keep track of
	 *  the handles in your private data structure, link->priv.
	 */
	if (xinw_debug)
	    printk(KERN_DEBUG "Setting mem speed of %d\n", mem_speed);
	req.Attributes = WIN_DATA_WIDTH_8|WIN_MEMORY_TYPE_CM|WIN_ENABLE;
	req.Base = NULL;
	req.Size = 0x8000;
	req.AccessSpeed = mem_speed;
	link->win = (window_handle_t)link->handle;
	i = CardServices(RequestWindow, &link->win, &req);
	if (i != 0) {
	    cs_error(RequestWindow, i);
	    break;
	}
	mem.CardOffset = 0x20000; mem.Page = 0; 
	i = CardServices(MapMemPage, link->win, &mem);
	if (i != 0) {
	    cs_error(MapMemPage, i);
	    break;
	}

	/* Store base address of the common window frame */
	((xircom_private*)dev->priv)->ramBase = req.Base;
	ramBase = req.Base;

	dev->irq = link->irq.AssignedIRQ;
	dev->base_addr = link->io.BasePort1;
	dev->tbusy = 0;
	i = register_netdev(dev);
	if (i != 0) {
	    printk(KERN_DEBUG "xircnw_cs: register_netdev() failed\n");
	    break;
	}
    } while (0);

    link->state &= ~DEV_CONFIG_PENDING;
    /* If any step failed, release any partially configured state */
    if (i != 0) {
	xircnw_release((u_long)link);
	return;
    }

    link->dev = &((xircom_private *)dev->priv)->node;


    /* Reset card before reading physical address */
    xinw_doreset(dev->base_addr, ramBase);
    

    /* Read the ethernet address and fill in the Netwave registers. */
    for (i = 0; i < 6; i++) 
	dev->dev_addr[i] = readb(ramBase + XIRCOM_EREG_PA + i);

    if (xinw_debug) {
        printk(KERN_DEBUG "xircnw_config: %s irq %d, io %lx\n", 
	       dev->name, dev->irq, dev->base_addr);
        printk(KERN_DEBUG "xircnw_config: memory base is %lx  id %c%c\n",
	       (u_long) ramBase, 
	       readb(ramBase+XIRCOM_EREG_NI),
	       readb(ramBase+XIRCOM_EREG_NI+1));
    }

    printk(KERN_DEBUG "xircnw_config: phys addr: %02x:%02x:%02x:%02x:%02x:%02x\n", 
	   readb(ramBase+XIRCOM_EREG_PA+0),
	   readb(ramBase+XIRCOM_EREG_PA+1),
	   readb(ramBase+XIRCOM_EREG_PA+2),
	   readb(ramBase+XIRCOM_EREG_PA+3),
	   readb(ramBase+XIRCOM_EREG_PA+4),
	   readb(ramBase+XIRCOM_EREG_PA+5));

    /* get revision words */
    printk(KERN_DEBUG "Xircom_reset: revision %04x %04x\n", 
	   get_uint16(ramBase + XIRCOM_EREG_ARW),
	   get_uint16(ramBase + XIRCOM_EREG_ARW+2));

    printk("Xircom Creditcard Netwave device loaded\n");
} /* xircnw_config */

/*======================================================================

    After a card is removed, xircnw_release() will unregister the net
    device, and release the PCMCIA configuration.  If the device is
    still open, this will be postponed until it is closed.
    
======================================================================*/

static void xircnw_release(u_long arg)
{
    dev_link_t *link = (dev_link_t *)arg;
    struct device *dev = link->priv;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "xircnw_release(0x%p)\n", link);
#endif

    /*
       If the device is currently in use, we won't release until it
       is actually closed.
    */
    if (link->open) {
	printk(KERN_DEBUG "xircnw_cs: release postponed, '%s' still open\n",
	       link->dev->dev_name);
	link->state |= DEV_STALE_CONFIG;
	return;
    }

    if (link->dev != '\0')
	unregister_netdev(dev);
    
    /* Unlink the device chain */
    link->dev = NULL;
    
    /* Don't bother checking to see if these succeed or not */
    CardServices(ReleaseWindow, link->win); 
    CardServices(ReleaseConfiguration, link->handle);
    CardServices(ReleaseIO, link->handle, &link->io);
    CardServices(ReleaseIRQ, link->handle, &link->irq);
    if (dev->irq != 0)
	irq2dev_map[dev->irq] = NULL;

    link->state &= ~DEV_CONFIG;
    
    if (link->state & DEV_STALE_LINK)
	xircnw_detach(link);
    
} /* xircnw_release */

/*======================================================================

    The card status event handler.  Mostly, this schedules other
    stuff to run after an event is received.  A CARD_REMOVAL event
    also sets some flags to discourage the net drivers from trying
    to talk to the card any more.

    When a CARD_REMOVAL event is received, we immediately set a flag
    to block future accesses to this device.  All the functions that
    actually access the device should check this flag to make sure
    the card is still present.
    
======================================================================*/

static int xircnw_event(event_t event, int priority,
			  event_callback_args_t *args)
{
    dev_link_t *link = args->client_data;
    struct device *dev = link->priv;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "xircnw_event()\n");
#endif
    
    switch (event) {
#ifdef PCMCIA_DEBUG
    case CS_EVENT_REGISTRATION_COMPLETE:
	if (pc_debug)
	    printk(KERN_DEBUG "xircnw_cs: registration complete\n");
	break;
#endif
    case CS_EVENT_CARD_REMOVAL:
	link->state &= ~DEV_PRESENT;
	if (link->state & DEV_CONFIG) {
	    dev->tbusy = 1; dev->start = 0;
/*	    ((xircom_private *)link->priv)->block = 1; */
	    link->release.expires = RUN_AT(5);
	    add_timer(&link->release);
	}
	break;
    case CS_EVENT_CARD_INSERTION:
	link->state |= DEV_PRESENT | DEV_CONFIG_PENDING;
	xircnw_config(link);
	break;
    case CS_EVENT_PM_SUSPEND:
	link->state |= DEV_SUSPEND;
	/* Fall through... */
    case CS_EVENT_RESET_PHYSICAL:
	if (link->state & DEV_CONFIG) {
	    if (link->open) {
		dev->tbusy = 1; dev->start = 0;
	    }
	    CardServices(ReleaseConfiguration, link->handle);
	}
	break;
    case CS_EVENT_PM_RESUME:
	link->state &= ~DEV_SUSPEND;
	/* Fall through... */
    case CS_EVENT_CARD_RESET:
	if (link->state & DEV_CONFIG) {
	    CardServices(RequestConfiguration, link->handle, &link->conf);
	    if (link->open) {
		xircnw_reset(dev);
		dev->tbusy = 0; dev->start = 1;
	    }
	}
	break;
    }
    return 0;
} /* xircnw_event */


/* Proper hardware reset of the card */
static void xinw_doreset(unsigned long ioBase, u_char* ramBase)
{
    /* Reset card */
    outb(0x80, ioBase + XIRCOM_REG_PMR);
    writeb(0x08, ramBase + XIRCOM_EREG_ASCC); /* Bit 3 is WOC */
    outb(0x0, ioBase + XIRCOM_REG_PMR); /* release reset */
    wait_WOC(ioBase);
}

/* Reset and restore all of the xircom registers */
static void xircnw_reset(struct device *dev)
{
/*    u_char state; */
    xircom_private *priv = (xircom_private*) dev->priv;
    u_char *ramBase = priv->ramBase;
    unsigned long iobase = dev->base_addr;
    printk("xircnw_reset\n");

    priv->timeoutCounter = 0;

    /* Reset card */
    xinw_doreset(iobase, ramBase);
    printk(KERN_DEBUG "xircnw_reset: Done with hardware reset\n");
   
    /* Write a NOP to check the card */
    writeb(XIRCOM_CMD_NOP, ramBase + XIRCOM_EREG_CB + 0);
    writeb(XIRCOM_CMD_EOC, ramBase + XIRCOM_EREG_CB + 1);
    wait_WOC(iobase);

    /* Set receive conf */
    writeb(XIRCOM_CMD_SRC, ramBase + XIRCOM_EREG_CB + 0);
    writeb(rxConfRxEna + rxConfBcast, ramBase + XIRCOM_EREG_CB + 1);
    writeb(XIRCOM_CMD_EOC, ramBase + XIRCOM_EREG_CB + 2);
    wait_WOC(iobase);
    
    /* Set transmit conf */
    writeb(XIRCOM_CMD_STC, ramBase + XIRCOM_EREG_CB + 0);
    writeb(txConfTxEna, ramBase + XIRCOM_EREG_CB + 1);
    writeb(XIRCOM_CMD_EOC, ramBase + XIRCOM_EREG_CB + 2);
    wait_WOC(iobase);
    

    /* Now set the MU Domain */
    printk(KERN_DEBUG "Setting domain to 0x%x%02x\n", (domain >> 8) & 0x01, domain & 0xff);
    writeb(XIRCOM_CMD_SMD, ramBase + XIRCOM_EREG_CB + 0);
    writeb(domain & 0xff, ramBase + XIRCOM_EREG_CB + 1);
    writeb((domain>>8) & 0x01, ramBase + XIRCOM_EREG_CB + 2);
    writeb(XIRCOM_CMD_EOC, ramBase + XIRCOM_EREG_CB + 3);
    wait_WOC(iobase);

    /* Set scramble key */
    printk(KERN_DEBUG "Setting scramble key to 0x%x\n", scramble_key);
    writeb(XIRCOM_CMD_SSK, ramBase + XIRCOM_EREG_CB + 0);
    writeb(scramble_key & 0xff, ramBase + XIRCOM_EREG_CB + 1);
    writeb((scramble_key>>8) & 0x01, ramBase + XIRCOM_EREG_CB + 2);
    writeb(XIRCOM_CMD_EOC, ramBase + XIRCOM_EREG_CB + 3);
    wait_WOC(iobase);


    /* Enable interrupts, bit 4 high to keep unused
     * source from interrupting us, bit 2 high to 
     * set interrupt enable, 567 to enable TxDN, 
     * RxErr and RxRdy
     */
    outb(imrConfIENA+imrConfRFU1, iobase + XIRCOM_REG_IMR);
    wait_WOC(iobase);


    /* Hent 4 bytes fra 0x170. Skal vaere 0a,29,88,36
     * waitWOC
     * skriv 80 til d000:3688
     * sjekk om det ble 80
     */

    /* Enable Receiver */
    writeb(XIRCOM_CMD_ER, ramBase + XIRCOM_EREG_CB + 0);
    writeb(XIRCOM_CMD_EOC, ramBase + XIRCOM_EREG_CB + 1);
    wait_WOC(iobase);

    /* Set the IENA bit in COR */
    outb(corConfIENA + corConfLVLREQ, iobase + XIRCOM_REG_COR);
    wait_WOC(iobase);
}

static int xinw_config(struct device *dev, struct ifmap *map)
{
    return 0;
}

static int xircnw_hw_xmit(unsigned char* data, int len, struct device* dev)
{
    unsigned int TxFreeList,
                 curBuff,
                 nextBuff, 
                 MaxData, 
                 DataOffset;

    xircom_private *priv = (xircom_private *) dev->priv;
    u_char* ramBase = priv->ramBase;
    unsigned long iobase = dev->base_addr;

    /* Check if there are transmit buffers available */
    if ((inb(iobase+XIRCOM_REG_ASR) & 0x1) == 0) {
	/* No buffers available */
	printk(KERN_DEBUG "xircnw_hw_xmit: %s - no xmit buffers available.\n",
	       dev->name);
	return 1;
    }

#if (LINUX_VERSION_CODE >= VERSION(2,1,25))
    priv->stats.tx_bytes += len;
#endif

    if (xinw_debug > 4) {
	printk(KERN_DEBUG "Transmitting with SPCQ %x SPU %x LIF %x ISPLQ %x\n",
	       readb(ramBase + XIRCOM_EREG_SPCQ),
	       readb(ramBase + XIRCOM_EREG_SPU),
	       readb(ramBase + XIRCOM_EREG_LIF),
	       readb(ramBase + XIRCOM_EREG_ISPLQ));
    }

    wait_WOC(iobase);

    /* Now try to insert it into the adapters free memory */
    TxFreeList = get_uint16(ramBase + XIRCOM_EREG_TDP);
    MaxData    = get_uint16(ramBase + XIRCOM_EREG_TDP+2);
    DataOffset = get_uint16(ramBase + XIRCOM_EREG_TDP+4);
    nextBuff   = get_uint16(ramBase + TxFreeList);
    curBuff = TxFreeList;

    if (xinw_debug > 4) 
	printk(KERN_DEBUG "TxFreeList %x, MaxData %x, DataOffset %x, nextBuff %x\n",
	       TxFreeList, MaxData, DataOffset, nextBuff);


    if (len <= MaxData)
	copy_to_pc(ramBase + TxFreeList + DataOffset, data, len);
    else {
	int tmplen = len;
	int tmpcount = 0;
	while (tmplen > 0) {
	    copy_to_pc(ramBase + curBuff + DataOffset, data + tmpcount, 
		       (tmplen < MaxData) ? tmplen : MaxData);
	    tmplen -= MaxData;
	    tmpcount += MaxData;

	    /* Update pointers into adapter */
	    curBuff = nextBuff;
	    nextBuff = get_uint16(ramBase + curBuff);
	}
    }
    
    /* Now issue transmit list */
    wait_WOC(iobase);
    writeb(XIRCOM_CMD_TL, ramBase + XIRCOM_EREG_CB + 0);
    writeb(len & 0xff, ramBase + XIRCOM_EREG_CB + 1);
    writeb((len>>8) & 0xff, ramBase + XIRCOM_EREG_CB + 2);
    writeb(XIRCOM_CMD_EOC, ramBase + XIRCOM_EREG_CB + 3);

    return 0;
}

static int xinw_start_xmit(struct sk_buff *skb, struct device *dev)
{
    if (dev->tbusy) {
	/* If we get here, some higher level has decided we are broken.
	   There should really be a 'kick me' function call instead.
	   */
	int tickssofar = jiffies - dev->trans_start;
	/*        printk("xmit called with busy. tickssofar %d\n", tickssofar); */
	if (tickssofar < TX_TIMEOUT) 
	    return 1;

        /* Should also detect if the kernel tries to xmit
	 * on a stopped card. 
	 */
       
	if (xinw_debug > 0)
	   printk(KERN_DEBUG "%s timed out.\n", dev->name);
        xircnw_reset(dev); 
	dev->trans_start = jiffies;
	dev->tbusy = 0;
    }

#if (LINUX_VERSION_CODE < VERSION(2,1,25))
    if (skb == NULL) {
        dev_tint(dev);
	return 0;
    }
    if (skb->len <= 0)
	return 0;
#endif
    
    /* Block a timer-based transmit from overlapping. This could 
       better be done with atomic_swap(1, dev->tbusy, but set_bit()
       works as well */
    if (set_bit(0, (void*)&dev->tbusy) != 0) {
	printk("%s: Transmitter access conflict.\n", dev->name);
    } else {
	short length = ETH_ZLEN < skb->len ? skb->len : ETH_ZLEN;
	unsigned char* buf = skb->data;
       
/*      checkre(buf, length, "xinwtx");
      checkwh(buf, length, "xinwtx");
  */     
	if (xircnw_hw_xmit(buf, length, dev) == 1) {
	    /* Some error, let's make them call us another time? */
	    dev->tbusy = 0;
	}

	dev->trans_start = jiffies;
    }

    dev_kfree_skb(skb, FREE_WRITE);

    return 0;
}

static void xinw_interrupt IRQ(int irq, void* dev_id, struct pt_regs *regs)
{
    unsigned long iobase;
    u_char *ramBase;
    struct device *dev = (struct device *)(irq2dev_map[irq]);
    struct xircom_private *priv;
    int i;
    dev_link_t *link;
    
    if (dev == NULL) {
	printk("xinw_interrupt: irq %d for unknown device.\n", irq);
	return;
    }
    priv = (xircom_private *)dev->priv;
    
    if (dev->interrupt) {
	printk("%s: re-entering the interrupt handler.\n", dev->name);
	return;
    }
    dev->interrupt = 1;

    /* Find the correct dev_link_t */
    for (link = dev_list; NULL != link; link = link->next)
	if (dev == link->priv) break;
    
    iobase = dev->base_addr;
    ramBase = priv->ramBase;

    /* Now find what caused the interrupt, check while interrupts ready */
    for (i = 0; (i < 10); i++) {
	u_char status;
       
	wait_WOC(iobase);	
	if (!(inb(iobase+XIRCOM_REG_CCSR) & 0x02))
	    break; /* None of the interrupt sources asserted */

        status = inb(iobase + XIRCOM_REG_ASR);
       
	if ( ! (link->state & DEV_PRESENT) ||
	     link->state & DEV_SUSPEND ) {
	    if (xinw_debug)
		printk("xinw_interupt: Interrupt with status 0x%x from removed or suspended card!\n",
		       status);
	    break;
	}
       
	/* RxRdy */
	if (status & 0x80) {
	    xinw_rx(dev);
	    /* wait_WOC(iobase); */
	    /* RxRdy cannot be reset directly by the host */
	}
	/* RxErr */
	if (status & 0x40) {
	    u_char rser;
	   
	    wait_WOC(iobase);
	    rser = readb(ramBase + XIRCOM_EREG_RSER);
	
	   
	    if (rser & 0x04) {
		++priv->stats.rx_dropped; 
		++priv->stats.rx_crc_errors;
	    }
	    if (rser & 0x02)
		++priv->stats.rx_frame_errors;
	      
	    /* Clear the RxErr bit in RSER. RSER+4 is the
	     * write part. Also clear the RxCRC (0x04) and 
	     * RxBig (0x02) bits if present */
	    writeb(0x40 | (rser & 0x06), ramBase + XIRCOM_EREG_RSER + 4);

	    /* Write bit 6 high to ASCC to clear RxErr in ASR,
	     * WOC must be set first! 
	     */
	    wait_WOC(iobase);
	    writeb(0x40, ramBase + XIRCOM_EREG_ASCC);
	    /* wait_WOC(iobase); */

	    /* Remember to count up priv->stats on error packets */
	    ++priv->stats.rx_errors;
	}
	/* TxDN */
	if (status & 0x20) {
	    int txStatus;
	    wait_WOC(iobase);
	    txStatus = readb(ramBase + XIRCOM_EREG_TSER);
	    if (xinw_debug > 4) 
		printk(KERN_DEBUG "Transmit done. TSER = %x id %x\n", 
		       txStatus, readb(ramBase + XIRCOM_EREG_TSER + 1));

	    if (txStatus & 0x20) {
		/* Transmitting was okay, clear bits */
		wait_WOC(iobase);
		writeb(0x2f, ramBase + XIRCOM_EREG_TSER + 4);
	        ++priv->stats.tx_packets;
	    }
	    
	    if (txStatus & 0xd0) {
	       if (txStatus & 0x80) {
		   ++priv->stats.collisions; /* Because of /proc/net/dev*/
		 /* ++priv->stats.tx_aborted_errors; */
		 /* printk("Collision. %ld\n", jiffies - dev->trans_start); */
	       }
	       if (txStatus & 0x40) 
		   ++priv->stats.tx_carrier_errors;
		/* 0x80 TxGU Transmit giveup - nine times and no luck
		 * 0x40 TxNOAP No access point. Discarded packet.
		 * 0x10 TxErr Transmit error. Always set when 
		 *      TxGU and TxNOAP is set. (Those are the only ones
		 *      to set TxErr).
		 */
		if (xinw_debug > 4) 
		    printk(KERN_DEBUG "xircnw_interrupt: TxDN with error status %x\n", 
			   txStatus);

		/* Clear out TxGU, TxNOAP, TxErr and TxTrys */
		wait_WOC(iobase);
		writeb(0xdf & txStatus, ramBase+XIRCOM_EREG_TSER+4);
	        ++priv->stats.tx_errors;
	    }
	    /* wait_WOC(iobase); */
	    
	    if (xinw_debug > 4) 
		printk(KERN_DEBUG "New status is TSER %x ASR %x\n",
		       readb(ramBase + XIRCOM_EREG_TSER),
		       inb(iobase + XIRCOM_REG_ASR));

	    dev->tbusy = 0;
	    mark_bh(NET_BH);
	}
	/* TxBA, this would trigger on all error packets received */
/*	if (status & 0x01) {
	    if (xinw_debug > 3) 
		printk(KERN_DEBUG "Transmit buffers available, %x\n", status); 
	} */
	   
    }

    /* done.. */
    dev->interrupt = 0;
    return;
} /* xinw_interrupt */

static struct net_device_stats *xinw_get_stats(struct device *dev)
{
    xircom_private *priv = (xircom_private*)dev->priv;
    unsigned long flags;

    save_flags(flags);
    cli();
    update_stats(dev);
    sti();
    restore_flags(flags);

    if (xinw_debug > 1) 
	printk(KERN_DEBUG "xircom: SPCQ %x SPU %x LIF %x ISPLQ %x MHS %x rxtx %x %x tx %x %x %x %x\n", 
	       readb(priv->ramBase + XIRCOM_EREG_SPCQ),
	       readb(priv->ramBase + XIRCOM_EREG_SPU),
	       readb(priv->ramBase + XIRCOM_EREG_LIF),
	       readb(priv->ramBase + XIRCOM_EREG_ISPLQ),
	       readb(priv->ramBase + XIRCOM_EREG_MHS),
	       readb(priv->ramBase + XIRCOM_EREG_EC + 0xe),
	       readb(priv->ramBase + XIRCOM_EREG_EC + 0xf),
	       readb(priv->ramBase + XIRCOM_EREG_EC + 0x18),
	       readb(priv->ramBase + XIRCOM_EREG_EC + 0x19),
	       readb(priv->ramBase + XIRCOM_EREG_EC + 0x1a),
	       readb(priv->ramBase + XIRCOM_EREG_EC + 0x1b)
	    );
    return &priv->stats;
}

static void update_stats(struct device *dev)
{
/*     xircom_private *priv = (xircom_private*) dev->priv;
    priv->stats.rx_packets = readb(priv->ramBase + 0x18e); 
    priv->stats.tx_packets = readb(priv->ramBase + 0x18f); */
}

static int xinw_rx(struct device *dev)
{
    xircom_private *priv = (xircom_private*)(dev->priv);
    u_char *ramBase = priv->ramBase;
    u_long iobase   = dev->base_addr;
    u_char rxStatus;
    struct sk_buff *skb = NULL;
    unsigned int curBuffer,
                 nextBuffer, 
                 dataOffset,
                 rcvList;
    int          rcvLen,
                 maxData;
    int extraOffset;
    int i;
    u_char *ptr;

    if (xinw_debug > 2) 
	printk(KERN_DEBUG "xinw_rx: Receiving ... \n");

    wait_WOC(iobase);
    /* Data offset and maxData */
    maxData    = get_int16(ramBase + XIRCOM_EREG_TDP + 2);
    dataOffset = get_uint16(ramBase + XIRCOM_EREG_TDP + 4);
   
    /* Receive max 10 packets for now. */
    for (i = 0; i < 10; i++) {
	wait_WOC(iobase);
	rxStatus = readb(ramBase + XIRCOM_EREG_RSER);
      
	if (!(rxStatus & 0x80)) /* No more packets */
	    break;
      
	/* For some reason, this is necessary. There is a different
	 * offset if the packet type is multicast or broadcast. 
	 */
	if (rxStatus & 0x20)
	    extraOffset = 0; /* Multicast or Broadcast */
	else 
	    extraOffset = -10;

	wait_WOC(iobase);	
	/* The receive list pointer and length of the packet */
	rcvLen  = get_int16(ramBase + XIRCOM_EREG_RDP);
	rcvList = get_uint16(ramBase + XIRCOM_EREG_RDP + 2);
	curBuffer = rcvList;
	nextBuffer = get_uint16(ramBase + curBuffer);

	if (rcvLen < 0) {
	    printk(KERN_DEBUG "xircnw_rx: Receive packet with len %d\n", rcvLen);
	    return 0;
	}
   
	 
	if (xinw_debug > 2) 
	    printk(KERN_DEBUG "Getting packet of length %d at %x, offset %x\n", 
		   rcvLen, rcvList, dataOffset);
      
	/* Use the pcmcia_cs k_compat.h version */
	skb = ALLOC_SKB(rcvLen+3);
      
	if (skb == NULL) {
	    printk(KERN_DEBUG "xircnw_rx: Could not allocate an sk_buff of length %d\n", rcvLen);
	    ++priv->stats.rx_dropped; 
	    return 0;
	}
	skb->len = rcvLen; 
	skb->dev = dev;
#if (LINUX_VERSION_CODE >= VERSION(1,3,0))
	skb_reserve(skb,2);  /* Align IP on 16 byte */
#endif
	 
	/* Must handle packet fragments */
	ptr = (u_char*) skb->data;
      
	if (rcvLen <= maxData) 
	    copy_from_pc(ptr, ramBase+rcvList+dataOffset+extraOffset, rcvLen);
	else {
	    int tmplen = rcvLen;
	    int tmpcount = 0;
	    while (tmplen > 0) {
		copy_from_pc(ptr + tmpcount ,
			     ramBase + curBuffer + dataOffset + extraOffset,
			     (tmplen < maxData) ? tmplen : maxData);
		tmplen -= maxData;
		tmpcount += maxData;
	       
		/* Update pointers into adapter */
		curBuffer = nextBuffer;
		if (nextBuffer)
		    nextBuffer = get_uint16(ramBase + curBuffer);
	    }
	}
      
#if (LINUX_VERSION_CODE >= VERSION(1,3,0)) 
	skb->protocol = eth_type_trans(skb,dev);
/*      printk("rx: protocol = %d pkt_type %d\n",
	     skb->protocol, skb->pkt_type); */
#endif
/*     if (rcvLen == 60) */
/*	printpacket(ptr, rcvLen, "rx"); */
/*      
      checkre(ptr, rcvLen, "xinwrx");
      checkwh(ptr, rcvLen, "xinwrx");
  */    
	netif_rx(skb);

	wait_WOC(iobase);
	/* Got the packet, tell the adapter to skip it */
	writeb(XIRCOM_CMD_SRP, ramBase + XIRCOM_EREG_CB + 0);
	writeb(XIRCOM_CMD_EOC, ramBase + XIRCOM_EREG_CB + 1);
      
	if (xinw_debug > 2) 
	    printk(KERN_DEBUG "Packet reception ok\n");
      
	priv->stats.rx_packets++;
#if (LINUX_VERSION_CODE >= VERSION(2,1,25))
	priv->stats.rx_bytes += skb->len;
#endif
    }
    return 0;
}


static int xinw_open(struct device *dev)
{
    dev_link_t *link;
    printk(KERN_DEBUG "xinw_open: starting.\n");

    for (link = dev_list; link; link = link->next)
	if (link->priv == dev) break;
    
    if (!DEV_OK(link))
	return -ENODEV;

    link->open++;
    MOD_INC_USE_COUNT;

    if (dev->irq != 0) 
	irq2dev_map[dev->irq] = dev;

    dev->interrupt = 0; dev->tbusy = 0; dev->start = 1;
    xircnw_reset(dev);

    return 0;
}

static int xinw_close(struct device *dev)
{
    dev_link_t *link;
    
    for (link = dev_list; link; link = link->next)
	if (link->priv == dev) break;
    if (link == NULL)
	return -ENODEV;

    link->open--;
    dev->start = 0;
    if (link->state & DEV_STALE_CONFIG) {
	link->release.expires = RUN_AT(5);
	link->state |= DEV_RELEASE_PENDING;
	add_timer(&link->release);
    }

    MOD_DEC_USE_COUNT;
    return 0;
}

int init_module(void)
{
    servinfo_t serv;
	
    CardServices(GetCardServicesInfo, &serv);
    if (serv.Revision != CS_RELEASE_CODE) {
	printk("xircnw_cs: Card Services release does not match!\n");
	return -1;
    }
    printk("xircnw_cs: Card services version = %d\n", CS_RELEASE_CODE);
    printk(version);
    register_pcmcia_driver(&dev_info, &xircnw_attach, &xircnw_detach);
  
    return 0;
}

void cleanup_module(void)
{
    printk("xircnw_cs: unloading\n");
    unregister_pcmcia_driver(&dev_info);
    while (dev_list != NULL) {
	if (dev_list->state & DEV_CONFIG)
	    xircnw_release((u_long)dev_list);
	xircnw_detach(dev_list);
    }
}


/* Set or clear the multicast filter for this adaptor.
   num_addrs == -1	Promiscuous mode, receive all packets
   num_addrs == 0	Normal mode, clear multicast list
   num_addrs > 0	Multicast mode, receive normal and MC packets, and do
			best-effort filtering.
 */
#ifdef NEW_MULTICAST
static void set_multicast_list(struct device *dev)
{
    short iobase = dev->base_addr;
    u_char* ramBase = ((xircom_private*) dev->priv)->ramBase;
    u_char  rcvMode = 0;
   
    if (xinw_debug > 3) {
	static int old = 0;
	if (old != dev->mc_count) {
	    old = dev->mc_count;
	    printk("%s: setting Rx mode to %d addresses.\n",
		   dev->name, old);
	}
    }
   
    if (dev->mc_count || (dev->flags & IFF_ALLMULTI)) {
	/* Multicast Mode */
	rcvMode = rxConfRxEna + rxConfAMP + rxConfBcast;
    } else if (dev->flags & IFF_PROMISC) {
	/* Promiscous mode */
	rcvMode = rxConfRxEna + rxConfPro + rxConfAMP + rxConfBcast;
    } else {
	/* Normal mode */
	rcvMode = rxConfRxEna + rxConfBcast;
    }
   
    printk("xircom set_multicast_list: rcvMode to %x\n", rcvMode);
    /* Now set receive mode */
    writeb(XIRCOM_CMD_SRC, ramBase + XIRCOM_EREG_CB + 0);
    writeb(rcvMode, ramBase + XIRCOM_EREG_CB + 1);
    writeb(XIRCOM_CMD_EOC, ramBase + XIRCOM_EREG_CB + 2);
    wait_WOC(iobase);
}
#else
static void
set_multicast_list(struct device *dev, int num_addrs, void *addrs)
{
    short iobase = dev->base_addr;
    u_char* ramBase = ((xircom_private*) dev->priv)->ramBase;
    u_char  rcvMode = 0;
   
    if (xinw_debug > 3) {
	static int old = 0;
	if (old != num_addrs) {
	    old = num_addrs;
	    printk("%s: setting Rx mode to %d addresses.\n",
		   dev->name, num_addrs);
	}
    }
   
    if ((num_addrs > 0) || (num_addrs == -2)) {
	/* Multicast Mode */
	rcvMode = rxConfRxEna + rxConfAMP + rxConfBcast;
    } else if (num_addrs < 0) {
	/* Promiscous mode */
	rcvMode = rxConfRxEna + rxConfPro + rxConfAMP + rxConfBcast;
    } else {
	/* Normal mode */
	rcvMode = rxConfRxEna + rxConfBcast;
    }

    printk("xircom set_multicast_list: rcvMode to %x\n", rcvMode);
    /* Now set receive mode */
    writeb(XIRCOM_CMD_SRC, ramBase + XIRCOM_EREG_CB + 0);
    writeb(rcvMode, ramBase + XIRCOM_EREG_CB + 1);
    writeb(XIRCOM_CMD_EOC, ramBase + XIRCOM_EREG_CB + 2);
    wait_WOC(iobase);
}
#endif

