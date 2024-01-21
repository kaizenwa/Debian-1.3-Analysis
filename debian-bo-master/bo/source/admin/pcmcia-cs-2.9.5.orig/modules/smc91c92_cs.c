/*======================================================================

    A PCMCIA ethernet driver for SMC91c92-based cards.

    This driver supports Megahertz PCMCIA ethernet cards, and Ositech
    ethernet/modem multifunction cards.

    This driver contains code written by Donald Becker
    (becker@cesdis.gsfc.nasa.gov), Rowan Hughes (x-csrdh@jcu.edu.au),
    David Hinds (dhinds@hyper.stanford.edu), and Erik Stahlman
    (erik@vt.edu).  Donald wrote the SMC 91c92 code using parts of
    Erik's SMC 91c94 driver.  Rowan wrote a similar driver, and I've
    incorporated some parts of his driver here.  I (Dave) wrote most
    of the PCMCIA glue code, and the Ositech support code.

    This software may be used and distributed according to the terms of
    the GNU Public License, incorporated herein by reference.

======================================================================*/

#include <pcmcia/config.h>
#include <pcmcia/k_compat.h>

#include <ctype.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/ptrace.h>
#include <linux/malloc.h>
#include <linux/string.h>
#include <linux/timer.h>
#include <linux/interrupt.h>
#include <linux/delay.h>
#include <asm/io.h>
#include <asm/system.h>

#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/if_arp.h>
#include <linux/ioport.h>

#include <pcmcia/version.h>
#include <pcmcia/cs_types.h>
#include <pcmcia/cs.h>
#include <pcmcia/cistpl.h>
#include <pcmcia/cisreg.h>
#include <pcmcia/ciscode.h>
#include <pcmcia/ds.h>

/*====================================================================*/

#ifdef PCMCIA_DEBUG
static int pc_debug = PCMCIA_DEBUG;
MODULE_PARM(pc_debug, "i");
static const char *version =
"smc91c92_cs.c 0.09 1996/8/4 Donald Becker, becker@cesdis.gsfc.nasa.gov.\n";
#endif

static char *if_names[] = { "Auto", "10baseT", "10base2"};

/* Parameters that can be set with 'insmod' */

/*
  Transceiver/media type.
   1 = 10baseT (and autoselect if #define AUTOSELECT),
   2 = AUI/10base2,
*/
static int if_port = 1;

/* Bit map of interrupts to choose from. */
static u_long irq_mask = 0xdeb8;

MODULE_PARM(if_port, "i");
MODULE_PARM(irq_mask, "i");

/* Operational parameter that usually are not changed. */

/* Do you want to use 32 bit xfers?  This should work on all chips,
   but could cause trouble with some PCMCIA controllers... */
#define USE_32_BIT		1

/* Time in jiffies before concluding Tx hung */
#define TX_TIMEOUT		40

/* Maximum events (Rx packets, etc.) to handle at each interrupt. */
#define INTR_WORK		4

/* Times to check the check the chip before concluding that it doesn't
   currently have room for another Tx packet. */
#define MEMORY_WAIT_TIME       	8

/* Values that should be specific lengths */
typedef unsigned short uint16;

static dev_info_t dev_info = "smc91c92_cs";

static dev_link_t *dev_list = NULL;

struct smc_private {
    u_short			manfid;
    u_short			cardid;
    struct net_device_stats	stats;
    dev_node_t			node;
    struct sk_buff		*saved_skb;
    int				packets_waiting;
    caddr_t			base;
};

/* Special definitions for Megahertz multifunction cards */
#define MEGAHERTZ_ISR		0x0380

/* Special function registers for Ositech cards */
#define OSITECH_AUI_CTL		0x0c
#define OSITECH_PWRDOWN		0x0d
#define OSITECH_RESET		0x0e
#define OSITECH_ISR		0x0f

#define OSI_AUI_PWR		0x40
#define OSI_LAN_PWRDOWN		0x02
#define OSI_MODEM_PWRDOWN	0x01
#define OSI_LAN_RESET		0x02
#define OSI_MODEM_RESET		0x01

/* Symbolic constants for the SMC91c9* series chips, from Erik Stahlman. */
#define	BANK_SELECT		14		/* Window select register. */
#define SMC_SELECT_BANK(x)  { outw( x, ioaddr + BANK_SELECT); }

/* Bank 0 registers. */
#define	TCR 		0	/* transmit control register */
#define	 TCR_CLEAR	0	/* do NOTHING */
#define  TCR_ENABLE	0x0001	/* if this is 1, we can transmit */
#define	 TCR_PAD_EN	0x0080	/* pads short packets to 64 bytes */
#define  TCR_FDUPLX	0x0800  /* Full duplex mode. */
#define	 TCR_NORMAL TCR_ENABLE | TCR_PAD_EN

#define EPH		2	/* Ethernet Protocol Handler report. */

/* Bank 1 registers. */
#define CONFIG			0
#define  CFG_NO_WAIT		0x1000
#define  CFG_FULL_STEP		0x0400
#define  CFG_SET_SQLCH		0x0200
#define  CFG_AUI_SELECT	 	0x0100
#define  CFG_16BIT		0x0080
#define  CFG_DIS_LINK		0x0040
#define  CFG_STATIC		0x0030
#define  CFG_IRQ_SEL_1		0x0004
#define  CFG_IRQ_SEL_0		0x0002
#define	ADDR0			4
#define	CONTROL			12
#define  CTL_AUTO_RELEASE	0x0800
#define	 CTL_POWERDOWN		0x2000

/* Bank 2 registers. */
#define MMU_CMD		0
#define	 MC_ALLOC	0x20  	/* or with number of 256 byte packets */
#define	 MC_RESET	0x40
#define  MC_RELEASE  	0x80  	/* remove and release the current rx packet */
#define  MC_FREEPKT  	0xA0  	/* Release packet in PNR register */
#define  MC_ENQUEUE	0xC0 	/* Enqueue the packet for transmit */
#define	PNR_ARR		2
#define FIFO_PORTS	4
#define  FP_RXEMPTY	0x8000
#define	POINTER		6
#define  PTR_AUTO_INC	0x0040
#define  PTR_READ	0x2000
#define	 PTR_AUTOINC 	0x4000
#define	 PTR_RCV	0x8000
#define	DATA_1		8
#define	INTERRUPT	12
#define INT_MASK	13
enum { IntrRxInt = 0x01, IntrTxInt = 0x02, IntrTxEmpty = 0x04,
       IntrAlloc = 0x08, IntrRxOvr = 0x10, IntrProtoErr = 0x20 };
#define  IM_RCV_INT		0x1
#define	 IM_TX_INT		0x2
#define	 IM_TX_EMPTY_INT	0x4
#define	 IM_ALLOC_INT		0x8
#define	 IM_RX_OVRN_INT		0x10
#define	 IM_EPH_INT		0x20

#define	RCR		4
enum RxCfg { RxAllMulti = 0x0004, RxPromisc = 0x0002,
	     RxEnable = 0x0100, RxStripCRC = 0x0200};
#define  RCR_SOFTRESET	0x8000 	/* resets the chip */
#define	 RCR_STRIP_CRC	0x200	/* strips CRC */
#define  RCR_ENABLE	0x100	/* IFF this is set, we can recieve packets */
#define  RCR_ALMUL	0x4 	/* receive all multicast packets */
#define	 RCR_PROMISC	0x2	/* enable promiscuous mode */

/* the normal settings for the RCR register : */
#define	 RCR_NORMAL	(RCR_STRIP_CRC | RCR_ENABLE)
#define  RCR_CLEAR	0x0		/* set it to a base state */
#define	COUNTER		6

/* BANK 3 -- not the same values as in smc9194! */
#define	MULTICAST0	0
#define	MULTICAST2	2
#define	MULTICAST4	4
#define	MULTICAST6	6

/* Transmit status bits. */
#define TS_SUCCESS 0x0001
#define TS_16COL   0x0010
#define TS_LATCOL  0x0200
#define TS_LOSTCAR 0x0400

/* Receive status bits. */
#define RS_ALGNERR	0x8000
#define RS_BADCRC	0x2000
#define RS_ODDFRAME	0x1000
#define RS_TOOLONG	0x0800
#define RS_TOOSHORT	0x0400
#define RS_MULTICAST	0x0001
#define RS_ERRORS	(RS_ALGNERR | RS_BADCRC | RS_TOOLONG | RS_TOOSHORT)

/*====================================================================*/

static dev_link_t *smc91c92_attach(void);
static void smc91c92_detach(dev_link_t *);
static void smc91c92_config(dev_link_t *link);
static void smc91c92_release(u_long arg);
static int smc91c92_event(event_t event, int priority,
			  event_callback_args_t *args);

static int smc91c92_open(struct device *dev);
static int smc91c92_close(struct device *dev);
static int smc_start_xmit(struct sk_buff *skb, struct device *dev);
static void smc_interrupt IRQ(int irq, void *instance, struct pt_regs *regs);
static void smc_rx(struct device *dev);
static struct net_device_stats *smc91c92_get_stats(struct device *dev);
static void set_rx_mode(struct device *dev);
static int s9k_config(struct device *dev, struct ifmap *map);
static void smc_reset(struct device *dev);

/*====================================================================*/

static void cs_error(int func, int ret)
{
    CardServices(ReportError, dev_info, (void *)func, (void *)ret);
}

/*====================================================================*/

static int smc91c92_init(struct device *dev)
{
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "%s: smc91c92_init called!\n", dev->name);
#endif
    return 0;
}

/*======================================================================

  smc91c92_attach() creates an "instance" of the driver, allocating
  local data structures for one device.  The device is registered
  with Card Services.

======================================================================*/

static dev_link_t *smc91c92_attach(void)
{
    client_reg_t client_reg;
    dev_link_t *link;
    struct device *dev;
    int ret;

#ifdef PCMCIA_DEBUG
    if (pc_debug > 1)
	printk(KERN_INFO "smc91c92_attach()\n");
#endif
    
    /* Create new ethernet device */
    link = kmalloc(sizeof(struct dev_link_t), GFP_KERNEL);
    memset(link, 0, sizeof(struct dev_link_t));
    link->release.function = &smc91c92_release;
    link->release.data = (u_long)link;
    link->io.NumPorts1 = 16;
    link->io.Attributes1 = IO_DATA_PATH_WIDTH_AUTO;
    link->io.IOAddrLines = 4;
    link->irq.Attributes = IRQ_TYPE_EXCLUSIVE | IRQ_HANDLE_PRESENT;
    link->irq.IRQInfo1 = IRQ_INFO2_VALID|IRQ_LEVEL_ID;
    link->irq.IRQInfo2 = irq_mask;
    link->irq.Handler = &smc_interrupt;
    link->conf.Attributes = CONF_ENABLE_IRQ;
    link->conf.Vcc = 50;
    link->conf.IntType = INT_MEMORY_AND_IO;
    
    dev = kmalloc(sizeof(struct device), GFP_KERNEL);
    memset(dev, 0, sizeof(struct device));
    
    /* Make up a SMC91-specific-data structure. */
    dev->priv = kmalloc(sizeof(struct smc_private), GFP_KERNEL);
    memset(dev->priv, 0, sizeof(struct smc_private));
    
    /* The SMC91c92-specific entries in the device structure. */
    dev->hard_start_xmit = &smc_start_xmit;
    dev->get_stats = &smc91c92_get_stats;
    dev->set_config = &s9k_config;
#ifdef NEW_MULTICAST
    dev->set_multicast_list = &set_rx_mode;
#else
#warning "This driver does not support multicast with old kernels."
#endif
    ether_setup(dev);
    dev->name = ((struct smc_private *)dev->priv)->node.dev_name;
    dev->init = &smc91c92_init;
    dev->open = &smc91c92_open;
    dev->stop = &smc91c92_close;
    dev->tbusy = 1;
    link->priv = dev;
    
    /* Register with Card Services */
    link->next = dev_list;
    dev_list = link;
    client_reg.dev_info = &dev_info;
    client_reg.Attributes = INFO_IO_CLIENT | INFO_CARD_SHARE;
    client_reg.EventMask = CS_EVENT_CARD_INSERTION | CS_EVENT_CARD_REMOVAL |
	CS_EVENT_RESET_PHYSICAL | CS_EVENT_CARD_RESET |
	CS_EVENT_PM_SUSPEND | CS_EVENT_PM_RESUME;
    client_reg.event_handler = &smc91c92_event;
    client_reg.Version = 0x0210;
    client_reg.event_callback_args.client_data = link;
    ret = CardServices(RegisterClient, &link->handle, &client_reg);
    if (ret != 0) {
	cs_error(RegisterClient, ret);
	smc91c92_detach(link);
	return NULL;
    }
    
    return link;
} /* smc91c92_attach */

/*======================================================================

    This deletes a driver "instance".  The device is de-registered
    with Card Services.  If it has been released, all local data
    structures are freed.  Otherwise, the structures will be freed
    when the device is released.

======================================================================*/

static void smc91c92_detach(dev_link_t *link)
{
    dev_link_t **linkp;
    long flags;

#ifdef PCMCIA_DEBUG
    if (pc_debug > 1)
	printk(KERN_INFO "smc91c92_detach(0x%p)\n", link);
#endif
    
    /* Locate device structure */
    for (linkp = &dev_list; *linkp; linkp = &(*linkp)->next)
	if (*linkp == link) break;
    if (*linkp == NULL)
	return;
    
    save_flags(flags);
    cli();
    if (link->state & DEV_RELEASE_PENDING) {
	del_timer(&link->release);
	link->state &= ~DEV_RELEASE_PENDING;
    }
    restore_flags(flags);
    
    if (link->state & DEV_CONFIG) {
	smc91c92_release((u_long)link);
	if (link->state & DEV_STALE_CONFIG) {
	    link->state |= DEV_STALE_LINK;
	    return;
	}
    }
    
    if (link->handle)
	CardServices(DeregisterClient, link->handle);
    
    /* Unlink device structure, free bits */
    *linkp = link->next;
    if (link->priv) {
	struct device *dev = link->priv;
	if (dev->priv)
	    kfree_s(dev->priv, sizeof(struct smc_private));
	kfree_s(link->priv, sizeof(struct device));
    }
    kfree_s(link, sizeof(struct dev_link_t));
    
} /* smc91c92_detach */

/*====================================================================*/

static int cvt_ascii_address(struct device *dev, char *s)
{
    int i, j, da, c;

    if (strlen(s) != 12)
	return -1;
    for (i = 0; i < 6; i++) {
	da = 0;
	for (j = 0; j < 2; j++) {
	    c = *s++;
	    da <<= 4;
	    da += ((c >= '0') && (c <= '9')) ?
		(c - '0') : ((c & 0x0f) + 9);
	}
	dev->dev_addr[i] = da;
    }
    return 0;
}

/*====================================================================*/

static int get_tuple(int fn, client_handle_t handle, tuple_t *tuple,
		     cisparse_t *parse)
{
    int i;
    i = CardServices(fn, handle, tuple);
    if (i != CS_SUCCESS) return i;
    i = CardServices(GetTupleData, handle, tuple);
    if (i != CS_SUCCESS) return i;
    return CardServices(ParseTuple, handle, tuple, parse);
}

#define first_tuple(a, b, c) get_tuple(GetFirstTuple, a, b, c)
#define next_tuple(a, b, c) get_tuple(GetNextTuple, a, b, c)

/*======================================================================

    Configuration stuff for Megahertz cards

    mhz_3288_power() is used to power up a 3288's ethernet chip.
    mhz_mfc_config() handles socket setup for multifunction (1144
    and 3288) cards.  mhz_setup() gets a card's hardware ethernet
    address.
    
======================================================================*/

static void busy_loop(u_long len)
{
    u_long timeout = jiffies + len;
    u_long flags;
    save_flags(flags);
    sti();
    while (timeout >= jiffies) ;
    restore_flags(flags);
}

static int mhz_3288_power(dev_link_t *link)
{
    struct device *dev = link->priv;
    struct smc_private *lp = dev->priv;
    u_char tmp;
    
    /* Read the ISR twice... */
    readb(lp->base+MEGAHERTZ_ISR);
    udelay(5);
    readb(lp->base+MEGAHERTZ_ISR);

    /* Pause 200ms... */
    busy_loop(HZ/5);
    
    /* Now read and write the COR... */
    tmp = readb(lp->base+link->conf.ConfigBase+CISREG_COR);
    udelay(5);
    writeb(tmp, lp->base+link->conf.ConfigBase+CISREG_COR);

    return 0;
}

static int mhz_mfc_config(dev_link_t *link)
{
    struct device *dev = link->priv;
    struct smc_private *lp = dev->priv;
    tuple_t tuple;
    cisparse_t parse;
    u_char buf[255];
    cistpl_cftable_entry_t *cf = &parse.cftable_entry;
    win_req_t req;
    memreq_t mem;
    int i, k;

    link->conf.Attributes |= CONF_ENABLE_SPKR;
    link->conf.Status = CCSR_AUDIO_ENA;
    link->irq.Attributes =
	IRQ_TYPE_DYNAMIC_SHARING|IRQ_FIRST_SHARED|IRQ_HANDLE_PRESENT;
    link->io.Attributes2 = IO_DATA_PATH_WIDTH_8;
    link->io.NumPorts2 = 8;

    tuple.Attributes = 0;
    tuple.TupleData = (cisdata_t *)buf;
    tuple.TupleDataMax = sizeof(buf);
    tuple.TupleOffset = 0;
    tuple.DesiredTuple = CISTPL_CFTABLE_ENTRY;

    i = first_tuple(link->handle, &tuple, &parse);
    /* The Megahertz combo cards have modem-like CIS entries, so
       we have to explicitly try a bunch of port combinations. */
    while (i == CS_SUCCESS) {
	link->conf.ConfigIndex = cf->index;
	link->io.BasePort2 = cf->io.win[0].base;
	for (k = 0x300; k < 0x380; k += 0x20) {
	    link->io.BasePort1 = k;
	    i = CardServices(RequestIO, link->handle, &link->io);
	    if (i == CS_SUCCESS) break;
	}
	if (i == CS_SUCCESS) break;
	i = next_tuple(link->handle, &tuple, &parse);
    }
    if (i != CS_SUCCESS)
	return i;
    dev->base_addr = link->io.BasePort1;
    
    /* Allocate a memory window, for accessing the ISR */
    req.Attributes = WIN_DATA_WIDTH_8|WIN_MEMORY_TYPE_AM|WIN_ENABLE;
    req.Base = NULL;
    req.Size = 0x1000;
    req.AccessSpeed = 0;
    link->win = (window_handle_t)link->handle;
    i = CardServices(RequestWindow, &link->win, &req);
    if (i != CS_SUCCESS)
	return i;
    lp->base = req.Base;
    mem.CardOffset = mem.Page = 0;
    i = CardServices(MapMemPage, link->win, &mem);
    
    if ((i == CS_SUCCESS) && (lp->cardid == PRODID_MEGAHERTZ_EM3288))
	mhz_3288_power(link);
    
    return i;
}

static int mhz_setup(dev_link_t *link)
{
    client_handle_t handle = link->handle;
    struct device *dev = link->priv;
    tuple_t tuple;
    cisparse_t parse;
    u_char buf[255], *station_addr;

    tuple.Attributes = 0;
    tuple.TupleData = buf;
    tuple.TupleDataMax = sizeof(buf);
    tuple.TupleOffset = 0;

    /* Read the station address from the CIS.  It is stored as the last
       (fourth) string in the Version 1 Version/ID tuple. */
    tuple.DesiredTuple = CISTPL_VERS_1;
    if (first_tuple(handle, &tuple, &parse) != CS_SUCCESS)
	return -1;
    /* Ugh -- the EM1144 card has two VERS_1 tuples!?! */
    if (next_tuple(handle, &tuple, &parse) != CS_SUCCESS)
	first_tuple(handle, &tuple, &parse);
    if (parse.version_1.ns > 3) {
	station_addr = parse.version_1.str + parse.version_1.ofs[3];
	if (cvt_ascii_address(dev, station_addr) == 0)
	    return 0;
    }

    /* Another possibility: for the EM3288, in a special tuple */
    tuple.DesiredTuple = 0x81;
    if (CardServices(GetFirstTuple, handle, &tuple) != CS_SUCCESS)
	return -1;
    if (CardServices(GetTupleData, handle, &tuple) != CS_SUCCESS)
	return -1;
    buf[12] = '\0';
    if (cvt_ascii_address(dev, buf) == 0)
	return 0;
    
    return -1;
}

/*====================================================================*/

static int smc_setup(dev_link_t *link)
{
    client_handle_t handle = link->handle;
    struct device *dev = link->priv;
    tuple_t tuple;
    cisparse_t parse;
    cistpl_lan_node_id_t *node_id;
    u_char buf[255], *station_addr;
    int i;
    
    tuple.Attributes = 0;
    tuple.TupleData = buf;
    tuple.TupleDataMax = sizeof(buf);
    tuple.TupleOffset = 0;
    
    /* Read the station address from the CIS.  It is stored in the
       third string in the Version 1 Version/ID tuple. */
    tuple.DesiredTuple = CISTPL_VERS_1;
    if (first_tuple(handle, &tuple, &parse) != CS_SUCCESS)
	return -1;
    station_addr = parse.version_1.str + parse.version_1.ofs[2];
    if (cvt_ascii_address(dev, station_addr) == 0)
	return 0;

    /* Another possibility: in the LAN function extension tuples */
    tuple.DesiredTuple = CISTPL_FUNCE;
    i = first_tuple(handle, &tuple, &parse);
    while (i == CS_SUCCESS) {
	if (parse.funce.type == CISTPL_FUNCE_LAN_NODE_ID)
	    break;
	i = next_tuple(handle, &tuple, &parse);
    }
    if (i != CS_SUCCESS)
	return -1;
    node_id = (cistpl_lan_node_id_t *)parse.funce.data;
    if (node_id->nb == 6) {
	for (i = 0; i < 6; i++)
	    dev->dev_addr[i] = node_id->id[i];
	return 0;
    }
    return -1;
}

/*====================================================================*/

static int osi_config(dev_link_t *link)
{
    struct device *dev = link->priv;
    static u_short com[4] = { 0x3f8, 0x2f8, 0x3e8, 0x2e8 };
    int i, j;
    
    link->conf.Attributes |= CONF_ENABLE_SPKR;
    link->conf.Status = CCSR_AUDIO_ENA;
    link->irq.Attributes =
	IRQ_TYPE_DYNAMIC_SHARING|IRQ_FIRST_SHARED|IRQ_HANDLE_PRESENT;
    link->io.NumPorts1 = 64;
    link->io.Attributes2 = IO_DATA_PATH_WIDTH_8;
    link->io.NumPorts2 = 8;
    
    /* Enable Hard Decode, LAN, Modem */
    link->conf.ConfigIndex = 0x23;
    
    for (j = 0; j < 4; j++) {
	link->io.BasePort2 = com[j];
	i = CardServices(RequestIO, link->handle, &link->io);
	if (i == CS_SUCCESS) break;
    }
    if (i != CS_SUCCESS) {
	/* Fallback: turn off hard decode */
	link->conf.ConfigIndex = 0x03;
	link->io.NumPorts2 = 0;
	i = CardServices(RequestIO, link->handle, &link->io);
    }
    dev->base_addr = link->io.BasePort1 + 0x10;
    return i;
}

static int osi_setup(dev_link_t *link)
{
    client_handle_t handle = link->handle;
    struct device *dev = link->priv;
    tuple_t tuple;
    u_char buf[255];
    int i;
    
    tuple.Attributes = 0;
    tuple.TupleData = buf;
    tuple.TupleDataMax = sizeof(buf);
    tuple.TupleOffset = 0;
    
    /* Read the station address from tuple 0x90, subtuple 0x04 */
    tuple.DesiredTuple = 0x90;
    i = CardServices(GetFirstTuple, handle, &tuple);
    while (i == CS_SUCCESS) {
	i = CardServices(GetTupleData, handle, &tuple);
	if ((i != CS_SUCCESS) || (buf[0] == 0x04))
	    break;
	i = CardServices(GetNextTuple, handle, &tuple);
    }
    if (i != CS_SUCCESS)
	return -1;
    for (i = 0; i < 6; i++)
	dev->dev_addr[i] = buf[i+2];

    /* Make sure both functions are powered up */
    outb(0x03, link->io.BasePort1 + OSITECH_PWRDOWN);
    /* Now, turn on the interrupt for both card functions */
    outb(0x03, link->io.BasePort1 + OSITECH_ISR);

    return 0;
}

/*======================================================================

    smc91c92_config() is scheduled to run after a CARD_INSERTION event
    is received, to configure the PCMCIA socket, and to make the
    ethernet device available to the system.

======================================================================*/

#define CS_EXIT_TEST(ret, svc, label) \
if (ret != CS_SUCCESS) { cs_error(svc, ret); goto label; }

static void smc91c92_config(dev_link_t *link)
{
    client_handle_t handle = link->handle;
    struct device *dev = link->priv;
    struct smc_private *lp = dev->priv;
    tuple_t tuple;
    cisparse_t parse;
    u_short buf[32];
    int i, j;

#ifdef PCMCIA_DEBUG
    if (pc_debug > 2)
	printk(KERN_DEBUG "smc91c92_config(0x%p)\n", link);
#endif
    
    tuple.Attributes = 0;
    tuple.TupleData = (cisdata_t *)buf;
    tuple.TupleDataMax = sizeof(buf);
    tuple.TupleOffset = 0;

    tuple.DesiredTuple = CISTPL_CONFIG;
    i = first_tuple(handle, &tuple, &parse);
    CS_EXIT_TEST(i, ParseTuple, config_failed);
    link->conf.ConfigBase = parse.config.base;
    link->conf.Present = parse.config.rmask[0];
    
    tuple.DesiredTuple = CISTPL_MANFID;
    i = first_tuple(handle, &tuple, &parse);
    CS_EXIT_TEST(i, GetFirstTuple, config_failed);
    lp->manfid = buf[0];
    lp->cardid = buf[1];
    
    /* Configure card */
    link->state |= DEV_CONFIG;

    if (lp->manfid == MANFID_OSITECH)
	i = osi_config(link);
    else if ((lp->manfid == MANFID_MEGAHERTZ) &&
	     ((lp->cardid == PRODID_MEGAHERTZ_VARIOUS) ||
	      (lp->cardid == PRODID_MEGAHERTZ_EM3288)))
	i = mhz_mfc_config(link);
    else {
	link->io.NumPorts1 = 16;
	link->conf.ConfigIndex = 0x01;
	for (j = 0x200; j < 0x400; j += 0x10) {
	    link->io.BasePort1 = j;
	    i = CardServices(RequestIO, link->handle, &link->io);
	    if (i == CS_SUCCESS) break;
	}
	dev->base_addr = link->io.BasePort1;
    }
    CS_EXIT_TEST(i, RequestIO, config_failed);
    
    i = CardServices(RequestIRQ, link->handle, &link->irq);
    CS_EXIT_TEST(i, RequestIRQ, config_failed);
    i = CardServices(RequestConfiguration, link->handle, &link->conf);
    CS_EXIT_TEST(i, RequestConfiguration, config_failed);
    
    dev->irq = link->irq.AssignedIRQ;

    if ((if_port == 1) || (if_port == 2))
	dev->if_port = if_port;
    else
	printk(KERN_NOTICE "smc91c92_cs: invalid if_port requested\n");
    dev->tbusy = 0;
    
    if (register_netdev(dev) != 0) {
	printk(KERN_ERR "smc91c92_cs: register_netdev() failed\n");
	goto config_undo;
    }

    switch (lp->manfid) {
    case MANFID_OSITECH:
	i = osi_setup(link); break;
    case MANFID_SMC:
	i = smc_setup(link); break;
    case 0x128: /* For broken Megahertz cards */
    case MANFID_MEGAHERTZ:
	i = mhz_setup(link); break;
    default:
	printk(KERN_NOTICE "smc91c92_cs: unrecognized card type\n");
	goto config_undo;
    }
    
    if (i != 0) {
	printk(KERN_NOTICE "smc91c92_cs: Unable to find hardware address.\n");
	link->state &= ~DEV_CONFIG_PENDING;
	goto config_undo;
    }

    link->dev = &lp->node;
    link->state &= ~DEV_CONFIG_PENDING;
    
    printk(KERN_INFO "%s: SMC91c92 at port %#3lx, irq %d, %s port, ",
	   dev->name, dev->base_addr, dev->irq, if_names[dev->if_port]);
    for (i = 0; i < 5; i++)
	printk("%02X:", dev->dev_addr[i]);
    printk("%02X.\n", dev->dev_addr[i]);
    
    return;
    
config_undo:
    unregister_netdev(dev);
config_failed:			/* CS_EXIT_TEST() calls jump to here... */
    smc91c92_release((u_long)link);
    
} /* smc91c92_config */

/*======================================================================

    After a card is removed, smc91c92_release() will unregister the net
    device, and release the PCMCIA configuration.  If the device is
    still open, this will be postponed until it is closed.

======================================================================*/

static void smc91c92_release(u_long arg)
{
    dev_link_t *link = (dev_link_t *)arg;
    struct device *dev = link->priv;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "smc91c92_release(0x%p)\n", link);
#endif
    
    if (link->open) {
#ifdef PCMCIA_DEBUG
	if (pc_debug)
	    printk(KERN_DEBUG "smc91c92_cs: release postponed, "
		   "'%s' still open\n", dev->name);
#endif
	link->state |= DEV_STALE_CONFIG;
	return;
    }
    
    if (link->dev)
	unregister_netdev(dev);
    link->dev = NULL;
    
    CardServices(ReleaseConfiguration, link->handle);
    CardServices(ReleaseIO, link->handle, &link->io);
    CardServices(ReleaseIRQ, link->handle, &link->irq);
    if (link->win)
	CardServices(ReleaseWindow, link->win);
    if (dev->irq != 0)
	irq2dev_map[dev->irq] = NULL;
    
    link->state &= ~(DEV_CONFIG | DEV_RELEASE_PENDING);
    if (link->state & DEV_STALE_LINK)
	smc91c92_detach(link);

} /* smc91c92_release */

/*======================================================================

    The card status event handler.  Mostly, this schedules other
    stuff to run after an event is received.  A CARD_REMOVAL event
    also sets some flags to discourage the net drivers from trying
    to talk to the card any more.

======================================================================*/

static int smc91c92_event(event_t event, int priority,
			  event_callback_args_t *args)
{
    dev_link_t *link = args->client_data;
    struct device *dev = link->priv;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "smc91c92_event(0x%04x)\n", event);
#endif

    switch (event) {
#ifdef PCMCIA_DEBUG
    case CS_EVENT_REGISTRATION_COMPLETE:
	if (pc_debug)
	    printk(KERN_DEBUG "smc91c92_cs: registration complete\n");
	break;
#endif
    case CS_EVENT_CARD_REMOVAL:
	link->state &= ~DEV_PRESENT;
	if (link->state & DEV_CONFIG) {
	    dev->tbusy = 1;
	    dev->start = 0;
	    link->release.expires = RUN_AT(HZ/20);
	    link->state |= DEV_RELEASE_PENDING;
	    add_timer(&link->release);
	}
	break;
    case CS_EVENT_CARD_INSERTION:
	link->state |= DEV_PRESENT;
	smc91c92_config(link);
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
	    struct smc_private *lp = dev->priv;
	    if ((lp->manfid == MANFID_MEGAHERTZ) &&
		(lp->cardid == PRODID_MEGAHERTZ_EM3288))
		mhz_3288_power(link);
	    CardServices(RequestConfiguration, link->handle, &link->conf);
	    if (lp->manfid == MANFID_OSITECH) {
		/* Power up the card and enable interrupts */
		outb(0x03, dev->base_addr - 0x10 + OSITECH_PWRDOWN);
		outb(0x03, dev->base_addr - 0x10 + OSITECH_ISR);
	    }
	    if (link->open) {
		smc_reset(dev);
		dev->tbusy = 0;	dev->start = 1;
	    }
	}
	break;
    }
    return 0;
} /* smc91c92_event */

/*======================================================================
  
    The driver core code, most of which should be common with a
    non-PCMCIA implementation.
    
======================================================================*/

static int smc91c92_open(struct device *dev)
{
    struct smc_private *lp = (struct smc_private *)dev->priv;
    int ioaddr = dev->base_addr;
    dev_link_t *link;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "%s: smc91c92_open(%p), ID/Window %4.4x.\n",
	       dev->name, dev, inw(ioaddr + BANK_SELECT));

    if (pc_debug > 1) {
	int i, window;
	for (window = 0; window < 4; window++) {
	    printk(KERN_DEBUG "%s:  Register bank %d ", dev->name, window);
	    SMC_SELECT_BANK(window);
	    for (i = 0; i < 16; i += 2)
		printk(" %4.4x", inw(ioaddr + i));
	    printk(".\n");
	}
    }
#endif
    
    /* Check that the PCMCIA card is still here. */
    for (link = dev_list; link; link = link->next)
	if (link->priv == dev) break;
    /* Physical device present signature. */
    if (!DEV_OK(link))
	return -ENODEV;
    if (inb(ioaddr + BANK_SELECT + 1) != 0x33) {
	/* Try powering up the chip */
	outw(0, ioaddr + CONTROL);
	/* Wait 55 msec for startup */
	busy_loop(HZ/18);
	if (inb(ioaddr + BANK_SELECT + 1) != 0x33)
	    return -ENODEV;
    }
    
    link->open++;
    MOD_INC_USE_COUNT;
    
    irq2dev_map[dev->irq] = dev;
    
    dev->interrupt = 0; dev->tbusy = 0; dev->start = 1;
    lp->saved_skb = 0;
    lp->packets_waiting = 0;
    
    smc_reset(dev);
    
    return 0;
} /* smc91c92_open */

/*====================================================================*/

static int smc91c92_close(struct device *dev)
{
    int ioaddr = dev->base_addr;
    dev_link_t *link;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "%s: smc91c92_close(), status %4.4x.\n",
	       dev->name, inw(ioaddr + BANK_SELECT));
#endif
    
    dev->tbusy = 1;
    dev->start = 0;
    
    /* Shut off all interrupts, and turn off the Tx and Rx sections.
       Don't bother to check for chip present. */
    SMC_SELECT_BANK( 2 );	/* Nominally paranoia, but do no assume... */
    outb( 0, ioaddr + INT_MASK );
    SMC_SELECT_BANK( 0 );
    outb( RCR_CLEAR, ioaddr + RCR );
    outb( TCR_CLEAR, ioaddr + TCR );
    
    /* Put the chip into power-down mode. */
    SMC_SELECT_BANK( 1 );
    outw(CTL_POWERDOWN, ioaddr + CONTROL  );
    
    for (link = dev_list; link; link = link->next)
	if (link->priv == dev) break;
    if (link == NULL)
	return -ENODEV;
    
    link->open--; dev->start = 0;
    if (link->state & DEV_STALE_CONFIG) {
	link->release.expires = RUN_AT(HZ/20);
	link->state |= DEV_RELEASE_PENDING;
	add_timer(&link->release);
    }
    
    MOD_DEC_USE_COUNT;
    
    return 0;
} /* smc91c92_close */

/*======================================================================

   Transfer a packet to the hardware and trigger the packet send.
   This may be called at either from either the Tx queue code
   or the interrupt handler.
   
======================================================================*/

static void smc_hardware_send_packet( struct device * dev )
{
    struct smc_private *lp = (struct smc_private *)dev->priv;
    struct sk_buff *skb = lp->saved_skb;
    int ioaddr = dev->base_addr;
    unsigned char packet_no;
    
    if ( !skb ) {
	printk(KERN_ERR "%s: In XMIT with no packet to send.\n", dev->name);
	return;
    }
    
    /* There should be a packet slot waiting. */
    packet_no = inb(ioaddr + PNR_ARR + 1);
    if ( packet_no & 0x80 ) {
	/* If not, there is a hardware problem!  Likely an ejected card. */
	printk(KERN_WARNING "%s: 91c92 hardware Tx buffer allocation"
	       " failed, status %#2.2x.\n", dev->name, packet_no);
	dev_kfree_skb (skb, FREE_WRITE);
	lp->saved_skb = NULL;
	dev->tbusy = 0;
	return;
    }
    
#if (LINUX_VERSION_CODE >= VERSION(2,1,25))
    lp->stats.tx_bytes += skb->len;
#endif
    /* The card should use the just-allocated buffer. */
    outb( packet_no, ioaddr + PNR_ARR );
    /* point to the beginning of the packet */
    outw( PTR_AUTOINC , ioaddr + POINTER );
    
    /* Send the packet length ( +6 for status, length and ctl byte )
       and the status word ( set to zeros ). */
    {
	unsigned char *buf = skb->data;
	int length = skb->len;	/* The chip will pad to ethernet min length. */

#ifdef PCMCIA_DEBUG
	if (pc_debug > 2)
	    printk(KERN_DEBUG "%s: Trying to xmit packet of length %x.\n",
		   dev->name, length);
#endif
	
#ifdef USE_32_BIT
	outl((length+6) << 16, ioaddr + DATA_1);
	if (length & 0x2) {
	    outsl(ioaddr + DATA_1, buf,  length >> 2 );
	    outw( *((uint16 *)(buf + (length & 0xFFFFFFFC))),ioaddr +DATA_1);
	} else
	    outsl(ioaddr + DATA_1, buf,  length >> 2 );
#else
	/* send the packet length: +6 for status words, length, and ctl */
	outw( 0, ioaddr + DATA_1 );
	outw(length + 6, ioaddr + DATA_1 );
	outsw(ioaddr + DATA_1 , buf, length >> 1);
#endif
	
	/* The odd last byte, if there is one, goes in the control word. */
	outw((length & 1) ? 0x2000 | buf[length-1] : 0, ioaddr + DATA_1 );
    }
    
    /* Enable the Tx interrupts, both Tx (TxErr) and TxEmpty. */
    outb(IM_TX_INT | IM_TX_EMPTY_INT | inb(ioaddr + INT_MASK),
	 ioaddr + INT_MASK);
    
    /* The chip does the rest of the work. */
    outw( MC_ENQUEUE , ioaddr + MMU_CMD );
    
    lp->saved_skb = NULL;
    dev_kfree_skb (skb, FREE_WRITE);
    dev->trans_start = jiffies;
    dev->tbusy = 0;
    return;
}

/*====================================================================*/

static int smc_start_xmit(struct sk_buff *skb, struct device *dev)
{
    struct smc_private *lp = (struct smc_private *)dev->priv;
    int ioaddr = dev->base_addr;
    unsigned short num_pages;
    short time_out;
    
    /* Transmitter timeout, serious problems. */
    if (dev->tbusy) {
	int tickssofar = jiffies - dev->trans_start;
	if (tickssofar < TX_TIMEOUT)
	    return 1;
	printk(KERN_NOTICE "%s: SMC91c92 transmit timed out, "
	       "Tx_status %2.2x status %4.4x.\n",
	       dev->name, inb(ioaddr + 0), inw(ioaddr + 2));
	lp->stats.tx_errors++;
	smc_reset(dev);
	dev->trans_start = jiffies;
	dev->tbusy = 0;
    }

#if (LINUX_VERSION_CODE < VERSION(2,1,25))
    /* Sanity check the queue-layer request. */
    if (skb == NULL) {
	dev_tint(dev);
	return 0;
    }
    if (skb->len <= 0)
	return 0;
#endif
    
#ifdef PCMCIA_DEBUG
    if (pc_debug > 2)
	printk(KERN_DEBUG "%s: smc91c92_start_xmit(length = %ld) called,"
	       " status %4.4x.\n", dev->name, skb->len, inw(ioaddr + 2));
#endif
    
    /* Avoid timer-based retransmission conflicts. */
    if (set_bit(0, (void*)&dev->tbusy) != 0) {
	printk(KERN_ERR "%s: transmitter access conflict.\n", dev->name);
	return 1;
    }
    
    if ( lp->saved_skb) {
	/* THIS SHOULD NEVER HAPPEN. */
	lp->stats.tx_aborted_errors++;
	printk(KERN_DEBUG "%s: Internal error -- sent packet while busy.\n",
	       dev->name);
	return 1;
    }
    lp->saved_skb = skb;
    
    num_pages = skb->len >> 8;
    
    if (num_pages > 7 ) {
	printk(KERN_ERR "%s: Far too big packet error.\n", dev->name);
	dev_kfree_skb (skb, FREE_WRITE);
	lp->saved_skb = NULL;
	lp->stats.tx_dropped++;
	return 0;		/* Do not re-queue this packet. */
    }
    /* A packet is now waiting. */
    lp->packets_waiting++;
    
    SMC_SELECT_BANK( 2 );	/* Paranoia, we should always be in window 2 */
    
    /* Allocate the memory; send the packet now if we win. */
    outw( MC_ALLOC | num_pages, ioaddr + MMU_CMD );
    for (time_out = MEMORY_WAIT_TIME; time_out >= 0; time_out--) {
	if ( inb(ioaddr + INTERRUPT) & IM_ALLOC_INT) {
	    /* Acknowledge the interrupt, send the packet. */
	    outb( IM_ALLOC_INT, ioaddr + INTERRUPT);
	    smc_hardware_send_packet(dev);	/* Send the packet now.. */
	    return 0;
	}
    }
    /* Otherwise defer until the Tx-space-allocated interrupt. */
    outb(IM_ALLOC_INT | inb(ioaddr + INT_MASK),  ioaddr + INT_MASK);
    
    return 0;
}

/*======================================================================

    Handle a Tx anomolous event.  Entered while in Window 2.
    
======================================================================*/

static void smc_tx_err( struct device * dev )
{
    struct smc_private *lp = (struct smc_private *)dev->priv;
    int ioaddr = dev->base_addr;
    int saved_packet = inb(ioaddr + PNR_ARR);
    int packet_no = inw(ioaddr + FIFO_PORTS) & 0x7f;
    int tx_status;
    
    /* select this as the packet to read from */
    outb( packet_no, ioaddr + PNR_ARR );
    
    /* read the first word from this packet */
    outw( PTR_AUTOINC | PTR_READ | 0, ioaddr + POINTER );
    
    tx_status = inw(ioaddr + DATA_1);
    
    lp->stats.tx_errors++;
    if (tx_status & TS_LOSTCAR ) lp->stats.tx_carrier_errors++;
    if (tx_status & TS_LATCOL )  lp->stats.tx_window_errors++;
    if ( tx_status & TS_16COL ) lp->stats.tx_aborted_errors++;
    
    if ( tx_status & TS_SUCCESS ) {
	printk(KERN_NOTICE "%s: Successful packet caused error "
	       "interrupt?\n", dev->name);
    }
    /* re-enable transmit */
    SMC_SELECT_BANK( 0 );
    outw( inw(ioaddr + TCR) | TCR_ENABLE, ioaddr + TCR);
    SMC_SELECT_BANK( 2 );
    
    outw( MC_FREEPKT, ioaddr + MMU_CMD ); 	/* Free the packet memory. */
    
    /* one less packet waiting for me */
    lp->packets_waiting--;
    
    outb( saved_packet, ioaddr + PNR_ARR );
    return;
}

/*====================================================================*/

static void smc_eph_irq(struct device *dev)
{
    struct smc_private *lp = dev->priv;
    int ioaddr = dev->base_addr;
    unsigned short card_stats, ephs;
    
    SMC_SELECT_BANK(0);
    ephs = inw(ioaddr + EPH);
#ifdef PCMCIA_DEBUG
    if (pc_debug > 2)
	printk(KERN_NOTICE "%s: Ethernet protocol handler interrupt"
	       ", status %4.4x.\n", dev->name, ephs);
#endif
    /* Could be a counter roll-over warning: update stats. */
    card_stats = inw( ioaddr + COUNTER );
    /* single collisions */
    lp->stats.collisions += card_stats & 0xF;
    card_stats >>= 4;
    /* multiple collisions */
    lp->stats.collisions += card_stats & 0xF;
#if 0 		/* These are for when linux supports these statistics */
    card_stats >>= 4;			/* deferred */
    card_stats >>= 4;			/* excess deferred */
#endif
    /* If we had a transmit error we must re-enable the transmitter. */
    outw( inw(ioaddr + TCR) | TCR_ENABLE, ioaddr + TCR);
    /* Ack potential Link Error status. */
    SMC_SELECT_BANK(1);
    if ((dev->if_port == 1) && !(ephs & 0x4000)) {
	printk(KERN_INFO "%s: Link beat lost\n", dev->name);
	if (lp->stats.rx_packets < 10) {
	    printk(KERN_INFO "%s: switching to 10base2.\n", dev->name);
	    dev->if_port = 2;
	    outw(CFG_NO_WAIT | CFG_16BIT | CFG_STATIC | CFG_AUI_SELECT |
		 (lp->manfid == MANFID_OSITECH ?
		  (CFG_IRQ_SEL_1 | CFG_IRQ_SEL_0) : 0),
		 ioaddr + CONFIG);
	    if (lp->manfid == MANFID_OSITECH)
		outb(OSI_AUI_PWR, ioaddr - 0x10 + OSITECH_AUI_CTL);
	}
    }

    /* Clear a link error interrupt. */
    outw( CTL_AUTO_RELEASE | 0x0000, ioaddr + CONTROL);
    outw( CTL_AUTO_RELEASE | 0x00E0, ioaddr + CONTROL);
    SMC_SELECT_BANK(2);
}

/*====================================================================*/
    
static void smc_interrupt IRQ(int irq, void *instance, struct pt_regs *regs)
{
    struct device *dev = (struct device *)(irq2dev_map[irq]);
    struct smc_private *lp;
    int ioaddr;
    unsigned short saved_bank, saved_pointer;
    char bogus_cnt = INTR_WORK;		/* Work we are willing to do. */
    unsigned char mask;
    
    if (dev == NULL)
	return;
    ioaddr = dev->base_addr;
    
#ifdef PCMCIA_DEBUG
    if (dev->interrupt) {
	printk(KERN_ERR "%s: re-entering the interrupt handler.\n", dev->name);
	return;
    }
    dev->interrupt = 1;

    if (pc_debug > 5)
	printk(KERN_DEBUG "%s: SMC91c92 interrupt %d at %#x.\n",
	       dev->name, irq, ioaddr);
#endif
    
    lp = (struct smc_private *)dev->priv;
    saved_bank = inw(ioaddr + BANK_SELECT);
    if ((saved_bank & 0xff00) != 0x3300) {
	/* The device does not exist -- the card could be off-line, or
	   maybe it has been ejected. */
#ifdef PCMCIA_DEBUG
	if (dev->start && (pc_debug > 4))
	    printk(KERN_ERR "%s: SMC91c92 interrupt %d for non-existent"
		   "/ejected device.\n", dev->name, irq);
	dev->interrupt = 0;
#endif
	goto irq_done;
    }
    
    SMC_SELECT_BANK(2);
    saved_pointer = inw(ioaddr + POINTER);
    mask = inb(ioaddr + INT_MASK);
    /* clear all interrupts */
    outb( 0, ioaddr + INT_MASK );
    
    do { /* read the status flag, and mask it */
	int status = inb( ioaddr + INTERRUPT );
#ifdef PCMCIA_DEBUG
	if (pc_debug > 6)
	    printk(KERN_DEBUG "%s: Status is %#2.2x (mask %#2.2x).\n",
		   dev->name, status, mask);
#endif
	if ((status & mask) == 0 )
	    break;
	
	if (status & IM_RCV_INT) {
	    /* Got a packet(s). */
	    smc_rx(dev);
	}
	if (status & IM_TX_INT) {
	    smc_tx_err(dev);
	    outb(IM_TX_INT, ioaddr + INTERRUPT );
	}
	status &= mask;
	if (status & IM_TX_EMPTY_INT) {
	    outb( IM_TX_EMPTY_INT, ioaddr + INTERRUPT );
	    mask &= ~IM_TX_EMPTY_INT;
	    lp->stats.tx_packets += lp->packets_waiting;
	    lp->packets_waiting = 0;
	}
	if (status & IM_ALLOC_INT) {
	    /* Clear this interrupt so it doesn't happen again */
	    mask &= ~IM_ALLOC_INT;
	    
	    smc_hardware_send_packet(dev);
	    
	    /* enable xmit interrupts based on this */
	    mask |= ( IM_TX_EMPTY_INT | IM_TX_INT );
	    
	    /* and let the card send more packets to me */
	    mark_bh( NET_BH );
	}
	if (status & IM_RX_OVRN_INT) {
	    lp->stats.rx_errors++;
	    lp->stats.rx_fifo_errors++;		
	    outb( IM_RX_OVRN_INT, ioaddr + INTERRUPT );
	}
	if (status & IM_EPH_INT)
	    smc_eph_irq(dev);
    } while ( --bogus_cnt);

#ifdef PCMCIA_DEBUG
    if (pc_debug > 6)
	printk(KERN_DEBUG "  Restoring saved registers mask %2.2x bank"
	       " %d pointer %d.\n", mask, saved_bank, saved_pointer);
#endif
    
    /* restore state register */
    outb( mask, ioaddr + INT_MASK );
    outw( saved_pointer, ioaddr + POINTER );
    
    SMC_SELECT_BANK( saved_bank );

#ifdef PCMCIA_DEBUG
    dev->interrupt = 0;
    if (pc_debug > 5)
	printk(KERN_DEBUG "%s: Exiting interrupt IRQ%d.\n",
	       dev->name, irq);
#endif

irq_done:
    
    if (lp->manfid == MANFID_OSITECH) {
	/* Retrigger interrupt if needed */
	outb(0x00, ioaddr - 0x10 + OSITECH_ISR);
	outb(0x03, ioaddr - 0x10 + OSITECH_ISR);
    }
#ifdef DOES_NOT_WORK
    if (lp->base != NULL) { /* Megahertz MFC's */
	readb(lp->base+MEGAHERTZ_ISR);
	readb(lp->base+MEGAHERTZ_ISR);
    }
#endif
}

/*====================================================================*/

static void smc_rx(struct device *dev)
{
    struct smc_private *lp = (struct smc_private *)dev->priv;
    int ioaddr = dev->base_addr;
    int rx_status;
    int packet_length;	/* Caution: not frame length, rather words
			   to transfer from the chip. */
    
    /* Assertion: we are in Window 2. */
    
    if (inw(ioaddr + FIFO_PORTS) & FP_RXEMPTY) {
	printk(KERN_ERR "%s: smc_rx() with nothing on Rx FIFO.\n",
	       dev->name);
	return;
    }
    
    /*  Reset the read pointer, and read the status and packet length. */
    outw( PTR_READ | PTR_RCV | PTR_AUTOINC, ioaddr + POINTER );
    rx_status = inw(ioaddr + DATA_1);
    packet_length = inw(ioaddr + DATA_1) & 0x07ff;

#ifdef PCMCIA_DEBUG
    if (pc_debug > 4)
	printk(KERN_DEBUG "%s: Receive status %4.4x length %4d.\n",
	       dev->name, rx_status, packet_length);
#endif
    
    if ( !(rx_status & RS_ERRORS )) {		
	/* do stuff to make a new packet */
	struct sk_buff *skb;
	
	/* Note: packet_length adds 5 or 6 extra bytes here! */
	skb = ALLOC_SKB(packet_length);
	
	if ( skb == NULL ) {
#ifdef PCMCIA_DEBUG
	    if (pc_debug)
		printk(KERN_NOTICE "%s: Low memory, packet dropped.\n",
		       dev->name);
#endif
	    lp->stats.rx_dropped++;
	    outw( MC_RELEASE, ioaddr + MMU_CMD );
	    return;
	}
	
	packet_length -= (rx_status & RS_ODDFRAME ? 5 : 6);

#define BLOCK_INPUT(buf, len) insw(ioaddr+DATA_1, buf, (len+1)>>1)
	GET_PACKET(dev, skb, packet_length);
	
	skb->dev = dev;
	netif_rx(skb);
	lp->stats.rx_packets++;
#if (LINUX_VERSION_CODE >= VERSION(2,1,25))
	lp->stats.rx_bytes += skb->len;
#endif
	if (rx_status & RS_MULTICAST)
	    lp->stats.multicast++;
    } else {
	/* error ... */
	lp->stats.rx_errors++;
	
	if (rx_status & RS_ALGNERR)  lp->stats.rx_frame_errors++;
	if (rx_status & (RS_TOOSHORT | RS_TOOLONG))
	    lp->stats.rx_length_errors++;
	if (rx_status & RS_BADCRC)	lp->stats.rx_crc_errors++;
    }
    /* Let the MMU free the memory of this packet. */
    outw(MC_RELEASE, ioaddr + MMU_CMD);
    
    return;
}

/*====================================================================*/

static struct net_device_stats *smc91c92_get_stats(struct device *dev)
{
    struct smc_private *lp = (struct smc_private *)dev->priv;
    /* Nothing to update - the 91c92 is a pretty primative chip. */
    return &lp->stats;
}

/*======================================================================

    Compute the AUTODIN polynomial "CRC32" for ethernet packets.

======================================================================*/

static unsigned const ethernet_polynomial = 0x04c11db7U;

static unsigned ether_crc(int length, unsigned char *data)
{
    int crc = 0xffffffff;	/* Initial value. */
    
    while (--length >= 0) {
	unsigned char current_octet = *data++;
	int bit;
	for (bit = 0; bit < 8; bit++, current_octet >>= 1) {
	    crc = (crc << 1) ^
		((crc < 0) ^ (current_octet & 1) ? ethernet_polynomial : 0);
	}
    }
    /* The hash index is the either the upper or lower bits of the CRC, so
     * we return the entire CRC.
     */
    return crc;
}

/*======================================================================
  
    Calculate values for the hardware multicast filter hash table.
    
======================================================================*/

static void fill_multicast_tbl(int count, struct dev_mc_list *addrs,
			       unsigned char *multicast_table)
{
    struct dev_mc_list	*mc_addr;
    
    for (mc_addr = addrs;  mc_addr && --count > 0;  mc_addr = mc_addr->next) {
	unsigned int position = ether_crc(6, mc_addr->dmi_addr);
#ifndef final_version		/* Verify multicast address. */
	if ( (mc_addr->dmi_addr[0] & 1) == 0)
	    continue;
#endif
	multicast_table[position >> 29] |= 1 << ((position >> 26) & 7);
    }
}

/*======================================================================
  
    Set the receive mode.
    
    This routine is used by both the protocol level to notify us of
    promiscuous/multicast mode changes, and by the open/reset code to
    initialize the Rx registers.  We always set the multicast list and
    leave the receiver running.
    
======================================================================*/

static void set_rx_mode(struct device *dev)
{
    int ioaddr = dev->base_addr;
    unsigned int multicast_table[ 2 ] = { 0, };
    long flags;
    uint16 rx_cfg_setting;
    
    if (dev->flags & IFF_PROMISC) {
	printk(KERN_NOTICE "%s: setting Rx mode to promiscuous.\n", dev->name);
	rx_cfg_setting = RxStripCRC | RxEnable | RxPromisc | RxAllMulti;
    } else if (dev->flags & IFF_ALLMULTI)
	rx_cfg_setting = RxStripCRC | RxEnable | RxAllMulti;
    else {
	if (dev->mc_count)  {
	    fill_multicast_tbl(dev->mc_count, dev->mc_list,
			       (unsigned char *)multicast_table);
	}
	rx_cfg_setting = RxStripCRC | RxEnable;
    }
    
    /* Load MC table and Rx setting into the chip without interrupts. */
    save_flags(flags);
    cli();
    SMC_SELECT_BANK( 3 );
    outl(multicast_table[0], ioaddr + MULTICAST0);
    outl(multicast_table[1], ioaddr + MULTICAST4);
    SMC_SELECT_BANK(0);
    outw(rx_cfg_setting, ioaddr + RCR);
    SMC_SELECT_BANK(2);
    restore_flags(flags);
    
    return;
}

/*======================================================================

    Senses when a card's config changes. Here, it's coax or TP.
 
======================================================================*/

static int s9k_config(struct device *dev, struct ifmap *map)
{
    if ((map->port != (u_char)(-1)) && (map->port != dev->if_port)) {
	if ((map->port == 1) || (map->port == 2)) {
	    dev->if_port = map->port;
	    printk(KERN_INFO "%s: switched to %s port\n",
		   dev->name, if_names[dev->if_port]);
	}
	else
	    return -EINVAL;
    }
    return 0;
}

/*======================================================================

    Reset the chip, reloading every register that might be corrupted.

======================================================================*/

static void smc_reset(struct device *dev)
{
    int ioaddr = dev->base_addr;
    struct smc_private *lp = dev->priv;
    int i;

#ifdef PCMCIA_DEBUG
    if (pc_debug > 1)
	printk(KERN_DEBUG "%s: smc91c92 reset called.\n", dev->name);
#endif
    
    /* The first interaction must be a write to bring the chip out
       of sleep mode. */
    SMC_SELECT_BANK( 0 );
    /* Reset the chip. */
    outw( RCR_SOFTRESET, ioaddr + RCR );
    udelay(10);
    
    /* Clear the transmit and receive configuration registers. */
    outw( RCR_CLEAR, ioaddr + RCR );
    outw( TCR_CLEAR, ioaddr + TCR );
    
    /* Set the Window 1 control, configuration and station addr registers.
       No point in writing the I/O base register ;-> */
    SMC_SELECT_BANK(1);
    /* Automatically release succesfully transmitted packets,
       Accept link errors, counter and Tx error interrupts. */
    outw(CTL_AUTO_RELEASE | 0x00e0, ioaddr + CONTROL);
    outw(CFG_NO_WAIT | CFG_16BIT | CFG_STATIC |
	 (lp->manfid == MANFID_OSITECH ?
	  (CFG_IRQ_SEL_1 | CFG_IRQ_SEL_0) : 0) |
	 (dev->if_port == 2 ? CFG_AUI_SELECT : 0),
	 ioaddr + CONFIG);
    if (lp->manfid == MANFID_OSITECH)
	outb(dev->if_port == 2 ? OSI_AUI_PWR : 0,
	     ioaddr - 0x10 + OSITECH_AUI_CTL);
    
    /* Fill in the physical address.  The databook is wrong about the order! */
    for (i = 0; i < 6; i++)
	outb(dev->dev_addr[i], ioaddr + ADDR0 + i);
    
    /* Reset the MMU */
    SMC_SELECT_BANK(2);
    outw(MC_RESET, ioaddr + MMU_CMD );
    outb(0, ioaddr + INT_MASK );
    
    /* Re-enable the chip. */
    SMC_SELECT_BANK(0);
    outw(TCR_ENABLE | TCR_PAD_EN, ioaddr + TCR);
    set_rx_mode(dev);
    
    /* Enable interrupts. */
    SMC_SELECT_BANK( 2 );
    outb( IM_EPH_INT | IM_RX_OVRN_INT | IM_RCV_INT, ioaddr + INT_MASK );
}

/*====================================================================*/

int init_module(void)
{
    servinfo_t serv;
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_INFO "%s\n", version);
#endif
    CardServices(GetCardServicesInfo, &serv);
    if (serv.Revision != CS_RELEASE_CODE) {
	printk(KERN_ERR
	       "smc91c92_cs: Card Services release does not match!\n");
	return -1;
    }
    register_pcmcia_driver(&dev_info, &smc91c92_attach, &smc91c92_detach);
    return 0;
}

void cleanup_module(void)
{
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "smc91c92_cs: unloading\n");
#endif
    unregister_pcmcia_driver(&dev_info);
    while (dev_list != NULL)
	smc91c92_detach(dev_list);
}
