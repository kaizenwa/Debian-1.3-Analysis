/*======================================================================

    A driver for PCMCIA serial devices

    Written by David Hinds, dhinds@allegro.stanford.edu
    
======================================================================*/

#include <pcmcia/config.h>
#include <pcmcia/k_compat.h>

#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/ptrace.h>
#include <linux/malloc.h>
#include <linux/string.h>
#include <linux/timer.h>
#include <linux/tty.h>
#include <linux/serial.h>
#include <linux/major.h>
#include <asm/io.h>
#include <asm/system.h>

#include <pcmcia/version.h>
#include <pcmcia/cs_types.h>
#include <pcmcia/cs.h>
#include <pcmcia/cistpl.h>
#include <pcmcia/ciscode.h>
#include <pcmcia/ds.h>
#include <pcmcia/cisreg.h>

#ifdef PCMCIA_DEBUG
static int pc_debug = PCMCIA_DEBUG;
MODULE_PARM(pc_debug, "i");
static char *version =
"serial_cs.c 1.82 1997/04/18 03:54:54 (David Hinds)";
#endif

/*====================================================================*/

/* Parameters that can be set with 'insmod' */

/* Bit map of interrupts to choose from */
static u_long irq_mask = 0xdeb8;

/* Enable the speaker? */
static int do_sound = 1;

MODULE_PARM(irq_mask, "i");
MODULE_PARM(do_sound, "i");

/*====================================================================*/

/* Table of multifunction card ID's */

typedef struct {
    u_short	manfid;
    u_short	prodid;
    int		multi;		/* 1 = multifunction, > 1 = # ports */
    int		slave;		/* 1 = not compliant with MFC spec */
} multi_id_t;

static multi_id_t multi_id[] = {
    { MANFID_IBM, PRODID_IBM_HOME_AND_AWAY, 1, 0 },
    { MANFID_3COM, PRODID_3COM_3C562, 1, 0 },
    { MANFID_LINKSYS, PRODID_LINKSYS_PCMLM28, 1, 1 },
    { MANFID_OSITECH, PRODID_OSITECH_JACK_144, 1, 1 },
    { MANFID_OSITECH, PRODID_OSITECH_JACK_288, 1, 1 },
    { MANFID_MEGAHERTZ, PRODID_MEGAHERTZ_VARIOUS, 1, 1 },
    { MANFID_MEGAHERTZ, PRODID_MEGAHERTZ_EM3288, 1, 1 },
    { MANFID_QUATECH, PRODID_QUATECH_DUAL_RS232, 2, 0 },
    { MANFID_QUATECH, PRODID_QUATECH_QUAD_RS232, 4, 0 },
    { MANFID_SOCKET, PRODID_SOCKET_DUAL_RS232, 2, 0 }
};
#define MULTI_COUNT (sizeof(multi_id)/sizeof(multi_id_t))

typedef struct serial_info_t {
    int		ndev;
    int		multi;
    int		slave;
    int		manfid;
    dev_node_t	node[4];
    int		line[4];
} serial_info_t;

static void serial_config(dev_link_t *link);
static void serial_release(u_long arg);
static int serial_event(event_t event, int priority,
			event_callback_args_t *args);

static dev_info_t dev_info = "serial_cs";

static dev_link_t *serial_attach(void);
static void serial_detach(dev_link_t *);

static dev_link_t *dev_list = NULL;

/*====================================================================*/

static void cs_error(int func, int ret)
{
    CardServices(ReportError, dev_info, (void *)func, (void *)ret);
}

/*======================================================================

    serial_attach() creates an "instance" of the driver, allocating
    local data structures for one device.  The device is registered
    with Card Services.

======================================================================*/

static dev_link_t *serial_attach(void)
{
    client_reg_t client_reg;
    dev_link_t *link;
    int ret;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "serial_attach()\n");
#endif

    /* Create new serial device */
    link = kmalloc(sizeof(struct dev_link_t), GFP_KERNEL);
    memset(link, 0, sizeof(struct dev_link_t));
    link->release.function = &serial_release;
    link->release.data = (u_long)link;
    link->io.Attributes1 = IO_DATA_PATH_WIDTH_8;
    link->io.NumPorts1 = 8;
    link->io.IOAddrLines = 3;
    link->irq.Attributes = IRQ_TYPE_EXCLUSIVE;
    link->irq.IRQInfo1 = IRQ_INFO2_VALID|IRQ_LEVEL_ID;
    link->irq.IRQInfo2 = irq_mask;
    link->conf.Attributes = CONF_ENABLE_IRQ;
    link->conf.Vcc = 50;
    if (do_sound) {
	link->conf.Attributes |= CONF_ENABLE_SPKR;
	link->conf.Status = CCSR_AUDIO_ENA;
    }
    link->conf.IntType = INT_MEMORY_AND_IO;
    link->priv = kmalloc(sizeof(struct serial_info_t), GFP_KERNEL);
    memset(link->priv, 0, sizeof(struct serial_info_t));
    
    /* Register with Card Services */
    link->next = dev_list;
    dev_list = link;
    client_reg.dev_info = &dev_info;
    client_reg.Attributes = INFO_IO_CLIENT | INFO_CARD_SHARE;
    client_reg.EventMask =
	CS_EVENT_CARD_INSERTION | CS_EVENT_CARD_REMOVAL |
	CS_EVENT_RESET_PHYSICAL | CS_EVENT_CARD_RESET |
	CS_EVENT_PM_SUSPEND | CS_EVENT_PM_RESUME;
    client_reg.event_handler = &serial_event;
    client_reg.Version = 0x0210;
    client_reg.event_callback_args.client_data = link;
    ret = CardServices(RegisterClient, &link->handle, &client_reg);
    if (ret != CS_SUCCESS) {
	cs_error(RegisterClient, ret);
	serial_detach(link);
	return NULL;
    }
    
    return link;
} /* serial_attach */

/*======================================================================

    This deletes a driver "instance".  The device is de-registered
    with Card Services.  If it has been released, all local data
    structures are freed.  Otherwise, the structures will be freed
    when the device is released.

======================================================================*/

static void serial_detach(dev_link_t *link)
{
    dev_link_t **linkp;
    long flags;
    int ret;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "serial_detach(0x%p)\n", link);
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
    
    if (link->state & DEV_CONFIG)
	serial_release((u_long)link);
    
    if (link->handle) {
	ret = CardServices(DeregisterClient, link->handle);
	if (ret != CS_SUCCESS)
	    cs_error(DeregisterClient, ret);
    }
    
    /* Unlink device structure, free bits */
    *linkp = link->next;
    kfree_s(link->priv, sizeof(serial_info_t));
    kfree_s(link, sizeof(struct dev_link_t));
    
} /* serial_detach */

/*====================================================================*/

static int setup_serial(serial_info_t *info, ioaddr_t port, int irq)
{
    struct serial_struct serial;
    int line;
    
    serial.port = port;
    serial.irq = irq;
    serial.flags = ASYNC_SKIP_TEST;
#ifdef ASYNC_SHARE_IRQ
    serial.flags |= (info->multi) ? ASYNC_SHARE_IRQ : 0;
#endif
    line = register_serial(&serial);
    if (line < 0) {
	printk(KERN_NOTICE "serial_cs: register_serial() at 0x%04x, "
	       "irq %d failed\n", serial.port, serial.irq);
	return -1;
    }
    
    info->line[info->ndev] = line;
    sprintf(info->node[info->ndev].dev_name, "ttyS%d", line);
    info->node[info->ndev].major = TTY_MAJOR;
    info->node[info->ndev].minor = 0x40+line;
    if (info->ndev > 0)
	info->node[info->ndev-1].next = &info->node[info->ndev];
    info->ndev++;
    
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

/*====================================================================*/

static int simple_config(dev_link_t *link)
{
    static ioaddr_t base[5] = { 0x3f8, 0x2f8, 0x3e8, 0x2e8, 0x0 };
    client_handle_t handle = link->handle;
    serial_info_t *info = link->priv;
    tuple_t tuple;
    u_char buf[256];
    cisparse_t parse;
    cistpl_cftable_entry_t *cf = &parse.cftable_entry;
    int i, j;

    /* First pass: look for a config entry that looks normal. */
    tuple.TupleData = (cisdata_t *)buf;
    tuple.TupleOffset = 0; tuple.TupleDataMax = 255;
    tuple.Attributes = 0;
    tuple.DesiredTuple = CISTPL_CFTABLE_ENTRY;
    i = first_tuple(handle, &tuple, &parse);
    while (i == CS_SUCCESS) {
	if ((cf->io.nwin > 0) && ((cf->io.win[0].base & 0xf) == 8)) {
	    link->conf.ConfigIndex = cf->index;
	    link->io.BasePort1 = cf->io.win[0].base;
	    i = CardServices(RequestIO, link->handle, &link->io);
	    if (i == CS_SUCCESS) goto found_port;
	}
	i = next_tuple(handle, &tuple, &parse);
    }
    
    /* Second pass: try to find an entry that isn't picky about
       its base address, then try to grab any standard serial port
       address, and finally try to get any free port. */
    i = first_tuple(handle, &tuple, &parse);
    while (i == CS_SUCCESS) {
	if ((cf->io.nwin > 0) &&
	    ((cf->io.flags & CISTPL_IO_LINES_MASK) <= 3)) {
	    link->conf.ConfigIndex = cf->index;
	    for (j = 0; j < 5; j++) {
		link->io.BasePort1 = base[j];
		i = CardServices(RequestIO, link->handle,
				 &link->io);
		if (i == CS_SUCCESS) goto found_port;
	    }
	}
	i = next_tuple(handle, &tuple, &parse);
    }

found_port:
    if (i != CS_SUCCESS) {
	cs_error(RequestIO, i);
	return -1;
    }
    
    i = CardServices(RequestIRQ, link->handle, &link->irq);
    if (i != CS_SUCCESS) {
	cs_error(RequestIRQ, i);
	link->irq.AssignedIRQ = 0;
    }
    if (info->multi && (info->manfid == MANFID_3COM))
	link->conf.ConfigIndex &= ~(0x08);
    i = CardServices(RequestConfiguration, link->handle, &link->conf);
    if (i != CS_SUCCESS) {
	cs_error(RequestConfiguration, i);
	return -1;
    }

    return setup_serial(info, link->io.BasePort1, link->irq.AssignedIRQ);
}

static int slave_config(dev_link_t *link)
{
    client_handle_t handle = link->handle;
    serial_info_t *info = link->priv;
    config_info_t config;
    ioaddr_t port;
    int i;

    /* If the card is already configured, look up the port and irq */
    i = CardServices(GetConfigurationInfo, handle, &config);
    if ((i == CS_SUCCESS) &&
	(config.Attributes & CONF_VALID_CLIENT)) {
	if (config.BasePort2 != 0)
	    port = config.BasePort2;
	else
	    /* for Ositech cards */
	    port = config.BasePort1 + 0x28;
	info->slave = 1;
	return setup_serial(info, port, config.AssignedIRQ);
    } else
	return simple_config(link);
}

static int multi_config(dev_link_t *link)
{
    client_handle_t handle = link->handle;
    serial_info_t *info = link->priv;
    tuple_t tuple;
    u_char buf[256];
    cisparse_t parse;
    cistpl_cftable_entry_t *cf = &parse.cftable_entry;
    int i, base2;

    tuple.TupleData = (cisdata_t *)buf;
    tuple.TupleOffset = 0; tuple.TupleDataMax = 255;
    tuple.Attributes = 0;
    tuple.DesiredTuple = CISTPL_CFTABLE_ENTRY;

    /* First, look for a generic full-sized window */
    link->io.NumPorts1 = info->multi * 8;
    i = first_tuple(handle, &tuple, &parse);
    while (i == CS_SUCCESS) {
	if ((cf->io.nwin == 1) && (cf->io.win[0].len > 8)) {
	    link->conf.ConfigIndex = cf->index;
	    link->io.BasePort1 = cf->io.win[0].base;
	    i = CardServices(RequestIO, link->handle, &link->io);
	    base2 = link->io.BasePort1 + 8;
	    if (i == CS_SUCCESS) break;
	}
	i = next_tuple(handle, &tuple, &parse);
    }

    /* If that didn't work, look for two windows */
    if (i != CS_SUCCESS) {
	link->io.NumPorts1 = link->io.NumPorts2 = 8;
	i = first_tuple(handle, &tuple, &parse);
	while (i == CS_SUCCESS) {
	    if (cf->io.nwin == 2) {
		link->conf.ConfigIndex = cf->index;
		link->io.BasePort1 = cf->io.win[0].base;
		link->io.BasePort2 = cf->io.win[1].base;
		i = CardServices(RequestIO, link->handle, &link->io);
		base2 = link->io.BasePort2;
		if (i == CS_SUCCESS) break;
	    }
	    i = next_tuple(handle, &tuple, &parse);
	}
    }
    
    if (i != CS_SUCCESS) {
	cs_error(RequestIO, i);
	return -1;
    }
    
    i = CardServices(RequestIRQ, link->handle, &link->irq);
    if (i != CS_SUCCESS) {
	cs_error(RequestIRQ, i);
	link->irq.AssignedIRQ = 0;
    }
    i = CardServices(RequestConfiguration, link->handle, &link->conf);
    if (i != CS_SUCCESS) {
	cs_error(RequestConfiguration, i);
	return -1;
    }
    
    setup_serial(info, link->io.BasePort1, link->irq.AssignedIRQ);
    for (i = 1; i < info->multi; i++)
	setup_serial(info, base2+(8*i), link->irq.AssignedIRQ);
    
    return 0;
}

/*======================================================================

    serial_config() is scheduled to run after a CARD_INSERTION event
    is received, to configure the PCMCIA socket, and to make the
    serial device available to the system.

======================================================================*/

#define CS_EXIT_TEST(ret, svc, label) \
if (ret != CS_SUCCESS) { cs_error(svc, ret); goto label; }

void serial_config(dev_link_t *link)
{
    client_handle_t handle;
    serial_info_t *info;
    tuple_t tuple;
    u_short buf[128];
    cisparse_t parse;
    cistpl_cftable_entry_t *cf = &parse.cftable_entry;
    int i;

    sti();
    handle = link->handle;
    info = link->priv;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "serial_config(0x%p)\n", link);
#endif
    
    tuple.TupleData = (cisdata_t *)buf;
    tuple.TupleOffset = 0; tuple.TupleDataMax = 255;
    tuple.Attributes = 0;
    /* Get configuration register information */
    tuple.DesiredTuple = CISTPL_CONFIG;
    i = first_tuple(handle, &tuple, &parse);
    CS_EXIT_TEST(i, GetFirstTuple, config_failed);
    link->conf.ConfigBase = parse.config.base;
    link->conf.Present = parse.config.rmask[0];
    
    /* Configure card */
    link->state |= DEV_CONFIG;

    /* Is this a multifunction card? */
    tuple.DesiredTuple = CISTPL_MANFID;
    tuple.Attributes = TUPLE_RETURN_COMMON;
    if (first_tuple(handle, &tuple, &parse) == CS_SUCCESS) {
	info->manfid = buf[0];
	for (i = 0; i < MULTI_COUNT; i++)
	    if ((buf[0] == multi_id[i].manfid) &&
		(buf[1] == multi_id[i].prodid))
		break;
	if (i < MULTI_COUNT) {
	    info->multi = multi_id[i].multi;
	    info->slave = multi_id[i].slave;
	}
    }

    /* Another check for dual-serial cards */
    tuple.DesiredTuple = CISTPL_CFTABLE_ENTRY;
    if ((info->multi == 0) &&
	(first_tuple(handle, &tuple, &parse) == CS_SUCCESS) &&
	(((cf->io.nwin == 1) && (cf->io.win[0].len == 16)) ||
	 ((cf->io.nwin == 2) && (cf->io.win[0].len == 8) &&
	  (cf->io.win[1].len == 8))))
	info->multi = 2;

    if (info->multi > 1)
	multi_config(link);
    else if (info->slave)
	slave_config(link);
    else
	simple_config(link);
    
    if (info->ndev == 0)
	goto config_failed;
    
    if (info->manfid == MANFID_IBM) {
	conf_reg_t reg = { 0, CS_READ, 0x800, 0 };
	i = CardServices(AccessConfigurationRegister,
			 link->handle, &reg);
	CS_EXIT_TEST(i, AccessConfigurationRegister, config_failed);
	reg.Action = CS_WRITE;
	reg.Value = reg.Value | 1;
	i = CardServices(AccessConfigurationRegister,
			 link->handle, &reg);
	CS_EXIT_TEST(i, AccessConfigurationRegister, config_failed);
    }

    link->dev = &info->node[0];
    link->state &= ~DEV_CONFIG_PENDING;
    return;

config_failed:
    serial_release((u_long)link);

} /* serial_config */

/*======================================================================

    After a card is removed, serial_release() will unregister the net
    device, and release the PCMCIA configuration.
    
======================================================================*/

void serial_release(u_long arg)
{
    dev_link_t *link = (dev_link_t *)arg;
    serial_info_t *info = link->priv;
    int i;
    
    sti();
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "serial_release(0x%p)\n", link);
#endif

    for (i = 0; i < info->ndev; i++) {
	unregister_serial(info->line[i]);
    }
    link->dev = NULL;
    
    CardServices(ReleaseConfiguration, link->handle);
    CardServices(ReleaseIO, link->handle, &link->io);
    CardServices(ReleaseIRQ, link->handle, &link->irq);
    
    link->state &= ~DEV_CONFIG;

} /* serial_release */

/*======================================================================

    The card status event handler.  Mostly, this schedules other
    stuff to run after an event is received.  A CARD_REMOVAL event
    also sets some flags to discourage the serial drivers from
    talking to the ports.
    
======================================================================*/

static int serial_event(event_t event, int priority,
			event_callback_args_t *args)
{
    dev_link_t *link = args->client_data;
    serial_info_t *info = link->priv;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "serial_event()\n");
#endif
    
    switch (event) {
#ifdef PCMCIA_DEBUG
    case CS_EVENT_REGISTRATION_COMPLETE:
	if (pc_debug)
	    printk(KERN_DEBUG "serial_cs: registration complete\n");
	break;
#endif
    case CS_EVENT_CARD_REMOVAL:
	link->state &= ~DEV_PRESENT;
	if (link->state & DEV_CONFIG) {
	    link->release.expires = RUN_AT(HZ/20);
	    link->state |= DEV_RELEASE_PENDING;
	    add_timer(&link->release);
	}
	break;
    case CS_EVENT_CARD_INSERTION:
	link->state |= DEV_PRESENT | DEV_CONFIG_PENDING;
	serial_config(link);
	break;
    case CS_EVENT_PM_SUSPEND:
	link->state |= DEV_SUSPEND;
	/* Fall through... */
    case CS_EVENT_RESET_PHYSICAL:
	if ((link->state & DEV_CONFIG) && !info->slave)
	    CardServices(ReleaseConfiguration, link->handle);
	break;
    case CS_EVENT_PM_RESUME:
	link->state &= ~DEV_SUSPEND;
	/* Fall through... */
    case CS_EVENT_CARD_RESET:
	if (DEV_OK(link) && !info->slave)
	    CardServices(RequestConfiguration, link->handle, &link->conf);
	break;
    }
    return 0;
} /* serial_event */

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
	printk(KERN_NOTICE "serial_cs: Card Services release "
	       "does not match!\n");
	return -1;
    }
    register_pcmcia_driver(&dev_info, &serial_attach, &serial_detach);
    return 0;
}

void cleanup_module(void)
{
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "serial_cs: unloading\n");
#endif
    unregister_pcmcia_driver(&dev_info);
    while (dev_list != NULL)
	serial_detach(dev_list);
}
