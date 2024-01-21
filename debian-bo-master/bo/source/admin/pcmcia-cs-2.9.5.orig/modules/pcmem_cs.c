/*======================================================================

    A general driver for accessing PCMCIA card memory

    This driver provides the equivalent of /dev/mem for a PCMCIA
    card's attribute and common memory.  It also includes a block
    device driver for accessing common memory.

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
#include <linux/major.h>
#include <linux/fs.h>
#include <asm/io.h>
#include <asm/system.h>
#include <asm/segment.h>
#include <asm/string.h>
#include <stdarg.h>

#include <pcmcia/version.h>
#include <pcmcia/cs_types.h>
#include <pcmcia/cs.h>
#include <pcmcia/cistpl.h>
#include <pcmcia/ds.h>
#include <pcmcia/mem_op.h>

/* Major device #'s for pcmem device */
static int major_dev = 0;

/* Funky stuff for setting up a block device */
#define MAJOR_NR		major_dev
#define DEVICE_NAME		"pcmem"
#define DEVICE_REQUEST		do_pcmem_request
#define DEVICE_ON(device)
#define DEVICE_OFF(device)
#define DEVICE_NR(device)	(MINOR(device)>>2)
#define MINOR_NR(dev)		((dev)<<2)

#include BLK_DEV_HDR

#ifdef PCMCIA_DEBUG
static int pc_debug = PCMCIA_DEBUG;
MODULE_PARM(pc_debug, "i");
static char *version =
"pcmem_cs.c 1.71 1997/04/15 16:00:27 (David Hinds)";
#endif

/*====================================================================*/

/* Parameters that can be set with 'insmod' */

/* 1 = do 16-bit transfers, 0 = do 8-bit transfers */
static int word_width = 1;

/* Speed of memory accesses, in ns */
static int mem_speed = 0;

MODULE_PARM(word_width, "i");
MODULE_PARM(mem_speed, "i");

/*====================================================================*/

/* Maximum number of separate memory devices we'll allow */
#define MAX_DEV		8

/* Size of the PCMCIA address space: 26 bits = 64 MB */
#define HIGH_ADDR	0x4000000

/* Size of the memory window: 4K */
#define WINDOW_SIZE	0x1000

/* Sector size -- shouldn't need to change */
#define SECTOR_SIZE	512

static void pcmem_config(dev_link_t *link);
static void pcmem_release(u_long arg);
static int pcmem_event(event_t event, int priority,
		       event_callback_args_t *args);

static dev_link_t *pcmem_attach(void);
static void pcmem_detach(dev_link_t *);

typedef struct pcmem_dev_t {
    dev_node_t	node;
    u_long	flags;
    caddr_t	Base;
    u_long	size;
} pcmem_dev_t;

#define MEM_WRPROT	1

static dev_info_t dev_info = "pcmem_cs";
static dev_link_t *dev_table[MAX_DEV] = { NULL, /* ... */ };

static int pcmem_blocksizes[MINOR_NR(MAX_DEV)] =
{ 0, /* ... */ };
    
/*====================================================================*/

static int pcmem_ioctl(struct inode *inode, struct file *file,
		       u_int cmd, u_long arg);
static FS_SIZE_T pcmem_read(struct inode *inode, struct file *file,
			    char *buf, U_FS_SIZE_T count);
static FS_SIZE_T pcmem_write(struct inode *inode, struct file *file,
			     CONST char *buf, U_FS_SIZE_T count);
static int pcmem_open(struct inode *inode, struct file *file);
static FS_RELEASE_T pcmem_close(struct inode *inode, struct file *file);
static FS_RELEASE_T pcmem_blk_close(struct inode *inode, struct file *file);

static struct file_operations pcmem_chr_fops = {
    NULL,		/* lseek */
    pcmem_read,		/* read */
    pcmem_write,	/* write */
    NULL,		/* readdir */
    NULL,		/* select */
    NULL,		/* ioctl */
    NULL,		/* mmap */
    pcmem_open,		/* open */
    pcmem_close,	/* release */
    NULL		/* fsync */
};

static struct file_operations pcmem_blk_fops = {
    NULL,		/* lseek */
    block_read,		/* read */
    block_write,	/* write */
    NULL,		/* readdir */
    NULL,		/* select */
    pcmem_ioctl,	/* ioctl */
    NULL,		/* mmap */
    pcmem_open,		/* open */
    pcmem_blk_close,	/* release */
    block_fsync		/* fsync */
};

/*====================================================================*/

static void cs_error(int func, int ret)
{
    CardServices(ReportError, dev_info, (void *)func, (void *)ret);
}

/*======================================================================

    pcmem_attach() creates an "instance" of the driver, allocating
    local data structures for one device.  The device is registered
    with Card Services.

======================================================================*/

static dev_link_t *pcmem_attach(void)
{
    client_reg_t client_reg;
    dev_link_t *link;
    pcmem_dev_t *dev;
    int i, ret;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "pcmem_attach()\n");
#endif

    for (i = 0; i < MAX_DEV; i++)
	if (dev_table[i] == NULL) break;
    if (i == MAX_DEV) {
	printk(KERN_NOTICE "pcmem_cs: no devices available\n");
	return NULL;
    }
    
    /* Create new memory card device */
    link = kmalloc(sizeof(struct dev_link_t), GFP_KERNEL);
    dev_table[i] = link;
    memset(link, 0, sizeof(struct dev_link_t));
    link->release.function = &pcmem_release;
    link->release.data = (u_long)link;
    
    dev = kmalloc(sizeof(struct pcmem_dev_t), GFP_KERNEL);
    memset(dev, 0, sizeof(struct pcmem_dev_t));
    link->priv = dev;

    /* Register with Card Services */
    client_reg.dev_info = &dev_info;
    client_reg.Attributes = INFO_IO_CLIENT | INFO_CARD_SHARE;
    client_reg.EventMask =
	CS_EVENT_RESET_PHYSICAL | CS_EVENT_CARD_RESET |
	CS_EVENT_CARD_INSERTION | CS_EVENT_CARD_REMOVAL |
	CS_EVENT_PM_SUSPEND | CS_EVENT_PM_RESUME;
    client_reg.event_handler = &pcmem_event;
    client_reg.Version = 0x0210;
    client_reg.event_callback_args.client_data = link;
    ret = CardServices(RegisterClient, &link->handle, &client_reg);
    if (ret != 0) {
	cs_error(RegisterClient, ret);
	pcmem_detach(link);
	return NULL;
    }

    return link;
} /* pcmem_attach */

/*======================================================================

    This deletes a driver "instance".  The device is de-registered
    with Card Services.  If it has been released, all local data
    structures are freed.  Otherwise, the structures will be freed
    when the device is released.

======================================================================*/

static void pcmem_detach(dev_link_t *link)
{
    int i;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "pcmem_detach(0x%p)\n", link);
#endif
    
    /* Locate device structure */
    for (i = 0; i < MAX_DEV; i++)
	if (dev_table[i] == link) break;
    if (i == MAX_DEV)
	return;

    if (link->state & DEV_CONFIG) {
	printk(KERN_INFO "detach postponed, '%s' still locked\n",
	       link->dev->dev_name);
	link->state |= DEV_STALE_LINK;
	return;
    }

    if (link->handle)
	CardServices(DeregisterClient, link->handle);
    
    /* Unlink device structure, free bits */
    dev_table[i] = NULL;
    kfree_s(link->priv, sizeof(struct pcmem_dev_t));
    kfree_s(link, sizeof(struct dev_link_t));
    
} /* pcmem_detach */

/*======================================================================

    Figure out the size of an arbitrary SRAM card
    
======================================================================*/

#define SIZE_STEP	0x10000
#define SIZE_NSTEP	(HIGH_ADDR/SIZE_STEP)

static void get_size(dev_link_t *link, pcmem_dev_t *dev)
{
    modwin_t mod;
    memreq_t mem;
    u_char *buf = kmalloc(SIZE_NSTEP, GFP_KERNEL);
    int s, t, ret;

    dev->size = 0;
    mod.Attributes = WIN_ENABLE | WIN_MEMORY_TYPE_CM;
    mod.AccessSpeed = 0;
    ret = CardServices(ModifyWindow, link->win, &mod);
    if (ret != CS_SUCCESS)
	cs_error(ModifyWindow, ret);

    /* Look for wrap-around or dead end */
    mem.Page = 0;
    for (s = 0; s < SIZE_NSTEP; s++) {
	mem.CardOffset = s * SIZE_STEP;
	CardServices(MapMemPage, link->win, &mem);
	buf[s] = readb(dev->Base);
	writeb(~buf[s], dev->Base);
	for (t = 0; t < s; t++) {
	    mem.CardOffset = t * SIZE_STEP;
	    CardServices(MapMemPage, link->win, &mem);
	    if (readb(dev->Base) != buf[t]) {
		writeb(buf[t], dev->Base);
		break;
	    }
	}
	if (t < s) break;
	mem.CardOffset = s * SIZE_STEP;
	CardServices(MapMemPage, link->win, &mem);
	if (readb(dev->Base) != ~buf[s]) break;
	writeb(buf[s], dev->Base);
    }

    /* Restore that last byte on wrap-around */
    if (t < s) {
	mem.CardOffset = t * SIZE_STEP;
	CardServices(MapMemPage, link->win, &mem);
	writeb(buf[t], dev->Base);
    }

    dev->size = s * SIZE_STEP;
    kfree(buf);
} /* get_size */

/*======================================================================

    pcmem_config() is scheduled to run after a CARD_INSERTION event
    is received, to configure the PCMCIA socket, and to make the
    memory device available to the system.
    
======================================================================*/

static void pcmem_config(dev_link_t *link)
{
    pcmem_dev_t *dev;
    status_t status;
    win_req_t req;
    int i, ret;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "pcmem_config(0x%p)\n", link);
#endif

    /* Configure card */
    link->state |= DEV_CONFIG;

    for (i = 0; i < MAX_DEV; i++)
	if (dev_table[i] == link) break;

    do {
	/* Allocate a 4K memory window */
	if (word_width)
	    req.Attributes = WIN_DATA_WIDTH_16;
	else
	    req.Attributes = WIN_DATA_WIDTH_8;
	req.Base = NULL; req.Size = WINDOW_SIZE;
	req.AccessSpeed = mem_speed;
	link->win = (window_handle_t)link->handle;
	ret = CardServices(RequestWindow, &link->win, &req);
	if (ret != 0) {
	    cs_error(RequestWindow, ret);
	    break;
	}
	/* Get write protect status */
	ret = CardServices(GetStatus, link->handle, &status);
	if (ret != 0) {
	    cs_error(GetStatus, ret);
	    break;
	}
    } while (0);
    if (ret != CS_SUCCESS) {
	link->state &= ~DEV_CONFIG_PENDING;
	pcmem_release((u_long)link);
	return;
    }
    
    dev = (pcmem_dev_t *)link->priv;
    dev->Base = req.Base;
    if (status.CardState & CS_EVENT_WRITE_PROTECT) {
	dev->flags |= MEM_WRPROT;
	dev->size = 0;
    }
    else
	get_size(link, dev);

    sprintf(dev->node.dev_name, "pcmem%d", i);
    printk(KERN_INFO "pcmem_cs: loading ");
    if (dev->size) {
	if ((dev->size & 0x0fffff) == 0)
	    printk("%lumb ", dev->size >> 20);
	else
	    printk("%luk ", dev->size >> 10);
    }
    else
	dev->size = HIGH_ADDR;
    if (dev->flags & MEM_WRPROT)
	printk("write-protected ");
    printk("memory card '%s'\n", dev->node.dev_name);
    dev->node.major = major_dev;
    dev->node.minor = MINOR_NR(i);
    link->dev = &dev->node;
    link->state &= ~DEV_CONFIG_PENDING;
    
} /* pcmem_config */

/*======================================================================

    After a card is removed, pcmem_release() will unregister the 
    device, and release the PCMCIA configuration.  If the device is
    still open, this will be postponed until it is closed.
    
======================================================================*/

static void pcmem_release(u_long arg)
{
    dev_link_t *link = (dev_link_t *)arg;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "pcmem_release(0x%p)\n", link);
#endif

    if (link->open) {
#ifdef PCMCIA_DEBUG
	if (pc_debug)
	    printk(KERN_DEBUG "pcmem_cs: release postponed, '%s' "
		   "still open\n", link->dev->dev_name);
#endif
	link->state |= DEV_STALE_CONFIG;
	return;
    }

    if (link->win)
	CardServices(ReleaseWindow, link->win);
    link->state &= ~DEV_CONFIG;
    link->dev = NULL;
    
    if (link->state & DEV_STALE_LINK)
	pcmem_detach(link);
    
} /* pcmem_release */

/*======================================================================

    The card status event handler.  Mostly, this schedules other
    stuff to run after an event is received.  A CARD_REMOVAL event
    also sets some flags to discourage the net drivers from trying
    to talk to the card any more.
    
======================================================================*/

static int pcmem_event(event_t event, int priority,
		       event_callback_args_t *args)
{
    dev_link_t *link = args->client_data;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "pcmem_event()\n");
#endif
    
    switch (event) {
#ifdef PCMCIA_DEBUG
    case CS_EVENT_REGISTRATION_COMPLETE:
	if (pc_debug)
	    printk(KERN_DEBUG "registration complete\n");
	break;
#endif
    case CS_EVENT_CARD_REMOVAL:
	link->state &= ~DEV_PRESENT;
	if (link->state & DEV_CONFIG) {
	    link->release.expires = RUN_AT(HZ/20);
	    add_timer(&link->release);
	}
	break;
    case CS_EVENT_CARD_INSERTION:
	link->state |= DEV_PRESENT | DEV_CONFIG_PENDING;
	pcmem_config(link);
	break;
    case CS_EVENT_PM_SUSPEND:
	link->state |= DEV_SUSPEND;
	/* Fall through... */
    case CS_EVENT_RESET_PHYSICAL:
	break;
    case CS_EVENT_PM_RESUME:
	link->state &= ~DEV_SUSPEND;
	/* Fall through... */
    case CS_EVENT_CARD_RESET:
	break;
    }
    return 0;
} /* pcmem_event */

/*======================================================================

    For now, all this does is verify that the device exists, and
    makes sure the module won't be unloaded while a file is open.
    
======================================================================*/

static int pcmem_open(struct inode *inode, struct file *file)
{
    dev_link_t *link;
    pcmem_dev_t *dev;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "pcmem_open(%d)\n", MINOR(inode->i_rdev));
#endif

    link = dev_table[DEVICE_NR(inode->i_rdev)];
    if (!DEV_OK(link))
	return -ENODEV;
    
    dev = (pcmem_dev_t *)link->priv;
    
    if ((file->f_mode & 2) && (dev->flags & MEM_WRPROT))
	return -EROFS;
    
    MOD_INC_USE_COUNT;
    return 0;
} /* pcmem_open */

/*====================================================================*/

static FS_RELEASE_T pcmem_close(struct inode *inode, struct file *file)
{
    dev_link_t *link;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "pcmem_close(%d)\n", MINOR(inode->i_rdev));
#endif

    link = dev_table[DEVICE_NR(inode->i_rdev)];
    if (link == NULL) return (FS_RELEASE_T)0;

    MOD_DEC_USE_COUNT;
    return (FS_RELEASE_T)0;
} /* pcmem_close */

static FS_RELEASE_T pcmem_blk_close(struct inode *inode,
				    struct file *file)
{
    fsync_dev(inode->i_rdev);
#if (LINUX_VERSION_CODE >= VERSION(1,3,0))
    invalidate_inodes(inode->i_rdev);
#endif
    invalidate_buffers(inode->i_rdev);
    return pcmem_close(inode, file);
}

/*======================================================================

    Read for character-mode device
    
======================================================================*/

static FS_SIZE_T pcmem_read(struct inode *inode, struct file *file,
			    char *buf, U_FS_SIZE_T count)
{
    int minor = MINOR(inode->i_rdev);
    dev_link_t *link;
    pcmem_dev_t *dev;
    u_long p, read, from, nb;
    int i, ret;
    modwin_t mod;
    memreq_t mem;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "pcmem_read(%d, %d)\n", minor, count);
#endif

    link = dev_table[DEVICE_NR(inode->i_rdev)];
    
    if (!DEV_OK(link))
	return -ENODEV;
    dev = (pcmem_dev_t *)link->priv;

    p = file->f_pos;
    if (count < 0)
	return -EINVAL;
    if (p >= HIGH_ADDR)
	return -EFBIG;
    if (p >= dev->size)
	return -ENOSPC;
    /* Don't read more than # of bytes left */
    if (count > dev->size - p)
	count = dev->size - p;

    mod.Attributes = WIN_ENABLE;
    mod.AccessSpeed = 0;
    if (minor & 1) {
	mod.Attributes |= WIN_MEMORY_TYPE_AM;
	p = p * 2; count = count * 2;
    }
    ret = CardServices(ModifyWindow, link->win, &mod);
    if (ret != CS_SUCCESS) {
	cs_error(ModifyWindow, ret);
	return -EIO;
    }
    
    mem.CardOffset = p & ~(WINDOW_SIZE-1);
    mem.Page = 0;
    from = p & (WINDOW_SIZE-1);
    read = 0;
    for ( ; count > 0; count -= nb) {
	ret = CardServices(MapMemPage, link->win, &mem);
	if (ret != CS_SUCCESS) {
	    cs_error(MapMemPage, ret);
	    return -EIO;
	}
	nb = (from+count > WINDOW_SIZE) ? WINDOW_SIZE-from : count;
	if (minor & 1) {
	    for (i = 0; i < nb; i += 2, buf++)
		put_user(readb(dev->Base+from+i), buf);
	    read += nb>>1;
	}
	else {
	    copy_pc_to_user(buf, dev->Base+from, nb);
	    buf += nb;
	    read += nb;
	}
        from = 0;
	mem.CardOffset += WINDOW_SIZE;
    }

    file->f_pos += read;
    return read;
} /* pcmem_read */

/*======================================================================

    Write for character-mode device
    
======================================================================*/

static FS_SIZE_T pcmem_write(struct inode *inode, struct file *file,
			     CONST char *buf, U_FS_SIZE_T count)
{
    int minor = MINOR(inode->i_rdev);
    dev_link_t *link;
    pcmem_dev_t *dev;
    u_long p, wrote, to, nb;
    int i, ret;
    modwin_t mod;
    memreq_t mem;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "pcmem_write(%d, %d)\n", minor, count);
#endif
    
    link = dev_table[DEVICE_NR(inode->i_rdev)];
    
    if (!DEV_OK(link))
	return -ENODEV;
    
    dev = (pcmem_dev_t *)link->priv;
    
    /* Check for write protect */
    if (dev->flags & MEM_WRPROT)
	return -EROFS;
    
    p = file->f_pos;
    if (count < 0)
	return -EINVAL;
    if (p >= HIGH_ADDR)
	return -EFBIG;
    if (p >= dev->size)
        return -ENOSPC;
    if (count > dev->size - p)
	count = dev->size - p;

    mod.Attributes = WIN_ENABLE;
    mod.AccessSpeed = 0;
    if (minor & 1) {
	mod.Attributes |= WIN_MEMORY_TYPE_AM;
	p = p * 2; count = count * 2;
    }
    ret = CardServices(ModifyWindow, link->win, &mod);
    if (ret != CS_SUCCESS) {
	cs_error(ModifyWindow, ret);
	return -EIO;
    }
    
    mem.CardOffset = p & ~(WINDOW_SIZE-1);
    mem.Page = 0;
    to = p & (WINDOW_SIZE-1);
    wrote = 0;
    for ( ; count > 0; count -= nb) {
	ret = CardServices(MapMemPage, link->win, &mem);
	if (ret != CS_SUCCESS) {
	    cs_error(MapMemPage, ret);
	    return -EIO;
	}
	nb = (to+count > WINDOW_SIZE) ? WINDOW_SIZE-to : count;
	if (minor & 1) {
	    for (i = 0; i < nb; i += 2, buf++) {
		char c;
		get_user(c, buf);
		writeb(c, dev->Base+to+i);
	    }
	    wrote += nb>>1;
	}
	else {
	    copy_user_to_pc(dev->Base+to, buf, nb);
	    buf += nb;
	    wrote += nb;
	}
        to = 0;
	mem.CardOffset += WINDOW_SIZE;
    }

    file->f_pos += wrote;
    return wrote;
} /* pcmem_write */

/*======================================================================

    IOCTL calls for getting device parameters.

======================================================================*/

static int pcmem_ioctl(struct inode *inode, struct file *file,
		       u_int cmd, u_long arg)
{
    dev_link_t *link;
    pcmem_dev_t *dev;
    int ret = 0;
    
    link = dev_table[DEVICE_NR(inode->i_rdev)];
    if (!DEV_OK(link)) return -ENODEV;
    dev = (pcmem_dev_t *)link->priv;

    switch (cmd) {
    case BLKGETSIZE:
	if (dev->size == 0) return -EINVAL;
	ret = verify_area(VERIFY_WRITE, (long *)arg, sizeof(long));
	if (ret) break;
	put_user(dev->size/SECTOR_SIZE, (long *)arg);
	break;
    case BLKFLSBUF:
	if (!suser()) return -EACCES;
	if (!(inode->i_rdev)) return -EINVAL;
	fsync_dev(inode->i_rdev);
	invalidate_buffers(inode->i_rdev);
	break;
    default:
	ret = -EINVAL;
    }
    return ret;
} /* pcmem_ioctl */

/*======================================================================

    Handler for block device requests
    
======================================================================*/

static void do_pcmem_request(void)
{
    int addr, len, from, nb, ret;
    char *buf;
    dev_link_t *link;
    pcmem_dev_t *dev;
    modwin_t mod;
    memreq_t mem;
    
    sti();
    do {
	INIT_REQUEST;

	link = dev_table[DEVICE_NR(DEVICE(CURRENT))];
	
	if (!DEV_OK(link)) {
	    end_request(0);
	    continue;
	}
	
	dev = (pcmem_dev_t *)link->priv;
	
	mod.Attributes = WIN_ENABLE;
	mod.AccessSpeed = 0;
	ret = CardServices(ModifyWindow, link->win, &mod);
	if (ret != CS_SUCCESS) {
	    cs_error(ModifyWindow, ret);
	    end_request(0);
	    continue;
	}

	addr = CURRENT->sector * SECTOR_SIZE;
	mem.Page = 0;
	mem.CardOffset = addr & ~(WINDOW_SIZE-1);
	from = addr & (WINDOW_SIZE-1);
	len = CURRENT->current_nr_sectors * SECTOR_SIZE;
	buf = CURRENT->buffer;
	ret = 0;
	
	switch (CURRENT->cmd) {
	    
	case READ:
	    for ( ; len > 0; len -= nb, from = 0) {
		ret = CardServices(MapMemPage, link->win, &mem);
		if (ret != CS_SUCCESS) break;
		nb = (from+len > WINDOW_SIZE) ? WINDOW_SIZE-from : len;
		copy_from_pc(buf, &dev->Base[from], nb);
		mem.CardOffset += WINDOW_SIZE;
	    }
	    break;
	    
	case WRITE:
	    for ( ; len > 0; len -= nb, from = 0) {
		ret = CardServices(MapMemPage, link->win, &mem);
		if (ret != CS_SUCCESS) break;
		nb = (from+len > WINDOW_SIZE) ? WINDOW_SIZE-from : len;
		copy_to_pc(&dev->Base[from], buf, nb);
		mem.CardOffset += WINDOW_SIZE;
	    }
	    break;
	    
	default:
	    panic("pcmem_cs: unknown block command!\n");
	    
	}
	if (ret == CS_SUCCESS)
	    end_request(1);
	else {
	    cs_error(MapMemPage, ret);
	    end_request(0);
	}
    } while (1);
} /* do_pcmem_request */

/*====================================================================*/

int init_module(void)
{
    servinfo_t serv;
    int i;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_INFO "%s\n", version);
#endif
    
    CardServices(GetCardServicesInfo, &serv);
    if (serv.Revision != CS_RELEASE_CODE) {
	printk(KERN_NOTICE "pcmem_cs: Card Services release "
	       "does not match!\n");
	return -1;
    }
    
    register_pcmcia_driver(&dev_info, &pcmem_attach, &pcmem_detach);

    for (i = MAX_CHRDEV-1; i > 0; i--) {
	if (register_chrdev(i, "pcmem", &pcmem_chr_fops) == 0) {
	    if (register_blkdev(i, "pcmem", &pcmem_blk_fops) == 0)
		break;
	    else
		unregister_chrdev(i, "pcmem");
	}
    }
    if (i == 0)
	printk(KERN_NOTICE "pcmem_cs: unable to grab a device #\n");
    else
	major_dev = i;

    blk_dev[major_dev].request_fn = DEVICE_REQUEST;
    for (i = 0; i < MINOR_NR(MAX_DEV); i++)
	pcmem_blocksizes[i] = 1024;
    blksize_size[major_dev] = pcmem_blocksizes;
    
    return 0;
}

void cleanup_module(void)
{
    int i;
    dev_link_t *link;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "pcmem_cs: unloading\n");
#endif
    unregister_pcmcia_driver(&dev_info);
    if (major_dev != 0) {
	unregister_chrdev(major_dev, "pcmem");
	unregister_blkdev(major_dev, "pcmem");
    }
    for (i = 0; i < MAX_DEV; i++) {
	link = dev_table[i];
	if (link) {
	    if (link->state & DEV_CONFIG)
		pcmem_release((u_long)link);
	    pcmem_detach(link);
	}
    }
}
