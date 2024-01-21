/*======================================================================

    A general driver for accessing PCMCIA card memory via Bulk
    Memory Services.

    This driver provides the equivalent of /dev/mem for a PCMCIA
    card's attribute and common memory.  It includes character
    and block devices.

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
#include <linux/ioctl.h>
#include <asm/io.h>
#include <asm/system.h>
#include <asm/segment.h>
#include <stdarg.h>

#include <pcmcia/version.h>
#include <pcmcia/cs_types.h>
#include <pcmcia/cs.h>
#include <pcmcia/bulkmem.h>
#include <pcmcia/cistpl.h>
#include <pcmcia/ds.h>
#include <pcmcia/memory.h>

/* Major device #'s for memory device */
static int major_dev = 0;

/* Funky stuff for setting up a block device */
#define MAJOR_NR		major_dev
#define DEVICE_NAME		"memory"
#define DEVICE_REQUEST		do_memory_request
#define DEVICE_ON(device)
#define DEVICE_OFF(device)

#define DEVICE_NR(minor)	((minor)>>3)
#define REGION_NR(minor)	((minor)&7)
#define MINOR_NR(dev,attr,rgn)	(((dev)<<3)+((attr)<<2)+(rgn))

#include BLK_DEV_HDR

#ifdef PCMCIA_DEBUG
static int pc_debug = PCMCIA_DEBUG;
MODULE_PARM(pc_debug, "i");
static char *version =
"memory_cs.c 1.30 1997/04/15 16:12:37 (David Hinds)";
#endif

/*====================================================================*/

/* Maximum number of separate memory devices we'll allow */
#define MAX_DEV		8

/* Maximum number of outstanding erase requests per socket */
#define MAX_ERASE	8

/* Sector size -- shouldn't need to change */
#define SECTOR_SIZE	512

static void memory_config(dev_link_t *link);
static void memory_release(u_long arg);
static int memory_event(event_t event, int priority,
			event_callback_args_t *args);

static dev_link_t *memory_attach(void);
static void memory_detach(dev_link_t *);

/* Each memory region corresponds to a minor device */
typedef struct minor_dev_t {
    dev_node_t		dev;
    region_info_t	region;
    memory_handle_t	handle;
    int			open;
} minor_dev_t;

typedef struct memory_dev_t {
    eraseq_handle_t	eraseq_handle;
    eraseq_entry_t	eraseq[MAX_ERASE];
    struct wait_queue	*erase_pending;
    minor_dev_t		minor[2*CISTPL_MAX_DEVICES];
} memory_dev_t;

static dev_info_t dev_info = "memory_cs";
static dev_link_t *dev_table[MAX_DEV] = { NULL, /* ... */ };

static int memory_blocksizes[MINOR_NR(MAX_DEV, 0, 0)] =
{ 0, /* ... */ };
    
/*====================================================================*/

static int memory_ioctl(struct inode *inode, struct file *file,
			u_int cmd, u_long arg);
static FS_SIZE_T memory_read(struct inode *inode, struct file *file,
			     char *buf, U_FS_SIZE_T count);
static FS_SIZE_T memory_write(struct inode *inode, struct file *file,
			      CONST char *buf, U_FS_SIZE_T count);
static int memory_open(struct inode *inode, struct file *file);
static FS_RELEASE_T memory_close(struct inode *inode,
				 struct file *file);
static FS_RELEASE_T memory_blk_close(struct inode *inode,
				     struct file *file);

static struct file_operations memory_chr_fops = {
    NULL,		/* lseek */
    memory_read,	/* read */
    memory_write,      	/* write */
    NULL,		/* readdir */
    NULL,		/* select */
    memory_ioctl,	/* ioctl */
    NULL,		/* mmap */
    memory_open,	/* open */
    memory_close,	/* release */
    NULL		/* fsync */
};

static struct file_operations memory_blk_fops = {
    NULL,		/* lseek */
    block_read,		/* read */
    block_write,	/* write */
    NULL,		/* readdir */
    NULL,		/* select */
    memory_ioctl,	/* ioctl */
    NULL,		/* mmap */
    memory_open,	/* open */
    memory_blk_close,	/* release */
    block_fsync		/* fsync */
};

/*====================================================================*/

static void cs_error(int func, int ret)
{
    CardServices(ReportError, dev_info, (void *)func, (void *)ret);
}

/*======================================================================

    memory_attach() creates an "instance" of the driver, allocating
    local data structures for one device.  The device is registered
    with Card Services.

======================================================================*/

static dev_link_t *memory_attach(void)
{
    client_reg_t client_reg;
    dev_link_t *link;
    memory_dev_t *dev;
    eraseq_hdr_t eraseq_hdr;
    int i, ret;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "memory_attach()\n");
#endif

    for (i = 0; i < MAX_DEV; i++)
	if (dev_table[i] == NULL) break;
    if (i == MAX_DEV) {
	printk(KERN_NOTICE "memory_cs: no devices available\n");
	return NULL;
    }
    
    /* Create new memory card device */
    link = kmalloc(sizeof(struct dev_link_t), GFP_KERNEL);
    dev_table[i] = link;
    memset(link, 0, sizeof(struct dev_link_t));
    link->release.function = &memory_release;
    link->release.data = (u_long)link;
    
    dev = kmalloc(sizeof(struct memory_dev_t), GFP_KERNEL);
    memset(dev, 0, sizeof(memory_dev_t));
    init_waitqueue(&dev->erase_pending);
    link->priv = dev;

    /* Register with Card Services */
    client_reg.dev_info = &dev_info;
    client_reg.Attributes = INFO_IO_CLIENT | INFO_CARD_SHARE;
    client_reg.EventMask =
	CS_EVENT_RESET_PHYSICAL | CS_EVENT_CARD_RESET |
	CS_EVENT_CARD_INSERTION | CS_EVENT_CARD_REMOVAL |
	CS_EVENT_PM_SUSPEND | CS_EVENT_PM_RESUME;
    client_reg.event_handler = &memory_event;
    client_reg.Version = 0x0210;
    client_reg.event_callback_args.client_data = link;
    ret = CardServices(RegisterClient, &link->handle, &client_reg);
    if (ret != 0) {
	cs_error(RegisterClient, ret);
	memory_detach(link);
	return NULL;
    }

    for (i = 0; i < MAX_ERASE; i++)
	dev->eraseq[i].State = ERASE_IDLE;
    eraseq_hdr.QueueEntryCnt = MAX_ERASE;
    eraseq_hdr.QueueEntryArray = dev->eraseq;
    dev->eraseq_handle = (void *)link->handle;
    ret = CardServices(RegisterEraseQueue, &dev->eraseq_handle, &eraseq_hdr);
    if (ret != 0) {
	cs_error(RegisterEraseQueue, ret);
	dev->eraseq_handle = NULL;
	memory_detach(link);
	return NULL;
    }
    
    return link;
} /* memory_attach */

/*======================================================================

    This deletes a driver "instance".  The device is de-registered
    with Card Services.  If it has been released, all local data
    structures are freed.  Otherwise, the structures will be freed
    when the device is released.

======================================================================*/

static void memory_detach(dev_link_t *link)
{
    memory_dev_t *dev;
    int i;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "memory_detach(0x%p)\n", link);
#endif
    
    /* Locate device structure */
    for (i = 0; i < MAX_DEV; i++)
	if (dev_table[i] == link) break;
    if (i == MAX_DEV)
	return;

    if (link->state & DEV_CONFIG) {
	memory_release((u_long)link);
	if (link->state & DEV_STALE_CONFIG) {
	    link->state |= DEV_STALE_LINK;
	    return;
	}
    }

    dev = (memory_dev_t *)link->priv;
    if (dev->eraseq_handle)
	CardServices(DeregisterEraseQueue, dev->eraseq_handle);
    if (link->handle)
	CardServices(DeregisterClient, link->handle);
    
    /* Unlink device structure, free bits */
    dev_table[i] = NULL;
    kfree_s(dev, sizeof(struct memory_dev_t));
    kfree_s(link, sizeof(struct dev_link_t));
    
} /* memory_detach */

/*======================================================================

    memory_config() is scheduled to run after a CARD_INSERTION event
    is received, to configure the PCMCIA socket, and to make the
    ethernet device available to the system.
    
======================================================================*/

static void memory_config(dev_link_t *link)
{
    memory_dev_t *dev;
    minor_dev_t *minor;
    dev_node_t **tail;
    region_info_t region;
    int i, attr, ret, nr[2];

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "memory_config(0x%p)\n", link);
#endif

    /* Configure card */
    link->state |= DEV_CONFIG;

    for (i = 0; i < MAX_DEV; i++)
	if (dev_table[i] == link) break;
    
    dev = (memory_dev_t *)link->priv;
    tail = &link->dev;
    for (attr = 0; attr < 2; attr++) {
	minor = dev->minor + attr*CISTPL_MAX_DEVICES;
	nr[attr] = 0;
	region.Attributes =
	    (attr) ? REGION_TYPE_AM : REGION_TYPE_CM;
	ret = CardServices(GetFirstRegion, link->handle, &region);
	while (ret == CS_SUCCESS) {
	    minor->region = region;
	    sprintf(minor->dev.dev_name, "mem%d%c%d",
		    i, attr ? 'a' : 'c', nr[attr]);
	    minor->dev.major = major_dev;
	    minor->dev.minor = MINOR_NR(i, attr, nr[attr]);
	    *tail = &minor->dev; tail = &minor->dev.next;
	    minor++; nr[attr]++;
	    ret = CardServices(GetNextRegion, link->handle, &region);
	}
    }
    *tail = NULL;
    
    link->state &= ~DEV_CONFIG_PENDING;
    
    if ((nr[0] == 0) && (nr[1] == 0))
	printk(KERN_NOTICE "memory_cs: no regions found!\n");
    else {
	printk(KERN_INFO "memory_cs: mem%d:", i);
	for (attr = 0; attr < 2; attr++) {
	    minor = dev->minor + attr*CISTPL_MAX_DEVICES;
	    if (attr && nr[0] && nr[1])
		printk(",");
	    if (nr[attr])
		printk(" %s", attr ? "attribute" : "common");
	    for (i = 0; i < nr[attr]; i++) {
		if (minor[i].region.RegionSize & 0x03ff)
		    printk(" %ld bytes", minor[i].region.RegionSize);
		else if (minor[i].region.RegionSize & 0x0fffff)
		    printk(" %ld kb", minor[i].region.RegionSize >> 10);
		else
		    printk(" %ld mb", minor[i].region.RegionSize >> 20);
	    }
	}
	printk("\n");
    }
    
} /* memory_config */

/*======================================================================

    After a card is removed, memory_release() will unregister the 
    device, and release the PCMCIA configuration.  If the device is
    still open, this will be postponed until it is closed.
    
======================================================================*/

static void memory_release(u_long arg)
{
    dev_link_t *link = (dev_link_t *)arg;
    int i;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "memory_release(0x%p)\n", link);
#endif

    for (i = 0; i < MAX_DEV; i++)
	if (dev_table[i] == link) break;
    if (link->open) {
#ifdef PCMCIA_DEBUG
	if (pc_debug)
	    printk(KERN_DEBUG "memory_cs: release postponed, 'mem%d'"
		   " still open\n", i);
#endif
	link->state |= DEV_STALE_CONFIG;
	return;
    }

    link->dev = NULL;
    if (link->win)
	CardServices(ReleaseWindow, link->win);
    link->state &= ~DEV_CONFIG;
    
    if (link->state & DEV_STALE_LINK)
	memory_detach(link);
    
} /* memory_release */

/*======================================================================

    The card status event handler.  Mostly, this schedules other
    stuff to run after an event is received.  A CARD_REMOVAL event
    also sets some flags to discourage the driver from trying
    to talk to the card any more.
    
======================================================================*/

static int memory_event(event_t event, int priority,
		       event_callback_args_t *args)
{
    dev_link_t *link = args->client_data;
    memory_dev_t *dev;
    eraseq_entry_t *erase;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "memory_event()\n");
#endif
    
    switch (event) {
#ifdef PCMCIA_DEBUG
    case CS_EVENT_REGISTRATION_COMPLETE:
	if (pc_debug)
	    printk(KERN_DEBUG "memory_cs: registration complete\n");
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
	memory_config(link);
	break;
    case CS_EVENT_ERASE_COMPLETE:
	erase = (eraseq_entry_t *)(args->info);
	wake_up((struct wait_queue **)&erase->Optional);
	dev = (memory_dev_t *)(link->priv);
	wake_up_interruptible(&dev->erase_pending);
	break;
    case CS_EVENT_PM_SUSPEND:
	link->state |= DEV_SUSPEND;
	/* Fall through... */
    case CS_EVENT_RESET_PHYSICAL:
	/* get_lock(link); */
	break;
    case CS_EVENT_PM_RESUME:
	link->state &= ~DEV_SUSPEND;
	/* Fall through... */
    case CS_EVENT_CARD_RESET:
	/* free_lock(link); */
	break;
    }
    return 0;
} /* memory_event */

/*======================================================================

    This gets a memory handle for the region corresponding to the
    minor device number.
    
======================================================================*/

static int memory_open(struct inode *inode, struct file *file)
{
    int minor = MINOR(inode->i_rdev);
    dev_link_t *link;
    memory_dev_t *dev;
    minor_dev_t *minor_dev;
    open_mem_t open;
    int ret;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "memory_open(%d)\n", minor);
#endif

    link = dev_table[DEVICE_NR(minor)];
    if (!DEV_OK(link))
	return -ENODEV;
    
    dev = (memory_dev_t *)link->priv;
    minor_dev = &dev->minor[REGION_NR(minor)];
    if (minor_dev->region.RegionSize == 0)
	return -ENODEV;
    if (minor_dev->handle == NULL) {
	minor_dev->handle = (memory_handle_t)link->handle;
	open.Attributes = minor_dev->region.Attributes;
	open.Offset = minor_dev->region.CardOffset;
	ret = CardServices(OpenMemory, &minor_dev->handle, &open);
	if (ret != CS_SUCCESS)
	    return -ENOMEM;
    }
    file->private_data = minor_dev;
    
    minor_dev->open++;
    link->open++;
    MOD_INC_USE_COUNT;
    return 0;
} /* memory_open */

/*====================================================================*/

static FS_RELEASE_T memory_close(struct inode *inode,
				 struct file *file)
{
    dev_link_t *link;
    int minor = MINOR(inode->i_rdev);
    memory_dev_t *dev;
    minor_dev_t *minor_dev;
    
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "memory_close(%d)\n", minor);
#endif

    link = dev_table[DEVICE_NR(minor)];
    dev = (memory_dev_t *)link->priv;
    minor_dev = &dev->minor[REGION_NR(minor)];
    link->open--;
    minor_dev->open--;
    if (minor_dev->open == 0) {
	CardServices(CloseMemory, minor_dev->handle);
	minor_dev->handle = NULL;
    }

    MOD_DEC_USE_COUNT;
    return (FS_RELEASE_T)0;
} /* memory_close */

static void memory_blk_close(struct inode *inode, struct file *file)
{
    fsync_dev(inode->i_rdev);
#if (LINUX_VERSION_CODE >= VERSION(1,3,0))
    invalidate_inodes(inode->i_rdev);
#endif
    invalidate_buffers(inode->i_rdev);
    return memory_close(inode, file);
}

/*======================================================================

    Read for character-mode device
    
======================================================================*/

static FS_SIZE_T memory_read(struct inode *inode, struct file *file,
			     char *buf, U_FS_SIZE_T count)
{
    minor_dev_t *minor;
    mem_op_t req;
    int ret;

    minor = file->private_data;
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "memory_read(0x%p, 0x%lx, %d)\n",
	       minor->handle, (u_long)file->f_pos, count);
#endif
    req.Attributes = MEM_OP_BUFFER_USER;
    req.Offset = file->f_pos;
    req.Count = count;
    ret = CardServices(ReadMemory, minor->handle, &req, buf);
    if (ret == CS_SUCCESS) {
	file->f_pos += count;
	return count;
    }
    else
	return -EIO;
} /* memory_read */

/*======================================================================

    Erase a memory region.  This is used by the write routine for
    suitably aligned and sized blocks.  It is also used for the
    MEMERASE ioctl().
    
======================================================================*/

static int memory_erase(memory_dev_t *dev, minor_dev_t *minor_dev,
			u_long f_pos, int count)
{
    int i, ret;

    /* Find a free erase slot, or wait for one to become available */
    for (;;) {
	for (i = 0; i < MAX_ERASE; i++)
	    if (!ERASE_IN_PROGRESS(dev->eraseq[i].State)) break;
	if (i < MAX_ERASE) break;
#ifdef PCMCIA_DEBUG
	if (pc_debug)
	    printk(KERN_DEBUG "waiting for erase slot...\n");
#endif
	interruptible_sleep_on(&dev->erase_pending);
	if (current->signal & ~current->blocked)
	    return -ERESTARTSYS;
    }

    /* Queue a new request */
    dev->eraseq[i].State = ERASE_QUEUED;
    dev->eraseq[i].Handle = minor_dev->handle;
    dev->eraseq[i].Offset = f_pos;
    dev->eraseq[i].Size = count;
    ret = CardServices(CheckEraseQueue, dev->eraseq_handle);
    if (ret != CS_SUCCESS) {
	cs_error(CheckEraseQueue, ret);
	return -EIO;
    }

    /* Wait for request to complete */
    init_waitqueue((struct wait_queue **)&dev->eraseq[i].Optional);
    if (ERASE_IN_PROGRESS(dev->eraseq[i].State))
	sleep_on((struct wait_queue **)&dev->eraseq[i].Optional);
    if (dev->eraseq[i].State != ERASE_PASSED)
	return -EIO;
    return 0;
}

/*======================================================================

    Write for character-mode device
    
======================================================================*/

static FS_SIZE_T memory_write(struct inode *inode, struct file *file,
			      CONST char *buf, U_FS_SIZE_T count)
{
    int minor = MINOR(inode->i_rdev);
    memory_dev_t *dev;
    minor_dev_t *minor_dev;
    mem_op_t req;
    int ret;

    dev = (memory_dev_t *)dev_table[DEVICE_NR(minor)]->priv;
    minor_dev = &dev->minor[REGION_NR(minor)];
#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "memory_write(0x%p, 0x%lx, %d)\n",
	       minor_dev->handle, (u_long)file->f_pos, count);
#endif
    if ((minor_dev->region.BlockSize > 1) &&
	((file->f_pos & (minor_dev->region.BlockSize-1)) == 0) &&
	((count & (minor_dev->region.BlockSize-1)) == 0)) {
	ret = memory_erase(dev, minor_dev, file->f_pos, count);
	if (ret != 0)
	    return ret;
    }
    
    req.Attributes = 0;
    req.Offset = file->f_pos;
    req.Count = count;
    ret = CardServices(WriteMemory, minor_dev->handle, &req, buf);
    if (ret == CS_SUCCESS) {
	file->f_pos += count;
	return count;
    }
    else
	return -EIO;
} /* memory_write */

/*======================================================================

    IOCTL calls for getting device parameters.

======================================================================*/

static int memory_ioctl(struct inode *inode, struct file *file,
		       u_int cmd, u_long arg)
{
    int minor = MINOR(inode->i_rdev);
    dev_link_t *link;
    memory_dev_t *dev;
    minor_dev_t *minor_dev;
    erase_info_t erase;
    u_int size;
    int ret = 0;

    link = dev_table[DEVICE_NR(minor)];
    
    if (!DEV_OK(link))
	return -ENODEV;
    dev = (memory_dev_t *)link->priv;
    minor_dev = &dev->minor[REGION_NR(minor)];

    size = (cmd & IOCSIZE_MASK) >> IOCSIZE_SHIFT;
    if (cmd & IOC_IN) {
	ret = verify_area(VERIFY_READ, (char *)arg, size);
	if (ret) return ret;
    }
    if (cmd & IOC_OUT) {
	ret = verify_area(VERIFY_WRITE, (char *)arg, size);
	if (ret) return ret;
    }

    switch (cmd) {
    case BLKGETSIZE:
	put_user(minor_dev->region.RegionSize/SECTOR_SIZE,
		 (long *)arg);
	break;
    case BLKFLSBUF:
	if (!suser()) return -EACCES;
	if (!(inode->i_rdev)) return -EINVAL;
	fsync_dev(inode->i_rdev);
	invalidate_buffers(inode->i_rdev);
	break;
    case MEMGETINFO:
	copy_to_user((region_info_t *)arg, &minor_dev->region,
		     sizeof(struct region_info_t));
	break;
    case MEMERASE:
	copy_from_user(&erase, (erase_info_t *)arg,
		       sizeof(struct erase_info_t));
	ret = memory_erase(dev, minor_dev, erase.Offset, erase.Size);
	break;
    default:
	ret = -EINVAL;
    }
    
    return ret;
} /* memory_ioctl */

/*======================================================================

    Handler for block device requests
    
======================================================================*/

static void do_memory_request(void)
{
    int ret, minor;
    char *buf;
    mem_op_t req;
    dev_link_t *link;
    memory_dev_t *dev;
    minor_dev_t *minor_dev;
    
    sti();
    do {
	INIT_REQUEST;

	minor = MINOR(DEVICE(CURRENT));
	
	link = dev_table[DEVICE_NR(minor)];
	dev = (memory_dev_t *)link->priv;
	minor_dev = &dev->minor[REGION_NR(minor)];

	req.Attributes = MEM_OP_BUFFER_KERNEL;
	req.Offset = CURRENT->sector * SECTOR_SIZE;
	req.Count = CURRENT->current_nr_sectors * SECTOR_SIZE;
	buf = CURRENT->buffer;
	ret = CS_SUCCESS;
	
	switch (CURRENT->cmd) {
	    
	case READ:
	    ret = CardServices(ReadMemory, minor_dev->handle,
			       &req, buf);
	    if (ret != CS_SUCCESS)
		cs_error(ReadMemory, ret);
	    break;
	    
	case WRITE:
	    ret = CardServices(WriteMemory, minor_dev->handle,
			       &req, buf);
	    if (ret != CS_SUCCESS)
		cs_error(WriteMemory, ret);
	    break;
	    
	default:
	    panic("memory_cs: unknown block command!\n");
	    
	}
	if (ret == CS_SUCCESS)
	    end_request(1);
	else
	    end_request(0);
    } while (1);
} /* do_memory_request */

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
	printk(KERN_NOTICE "memory_cs: Card Services release "
	       "does not match!\n");
	return -1;
    }
    
    register_pcmcia_driver(&dev_info, &memory_attach, &memory_detach);

    for (i = MAX_CHRDEV-1; i > 0; i--) {
	if (register_chrdev(i, "memory", &memory_chr_fops) == 0) {
	    if (register_blkdev(i, "memory", &memory_blk_fops) == 0)
		break;
	    else
		unregister_chrdev(i, "memory");
	}
    }
    if (i == 0)
	printk(KERN_NOTICE "memory_cs: unable to grab a device #\n");
    else
	major_dev = i;

    blk_dev[major_dev].request_fn = DEVICE_REQUEST;
    for (i = 0; i < MINOR_NR(MAX_DEV, 0, 0); i++)
	memory_blocksizes[i] = 1024;
    blksize_size[major_dev] = memory_blocksizes;
    
    return 0;
}

void cleanup_module(void)
{
    int i;
    dev_link_t *link;

#ifdef PCMCIA_DEBUG
    if (pc_debug)
	printk(KERN_DEBUG "memory_cs: unloading\n");
#endif
    unregister_pcmcia_driver(&dev_info);
    if (major_dev != 0) {
	unregister_chrdev(major_dev, "memory");
	unregister_blkdev(major_dev, "memory");
    }
    for (i = 0; i < MAX_DEV; i++) {
	link = dev_table[i];
	if (link) {
	    if (link->state & DEV_CONFIG)
		memory_release((u_long)link);
	    memory_detach(link);
	}
    }
}
