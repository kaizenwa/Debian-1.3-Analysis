/*======================================================================

    Resource management routines

    rsrc_mgr.c 1.2 1995/05/17 19:21:26 (David Hinds)
    
======================================================================*/

#include <pcmcia/config.h>
#define __NO_VERSION__
#include <pcmcia/k_compat.h>

#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/errno.h>
#include <linux/types.h>
#include <linux/malloc.h>
#include <linux/ioport.h>
#include <linux/timer.h>
#include <asm/io.h>

#include <pcmcia/cs_types.h>
#include <pcmcia/ss.h>
#include <pcmcia/cs.h>
#include <pcmcia/bulkmem.h>
#include <pcmcia/cistpl.h>
#include "cs_internal.h"
#include "rsrc_mgr.h"

/*====================================================================*/

typedef struct memory_entry_t {
    u_long base, num;
    char *name;
    struct memory_entry_t *next;
} memory_entry_t;

/* An ordered linked list of allocated memory blocks */
static memory_entry_t memlist = { 0, 0, NULL, NULL };

typedef struct resource_entry_t {
    u_long		base, num;
    struct		resource_entry_t *next;
} resource_entry_t;

/* Memory resource database */
resource_entry_t mem_db = { 0, 0, &mem_db };

/* IO port resource database */
resource_entry_t io_db = { 0, 0, &io_db };

typedef struct irq_info_t {
    u_long		Attributes;
    int			time_share, dyn_share;
    struct socket_info_t *Socket;
} irq_info_t;

/* Table of IRQ assignments */
irq_info_t irq_table[16] = { { 0, 0, 0 }, /* etc */ };

/*====================================================================*/

/* Parameters that can be set with 'insmod' */

/* Should we probe resources for conflicts? */
static int probe_io = 1;
static int probe_mem = 0;

MODULE_PARM(probe_io, "i");
MODULE_PARM(probe_mem, "i");

/*======================================================================

    These functions manage the database of allocated memory blocks.
    
======================================================================*/

static memory_entry_t *find_gap(memory_entry_t *root,
				memory_entry_t *entry)
{
    unsigned long flags;
    memory_entry_t *p;
    
    if (entry->base > entry->base+entry->num-1)
	return NULL;
    save_flags(flags);
    cli();
    for (p = root; ; p = p->next) {
	if ((p != root) && (p->base+p->num-1 >= entry->base)) {
	    p = NULL;
	    break;
	}
	if ((p->next == NULL) ||
	    (p->next->base > entry->base+entry->num-1))
	    break;
    }
    restore_flags(flags);
    return p;
}

int check_mem_region(u_long base, u_long num)
{
    memory_entry_t entry;
    entry.base = base;
    entry.num = num;
    return (find_gap(&memlist, &entry) == NULL) ? -EBUSY : 0;
}

int register_mem_region(u_long base, u_long num, char *name)
{
    unsigned long flags;
    memory_entry_t *p, *entry;

    entry = kmalloc(sizeof(memory_entry_t), GFP_KERNEL);
    entry->base = base;
    entry->num = num;
    entry->name = name;

    save_flags(flags);
    cli();
    p = find_gap(&memlist, entry);
    if (p == NULL) {
	restore_flags(flags);
	kfree_s(entry, sizeof(memory_entry_t));
	return -EBUSY;
    }
    entry->next = p->next;
    p->next = entry;
    restore_flags(flags);
    return 0;
}

int release_mem_region(u_long base, u_long num)
{
    unsigned long flags;
    memory_entry_t *p, *q;

    save_flags(flags);
    cli();
    for (p = &memlist; ; p = q) {
	q = p->next;
	if (q == NULL) break;
	if ((q->base == base) && (q->num == num)) {
	    p->next = q->next;
	    kfree_s(q, sizeof(memory_entry_t));
	    restore_flags(flags);
	    return 0;
	}
    }
    restore_flags(flags);
    return -EINVAL;
}

/*======================================================================

    These manage the internal databases of available resources
    
======================================================================*/

static int add_interval(resource_entry_t *map, u_long base, u_long num)
{
    resource_entry_t *p, *q;

    for (p = map; ; p = p->next) {
	if ((p != map) && (p->base+p->num-1 >= base))
	    return -1;
	if ((p->next == map) || (p->next->base > base+num-1))
	    break;
    }
    q = kmalloc(sizeof(resource_entry_t), GFP_KERNEL);
    q->base = base; q->num = num;
    q->next = p->next; p->next = q;
    return 0;
} /* add_interval */

/*====================================================================*/

static int sub_interval(resource_entry_t *map, u_long base, u_long num)
{
    resource_entry_t *p, *q;

    for (p = map; ; p = q) {
	q = p->next;
	if (q == map)
	    break;
	if ((q->base+q->num > base) && (base+num > q->base)) {
	    if (q->base >= base) {
		if (q->base+q->num <= base+num) {
		    /* Delete whole block */
		    p->next = q->next;
		    kfree_s(q, sizeof(resource_entry_t));
		    /* don't advance the pointer yet */
		    q = p;
		}
		else {
		    /* Cut off bit from the front */
		    q->num = q->base + q->num - base - num;
		    q->base = base + num;
		}
	    }
	    else if (q->base+q->num <= base+num) {
		/* Cut off bit from the end */
		q->num = base - q->base;
	    }
	    else {
		/* Split the block into two pieces */
		p = kmalloc(sizeof(resource_entry_t), GFP_KERNEL);
		p->base = base+num;
		
		p->num = q->base+q->num - p->base;
		q->num = base - q->base;
		p->next = q->next ; q->next = p;
	    }
	}
    }
    return 0;
} /* sub_interval */

/*======================================================================

    These routines examine a region of IO or memory addresses to
    determine what ranges might be genuinely available.
    
======================================================================*/

static void do_io_probe(ioaddr_t base, ioaddr_t num)
{
    
    ioaddr_t i, j, bad, any;
    u_char *b, hole;

    /* First, what does a floating port look like? */
    b = kmalloc(256, GFP_KERNEL);
    memset(b, 0, 256);
    for (i = 0x100; i < 0x400; i += 8) {
	if (check_region(i, 8) != 0)
	    continue;
	b[inb_p(i)]++;
    }
    hole = 0;
    for (i = 0; i < 256; i++)
	if (b[i] > b[hole]) hole = i;
    kfree(b);

    printk(KERN_INFO "cs: IO port probe 0x%04x-0x%04x:",
	   base, base+num-1);
    bad = any = 0;
    for (i = base; i < base+num; i += 8) {
	if (check_region(i, 8) != 0)
	    continue;
	for (j = 0; j < 8; j++)
	    if (inb_p(i+j) != hole) break;
	if (j < 8) {
	    if (!any)
		printk(" excluding");
	    if (!bad)
		bad = any = i;
	}
	else {
	    if (bad) {
		sub_interval(&io_db, bad, i-bad);
		printk(" %#04x-%#04x", bad, i-1);
		bad = 0;
	    }
	}
    }
    if (bad) {
	if ((num > 16) && (bad == base) && (i == base+num))
	    printk(" nothing: probe failed.\n");
	else {
	    sub_interval(&io_db, bad, i-bad);
	    printk(" %#04x-%#04x", bad, i-1);
	}
    }
    
    printk(any ? "\n" : " clean.\n");
}

static void do_mem_probe(u_long base, u_long num)
{
    u_long i, j, bad, any;
    u_short match;

    printk(KERN_DEBUG "PCMCIA memory window probe:");
    bad = any = 0;
    for (i = base; i < base+num; i += 8192) {
	if (check_mem_region(i, 8192) == 0) {
	    match = *(u_short *)i;
	    for (j = 0; j < 8192; j += 82)
		if (match != *(u_short *)(i+j))
		    break;
	}
	else j = 8192;
	if (j < 8192) {
	    if (!any)
		printk(" excluding");
	    if (!bad)
		bad = any = i;
	}
	else {
	    if (bad) {
		sub_interval(&mem_db, bad, i-bad);
		printk(" %#05lx-%#05lx", bad, i-1);
	    }
	    bad = 0;
	}
    }
    if (bad) {
	sub_interval(&mem_db, bad, i-bad);
	printk(" %#05lx-%#05lx", bad, i-1);
    }
    printk(any ? "\n" : " clean.\n");
}

/*======================================================================

    These find ranges of I/O ports or memory addresses that are not
    currently allocated by other devices.
    
======================================================================*/

int find_io_region(ioaddr_t *base, ioaddr_t num, char *name)
{
    u_long align;
    resource_entry_t *m;
    
    if (*base != 0) {
	for (m = io_db.next; m != &io_db; m = m->next) {
	    if ((*base >= m->base) && (*base+num <= m->base+m->num)) {
		if (check_region(*base, num))
		    return -1;
		else {
		    request_region(*base, num, name);
		    return 0;
		}
	    }
	}
	return -1;
    }
    
    for (align = 1; align < num; align *= 2) ;
    for (m = io_db.next; m != &io_db; m = m->next) {
	for (*base = (m->base + align - 1) & (~(align-1));
	     *base+align <= m->base + m->num;
	     *base += align)
	    if (check_region(*base, num) == 0) {
		request_region(*base, num, name);
		return 0;
	    }
    }
    return -1;
} /* find_io_region */

int find_mem_region(u_long *base, u_long num, char *name)
{
    u_long align;
    resource_entry_t *m;
    
    if (*base != 0) {
	for (m = mem_db.next; m != &mem_db; m = m->next) {
	    if ((*base >= m->base) && (*base+num <= m->base+m->num))
		return register_mem_region(*base, num, name);
	}
	return -1;
    }
    
    for (align = 4096; align < num; align *= 2) ;
    for (m = mem_db.next; m != &mem_db; m = m->next) {
	for (*base = (m->base + align - 1) & (~(align-1));
	     *base+align <= m->base+m->num;
	     *base += align)
	    if (register_mem_region(*base, num, name) == 0)
		return 0;
    }
    return -1;
} /* find_mem_region */

/*======================================================================

    This checks to see if an interrupt is available, with support
    for interrupt sharing.  We don't support reserving interrupts
    yet.  If the interrupt is available, we allocate it.
    
======================================================================*/

int try_irq(irq_req_t *req, int irq, int specific)
{
    irq_info_t *info = &irq_table[irq];
    if (info->Attributes & RES_ALLOCATED) {
	switch (req->Attributes & IRQ_TYPE) {
	case IRQ_TYPE_EXCLUSIVE:
	    return CS_IN_USE;
	case IRQ_TYPE_TIME:
	    if ((info->Attributes & RES_IRQ_TYPE)
		!= RES_IRQ_TYPE_TIME)
		return CS_IN_USE;
	    if (req->Attributes & IRQ_FIRST_SHARED)
		return CS_BAD_ATTRIBUTE;
	    info->Attributes |= RES_IRQ_TYPE_TIME | RES_ALLOCATED;
	    info->time_share++;
	    break;
	case IRQ_TYPE_DYNAMIC_SHARING:
	    if ((info->Attributes & RES_IRQ_TYPE)
		!= RES_IRQ_TYPE_DYNAMIC)
		return CS_IN_USE;
	    if (req->Attributes & IRQ_FIRST_SHARED)
		return CS_BAD_ATTRIBUTE;
	    info->Attributes |= RES_IRQ_TYPE_DYNAMIC | RES_ALLOCATED;
	    info->dyn_share++;
	    break;
	}
    }
    else {
	if ((info->Attributes & RES_RESERVED) && !specific)
	    return CS_IN_USE;
	if (REQUEST_IRQ(irq, NULL, 0, "bogus", NULL) == -EBUSY)
	    return CS_IN_USE;
	switch (req->Attributes & IRQ_TYPE) {
	case IRQ_TYPE_EXCLUSIVE:
	    info->Attributes |= RES_ALLOCATED;
	    break;
	case IRQ_TYPE_TIME:
	    if (!(req->Attributes & IRQ_FIRST_SHARED))
		return CS_BAD_ATTRIBUTE;
	    info->Attributes |= RES_IRQ_TYPE_TIME | RES_ALLOCATED;
	    info->time_share = 1;
	    break;
	case IRQ_TYPE_DYNAMIC_SHARING:
	    if (!(req->Attributes & IRQ_FIRST_SHARED))
		return CS_BAD_ATTRIBUTE;
	    info->Attributes |= RES_IRQ_TYPE_DYNAMIC | RES_ALLOCATED;
	    info->dyn_share = 1;
	    break;
	}
    }
    return 0;
} /* try_irq */

/*====================================================================*/

void undo_irq(irq_req_t *req)
{
    irq_info_t *info;

    info = &irq_table[req->AssignedIRQ];
    switch (req->Attributes & IRQ_TYPE) {
    case IRQ_TYPE_EXCLUSIVE:
	info->Attributes &= RES_RESERVED;
	break;
    case IRQ_TYPE_TIME:
	info->time_share--;
	if (info->time_share == 0)
	    info->Attributes &= RES_RESERVED;
	break;
    case IRQ_TYPE_DYNAMIC_SHARING:
	info->dyn_share--;
	if (info->dyn_share == 0)
	    info->Attributes &= RES_RESERVED;
	break;
    }
}
    
/*======================================================================

    The various adjust_* calls form the external interface to the
    resource database.
    
======================================================================*/

static int adjust_memory(adjust_t *adj)
{
    u_long base, num;
    int i;

    base = (u_long)adj->resource.memory.Base;
    num = adj->resource.memory.Size;
    if ((num == 0) || (base+num-1 < base))
	return CS_BAD_SIZE;

    switch (adj->Action) {
    case ADD_MANAGED_RESOURCE:
	if (add_interval(&mem_db, base, num) != 0)
	    return CS_IN_USE;
	if (probe_mem)
	    do_mem_probe(base, num);
	break;
    case REMOVE_MANAGED_RESOURCE:
	sub_interval(&mem_db, base, num);
	for (i = 0; i < sockets; i++) {
	    socket_info_t *s = socket_table[i];
	    release_mem_region(s->cis_mem.sys_start, 0x1000);
	    s->cis_mem.sys_start = 0;
	}
	break;
    default:
	return CS_UNSUPPORTED_FUNCTION;
	break;
    }
    
    return CS_SUCCESS;
} /* adjust_mem */

/*====================================================================*/

static int adjust_io(adjust_t *adj)
{
    int base, num;
    
    base = adj->resource.io.BasePort;
    num = adj->resource.io.NumPorts;
    if ((base < 0) || (base > 0xffff))
	return CS_BAD_BASE;
    if ((num <= 0) || (base+num > 0x10000) || (base+num <= base))
	return CS_BAD_SIZE;

    switch (adj->Action) {
    case ADD_MANAGED_RESOURCE:
	if (add_interval(&io_db, base, num) != 0)
	    return CS_IN_USE;
	if (probe_io)
	    do_io_probe(base, num);
	break;
    case REMOVE_MANAGED_RESOURCE:
	sub_interval(&io_db, base, num);
	break;
    default:
	return CS_UNSUPPORTED_FUNCTION;
	break;
    }

    return CS_SUCCESS;
} /* adjust_io */

/*====================================================================*/

static int adjust_irq(adjust_t *adj)
{
    int irq;
    irq_info_t *info;
    
    irq = adj->resource.irq.IRQ;
    if ((irq < 0) || (irq > 15))
	return CS_BAD_IRQ;
    info = &irq_table[irq];
    
    switch (adj->Action) {
    case ADD_MANAGED_RESOURCE:
	if (info->Attributes & RES_REMOVED)
	    info->Attributes &= ~(RES_REMOVED|RES_ALLOCATED);
	else
	    if (adj->Attributes & RES_ALLOCATED)
		return CS_IN_USE;
	if (adj->Attributes & RES_RESERVED)
	    info->Attributes |= RES_RESERVED;
	else
	    info->Attributes &= ~RES_RESERVED;
	break;
    case REMOVE_MANAGED_RESOURCE:
	if (info->Attributes & RES_REMOVED)
	    return 0;
	if (info->Attributes & RES_ALLOCATED)
	    return CS_IN_USE;
	info->Attributes |= RES_ALLOCATED|RES_REMOVED;
	info->Attributes &= ~RES_RESERVED;
	break;
    default:
	return CS_UNSUPPORTED_FUNCTION;
	break;
    }
    
    return 0;
} /* adjust_irq */

/*====================================================================*/

int adjust_resource_info(client_handle_t handle, adjust_t *adj)
{
    if (CHECK_HANDLE(handle))
	return CS_BAD_HANDLE;
    
    switch (adj->Resource) {
    case RES_MEMORY_RANGE:
	return adjust_memory(adj);
	break;
    case RES_IO_RANGE:
	return adjust_io(adj);
	break;
    case RES_IRQ:
	return adjust_irq(adj);
	break;
    }
    return CS_UNSUPPORTED_FUNCTION;
} /* adjust_resource_info */

/*====================================================================*/

void release_resource_db(void)
{
    resource_entry_t *p, *q;
    
    for (p = mem_db.next; p != &mem_db; p = q) {
	q = p->next;
	kfree_s(p, sizeof(resource_entry_t));
    }
    for (p = io_db.next; p != &io_db; p = q) {
	q = p->next;
	kfree_s(p, sizeof(resource_entry_t));
    }
}
