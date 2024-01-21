/***************************************************************************** 
* Copyright (C) 1996 by Sun Microsystems Computer Co.
* 
* 
* Permission to use, copy, modify, and distribute this software and its
* documentation for any purpose and without fee is hereby granted, provided
* that the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation.  This software is provided "as is" without express or
* implied warranty.
*
******************************************************************************/

#include <sys/types.h>
#include <sys/conf.h>
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/stream.h>
#include <sys/stropts.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <sys/modctl.h>
#include <sys/cmn_err.h>
#include <sys/debug.h>
#include <sys/kmem.h>

#include <inet/common.h>
#include "ddp.h"

#include <sys/ddi.h>		/* ddi's should be last includes */
#include <sys/sunddi.h>

/* Local Prototypes */
static void ddp_rput(queue_t *q, mblk_t *mp_orig);
static int  ddp_open(queue_t *q, dev_t *devp,int flag,int sflag,cred_t *credp);
static int  ddp_close(queue_t *q);
static void ddp_wput(queue_t *q, mblk_t *mp);
static int  ddp_attach(dev_info_t *devi, ddi_attach_cmd_t cmd);
static int  ddp_identify(dev_info_t *devi);
static int  ddp_detach(dev_info_t *devi, ddi_detach_cmd_t cmd);
static int  ddp_getinfo(dev_info_t *, ddi_info_cmd_t, void *, void **);
static int  ddp_ioctl(dev_t, int, int, int, cred_t *, int *);
static char *GetStreamTypeDescription(int Type);
static int asDriver(queue_t *q);
static int asModule(queue_t *q);

/* Public Prototypes */
int  dlpi_open(queue_t *q, dev_t *devp, int flag, int sflag, cred_t *credp);
int  dlpi_close(queue_t *q);
void dlpi_wput(queue_t *q, mblk_t *mp);
void dlpi_rput(queue_t *q, mblk_t *mp);

int  tpi_open(queue_t *q, dev_t *devp, int flag, int sflag, cred_t *credp);
int  tpi_close(queue_t *q);
void tpi_wput(queue_t *q, mblk_t *mp);
void tpi_rput(queue_t *q, mblk_t *mp);


/* STREAMS data */
static struct module_info info = { 
  0,
  "ddp",
  0,
  INFPSZ,
  512,
  128
};

static struct qinit rinit = {
  (pfi_t) ddp_rput,
  nil(pfi_t),
  (pfi_t) ddp_open,
  (pfi_t) ddp_close,
  nil(pfi_t),
  &info
};

static struct qinit winit = {
  (pfi_t) ddp_wput,
  nil(pfi_t),
  (pfi_t) ddp_open,
  (pfi_t) ddp_close,
  nil(pfi_t),
  &info
};

struct streamtab ddp_info = { &rinit, &winit };

/* device driver data */
static struct cb_ops ddp_cbops = {
	nulldev,		/* cb_open */
	nulldev,		/* cb_close */
	nodev,			/* cb_strategy */
	nodev,			/* cb_print */
	nodev,			/* cb_dump */
	nodev,			/* cb_read */
	nodev,			/* cb_write */
	nodev,			/* cb_ioctl */
	nodev,			/* cb_devmap */
	nodev,			/* cb_mmap */
	nodev,			/* cb_segmap */
	nochpoll,		/* cb_chpoll */
	ddi_prop_op,		/* cb_prop_op */
	&ddp_info,		/* cb_stream */
	(D_MP|D_MTPERQ)	/* cb_flag */

};

static struct dev_ops ddp_devops = {
	DEVO_REV,			/* devo_rev */
	0,				/* devo_refcnt */
	ddp_getinfo,		/* devo_getinfo */
	ddp_identify,		/* devo_identify */
	nulldev,			/* devo_probe */
	ddp_attach,		/* devo_attach */
	ddp_detach,		/* devo_detach */
	nodev,				/* devo_reset */
	&ddp_cbops,		/* devo_cb_ops */
	(struct bus_ops *)NULL		/* devo_bus_ops */
};

static struct modldrv modldrv = {
	&mod_driverops,		/* define loadable driver */
	"ddp Streams device",	/* display text for modinfo() */
	&ddp_devops,		/* pointer to driver ops */
};

static struct fmodsw ddp_module_fmodsw = {
	"ddp",			/* module name */
	&ddp_info,		/* module information */
	(D_MP|D_MTPERQ) /* module flags */
};

static struct modlstrmod modlstrmod = {
	&mod_strmodops,		/* define loadable streams module */
	"ddp Streams module",	/* display text for modinfo() */
	&ddp_module_fmodsw	/* template of class entry for this module */
};

static struct modlinkage modlinkage = {
	MODREV_1,		/* revision number */
	&modlstrmod,		/* streams module */
	&modldrv,		/* streams device */
	NULL
};


static void *statep=NULL;
static dev_info_t *TheDip=NULL;

/* GLOBAL VARIABLES */
int ddp_debug = 0;		/* 0==no debug msgs  !0==full debug msgs */
struct atalk_socket *atalk_socket_list=NULL;
atalk_iface *atalk_iface_list=NULL;
struct atalk_route *atalk_router_list=NULL;
struct atalk_route defaultRoute;
krwlock_t AarpGLock;
kmutex_t AtalkSocketMutex;
kmutex_t AtalkIfaceMutex;
kmutex_t AtalkRouteMutex;
char *ddp_mindevs;
int ddp_ndevs = 64;			/* arbitrary, "ndevs" in ddp.conf */
int ddp_nlocalrts = 256;

/*
 * Dynamic loading routines
 */

int _init(void)
{
  int sts;
  sts = mod_install(&modlinkage);
  if (sts != 0)
    cmn_err(CE_WARN,"Ddp: _init failed: mod_install() returned %d",sts);

  ddi_soft_state_init(&statep, 100, 1);	/* MAD: ??? */

  mutex_init(&AtalkSocketMutex, "atalk-socket", MUTEX_DRIVER, NULL);
  mutex_init(&AtalkIfaceMutex, "atalk-socket", MUTEX_DRIVER, NULL);
  mutex_init(&AtalkRouteMutex, "atalk-socket", MUTEX_DRIVER, NULL);

  /* initialize aarp */
  aarp_startup();
  dlpi_startup();		/* set up loopback interface and stuff */

  return sts;
}

int _fini(void)
{
  int sts;
  sts = mod_remove(&modlinkage);
  if (sts != 0) {
    return sts;
  }

  ddi_soft_state_fini(&statep);
  mutex_destroy(&AtalkSocketMutex);
  mutex_destroy(&AtalkIfaceMutex);
  mutex_destroy(&AtalkRouteMutex);

  /* clean up aarp */
  aarp_cleanup();

  return 0;
}

int _info(struct modinfo *modinfop)
{
    return (mod_info(&modlinkage, modinfop));
}


/*
 * Functions specific for driver
 */

static int ddp_attach(dev_info_t *dip, ddi_attach_cmd_t cmd)
{
  int sts, instance;
  
  dcmn_err(( CE_NOTE, "ddp_attach()" ));

  if (cmd != DDI_ATTACH)
    return DDI_FAILURE;

  instance = ddi_get_instance(dip);
  TheDip = dip;	/* keep a pointer to this dip */

  /* create this interface for normal 'socket' accesses */
  sts = ddi_create_minor_node(dip, "ddp", S_IFCHR, 0, NULL, CLONE_DEV);
  if (sts == DDI_FAILURE) {
    cmn_err(CE_WARN, "ddp_attach failed to create minor node");
    return DDI_FAILURE;
  }

  ddp_nlocalrts = ddi_getprop(DDI_DEV_T_ANY, dip, DDI_PROP_DONTPASS,
			  "nlocalrts", ddp_nlocalrts);
  if ( ddp_nlocalrts < 256 ) {
    cmn_err(CE_WARN, "ddp_attach: can't set nlocalrts to %d", ddp_nlocalrts );
    ddp_nlocalrts = 256;
  }

  ddp_ndevs = ddi_getprop(DDI_DEV_T_ANY, dip, DDI_PROP_DONTPASS,
			  "ndevs", ddp_ndevs);

  if ((ddp_mindevs = (char *) kmem_zalloc(ddp_ndevs, KM_SLEEP)) == 0) {
      ddi_remove_minor_node(dip, NULL);
      cmn_err(CE_WARN, "ddp_attach - kmem_zalloc failure\n");
      return DDI_FAILURE;
  }

  ddi_report_dev(dip);

  return sts;
}

static int ddp_detach (dev_info_t *devi, ddi_detach_cmd_t cmd)
{
  dcmn_err((CE_CONT, "ddp_detach()\n"));

  if (cmd != DDI_DETACH)
    return (DDI_FAILURE);

  ddi_remove_minor_node(devi, NULL);

  kmem_free(ddp_mindevs, ddp_ndevs);

  return DDI_SUCCESS;
}

static int
ddp_getinfo(dev_info_t *dip, ddi_info_cmd_t cmd, void *arg, void **result)
{
  long *s;
  dcmn_err((CE_CONT, "ddp_getinfo()\n"));

  switch (cmd) {
  case DDI_INFO_DEVT2DEVINFO:
    dcmn_err((CE_CONT, "DEV => DEVINFO\n"));
    *result = TheDip;
    if (TheDip == NULL)
      return DDI_FAILURE;
    break;

  case DDI_INFO_DEVT2INSTANCE:
    dcmn_err((CE_CONT, "DEV => INSTANCE\n"));
    *result = (void *)getminor((dev_t)arg);
    break;

  default:
    return DDI_FAILURE;
  }
  return DDI_SUCCESS;
}

static int ddp_identify(dev_info_t *devi)
{
    dcmn_err((CE_CONT, "ddp_identify()\n"));

    if (strcmp(ddi_get_name(devi), "ddp") == 0)
	return DDI_IDENTIFIED;

    if (strcmp(ddi_get_name(devi), "ddp-loopback") == 0)
	return DDI_IDENTIFIED;

    return DDI_NOT_IDENTIFIED;
}

static int ddp_open(queue_t *q, dev_t *devp,int flag,int sflag,cred_t *credp)
{
  int ret;
  dcmn_err((CE_CONT, "DDP: ddp_open()\n"));

  if (sflag == MODOPEN || sflag != CLONEOPEN)	/* Module open OR loopback */
    ret = dlpi_open(q, devp, flag, sflag, credp);
  else				/* Driver open normally called by socket() */
    ret = tpi_open(q, devp, flag, sflag, credp);
  
  dcmn_err((CE_CONT, "done ddp_open()\n"));
  return ret;
}

static int ddp_close (queue_t *q)
{
  int ret = 0;
  dcmn_err((CE_CONT, "ddp_close(%x)\n", q));

  /* was using the q->next pointer - but it doesn't work ??? */
  if (asModule(q))
    ret = dlpi_close(q);	/* close the module */
  else if (asDriver(q))
    ret = tpi_close(q);		/* close the driver */
  else {
    cmn_err(CE_WARN, "Invalid close q");
    ASSERT(0);
  }
  dcmn_err((CE_CONT, "leaving ddp_close(%x)\n", q));
  return ret;
}

static void ddp_rput (queue_t *q, mblk_t *mp)
{
  if (asModule(q))
    dlpi_rput(q, mp);
  else if (asDriver(q))
    tpi_rput(q, mp);
  else {
    cmn_err(CE_WARN, "ddp_rput: Invalid q <%x>", q);
    ASSERT(0);
  }
  dcmn_err((CE_CONT, "done ddp_rput\n"));
}

static void ddp_wput (queue_t *q, mblk_t *mp)
{
  int mtype = mp->b_datap->db_type;

  dcmn_err((CE_CONT,"ddp_wput(): db_type=%d (%s)\n", mtype,
	     GetStreamTypeDescription(mtype) ));

  if (asModule(WR(q)))
    dlpi_wput(q, mp);
  else if (asDriver(WR(q)))
    tpi_wput(q, mp);		/* the driver must conform to TPI */
  else {
    cmn_err(CE_WARN, "ddp_wput: Invalid q <%x>", q);
    ASSERT(0);
  }
  dcmn_err((CE_CONT, "done ddp_wput\n"));
}


static char *GetStreamTypeDescription (int Type)
{
  typedef struct streams_type_description {
    int		Type;
    char	*Description;
  } STREAMS_TYPE_DESCRIPTION;

  static STREAMS_TYPE_DESCRIPTION StreamDes [] = {
    M_DATA, "regular data",
    M_PROTO, "protocol control",
    M_BREAK, "line break",
    M_PASSFP, "pass file pointer",
    M_EVENT, "post an event to an event queue",
    M_SIG, "generate process signal",
    M_DELAY, "real-time xmit delay (1 param)",
    M_CTL, "device-specific control message",
    M_IOCTL, "ioctl; set/get params",
    M_SETOPTS, "set various stream head options",
    M_RSE, "reserved for RSE use only",
    M_IOCACK, "acknowledge ioctl",
    M_IOCNAK, "negative ioctl acknowledge",
    M_PCPROTO, "priority proto message",
    M_PCSIG, "generate process signal",
    M_READ, "generate read notification",
    M_FLUSH, "flush your queues",
    M_STOP, "stop transmission immediately",
    M_START, "restart transmission after stop",
    M_HANGUP, "line disconnect",
    M_ERROR, "fatal error used to set u.u_error",
    M_COPYIN, "request to copyin data",
    M_COPYOUT, "request to copyout data",
    M_IOCDATA, "response to M_COPYIN and M_COPYOUT",
    M_PCRSE, "reserved for RSE use only",
    M_STOPI, "stop reception immediately",
    M_STARTI, "restart reception after stop",
    M_PCEVENT, "post an event to an event queue",
    M_UNHANGUP, "line reconnect, sigh",
    -1, ""
  };
  STREAMS_TYPE_DESCRIPTION *pStd;

  for (pStd = StreamDes; pStd->Type >= 0 && pStd->Type != Type; pStd++)
    ;

  return pStd->Description;
}

/* If the Q is associated with a STREAMS module, then it represents a control channel
 * created by opening a network device and I_PUSH'ing ddp onto the stream as a module.
 * This is only done by altalkd and only to configure the netatalk ddp system layer.
 * We can detect that Q belongs to a module by a structure in our global interface list
 */
static int asModule(queue_t *q)
{
  atalk_iface *p;
  int sts = 0;

  mutex_enter(&AtalkIfaceMutex);
  for (p = atalk_iface_list; p != NULL; p = p->next) {
    if (p->ReadQ == q || p->WriteQ == q) {
      sts = 1;
      break;
    } 
  } 
  mutex_exit(&AtalkIfaceMutex);
  return sts;
}

/* If the Q is associated with a STREAMS driver, then it represents an I/O socket channel
 * created by applications typically using the socket(AF_APPLETALK, ...) interface.
 * We can detect that Q belongs to a driver by a structure in our global socket list
 */
static int asDriver(queue_t *q)
{
  atalk_socket *p;
  int sts;

  mutex_enter(&AtalkSocketMutex);
  for (p = atalk_socket_list; p != NULL; p = p->next) {
    if (p->rdq == q || p->wrq == q) {
      sts = 1;
      break;
    }
  }
  mutex_exit(&AtalkSocketMutex);
  return sts;
}

