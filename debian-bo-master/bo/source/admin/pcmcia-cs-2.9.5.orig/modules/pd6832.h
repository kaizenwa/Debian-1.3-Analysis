/*
 * pd6832.h $Revision: 1.1 $ $Date: 1996/11/20 07:49:24 $ (David Hinds)
 */

#ifndef _LINUX_PD6832_H
#define _LINUX_PD6832_H

/* Register definitions for Cirrus PD6832 PCI-to-CardBus bridge */

/* Bridge Control Register */
#define  PD6832_BCR_MGMT_IRQ_ENA	0x0800

/* Socket Number Register */
#define PD6832_SOCKET_NUMBER		0x004c	/* 8 bit */

#endif /* _LINUX_PD6832_H */
