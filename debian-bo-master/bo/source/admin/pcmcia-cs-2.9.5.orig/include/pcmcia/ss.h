/*
 * ss.h v1.6: 1995/04/07 18:19:15 (David Hinds)
 */

#ifndef _LINUX_SS_H
#define _LINUX_SS_H

/* For RegisterCallback */
typedef struct ss_callback_t {
    void	(*handler)(void *info, u_long events);
    void	*info;
} ss_callback_t;

/* Definitions for card status flags for GetStatus */
#define SS_WRPROT	0x01
#define SS_CARDLOCK	0x02
#define SS_EJECTION	0x04
#define SS_INSERTION	0x08
#define SS_BATDEAD	0x10
#define SS_BATWARN	0x20
#define SS_READY	0x40
#define SS_DETECT	0x80
#define SS_POWERON	0x100
#define SS_GPI		0x200
#define SS_STSCHG	0x400

/* for InquireSocket */
typedef struct socket_cap_t {
    u_long	irq_mask;
} socket_cap_t;

/* for GetSocket, SetSocket */
typedef struct socket_state_t {
    u_long	flags;
    u_long	csc_mask;
    u_char	Vcc, Vpp;
    int		io_irq;
} socket_state_t;

/* Various card configuration flags */
#define SS_PWR_AUTO	0x10
#define SS_IOCARD	0x20
#define SS_RESET	0x40
#define SS_DMA_MODE	0x80
#define SS_SPKR_ENA	0x100
#define SS_OUTPUT_ENA	0x200

/* Flags for I/O port and memory windows */
#define MAP_ACTIVE	0x01
#define MAP_16BIT	0x02
#define MAP_AUTOSZ	0x04
#define MAP_0WS		0x08
#define MAP_WRPROT	0x10
#define MAP_ATTRIB	0x20
#define MAP_USE_WAIT	0x40

typedef struct pcmcia_io_map {
    u_char	map;
    u_char	flags;
    u_short	speed;
    u_short	start, stop;
} pcmcia_io_map;

typedef struct pcmcia_mem_map {
    u_char	map;
    u_char	flags;
    u_short	speed;
    u_long	sys_start, sys_stop;
    u_long	card_start;
} pcmcia_mem_map;

enum ss_service {
    SS_RegisterCallback, SS_GetStatus, SS_InquireSocket,
    SS_GetSocket, SS_SetSocket,
    SS_GetIOMap, SS_SetIOMap, SS_GetMemMap, SS_SetMemMap
};

#endif /* _LINUX_SS_H */
