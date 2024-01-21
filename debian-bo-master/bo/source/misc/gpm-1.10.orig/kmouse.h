/*
 * kmouse.h -- common information to kmouse and gpm configuring it
 *
 * Copyright (C) 1996 Alessandro Rubini (rubini@ipvvis.unipv.it)
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ********/

#define KMOUSE_VERSION   "0.41A"
#define KMOUSE_VERSION_N    41   /* decimal: MMMmmm */

#define KMOUSE_V_MAJOR(n) ((n)/1000)
#define KMOUSE_V_MINOR(n) ((n)%1000)

#ifdef __KERNEL__

#undef PDEBUG
#undef PDEBUGG

#ifdef DEBUG_KMOUSE
#  define PDEBUG(fmt,args...) printk (KERN_DEBUG "kmouse: " fmt , ## args)
#  define Static
#else
#  define PDEBUG(fmt,args...)
#endif
#define PDEBUGG(fmt,args...)


/* This is used by kmouse to grab data from the mouse drivers */

struct mouse_steal {
       void (*steal_char)(kdev_t dev, unsigned char datum);
       void (*steal_packet)(kdev_t dev, short buttons, short dx, short dy);
};

extern struct mouse_steal *mouse_stealing;

typedef struct kmouse_protocol {
	short protoid;
	short howmany;
	unsigned char proto[4];
	int (*fun)(Gpm_Event *state, unsigned char *data);
	unsigned char extra[2];
	short busmouse;
} kmouse_protocol;

#endif /* __KERNEL__ */


enum kmouse_proto {
    PROTO_UNKNOWN = 0,
    PROTO_BARE    = 1,
    PROTO_MSC     = 2,
    PROTO_SUN     = 3, 
    PROTO_PS2     = 4,
    PROTO_MS      = 5,     /* with three button extension */
    PROTO_LOGI    = 6,
    PROTO_MM      = 6,      /* the same as logi */
    PROTO_MMAN    = 7,
    PROTO_NCR     = 8,
    PROTO_WACOM   = 9
};

struct kmouse_options {
    dev_t dev;
    char sequence[8];
    unsigned short protoid, three;
    unsigned short delta, accel;
    unsigned short scale, scaley;
    unsigned short time, tap;
    unsigned short ptrdrag;
};

/* ioctl magic is 'k', no _IOR() is used -- my fault. 1.0 will change it */
    
#define KMOUSE_GETVER 0x6bc0
#define KMOUSE_SETOPT 0x6bc1


