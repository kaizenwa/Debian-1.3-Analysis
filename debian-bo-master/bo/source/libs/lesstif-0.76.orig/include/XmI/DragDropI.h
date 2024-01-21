/**
 *
 * $Id: DragDropI.h,v 1.2 1997/01/14 04:16:51 miers Exp $
 * 
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

/***********************************************************/
/* Copyright 1996 Daniel Dardailler.  
Permission to use, copy, modify, distribute, and sell this software
for any purpose is hereby granted without fee, provided that the above
copyright notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting documentation,
and that the name of Daniel Dardailler not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  Daniel Dardailler makes no representations
about the suitability of this software for any purpose.  It is
provided "as is" without express or implied warranty.
************************************************************/

#ifndef DRAGDROP_I_H
#define DRAGDROP_I_H

#include <X11/Xmd.h>

typedef struct _XmDndSourceProp {
  BYTE		byte_order;
  BYTE		protocol_version;
  CARD16	target_index;
  CARD32	selection;
} XmDndSourceProp;

typedef struct _XmDndReceiverProp {
  BYTE		byte_order;
  BYTE		protocol_version;
  BYTE		protocol_style;
  BYTE		pad1;
  CARD32	proxy_window;
  CARD16	num_drop_sites;
  CARD16	pad2;
  CARD32	total_size;
} XmDndReceiverProp;

/*
 * need to use some union hack since window and property are in
 * different order depending on the message ...
 */
typedef struct _XmDndTop {
  CARD32	src_window;
  CARD32	property;
} XmDndTop;

typedef struct _XmDndPot {
  INT16		x;
  INT16		y;
  CARD32	property;
  CARD32	src_window;
} XmDndPot;

typedef struct _XmDndMessage {
  BYTE		reason;
  BYTE		byte_order;
  CARD16	flags;
  CARD32	time;
  union {
    XmDndTop top;
    XmDndPot pot;
  } data;
} DndMessage;

typedef struct _XmDndTargets {
  BYTE		byte_order;
  BYTE		protocol_version;
  CARD16	num_target_lists;
  CARD32	data_size;
    /* then come series of CARD16,CARD32,CARD32,CARD32... */
} XmDndTargets;

/* protocol version */
#define DND_PROTOCOL_VERSION 0

#define DND_EVENT_TYPE_MASK  ((BYTE)0x80)
#define DND_EVENT_TYPE_SHIFT 7
#define DND_CLEAR_EVENT_TYPE  ((BYTE)0x7F)

/*
 * message_type is data[0] of the client_message
 * this return 1 (receiver bit up) or 0 (initiator)
 */
#define DND_GET_EVENT_TYPE(message_type) \
  ((char) (((message_type) & DND_EVENT_TYPE_MASK) >> DND_EVENT_TYPE_SHIFT))

/*
 * event_type can be 0 (initiator) or 1 (receiver)
 */
#define DND_SET_EVENT_TYPE(event_type) \
  (((BYTE)(event_type) << DND_EVENT_TYPE_SHIFT) & DND_EVENT_TYPE_MASK)


#define DND_OPERATION_MASK ((CARD16) 0x000F)
#define DND_OPERATION_SHIFT 0
#define DND_STATUS_MASK ((CARD16) 0x00F0)
#define DND_STATUS_SHIFT 4
#define DND_OPERATIONS_MASK ((CARD16) 0x0F00)
#define DND_OPERATIONS_SHIFT 8
#define DND_COMPLETION_MASK ((CARD16) 0xF000)
#define DND_COMPLETION_SHIFT 12

#define DND_GET_OPERATION(flags) \
  ((unsigned char) \
   (((flags) & DND_OPERATION_MASK) >> DND_OPERATION_SHIFT))

#define DND_SET_OPERATION(operation) \
  (((CARD16)(operation) << DND_OPERATION_SHIFT)\
   & DND_OPERATION_MASK)

#define DND_GET_STATUS(flags) \
  ((unsigned char) \
   (((flags) & DND_STATUS_MASK) >> DND_STATUS_SHIFT))

#define DND_SET_STATUS(status) \
  (((CARD16)(status) << DND_STATUS_SHIFT)\
   & DND_STATUS_MASK)

#define DND_GET_OPERATIONS(flags) \
  ((unsigned char) \
   (((flags) & DND_OPERATIONS_MASK) >> DND_OPERATIONS_SHIFT))

#define DND_SET_OPERATIONS(operation) \
  (((CARD16)(operation) << DND_OPERATIONS_SHIFT)\
   & DND_OPERATIONS_MASK)

#define DND_GET_COMPLETION(flags) \
  ((unsigned char) \
   (((flags) & DND_COMPLETION_MASK) >> DND_COMPLETION_SHIFT))

#define DND_SET_COMPLETION(completion) \
  (((CARD16)(completion) << DND_COMPLETION_SHIFT)\
   & DND_COMPLETION_MASK)


#define SWAP4BYTES(l) {\
	struct { unsigned t :32;} bit32;\
        char n,	*tp = (char *) &bit32;\
	bit32.t = l;\
	n = tp[0]; tp[0] = tp[3]; tp[3] = n;\
	n = tp[1]; tp[1] = tp[2]; tp[2] = n;\
        l = bit32.t;\
}

#define SWAP2BYTES(s) {\
	struct { unsigned t :16; } bit16;\
        char n, *tp = (char *) &bit16;\
	bit16.t = s;\
	n = tp[0]; tp[0] = tp[1]; tp[1] = n;\
        s = bit16.t;\
}

/***** Targets/Index stuff */

typedef struct {
  int	num_targets;
  Atom	*targets;
} XmDndTargetsTableEntryRec, * XmDndTargetsTableEntry;

typedef struct {
  int	num_entries;
  XmDndTargetsTableEntry entries;
} XmDndTargetsTableRec, * XmDndTargetsTable;

extern int _XmDndTargetsToIndex(Display * display, Atom * targets,
				int num_targets);

extern int _XmDndIndexToTargets(Display * display,
				int index,
				Atom ** targets);
#endif /* DRAGDROP_I_H */

