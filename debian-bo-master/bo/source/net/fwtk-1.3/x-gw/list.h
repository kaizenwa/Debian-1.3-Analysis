/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

 /*
  *      Author: Wei Xu, Trusted Information Systems, Inc.
  *	"Header: ";
  */

#ifndef __LIST__
struct list_t {
	int	 	 id;
	void		*item;
	struct list_t   *next;
};

typedef struct list_t list_t;

#define __LIST__
#endif

