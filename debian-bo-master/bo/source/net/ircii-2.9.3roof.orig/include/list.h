/*
 * list.h: header for list.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: list.h,v 1.10 1995/09/03 13:45:18 mrg Exp $
 */

#ifndef __list_h_
#define __list_h_

	void	add_to_list _((List **, List *));
	List	*find_in_list _((List **, char *, int));
	List	*remove_from_list _((List **, char *));
	List	*list_lookup_ext _((List **, char *, int, int, int (*)(List *, char *)));
	List	*list_lookup _((List **, char *, int, int));
	List	*remove_from_list_ext _((List **, char *, int (*)(List *, char *)));
	void	add_to_list_ext _((List **, List *, int (*)(List *, List *)));
	List	*find_in_list_ext _((List **, char *, int, int (*)(List *, char *)));

#define REMOVE_FROM_LIST 1
#define USE_WILDCARDS 1

#endif /* __list_h_ */
