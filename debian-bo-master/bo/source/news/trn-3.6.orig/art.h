/* $Id: art.h,v 3.0 1991/09/09 20:18:23 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

/* do_article() return values */

#define DA_NORM 0
#define DA_RAISE 1
#define DA_CLEAN 2
#define DA_TOEND 3

EXT ART_LINE highlight INIT(-1);/* next line to be highlighted */
EXT ART_POS artsize;		/* size in bytes of article */

#ifdef INNERSEARCH
EXT ART_POS innersearch INIT(0); /* artpos of end of line we want to visit */
EXT ART_LINE innerlight INIT(0); /* highlight position for innersearch or 0 */
EXT char hide_everything INIT(0);/* if set, do not write page now, */
#endif				 /* ...but execute char when done with page */

void	art_init _((void));
int	do_article _((void));
int	page_switch _((void));
bool	innermore _((void));
