/* @(#) smartuser.h,v 1.4 1995/05/30 16:16:29 nm4 Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * smartuser.h:
 *	interface file for the smartuser driver.
 */

/* boolean attributes for director.flags field */
#define SMARTUSER_WELLFORMED	0x00010000

/* private information from director file entry */
struct smartuser_private {
    char *new_user;			/* template for new address */
    char *transport;			/* transport if routing to "smart" transport */
    struct transport *transport_ptr;	/* transport structure */
};
