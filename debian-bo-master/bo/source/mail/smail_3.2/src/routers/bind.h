/* @(#) bind.h,v 1.9 1992/07/11 11:50:21 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * bind.h:
 *	interface file for bind driver.
 */

#include "../bindlib.h"

/* private information stored per router file entry */
struct bind_private {
    struct bindlib_private bindlib_attr;
};
