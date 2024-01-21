/* 
**  $Id: ypxfr.h,v 1.2 1996/10/01 20:21:58 kukuk Exp $
*/

#ifndef __YPXFR_H__
#define __YPXFR_H__

#include "system.h"

extern bool_t ypxfr_xdr_ypresp_all(XDR *, ypresp_all *);
extern int ypxfrd_transfer(char *host, char *map, char *domain, char *tmpname);

#endif
