/*
** config.h                 Option control file for the NYS project
**
** Copyright (c) 1993 Signum Support AB
**
** This file is part of the NYS Library.
**
** The NYS Library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS Library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with the NYS Library; see the file COPYING.LIB.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
*/

#ifndef __NYS_CONFIG_H__
#define __NYS_CONFIG_H__


/*
** Define this when/if you want to produce a final
** version of these libraries (for inclusion into the
** normal libc). You must also rename the old functions
** in libc to "_" + the old name (Ie, "getpwnam" -> "_getpwnam")
** if you do this.
*/
#define FINALOUT 1


/*
** Build options, use these to select what you want to include
** in the Name Service Switch handler.
*/
#define ENABLE_YP 	1		/* NSSwitch YP (NIS v2) support   */
#define ENABLE_COMPAT 	1		/* NSSwitch COMPAT support (+/- entries) */
#define ENABLE_NIS	1		/* NSSwitch NIS+ (NIS v3) support */
#define ENABLE_DNS      1		/* NSSwitch DNS/Hesiod support    */
#if 0
#define ENABLE_DBM      1		/* NSSwitch DBM support           */
#else
#undef ENABLE_DBM
#endif

/*
** Low level support options, use these to enable emulation code
** if you don't have local support for them.
*/
#define ENABLE_YPEMU	1		/* YP client side emulation   */
#define ENABLE_NISEMU	1		/* NIS+ client side emulation */


/*
** Define this if you are using an 'yp.x' with buggy ypresp_key_val
** definition to generate the yp_clnt.c and yp_xdr.c files (like the
** one included in SUNRPC 4.0). See yp/src/yp_if.c for more information.
** The one included here has been fixed.
*/
/*#define HAVE_BUGGY_YP_X 1*/


#endif
