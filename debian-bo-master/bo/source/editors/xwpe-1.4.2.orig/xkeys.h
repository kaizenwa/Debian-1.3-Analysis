/* xkeys.h						  */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#define LASTCUR 0
#define EDCUR 48
#define WTCUR 150
#define PTCUR 124
#define ERCUR 88
#define DBCUR 142

#define fk_pt_tmp(x) fk_x_pointer(x)
#define fk_pointer(x) ( e_pt_old = x, fk_x_pointer(x) )
#define fk_pt_bak() fk_x_pointer(e_pt_old)
 
extern int e_pt_old;
