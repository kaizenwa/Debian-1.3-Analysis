/* $Id: charsubst.h,v 3.0 1993/12/21 21:54:40 davison Trn $
 */
/*
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

#ifdef CHARSUBST

/* Conversions are: plain, ISO->ascii, TeX -> ISO, ISO->ascii monospaced */
EXT char *charsets INIT("patm");
EXT char *charsubst;

int putsubstchar _((int, int, bool_int));
char *current_charsubst _((void));
void strcharsubst _((char*,char*));

#endif
