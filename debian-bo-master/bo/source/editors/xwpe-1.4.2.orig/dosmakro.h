/* dosmakro.h
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

extern char far *schirm;

#define e_pr_char(x, y, c, frb)   \
(  *(schirm + 160*(y) + 2*(x)) = (c),  *(schirm + 160*(y) + 2*(x) + 1) = (frb) )

#define e_gt_char(x, y)  (*(schirm + 160*(y) + 2*(x)))

#define e_gt_col(x, y)  (*(schirm + 160*(y) + 2*(x)+1))

#define e_gt_byte(x, y)  (*(schirm + 160*(y) + (x)))

#define e_pt_byte(x, y, c)  ( *(schirm + 160*(y) + (x)) = (c) )

#define fk_getch()  getch()

#define e_s_clr(x, y) e_s_x_clr((x), (y))

#define e_n_clr(x) e_n_x_clr(x)

#define e_frb_menue(x, y, z, f) e_frb_x_menue(x, y, z, f)
#define e_pr_col_kasten(xa, ya, x, y, f, sw) \
	e_pr_x_col_kasten(xa, ya, x, y, f, sw)

#define e_mouse_leiste(x, y, n, sw, frb)		\
	e_mouse_Leiste(x, y, n, sw, frb)
#define e_lst_zeichen(x, y, n, sw, frb, max, iold, new) \
	e_Lst_zeichen(x, y, n, sw, frb, max, iold, new)
#define e_refresh()
#define e_abs_refr()

#ifndef SWAP
#define REALLOC(p, n) realloc((p), (n))
#define MALLOC(n) malloc(n)
#define FARMALLOC(n) farmalloc(n)
#define FREE(n) free(n)
#define FARFREE(n) farfree(n)
#endif

