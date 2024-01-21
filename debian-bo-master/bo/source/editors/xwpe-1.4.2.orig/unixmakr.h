/* unixmakr.h						  */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

/*  #define TERMIN  */

#ifdef NOSTRSTR
char *strstr(char *s1, char *s2);
char *getcwd(char *dir, int n);
#endif

#define far


#define e_putp(s) tputs((s), 1, fk_u_putchar)

/*

#ifndef TERMCAP

#define fk_locate(x, y) \
(  cur_x = x, cur_y = y, e_putp(tparm(cur_rc, (y), (x))), fflush(stdout)  )

#else

#define fk_locate(x, y) \
(  cur_x = x, cur_y = y, e_putp(tgoto(cur_rc, (x), (y))), fflush(stdout)  )

#endif

#else

#define fk_locate(x, y) (  cur_x = x, cur_y = y )

#define fk_cursor(x) ( cur_on = (x) )

*/

extern cur_on;

#ifdef DEFPGC
#define getc(fp) fgetc((fp))
#endif
#ifndef DJGPP
#define fk_getch() fgetc(stdin)
#else
#define fk_getch() getkey()
#endif
#define exit(n) e_exit((n))

#ifndef DJGPP
extern char *cur_rc, *cur_vs, *cur_nvs, *cur_vvs, *schirm;
extern char *att_no, *att_so, *att_ul, *att_rv, *att_bl, *att_dm, *att_bo;
#else
extern unsigned char *schirm;
#endif
extern int cur_x, cur_y;
extern char *user_shell;

#ifdef NEWSTYLE
#define e_pr_char(x, y, c, frb)   \
(  *(schirm + 2*MAXSCOL*(y) + 2*(x)) = (c),  \
   *(schirm + 2*MAXSCOL*(y) + 2*(x) + 1) = (frb), \
   *(extbyte + MAXSCOL*(y) + (x)) = 0  )
#else
#define e_pr_char(x, y, c, frb)   \
(  *(schirm + 2*MAXSCOL*(y) + 2*(x)) = (c),  \
   *(schirm + 2*MAXSCOL*(y) + 2*(x) + 1) = (frb)  )
#define e_pt_col(x, y, c)  ( *(schirm + 2*MAXSCOL*(y) + 2*(x) + 1) = (c) )
#endif
#define e_gt_char(x, y)  (*(schirm + 2*MAXSCOL*(y) + 2*(x)))

#define e_gt_col(x, y)  (*(schirm + 2*MAXSCOL*(y) + 2*(x)+1))

#define e_gt_byte(x, y)  (*(schirm + 2*MAXSCOL*(y) + (x)))

#define e_pt_byte(x, y, c)  ( *(schirm + 2*MAXSCOL*(y) + (x)) = (c) )
#ifndef DJGPP
#define e_mouse_leiste(x, y, n, sw, frb)		\
	if(e_we_sw & 1) e_mouse_Leiste(x, y, n, sw, frb)
#define e_lst_zeichen(x, y, n, sw, frb, max, iold, new) \
(  (e_we_sw & 1) ? e_Lst_zeichen(x, y, n, sw, frb, max, iold, new) : 0  )
#else
#define e_mouse_leiste(x, y, n, sw, frb)		\
	e_mouse_Leiste(x, y, n, sw, frb)
#define e_lst_zeichen(x, y, n, sw, frb, max, iold, new) \
        e_Lst_zeichen(x, y, n, sw, frb, max, iold, new) 
#endif

/*  Dummy Definitionen   */

#define e_pr_mtul(x, y, z)
#define e_pr_ul(x)

/*  Zeiger auf Funktionen zu Funktionsaufrufe  */

#define fk_locate(x, y) (*fk_u_locate)(x, y)
#define fk_cursor(x) (*fk_u_cursor)(x)
#ifndef DJGPP
#define e_refresh() (*e_u_refresh)()
#else
#define e_refresh()
#define e_abs_refr()
#endif
#define e_initscr(argc, argv) (*e_u_initscr)(argc, argv)
#define e_getch() (*e_u_getch)()
#define fk_putchar(c) (*fk_u_putchar)(c)
#define bioskey(sw) (*u_bioskey)(sw)
#define e_sys_ini() (*e_u_sys_ini)()
#define e_sys_end() (*e_u_sys_end)()
#define e_frb_menue(sw, xa, ya, f, md) (*e_frb_u_menue)(sw, xa, ya, f, md)
#define e_pr_col_kasten(xa, ya, x, y, f, sw) \
		(*e_pr_u_col_kasten)(xa, ya, x, y, f, sw)
#define e_s_clr(f, b) (*e_s_u_clr)(f, b)
#define e_n_clr(fb) (*e_n_u_clr)(fb)

#ifdef DJGPP
#define getcwd(s, n)  e_getcwd(s, n)
#define chdir(s)  e_chdir(s)
#endif

#ifdef XWINDOW   /*   XWindow Definitionen   */
/*
#define e_sys_ini()

#define e_sys_end()

#define fk_putchar(c)  fputc(c, stdout)

#define bioskey(sw) e_mouse.k
*/
#endif

#ifndef SWAP
#define REALLOC(p, n) realloc((p), (n))
#define MALLOC(n) malloc(n)
#define FARMALLOC(n) malloc(n)
#ifdef FREETEST
void test_free(void *p);
#define FREE(n) test_free(n)
#define FARFREE(n) test_free(n)
#else
#define FREE(n) free(n)
#define FARFREE(n) free(n)
#endif
#endif

#ifdef NEWSTYLE
extern char *extbyte, *altextbyte;
#endif

#define sc_txt_1(f) { if(f->c_sw) f->c_sw = e_sc_txt(f->c_sw, f->b); }

#ifndef FREETEST
#define sc_txt_2(f) 							\
{   if(f->c_sw) 							\
    {	if(f->s->ka.y == f->s->ke.y) e_sc_nw_txt(f->s->ke.y, f->b, 0);	\
	else								\
	{  f->c_sw = REALLOC(f->c_sw, f->b->mx.y * sizeof(int));	\
	   f->c_sw = e_sc_txt(f->c_sw, f->b);				\
	}								\
    }									\
}
#endif
#define sc_txt_3(y, b, sw) {  if(b->f->c_sw) e_sc_nw_txt(y, b, sw);  }
#define sc_txt_4(y, b, sw)						\
{  if(b->f->c_sw && !e_undo_sw) e_sc_nw_txt(y, b, sw);  }

#ifdef DJGPP
#define getenv(v) e_getenv(v)
char *e_getenv(char *v);
#endif

