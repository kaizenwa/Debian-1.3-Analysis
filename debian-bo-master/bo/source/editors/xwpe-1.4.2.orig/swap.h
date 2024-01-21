/* swap.h
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

/*
	  Header - File  fuer  Editor  mit  Swap-Routinen      */


#define SWPBLCK 150


typedef struct SFLMG { FILE *fp, *fp2; unsigned int max, anz;
		 unsigned long *pos; unsigned char *bl, *zl; } SFM;

typedef struct STR{ unsigned char far *s; int len, nrc;
					 unsigned int sp; } STRING;

typedef struct BFF { STRING *bf; PUNKT b; PUNKT mx;
		    unsigned int pos; 
	      int mxlines, cl, clsv;
#ifdef WEUNDO
	      struct undo *ud, *rd;
#endif
	      struct CNT *cn; SFM *sp; struct FNST *f; FARBE *fb; } BUFFER;

void *e_malloc(unsigned len);
void *e_realloc(void *p, unsigned len);
void e_swap_init(struct CNT *cn);
int e_swp_out_line(int n, int len, STRING *str, SFM *sp);
int e_swp_out_l2(int n, int len, STRING *str, SFM *sp);
int e_swap_out();
int e_swap_out2();
int e_swap_in_b(BUFFER *b);
int e_swap_out_b(SFM *sp);
void e_swap_in(unsigned n, BUFFER *b);
int e_swp_in_line(long i, BUFFER *b);
int e_swp_in_l2(long i, BUFFER *b);
int e_swp_bl_ad(long pos, int n);

extern int (*e_swap_line_out)(int n, int len, STRING *str, SFM *sp);
extern int (*e_swap_line_in)(long i, BUFFER *b);
extern int swp_b_begin;

#define REALLOC(p, n) e_realloc((p), (n))
#define MALLOC(n) e_malloc(n)
#define FARMALLOC(n) e_malloc(n)
#ifdef DOS
#define FARFREE(n) farfree(n)
#define FREE(n) free(n)
#else
#define FARFREE(n) e_free(n)
#define FREE(n) e_free(n)
#endif
