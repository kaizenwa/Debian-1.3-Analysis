/* we_swap.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"

#ifdef SWAP

int e_s_error(char *str, int n, int ex, FARBE *fb);
int (*e_swap_line_out)(int n, int len, STRING *str, SFM *sp);
int (*e_swap_line_in)(long i, BUFFER *b);
int swp_b_begin = 0;
#ifdef UNIX
#define MAXMALLOC 200000
int e_tot_swp = 0;
struct e_swp_mal {  void *pt; unsigned len;  };
struct e_swp_vec {  int n; struct e_swp_mal **v;  }
e_swp_vec = {  0, NULL  };
#define WOTYPE "w+"
#else
#define WOTYPE "w+b"
#endif

/*  Achtung unter DOS nur fuer Modell 'Large'  */

#ifdef DOS

void *e_malloc(unsigned len)
{
   void *pt;
   do
   {  if((pt = malloc(len)) != NULL) return(pt);
      if(e_swap_out()) return(NULL);
   }  while(pt == NULL);
   return(pt);
}

void *e_realloc(void *p, unsigned len)
{
   void *pt = p;
   do
   {  if((pt = realloc(p, len)) != NULL) return(pt);
      if(e_swap_out()) return(NULL);
   }  while(pt == NULL);
   return(pt);
}

#else

void *e_malloc(unsigned len)
{
   void *pt = NULL;
   do
   {  if(e_tot_swp < MAXMALLOC && (pt = malloc(len)) != NULL)
      {  if(e_swp_vec.n == 0) e_swp_vec.v = malloc(sizeof(struct e_swp_mal *));
	 else   e_swp_vec.v = realloc(e_swp_vec.v,
				(e_swp_vec.n + 1) * sizeof(struct e_swp_mal *));
	 e_swp_vec.v[e_swp_vec.n] = malloc( sizeof(struct e_swp_mal));
	 e_swp_vec.v[e_swp_vec.n]->len = len;
	 e_swp_vec.v[e_swp_vec.n]->pt = pt;
	 e_swp_vec.n++;
	 e_tot_swp += len;
	 return(pt);
      }
      if(e_swap_out()) return(NULL);
   }  while(pt == NULL);
   return(pt);
}

void *e_realloc(void *p, unsigned len)
{
   void *pt = p;
   int i;
   do
   {  if((pt = realloc(p, len)) != NULL);
      {  for(i = e_swp_vec.n - 1 ; i <= 0 && e_swp_vec.v[i]->pt != p; i--);
	 if(i < 0)
	 {  extern struct EXT h_error;
	    e_error("error: pointer not found!", 0, h_error.cn->fb);
	    return(pt);
	 }
	 e_tot_swp += (len - e_swp_vec.v[i]->len);
	 e_swp_vec.v[i]->len = len;
	 e_swp_vec.v[i]->pt = pt;
	 return(pt);
      }
      if(e_swap_out()) return(NULL);
   }  while(pt == NULL);
   return(pt);
}

void e_free(void *p)
{
   int i;
   free(p);
   for(i = e_swp_vec.n - 1 ; i <= 0 && e_swp_vec.v[i]->pt != p; i--);
   if(i < 0)
   {  extern struct EXT h_error;
      e_error("error: pointer not found!", 0, h_error.cn->fb);
      return;
   }
   e_tot_swp -= e_swp_vec.v[i]->len;
   free(e_swp_vec.v[i]);
   for(i++ ; i < e_swp_vec.n; i++) e_swp_vec.v[i-1] = e_swp_vec.v[i];
   e_swp_vec.n--;
}
#endif

void e_swap_init(ECNT *cn)
{
   int i;
   if( (cn->sp = malloc(sizeof(SFM))) == NULL)
   e_s_error(e_msg[0], 1, 2, cn->fb);
   cn->sp->fp = NULL;
   cn->sp->fp2 = NULL;
   cn->sp->max = 2 * (SWPBLCK + 1) + MAXEDT;
   cn->sp->anz = MAXEDT + 1;
   if((cn->sp->bl = malloc(cn->sp->max)) == NULL)
   e_s_error(e_msg[0], 2, 2, cn->fb);
   if((cn->sp->zl = malloc(cn->sp->max)) == NULL)
   e_s_error(e_msg[0], 3, 2, cn->fb);
   if((cn->sp->pos = malloc(cn->sp->max * sizeof(unsigned long))) == NULL)
   e_s_error(e_msg[0], 4, 2, cn->fb);
   e_swap_line_out = e_swp_out_line;
   e_swap_line_in = e_swp_in_line;
   for(i = 0; i < cn->sp->max; i++)
   {  cn->sp->bl[i] = 0;
      cn->sp->zl[i] = 0;
   }
}

int e_swp_out_line(int n, int len, STRING *str, SFM *sp)
{
   extern struct EXT h_error;
   ECNT *cn = h_error.cn;
   if(n <= MAXEDT) n = MAXEDT + 1;
   for(; n < sp->anz; n++)
   {  if(sp->bl[n] <= 0 && sp->zl[n] <= len)
      {  if(fseek(sp->fp, sp->pos[n], SEEK_SET))
	 e_s_error(e_msg[17], 5, 1, cn->fb);
	 if(fwrite(str->s, len, 1, sp->fp) != 1)
	 e_s_error(e_msg[17], 6, 1, cn->fb);
	 sp->bl[n] = 1;
	 str->sp = n;
	 FREE(str->s);
	 str->s = NULL;
	 return(n+1);
      }
   }
   if(sp->anz >= sp->max)
   e_s_error(e_msg[18], 7, 1, cn->fb);
   if(fseek(sp->fp, 0, SEEK_END))
   e_s_error(e_msg[17], 8, 1, cn->fb);
   if((sp->pos[sp->anz] = ftell(sp->fp)) < 0)
   e_s_error(e_msg[17], 9, 1, cn->fb);
   if(fwrite(str->s, len, 1, sp->fp) != 1)
   {  e_error(str->s, 0, cn->fb);
      e_s_error(e_msg[17], 10, 1, cn->fb);
   }
   sp->bl[sp->anz] = 1;
   sp->zl[sp->anz] = len;
   str->sp = sp->anz;
   FREE(str->s);
   str->s = NULL;
   (sp->anz)++;
   return(sp->anz+1);
}


int e_swp_out_l2(int n, int len, STRING *str, SFM *sp)
{
   extern struct EXT h_error;
   ECNT *cn = h_error.cn;
   struct pos_strct {  char bl; char zl; unsigned long pos;  }  sp2[1];
   for(; n < sp->anz - MAXEDT - 1; n++)
   {  if(fseek(sp->fp2, n * sizeof(struct pos_strct), SEEK_SET))
      e_s_error(e_msg[4], 11, 1, cn->fb);
      if(fread(sp2, sizeof(struct pos_strct), 1, sp->fp2) != 1)
      e_s_error(e_msg[4], 12, 1, cn->fb);
      if(sp2->bl <= 0 && sp2->zl <= len)
      {  if(fseek(sp->fp, sp2->pos, SEEK_SET))
	 e_s_error(e_msg[17], 13, 1, cn->fb);
	 if(fwrite(str->s, len, 1, sp->fp) != 1)
	 e_s_error(e_msg[17], 14, 1, cn->fb);
	 sp2->bl = 1;
	 str->sp = n + MAXEDT + 1;
	 if(fseek(sp->fp2, n * sizeof(struct pos_strct), SEEK_SET))
	 e_s_error(e_msg[17], 15, 1, cn->fb);
	 if(fwrite(sp2, sizeof(struct pos_strct), 1, sp->fp2) != 1)
	 e_s_error(e_msg[17], 16, 1, cn->fb);
	 FREE(str->s);
	 str->s = NULL;
	 return(n+1);
      }
   }
   if(sp->anz >= sp->max)
   e_s_error(e_msg[18], 17, 1, cn->fb);
   if(fseek(sp->fp, 0, SEEK_END))
   e_s_error(e_msg[17], 18, 1, cn->fb);
   if((sp2->pos = ftell(sp->fp)) < 0)
   e_s_error(e_msg[17], 19, 1, cn->fb);
   if(fwrite(str->s, len, 1, sp->fp) != 1)
   e_s_error(e_msg[17], 20, 1, cn->fb);
   sp2->bl = 1;
   sp2->zl = len;
   if(fseek(sp->fp2, 0, SEEK_END))
   e_s_error(e_msg[17], 21, 1, cn->fb);
   if(fwrite(sp2, sizeof(struct pos_strct), 1, sp->fp2) != 1)
   e_s_error(e_msg[17], 22, 1, cn->fb);
   str->sp = sp->anz;
   FREE(str->s);
   str->s = NULL;
   (sp->anz)++;
   return(sp->anz+1);
}

int e_swap_out2(SFM *sp)
{
   extern struct EXT h_error;
   ECNT *cn = h_error.cn;
   long i;
   e_pr_mtul("SWAPPING", 3, cn->fb->mt.fb);
   if(sp->fp2 != NULL) return(-1);
   {  if((sp->fp2 = fopen(cn->swp2file, WOTYPE)) == NULL)
      e_s_error(e_msg[19], 23, 1, cn->fb);
   }
   for(i = MAXEDT+1; i < sp->anz; i++)
   {  if(fwrite(&(sp->bl[i]), 1, 1, sp->fp2) != 1)
      e_s_error(e_msg[17], 24, 1, cn->fb);
      if(fwrite(&(sp->zl[i]), 1, 1, sp->fp2) != 1)
      e_s_error(e_msg[17], 25, 1, cn->fb);
      if(fwrite(&(sp->pos[i]), sizeof(unsigned long), 1, sp->fp2) != 1)
      e_s_error(e_msg[17], 26, 1, cn->fb);
   }
   sp->pos = realloc(sp->pos, (MAXEDT+1) * sizeof(unsigned long));
   sp->bl = realloc(sp->bl, (MAXEDT+1));
   sp->zl = realloc(sp->zl, (MAXEDT+1));
   e_swap_line_out = e_swp_out_l2;
   e_swap_line_in = e_swp_in_l2;
   e_pr_ul(cn->fb);
   return(0);
}

int e_swap_out_b(SFM *sp)
{
   extern struct EXT h_error;
   ECNT *cn = h_error.cn;
   BUFFER *b;
   int n;
   e_pr_mtul("SWAPPING", 2, cn->fb->mt.fb);
   for(n = swp_b_begin; n < cn->mxedt; n++)
   {  b = cn->f[n]->b;
      if(b->bf != NULL)
      {  if(((((int)sp->bl[b->pos]) << 8) + sp->zl[b->pos]) >= b->mx.y)
	 {  if(fseek(sp->fp, sp->pos[b->pos], SEEK_SET))
	    e_s_error(e_msg[17], 27, 1, cn->fb);
	 }
	 else
	 {  if(fseek(sp->fp, 0, SEEK_END))
	    e_s_error(e_msg[17], 28, 1, cn->fb);
	    if((sp->pos[b->pos] = ftell(sp->fp)) < 0)
	    e_s_error(e_msg[17], 29, 1, cn->fb);
	    sp->zl[b->pos] = (b->mx.y & 8);
	    sp->bl[b->pos] = (b->mx.y >> 8);
	 }
	 if(fwrite(b->bf, b->mx.y * sizeof(STRING), 1, sp->fp) != 1)
	 e_s_error(e_msg[17], 30, 1, cn->fb);
	 FREE(b->bf);
	 b->bf = NULL;
	 e_pr_ul(cn->fb);
	 return(0);
      }
   }
   return(-1);
}

int e_swap_out()
{
   extern struct EXT h_error;
   ECNT *cn = h_error.cn;
   BUFFER *b;
   unsigned long *tmp1 = NULL;
   char *tmp2 = NULL, *tmp3 = NULL;
   long int i, j, len, n, k = cn->sp->fp2 ? MAXEDT + 1 : 0;
   if(cn->sp->fp == NULL)
   {  if((cn->sp->fp = fopen(cn->swpfile, WOTYPE)) == NULL)
      e_s_error(e_msg[19], 31, 1, cn->fb);
   }
   e_pr_mtul("SWAPPING", 1, cn->fb->mt.fb);
   for(j = 0, n = 0; n < cn->mxedt && j < 2*SWPBLCK; n++)
   {  b = cn->f[n]->b;
      if(b->bf == NULL) continue;
      for(i = 0; i < b->mxlines && i < b->b.y - SWPBLCK && j < 2*SWPBLCK; i++)
      {  if(cn->sp->anz >= cn->sp->max) break;
	 if(b->bf[i].s != NULL)
	 {  k = e_swap_line_out(k, b->mx.x, b->bf+i, cn->sp);
	    j++;
	 }
      }
      for(i = b->b.y + SWPBLCK + 1; i < b->mxlines && j < 2*SWPBLCK; i++)
      {  if(cn->sp->anz >= cn->sp->max) break;
	 if(b->bf[i].s != NULL)
	 {  k = e_swap_line_out(k, b->mx.x, b->bf+i, cn->sp);
	    j++;
	 }
      }
   }
   for(n = 0; n < cn->mxedt && j < 2*SWPBLCK; n++)
   {  b = cn->f[n]->b;
      if(b->bf == NULL) continue;
      for(i = 0; i < b->mxlines && j < 2*SWPBLCK; i++)
      {  if(cn->sp->anz >= cn->sp->max) break;
	 if(b->bf[i].s != NULL && i != b->b.y)
	 {  k = e_swap_line_out(k, b->mx.x, b->bf+i, cn->sp);
	    j++;
	 }
      }
   }
   b = cn->f[cn->mxedt]->b;
   for(i = 0; i < b->mxlines && i < b->b.y - SWPBLCK && j < 2*SWPBLCK; i++)
   {  if(cn->sp->anz >= cn->sp->max) break;
      if(b->bf[i].s != NULL)
      {  k = e_swap_line_out(k, b->mx.x, b->bf+i, cn->sp);
	 j++;
      }
   }
   for(i = b->b.y + SWPBLCK + 1; i < b->mxlines && j < 2*SWPBLCK; i++)
   {  if(cn->sp->anz >= cn->sp->max) break;
      if(b->bf[i].s != NULL)
      {  k = e_swap_line_out(k, b->mx.x, b->bf+i, cn->sp);
	 j++;
      }
   }
   if(j < 1)
   {  if(e_swap_out_b(cn->sp))
      {  e_pr_ul(cn->fb);
	 if(cn->sp->fp2 != NULL) return(-1);
	 e_swap_out2(cn->sp);
      }
   }
   if(cn->sp->fp2 != NULL)
   {  e_pr_ul(cn->fb);
      return(0);
   }
   if(cn->sp->max - cn->sp->anz <= 2 * SWPBLCK)
   {  len = cn->sp->max;
      cn->sp->max = 2 * (SWPBLCK + 1) + MAXEDT + cn->sp->anz;
      if((tmp1 = malloc(cn->sp->max * sizeof(unsigned long))) != NULL
	    && (tmp2 = malloc(cn->sp->max)) != NULL
	    && (tmp3 = malloc(cn->sp->max)) != NULL)
      {  for(i = 0; i < len; i++) tmp1[i] = cn->sp->pos[i];
	 for(i = 0; i < len; i++) tmp2[i] = cn->sp->bl[i];
	 for(i = 0; i < len; i++) tmp3[i] = cn->sp->zl[i];
	 free(cn->sp->pos);
	 free(cn->sp->bl);
	 free(cn->sp->zl);
	 cn->sp->pos = tmp1;
	 cn->sp->bl = tmp2;
	 cn->sp->zl = tmp3;
	 e_pr_ul(cn->fb);
	 return(0);
      }
      else
      {  if(tmp1 != NULL) free(tmp1);
	 if(tmp2 != NULL) free(tmp2);
	 if(tmp3 != NULL) free(tmp3);
	 cn->sp->max = len;
	 if(e_swap_out()) return(-2);
      }
      
/*	if((cn->sp->pos = realloc(cn->sp->pos,
		cn->sp->max * sizeof(unsigned long))) == NULL) return(-4);
	if((cn->sp->bl = realloc(cn->sp->bl, cn->sp->max)) == NULL)
								return(-2);
	if((cn->sp->zl = realloc(cn->sp->zl, cn->sp->max)) == NULL)
								return(-3);
	for(i = cn->sp->anz; i < cn->sp->max; i++) cn->sp->bl[i] = 0;
*/
   }
   e_pr_ul(cn->fb);
   return(0);
}

int e_swp_in_line(long i, BUFFER *b)
{
   if(fseek(b->sp->fp, b->sp->pos[b->bf[i].sp], SEEK_SET))
   e_s_error(e_msg[4], 32, 1, b->fb);
   if(fread(b->bf[i].s, b->bf[i].nrc, 1, b->sp->fp) != 1)
   e_s_error(e_msg[4], 33, 1, b->fb);
   (b->sp->bl[b->bf[i].sp])--;
   return(0);
}

int e_swp_in_l2(long i, BUFFER *b)
{
   struct pos_strct {  char bl; char zl; unsigned long pos;  }  sp2[1];
   if(fseek(b->sp->fp2, (b->bf[i].sp - MAXEDT - 1)
				* sizeof(struct pos_strct), SEEK_SET))
   e_s_error(e_msg[4], 34, 1, b->fb);
   if(fread(sp2, sizeof(struct pos_strct), 1, b->sp->fp2) != 1)
   e_s_error(e_msg[4], 35, 1, b->fb);
   if(fseek(b->sp->fp, sp2->pos, SEEK_SET))
   e_s_error(e_msg[4], 36, 1, b->fb);
   if(fread(b->bf[i].s, b->bf[i].nrc, 1, b->sp->fp) != 1)
   e_s_error(e_msg[4], 37, 1, b->fb);
   (sp2->bl)--;
   if(fseek(b->sp->fp2, (b->bf[i].sp - MAXEDT - 1)
				* sizeof(struct pos_strct), SEEK_SET))
   e_s_error(e_msg[17], 38, 1, b->fb);
   if(fwrite(sp2, sizeof(struct pos_strct), 1, b->sp->fp2) != 1)
   e_s_error(e_msg[17], 39, 1, b->fb);
   return(0);
}

int e_swap_in_b(BUFFER *b)
{
   SFM *sp = b->sp;
   if(b->bf != NULL) return(0);
   e_pr_mtul("SWAPPING", 2, b->fb->mt.fb);
   if((b->bf = e_malloc(b->mx.y * sizeof(STRING))) == NULL)
   e_s_error(e_msg[0], 40, 2, b->fb);
   if(fseek(sp->fp, sp->pos[b->pos], SEEK_SET))
   e_s_error(e_msg[4], 41, 2, b->fb);
   if(fread(b->bf, b->mx.y * sizeof(STRING), 1, sp->fp) != 1)
   e_s_error(e_msg[4], 42, 2, b->fb);
   e_pr_ul(b->fb);
   return(0);
}

void e_swap_in(unsigned n, BUFFER *b)
{
   long i, ys = b->b.y;
   b->b.y = n;
   e_pr_mtul("SWAPPING", 1, b->fb->mt.fb);
   for(i = n; i < n + SWPBLCK && i < b->mxlines; i++)
   {  if(b->bf[i].s == NULL)
      {  if((b->bf[i].s = e_malloc(b->mx.x + 1)) == NULL)
	 e_s_error(e_msg[0], 43, 1, b->fb);
	 e_swap_line_in(i, b);
      }
   }
   b->b.y = ys;
   e_pr_ul(b->fb);
}

int e_swp_bl_ad(long pos, int n)
{
   extern struct EXT h_error;
   SFM *sp = h_error.cn->sp;
   char bl;
   if(fseek(sp->fp2, pos - MAXEDT - 1, SEEK_SET))
   e_s_error(e_msg[4], 44, 1, h_error.cn->fb);
   if(fread(&bl, 1, 1, sp->fp2) != 1)
   e_s_error(e_msg[4], 45, 1, h_error.cn->fb);
   bl += n;
   if(fseek(sp->fp2, - 1, SEEK_CUR))
   e_s_error(e_msg[17], 46, 1, h_error.cn->fb);
   if(fwrite(&bl, 1, 1, sp->fp2) != 1)
   e_s_error(e_msg[17], 47, 1, h_error.cn->fb);
   return(0);
}

int e_s_error(char *str, int n, int ex, FARBE *fb)
{
   char tmp[128];
   sprintf(tmp, "%s N: %d", str, n);
   return(e_error(tmp, ex, fb));
}





#endif
