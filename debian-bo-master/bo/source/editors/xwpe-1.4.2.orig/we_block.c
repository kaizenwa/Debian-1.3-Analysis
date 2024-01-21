/* we_block.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"
#include <ctype.h>
#ifdef DOS
#include <alloc.h>
#endif
#ifdef WEUNDO
extern int e_undo_sw;
#endif

/*
	    Block loeschen   */

int e_blck_del(f)
     FENSTER *f;
{
   BUFFER *b;
   SCHIRM *s;
   int i;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;  s = f->s;
   if(f->ins == 8) return(ESC);
   if(s->ka.y == s->ke.y)
   {
#ifdef SWAP
      if(b->bf[s->ka.y].s == NULL) e_swap_in(s->ka.y, b);
#endif
      e_del_nchar(b, s, s->ka.x, s->ka.y, s->ke.x - s->ka.x);
      b->b.x = s->ke.x = s->ka.x;
   }
   else
#ifdef WEUNDO
   {  e_add_undo('d', b, s->ka.x, s->ka.y, 0);
#else
   {  e_blck_clear(b, s);
#endif
      f->save = b->cn->maxchg + 1;  }
   sc_txt_1(f);
   e_cursor(f, 1);
   e_schirm(f, 1);
   return(0);
}

int e_blck_clear(b, s)
     BUFFER *b;
     SCHIRM *s;
{
   int i;
   int len = (s->ke.y-s->ka.y-1);
   if(s->ke.y == s->ka.y)
   {
#ifdef SWAP
      if(b->bf[s->ka.y].s == NULL) e_swap_in(s->ka.y, b);
#endif
      e_del_nchar(b, s, s->ka.x, s->ke.y, s->ke.x - s->ka.x);
      b->b.x = s->ke.x = s->ka.x;
      b->b.y = s->ke.y = s->ka.y;
      return(0);
   }
   for (i = s->ka.y + 1; i < s->ke.y; i++)
#ifdef SWAP
   {  if(b->bf[i].s == NULL)
      {  if(b->sp->fp2)  e_swp_bl_ad(b->bf[i].sp, -1);
	 else  (b->sp->bl[b->bf[i].sp])--;
      }
      else  FREE(b->bf[i].s);
   }
#else
   FARFREE(b->bf[i].s);
#endif
   for (i = s->ka.y + 1; i <= b->mxlines-len; i++)
   b->bf[i] = b->bf[i+len];
   (b->mxlines) -= len;
   (s->ke.y) -= len;
#ifdef SWAP
   if(b->bf[s->ka.y].s == NULL) e_swap_in(s->ka.y, b);
#endif
   e_del_nchar(b, s, 0, s->ke.y, s->ke.x);
   if(*(b->bf[s->ka.y].s+(len = b->bf[s->ka.y].len)) != '\0') len++;
   e_del_nchar(b, s, s->ka.x, s->ka.y, len - s->ka.x);
   b->b.x = s->ke.x = s->ka.x;
   b->b.y = s->ke.y = s->ka.y;
   return(0);
}
/*
            Puffer auf Schirm anzeigen  */


int e_show_clipboard(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   FENSTER *fo;
   int i, j;
   for (j = 1; j <= cn->mxedt; j++)
   if(cn->f[j] == cn->f[0])
   {  e_switch_window(cn->edt[j], f);
      return(0);
   }
   
   if(cn->mxedt > MAXEDT)
   {  e_error(e_msg[3], 0, cn->fb);
      return(0);
   }
   for (j = 1; j <= MAXEDT; j++)
   {  for (i = 1; i <= cn->mxedt && cn->edt[i] != j; i++);
      if( i > cn->mxedt) break;
   }
   cn->curedt=j;
   (cn->mxedt)++;
   cn->edt[cn->mxedt]=j;
   
   f = cn->f[cn->mxedt] = cn->f[0];
#ifdef PROG
   if(e_we_sw & 2)
   {  for(i = cn->mxedt-1; i > 0; i--);
      if(i < 1)
      {  f->a = e_s_pkt(0, 1);
	 f->e = e_s_pkt(MAXSCOL-1, 2*MAXSLNS/3);
      }
      else
      {  f->a = e_s_pkt(cn->f[i]->a.x+1, cn->f[i]->a.y+1);
	 f->e = e_s_pkt(cn->f[i]->e.x, cn->f[i]->e.y);
      }
   }
   else
#endif
   {  if(cn->mxedt < 2)
      {  f->a = e_s_pkt(0, 1);
	 f->e = e_s_pkt(MAXSCOL-1, MAXSLNS-2);
      }
      else
      {  f->a = e_s_pkt(cn->f[cn->mxedt-1]->a.x+1,
					cn->f[cn->mxedt-1]->a.y+1);
	 f->e = e_s_pkt(cn->f[cn->mxedt-1]->e.x,
					cn->f[cn->mxedt-1]->e.y);
      }
   }
   f->winnum = cn->curedt;
   
   if(cn->mxedt > 1)
   {  fo = cn->f[cn->mxedt-1];
      e_ed_rahmen(fo, 0);
   }
   e_firstl(f, 1);
   e_zlsplt(f);
   e_schirm(f, 1);
   e_cursor(f, 1);
   return(0);
}
/*
            Block in Puffer schieben  */

int e_edt_del(f)
     FENSTER *f;
{
#ifdef WEUNDO
   e_edt_copy(f);
   e_blck_del(f);
#else
   BUFFER *b;
   BUFFER *b0 = f->ed->e[0]->b;
   int i;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;
#ifdef SWAP
   swp_b_begin = 1;
   if(b0->bf == NULL) e_swap_in_b(b0);
   for(i = 1; i < b0->mxlines; i++)
   {  if(b0->bf[i].s == NULL)
      {  if(b0->sp->fp2)  e_swp_bl_ad(b0->bf[i].sp, -1);
	 else  (b0->sp->bl[b0->bf[i].sp])--;
      }
      else  FREE(b0->bf[i].s);
   }
#else
   for(i = 1; i < b0->mxlines; i++)
   FARFREE(b0->bf[i].s);
#endif
   b0->mxlines = 1;
   *(b0->bf[0].s) = WR;
   *(b0->bf[0].s+1) = '\0';
   b0->bf[0].len = 0;
   e_vsch_block(0, 0, b, b0, f);
#ifdef SWAP
   swp_b_begin = 0;
#endif
#endif
   return(0);
}
/*
            Block in Puffer kopieren  */

int e_edt_copy(f)
     FENSTER *f;
{
   BUFFER *b;
   BUFFER *b0 = f->ed->f[0]->b;
   int i, save;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;  save = f->save;
#ifdef SWAP
   swp_b_begin = 1;
   if(b0->bf == NULL) e_swap_in_b(b0);
   for(i = 1; i < b0->mxlines; i++)
   {  if(b0->bf[i].s == NULL)
      {  if(b0->sp->fp2)  e_swp_bl_ad(b0->bf[i].sp, -1);
	 else  (b0->sp->bl[b0->bf[i].sp])--;
      }
      else  FREE(b0->bf[i].s);
   }
#else
   for(i = 1; i < b0->mxlines; i++)
   FARFREE(b0->bf[i].s);
#endif
   b0->mxlines = 1;
   *(b0->bf[0].s) = WR;
   *(b0->bf[0].s+1) = '\0';
   b0->bf[0].len = 0;
   e_copy_block(0, 0, b, b0, f);
#ifdef SWAP
   swp_b_begin = 0;
#endif
   f->save = save;
   return(0);
}
/*
            Block Puffer in Fenster kopieren  */

int e_edt_einf(f)
     FENSTER *f;
{
   BUFFER *b;
   BUFFER *b0 = f->ed->f[0]->b;
   int i;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;
   if(f->ins == 8) return(0);
#ifdef SWAP
   swp_b_begin = 1;
   if(b0->bf == NULL) e_swap_in_b(b0);
#endif
#ifdef WEUNDO
   e_undo_sw = 1;
   e_copy_block(b->b.x, b->b.y, b0, b, f);
   e_undo_sw = 0;
   e_add_undo('c', b, b->b.x, b->b.y, 0);
#else
   e_copy_block(b->b.x, b->b.y, b0, b, f);
#endif
#ifdef SWAP
   swp_b_begin = 0;
#endif
   sc_txt_2(f);
   f->save = b->cn->maxchg + 1;
   return(0);
}
/*
            Block innerhalb eines Fensters verschieben   */

int e_blck_versch(f)
     FENSTER *f;
{
   BUFFER *b;
   int i;
#ifdef WEUNDO
   PUNKT ka;
#endif
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;
#ifdef WEUNDO
   ka.x = f->ed->f[f->ed->mxedt]->s->ka.x;
   if(b->b.y > f->ed->f[f->ed->mxedt]->s->ka.y)
   ka.y = f->ed->f[f->ed->mxedt]->s->ka.y;
   else
   {  ka.y = f->ed->f[f->ed->mxedt]->s->ke.y;
      if(b->b.y == f->ed->f[f->ed->mxedt]->s->ka.y &&
		b->b.x < f->ed->f[f->ed->mxedt]->s->ka.x)
      ka.x = f->ed->f[f->ed->mxedt]->s->ke.x
				+ f->ed->f[f->ed->mxedt]->s->ka.x - b->b.x;
   }
   e_undo_sw = 1;
   e_vsch_block(b->b.x, b->b.y, b, b, f);
   e_undo_sw = 0;
   e_add_undo('v', b, ka.x, ka.y, 0);
#else
   e_vsch_block(b->b.x, b->b.y, b, b, f);
#endif
   f->save = b->cn->maxchg + 1;
   return(0);
}
/*
	    Block verschieben   */

void e_vsch_block(x, y, bv, bz, f)
     int x;
     int y;
     BUFFER *bv;
     BUFFER *bz;
     FENSTER *f;
{
   SCHIRM *s = f->ed->f[f->ed->mxedt]->s;
   SCHIRM *sv = bv->f->s;
   SCHIRM *sz = bz->f->s;
   int sw = (y < s->ka.y) ? 0 : 1, i, n = s->ke.y - s->ka.y - 1;
   int kax = s->ka.x, kay = s->ka.y, kex = s->ke.x, key = s->ke.y;
   STRING *str, *tmp;
   unsigned char far *cstr;
   if(key < kay || (kay == key && kex <= kax)) return;
   if((cstr = FARMALLOC(bv->mx.x + 1)) == NULL)
   if(f->ins == 8)
   {
      FARFREE(cstr);
      return;
   }
   if(bv == bz && y >= kay && y <= key && x >= kax && x <= kex)
   {
      FARFREE(cstr);
      return;
   }
   if(kay == key)
   {  if( bv == bz && y == kay && x >= kax && x <= kex )
      {
	 FARFREE(cstr);
	 return;
      }
      n = kex - kax;
      bz->b.x = x; bz->b.y = y;
      if((cstr = FARMALLOC(f->ed->maxcol * sizeof(char far))) == NULL)
      {  e_error(e_msg[0], 0, bz->fb);
	 FARFREE(cstr);
	 return;
      }
#ifdef SWAP
      if(bv->bf[key].s == NULL) e_swap_in(key, bv);
#endif
      for(i = 0; i < n; i++) cstr[i] = bv->bf[key].s[kax+i];
/*
	if(bv == bz)
	{  if(kay == y && x < kax) {  kax += n; kex += n;  }
	   kay += (bz->b.y-y);
	}
*/
#ifdef SWAP
      if(bz->bf[y].s == NULL) e_swap_in(y, bz);
#endif
      e_ins_nchar(bz, sz, cstr, x, y, n);
      bv->b.x = kax;  bv->b.y = kay+bz->b.y-y;
/*
	sv->ka.x = sv->ke.x = bv->b.x;  sv->ka.y = sv->ke.y = bv->b.y;
*/
#ifdef SWAP
      if(bv->bf[kay].s == NULL) e_swap_in(kay, bv);
#endif
/*
	e_del_nchar(bv, sv, kax, kay, n);
*/
      e_del_nchar(bv, sv, sv->ka.x, sv->ka.y, n);
      if(bv == bz && kay == y && x > kex)  x -= n;
      sz->ka.x = x;   sz->ke.x = bz->b.x = x + n;
      sz->ka.y = sz->ke.y = bz->b.y = y;
      e_cursor(f, 1);
      e_schirm(f, 1);
      FARFREE(cstr);
      return;
   }
   
   if(bv == bz && y == kay && x < kax )
   {  n = kax - x;
      bv->b.x = x; bv->b.y = y;
      if((cstr = FARMALLOC(f->ed->maxcol * sizeof(char far))) == NULL)
      {  e_error(e_msg[0], 0, bz->fb);
	 return;
      }
#ifdef SWAP
      if(bv->bf[y].s == NULL) e_swap_in(y, bv);
#endif
      for(i = 0; i < n; i++) cstr[i] = bv->bf[y].s[x+i];
      e_del_nchar(bv, sv, x, y, n);
      bz->b.x = kex; bz->b.y = key;
#ifdef SWAP
      if(bz->bf[y].s == NULL) e_swap_in(y, bz);
#endif
      e_ins_nchar(bz, sz, cstr, kex, key, n);
      bv->b.x = sv->ka.x = x;  bv->b.y = sv->ka.y = y;
      sv->ke.x = kex;  sv->ke.y = key;
      e_cursor(f, 1);
      e_schirm(f, 1);
      FARFREE(cstr);
      return;
   }
   
   if(bv == bz && y == key && x > kex )
   {  n = x - kex;
      bv->b.x = kex; bv->b.y = y;
      if((cstr = FARMALLOC(f->ed->maxcol * sizeof(char far))) == NULL)
      {  e_error(e_msg[0], 0, bz->fb);
	 return;
      }
#ifdef SWAP
      if(bv->bf[y].s == NULL) e_swap_in(y, bv);
#endif
      for(i = 0; i < n; i++) cstr[i] = bv->bf[y].s[kex+i];
      e_del_nchar(bv, sv, kex, y, n);
      bz->b.x = kex; bz->b.y = key;
#ifdef SWAP
      if(bz->bf[y].s == NULL) e_swap_in(y, bz);
#endif
      e_ins_nchar(bz, sz, cstr, kax, kay, n);
      
      bv->b.x = sv->ke.x;  bv->b.y = sv->ke.y;
      e_cursor(f, 1);
      e_schirm(f, 1);
      FARFREE(cstr);
      return;
   }
   
   if(bv == bz && ( (y > kay && y < key) ||
       ( y == kay && x >= kax ) || ( y == key && x <= kex) ))
   {
      FARFREE(cstr);
   }
   while(bz->mxlines+n > bz->mx.y-2)
   {  bz->mx.y += MAXLINES;
      if((tmp = REALLOC(bz->bf, bz->mx.y * sizeof(STRING))) == NULL)
	 e_error(e_msg[0], 1, bz->fb);
      else
	 bz->bf = tmp;
      if(bz->f->c_sw) bz->f->c_sw = REALLOC(bz->f->c_sw ,
	 bz->mx.y * sizeof(int));
   }
   if((str = MALLOC((n+2) * sizeof(STRING))) == NULL)
   {  e_error(e_msg[0], 0, bz->fb);
      FARFREE(cstr);
      return;
   }
   for(i = kay; i <= key; i++)
   str[i-kay] = bv->bf[i];
   e_new_line(y+1, bz);
#ifdef SWAP
   if(bz->bf[y].s == NULL) e_swap_in(y, bz);
#endif
   if(*(bz->bf[y].s+bz->bf[y].len) != '\0') (bz->bf[y].len)++;
   for(i = x;  i <= bz->bf[y].len; i++)
		*(bz->bf[y+1].s+i-x) = *(bz->bf[y].s+i);
   *(bz->bf[y].s+x) = '\0';
   bz->bf[y].len = bz->bf[y].nrc = x;
   bz->bf[y+1].len = e_str_len(bz->bf[y+1].s);
   bz->bf[y+1].nrc = e_str_nrc(bz->bf[y+1].s);
   for(i = bz->mxlines; i > y; i--)
   bz->bf[i+n] = bz->bf[i];
   (bz->mxlines) += n;
   for(i = 1; i <= n; i++)
   bz->bf[y+i] = str[i];
   
   if(bz == bv)
   {  if(y < kay)  {  kay += (n+1);  key +=(n+1);  }
      else if(y > kay)  y -= n;
   }
   
   for (i = kay + 1; i <= bv->mxlines-n; i++)
   bv->bf[i] = bv->bf[i+n];
   (bv->mxlines) -= n;
   
   if(*(str[0].s+(n = str[0].len)) != '\0') n++;
#ifdef SWAP
   if(str[key-kay].s == NULL)
   {  if(fseek(bv->sp->fp, bv->sp->pos[str[key-kay].sp], SEEK_SET))
      e_error(e_msg[4], 1, bv->fb);
      if(fread(cstr, str[key-kay].nrc, 1, bv->sp->fp) != 1)
      e_error(e_msg[4], 1, bv->fb);
   }
   else strcpy(cstr, str[key-kay].s);
   if(bz->bf[y+key-kay].s == NULL) e_swap_in(y+key-kay, bz);
   e_ins_nchar(bz, sz, cstr, 0, y+key-kay, kex);
   if(str[0].s == NULL)
   {  if(fseek(bv->sp->fp, bv->sp->pos[str[0].sp], SEEK_SET))
      e_error(e_msg[4], 1, bv->fb);
      if(fread(cstr, str[0].nrc, 1, bv->sp->fp) != 1)
      e_error(e_msg[4], 1, bv->fb);
   }
   else strcpy(cstr, str[0].s);
   if(bz->bf[y].s == NULL) e_swap_in(y, bz);
   e_ins_nchar(bz, sz, cstr+kax, x, y, n-kax);
#else
   e_ins_nchar(bz, sz, (str[key-kay].s), 0, y+key-kay, kex);
   e_ins_nchar(bz, sz, (str[0].s+kax), x, y, n-kax);
#endif
#ifdef SWAP
   if(bv->bf[kay].s == NULL) e_swap_in(kay, bv);
#endif
   e_del_nchar(bv, sv, 0, kay+1, kex);
   e_del_nchar(bv, sv, kax, kay, n - kax);
   
   if(bz == bv && sw != 0) y--;
   
   sv->ka.x = sv->ke.x = bv->b.x = kax;  sv->ka.y = sv->ke.y = bv->b.y = kay;
   
   sz->ka.x = x;
   sz->ka.y = y;
   bz->b.x = sz->ke.x = kex;
   bz->b.y = sz->ke.y = key-kay+y;
   
   f->save = f->ed->maxchg + 1;
   sc_txt_1(bv->f);
   sc_txt_1(bz->f);
   e_cursor(f, 1);
   e_schirm(f, 1);
   FARFREE(cstr);
   FREE(str);
}

/*
	    Block innerhalb eines Fensters kopieren   */

int e_blck_copy(f)
     FENSTER *f;
{
   BUFFER *b;
   int i;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;
   if(f->ins == 8) return(0);
   f->save = 1;
#ifdef WEUNDO
   e_undo_sw = 1;
   e_copy_block(b->b.x, b->b.y, b, b, f);
   e_undo_sw = 0;
   e_add_undo('c', b, b->b.x, b->b.y, 0);
#else
   e_copy_block(b->b.x, b->b.y, b, b, f);
#endif
   sc_txt_2(f);
   f->save = b->cn->maxchg + 1;
   return(0);
}
/*
            Block kopieren   */

void e_copy_block(x, y, bv, bz, f)
     int x;
     int y;
     BUFFER *bv;
     BUFFER *bz;
     FENSTER *f;
{
   BUFFER *b = f->ed->f[f->ed->mxedt]->b;
   SCHIRM *sv = bv->f->s;
   SCHIRM *sz = bz->f->s;
   int i, j, n = sv->ke.y - sv->ka.y - 1;
   int kax = sv->ka.x, kay = sv->ka.y, kex = sv->ke.x, key = sv->ke.y;
   int kse = key, ksa = kay;
   STRING **str, *tmp;
   unsigned char far *cstr;
   if(key < kay || (kay == key && kex <= kax)) return;
   if((cstr = FARMALLOC(bv->mx.x + 1)) == NULL)
   {  e_error(e_msg[0], 0, bz->fb);
      return;
   }
   if(kay == key)
   {  if( bv == bz && y == kay && x >= kax && x < kex )
      {
	 FARFREE(cstr);
	 return;
      }
      n = kex - kax;
      bz->b.x = x; bz->b.y = y;
#ifdef SWAP
      if(bv->bf[key].s == NULL) e_swap_in(key, bv);
#endif
      for(i = 0; i < n; i++) cstr[i] = bv->bf[key].s[kax+i];
#ifdef SWAP
      if(bz->bf[y].s == NULL) e_swap_in(y, bz);
#endif
      e_ins_nchar(bz, sz, cstr, x, y, n);
      sz->ka.x = x;   sz->ke.x = bz->b.x = x + n;
      sz->ka.y = sz->ke.y = bz->b.y = y;
      FARFREE(cstr);
      e_cursor(f, 1);
      e_schirm(f, 1);
      return;
   }
   
   if(bv == bz && ( (y > kay && y < key) ||
       ( y == kay && x >= kax ) || ( y == key && x < kex) ))
   {
      FARFREE(cstr);
      return;
   }
   while(bz->mxlines+n > bz->mx.y-2)
   {  bz->mx.y += MAXLINES;
      if((tmp = REALLOC(bz->bf, bz->mx.y * sizeof(STRING))) == NULL)
	 e_error(e_msg[0], 1, bz->fb);
      else
	 bz->bf = tmp;
      if(bz->f->c_sw) bz->f->c_sw = REALLOC(bz->f->c_sw ,
	 bz->mx.y * sizeof(int));
   }
   if((str = MALLOC((n+2) * sizeof(STRING *))) == NULL)
   {  e_error(e_msg[0], 0, bz->fb);
      FARFREE(cstr);
      return;
   }
   e_new_line(y+1, bz);
#ifdef SWAP
   yvs = bv->b.y;
   if(bz->bf[y].s == NULL) e_swap_in(y, bz);
#endif
   if(bz == bv && y < ksa) {  kse += (n+1);  ksa += (n+1);  }
   if(*(bz->bf[y].s+bz->bf[y].len) != '\0') (bz->bf[y].len)++;
   for(i = x;  i <= bz->bf[y].len; i++)
		*(bz->bf[y+1].s+i-x) = *(bz->bf[y].s+i);
   *(bz->bf[y].s+x) = '\0';
   bz->bf[y].len = bz->bf[y].nrc = x;
   bz->bf[y+1].len = e_str_len(bz->bf[y+1].s);
   bz->bf[y+1].nrc = e_str_nrc(bz->bf[y+1].s);
   for(i = bz->mxlines; i > y; i--)  bz->bf[i+n] = bz->bf[i];
   (bz->mxlines) += n;
   for(i = ksa; i <= kse; i++)
   str[i-ksa] = &(bv->bf[i]);
   for(i = 1; i <= n; i++)
#ifdef SWAP
   {  bz->bf[i+y].s = NULL;   bz->bf[i+y].sp = 0;  }
#else
   bz->bf[i+y].s = NULL;
#endif
   for(i = 1; i <= n; i++)
#ifdef SWAP
   {  again:
       if( str[i]->s == NULL)
      {  (bz->sp->bl[str[i]->sp])++;
	 if(bz->sp->fp2)  e_swp_bl_ad(str[i]->sp, 1);
	 else  (bz->sp->bl[str[i]->sp])++;
	 bz->bf[i+y].sp = str[i]->sp;
      }
      else
      {  if( (bz->bf[i+y].s = MALLOC((bz->mx.x+1)*sizeof(char))) == NULL)
	 e_error(e_msg[0], 1, b->fb);
	 if( str[i]->s == NULL) goto again;
	 for(j = 0; j <= str[i]->len; j++)
		      *(bz->bf[i+y].s+j) = *(str[i]->s+j);
	 if(*(str[i]->s+str[i]->len) != '\0') *(bz->bf[i+y].s+j) = '\0';
      }
#else
   {  if( (bz->bf[i+y].s = FARMALLOC(bz->mx.x+1)) == NULL)
      e_error(e_msg[0], 1, b->fb);
      for(j = 0; j <= str[i]->len; j++)
	  *(bz->bf[i+y].s+j) = *(str[i]->s+j);
      if(*(str[i]->s+str[i]->len) != '\0') *(bz->bf[i+y].s+j) = '\0';
#endif
      bz->bf[i+y].len = str[i]->len;
      bz->bf[i+y].nrc = str[i]->nrc;
   }
   
#ifdef SWAP
   if(str[key-kay]->s == NULL) e_swap_in(kse, bv);
#endif
   for(i = 0; i < kex; i++) cstr[i] = str[key-kay]->s[i];
#ifdef SWAP
   if(bz->bf[y+key-kay].s == NULL) e_swap_in(y+key-kay, bz);
#endif
   e_ins_nchar(bz, sz, cstr, 0, y+key-kay, kex);
#ifdef SWAP
   if(str[0]->s == NULL) e_swap_in(ksa, bv);
#endif
   if(*(str[0]->s+(n = str[0]->len)) != '\0') n++;
   for(i = 0; i < n - kax; i++) cstr[i] = str[0]->s[i+kax];
   cstr[n-kax] = '\0';
#ifdef SWAP
   if(bz->bf[y].s == NULL) e_swap_in(y, bz);
   bv->b.y = yvs;
#endif
   e_ins_nchar(bz, sz, cstr, x, y, n-kax);
   sz->ka.x = x;
   sz->ka.y = y;
   bz->b.x = sz->ke.x = kex;
   bz->b.y = sz->ke.y = key-kay+y;
   
   e_cursor(f, 1);
   e_schirm(f, 1);
   FARFREE(cstr);
   FREE(str);
}
/*
	    Blockmarken loeschen   */

int e_blck_hide(f)
     FENSTER *f;
{
   BUFFER *b;
   SCHIRM *s;
   int i;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;  s = f->s;
   s->ka = e_s_pkt(0, 0);
   s->ke = e_s_pkt(0, 0);
   e_schirm(f, 1);
   return(0);
}

/*
            Block - Beginn setzen   */

int e_blck_begin(f)
     FENSTER *f;
{
   int i;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   f->s->ka = f->b->b;
   e_schirm(f, 1);
   return(0);
}

/*
            Block - Ende setzen   */

int e_blck_end(f)
     FENSTER *f;
{
   int i;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   f->s->ke = f->b->b;
   e_schirm(f, 1);
   return(0);
}

/*
            Gehe zu Block - Beginn   */

int e_blck_gt_beg(f)
     FENSTER *f;
{
   int i;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   f->b->b = f->s->ka;
   e_schirm(f, 1);
   return(0);
}

/*
            Gehe zu Block - Ende   */

int e_blck_gt_end(f)
     FENSTER *f;
{
   int i;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   f->b->b = f->s->ke;
   e_schirm(f, 1);
   return(0);
}

/*
            Block - Zeile markieren   */

int e_blck_mrk_all(f)
     FENSTER *f;
{
   int i;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   f->s->ka.x = 0;
   f->s->ka.y = 0;
   f->s->ke.y = f->b->mxlines-1;
   f->s->ke.x = f->b->bf[f->b->mxlines-1].len;
   e_schirm(f, 1);
   return(0);
}

/*
            Block - Zeile markieren   */

int e_blck_mrk_line(f)
     FENSTER *f;
{
   int i;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   f->s->ka.x = 0;
   f->s->ka.y = f->b->b.y;
   if(f->b->b.y < f->b->mxlines-1)
   {  f->s->ke.x = 0;
      f->s->ke.y = f->b->b.y+1;
   }
   else
   {  f->s->ke.x = f->b->bf[f->b->b.y].len;
      f->s->ke.y = f->b->b.y;
   }
   e_schirm(f, 1);
   return(0);
}

/*
            Block - ausruecken   */

int e_blck_to_left(f)
     FENSTER *f;
{
   BUFFER *b;
   SCHIRM *s;
   int n = f->ed->tabn/2, i, j, k, l, m, nn;
   char *tstr = MALLOC((n+2)*sizeof(char));
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;
   s = f->s;
   for(i = 0; i <= n; i++) tstr[i] = ' ';
   tstr[n] = '\0';
   for(i = (!s->ka.x) ? s->ka.y : s->ka.y + 1;
			i < s->ke.y || (i == s->ke.y && s->ke.x > 0); i++)
   {  for(j = 0; j < b->bf[i].len && isspace(b->bf[i].s[j]); j++);
      for(l = j - 1, k = 0; l >= 0 && k < n; l--)
      {  if(b->bf[i].s[l] == ' ') k++;
	 else if(b->bf[i].s[l] == '\t')
	 {  for(nn = m = 0; m < l; m++)
	    {  if(b->bf[i].s[m] == ' ') nn++;
	       else if(b->bf[i].s[m] == '\t')
	       nn += f->ed->tabn - (nn % f->ed->tabn);
	    }
	    k += f->ed->tabn - (nn % f->ed->tabn);
	 }
      }
      l = j - l - 1;
      if(l > 0)
      {  nn = s->ka.x;
	 e_del_nchar(b, s, j - l, i, l);
	 if(k > n) e_ins_nchar(b, s, tstr, j-l, i, k-n);
	 s->ka.x = nn;
      }
   }
   FREE(tstr);
   e_schirm(f, 1);
   return(0);
}

/*
            Block - einruecken   */

int e_blck_to_right(f)
     FENSTER *f;
{
   BUFFER *b;
   SCHIRM *s;
   int n = f->ed->tabn/2, i, j;
   char *tstr = MALLOC((n+1)*sizeof(char));
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;
   s = f->s;
   for(i = 0; i < n; i++) tstr[i] = ' ';
   tstr[n] = '\0';
   for(i = (!s->ka.x) ? s->ka.y : s->ka.y + 1;
			i < s->ke.y || (i == s->ke.y && s->ke.x > 0); i++)
   {  for(j = 0; b->bf[i].len && isspace(b->bf[i].s[j]); j++);
      e_ins_nchar(b, s, tstr, j, i, n);
      if(i == s->ka.y) s->ka.x = 0;
   }
   FREE(tstr);
   e_schirm(f, 1);
   return(0);
}

/*
            Block aus File einlesen   */

int e_blck_read(f)
     FENSTER *f;
{
   if(f->ins == 8) return(ESC);
   e_ed_manager(1, f);
   f->save = f->ed->maxchg + 1;
   return(0);
}
/*
            Block in File schreiben   */

int e_blck_write(f)
     FENSTER *f;
{
   e_ed_manager(2, f);
   return(0);
}
/*
	    Wiederholtes Suchen   */

int e_rep_search(f)
     FENSTER *f;
{
   SCHIRM *s;
   BUFFER *b;
   FIND *fd = &(f->ed->fd);
   int i, ym, c, ret, j, iend, jend, end;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;  s = f->s;
   ret = b->b.x;
   j = b->b.y;
   if((fd->sw & 8) == 0)
   {  jend = (fd->sw & 4) ? 0 : b->mxlines-1;
      iend = (fd->sw & 4) ? 0 : b->bf[b->mxlines-1].len;
   }
   else
   {  jend = (fd->sw & 4) ? s->ka.y : s->ke.y;
      iend = (fd->sw & 4) ? s->ka.x : s->ke.x;
   }
   
   for(; ; ret = -1)
   {  if((fd->sw & 4) == 0 && j > jend) break;
      else if((fd->sw & 4) != 0 && j < jend) break;
      do
      {
	 if(j == jend) end = iend;
	 else if(!(fd->sw & 4)) end = b->bf[j].len;
	 else end = 0;
	 if(fd->sw & 4 && ret == -1) ret = b->bf[j].len;
#ifdef SWAP
	 if(b->bf[j].s == NULL)
	 {  if((fd->sw & 4) == 0) e_swap_in(j, b);
	    else  e_swap_in(j - SWPBLCK + 1, b);
	 }
#endif
	 if((fd->sw & 32) == 0)
	 {  if((fd->sw & 128) != 0)
	    ret = e_strstr(ret, end, b->bf[j].s, fd->search);
	    else
	    ret = e_ustrstr(ret, end, b->bf[j].s, fd->search);
	 }
	 else
	 {  if((fd->sw & 128) != 0)
	    ret = e_rstrstr(ret, end, b->bf[j].s, fd->search, &(fd->sn));
	    else
	    ret = e_urstrstr(ret, end, b->bf[j].s, fd->search, &(fd->sn));
	 }
	 if((fd->sw & 4) == 0 && j == jend && ret > iend) break;
	 else if((fd->sw & 4) != 0 && j == jend && ret < iend) break;
	 if(ret >= 0 && ( (fd->sw & 64) == 0
	    || ( isalnum(*(b->bf[j].s+ret+fd->sn)) == 0
	    && (ret == 0 || isalnum(*(b->bf[j].s+ret-1)) == 0) ) ) )
	 {  s->fa.x = ret;
	    b->b.y = s->fa.y = s->fe.y = j;
	    s->fe.x = ret + fd->sn;
	    b->b.x = ret = !(fd->sw & 4) ? s->fe.x : ret;
	    if((fd->sw & 1) != 0)
	    {  if(f->a.y < 11) {  s->c.y = b->b.y - 1; ym = 15;  }
	       else  {  ym = 1;  }
	       e_schirm(f, 1);
	       c = 'Y';
	       if((fd->sw & 16) != 0)
	       c = e_message(1, "String found:\nReplace this occurence ?",f);
	       if(c == 'Y')
	       {
#ifdef WEUNDO
		  e_add_undo('s', b, s->fa.x, b->b.y, fd->sn);
		  e_undo_sw = 1;
#endif
		  e_del_nchar(b, s, s->fa.x, b->b.y, fd->sn);
		  e_ins_nchar(b, s, (unsigned char *)fd->replace,
						s->fa.x, b->b.y, fd->rn);
#ifdef WEUNDO
		  e_undo_sw = 0;
#endif
		  s->fe.x = ret + fd->rn - fd->sn;
		  b->b.x = ret = !(fd->sw & 4) ? s->fe.x : ret;
		  e_schirm(f, 1);
	       }
	       else if(c == ESC) return(0);
	    }
	    else {  e_cursor(f, 1);  s->fa.y = j;  e_schirm(f, 1);  }
	    return(1);
	 }
	 else if(ret >= 0 && (fd->sw & 64) != 0) ret++;
      }  while(ret >= 0);
      if((fd->sw & 4) == 0 && j > jend) break;
      else if((fd->sw & 4) != 0 && j < jend) break;
      j= ((fd->sw & 4) == 0) ? j+1 : j-1;
   }
   e_message(0, e_msg[5], f);
   return(0);
}

/*
            Zu benannten Zeile gehen   */

int e_goto_line(f)
     FENSTER *f;
{
   int i, num;
   SCHIRM *s;
   BUFFER *b;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;  s = f->s;
   if((num = e_num_kst("Goto Line Number", b->b.y+1,
					b->mxlines, f, 0, AltG)) > 0)
   b->b.y = num - 1;
   else if(num == 0) b->b.y = num;
   e_cursor(f, 1);
   return(0);
}

int e_find(f)
     FENSTER *f;
{
   SCHIRM *s;
   BUFFER *b;
   FIND *fd = &(f->ed->fd);
   int i, ret;
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o) return(-1);
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;  s = f->s;
   o->xa = 7;  o->ya = 3;  o->xe = 63;  o->ye = 18;
   o->bgsw = 0;
   o->crsw = AltO;
   o->name = "Find";
   e_add_txtstr(32, 4, "Direction:", o);
   e_add_txtstr(4, 4, "Options:", o);
   e_add_txtstr(4, 9, "Scope:", o);
   e_add_txtstr(32, 9, "Begin:", o);
   e_add_wrstr(4, 2, 18, 2, 35, 128, 0, AltT, "Text to Find:", fd->search, &f->ed->sdf, o);
   e_add_sswstr(5, 5, 0, AltC, fd->sw & 128 ? 1 : 0, "Case sensative    ", o);
   e_add_sswstr(5, 6, 0, AltW, fd->sw & 64 ? 1 : 0, "Whole words only  ", o);
   e_add_sswstr(5, 7, 0, AltR, fd->sw & 32 ? 1 : 0, "Regular expression", o);
   e_add_pswstr(0, 33, 5, 6, AltD, 0, "ForwarD        ", o);
   e_add_pswstr(0, 33, 6, 0, AltB, fd->sw & 4 ? 1 : 0, "Backward       ", o);
   e_add_pswstr(1, 5, 10, 0, AltG, 0, "Global            ", o);
   e_add_pswstr(1, 5, 11, 0, AltS, fd->sw & 8 ? 1 : 0, "Selected Text     ", o);
   e_add_pswstr(2, 33, 10, 0, AltF, 0, "From Cursor    ", o);
   e_add_pswstr(2, 33, 11, 0, AltE, fd->sw & 2 ? 1 : 0, "Entire Scope   ", o);
   e_add_bttstr(16, 13, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(34, 13, -1, ESC, "Cancel", NULL, o);
   ret = e_opt_kst(o);
   if(ret != ESC)
   {  fd->sw = (o->pstr[0]->num << 2) + (o->pstr[1]->num << 3)
		+ (o->pstr[2]->num << 1) + (o->sstr[0]->num << 7)
		+ (o->sstr[1]->num << 6) + (o->sstr[2]->num << 5);
      strcpy(fd->search, o->wstr[0]->txt);
      fd->sn = strlen(fd->search);
      if((fd->sw & 2) != 0)
      {  if((fd->sw & 4) == 0)
	 {  b->b.x = (fd->sw & 8) == 0 ? 0 : s->ka.x;
	    b->b.y = (fd->sw & 8) == 0 ? 0 : s->ka.y;
	 }
	 else
	 {  b->b.x = (fd->sw & 8) == 0 ? b->bf[b->mxlines-1].len : s->ke.x;
	    b->b.y = (fd->sw & 8) == 0 ? b->mxlines-1 : s->ke.y;
	 }
      }
   }
   freeostr(o);
   if(ret != ESC) e_rep_search(f);
   return(0);
}

int e_replace(f)
     FENSTER *f;
{
   SCHIRM *s;
   BUFFER *b;
   FIND *fd = &(f->ed->fd);
   int i, ret;
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o) return(-1);
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;  s = f->s;
   o->xa = 7;  o->ya = 3;  o->xe = 63;  o->ye = 21;
   o->bgsw = 0;
   o->name = "Replace";
   o->crsw = AltO;
   e_add_txtstr(32, 6, "Direction:", o);
   e_add_txtstr(4, 6, "Options:", o);
   e_add_txtstr(4, 12, "Scope:", o);
   e_add_txtstr(32, 12, "Begin:", o);
   e_add_wrstr(4, 2, 18, 2, 35, 128, 0, AltT, "Text to Find:", fd->search, &f->ed->sdf, o);
   e_add_wrstr(4, 4, 18, 4, 35, 128, 0, AltN, "New Text:", fd->replace, &f->ed->rdf, o);
   e_add_sswstr(5, 7, 0, AltC, fd->sw & 128 ? 1 : 0, "Case sensative    ", o);
   e_add_sswstr(5, 8, 0, AltW, fd->sw & 64 ? 1 : 0, "Whole words only  ", o);
   e_add_sswstr(5, 9, 0, AltR, fd->sw & 32 ? 1 : 0, "Regular expression", o);
   e_add_sswstr(5, 10, 0, AltP, 1, "Prompt on Replace ", o);
   e_add_pswstr(0, 33, 7, 6, AltD, 0, "ForwarD        ", o);
   e_add_pswstr(0, 33, 8, 0, AltB, fd->sw & 4 ? 1 : 0, "Backward       ", o);
   e_add_pswstr(1, 5, 13, 0, AltG, 0, "Global            ", o);
   e_add_pswstr(1, 5, 14, 0, AltS, fd->sw & 8 ? 1 : 0, "Selected Text     ", o);
   e_add_pswstr(2, 33, 13, 0, AltF, 0, "From Cursor    ", o);
   e_add_pswstr(2, 33, 14, 0, AltE, fd->sw & 2 ? 1 : 0, "Entire Scope   ", o);
   e_add_bttstr(10, 16, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(41, 16, -1, ESC, "Cancel", NULL, o);
   e_add_bttstr(22, 16, 7, AltA, "Change All", NULL, o);
   ret = e_opt_kst(o);
   if(ret != ESC)
   {  fd->sw = 1 + (o->pstr[0]->num << 2) + (o->pstr[1]->num << 3)
		+ (o->pstr[2]->num << 1) + (o->sstr[0]->num << 7)
		+ (o->sstr[1]->num << 6) + (o->sstr[2]->num << 5)
		+ (o->sstr[3]->num << 4);
      strcpy(fd->search, o->wstr[0]->txt);
      fd->sn = strlen(fd->search);
      strcpy(fd->replace, o->wstr[1]->txt);
      fd->rn = strlen(fd->replace);
      if((fd->sw & 2) != 0)
      {  if((fd->sw & 4) == 0)
	 {  b->b.x = (fd->sw & 8) == 0 ? 0 : s->ka.x;
	    b->b.y = (fd->sw & 8) == 0 ? 0 : s->ka.y;
	 }
	 else
	 {  b->b.x = (fd->sw & 8) == 0 ? b->bf[b->mxlines-1].len : s->ke.x;
	    b->b.y = (fd->sw & 8) == 0 ? b->mxlines-1 : s->ke.y;
	 }
      }
   }
   freeostr(o);
   if(ret == AltA)  while(e_rep_search(f) != 0);
   else if(ret != ESC) e_rep_search(f);
   return(0);
}

int e_grp_fl(f)
     FENSTER *f;
{
   FIND *fd = &(f->ed->fd);
   int ret;
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o) return(-1);
   o->xa = 7;  o->ya = 3;  o->xe = 63;  o->ye = 19;
   o->bgsw = 0;
   o->name = "Grep";
   o->crsw = AltO;
   e_add_txtstr(4, 4, "Options:", o);
   e_add_wrstr(4, 2, 18, 2, 35, 128, 0, AltT, "Text to Find:", fd->search, &f->ed->sdf, o);
   e_add_wrstr(4, 10, 17, 10, 36, 128, 0, AltF, "File:", fd->file, &f->ed->fdf, o);
   e_add_wrstr(4, 12, 17, 12, 36, 128, 0, AltD, "Directory:", fd->dirct, &f->ed->ddf, o);
   e_add_sswstr(5, 5, 0, AltC, fd->sw & 128 ? 1 : 0, "Case sensative    ", o);
   e_add_sswstr(5, 6, 0, AltW, fd->sw & 64 ? 1 : 0, "Whole words only  ", o);
   e_add_sswstr(5, 7, 0, AltR, fd->sw & 32 ? 1 : 0, "Regular expression", o);
   e_add_sswstr(5, 8, 0, AltS, 0, "Search Rekursiv   ", o);
   e_add_bttstr(16, 14, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(34, 14, -1, ESC, "Cancel", NULL, o);
   ret = e_opt_kst(o);
   if(ret != ESC)
   {  fd->sw = 1024 + (o->sstr[0]->num << 7) + (o->sstr[1]->num << 6)
		    + (o->sstr[2]->num << 5) + (o->sstr[3]->num << 9);
      strcpy(fd->search, o->wstr[0]->txt);
      fd->sn = strlen(fd->search);
      strcpy(fd->file, o->wstr[1]->txt);
      strcpy(fd->dirct, o->wstr[2]->txt);
   }
   freeostr(o);
   if(ret != ESC) ret = e_data_first(2, f->ed, fd->dirct);
   return(ret);
}

int e_fnd_fl(f)
     FENSTER *f;
{
   FIND *fd = &(f->ed->fd);
   int ret;
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o) return(-1);
   o->xa = 7;  o->ya = 3;  o->xe = 61;  o->ye = 14;
   o->bgsw = 0;
   o->name = "Find File";
   o->crsw = AltO;
   e_add_txtstr(4, 6, "Options:", o);
   e_add_wrstr(4, 2, 15, 2, 36, 128, 0, AltF, "File:", fd->file, &f->ed->fdf, o);
   e_add_wrstr(4, 4, 15, 4, 36, 128, 0, AltD, "Directory:", fd->dirct, &f->ed->ddf, o);
   e_add_sswstr(5, 7, 0, AltS, 1, "Search Rekursiv   ", o);
   e_add_bttstr(13, 9, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(33, 9, -1, ESC, "Cancel", NULL, o);
   ret = e_opt_kst(o);
   if(ret != ESC)
   {  fd->sw = (o->sstr[0]->num << 9);
      strcpy(fd->file, o->wstr[0]->txt);
      strcpy(fd->dirct, o->wstr[1]->txt);
   }
   freeostr(o);
   if(ret != ESC) ret = e_data_first(3, f->ed, fd->dirct);
   return(ret);
}

