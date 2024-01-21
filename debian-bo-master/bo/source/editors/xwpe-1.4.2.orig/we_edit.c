/* we_edit.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"
#include "makro.h"

#ifdef DOS
#include <alloc.h>
#endif
#ifdef UNIX
#include<sys/types.h> /*  included fuer digital station  */
#include<sys/stat.h>
#endif

#ifdef WEUNDO
int e_undo_sw = 0, e_redo_sw = 0;
#endif

char *e_make_postf();
int e_del_a_ind();
int e_tab_a_ind();
int e_help_next();

#ifdef PROG
BUFFER *e_p_m_buffer = NULL;
#ifdef DEBUGGER
BUFFER *e_p_w_buffer = NULL;
#endif
#endif

/*
		Oeffnen eines Edit-Fensters                            */

int e_edit(cn, filename)
     ECNT *cn;
     char *filename;
{
   extern char *e_hlp_str[];
   extern WOPT *eblst, *hblst, *mblst, *dblst;
   FILE *fp = NULL;
   FENSTER *f, *fo;
   char *ptmp;
   int ftype = 0, i, j, st = 0;
   if(cn->mxedt >= MAXEDT)
   {  e_error(e_msg[3], 0, cn->fb);
      return(-1);
   }
   for (j = 1; j <= MAXEDT; j++)
   {  for (i = 1; i <= cn->mxedt && cn->edt[i] != j; i++);
      if( i > cn->mxedt) break;
   }
   cn->curedt=j;
   (cn->mxedt)++;
   cn->edt[cn->mxedt]=j;
   
   if( (f = (FENSTER *) MALLOC(sizeof(FENSTER))) == NULL)
   e_error(e_msg[0], 1, cn->fb);
   
   f->fb = cn->fb;
   cn->f[cn->mxedt] = f;
   
   if( (f->b = (BUFFER *) MALLOC(sizeof(BUFFER))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   if( (f->s = (SCHIRM*) MALLOC(sizeof(SCHIRM))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   if( (f->b->bf = (STRING *) MALLOC(MAXLINES*sizeof(STRING))) == NULL)
   e_error(e_msg[0], 1, f->fb);
#ifdef PROG
   for(i = cn->mxedt-1; i > 0 && (!strcmp(cn->f[i]->datnam, "Messages")
			|| cn->f[i]->dtmd <= 'Z'
			|| !strcmp(cn->f[i]->datnam, "Watches")
			|| !strcmp(cn->f[i]->datnam, "Stack")); i--);
   for(j = cn->mxedt-1; j > 0 && !st; j--)
   if(!strcmp(cn->f[j]->datnam, "Stack")) st = 1;
#else
   for(i = cn->mxedt-1; i > 0 && cn->f[i]->dtmd <= 'Z'; i--);
#endif
#ifdef PROG
   if(e_we_sw & 2)
   {  if((e_we_sw & 8) || !strcmp(filename, "Messages")
			 || !strcmp(filename, "Watches"))
      {  f->a = e_s_pkt(0, 2*MAXSLNS/3 + 1);
	 f->e = e_s_pkt(MAXSCOL-1, MAXSLNS-2);
      }
      else if(!strcmp(filename, "Stack"))
      {  f->a = e_s_pkt(2*MAXSCOL/3, 1);
	 f->e = e_s_pkt(MAXSCOL-1, 2*MAXSLNS/3);
      }
      else
      {  if(i < 1)
	 {  f->a = e_s_pkt(0, 1);
	    f->e = e_s_pkt(st ? 2*MAXSCOL/3-1 : MAXSCOL-1, 2*MAXSLNS/3);
	 }
	 else
	 {  f->a = e_s_pkt(cn->f[i]->a.x+1, cn->f[i]->a.y+1);
	    f->e = e_s_pkt(st ? 2*MAXSCOL/3-1 : cn->f[i]->e.x, cn->f[i]->e.y);
	 }
      }
      
   }
   else
#endif
   {  if(i < 1)
      {  f->a = e_s_pkt(0, 1);
	 f->e = e_s_pkt(MAXSCOL-1, MAXSLNS-2);
      }
      else
      {  f->a = e_s_pkt(cn->f[i]->a.x+1,
					cn->f[i]->a.y+1);
	 f->e = e_s_pkt(cn->f[i]->e.x,
					cn->f[i]->e.y);
      }
   }
   if(f->e.x - f->a.x < 26) f->a.x = f->e.x - 26;
   if(f->e.y - f->a.y < 3) f->a.y = f->e.y - 3;
   f->fb = f->fb;
   f->winnum = cn->curedt;
   f->dtmd = cn->dtmd;
   f->ins = 1;
   f->save = 0;
   f->zoom = 0;
   f->ed = cn;
   f->pic = NULL;
   f->hlp_str = e_hlp_str[0];
   f->blst = eblst;
   f->nblst = 7;
   f->b->f = f;
   f->b->b = e_s_pkt(0, 0);
   f->b->cl = f->b->clsv = 0;
   f->b->mx = e_s_pkt(cn->maxcol, MAXLINES);
   f->b->mxlines = 0;
   f->b->fb = f->fb;
   f->b->cn = cn;
#ifdef SWAP
   f->b->sp = cn->sp;
   f->b->pos = cn->curedt;
#endif
#ifdef WEUNDO
   f->b->ud = NULL;
   f->b->rd = NULL;
#endif
   if(e_we_sw & 2) e_add_synt_tl(filename, f);
   else {  f->c_st = NULL;  f->c_sw = NULL;  }
   if((f->ed->edopt & 256) || ((f->ed->edopt & 128) && f->c_st)) f->flg = 1;
   else f->flg = 0;
   f->s->c = e_s_pkt(0, 0);
   f->s->ks = e_s_pkt(0, 0);
   f->s->ka = e_s_pkt(0, 0);
   f->s->ke = e_s_pkt(0, 0);
   f->s->fa = e_s_pkt(0, 0);
   f->s->fe = e_s_pkt(0, 0);
   f->s->fb = f->fb;
#ifdef DEBUGGER
   f->s->brp = MALLOC(sizeof(int));
   f->s->brp[0] = 0;
   f->s->da.y = -1;
#endif
   for(i = 0; i < 9; i++) f->s->pt[i] = e_s_pkt(-1, -1);
   if(cn->mxedt == 0)    /*  Clipboard  */
   {  cn->curedt=0;
      cn->edt[cn->mxedt]=0;
      if( (f->datnam = MALLOC(10*sizeof(char))) == NULL)
      e_error(e_msg[0], 1, f->fb);
      strcpy(f->datnam, BUFFER_NAME);
      f->dirct = cn->dirct;
#ifdef UNIX
      f->filemode = 0600;
#endif
      e_new_line(0,f->b);
      *(f->b->bf[0].s) = WR;
      *(f->b->bf[0].s+1) = '\0';
      f->b->bf[0].len = 0;
      f->b->bf[0].nrc = 1;
#ifdef SWAP
      f->b->pos = cn->curedt;
#endif
      return(0);
   }
   if(strcmp(filename,"") == 0)
   e_mkeddir(f, "Noname");
   else
   e_mkeddir(f, filename);
   if(strcmp(filename, "Help") == 0)
   {  ptmp = e_mkfilename(cn->libdrct, WE_HELP_ED);
      f->dtmd = 'h';
      f->ins = 8;
      f->hlp_str = e_hlp_str[15+MENOPT];
      f->nblst = 7;  f->blst = hblst;
      ftype = 1;
   }
   else
   ptmp = e_mkfilename(f->dirct, f->datnam);
#ifdef PROG
   if(e_we_sw & 2)
   {  if(!strcmp(filename, "Messages"))
      {  f->ins = 8;  f->hlp_str = e_hlp_str[3];
	 f->nblst = 8;  f->blst = mblst;
	 ftype = 2;
      }
      else if(!strcmp(filename, "Watches"))
      {  f->ins = 8;  f->hlp_str = e_hlp_str[1];  f->blst = dblst;
	 ftype = 3;
      }
      else if(!strcmp(filename, "Stack"))
      {  f->ins = 8;  f->hlp_str = e_hlp_str[2];  f->blst = dblst;
	 ftype = 4;
      }
   }
#endif
   if(ftype != 1) fp = fopen(ptmp, "rb");
   if(fp != NULL && access(ptmp, 2) != 0) f->ins = 8;
#ifdef UNIX
   if(fp != NULL)
   {  struct stat buf[1];
      stat(ptmp, buf);
      f->filemode = buf->st_mode;
   }
   else
   {  umask(i = umask(077));
      f->filemode = 0666 & ~i;
   }
#endif
   FREE(ptmp);
   
   if(fp != NULL && ftype != 1)
   {  e_readin(0, 0, fp, f->b, f->dtmd);
      if(fclose(fp) != 0) e_error(e_msg[9], 0, cn->fb);
      if(cn->dtmd == 'k' || cn->dtmd == 'm' || cn->dtmd == 'h')
      cn->dtmd = 'n';
#ifdef PROG
      if(e_we_sw & 2)
      {  if(e_we_sw & 8)
	 {  strcpy(f->datnam, "Messages");  e_we_sw &= ~8;  }
	 if(!strcmp(f->datnam, "Messages"))
	 {  e_make_error_list(f);   f->ins = 8;  }
      }
#endif
   }
   else
   {  e_new_line(0,f->b);
      *(f->b->bf[0].s) = WR;
      *(f->b->bf[0].s+1) = '\0';
      f->b->bf[0].len = 0;
      f->b->bf[0].nrc = 1;
   }
#ifdef PROG
   if(ftype == 2)
   {  if(e_p_m_buffer != NULL)
      {  e_close_buffer(f->b);  f->b = e_p_m_buffer;  f->b->f = f;  }
      else
      {  e_p_m_buffer = f->b;
	 FARFREE( f->b->bf[0].s );
	 f->b->mxlines = 0;
      }
   }
#ifdef DEBUGGER
   if(ftype == 3)
   {  if(e_p_w_buffer != NULL)
      {  e_close_buffer(f->b);  f->b = e_p_w_buffer;  f->b->f = f;  }
      else
      {  e_p_w_buffer = f->b;
	 e_ins_nchar(f->b, f->s, "No Watches", 0, 0, 10);
      }
   }
#endif
#endif
   sc_txt_1(f);
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
        Funktion fuer Tastatur Ausgabe                     */

int e_eingabe(e)
     ECNT *e;
{
   BUFFER *b = e->f[e->mxedt]->b;
   SCHIRM *s = e->f[e->mxedt]->s;
   FENSTER *f = e->f[e->mxedt];
   int ret, c = 0;
#ifdef UNIX
   unsigned char far cc;
#endif
   fk_cursor(1);
   while (c != ESC)
   {  if(e->mxedt < 1) c = e_men_lst(-1, f);
      else if(f->dtmd <= 'Z') return(0);
      else
      {
	 if(f->save > f->ed->maxchg) e_autosave(f);
#if  MOUSE
#ifdef UNIX
	 if( (c = e_getch()) < 0) cc = c = e_edt_mouse(c, f);
	 else cc = c;
#else
	 else if( (c = e_getch()) < 0) c = e_edt_mouse(c, f);
#endif
#else
#ifdef UNIX
	 else cc = c = e_getch();
#else
	 else c = e_getch();
#endif
#endif
      }
      if((c > 31 || (c == TAB && !(f->flg & 1))
		|| (f->ins > 1 && f->ins != 8)) && c < 255)
      {  if(f->ins == 8) continue;
	 if (f->ins == 0 || f->ins == 2)  e_put_char(c, b, s);
#ifdef UNIX
	 else  e_ins_nchar(b, s, &cc, b->b.x, b->b.y, 1);
#else
	 else  e_ins_nchar(b, s, (unsigned char far *)&c,
                                                        b->b.x, b->b.y, 1);
#endif
	 e_schirm(f, 1);
      }
      else if(c == DC)
      {  if(f->ins == 8)
	 {  if(f->dtmd == 'h') e_help_last(f);
	    continue;
	 }
	 if( b->b.y > 0 || b->b.x > 0)
	 {  if(b->bf[b->b.y].len == 0)
	    {  e_del_line(b->b.y, b, s);
	       b->b.y--;
	       b->b.x = b->bf[b->b.y].len;
	       if(*(b->bf[b->b.y].s+b->b.x) == '\0') b->b.x--;
	    }
	    else
	    {  if(b->b.x > 0) b->b.x--;
	       else
	       {  b->b.y--;
		  b->b.x = b->bf[b->b.y].len;
		  if(*(b->bf[b->b.y].s+b->b.x) == '\0') b->b.x--;
	       }
	       if(f->flg & 1) e_del_a_ind(b,  s);
	       else e_del_nchar(b, s, b->b.x, b->b.y, 1);
	    }
	    e_schirm(f, 1);
	 }
      }
      else if(c == ENTF || c == 4)
      {  if(f->ins == 8) {
#ifdef DEBUGGER
	    if(e_we_sw & 2) e_d_is_watch(c, f);
#endif
	    continue;  }
	 if( *(b->bf[b->b.y].s + b->b.x) != '\0'
		  && (b->b.y < b->mxlines-1
			|| *(b->bf[b->b.y].s + b->b.x) != WR))
	 {  e_del_nchar(b, s, b->b.x, b->b.y, 1);
	    e_schirm(f, 1);
	 }
      }
      else if(c == CR)
      {
#ifdef PROG
	 if(f->ins == 8)
	 {  if(f->dtmd == 'h') {  e_help_ret(f);  goto weiter;  }
	    if(e_we_sw & 2)  {  e_d_car_ret(f);  goto weiter;  }
	    else continue;
	 }
#else
	 if(f->ins == 8) continue;
#endif
	 e_car_ret(b, s);
	 e_schirm(f, 1);
      }
      else if(c == TAB)
      {  e_tab_a_ind(b, s);
	 e_schirm(f, 1);
      }
/*
          else if(c == TAB)
          {    if(f->ins == 8) continue;
               if (f->ins == 0 || f->ins == 2)  
                 for(c = f->ed->tabn - b->b.x % f->ed->tabn; c > 0 
                                              && b->b.x < b->mx.x; c--) 
              		{   e_put_char(' ', b, s);  b->b.x++;  }
               else  e_ins_nchar(b, s, f->ed->tabs, b->b.x, b->b.y, 
                                       f->ed->tabn - b->b.x % f->ed->tabn);
               e_schirm(b, s, f, 1);
          }
*/
      else
      {  ret = e_tst_cur(c, e);
	 if(ret != 0) ret = e_tst_fkt(c, e);
      }
      weiter:
      f = e->f[e->mxedt];
      if(e->mxedt > 0 && f->dtmd > 'Z')
      {  b = f->b;
         s = f->s;
         s->ks.x = b->b.x;  s->ks.y = b->b.y;
         e_cursor(f, 1);
	 if((c & 511) != CUP && (c & 511) != CDO) b->clsv = b->cl;
	 e_zlsplt(f);
      }
   }
   return(c);
}
/*
              Interpretation der Cursor Tasten
                     ( Primitiv Editor )                    */

int e_tst_cur(c, e)
     int c;
     ECNT *e;
{
   BUFFER *b = e->f[e->mxedt]->b;
   SCHIRM *s = e->f[e->mxedt]->s;
   FENSTER *f = e->f[e->mxedt];
   
   switch (c)
   {  case CtrlP:
      case CUP:
      case CUP+512:
	 if(b->b.y > 0) (b->b.y)--;
	 b->b.x = e_chr_sp(b->clsv, b, f);
	 break;
      case CtrlN:
      case CDO:
      case CDO+512:
	 if(b->b.y < b->mxlines - 1) (b->b.y)++;
	 b->b.x = e_chr_sp(b->clsv, b, f);
	 break;
      case CtrlB:
      case CLE:
      case CLE+512:
	 (b->b.x)--;
	 if(b->b.x < 0)
	 {  if(b->b.y > 0)
	    {  (b->b.y)--; b->b.x = b->bf[b->b.y].len;  }
	    else {  b->b.x = 0;  }
	 }
	 break;
      case CtrlF:
      case CRI:
      case CRI+512:
	 (b->b.x)++;
	 if(b->b.x > b->bf[b->b.y].len)
	 {  if(b->b.y < b->mxlines - 1)  {  (b->b.y)++; b->b.x = 0;  }
	    else {  b->b.x = b->bf[b->b.y].len;  }
	 }
	 break;
	 
      case CCLE:
      case CCLE+512:
	 if(b->b.x <= 0 && b->b.y > 0)
	 {  b->b.y--;
	    b->b.x = b->bf[b->b.y].len;
	 }
	 else if( b->b.x > 0 )
	 b->b.x = e_su_rblk(b->b.x - 1, b->bf[b->b.y].s);
	 break;
      case CCRI:
      case CCRI+512:
	 if(b->b.x >= b->bf[b->b.y].len && b->b.y < b->mxlines)
	 {  b->b.x = 0; b->b.y++;  }
	 else if( b->b.x < b->bf[b->b.y].len )
	 b->b.x = e_su_lblk(b->b.x, b->bf[b->b.y].s);
	 break;
      case BDO:
      case BDO+512:
	 b->b.y = b->b.y + f->e.y - f->a.y - 2;
	 if(b->b.y > b->mxlines - 1) b->b.y = b->mxlines - 1;
	 e_schirm(f, 1);
	 e_cursor(f, 1);
	 break;
      case BUP:
      case BUP+512:
	 b->b.y = b->b.y - f->e.y + f->a.y + 2;
	 if(b->b.y < 0) b->b.y = 0;
	 e_schirm(f, 1);
	 e_cursor(f, 1);
	 break;
      case CBDO:
      case CBDO+512:
	 b->b.y = b->mxlines - 1;
	 b->b.x = b->bf[b->mxlines - 1].len;
	 e_schirm(f, 1);
	 break;
      case CBUP:
      case CBUP+512:
	 if(b->b.y != 0)
	 {  b->b.x = 0;
	    b->b.y = 0;
	    e_schirm(f, 1);  }
	 break;
      case CEND:
      case CEND+512:
	 if(f->e.y - f->a.y + s->c.y - 1 < b->mxlines)
	 b->b.y = f->e.y - f->a.y + s->c.y - 2;
	 else
	 b->b.y = b->mxlines - 1;
	 b->b.x = b->bf[b->b.y].len;
	 break;
      case CPS1:
      case CPS1+512:
	 b->b.x = 0;
	 b->b.y = s->c.y;
	 break;
      case AltI:
      case EINFG:
	 if (f->ins == 8) {
#ifdef DEBUGGER
	    if(e_we_sw & 2) e_d_is_watch(c, f);
#endif
	    break;  }
	 if(f->ins & 1) f->ins &= ~1;
	 else f->ins |= 1;
#ifdef NEWSTYLE
	 e_ed_rahmen(f, 1);
#else
	 e_pr_filetype(f);
#endif
	 break;
      case AltJ:
	 if (f->ins == 8) break;
	 if(f->ins & 2) f->ins &= ~2;
	 else f->ins |= 2;
#ifdef NEWSTYLE
	 e_ed_rahmen(f, 1);
#else
	 e_pr_filetype(f);
#endif
	 break;
      case CtrlA:
      case POS1:
      case POS1+512:
	 b->b.x = 0;
	 break;
      case CtrlE:
      case ENDE:
      case ENDE+512:
	 b->b.x = b->bf[b->b.y].len;
	 break;
      case CtrlT:
	 if(f->ins == 8) break;
	 if( b->b.x < b->bf[b->b.y].len )
	 {  c = e_su_lblk(b->b.x, b->bf[b->b.y].s);
	    e_del_nchar(b, s, b->b.x, b->b.y, c-b->b.x);
	 }
	 else if(*(b->bf[b->b.y].s+b->b.x) == WR)
	 e_del_nchar(b, s, b->b.x, b->b.y, 1);
	 else if(b->b.x >= b->bf[b->b.y].len && b->b.y < b->mxlines)
	 {  b->b.x = 0; (b->b.y)++;  }
	 e_schirm(f, 1);
	 break;
      case CtrlZ:
	 if(f->ins == 8) break;
	 e_del_nchar(b, s, b->b.x, b->b.y, b->bf[b->b.y].len-b->b.x);
	 e_schirm(f, 1);
	 break;
      case DGZ:
	 if(f->ins == 8) break;
	 e_del_line(b->b.y, b, s);
	 if(b->b.y > b->mxlines - 1) (b->b.y)--;
	 e_schirm(f, 1);
	 break;
      case AF7:
      case AltV:
	 if(f->dtmd != 'h' || f->ins != 8) return(c);
         e_help_next(f, 0);
	 break;
      case AF8:
      case AltT:
	 if(f->dtmd != 'h' || f->ins != 8) return(c);
         e_help_next(f, 1);
	 break;
      default:
	 return(c);
   }
   if(c >= 512 )
   {  if(s->ks.y == s->ka.y && s->ks.x == s->ka.x &&
		(s->ka.y != s->ke.y || s->ka.x != s->ke.x))
      {  s->ka.x = b->b.x;  s->ka.y = b->b.y;  }
      else if(s->ks.y == s->ke.y && s->ks.x == s->ke.x &&
		(s->ka.y != s->ke.y || s->ka.x != s->ke.x))
      {  s->ke.x = b->b.x;  s->ke.y = b->b.y;  }
      else if(s->ks.y < b->b.y || ( s->ks.y == b->b.y && s->ks.x < b->b.x))
      {  s->ka.x = s->ks.x;  s->ka.y = s->ks.y;
	 s->ke.x = b->b.x;  s->ke.y = b->b.y;
      }
      else
      {  s->ke.x = s->ks.x;  s->ke.y = s->ks.y;
	 s->ka.x = b->b.x;  s->ka.y = b->b.y;
      }
      e_schirm(f, 1);
   }
   return(0);
}
/*
              Interpretation der Funktions Tasten nur Editor          */

int e_tst_fkt(c, e)
     int c;
     ECNT *e;
{
   extern OPT opt[];
   int i;
   BUFFER *b = e->f[e->mxedt]->b;
   SCHIRM *s = e->f[e->mxedt]->s;
   FENSTER *f = e->f[e->mxedt];
#ifdef PROG
   if(e_tst_dfkt(f, c) == 0 ||
		((e_we_sw & 2) && e_prog_switch(f, c) == 0))
#else
   if(e_tst_dfkt(f, c) == 0)
#endif
   {  b = e->f[e->mxedt]->b;
      s = e->f[e->mxedt]->s;
      f = e->f[e->mxedt];
      fk_cursor(1);
      if(e->mxedt > 0) e_cursor(f, 1);
      return(0);
   }
   
   for(i = 0; i < MENOPT; i++)
   if(c == opt[i].as) e_men_lst(i, f);
   
   switch (c)
   {  case CtrlK:
#ifdef DOS
	 ctrlbrk(e_ctrl_break);
#endif
	 e_ctrl_k(f);    /*  ctrl k  */
	 break;
#ifdef UNIX
      case CtrlO:      /*  ctrl o  */
#else
      case CtrlQ:      /*  ctrl q  */
#endif
	 e_ctrl_q(f);
	 break;
      case AltG:
	 e_goto_line(f);
	 break;
#ifndef UNIX
      case CF10:
#endif
      case CtrlW:
	 e_show_clipboard(f);
	 break;
      case CtrlDel:
#ifdef WEUNDO
	 e_blck_del(f);
	 break;
#ifndef DOS
      case UNDO:
#endif
      case AltBS:
      case CtrlU:
	 e_make_undo(f);
	 break;
#ifndef DOS
      case AGAIN:
#endif
      case SABS:
      case CtrlR:
	 e_make_redo(f);
	 break;
#endif
#ifndef DOS
      case CUT:
#endif
      case ShiftDel:    /*  shift Del  :  Delete to Clipboard  */
/*               case 402:     */
      case CtrlX:
	 e_edt_del(f);
	 break;
#ifndef DOS
      case PASTE:
#endif
      case ShiftEin:    /*  shift Einf  */
      case CtrlV:
	 e_edt_einf(f);
	 break;
#if defined(XWINDOW) && !defined(DJGPP)
      case AltEin:
	 e_copy_X_buffer(f);
	 break;
      case AltDel:
	 e_paste_X_buffer(f);
	 break;
#endif
#ifndef DOS
      case COPY:
#endif
      case CtrlEin:    /*  ctrl Einf  */
/*               case 401:    */
      case CtrlC:
	 e_edt_copy(f);
	 break;
      default:
	 if(f->ed->edopt & 1)
	 {  switch(c)
	    {  case AF2:
		  e_m_save(f);
		  break;
#ifndef DOS
	       case FID:
#endif
	       case AF3:
		  e_find(f);
		  break;
	       case SF3:
		  e_replace(f);
		  break;
	       case F3:
		  e_rep_search(f);
		  break;
	       default:
		  return(c);
	    }
	 }
	 else
	 {  switch(c)
	    {  case F2:
		  e_m_save(f);
		  break;
#ifndef DOS
	       case FID:
#endif
	       case F4:
		  e_find(f);
		  break;
	       case AF4:
		  e_replace(f);
		  break;
	       case CtrlL:
	       case CF4:
		  e_rep_search(f);
		  break;
	       default:
		  return(c);
	    }
	 }
   }
   fk_cursor(1);
   return(0);
}

int e_ctrl_k(f)
     FENSTER *f;
{
   BUFFER *b = f->ed->f[f->ed->mxedt]->b;
   SCHIRM *s = f->ed->f[f->ed->mxedt]->s;
   int c;
   c = e_toupper(e_getch());
   if( c < 32 ) c = c + 'A' - 1;
   switch(c)
   {  case 'A':
	 b->b = s->ka;
	 e_schirm(f, 1);
	 break;
      case 'B':
	 s->ka = e_s_pkt(b->b.x, b->b.y);
	 e_schirm(f, 1);
	 break;
      case 'C':
	 e_blck_copy(f);
	 break;
      case 'F':
	 e_mk_beauty(1, 3, f);
	 break;
      case 'H':
	 e_blck_hide(f);
	 break;
      case 'I':
	 e_blck_to_right(f);
	 break;
      case 'K':
	 s->ke = e_s_pkt(b->b.x, b->b.y);
	 e_schirm(f, 1);
	 break;
      case 'L':
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
	 break;
      case 'R':
	 e_blck_read(f);
	 break;
      case 'U':
	 e_blck_to_left(f);
	 break;
      case 'V':
	 e_blck_versch(f);
	 break;
      case 'W':
	 e_blck_write(f);
	 break;
      case 'X':
	 s->ka.x = 0;
	 s->ka.y = 0;
	 s->ke.y = b->mxlines-1;
	 s->ke.x = b->bf[b->mxlines-1].len;
	 e_schirm(f, 1);
	 break;
      case 'Y':
	 e_blck_del(f);
	 break;
      case 'Z':
	 b->b = s->ke;
	 e_schirm(f, 1);
	 break;
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
	 s->pt[c-'0'] = e_s_pkt(b->b.x, b->b.y);
	 s->fa.y = b->b.y;  s->fa.x = b->b.x;  s->fe.x = b->b.x+1;
	 e_schirm(f, 1);
	 break;
   }
   return(0);
}

/*   Ctrl - Q - Verteiler     */

int e_ctrl_q(f)
     FENSTER *f;
{
   BUFFER *b = f->ed->f[f->ed->mxedt]->b;
   SCHIRM *s = f->ed->f[f->ed->mxedt]->s;
   int i, c;
   unsigned char cc;
   c = e_toupper(e_getch());
   if( c < 32 ) c = c + 'A' - 1;
   switch(c)
   {  case 'Y':     /*  loescht bis Ende der Zeile    */
	 if(f->ins == 8) break;
	 e_del_nchar(b, s, b->b.x, b->b.y, b->bf[b->b.y].len-b->b.x);
	 e_schirm(f, 1);
	 e_cursor(f, 1);
	 break;
      case 'T':     /*  loescht bis Anfang des Worts    */
	 if(f->ins == 8) break;
	 if(b->b.x <= 0 && b->b.y > 0)
	 {  b->b.y--;
	    b->b.x = b->bf[b->b.y].len;
	 }
	 else if( b->b.x > 0 )
	 {  c = b->b.x;
	    b->b.x = e_su_rblk(b->b.x - 1, b->bf[b->b.y].s);
	    e_del_nchar(b, s, b->b.x, b->b.y, c-b->b.x);
	 }
	 e_schirm(f, 1);
	 break;
      case 'F':     /*  Zeichenkette suchen    */
	 e_find(f);
	 break;
      case 'A':     /*  Zeichenkette ersetzen    */
	 e_replace(f);
	 break;
      case 'S':     /*  Definition suchen    */
	 e_sh_def(f);
	 break;
      case 'N':     /*  Naechste Definition suchen  */
	 e_sh_nxt_def(f);
	 break;
      case 'K':     /*  Naechste Klammer suchen  */
	 e_nxt_brk(f);
	 break;
      case 'B':     /*  Text verschoenern  */
	 e_p_beautify(f);
	 break;
      case 'U':     /*   fuer hilfe file: Button erzeugen  */
	 if(s->ka.y == s->ke.y && s->ka.y >= 0 && s->ka.x < s->ke.x)
	 {  cc = HED;
	    e_ins_nchar(b, s, &cc, s->ke.x, s->ke.y, 1);
	    cc = HBG;
	    e_ins_nchar(b, s, &cc, s->ka.x, s->ke.y, 1);
	    e_schirm(f, 1);
	 }
	 break;
      case 'M':     /*   fuer hilfe file: Mark-Line  */
	 if(s->ka.y == s->ke.y && s->ka.y >= 0 && s->ka.x < s->ke.x)
	 {  cc = HED;
	    e_ins_nchar(b, s, &cc, s->ke.x, s->ke.y, 1);
	    cc = HBB;
	    e_ins_nchar(b, s, &cc, s->ka.x, s->ke.y, 1);
	    e_schirm(f, 1);
	 }
	 break;
      case 'H':     /*   fuer hilfe file: Header erzeugen  */
	 if(s->ka.y == s->ke.y && s->ka.y >= 0 && s->ka.x < s->ke.x)
	 {  cc = HED;
	    e_ins_nchar(b, s, &cc, s->ke.x, s->ke.y, 1);
	    cc = HHD;
	    e_ins_nchar(b, s, &cc, s->ka.x, s->ke.y, 1);
	    e_schirm(f, 1);
	 }
	 break;
      case 'E':     /*   fuer hilfe file: Ende-Marke erzeugen  */
	 e_new_line(b->b.y, b);
	 cc = HFE;
	 e_ins_nchar(b, s, &cc, 0, b->b.y, 1);
	 e_schirm(f, 1);
	 break;
      case 'L':     /*   fuer hilfe file: Hilfe-Sonderzeichen loeschen  */
	 for(i = 0; i < b->bf[b->b.y].len; i++)
	 if(b->bf[b->b.y].s[i] == HBG || b->bf[b->b.y].s[i] == HED
		   || b->bf[b->b.y].s[i] == HHD || b->bf[b->b.y].s[i] == HBB
		   || b->bf[b->b.y].s[i] == HFE || b->bf[b->b.y].s[i] == HFB
		   || b->bf[b->b.y].s[i] == HFE)
	 {  e_del_nchar(b, s, i, b->b.y, 1);  i--;  }
	 e_schirm(f, 1);
	 break;
      case 'C':     /*   fuer hilfe file: Hilfe-File ueberpruefen  */
	 e_help_comp(f);
	 break;
#ifdef PROGTOOLS   /*   Programmierhilfen (bisher nur xwpe-Entwicklung)  */
      case 'O':
	 e_make_opt_kst(f);
	 break;
      case 'R':
	 e_read_opt_kst(f);
	 break;
      case 'W':
	 e_write_opt_kst(f);
	 break;
#endif
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
	 b->b.x = s->pt[c-'0'].x;  b->b.y = s->pt[c-'0'].y;
	 s->fa.y = b->b.y;  s->fa.x = b->b.x;  s->fe.x = b->b.x+1;
	 e_cursor(f, 1);
	 break;
   }
   return(0);
}

/*   Funktionstasten - Verteiler (allgemein)  */

int e_tst_dfkt(f, c)
     FENSTER *f;
     int c;
{
   if(c >= Alt1 && c <= Alt9)
   {  e_switch_window(c - Alt1 + 1, f); return(0);  }
   if(c >= 1024 && c <= 1049)
   {  e_switch_window(c - 1014, f); return(0);  }
   switch(c)
   {  case F1:
#ifndef DOS
      case HELP:
#endif
	 e_help_loc(f, 0);
	 break;
#if defined(PROG) && !defined(DJGPP)
      case AF1:
	 if(e_we_sw & 2) e_funct(f);
	 break;
      case CF1:
	 if(e_we_sw & 2) e_funct_in(f);
	 break;
#endif
      case ESC:
      case F10:
      case AltBl:
	 e_men_lst(-1, f);
	 break;
      case Alt0:
	 e_list_all_win(f);
	 break;
      case AF5:
         e_deb_out(f);
         break;
      default:
	 if(f->ed->edopt & 1)
	 {  switch(c)
	    {  case CtrlL:
		  e_size_move(f);
		  break;
#ifndef DOS
	       case OPEN:
#endif
	       case F2:
		  e_manager(f);
		  break;
	       case SF2:
		  e_manager_first(f);
		  break;
	       case CF4:
	       case AltX:
		  e_close_window(f);
		  break;
#ifndef DOS
	       case FRONT:
#endif
	       case AltZ:
	       case SF4:
		  e_ed_cascade(f);
		  break;
	       case SF5:
		  e_ed_tile(f);
		  break;
	       case SF6:
		  e_ed_zoom(f);
		  break;
	       case CF6:
	       case AltN:
		  e_ed_next(f);
		  break;
#ifndef DOS
	       case STOP:
#endif
	       case AF4:
		  e_quit(f);
		  break;
	       default:
		  return(c);
	    }
	 }
	 else
	 {  switch(c)
	    {  case CF5:
	       case AF2:
		  e_size_move(f);
		  break;
#ifndef DOS
	       case OPEN:
#endif
	       case F3:
		  e_manager(f);
		  break;
	       case CF3:
		  e_manager_first(f);
		  break;
	       case AF3:
		  e_close_window(f);
		  break;
#ifndef DOS
	       case FRONT:
#endif
	       case AltZ:
	       case F5:
		  e_ed_zoom(f);
		  break;
	       case F6:
	       case AltN:
		  e_ed_next(f);
		  break;
#ifndef DOS
	       case STOP:
#endif
	       case AltX:
		  e_quit(f);
		  break;
	       default:
		  return(c);
	    }
	 }
	 
   }
   return(0);
}

int e_chr_sp(x, b, f)
     int x;
     BUFFER *b;
     FENSTER *f;
{
   int i, j;
   for(i = j = 0; i + j < x && i < b->bf[b->b.y].len; i++)
   {  if(*(b->bf[b->b.y].s + i) == TAB)
      j += (f->ed->tabn - ((j + i) % f->ed->tabn) - 1);
#ifdef UNIX
      else if(!(e_we_sw & 1) && ((unsigned char) *(b->bf[b->b.y].s + i)) > 126)
      {  j++;
	 if(((unsigned char) *(b->bf[b->b.y].s + i)) < 128 + ' ') j++;
      }
      else if(*(b->bf[b->b.y].s + i) < ' ') j++;
      if(f->dtmd == 'h')
      {  if(b->bf[b->b.y].s[i] == HBG || b->bf[b->b.y].s[i] == HFB
		|| b->bf[b->b.y].s[i] == HED || b->bf[b->b.y].s[i] == HHD
		|| b->bf[b->b.y].s[i] == HFE || b->bf[b->b.y].s[i] == HBB
                || b->bf[b->b.y].s[i] == HNF)
	 j -= 2;
      }
#endif
   }
   return(i);
}

/*   Auto-Indent  */

int e_tab_a_ind(b, s)
     BUFFER *b;
     SCHIRM *s;
{
   extern int b_dif;
   int i, j = -1, k;
   char *str;
   for(i = 0; i < b->bf[b->b.y].len && i < b->b.x
	&& (isspace(b->bf[b->b.y].s[i]) || b->bf[b->b.y].s[i] == '{'); i++);
   if(i == b->b.x && b->b.y > 0)
   {  for(j = 0, i = 0; j < b->b.x; j++)
      i = b->bf[b->b.y].s[j] == '\t' ?
		(i / b->cn->tabn + 1) * b->cn->tabn : i + 1;
      if(i != j)
      {  str = MALLOC(i * sizeof(char));
	 for(k = 0; k < i; k++) str[k] = ' ';
	 for(j = 0, i = 0; j < b->b.x; j++)
	 {  i = b->bf[b->b.y].s[j] == '\t' ?
		(i / b->cn->tabn + 1) * b->cn->tabn : i + 1;
	    if(b->bf[b->b.y].s[j] == '{') str[i-1] = '{';
   	 }
	 e_del_nchar(b, s, 0, b->b.y, j);
	 e_ins_nchar(b, s, str, 0, b->b.y, i);
	 FREE(str);
      }
      j = b->b.y;
      do
      {  j--;
	 for(i = 0, k = 0; k < b->bf[j].len
	    && (isspace(b->bf[j].s[k]) || b->bf[j].s[i] == '{'); k++)
	 i = b->bf[j].s[k] == '\t' ?
		 (i / b->cn->tabn + 1) * b->cn->tabn : i + 1;
      }
      while(j > 0 && b->bf[j].s[k] == '#');
      if(k == b->bf[j].len && k > 0 && b->bf[j].s[k-1] == '{')
      {  i = i - 1 + b_dif > b->b.x ? i + b_dif - b->b.x - 1 : -1;  }
      if(i <= b->b.x)
      {  i = i + b_dif > b->b.x ? i + b_dif - b->b.x : -1;  }
      else i -= b->b.x;
   }
   else i = -1;
   if(i > 0)
   {  str = MALLOC(i * sizeof(char));
      for(j = 0; j < i; j++) str[j] = ' ';
      e_ins_nchar(b, s, str, b->b.x, b->b.y, i);
      FREE(str);
   }
   else if(i < 0)
   {  str = MALLOC(sizeof(char));
      str[0] = '\t';
      e_ins_nchar(b, s, str, b->b.x, b->b.y, 1);
      FREE(str);
   }
   return(b->b.x);
}

int e_del_a_ind(b, s)
     BUFFER *b;
     SCHIRM *s;
{
   int i = 1, j = -1, k;
   if(b->b.y > 0)
   {  for(i = 0; i < b->bf[b->b.y].len && i < b->b.x
			&& isspace(b->bf[b->b.y].s[i]); i++);
      if(i == b->b.x)
      {  for(j = 0, i = 0; j <= b->b.x; j++)
	 i = b->bf[b->b.y].s[j] == '\t' ?
		(i / b->cn->tabn + 1) * b->cn->tabn : i + 1;
	 if(i != j)
	 {  char *str = MALLOC(i * sizeof(char));
	    e_del_nchar(b, s, 0, b->b.y, j);
	    for(j = 0; j < i; j++) str[j] = ' ';
	    e_ins_nchar(b, s, str, 0, b->b.y, i);
	    b->b.x = i - 1;
	    FREE(str);
	 }
	 for(j = b->b.y-1; j >= 0; j--)
	 {  for(i = 0, k = 0; k < b->bf[j].len && isspace(b->bf[j].s[k]); k++)
	    i = b->bf[j].s[k] == '\t' ?
		 (i / b->cn->tabn + 1) * b->cn->tabn : i + 1;
	    if(k < b->bf[j].len && b->bf[j].s[k] != '#' && i <= b->b.x)
	    {  i = b->b.x - i + 1;
	       b->b.x -= i - 1;
	       break;
	    }
	 }
	 if(j < 0) {  i = b->b.x;  b->b.x = 0;  }
      }
   }
   if(j < 0) i = 1;
   e_del_nchar(b, s, b->b.x, b->b.y, i);
   return(i);
}

int e_car_a_ind(b, s)
     BUFFER *b;
     SCHIRM *s;
{
   int i, j, k;
   unsigned char *str;
   
   if(b->b.y == 0) return(0);
   j = b->b.y;
   do
   {  j--;
      for(i = 0, k = 0; k < b->bf[j].len
	    && (isspace(b->bf[j].s[k]) || b->bf[j].s[i] == '{'); k++)
      i = b->bf[j].s[k] == '\t' ?
		 (i / b->cn->tabn + 1) * b->cn->tabn : i + 1;
   }
   while(j > 0 && b->bf[j].s[k] == '#');
   if(k == b->bf[j].len && k > 0 && b->bf[j].s[k-1] == '{') i--;
   if(i > 0)
   {  str = MALLOC(i * sizeof(char));
      for(j = 0; j < i; j++) str[j] = ' ';
      e_ins_nchar(b, s, str, 0, b->b.y, i);
      b->b.x = i;
      FREE(str);
   }
   return(i);
}

/*
       Schreiben von n Leerzeichen   */

int e_blk(anz, xa, ya, col)
     int anz;
     int xa;
     int ya;
     int col;
{
   for (anz--; anz >= 0 ; anz--)
   e_pr_char(xa+anz, ya, ' ', col);
   return(anz);
}
/*
       Carridge Return einfuegen     */

int e_car_ret(b, s)
     BUFFER *b;
     SCHIRM *s;
{
   int len, i;
   len = b->bf[b->b.y].len;
#ifdef WEUNDO
   e_add_undo('a', b, b->b.x, b->b.y, 1);
#endif
   (b->f->save)++;
   if(b->b.x != len || *(b->bf[b->b.y].s+len) != '\0')
   {  e_new_line(b->b.y+1, b);
      for(i=0; i <= len - b->b.x; i++)
              *(b->bf[b->b.y+1].s + i) = *(b->bf[b->b.y].s+b->b.x+i);
      *(b->bf[b->b.y+1].s+i)='\0';
      b->bf[b->b.y+1].len = e_str_len(b->bf[b->b.y+1].s);
      b->bf[b->b.y+1].nrc = e_str_nrc(b->bf[b->b.y+1].s);
      if(s->ka.y > b->b.y) (s->ka.y)++;
      else if(s->ka.y == b->b.y && s->ka.x > b->b.x)
      {  (s->ka.y)++;  (s->ka.x) -= (b->b.x);  }
      if(s->ke.y > b->b.y) (s->ke.y)++;
      else if(s->ke.y == b->b.y && s->ke.x > b->b.x)
      {  (s->ke.y)++;  (s->ke.x) -= (b->b.x);  }
   }
   *(b->bf[b->b.y].s+b->b.x) = WR;
   *(b->bf[b->b.y].s+b->b.x+1) = '\0';
   b->bf[b->b.y].len = e_str_len(b->bf[b->b.y].s);
   b->bf[b->b.y].nrc = e_str_nrc(b->bf[b->b.y].s);
   sc_txt_3(b->b.y, b, 1);
   if (b->b.y < b->mxlines - 1)
   {  (b->b.y)++;
      b->b.x = 0;
   }
   if(b->f->flg & 1) e_car_a_ind(b, s);
   return(b->b.y);
}

/*
        Cursor Positionierung                       */

void e_cursor(f, sw)
     FENSTER *f;
     int sw;
{
   BUFFER *b = f->b;
   SCHIRM *s = f->s;
   static int iold = 0, jold = 0;
   int i, j;
   if(f->dtmd <= 'Z') return;
   if(b->b.y < 0) b->b.y = 0;
   if(b->b.y > b->mxlines-1) b->b.y = b->mxlines-1;
   if(b->b.x < 0) b->b.x = 0;
#ifdef SWAP
   if(b->bf == NULL) e_swap_in_b(b);
   if(b->bf[b->b.y].s == NULL) e_swap_in(b->b.y, b);
#endif
   if(b->b.x > b->bf[b->b.y].len) b->b.x = b->bf[b->b.y].len;
   for(i = j = 0; i < b->b.x; i++)
   {  if(*(b->bf[b->b.y].s + i) == TAB)
      j += (f->ed->tabn - ((j + i) % f->ed->tabn) - 1);
#ifdef UNIX
      else if(!(e_we_sw & 1)
		&& ((unsigned char) *(b->bf[b->b.y].s + i)) > 126)
      {  j++;
	 if(((unsigned char) *(b->bf[b->b.y].s + i)) < 128 + ' ') j++;
      }
      else if(*(b->bf[b->b.y].s + i) < ' ') j++;
      if(f->dtmd == 'h')
      {  if(b->bf[b->b.y].s[i] == HBG || b->bf[b->b.y].s[i] == HED
		|| b->bf[b->b.y].s[i] == HHD || b->bf[b->b.y].s[i] == HFE
		|| b->bf[b->b.y].s[i] == HFB || b->bf[b->b.y].s[i] == HBB
                || b->bf[b->b.y].s[i] == HNF)
	 j -= 2;
      }
#endif
   }
   if( b->b.y - s->c.y < 0 || b->b.y - s->c.y >= f->e.y - f->a.y - 1
         || s->c.y < 0 || s->c.y >= b->mxlines
         || b->b.x + j - s->c.x < 0
	 || b->b.x + j- s->c.x >= f->e.x - f->a.x - 1 )
   {
#if defined(UNIX) && !defined(DJGPP)
/*
     if(b->b.y - s->c.y < 0) s->c.y = e_we_sw & 1 ? b->b.y :
					b->b.y - (f->e.y - f->a.y)/2;
     else if(b->b.y - s->c.y >= f->e.y - f->a.y -1)
        (s->c.y) = e_we_sw & 1 ? b->b.y - f->e.y + f->a.y + 2 : 
					    b->b.y - (f->e.y - f->a.y)/2;
*/
      if(b->b.y - s->c.y < 0) s->c.y = b->b.y - (f->e.y - f->a.y)/2;
      else if(b->b.y - s->c.y >= f->e.y - f->a.y -1)
      s->c.y = b->b.y - (f->e.y - f->a.y)/2;
#else
      if(b->b.y - s->c.y < 0) s->c.y = b->b.y;
      else if(b->b.y - s->c.y >= f->e.y - f->a.y -1)
      (s->c.y) = b->b.y - f->e.y + f->a.y + 2 ;
#endif
      if(s->c.y >= b->mxlines - 1) s->c.y = b->mxlines - 2 ;
      if(s->c.y < 0) s->c.y = 0 ;
      
#ifdef UNIX
      if(b->b.x + j - s->c.x < 0)
      (s->c.x) = b->b.x + j - (f->e.x - f->a.x)/2 ;
      else if(b->b.x + j - s->c.x >= f->e.x - f->a.x - 1)
      (s->c.x) = b->b.x + j - (f->e.x - f->a.x)/2 ;
#else
      if(b->b.x + j - s->c.x < 0) (s->c.x) = b->b.x - 2 ;
      else if(b->b.x + j - s->c.x >= f->e.x - f->a.x - 1)
      (s->c.x) = b->b.x + j - f->e.x + f->a.x+ 2 ;
#endif
      if(s->c.x < 0) s->c.x = 0 ;
      else if(s->c.x >= b->bf[b->b.y].len + j) s->c.x = b->bf[b->b.y].len + j;
      e_schirm(f, sw);
   }
   if(s->fa.y == -1) {  e_schirm(f, sw);  s->fa.y--;  }
   else if(s->fa.y > -1) s->fa.y = -1;
   if(sw != 0)
   {  iold = e_lst_zeichen(f->e.x, f->a.y+1, f->e.y-f->a.y-1, 0,
                             f->fb->em.fb, b->mxlines, iold, b->b.y);
      jold = e_lst_zeichen(f->a.x+18, f->e.y, f->e.x-f->a.x-19, 1,
                             f->fb->em.fb, b->mx.x, jold, b->b.x);
   }
   b->cl = b->b.x+j;
   fk_locate(f->a.x+b->b.x-s->c.x+j+1, f->a.y+b->b.y-s->c.y+1);
}
/*
    Eine Zeile l?schen   */

int e_del_line(yd, b, s)
     int yd;
     BUFFER *b;
     SCHIRM *s;
{
   int i;
   if(b->mxlines == 1)
   {  *(b->bf[0].s) = '\0';
      b->bf[0].nrc = b->bf[0].len = 0;
   }
   else
#ifdef WEUNDO
   {  e_add_undo('l', b, 0, yd, 1);
#else
   {  FARFREE(b->bf[yd].s);
#endif
      (b->mxlines)--;
      for(i = yd; i < b->mxlines; i++)
         b->bf[i] = b->bf[i+1];
      if(s->ka.y > yd) (s->ka.y)--;
      else if(s->ka.y == yd)  (s->ka.x) = 0;
      if(s->ke.y > yd) (s->ke.y)--;
      else if(s->ke.y == yd)
      {  (s->ke.y)--;  s->ke.x = b->bf[yd-1].len;  }
      
   }
   sc_txt_3(yd, b, -1);
   if(b->f) (b->f->save) += 10;
   return(b->mxlines);
}
/*
       N - Zeichen aus Buffer l?schen          */

int e_del_nchar(b, s, x, y, n)
     BUFFER *b;
     SCHIRM *s;
     int x;
     int y;
     int n;
{
   extern struct EXT h_error;
   FENSTER *f = h_error.cn->f[h_error.cn->mxedt];
   int len, i, j;
   (f->save) += n;
#ifdef WEUNDO
   e_add_undo('r', b, x, y, n);
   e_undo_sw++;
   if(f->dtmd == 'b')
   {  i = e_del_nbchar(b, s, x, y, n);
      e_undo_sw--;
      return(i);
   }
#else
   if(f->dtmd == 'b') return(e_del_nbchar(b, s, x, y, n));
#endif
   len = b->bf[y].len;
   if( *(b->bf[y].s+len) == WR) len++;
   for (j = x; j <= len - n; ++j) *(b->bf[y].s+j) = *(b->bf[y].s+j+n);
   if(s->ka.y == y && s->ka.x > x)
   s->ka.x = (s->ka.x-n > x) ? s->ka.x-n : x;
   if(s->ke.y == y && s->ke.x > x)
   s->ke.x = (s->ke.x-n > x) ? s->ke.x-n : x;
   
   if(len <= n) e_del_line(y, b, s);
   else if(y < b->mxlines - 1 && *(b->bf[y].s+len-n-1) != WR )
   {  if(b->mx.x+n-len > b->bf[y+1].len)  i = b->bf[y+1].len;
      else  i = e_su_rblk(b->mx.x+n-len, b->bf[y+1].s);
      if(b->bf[y+1].s[i] == WR) i++;
      if(i > 0)
      {  for(j = 0; j < i; ++j) *(b->bf[y].s+len-n+j) = *(b->bf[y+1].s+j);
	 *(b->bf[y].s+len-n+i) = '\0';
	 if(s->ka.y == y+1 && s->ka.x <= i)
	 {  s->ka.y--;  s->ka.x += (len-n);  }
	 if(s->ke.y == y+1 && s->ke.x <= i)
	 {  s->ke.y--;  s->ke.x += (len-n);  }
	 e_del_nchar(b, s, 0, y+1, i);
      }
   }
   if(y < b->mxlines)
   {  b->bf[y].len = e_str_len(b->bf[y].s);
      b->bf[y].nrc = e_str_nrc(b->bf[y].s);
   }
#ifdef WEUNDO
   e_undo_sw--;
#endif
   sc_txt_4(y, b, 0);
   return(x+n);
}
/*
        N - Zeichen in Buffer einfuegen   */

int e_ins_nchar(b, sch, s, xa, ya, n)
     BUFFER *b;
     SCHIRM *sch;
     unsigned char *s;
     int xa;
     int ya;
     int n;
{
   extern struct EXT h_error;
   FENSTER *f = h_error.cn->f[h_error.cn->mxedt];
   int i, j;
   (f->save) += n;
#ifdef WEUNDO
   e_add_undo('a', b, xa, ya, n);
   e_undo_sw++;
   if(f->dtmd == 'b')
   {  i = e_ins_nbchar(b, sch, (char far *)s, xa, ya, n);
      e_undo_sw--;
      return(i);
   }
#else
   if(f->dtmd == 'b') return(e_ins_nbchar(b, sch, (char far *)s, xa, ya, n));
#endif
   if(b->bf[ya].len+n >= b->mx.x-1)
   {  if(xa < b->bf[ya].len)
      {  i=b->mx.x-n-1;
	 if(i >= b->bf[ya].len-1)  i = b->bf[ya].len-1;
      }
      else
      {  for(; xa < b->mx.x-1; xa++, s++, n--)
	 {  *(b->bf[ya].s + xa + 1) = *(b->bf[ya].s + xa);
	    *(b->bf[ya].s + xa) = *s;
	 }
	 *(b->bf[ya].s + xa + 1) = '\0';
	 i = b->mx.x;
	 b->bf[ya].len = e_str_len(b->bf[ya].s);
	 b->bf[ya].nrc = e_str_nrc(b->bf[ya].s);
      }
      for ( ; i > 0 && *(b->bf[ya].s+i) != ' '
                                    && *(b->bf[ya].s+i) != '-'; i--);
      if(i == 0)
      {  if(s[n-1] != ' ' && s[n-1] != '-')  i = b->bf[ya].len - 2;
	 else  i--;
      }
      if(*(b->bf[ya].s+b->bf[ya].len) == WR || ya == b->mxlines)
      {  e_new_line(ya+1, b);
	 if(sch->ka.y > ya) (sch->ka.y)++;
	 else if(sch->ka.y == ya)
	 {  if(sch->ka.x > i) {  (sch->ka.y)++;  (sch->ka.x) -= (i+1);  }
	    else if(sch->ka.x >= xa) sch->ka.x += n;
	 }
	 if(sch->ke.y > ya) (sch->ke.y)++;
	 else if(sch->ke.y == ya)
	 {  if(sch->ke.x > i) {  (sch->ke.y)++;  (sch->ke.x) -= (i+1);  }
	    else if(sch->ke.x >= xa) sch->ke.x += n;
	 }
	 for (j = i+1; *(b->bf[ya].s+j) != WR && *(b->bf[ya].s+j) != '\0'; j++)
                        *(b->bf[ya+1].s+j-i-1) = *(b->bf[ya].s+j);
	 *(b->bf[ya+1].s+j-i-1) = WR;
	 b->bf[ya+1].len = e_str_len(b->bf[ya+1].s);
	 b->bf[ya+1].nrc = e_str_nrc(b->bf[ya+1].s);
	 sc_txt_4(ya, b, 1);
      }
      else
      { /*  *(b->bf[ya].s+b->bf[ya].len) = ' ';  */
	 e_ins_nchar(b, sch, b->bf[ya].s+i+1, 0, ya+1, b->bf[ya].len-i-1);
	 if(sch->ka.y == ya)
	 {  if(sch->ka.x > i) {  (sch->ka.y)++;  (sch->ka.x) -= (i+1);  }
	    else if(sch->ka.x >= xa) sch->ka.x += n;
	 }
	 if(sch->ke.y == ya)
	 {  if(sch->ke.x > i) {  (sch->ke.y)++;  (sch->ke.x) -= (i+1);  }
	    else if(sch->ke.x >= xa) sch->ke.x += n;
	 }
      }
/*	 if(*(b->bf[ya].s+i) == ' ') *(b->bf[ya].s+i) = '\0';
         else
*/
         *(b->bf[ya].s+i+1) = '\0';
      b->bf[ya].len = e_str_len(b->bf[ya].s);
      b->bf[ya].nrc = e_str_nrc(b->bf[ya].s);
      if( xa > b->bf[ya].len )
      {  xa -= (b->bf[ya].len);
	 ya++;
	 b->bf[ya].len = e_str_len(b->bf[ya].s);
	 b->bf[ya].nrc = e_str_nrc(b->bf[ya].s);
	 if(sch->ka.y == ya && sch->ka.x >= xa) sch->ka.x += n;
	 if(sch->ke.y == ya && sch->ke.x >= xa) sch->ke.x += n;
      }
   }
   else
   {  if(sch->ka.y == ya && sch->ka.x >= xa) sch->ka.x += n;
      if(sch->ke.y == ya && sch->ke.x >= xa) sch->ke.x += n;
   }
   for (j = b->bf[ya].len; j >= xa; --j)
               *(b->bf[ya].s+j+n) = *(b->bf[ya].s+j);
   for (j = 0; j < n; ++j)  *(b->bf[ya].s+xa+j) = *(s+j);
   if(b->bf[ya].s[b->bf[ya].len] == WR) b->bf[ya].s[b->bf[ya].len+1] = '\0';
   b->b.x = xa + n;
   b->b.y = ya;
   b->bf[ya].len = e_str_len(b->bf[ya].s);
   if(b->bf[ya].s[b->bf[ya].len] == WR) b->bf[ya].s[b->bf[ya].len+1] = '\0';
   b->bf[ya].nrc = e_str_nrc(b->bf[ya].s);
#ifdef WEUNDO
   e_undo_sw--;
#endif
   sc_txt_4(ya, b, 0);
   return(xa+n);
}
/*
       Neue Zeile einfuegen       */

int e_new_line(yd, b)
     int yd;
     BUFFER *b;
{
   int i;
   if(b->mxlines > b->mx.y-2)
   {  b->mx.y += MAXLINES;
      if((b->bf = REALLOC(b->bf, b->mx.y * sizeof(STRING))) == NULL)
      e_error(e_msg[0], 1, b->fb);
      if(b->f->c_sw) b->f->c_sw = REALLOC(b->f->c_sw ,
						b->mx.y * sizeof(int));
   }
   for(i = b->mxlines-1; i >= yd; i--)
   {  b->bf[i+1] = b->bf[i];  }
   (b->mxlines)++;
   b->bf[yd].s = FARMALLOC(b->mx.x+1);
   if(b->bf[yd].s == NULL) e_error(e_msg[0], 1, b->fb);
   *(b->bf[yd].s) = '\0';
   b->bf[yd].len = 0;
   b->bf[yd].nrc = 0;
   return(b->mxlines);
}

/*
     ?berschreiben eines Buchstabens       */

/*  include "edit.h"  */

int e_put_char(c, b, s)
     int c;
     BUFFER *b;
     SCHIRM *s;
{
#ifdef UNIX
   unsigned char far cc = c;
#endif
/*
   if( b->b.x == b->bf[b->b.y].len )
   {  if( b->b.y == b->mxlines-1)
#ifdef UNIX
      e_ins_nchar(b, s, &cc, b->b.x, b->b.y, 1);
#else
      e_ins_nchar(b, s, (unsigned char far *)&c, b->b.x, b->b.y, 1);
#endif
      else
      {  b->b.x = 0;
	 (b->b.y)++;
	 (b->f->save)++;
#ifdef WEUNDO
	 e_add_undo('p', b, b->b.x, b->b.y, 1);
#endif
              *(b->bf[b->b.y].s+b->b.x) = c;  }  }
*/
   if( b->b.x == b->bf[b->b.y].len )
#ifdef UNIX
      e_ins_nchar(b, s, &cc, b->b.x, b->b.y, 1);
#else
      e_ins_nchar(b, s, (unsigned char far *)&c, b->b.x, b->b.y, 1);
#endif
   else
   {
#ifdef WEUNDO
      e_add_undo('p', b, b->b.x, b->b.y, 1);
#endif
      (b->f->save)++;
      *(b->bf[b->b.y].s+b->b.x) = c;
   }
   (b->b.x)++;
   return(b->b.x);
}
/*
    Suchen nach rechts (linkes Wortende)     */

int e_su_lblk(xa, s)
     int xa;
     char *s;
{
   int len = strlen(s);
   if(xa >= len) xa = len - 1;
   for (; xa < len  && isalnum1(s[xa]); xa++);
   for(; xa < len && !isalnum1(s[xa]); xa++);
   return(xa);
}
/*
     Suchen nach links (linkes Wortende)     */

int e_su_rblk(xa, s)
     int xa;
     char *s;
{
   int len = strlen(s);
   if(xa <= 0) return(xa);
   if(xa > len) xa = len;
   for(xa--; xa > 0 && !isalnum1(s[xa]); xa--);
   if(!xa) return(xa);
   for (; xa > 0 && isalnum1(s[xa]); xa--);
   return(!xa ? xa : xa + 1);
}
/*
       Zeilen und Spaltenposition des Cursors schreiben        */

void e_zlsplt(f)
     FENSTER *f;
{
   char str[20];
   if(f->dtmd <= 'Z') return;
   sprintf(str,"%5d:%-4d", f->b->b.y + 1, f->b->cl + 1);
   e_puts(str,f->a.x+4,f->e.y,f->fb->er.fb);
/*
     sprintf(str,"%4d:", f->b->b.y + 1);
     e_puts(str,f->a.x+5,f->e.y,f->fb->er.fb);
     sprintf(str,"%-4d", f->b->b.x + 1);
     e_puts(str,f->a.x+10,f->e.y,f->fb->er.fb);
*/
#ifdef NEWSTYLE
   if(e_we_sw & 1) e_make_xrect(f->a.x+4, f->e.y, f->a.x+13, f->e.y, 0);
#endif
}

char *e_mkeddir(f, pth)
     FENSTER *f;
     char *pth;
{
   int i, n, len = strlen(pth);
   char tmp[120];
#if defined(DOS) || defined(DJGPP)
   for(n = len-1; n >= 0 && pth[n] != DIRC && pth[n] != '/' && pth[n] != ':'; n--);
#else
   for(n = len-1; n >= 0 && pth[n] != DIRC; n--);
#endif
   if(n < 0 || (n == 1 && pth[n-1] == '.'))
   {  if(getcwd(tmp, 120) == NULL) strcpy(tmp, f->ed->dirct);
      if((f->dirct = MALLOC((strlen(tmp)+1)*sizeof(char))) == NULL)
      e_error(e_msg[0], 1, f->fb);
      strcpy(f->dirct, tmp);
      if((f->datnam = MALLOC((strlen(pth+n+1)+1)*sizeof(char))) == NULL)
      e_error(e_msg[0], 1, f->fb);
      strcpy(f->datnam, pth+n+1);
      return(f->dirct);
   }
   if((f->datnam = MALLOC((len-n)*sizeof(char))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   for(i = n+1; i <= len; i++) f->datnam[i-n-1] = pth[i];
#if defined(DOS) || defined(DJGPP)
   if(pth[n] == ':') n += 2;
   if(n == 0 || pth[n-1] == ':') n++;
#else
   if(n == 0) n++;
#endif
   if((f->dirct = MALLOC((n+1)*sizeof(char))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   for(i = 0; i < n; i++) f->dirct[i] = pth[i];
#if defined(DOS) || defined(DJGPP)
   if(pth[n-2] == ':') f->dirct[n-1] = DIRC;
#endif
   f->dirct[n] = '\0';
   return(f->dirct);
}

/*
       N binaere Zeichen einfuegen    */
int e_ins_nbchar(b, sch, s, xa, ya, n)
     BUFFER *b;
     SCHIRM *sch;
     char *s;
     int xa;
     int ya;
     int n;
{
   int i;
   if(b->bf[ya].len+n >= b->mx.x-1)
   {  e_new_line(ya+1, b);
      for(i = 0; i < b->bf[ya].len-xa; i++)
	    *(b->bf[ya+1].s+i) = *(b->bf[ya].s+i+xa);
      b->bf[ya+1].len = b->bf[ya].len - xa;
      b->bf[ya].len = xa;
      if(xa + n >= b->mx.x) e_ins_nbchar(b, sch, s, 0, ya+1, n);
      else
      {  for(i = 0; i < n; i++)
		 *(b->bf[ya].s+i+xa) = *(s+i);
	 b->bf[ya].len = b->bf[ya].nrc = b->bf[ya].len + n;
      }
   }
   else
   {  for(i = b->bf[ya].len - 1; i >= xa; i--)
		 *(b->bf[ya].s+i+n) = *(b->bf[ya].s+i);
      for(i = 0; i < n; i++)
		 *(b->bf[ya].s+i+xa) = *(s+i);
      b->bf[ya].len = b->bf[ya].nrc = b->bf[ya].len + n;
   }
   b->b.x = xa + n;
   b->b.y = ya;
   if(sch->ka.y == ya && sch->ka.x > xa) sch->ka.x += n;
   if(sch->ke.y == ya && sch->ke.x > xa) sch->ke.x += n;
   
   return(xa+n);
}
/*
      N binaere Zeichen loeschen    */

int e_del_nbchar(b, s, x, y, n)
     BUFFER *b;
     SCHIRM *s;
     int x;
     int y;
     int n;
{
   int j;
   if(b->bf[y].len <= n) e_del_line(y, b, s);
   else
   {  for (j = x; j < b->bf[y].len - n; ++j)
		       *(b->bf[y].s+j) = *(b->bf[y].s+j+n);
      b->bf[y].nrc = b->bf[y].len -= n;
   }
   if(s->ka.y == b->b.y && s->ka.x > b->b.x)
   s->ka.x = (s->ka.x-n > b->b.x) ? s->ka.x-n : b->b.x;
   if(s->ke.y == b->b.y && s->ke.x > b->b.x)
   s->ke.x = (s->ke.x-n > b->b.x) ? s->ke.x-n : b->b.x;
   
   return(x+n);
}
/*
      Maus-Leisten-Cursor-schreiben    */

int e_Lst_zeichen(x, y, n, sw, frb, max, iold, new)
     int x;
     int y;
     int n;
     int sw;
     int frb;
     int max;
     int iold;
     int new;
{
   int inew;
   double d = max ? 1./(float)max : 0;
   
   if(n < 3) return(1);
   
   if((inew = (int) (new*(n-2)*d + 1.5)) > n-2) inew = n-2;
   if(iold < 1) iold = 1;
   if(inew < 1) inew = 1;
   
   if(sw == 0)
   {  if(iold < n-1) e_pr_char(x, y+iold, MCI, frb);
      e_pr_char(x, y+inew, MCA, frb);
#ifdef NEWSTYLE
      e_make_xrect(x, y+1, x, y+n-2, 0);
      e_make_xrect(x, y+inew, x, y+inew, 0);
#endif
   }
   else
   {  if(iold < n-1) e_pr_char(x+iold, y, MCI, frb);
      e_pr_char(x+inew, y, MCA, frb);
#ifdef NEWSTYLE
      e_make_xrect(x+1, y, x+n-2, y, 0);
      e_make_xrect(x+inew, y, x+inew, y, 0);
#endif
   }
   return(inew);
}

/*   Maus-Leiste schreiben   */

void e_mouse_Leiste(x, y, n, sw, frb)
     int x;
     int y;
     int n;
     int sw;
     int frb;
{
#ifdef NEWSTYLE
   int sv = n;
#endif
   if(sw == 0)
   {  e_pr_char(x, y, MCU, frb);
      e_pr_char(x, y+n-1, MCD, frb);
      for(; n > 2; n--)
      e_pr_char(x, y+n-2, MCI, frb);
#ifdef NEWSTYLE
      e_make_xrect(x, y+1, x, y+sv-2, 0);
      e_make_xrect(x, y, x, y, 0);
      e_make_xrect(x, y+sv-1, x, y+sv-1, 0);
#endif
   }
   else
   {  e_pr_char(x, y, MCL, frb);
      e_pr_char(x+n-1, y, MCR, frb);
      for(; n > 2; n--)
      e_pr_char(x+n-2, y, MCI, frb);
#ifdef NEWSTYLE
      e_make_xrect(x+1, y, x+sv-2, y, 0);
      e_make_xrect(x, y, x, y, 0);
      e_make_xrect(x+sv-1, y, x+sv-1, y, 0);
#endif
   }
}

int e_autosave(f)
     FENSTER *f;
{
#ifndef WEUNDO
   return(f->save = 1);
#else
   char *tmp, *str;
   f->save = 1;
   if(!(f->ed->autosv & 2)) return(0);
   str = MALLOC(strlen(f->datnam) + 5);
   str = e_make_postf(str, f->datnam, ".ASV");
   tmp = f->datnam;
   f->datnam = str;
   e_save(f);
   f->datnam = tmp;
   FREE(str);
   return(0);
#endif
}

#ifdef WEUNDO

Undo *e_remove_undo(ud, sw)
     Undo *ud;
     int sw;
{
   extern struct EXT h_error;
   if(ud == NULL) return(ud);
   ud->next = e_remove_undo(ud->next, sw + 1);
   if(sw > h_error.cn->numundo)
   {  if(ud->type == 'l') FARFREE(ud->u.pt);
      else if(ud->type == 'd')
      {  BUFFER *b = (BUFFER*) ud->u.pt;
	 int i;
	 FREE(b->f->s);
	 FREE(b->f);
#ifdef SWAP
	 if(b->bf == NULL)  e_swap_in_b(b);
	 {  for(i = 0; i < b->mxlines; i++)
	    {  if(b->bf[i].s != NULL) FREE( b->bf[i].s );
	       else
	       {  if(b->sp->fp2)  e_swp_bl_ad(b->bf[i].sp, -1);
		  else  (b->sp->bl[b->bf[i].sp])--;
	       }
#else
	 if(b->bf != NULL)
	 {  for(i = 0; i < b->mxlines; i++)
	    {  if(b->bf[i].s != NULL) FARFREE( b->bf[i].s );
#endif
	       b->bf[i].s = NULL;
	    }
	    FREE(b->bf);
	 }
	 FREE(b);
      }
      FREE(ud);
      ud = NULL;
   }
   return(ud);
}

int e_add_undo(sw, b, x, y, n)
     int sw;
     BUFFER *b;
     int x;
     int y;
     int n;
{
   extern struct EXT h_error;
   Undo *next;
   if(e_undo_sw) return(0);
   if(!e_redo_sw && b->rd)
   b->rd = e_remove_undo(b->rd, h_error.cn->numundo+1);
   if((next = MALLOC(sizeof(Undo))) == NULL)
   {  e_error(e_msg[0], 0, b->fb);  return(-1);  }
   next->type = sw;
   next->b.x = x;
   next->b.y = y;
   next->a.x = n;
   if(e_redo_sw == 1) next->next = b->rd;
   else next->next = b->ud;
   if(sw == 'a');
   else if(sw == 'p') next->u.c = b->bf[y].s[x];
   else if(sw == 'r' || sw == 's')
   {  char far *str = FARMALLOC(n);
      int i;
      if( str == NULL)
      {  e_error(e_msg[0], 0, b->fb);  FREE(next); return(-1);  }
      for(i = 0; i < n; i++) str[i] = b->bf[y].s[x+i];
      next->u.pt = str;
      next->a.y = b->cn->fd.rn;
   }
   else if(sw == 'l') next->u.pt = b->bf[y].s;
   else if(sw == 'c' || sw == 'v')
   {  SCHIRM *s = b->cn->f[b->cn->mxedt]->s;
      next->a = s->ka;
      next->e = s->ke;
   }
   else if(sw == 'd')
   {  BUFFER *bn = MALLOC(sizeof(BUFFER));
      SCHIRM *sn = MALLOC(sizeof(SCHIRM));
      FENSTER *fn = MALLOC(sizeof(FENSTER));
      FENSTER *f = b->cn->f[b->cn->mxedt];
      bn->bf = (STRING *) MALLOC(MAXLINES*sizeof(STRING));
      if(bn == NULL || sn == 0 || bn->bf == NULL)
      return(e_error(e_msg[0], 0, b->fb));
      fn->b = bn;
      fn->c_sw = NULL;
      fn->c_st = NULL;
      bn->f = fn;
      bn->f->s = sn;
      bn->b = e_s_pkt(0, 0);
      bn->mx = e_s_pkt(b->cn->maxcol, MAXLINES);
      bn->mxlines = 0;
      sn->fb = bn->fb = b->fb;
      bn->cn = b->cn;
#ifdef SWAP
      bn->sp = b->cn->sp;
      bn->pos = b->cn->curedt;
#endif
      bn->ud = NULL;
      bn->rd = NULL;
      sn->c = sn->ks = sn->ka = sn->ke = sn->fa = sn->fe = e_s_pkt(0, 0);
      e_new_line(0 ,bn);
      *(bn->bf[0].s) = WR;
      *(bn->bf[0].s+1) = '\0';
      bn->bf[0].len = 0;
      bn->bf[0].nrc = 1;
      next->u.pt = bn;
      e_undo_sw = 1;
      e_vsch_block(0, 0, b, bn, f);
      e_undo_sw = 0;
   }
   if(e_redo_sw == 1) b->rd = next;
   else
   {  next->next = e_remove_undo(b->ud, 1);
      b->ud = next;
   }
   return(0);
}

int e_make_undo(f)
     FENSTER *f;
{
   return(e_make_rudo(f, 0));
}

int e_make_redo(f)
     FENSTER *f;
{
   return(e_make_rudo(f, 1));
}

int e_make_rudo(f, sw)
     FENSTER *f;
     int sw;
{
   BUFFER *b;
   SCHIRM *s;
   Undo *ud;
   int i;
   for(i = f->ed->mxedt; i > 0 && f->ed->f[i]->dtmd <= 'Z'; i--);
   if(i <= 0) return(0);
   e_switch_window(f->ed->edt[i], f);
   f = f->ed->f[f->ed->mxedt];
   b = f->b;  s = f->s;
   if(!sw) ud = b->ud;
   else ud = b->rd;
   if( ud == NULL)
   {  e_error((!sw ? e_msg[10] : e_msg[23]), 0, b->fb);  return(-1);  }
   f = f->ed->f[f->ed->mxedt];
   if(!sw) e_redo_sw = 1;
   else e_redo_sw = 2;
   b->b = ud->b;
   if(ud->type == 'r' || ud->type == 's')
   {  if(ud->type == 's')
      {  e_add_undo('s', b, ud->b.x, ud->b.y, ud->a.y);
	 e_undo_sw = 1;
	 e_del_nchar(b, s, ud->b.x, ud->b.y, ud->a.y);
      }
      if(*((char*)ud->u.pt) == '\n' && ud->a.x == 1) e_car_ret(b, s);
      else if(*((char*)ud->u.pt + ud->a.x - 1) == '\n')
      {  e_ins_nchar(b, s, ((char *)ud->u.pt), ud->b.x, ud->b.y, ud->a.x-1);
	 e_car_ret(b, s);
      }
      else e_ins_nchar(b, s, ((char *)ud->u.pt), ud->b.x, ud->b.y, ud->a.x);
      e_undo_sw = 0;
      s->ka = ud->b;
      s->ke.y = ud->b.y;
      s->ke.x = ud->b.x + ud->a.x;
      FARFREE(ud->u.pt);
   }
   else if(ud->type == 'l')
   {  for(i = b->mxlines; i > ud->b.y; i--) b->bf[i] = b->bf[i-1];
      (b->mxlines)++;
      b->bf[b->b.y].s = ud->u.pt;
      b->bf[b->b.y].len = e_str_len(b->bf[b->b.y].s);
      b->bf[b->b.y].nrc = e_str_nrc(b->bf[b->b.y].s);
      s->ka = ud->b;
      s->ke.y = ud->b.y + 1;
      s->ke.x = 0;
      e_add_undo('y', b, 0, b->b.y, 0);
   }
   else if(ud->type == 'y') e_del_line(b->b.y, b, s);
   else if(ud->type == 'a')
   e_del_nchar(b, s, ud->b.x, ud->b.y, ud->a.x);
   else if(ud->type == 'p') b->bf[ud->b.y].s[ud->b.x] = ud->u.c;
   else if(ud->type == 'c')
   {  b->b = s->ka = ud->a;
      s->ke = ud->e;
/*	e_blck_clear(b, s);   */
      e_blck_del(f);
   }
   else if(ud->type == 'v')
   {  b->b = ud->b;
      s->ka = ud->a;
      s->ke = ud->e;
      e_blck_versch(f);
   }
   else if(ud->type == 'd')
   {  BUFFER *bn = (BUFFER*)ud->u.pt;
      e_undo_sw = 1;
      s->ka = bn->f->s->ka;
      s->ke = bn->f->s->ke;
      e_vsch_block(ud->b.x, ud->b.y, bn, b, f);
      e_undo_sw = 0;
      FREE(bn->f->s);
      FREE(bn->f);
      FREE(bn->bf[0].s);
      FREE(bn->bf);
      FREE(ud->u.pt);
      e_add_undo('c', b, ud->b.x, ud->b.y, 0);
   }
   if(!sw) b->ud = ud->next;
   else b->rd = ud->next;
   e_redo_sw = 0;
   FREE(ud);
   e_schirm(f, 1);
   e_cursor(f, 1);
   return(0);
}

char *e_make_postf(out, name, pf)
     char *out;
     char *name;
     char *pf;
{
#ifndef DOS
   strcpy(out, name);
   strcat(out, pf);
#else
   int i;
   strcpy(out, name);
   for(i = strlen(out); i >= 0 && out[i] != '.'; i--);
   if(i >= 0) out[i+1] = '\0';
   strcat(out, pf);
#endif
   return(out);
}

#endif

