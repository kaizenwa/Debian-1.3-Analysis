/* we_fl_mg.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"

#ifdef UNIX

#include<sys/types.h>
#include<sys/stat.h>

#include "xkeys.h"

struct dirfile *e_make_win_list();
int e_s_sys_ini();
int e_s_sys_end();
extern char *e_tmp_dir;

#ifdef NOSYMLINKS
#define readlink(x, y, z) -1
#define fk_ren_link(x, y, z) 0
#define fk_link(x, y, sw) link(x, y)
#endif

int e_file_first(sw, cn, dirct)
     int sw;
     ECNT *cn;
     char *dirct;
{
   extern char *e_hlp_str[];
   extern WOPT *fblst, *rblst, *wblst, *xblst, *sblst, *ablst;
   FENSTER *f;
   int i, j;
   FLBFFR *b;
   char tmp[256];
   if(cn->mxedt >= MAXEDT)
   {  e_error(e_msg[3], 0, cn->fb);
      return(-1);
   }
   for (j = 1; j <= MAXEDT; j++)
   {  for (i = 1; i <= cn->mxedt && cn->edt[i] != j; i++);
      if( i > cn->mxedt) break;
   }
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   cn->curedt=j;
   (cn->mxedt)++;
   cn->edt[cn->mxedt]=j;
   
   if( (f = (FENSTER *) MALLOC(sizeof(FENSTER))) == NULL)
   e_error(e_msg[0], 1, cn->fb);
   if( (b = (FLBFFR *) MALLOC(sizeof(FLBFFR))) == NULL)
   e_error(e_msg[0], 1, cn->fb);
   f->fb = cn->fb;
   cn->f[cn->mxedt] = f;
   f->a = e_s_pkt(11, 2);
   f->e = e_s_pkt(f->a.x+55, f->a.y+20);
   f->winnum = cn->curedt;
   f->dtmd = 'F';
   f->ins = 1;
   f->save = 0;
   f->zoom = 0;
   f->ed = cn;
   f->c_sw = NULL;
   f->c_st = NULL;
   f->pic = NULL;
   if(sw == 6)
   {  sw = 0;
      f->datnam = "Wastebasket";
      f->save = 1;
   }
   else f->datnam = "File-Manager";
   if(sw == 0)  {  f->blst = fblst;  f->nblst = 8;  }
   else if(sw == 1)  {  f->blst = rblst;  f->nblst = 4;  }
   else if(sw == 2)  {  f->blst = wblst;  f->nblst = 4;  }
   else if(sw == 3)  {  f->blst = xblst;  f->nblst = 4;  }
   else if(sw == 4)  {  f->blst = sblst;  f->nblst = 5;  }
   else if(sw == 5)  {  f->blst = ablst;  f->nblst = 4;  }
   
   if(sw == 3) f->hlp_str = e_hlp_str[5];
   else f->hlp_str = e_hlp_str[4];
   
   if((f->dirct = MALLOC(256)) == NULL)
   e_error(e_msg[0], 1, cn->fb);
   if(!dirct || dirct[0] == '\0')
   {  getcwd(tmp, 256); strcpy(f->dirct, tmp);  }
   else strcpy(f->dirct, dirct);
   
   f->b = (BUFFER *)b;
   if((b->rdfile = MALLOC(256)) == NULL)
   e_error(e_msg[0], 1, cn->fb);
   strcpy(b->rdfile, cn->fd.file);
   b->sw = sw;
   
   if( (b->fw = (FLWND *) MALLOC(sizeof(FLWND))) == NULL)
   e_error(e_msg[0], 1, cn->fb);
   if( (b->dw = (FLWND *) MALLOC(sizeof(FLWND))) == NULL)
   e_error(e_msg[0], 1, cn->fb);
   
   b->dw->df = e_ext_dirdir(b->cd = e_mk_cur_dir(f->save),
		b->dd = e_find_dir(SUDIR, f->ed->flopt & 02 ? 1 : 0));
   
   i = f->ed->flopt & 01 ? 1 : 0;
   if(sw == 3) i |= 2;
   b->fw->df = e_ext_dirfile(b->df = e_find_files(b->rdfile, i), f->ed->flopt >> 9);
   
   b->fw->mxa = f->a.x; b->fw->mxe = f->e.x; b->fw->mya = f->a.y; b->fw->mye = f->e.y;
   b->fw->xa = f->e.x - 33; b->fw->xe = f->e.x - 17; b->fw->ya = f->a.y + 6;
   b->fw->ye = f->a.y + 17; b->fw->f = f;
   b->fw->ia = b->fw->nf = b->fw->nxfo = b->fw->nyfo = 0;
   b->fw->srcha = b->fw->ja = 12;
   
   b->dw->mxa = f->a.x; b->dw->mxe = f->e.x; b->dw->mya = f->a.y; b->dw->mye = f->e.y;
   b->dw->xa = f->a.x + 3; b->dw->xe = f->a.x + 28; b->dw->ya = f->a.y + 6;
   b->dw->ye = f->a.y + 17; b->dw->f = f;
   b->dw->ia = b->dw->ja = b->dw->nxfo = 0; b->dw->srcha = -1;
   b->dw->nf = b->dw->nyfo = b->cd->anz - 1;
   if(cn->mxedt > 1) e_ed_rahmen(cn->f[cn->mxedt-1], 0);
   e_firstl(f, 1);
   e_file_schirm(f, 1);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   return(0);
}

int e_file_schirm(f, sw)
     FENSTER *f;
     int sw;
{
   FLBFFR *b = (FLBFFR *)f->b;
   int i, j;
   int bx1 = 1, bx2 = 1, bx3 = 1, by = 4;
   
   for(j = f->a.y+1; j < f->e.y; j++)
   for(i = f->a.x+1; i < f->e.x; i++)
   e_pr_char(i, j, ' ', f->fb->nt.fb);
   
   if(f->e.y - f->a.y <= 17) by = -1;
   else if(b->sw != 0 || f->e.y - f->a.y <= 19) by = 2;
   
   if(f->e.y - f->a.y > 17)
   {  e_pr_str((f->a.x + 4), f->e.y - by, "Cancel", f->fb->nz.fb, -1, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
      e_pr_str((f->a.x + 14), f->e.y - by, "Change Dir", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
      if(b->sw == 1 && f->e.x - f->a.x >= 34)
      e_pr_str((f->a.x + 28), f->e.y - by, "Read", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
      else if(b->sw == 2 && f->e.x - f->a.x >= 35)
      e_pr_str((f->a.x + 28), f->e.y - by, "Write", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
      else if(b->sw == 4)
      {  if(f->e.x - f->a.x >= 34)
	 e_pr_str((f->a.x + 28), f->e.y - by, "Save", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
	 if(f->e.x - f->a.x >= 48)
	 e_pr_str((f->a.x + 36), f->e.y - by, "Save CrYpt",
			f->fb->nz.fb, 7, -1, f->fb->ns.fb, f->fb->nt.fb);
      }
      else if(b->sw == 3 && f->e.x - f->a.x >= 37)
      e_pr_str((f->a.x + 28), f->e.y - by, "Execute", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
      else if(b->sw == 5 && f->e.x - f->a.x >= 33)
      e_pr_str((f->a.x + 28), f->e.y - by, "Add", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
      else if(b->sw == 0)
      {  if(f->e.x - f->a.x >= 34)
	 e_pr_str((f->a.x + 28), f->e.y - by, "Edit", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
	 if(f->e.x - f->a.x >= 45)
	 e_pr_str((f->a.x + 36), f->e.y - by, "RePlace", f->fb->nz.fb, 2, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
	 if(f->e.x - f->a.x >= 54)
	 e_pr_str((f->a.x + 47), f->e.y - by, "MKdir", f->fb->nz.fb, 1, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
      }
   }
   if(b->sw == 0 && f->e.y - f->a.y > 19)
   {  e_pr_str((f->a.x + 4), f->e.y - 2, "Move", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
      e_pr_str((f->a.x + 12), f->e.y - 2, "DUplicate", f->fb->nz.fb, 1, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
      if(f->e.x - f->a.x >= 33)
      e_pr_str((f->a.x + 25), f->e.y - 2, "Remove", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
      if(f->e.x - f->a.x >= 41)
#ifndef DJGPP
      e_pr_str((f->a.x + 35), f->e.y - 2, "Link", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
#else
      e_pr_str((f->a.x + 35), f->e.y - 2, "COpy", f->fb->nz.fb, 1, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
#endif
      if(f->e.x - f->a.x >= 55)
      e_pr_str((f->a.x + 43), f->e.y - 2, "Attributes", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
#ifndef DJGPP
      if(f->e.x - f->a.x >= 63)
      e_pr_str((f->a.x + 57), f->e.y - 2, "COpy", f->fb->nz.fb, 1, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
#endif
   }
   if(f->e.x - f->a.x < 45) bx3 = 0;
   if(f->e.x - f->a.x < 44) bx2 = 0;
   if(f->e.x - f->a.x < 43) bx1 = 0;
   b->xfd = (f->e.x - f->a.x - bx1 - bx2 - bx3 - 6) / 2;
   b->xdd = f->e.x - f->a.x - bx1 - bx2 - bx3 - b->xfd - 6;
   b->xda = 2 + bx1;
   b->xfa = 4 + bx1 + bx2 + b->xdd;
   
   e_pr_str((f->a.x + b->xfa), f->a.y + 2, "Name:", f->fb->nt.fb, 0, 1,
                               f->fb->nsnt.fb, f->fb->nt.fb);
/*    e_schr_nchar(b->rdfile, f->a.x+b->xfa, f->a.y+3, 0, b->xfd+1, f->fb->fr.fb);   */
   e_schr_nchar_wsv(b->rdfile, f->a.x+b->xfa, f->a.y+3, 0, b->xfd+1,
				f->fb->fr.fb, f->fb->fz.fb);
   e_pr_str((f->a.x+b->xfa), f->a.y + 5, "Files:", f->fb->nt.fb, 0, 1,
                               f->fb->nsnt.fb, f->fb->nt.fb);
   
   e_pr_str((f->a.x + b->xda), f->a.y + 2, "Directory:", f->fb->nt.fb, 0, 1,
                               f->fb->nsnt.fb, f->fb->nt.fb);
/*    e_schr_nchar(f->dirct, f->a.x+b->xda, f->a.y+3, 0, b->xdd+1, f->fb->fr.fb);   */
   e_schr_nchar_wsv(f->dirct, f->a.x+b->xda, f->a.y+3, 0, b->xdd+1,
				f->fb->fr.fb, f->fb->fz.fb);
   e_pr_str((f->a.x + b->xda), f->a.y + 5, "DirTree:", f->fb->nt.fb, 3, 1,
                               f->fb->nsnt.fb, f->fb->nt.fb);
   
   b->fw->mxa = f->a.x; b->fw->mxe = f->e.x; b->fw->mya = f->a.y; b->fw->mye = f->e.y;
   b->fw->xa = f->a.x+b->xfa; b->fw->xe = b->fw->xa+b->xfd; b->fw->ya = f->a.y + 6;
   b->fw->ye = f->e.y - 2 - by;
   b->dw->mxa = f->a.x; b->dw->mxe = f->e.x; b->dw->mya = f->a.y; b->dw->mye = f->e.y;
   b->dw->xa = f->a.x+b->xda; b->dw->xe = b->dw->xa+b->xdd; b->dw->ya = f->a.y + 6;
   b->dw->ye = f->e.y - 2 - by;
   
   e_mouse_leiste(b->fw->xe, b->fw->ya, b->fw->ye-b->fw->ya, 0, b->fw->f->fb->em.fb);
   e_mouse_leiste(b->fw->xa, b->fw->ye, b->fw->xe-b->fw->xa, 1, b->fw->f->fb->em.fb);
   e_pr_file_window(b->fw, 0, 1, f->fb->ft.fb, f->fb->fz.fb, f->fb->frft.fb);
   e_mouse_leiste(b->dw->xe, b->dw->ya, b->dw->ye-b->dw->ya, 0, b->dw->f->fb->em.fb);
   e_mouse_leiste(b->dw->xa, b->dw->ye, b->dw->xe-b->dw->xa, 1, b->dw->f->fb->em.fb);
   e_pr_file_window(b->dw, 0, 1, f->fb->ft.fb, f->fb->fz.fb, f->fb->frft.fb);
   return(0);
}

/*    Aufruf des Datei-Managers    */

int e_manager_first(f)
     FENSTER *f;
{
   return(e_file_first(0, f->ed, ""));
}

int e_file_again(sw, f)
     int sw;
     FENSTER *f;
{
   int i;
   FLBFFR *b;
   for(i = f->ed->mxedt; i > 0; i--)
   if(f->ed->f[i]->dtmd == 'F')
   {  b = (FLBFFR *)f->ed->f[i]->b;
      if(sw == 0 && b->sw == sw && f->ed->f[i]->save != 1) break;
      else if(sw == 6 && b->sw == 0 && f->ed->f[i]->save == 1) break;
      else if(sw != 0 && sw != 6 && b->sw == sw) break;
   }
   if( i <= 0)
   {  if(sw == 6)
      {  char tmp[256];
	 return(e_file_first(sw, f->ed, e_ret_wastefile("", tmp)));
      }
      return(e_file_first(sw, f->ed, ""));
   }
   e_switch_window(f->ed->edt[i], f);
   return(0);
}

int e_manager(f)
     FENSTER *f;
{
   return(e_file_again(0, f));
}

int e_ed_manager(sw, f)
     int sw;
     FENSTER *f;
{
   return(e_file_first(sw, f->ed, ""));
}

int e_execute(f)
     FENSTER *f;
{
   return(e_file_again(3, f));
}

int e_saveas(f)
     FENSTER *f;
{
   return(e_file_first(4, f->ed, ""));
}

int e_shell(f)
     FENSTER *f;
{
   PIC *outp = NULL;
   int g[4];
#ifndef DJGPP
   if(!(e_we_sw & 1))
#endif
   {  outp = e_open_view(0,0,MAXSCOL-1,MAXSLNS-1,f->fb->ws,1);
      fk_locate(0,0);
      fk_cursor(1);
#if  MOUSE
      g[0] = 2;
      fk_mouse(g);
#endif
      e_s_sys_ini();
   }
#ifdef DJGPP
   system(user_shell);
#else
#ifdef XWINDOW
   if(e_we_sw & 1) e_x_system(user_shell);
   else system(user_shell);
#else
   system(user_shell);
#endif
#endif
#ifndef DJGPP
   if(!(e_we_sw & 1))
#endif
   {  e_s_sys_end();
      e_close_view(outp, 1);
      fk_cursor(0);
#if  MOUSE
      g[0] = 1;
      fk_mouse(g);
#endif
   }
   return(0);
}

/*    Datei-Manager     */

#ifndef VERSION14

int e_file_eingabe(cn)
     ECNT *cn;
{
   extern struct EXT h_error;
   FENSTER *f = cn->f[cn->mxedt], *fe = NULL;
   FLBFFR *b = (FLBFFR *)f->b;
   BUFFER *be = NULL;
   SCHIRM *se = NULL;
   int c = AltC, i, j;
   int winnum = 0, nco, svmode = -1, fmode;
   int g[4], cold = AltN;
   char filen[128], ftmp[256], *dtp = NULL, *ftp = NULL, *svdir = NULL;
   PIC *outp = NULL;
   FILE *fp;
   
   if(f->dtmd != 'F') return(0);
   
   if(f->save == 1)
   {  svmode = f->ed->flopt;  f->ed->flopt = 010023;  }
   if(f->save == 1 || b->sw == 5)
   {  svdir = MALLOC((strlen(f->ed->dirct)+1)*sizeof(char));
      strcpy(svdir, f->ed->dirct);
   }
   nco = b->cd->anz - 1;
   fmode = f->ed->flopt & 01 ? 1 : 0;
   if(b->sw == 3) fmode |= 2;
   
   for(i = cn->mxedt; i > 0; i--)
   {  if(cn->f[i]->dtmd > 'Z')
      {  fe = cn->f[i];  be = fe->b;  se = fe->s;  winnum = cn->edt[i];  break;  }
   }
   strcpy(cn->fd.file, b->rdfile);
   while(c != ESC)
   {  e_pr_file_window(b->fw, 0, 1, f->fb->ft.fb, f->fb->fz.fb, f->fb->frft.fb);
      e_pr_file_window(b->dw, 0, 1, f->fb->ft.fb, f->fb->fz.fb, f->fb->frft.fb);
      switch(c)
      {  case AltN:
	    cold = c;
	    fk_cursor(1);
	    c = e_schr_lst_wsv(b->rdfile, f->a.x+b->xfa, f->a.y+3,
		   b->xfd+1, 80, f->fb->fr.fb, f->fb->fz.fb, &f->ed->fdf, f);
#ifdef DOS
	    strupr(b->rdfile);
#endif
#ifdef DJGPP
	    for(i = strlen(b->rdfile); i>=0 && b->rdfile[i] != DIRC &&
		     b->rdfile[i] != '/' && b->rdfile[i] != ':'; i--);
#else
	    for(i = strlen(b->rdfile); i>=0 && b->rdfile[i] != DIRC; i--);
#endif
	    strcpy(cn->fd.file, b->rdfile+i+1);
	    if(i >= 0)
	    {  if(i == 0) i++;
	       b->rdfile[i] = '\0';
	       strcpy(f->dirct, b->rdfile);
	       strcpy(b->rdfile, cn->fd.file);
	       c = AltC;
	    }
#if  MOUSE
	    if(c == -1) c = e_mng_mouse(f);
#endif
	    if((c >= Alt1 && c <= Alt9) || (c >= 1024 && c <= 1049))
	    {  e_schr_nchar_wsv(b->rdfile, f->a.x+b->xfa, f->a.y+3, 0,
			b->xfd+1, f->fb->fr.fb, f->fb->fz.fb);
	       break;
	    }
	    if(c == CLE || c == CCLE) c = AltD;
	    else if(c == CDO || c == BDO) c = AltF;
	    else if((c == CR || (b->sw == 0 && (c == AltE || c == AltP))
		    || (b->sw == 1 && c == AltR) || (b->sw == 2 && c == AltW)
		    || (b->sw == 3 && c == AltE)
		    || (b->sw == 5 && c == AltA)
		    || (b->sw == 4 && (c == AltS || c == AltY)) )
                      && ( strstr(b->rdfile,"*") || strstr(b->rdfile,"?")
				|| strstr(b->rdfile,"[")) )
	    {
#ifdef XWINDOW
	       if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
	       freedf(b->df); FREE(b->fw->df);
	       b->fw->df = e_ext_dirfile(b->df =
			e_find_files(b->rdfile, fmode), f->ed->flopt >> 9);
	       b->fw->ia = b->fw->nf = 0;
	       b->fw->ja = b->fw->srcha;
	       c = AltF;
#ifdef XWINDOW
	       if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
	    }
	    else
	    {  strcpy(filen, b->rdfile);
	       if(c == CR)
	       {  if(b->sw == 1) c = AltR;
		  else if(b->sw == 2) c = AltW;
		  else if(b->sw == 4) c = AltS;
		  else if(b->sw == 5) c = AltA;
		  else c = AltE;
	       }
	    }
	    if (c != AltN)
	    e_schr_nchar_wsv(b->rdfile, f->a.x+b->xfa, f->a.y+3, 0,
			b->xfd+1, f->fb->fr.fb, f->fb->fz.fb);
	    fk_cursor(0);
	    break;
	 case AltD:
	    cold = c;
	    fk_cursor(1);
	    c = e_schr_lst_wsv(f->dirct, f->a.x+b->xda, f->a.y+3,
		    b->xdd+1, 80, f->fb->fr.fb, f->fb->fz.fb, &f->ed->ddf, f);
#ifdef DOS
	    strupr(f->dirct);
#endif
#if  MOUSE
	    if(c == -1) c = e_mng_mouse(f);
#endif
	    if(c == CRI || c == CCRI) c = AltN;
	    else if(c == CDO || c == BDO) c = AltT;
	    else if(c== CR) c = AltC;
	    if (c != AltD)
	    e_schr_nchar_wsv(f->dirct, f->a.x+b->xda, f->a.y+3, 0,
			b->xdd+1, f->fb->fr.fb, f->fb->fz.fb);
	    fk_cursor(0);
	    break;
	 case AltT:
	    cold = c;
	    c = e_file_window(1, b->dw, f->fb->ft.fb, f->fb->fz.fb);
#if  MOUSE
	    if(c == MBKEY) c = e_mng_mouse(f);
	    else if(c < 0) c = e_mv_mouse(c, 1, f);
#endif
	    if (c == CCRI) c = AltF;
	    else if (c == BUP) c = AltD;
#ifdef DJGPP
	    else if (c == AltC || (c == CR && b->dw->nf != nco))
	    {  c = AltT;
	       if(b->dw->nf == 0)
	       {  if(f->save == 1)
		  {  e_ret_wastefile("", f->dirct);
		     e_schr_nchar_wsv(f->dirct, f->a.x+b->xda, f->a.y+3, 0,
			b->xdd+1, f->fb->fr.fb, f->fb->fz.fb);
		     f->ed->ddf = e_add_df(f->dirct, f->ed->ddf);
		     c = AltC;
		  }
		  else
		  {  for(i = 1; i < b->cd->anz; i++)
		     if(b->cd->name[i]) FREE(b->cd->name[i]);
		     b->cd->anz = 1;
		     nco = b->dw->nf;
		     freedf(b->dd);
		     freedf(b->dw->df);
		     b->dd = e_mk_drives();
		     b->dw->df = e_ext_dirdir(b->cd, b->dd);
		     for(b->dw->nf = 1; b->dw->nf <= b->dd->anz &&
			    *b->dd->name[b->dw->nf-1] != toupper(*f->dirct)
				; b->dw->nf++);
		     if(b->dw->nf > b->dd->anz) b->dw->nf = 1;
		     nco = 0;
		     b->dw->ia = b->dw->ja = 0;
		     if(b->dw->nf-b->dw->ia >= b->dw->ye - b->dw->ya)
		     b->dw->ia = b->dw->nf+b->dw->ya-b->dw->ye+1;
		  }
	       }
	       else
	       {  if(b->cd->anz == 1 && !f->save)
		  {  h_error.sw = 0;
		     fk_setdisk(**(b->dd->name+b->dw->nf-1)-'A');
		     if(h_error.sw == 0) getcwd(f->dirct, 80);
		     if(h_error.sw != 0)
		     {  fk_setdisk(**(b->dd->name+nco-1)-'A');
			b->dw->nf = nco;  nco = 0;  break;
		     }
		     f->ed->dirct[0] = '\0';
		  }
		  else
		  e_mk_path(f->dirct, b->cd, b->dd, b->dw->nf);
		  e_schr_nchar_wsv(f->dirct, f->a.x+b->xda, f->a.y+3, 0,
			b->xdd+1, f->fb->fr.fb, f->fb->fz.fb);
		  f->ed->ddf = e_add_df(f->dirct, f->ed->ddf);
		  c = AltC;
	       }
#else
	    else if (c == AltC || (c == CR && b->dw->nf != nco))
	    {  c = AltT;
	       e_mk_path(f->dirct, b->cd, b->dd, b->dw->nf);
	       e_schr_nchar_wsv(f->dirct, f->a.x+b->xda, f->a.y+3, 0,
			b->xdd+1, f->fb->fr.fb, f->fb->fz.fb);
	       f->ed->ddf = e_add_df(f->dirct, f->ed->ddf);
	       c = AltC;
#endif
	    }
	    else if(c == CR) c = AltT;
	    break;
	 case AltF:
	    if (b->df->anz < 1) {  c = cold; break;  }
	    cold = c;
	    c = e_file_window(1, b->fw, f->fb->ft.fb, f->fb->fz.fb);
#if  MOUSE
	    if(c == MBKEY) c = e_mng_mouse(f);
	    else if(c < 0) c = e_mv_mouse(c, 0, f);
#endif
	    if (c == BUP) c = AltN;
	    else if (c == CCLE) c = AltT;
	    else if (c == CR)
	    {  if(b->sw == 1) c = AltR;
	       else if(b->sw == 2) c = AltW;
	       else if(b->sw == 4) c = AltS;
	       else if(b->sw == 5) c = AltA;
	       else c = AltE;
	    }
	    if( (b->sw == 1 && c == AltR)
			|| (b->sw == 2 && c == AltW)
			|| (b->sw == 3 && c == AltE)
			|| (b->sw == 4 && (c == AltS || c == AltY))
			|| (b->sw == 0 && (c == AltE || c == AltP)) )
	    {  strcpy(filen, *(b->df->name + b->fw->nf));  }
	    break;
	 case AltC:
	    c = cold;
	    if(!strcmp(f->ed->dirct, f->dirct)) break;
#ifdef DJGPP
	    if(f->dirct[1] == ':' && toupper(f->dirct[0]) != f->ed->dirct[0])
	    fk_setdisk(toupper(f->dirct[0])-'A');
	    if(h_error.sw != 0)
	    {  strcpy(f->dirct, f->ed->dirct);
	       freedf(b->cd);  freedf(b->dw->df);
	       freedf(b->dd);
	       b->dw->df = e_ext_dirdir(b->cd = e_mk_cur_dir(f->save),
		    b->dd = e_find_dir(SUDIR, f->ed->flopt & 02 ? 1 : 0));
	       nco = b->dw->nf = b->cd->anz - 1;
	       b->dw->ia = b->dw->ja = 0;
	       break;
	    }
#endif
#ifdef XWINDOW
	    if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
	    if(chdir(f->dirct) && f->save == 1)
	    chdir(e_ret_wastefile("", f->dirct));
	    getcwd(f->dirct, 80);
	    freedf(b->df);  freedf(b->fw->df);
	    freedf(b->cd);  freedf(b->dw->df);
	    freedf(b->dd);
	    if((f->ed->dirct = REALLOC(f->ed->dirct,
                                         strlen(f->dirct)+1)) == NULL)
	    e_error(e_msg[0], 0, f->fb);
	    else
	    strcpy(f->ed->dirct, f->dirct);
	    b->dw->df = e_ext_dirdir(b->cd = e_mk_cur_dir(f->save),
		    b->dd = e_find_dir(SUDIR, f->ed->flopt & 02 ? 1 : 0));
	    nco = b->dw->nf = b->cd->anz - 1;
	    b->dw->ia = b->dw->ja = 0;
	    b->fw->df = e_ext_dirfile(b->df =
			e_find_files(b->rdfile, fmode), f->ed->flopt >> 9);
	    b->fw->nf = b->fw->ia = 0;
	    b->fw->ja = 12;
#ifdef XWINDOW
	    if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
	    break;
#ifndef DJGPP
	 case AltL:
#endif
	 case AltO:
	 case AltM:
	    if(b->sw != 0) {  c = cold;  break;  }
	    j = c;
	    if(cold == AltF)
	    {  strcpy(ftmp, *(b->df->name + b->fw->nf));
	       c = e_schreib_leiste(ftmp, b->fw->xa,
			b->fw->ya+b->fw->nf-b->fw->ia, b->fw->xe-b->fw->xa,
			                   128, f->fb->fr.fb, f->fb->fz.fb);
	       if(c == CR)
	       {  if(j == AltM)
		  e_rename(*(b->df->name + b->fw->nf), ftmp, f);
#ifndef DJGPP
		  else if(j == AltL)
		  fk_link(*(b->df->name + b->fw->nf), ftmp, f->ed->flopt & 0400);
#endif
		  else if(j == AltO)
		  e_copy(*(b->df->name + b->fw->nf), ftmp, f);
		  freedf(b->df); FREE(b->fw->df);
		  b->fw->df = e_ext_dirfile(b->df = e_find_files(
				      b->rdfile, fmode), f->ed->flopt >> 9);
		  b->fw->ia = b->fw->nf = 0;
		  b->fw->ja = b->fw->srcha;
	       }
	    }
	    else if(cold == AltT && b->dw->nf >= b->cd->anz)
	    {  strcpy(ftmp, *(b->dd->name + b->dw->nf - b->cd->anz));
	       for(i = 0; *(b->dw->df->name[b->dw->nf]+i) &&
			(  *(b->dw->df->name[b->dw->nf]+i) <= 32 ||
			   *(b->dw->df->name[b->dw->nf]+i) >= 127); i++);
	       if(!(e_we_sw & 1)) i += 3;
	       b->dw->ja = i;
	       e_pr_file_window(b->dw, 0, 1, f->fb->ft.fb, f->fb->fz.fb, f->fb->frft.fb);
	       c = e_schreib_leiste(ftmp, b->dw->xa,
			b->dw->ya + b->dw->nf - b->dw->ia,
			b->dw->xe - b->dw->xa, 128, f->fb->fr.fb, f->fb->fz.fb);
	       if(c == CR)
	       {  if(j == AltM)
		  e_rename(*(b->dd->name + b->dw->nf - b->cd->anz), ftmp, f);
#ifndef DJGPP
		  else if(j == AltL)
		  e_link(*(b->dd->name + b->dw->nf - b->cd->anz), ftmp, f);
#endif
		  else if(j == AltO)
		  e_copy(*(b->dd->name + b->dw->nf - b->cd->anz), ftmp, f);
		  freedf(b->cd);  freedf(b->dw->df);  freedf(b->dd);
		  b->dw->df = e_ext_dirdir(b->cd = e_mk_cur_dir(f->save),
			          b->dd = e_find_dir(SUDIR,
						f->ed->flopt & 02 ? 1 : 0));
		  nco = b->dw->nf = b->cd->anz - 1;
		  b->dw->ia = b->dw->ja = 0;
	       }
	    }
	    c = cold;
	    cold = AltN;
	    break;
	 case AltU:
	    if(b->sw != 0) {  c = cold;  break;  }
	 case ENTF:
	    if(b->sw != 0) {  c = cold;  break;  }
	 case AltR:
	    if(b->sw == 0)
	    {  if(cold == AltF)
	       {  if(c == AltU) e_duplicate(*(b->df->name + b->fw->nf), f);
		  else if(c == AltR || c == ENTF)
		  e_remove(*(b->df->name + b->fw->nf), f);
		  freedf(b->df); FREE(b->fw->df);
		  b->fw->df = e_ext_dirfile(b->df =
			  e_find_files(b->rdfile, fmode), f->ed->flopt >> 9);
		  b->fw->ia = b->fw->nf = 0;
		  b->fw->ja = b->fw->srcha;
	       }
	       else if(cold == AltT && b->dw->nf >= b->cd->anz)
	       {  if(c == AltU)
		  e_duplicate(*(b->dd->name + b->dw->nf - b->cd->anz), f);
		  else if(c == AltR || c == ENTF)
		  e_remove(*(b->dd->name + b->dw->nf - b->cd->anz), f);
		  b->dw->df = e_ext_dirdir(b->cd = e_mk_cur_dir(f->save),
				b->dd = e_find_dir(SUDIR,
						f->ed->flopt & 02 ? 1 : 0));
		  nco = b->dw->nf = b->cd->anz - 1;
		  b->dw->ia = b->dw->ja = 0;
	       }
	       c = cold;
	       cold = AltN;
	       break;
	    }
	 case AltP:
	 case AltE:
	    if((c == AltE && b->sw != 0 && b->sw != 3)
			|| (c == AltP && b->sw != 0)
			|| (c == AltR && b->sw != 1) )
	    {  c = cold;  break;  }
	    if(b->sw == 3)
	    {  if(cold == AltF) strcpy(filen, *(b->df->name + b->fw->nf));
#ifndef DJGPP
	       if(!(e_we_sw & 1))
#endif
	       {  outp = e_open_view(0,0,MAXSCOL-1,MAXSLNS-1,f->fb->ws,1);
		  fk_locate(0,0);
		  fk_cursor(1);
#if  MOUSE
		  g[0] = 2;
		  fk_mouse(g);
#endif
		  e_sys_ini();
		  printf(e_msg[11], filen);
		  fflush(stdout);
	       }
#ifdef XWINDOW
	       if(((e_we_sw & 1) && e_x_system(filen)) ||
		      (!(e_we_sw & 1) && system(filen)))
#else
	       if(system(filen))
#endif
	       {  if(!(e_we_sw & 1)) e_sys_end();
		  e_error(e_msg[12], 0,f->fb);
	       }
#ifndef DJGPP
	       else if(!(e_we_sw & 1))
#else
	       else
#endif
	       {  printf(e_msg[13]);
		  fflush(stderr);
		  fflush(stdout);
		  fk_getch();
	       }
#ifndef DJGPP
	       if(!(e_we_sw & 1))
#endif
	       {  e_sys_end();
		  e_close_view(outp, 1);
		  fk_cursor(0);
#if  MOUSE
		  g[0] = 1;
		  fk_mouse(g);
#endif
	       }
	       c = cold;
	       break;
	    }
	    else
	    {  if(strstr(filen,"*") || strstr(filen,"?") )
	       {  c = AltF;  break;  }
	       if(c == AltP && fe && cn->mxedt > 0)
	       {  e_switch_window(winnum, fe);
		  e_close_window(cn->f[cn->mxedt]);
	       }
	       if(b->sw == 0 || !fe)  e_edit(cn, filen);
	       else
	       {  if((fp = fopen(filen,"rb")) == NULL)
		  {  c = cold; break;  }
		  if(access(filen, 2) != 0) f->ins = 8;
		  e_close_window(f);
		  e_switch_window(winnum, fe);
		  fe = cn->f[cn->mxedt]; be = fe->b; se = fe->s;
		  f = cn->f[cn->mxedt];
		  if(be->b.x != 0)
		  {  e_new_line(be->b.y+1, be);
		     if(*(be->bf[be->b.y].s+be->bf[be->b.y].len) != '\0')
		     (be->bf[be->b.y].len)++;
		     for(i = be->b.x;  i <= be->bf[be->b.y].len; i++)
				*(be->bf[be->b.y+1].s+i-be->b.x) = *(be->bf[be->b.y].s+i);
		     *(be->bf[be->b.y].s+be->b.x) = '\0';
		     be->bf[be->b.y].len = be->b.x;
		     be->bf[be->b.y+1].len = e_str_len(be->bf[be->b.y+1].s);
		     be->bf[be->b.y+1].nrc = e_str_nrc(be->bf[be->b.y+1].s);
		  }
		  se->ka.x = be->b.x; se->ka.y = be->b.y;
		  se->ke = e_readin(be->b.x, be->b.y, fp, be, fe->dtmd);
		  fclose(fp);
		  e_schirm(fe, 1);
	       }
	       if(h_error.sw == 0) getcwd(ftmp, 80);
	       if( (f->ed->dirct = REALLOC(f->ed->dirct, strlen(ftmp)+1)) == NULL)
	       {  e_error(e_msg[0], 0, f->fb); c = ESC; break;  }
	       strcpy(f->ed->dirct, ftmp);
	       if(svmode >= 0) f->ed->flopt = svmode;
	       return(0);
	    }
	 case AltY:
	 case AltW:
	 case AltS:
	    if((c == AltW && b->sw != 2) || (c == AltY && b->sw != 4)
                   || (c == AltS && b->sw != 4) || !fe || fe->ins == 8)
	    {  c = cold;  break;  }
	    if(strstr(filen,"*") || strstr(filen,"?") )
	    {  c = AltF;  break;  }
	    if( !access(filen, 0) )
	    {  sprintf(ftmp, "File %s exist\nDo You want to overwrite File ?",
			filen);
	       i = e_message(1, ftmp, f);
	       if( i == ESC ) {  c = ESC; break;  }
	       else if(i == 'N') {  c = AltF; break;  }
	    }
	    if(b->sw != 4)
	    {  dtp = fe->dirct;  ftp = fe->datnam;  }
	    else
	    {  FREE(fe->dirct);  FREE(fe->datnam);  }
	    e_mkeddir(fe, filen);
	    if(b->sw == 4 && c == AltY) e_crypt_save(fe);
	    else if(b->sw == 4) e_save(fe);
	    else
	    {  e_write(se->ka.x, se->ka.y, se->ke.x, se->ke.y, fe);
	       FREE(fe->dirct);  FREE(fe->datnam);
	       fe->dirct = dtp;  fe->datnam = ftp;
	    }
/*	       e_switch_window(winnum, f);     */
	    if(b->sw == 4 && (f->ed->edopt & 8))
	    {  if(fe->c_sw) FREE(fe->c_sw);
	       if(e_we_sw & 2) e_add_synt_tl(fe->datnam, fe);
	       if(fe->c_st)
	       {  if(fe->c_sw) FREE(fe->c_sw);
		  fe->c_sw = e_sc_txt(NULL, fe->b);
	       }
	       e_rep_win_tree(f->ed);
	    }
	    if(svmode >= 0) f->ed->flopt = svmode;
	    e_close_window(f);
	    return(0);
	 case EINFG:
	 case AltK:
	    if(b->sw != 0) {  c = cold;  break;  }
	    e_mk_newdir(f);
	    freedf(b->dd);  freedf(b->dw->df);
	    b->dw->df = e_ext_dirdir(b->cd,
		    b->dd = e_find_dir(SUDIR, f->ed->flopt & 02 ? 1 : 0));
	    for(i = 0; i < b->dd->anz
			&& strcmp(b->dd->name[i], "new.dir"); i++);
	    if((b->dw->nf = b->cd->anz + i) >= b->dw->df->anz)
	    b->dw->nf = b->cd->anz - 1;
	    if(b->dw->nf-b->dw->ia >= b->dw->ye - b->dw->ya)
	    b->dw->ia = b->dw->nf+b->dw->ya-b->dw->ye+1;
	    else if(b->dw->nf-b->dw->ia < 0) b->dw->ia = b->dw->nf;
	    cold = AltT;
	    c = AltM;
	    break;
	 case AltA:
	    if(b->sw != 0 && b->sw != 5) {  c = cold;  break;  }
	    if(b->sw == 0)
	    {  if(cold == AltF)
	       {  strcpy(filen, *(b->df->name + b->fw->nf));
		  e_attributes(filen, f);
		  freedf(b->df); FREE(b->fw->df);
		  b->fw->df = e_ext_dirfile(b->df =
			   e_find_files(b->rdfile, fmode), f->ed->flopt >> 9);
	       }
	       else if(cold == AltT && b->dw->nf >= b->cd->anz)
	       {  strcpy(ftmp, *(b->dd->name + b->dw->nf - b->cd->anz));
		  e_attributes(ftmp, f);
		  freedf(b->dd);  freedf(b->dw->df);
		  b->dw->df = e_ext_dirdir(b->cd, b->dd =
			      e_find_dir(SUDIR, f->ed->flopt & 02 ? 1 : 0));
	       }
	       c = cold;
	    }
	    else if(b->sw == 5)
	    {  FLWND *fw = (FLWND *)cn->f[cn->mxedt-1]->b;
	       if(cold != AltN)
	       strcpy(filen, *(b->df->name + b->fw->nf));
	       fw->df->anz++;
	       fw->df->name = REALLOC(fw->df->name,
				fw->df->anz * sizeof(char *));
	       for(i = fw->df->anz - 1; i > fw->nf; i--)
	       fw->df->name[i] = fw->df->name[i-1];
	       fw->df->name[i] = MALLOC(strlen(f->dirct)+strlen(filen)+2);
	       if(f->dirct[strlen(f->dirct)-1] == DIRC)
	       sprintf(fw->df->name[i], "%s%s", f->dirct, filen);
	       else sprintf(fw->df->name[i], "%s/%s", f->dirct, filen);
	       sprintf(ftmp, "File added to Project:\n%s",
						fw->df->name[i]);
	       e_message(0, ftmp, f);
	       fw->nf++;
	       if(fw->nf-fw->ia >= fw->ye - fw->ya)
	       fw->ia = fw->nf+fw->ya-fw->ye+1;
	       c = AltN;
	    }
	    break;
	 case F2:
	    c = AltS;
	    break;
	 case AltBl:
	    c = ESC;
	 default:
	    if(b->sw != 5)
	    {  if(svmode >= 0)
	       {  f->ed->flopt = svmode;
		  chdir(svdir);
		  getcwd(f->dirct, 80);
		  if((f->ed->dirct = REALLOC(f->ed->dirct,
                                         strlen(f->dirct)+1)) == NULL)
		  e_error(e_msg[0], 0, f->fb);
		  else  strcpy(f->ed->dirct, f->dirct);
	       }
#ifdef PROG
	       if(!e_tst_dfkt(f, c) ||
			((e_we_sw & 2) && !e_prog_switch(f, c)))
#else
	       if(!e_tst_dfkt(f, c))
#endif
	       {  if(b->sw != 0 && b->sw != 3) e_close_window(f);
		  return(0);
	       }
	       if(svmode >= 0) f->ed->flopt = 010023;
	    }
	    else if(c == ESC || (!(f->ed->edopt & 1) && c == AF3)
				|| (f->ed->edopt & 1 && c == CF4))
            {  chdir(svdir);
	       getcwd(f->dirct, 80);
	       if((f->ed->dirct = REALLOC(f->ed->dirct,
		  strlen(f->dirct)+1)) == NULL)
		  e_error(e_msg[0], 0, f->fb);
	       else  strcpy(f->ed->dirct, f->dirct);
	       e_close_window(f);
	       return(ESC);
	    }
	    c = cold;
	    break;
      }
   }
   if(b->sw == 5) 
   {  chdir(svdir);
      getcwd(f->dirct, 80);
      if((f->ed->dirct = REALLOC(f->ed->dirct, strlen(f->dirct)+1)) == NULL)
         e_error(e_msg[0], 0, f->fb);
      else  strcpy(f->ed->dirct, f->dirct);
      e_close_window(f);
      return(ESC);
   }
   e_close_window(f);
   if(svmode >= 0) f->ed->flopt = svmode;
   return(0);
}

#endif

struct dirfile *e_dfnull(df)
     struct dirfile *df;
{
   freedf(df);
   df = MALLOC(sizeof(struct dirfile));
   if(df) df->anz = 0;
   return(df);
}

int e_grep_file(file, string, sw)
     char *file;
     char *string;
     int sw;
{
   FILE *fp = fopen(file, "r");
   char str[256];
   int ret, nn;
   if(!fp) return(0);
   nn = strlen(string);
   while(fgets(str, 256, fp))
   {  if((sw & 32) == 0)
      {  if((sw & 128) != 0)
	 ret = e_strstr(0, strlen(str), str, string);
	 else
	 ret = e_ustrstr(0, strlen(str), str, string);
      }
      else
      {  if((sw & 128) != 0)
	 ret = e_rstrstr(0, strlen(str), str, string, &nn);
	 else
	 ret = e_urstrstr(0, strlen(str), str, string, &nn);
      }
      if( ret >= 0 && ( !(sw & 64)
	    || ( isalnum(str[ret+nn]) == 0
	    && (ret == 0 || isalnum(str[ret-1]) == 0) ) ) )
      {  fclose(fp);  return(1);  }
   }
   fclose(fp);
   return(0);
}

struct dirfile *e_search_files(dirct, file, string, df, sw)
     char *dirct;
     char *file;
     char *string;
     struct dirfile *df;
     int sw;
{
   struct dirfile *dd;
   char **tname, *tp, *tmp;
   int i;
   static int rec = 0;
   if(rec > MAXREC) return(df);
#ifdef DJGPP
   if(*dirct != DIRC && (dirct[1] != ':' || dirct[2] != DIRC))
#else
   if(*dirct != DIRC)
#endif
   {  if(!(tmp = MALLOC(256))) return(e_dfnull(df));
      getcwd(tmp, 256);
      tp = REALLOC(tmp, strlen(tmp) + strlen(dirct) + strlen(file) + 4);
      if(!tp) {  FREE(tmp);  return(e_dfnull(df));  }
      tmp = tp;
      if(tmp[strlen(tmp)-1] != DIRC)
      sprintf(tmp, "%s%c%s%c%s", tmp, DIRC, dirct, DIRC, file);
      else
      sprintf(tmp, "%s%s%c%s", tmp, dirct, DIRC, file);
   }
   else
   {  tmp = MALLOC(strlen(dirct) + strlen(file) + 2);
      if(!tmp) return(e_dfnull(df));
      if(dirct[strlen(dirct)-1] != DIRC)
      sprintf(tmp, "%s%c%s", dirct, DIRC, file);
      else
      sprintf(tmp, "%s%s", dirct, file);
   }
   if(df == NULL)
   {  if(!(df = MALLOC(sizeof(struct dirfile))))
      {  FREE(tmp);  return(df);  }
      df->anz = 0;
      if(!(df->name = MALLOC(sizeof(char *))))
      {  FREE(tmp);  FREE(df);  return(df);  }
   }
   dd = e_find_files(tmp, 0);
   if(dd && dd->anz > 0)
   {  if(!(sw & 1024))
      {  df->anz++;
	 tname = REALLOC(df->name, df->anz * sizeof(char *));
	 if(!tname) {  freedf(dd);  FREE(tmp);  return(e_dfnull(df));  }
	 df->name = tname;
	 if(!(df->name[df->anz-1] = MALLOC(strlen(tmp)+1)))
	 {  freedf(dd);  FREE(tmp);  df->anz--;  return(df);  }
	 strcpy(df->name[df->anz-1], tmp);
      }
      else
      {  for(i = 0; i < dd->anz; i++)
	 {  if(!(tp = MALLOC(strlen(tmp) + strlen(dd->name[i]) + 2)))
	    {  freedf(dd);  FREE(tmp);  return(df);  }
	    if(tmp[strlen(tmp)-1] != DIRC)
	    sprintf(tp, "%s%c%s", tmp, DIRC, dd->name[i]);
	    else
	    sprintf(tp, "%s%s", tmp, dd->name[i]);
	    if(e_grep_file(tp, string, sw))
	    {  df->anz++;
	       tname = REALLOC(df->name, df->anz * sizeof(char *));
	       if(!tname)
	       {  freedf(dd); FREE(tmp); FREE(tp); return(e_dfnull(df));  }
	       df->name = tname;
	       df->name[df->anz-1] = tp;
	    }
	    else FREE(tp);
	 }
      }
   }
   freedf(dd);
   FREE(tmp);
   if(!(sw & 512)) return(df);
   
   if(!(tmp = MALLOC(strlen(dirct) + strlen(SUDIR) + 3))) return(df);
   if(dirct[strlen(dirct)-1] != DIRC)
   sprintf(tmp, "%s%c%s", dirct, DIRC, SUDIR);
   else
   sprintf(tmp, "%s%s", dirct, SUDIR);
   dd = e_find_dir(tmp, 0);
   FREE(tmp);
   if(!dd) return(df);
   rec++;
   for(i = 0; i < dd->anz; i++)
   {  tmp = MALLOC(strlen(dirct) + strlen(dd->name[i]) + 3);
      if(!tmp) {  rec--; freedf(dd);  return(df);  }
      if(dirct[strlen(dirct)-1] != DIRC)
      sprintf(tmp, "%s%c%s", dirct, DIRC, dd->name[i]);
      else
      sprintf(tmp, "%s%s", dirct, dd->name[i]);
      df = e_search_files(tmp, file, string, df, sw);
      FREE(tmp);
      if(!df) {  rec--; freedf(dd);  return(df);  }
   }
   freedf(dd);
   rec--;
   return(df);
}

/*   Datei drucken    */

#ifndef NOPRINTER
int e_drucke_datei(f)
     FENSTER *f;
{
   char str[256], *dp;
   int c, sins = f->ins;
   for(c = f->ed->mxedt; c > 0 && f->ed->f[c]->dtmd <= 'Z'; c--);
   if(c <= 0) return(0);
   f = f->ed->f[c];
   sprintf(str, "File: %s\nWant you to print file?", f->datnam);
   c = e_message(1, str, f);
   if( c != 'Y' ) return(0);
   dp = f->dirct;
   f->dirct = e_tmp_dir;
   f->ins = 0;
   e_save(f);
   f->dirct=dp;
   f->ins = sins;
   sprintf(str, "cd %s; %s %s", e_tmp_dir, PRNTCMD, f->datnam);
   if(system(str)) e_error(e_msg[14], 0, f->fb);
   sprintf(str, "%s/%s", e_tmp_dir, f->datnam);
   remove(str);
   return(0);
}
#else
int e_drucke_datei(FENSTER *f)
{
   return(e_error(e_msg[22], 0, f->fb));
}
#endif

char *e_ret_wastefile(file, path)
     char *file;
     char *path;
{
   static char *wastebasket = NULL;
   int i;
   if(!wastebasket)
   {  char tmp[256], tmp2[256];
#ifdef DJGPP
      char *etmp;
      if(!(etmp = getenv("GO32TMP")))
      sprintf(tmp, "c:\\%s", WASTEBASKET);
      else
      sprintf(tmp, "%s\\%s", etmp, WASTEBASKET);
#else
      sprintf(tmp, "%s/%s", getenv("HOME"), WASTEBASKET);
#endif
      if(access(tmp, 0)) mkdir(tmp, 0700);
      getcwd(tmp2, 256);
      chdir(tmp);
      getcwd(tmp, 256);
      if(!(wastebasket = MALLOC(strlen(tmp) + 1))) return(NULL);
      strcpy(wastebasket, tmp);
      chdir(tmp2);
   }
   if(access(wastebasket, 0)) mkdir(wastebasket, 0700);
   if(file[0] == '\0') return(strcpy(path, wastebasket));
   for(i = strlen(file) - 1; i >= 0 && file[i] != DIRC; i--);
   sprintf(path, "%s%c%s", wastebasket, DIRC, file+i+1);
   return(path);
}
/*
int e_duplicate(char *file)
{
    char command[256];
    int ret;
#ifdef XWINDOW
    if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
    sprintf(command, "cp -r %s %s.dup > /dev/null 2> /dev/null", file, file);
    ret = system(command);
#ifdef XWINDOW
    if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
    return(ret);
}
*/
int e_remove_dir(dirct, file, f, rec)
     char *dirct;
     char *file;
     FENSTER *f;
     int rec;
{
   PIC *pic = NULL;
   char *tmp;
   int i, ret, svmode = f->ed->flopt;
   struct dirfile *dd;
   
   if(rec > MAXREC) return(0);
   tmp = MALLOC(256);
   if(f->ed->flopt & 0100)
   {  e_ret_wastefile(dirct, tmp);
      i = strlen(tmp);
      if(strncmp(tmp, dirct, i))
      {  ret = e_renocpy_dir(dirct, file, tmp, f, 0, 0);
	 FREE(tmp);
	 return(ret);
      }
   }
   sprintf(tmp, "%s%c%s", dirct, DIRC, file);
   dd = e_find_files(tmp, f->ed->flopt & 01 ? 1 : 0);
   if(!rec && (f->ed->flopt & 0200) && dd->anz > 0)
   {  if((ret = e_dir_del_options(f)) < 0)
      {  freedf(dd);  FREE(tmp);  return(ret == ESC ? 1 : 0);  }
      if(ret) f->ed->flopt |= 0200;
      else f->ed->flopt &= ~0200;
      rec = -1;
   }
   for(i = 0; i < dd->anz; i++)
   {  sprintf(tmp, "Remove File:\n%s%c%s", dirct, DIRC, dd->name[i]);
      if(f->ed->flopt & 0200)
      {  if(pic) {  e_close_view(pic, 1);  pic = NULL;  }
	 ret = e_message(1, tmp, f);
      }
      else ret = 'Y';
      if(ret == ESC)
      {  freedf(dd);  FREE(tmp);  f->ed->flopt = svmode;  return(1);  }
      else if(ret == 'Y')
      {  sprintf(tmp, "%s%c%s", dirct, DIRC, dd->name[i]);
	 if(e_mess_win("Remove", tmp, &pic, f)) break;
	 remove(tmp);
      }
   }
   if(pic) e_close_view(pic, 1);
   freedf(dd);
   if(f->ed->flopt & 010000)
   {  sprintf(tmp, "%s%c%s", dirct, DIRC, SUDIR);
      dd = e_find_dir(tmp, f->ed->flopt & 02 ? 1 : 0);
      if(!rec && (f->ed->flopt & 0200) && dd->anz > 0)
      {  if((ret = e_dir_del_options(f)) < 0)
	 {  freedf(dd);  FREE(tmp);  return(ret == ESC ? 1 : 0);  }
	 if(ret) f->ed->flopt |= 0200;
	 else f->ed->flopt &= ~0200;
      }
      else if(rec < 0) rec = 0;
      for(rec++, i = 0; i < dd->anz; i++)
      {  sprintf(tmp, "%s%c%s", dirct, DIRC, dd->name[i]);
	 if(e_remove_dir(tmp, file, f, rec))
	 {  freedf(dd);  FREE(tmp);  f->ed->flopt = svmode;  return(1);  }
      }
      freedf(dd);
   }
   FREE(tmp);
   rmdir(dirct);
   f->ed->flopt = svmode;
   return(0);
}

int e_remove(file, f)
     char *file;
     FENSTER *f;
{
   struct stat buf[1];
   char tmp[256];
   int ret;
   
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   stat(file, buf);
#ifndef DJGPP
   if(!(buf->st_mode & 040000) || (ret = readlink(file, tmp, 256)) >= 0)
#else
   if(!(buf->st_mode & 040000))
#endif
   {  if((f->ed->flopt & 0100))
      {  e_ret_wastefile(file, tmp);
	 ret = strlen(tmp);
	 if(strncmp(tmp, file, ret))
	 {  e_rename(file, tmp, f);   return(0);  }
      }
      if(f->ed->flopt & 0200)
      {  sprintf(tmp, "Remove File:\n%s", file);
	 ret = e_message(1, tmp, f);
      }
      else ret = 'Y';
      if(ret == 'Y') remove(file);
   }
   else e_remove_dir(file, f->ed->fd.file, f, 0);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   return(0);
}

#define E_C_BUFFERSIZE 524288   /*   1/2  Mega Byte   */

int fk_copy(old, new)
     char *old;
     char *new;
{
   struct stat buf[1];
   int ret;
   char *buffer;
   FILE *fpo, *fpn;
   if(!(fpo = fopen(old, "rb"))) return(-1);
   if(!(fpn = fopen(new, "wb"))) return(-2);
   if(!(buffer = malloc(E_C_BUFFERSIZE))) return(-3);
   do
   {  ret = fread(buffer, 1, E_C_BUFFERSIZE, fpo);
      if(fwrite(buffer, 1, ret, fpn) != ret)
      {  fclose(fpo);  fclose(fpn);  free(buffer);   return(-4);  }
   } while(ret == E_C_BUFFERSIZE);
   fclose(fpo);
   fclose(fpn);
   free(buffer);
   stat(old, buf);
   chmod(new, buf->st_mode);
   return(0);
}

#ifndef DJGPP
/* #define fk_rename(o, n) rename(o, n)  */
#define fk_rename rename
#ifndef NOSYMLINKS
int fk_link(fl, ln, sw)
     char *fl;
     char *ln;
     int sw;
{
   return((sw || link(fl, ln)) ? symlink(fl, ln) : 0);
}

int fk_ren_link(old, ln, fl)
     char *old;
     char *ln;
     char *fl;
{
   return(symlink(fl, ln) ? -1 : remove(old));
}
#endif

int e_link(file, newname, f)
     char *file;
     char *newname;
     FENSTER *f;
{
   return(e_renocpy(file, newname, f, 2));
}
#else
int fk_rename(char *old, char *new)
{
   extern struct EXT h_error;
   char s[128];
   int ret;
   if((old[1] != ':' && new[1] != ':') ||
		(old[1] == ':' && new[1] == ':' && old[0] == new[0]) ||
	(old[1] == ':' && new[1] != ':' && old[0] == 'A'+getdisk()) ||
	(new[1] == ':' && old[1] != ':' && new[0] == 'A'+getdisk()) )
   {  if(!(ret = rename(old, new))) return(0);  }
   else
   {  if(!(ret = fk_copy(old, new))) {  remove(old);  return(0);  }  }
   sprintf(s, "E: %d;  O: %s;  N: %s", ret, old, new);
   e_error(s, 0, h_error.cn->fb);
   return(1);
}
#endif

int e_copy(file, newname, f)
     char *file;
     char *newname;
     FENSTER *f;
{
   return(e_renocpy(file, newname, f, 1));
}

int e_rename_dir(dirct, file, newname, f, rec)
     char *dirct;
     char *file;
     char *newname;
     FENSTER *f;
     int rec;
{
   return(e_renocpy_dir(dirct, file, newname, f, rec, 0));
}


int e_renocpy_dir(dirct, file, newname, f, rec, sw)
     char *dirct;
     char *file;
     char *newname;
     FENSTER *f;
     int rec;
     int sw;
{
   char tmp[256], ntmp[256], mtmp[512];
   int i, ret, mode;
   struct dirfile *dd;
   struct stat buf[1];
   PIC *pic = NULL;
   if(rec > MAXREC) return(0);
#ifndef DJGPP
   if(sw == 0 && (ret = access(newname, 0)) && file[0] == '*' && file[1] == '\0')
   return(!sw ? rename(dirct, newname) :
			fk_link(dirct, newname, f->ed->flopt & 0400));
#else
   if((sw == 0 || sw == 2) && (ret = access(newname, 0))
		&& file[0] == '*' && file[1] == '\0')
   {  for(i = strlen(dirct); i >= 0 && dirct[i] != '\\' && dirct[i] != '/'
		&& dirct[i] != ':'; i--);
      if(i < 0)
      {  for(i = strlen(newname); i >= 0 && newname[i] != '\\'
		&& newname[i] != '/' && newname[i] != ':'; i--);
	 if(i < 0) return(rename(dirct, newname));
      }
   }
#endif
   stat(dirct, buf);
   mkdir(newname, buf->st_mode);
   
   if(f->ed->flopt & 010000)
   {  sprintf(tmp, "%s%c%s", dirct, DIRC, SUDIR);
      dd = e_find_dir(tmp, f->ed->flopt & 02 ? 1 : 0);
      for(rec++, i = 0; i < dd->anz; i++)
      {  sprintf(tmp, "%s%c%s", dirct, DIRC, dd->name[i]);
	 sprintf(ntmp, "%s%c%s", newname, DIRC, dd->name[i]);
	 if(e_renocpy_dir(tmp, file, ntmp, f, rec, sw))
	 {  freedf(dd);  return(1);  }
      }
      freedf(dd);
   }
   sprintf(tmp, "%s%c%s", dirct, DIRC, file);
   dd = e_find_files(tmp, f->ed->flopt & 01 ? 1 : 0);
   mode = f->ed->flopt;
   f->ed->flopt &= ~0200;
   for(i = 0; i < dd->anz; i++)
   {  sprintf(ntmp, "%s%c%s", newname, DIRC, dd->name[i]);
      ret = 'Y';
      if(!access(ntmp, 0))
      {  if(f->ed->flopt & 040)
	 {  sprintf(tmp, "File %s exist !\nOverwrite File ?", ntmp);
	    if(pic) {  e_close_view(pic, 1);  pic = NULL;  }
	    ret = e_message(1, tmp, f);
	    if(ret == 'Y') e_remove(ntmp, f);
	    else if(ret == ESC) {  freedf(dd);  return(1);  }
	 }
	 else if(f->ed->flopt & 020) e_remove(ntmp, f);
      }
      sprintf(tmp, "%s%c%s", dirct, DIRC, dd->name[i]);
      if(ret == 'Y')
      {  sprintf(mtmp, "%s %s", tmp, ntmp);
	 if(e_mess_win(!sw ? "Rename" : "Copy", mtmp, &pic, f)) break;
	 if(sw == 0) fk_rename(tmp, ntmp);
	 else if(sw == 1) fk_copy(tmp, ntmp);
#ifndef DJGPP
	 else if(sw == 2) fk_link(tmp, ntmp, f->ed->flopt & 0400);
#endif
      }
   }
   if(pic) e_close_view(pic, 1);
   f->ed->flopt = mode;
   if(sw == 0) rmdir(dirct);
   freedf(dd);
   return(0);
}

int e_rename(file, newname, f)
     char *file;
     char *newname;
     FENSTER *f;
{
   return(e_renocpy(file, newname, f, 0));
}

int e_renocpy(file, newname, f, sw)
     char *file;
     char *newname;
     FENSTER *f;
     int sw;
{
   struct stat buf[1];
   char tmp[256], tmpl[256];
   int ln = -1, ret = 'Y';
   
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   stat(file, buf);
#ifndef DJGPP
   if (sw == 0) ln = readlink(file, tmpl, 256);
#endif
   if(buf->st_mode & 040000 && ln < 0)
   e_renocpy_dir(file, f->ed->fd.file, newname, f, 0, sw);
   else
   {  if( !access(newname, 0) )
      {  if(f->ed->flopt & 020) e_remove(newname, f);
	 else if(f->ed->flopt & 040)
	 {  sprintf(tmp, "File %s exist\nRemove File ?", newname);
	    ret = e_message(1, tmp, f);
	    if(ret == 'Y') e_remove(newname, f);
	 }
      }
      if(ret == 'Y')
      {  if(sw == 1) fk_copy(file, newname);
#ifndef DJGPP
	 else if(sw == 2) fk_link(file, newname, f->ed->flopt & 0400);
	 else if(sw == 0 && ln < 0) fk_rename(file, newname);
	 else if(sw == 0)
	 {  tmpl[ln] = '\0';
	    for( ; ln >= 0 && tmpl[ln] != DIRC; ln--);
	    if(ln < 0)
	    {  sprintf(tmp, "%s%c%s", f->dirct, DIRC, tmpl);
	       fk_ren_link(file, newname, tmp);
	    }
	    else  fk_ren_link(file, newname, tmpl);
	 }
#else
	 else if(sw == 0) fk_rename(file, newname);
#endif
      }
   }
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   return(0);
}
#ifndef DJGPP
int e_duplicate(file, f)
     char *file;
     FENSTER *f;
{
   char dupl[256];
   int ret;
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   sprintf(dupl, "%s.dup", file);
   ret = e_renocpy(file, dupl, f, 1);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   return(ret);
}
#else
int e_duplicate(char *file, FENSTER *f)
{
   char dupl[128];
   int i, len = strlen(file);
   strcpy(dupl, file);
   for(i = len - 1; i >= 0 && dupl[i] != '.'  && dupl[i] != ':'
		&& dupl[i] != '/'  && dupl[i] != '\\'; i--);
   if(i < 0 || dupl[i] != '.')  strcat(dupl, ".~");
   else  dupl[len-1] = '~';
   return(e_renocpy(file, dupl, f, 1));
}
#endif
int e_mk_newdir(f)
     FENSTER *f;
{
   char dirct[256];
   int msk, mode;
   umask(msk = umask(077));
   mode = 0777 & ~msk;
   if(f->dirct[strlen(f->dirct)-1] != DIRC)
   sprintf(dirct, "%s/new.dir" , f->dirct);
   else sprintf(dirct, "%snew.dir" , f->dirct);
   return(mkdir(dirct, mode));
}

int e_fl_mng_options(f)
     FENSTER *f;
{
   int ret;
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o) return(-1);
   o->xa = 8;  o->ya = 3;  o->xe = 69;  o->ye = 23;
   o->bgsw = AltO;
   o->name = "File-Manager-Options";
   o->crsw = AltO;
   e_add_txtstr(4, 2, "Directories:", o);
   e_add_txtstr(35, 13, "Wastebasket:", o);
   e_add_txtstr(4, 8, "Move/Copy:", o);
   e_add_txtstr(35, 8, "Remove:", o);
   e_add_txtstr(35, 2, "Sort Files by:", o);
   e_add_txtstr(4, 13, "Links on Files:", o);
   e_add_sswstr(5, 3, 12, AltF, f->ed->flopt & 1, "Show Hidden Files      ", o);
   e_add_sswstr(5, 4, 12, AltD, f->ed->flopt & 2 ? 1 : 0, "Show Hidden Directories", o);
   e_add_sswstr(5, 5, 2, AltK, f->ed->flopt & 010000 ? 1 : 0, "ReKursive Actions      ", o);
   e_add_sswstr(36, 6, 0, AltR, f->ed->flopt & 04000 ? 1 : 0, "Reverse Order    ", o);
   e_add_pswstr(0, 36, 14, 0, AltP, 0, "Prompt for Delete", o);
   e_add_pswstr(0, 36, 15, 10, AltE, 0, "Delete at Exit   ", o);
   e_add_pswstr(0, 36, 16, 8, AltL, f->ed->flopt & 8 ? 0 :
		(f->ed->flopt & 4 ? 1 : 2), "Don't DeLete     ", o);
   e_add_pswstr(1, 36, 3, 0, AltN, 0, "Name             ", o);
   e_add_pswstr(1, 36, 4, 1, AltI, 0, "TIme             ", o);
   e_add_pswstr(1, 36, 5, 0, AltB, f->ed->flopt & 01000 ? 1 :
		(f->ed->flopt & 02000 ? 2 : 0), "Bytes            ", o);
   e_add_pswstr(2, 5, 9, 12, AltQ, 0, "Prompt for eQual Files ", o);
   e_add_pswstr(2, 5, 10, 1, AltV, 0, "OVerwrite equal Files  ", o);
   e_add_pswstr(2, 5, 11, 4, AltT, f->ed->flopt & 32 ? 0 :
		(f->ed->flopt & 16 ? 1 : 2), "Don'T overwrite        ", o);
   e_add_pswstr(3, 5, 14, 4, AltH, 0, "Try Hardlink           ", o);
   e_add_pswstr(3, 5, 15, 7, AltS, f->ed->flopt & 0400 ? 1 : 0, "Always Symbolic Link   ", o);
   e_add_pswstr(4, 36, 9, 5, AltW, 0, "into Wastebasket ", o);
   e_add_pswstr(4, 36, 10, 0, AltA, 0, "Absolut (Prompt) ", o);
   e_add_pswstr(4, 36, 11, 6, AltM, f->ed->flopt & 64 ? 0 :
		(f->ed->flopt & 128 ? 1 : 2), "No ProMpt        ", o);
   e_add_bttstr(16, 18, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(38, 18, -1, ESC, "Cancel", NULL, o);
   ret = e_opt_kst(o);
   if(ret != ESC)
   {  f->ed->flopt = o->sstr[0]->num + (o->sstr[1]->num << 1)
	   + (o->sstr[2]->num ? 010000 : 0)
	   + (o->sstr[3]->num ? 04000 : 0)
	   + (o->pstr[0]->num ? (o->pstr[0]->num == 1 ? 4 : 0) : 8)
	   + (o->pstr[2]->num ? (o->pstr[2]->num == 1 ? 16 : 0) : 32)
	   + (o->pstr[4]->num ? (o->pstr[4]->num == 1 ? 128 : 0) : 64)
	   + (o->pstr[1]->num ? (o->pstr[1]->num == 1 ? 01000 : 02000) : 0)
	   + (o->pstr[3]->num ? 0400 : 0);
   }
   freeostr(o);
   return(0);
}

int e_attributes(filen, f)
     char *filen;
     FENSTER *f;
{
   struct stat buf[1];
   int mode, ret;
   W_OPTSTR *o = e_init_opt_kst(f);
   
   stat(filen, buf);
   mode = buf->st_mode;
   if(!o) return(-1);
   o->xa = 14;  o->ya = 4;  o->xe = 62;  o->ye = 13;
   o->bgsw = AltO;
   o->name = "Attributes";
   o->crsw = AltO;
   e_add_txtstr(3, 2, "User:", o);
   e_add_txtstr(33, 2, "Other:", o);
   e_add_txtstr(18, 2, "Group:", o);
   e_add_sswstr(4, 3, 0, AltR, mode&256 ? 1 : 0, "Read   ", o);
   e_add_sswstr(4, 4, 0, AltW, mode&128 ? 1 : 0, "Write  ", o);
   e_add_sswstr(4, 5, 1, AltX, mode&64 ? 1 : 0, "EXecute", o);
   e_add_sswstr(19, 3, 2, AltA, mode&32 ? 1 : 0, "ReAd   ", o);
   e_add_sswstr(19, 4, 2, AltI, mode&16 ? 1 : 0, "WrIte  ", o);
   e_add_sswstr(19, 5, 0, AltE, mode&8 ? 1 : 0, "Execute", o);
   e_add_sswstr(34, 3, 3, AltD, mode&4 ? 1 : 0, "ReaD   ", o);
   e_add_sswstr(34, 4, 3, AltT, mode&2 ? 1 : 0, "WriTe  ", o);
   e_add_sswstr(34, 5, 4, AltU, mode&1, "ExecUte", o);
   e_add_bttstr(12, 7, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(29, 7, -1, ESC, "Cancel", NULL, o);
   ret = e_opt_kst(o);
   if(ret != ESC)
   {  mode = (o->sstr[0]->num << 8) + (o->sstr[1]->num << 7) +
	       (o->sstr[2]->num << 6) + (o->sstr[3]->num << 5) +
	       (o->sstr[4]->num << 4) + (o->sstr[5]->num << 3) +
	       (o->sstr[6]->num << 2) + (o->sstr[7]->num << 1) +
	       o->sstr[8]->num;
      chmod(filen, mode);
   }
   freeostr(o);
   return(0);
}

int e_dir_del_options(f)
     FENSTER *f;
{
   int ret;
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o) return(-1);
   o->xa = 19;  o->ya = 11;  o->xe = 53;  o->ye = 19;
   o->bgsw = AltO;
   o->name = "Message";
   o->crsw = AltO;
   e_add_txtstr(4, 2, "Delete Directory:", o);
   e_add_pswstr(0, 5, 3, 0, AltD, 0, "Delete without Prompt", o);
   e_add_pswstr(0, 5, 4, 0, AltP, 1, "Prompt for Files     ", o);
   e_add_bttstr(7, 6, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(22, 6, -1, ESC, "Cancel", NULL, o);
   ret = e_opt_kst(o);
   ret = (ret == ESC) ? -1 : o->pstr[0]->num;
   freeostr(o);
   return(ret);
}

int e_sh_wastebasket(f)
     FENSTER *f;
{
   return(e_file_again(6, f));
}

int e_del_wastebasket(f)
     FENSTER *f;
{
   char tmp[256];
   int ret, mode = f->ed->flopt;
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   f->ed->flopt = 010023;
   ret = e_remove_dir(e_ret_wastefile("", tmp), "*", f, 0);
   f->ed->flopt = mode;
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   return(ret);
}

int e_quit_wastebasket(f)
     FENSTER *f;
{
   char tmp[256];
   int ret = 0, mode = f->ed->flopt;
   if(mode & 010) f->ed->flopt = 010223;
   else if(mode & 04) f->ed->flopt = 010023;
   if((mode & 010) || (mode & 04))
   {
#ifdef XWINDOW
      if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
      ret = e_remove_dir(e_ret_wastefile("", tmp), "*", f, 0);
#ifdef XWINDOW
      if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   }
   f->ed->flopt = mode;
   return(ret);
}

/*  Struktur bis zur Arbeits-Directory erstellen  */

#ifndef VERSION14

struct dirfile *e_mk_cur_dir(sw)
     int sw;
{
   struct dirfile *df = MALLOC(sizeof(struct dirfile));
   char buf[256];
   char tmp[256];
   char **dftmp;
   int buflen = 256, maxd = 10, i, j, k;
   if (getcwd(buf, buflen) == NULL)
#ifdef DJGPP
   {  chdir("\\");
#else
   {  chdir(getenv("HOME"));
#endif
      if (getcwd(buf, buflen) == NULL) return(NULL);
   }
   df->name = MALLOC(sizeof(char *) * maxd);
   df->anz = 0;
   e_ret_wastefile("", tmp);
   i = strlen(tmp);
   if(sw == 1)
   {  df->anz = 1;
      *(df->name) = MALLOC(12*sizeof(char));
      strcpy(*(df->name), "Wastebasket");
      if(!buf[i]) return(df);
      i++;
   }
#ifdef DJGPP
   else
   {  i = 0;
      df->anz = 1;
      *(df->name) = MALLOC(7*sizeof(char));
      strcpy(*(df->name), "Drives");
   }
#else
   else i = 0;
#endif
   for ( j = 0; i < buflen; i++, j++ )
   {  tmp[j] = buf[i];
      if(tmp[j] == DIRC || tmp[j] == '\0')
      {  if(buf[i] == '\0' && j == 0) return(df);
#ifdef DJGPP
	 if(df->anz == 1) j++;
#else
	 if(df->anz == 0) j++;
#endif
	 tmp[j] = '\0';
	 if(df->anz >= maxd)
	 {  maxd += 10;
	    dftmp = df->name;
	    df->name = MALLOC(sizeof(char *) * maxd);
	    for(k = 0; k < maxd-10; k++)  *(df->name+k) = *(dftmp+k);
	    FREE(dftmp);
	 }
	 *(df->name + df->anz) = MALLOC((strlen(tmp)+1)*sizeof(char));
	 strcpy(*(df->name+df->anz), tmp);
	 df->anz++;
	 j = -1;
	 if(buf[i] == '\0') return(df);
      }
   }
   return(df);
}

#endif

int e_ed_man(str, f)
     char *str;
     FENSTER *f;
{
   char command[256], tstr[120];
   char cc, hstr[80], nstr[10];
   int mdsv = f->ed->dtmd, bg, i, j = 0;
   BUFFER *b;
   if(!str) return(0);
   while(isspace(*str++));
   if(!*--str) return(0);
   for(i = f->ed->mxedt; i >= 0; i--)
   {  if(!strcmp(f->ed->f[i]->datnam, str))
      {  e_switch_window(f->ed->edt[i], f);  return(0);  }
   }
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   sprintf(tstr, "%s/%s", e_tmp_dir, str);
   for(i=0; (hstr[i] = str[i]) && str[i] != '(' && str[i] != ')'; i++);
   hstr[i] = '\0';
   if(str[i] == '(')
/*
   for(++i; (nstr[j] = tolower(str[i])) && str[i] != ')'
					&& str[i] != '('; i++, j++);
*/
   for(++i; (nstr[j] = str[i]) && str[i] != ')'
					&& str[i] != '('; i++, j++);
   nstr[j] = '\0';
   while(1)
#ifdef MAN_S_OPT
   {  if(!nstr[0])
         sprintf(command, " man %s > \'%s\' 2> /dev/null", hstr, tstr);
      else
         sprintf(command, " man -s %s %s > \'%s\' 2> /dev/null", nstr, hstr, tstr);
#else
   {  sprintf(command, " man %s %s > \'%s\' 2> /dev/null", nstr, hstr, tstr);
#endif
      system(command);
      chmod(tstr, 0400);
      f->ed->dtmd = 'h';
      e_edit(f->ed, tstr);
      f->ed->dtmd = mdsv;
      f = f->ed->f[f->ed->mxedt];
      b = f->b;
      if(b->mxlines > 1 || !nstr[1]) break;
      nstr[1] = '\0';
      chmod(tstr, 0600);
      remove(tstr);
      e_close_window(f);
   }
   if(b->mxlines == 1 && b->bf[0].len == 0)
   {  e_ins_nchar(f->b, f->s, "No manual entry for ", 0, 0, 20);
      e_ins_nchar(f->b, f->s, hstr, b->b.x, b->b.y, strlen(hstr));
      e_ins_nchar(f->b, f->s, ".", b->b.x, b->b.y, 1);
   }
   for(i = 0; i < b->mxlines; i++)
   if(b->bf[i].len == 0 && (i == 0 || b->bf[i-1].len == 0))
   {  e_del_line(i, b, f->s);  i--;  }
   for(bg = 0; bg < b->bf[0].len && isspace(b->bf[0].s[bg]); bg++);
   if(bg == b->bf[0].len) bg = 0;
   for(i = 0; i < b->mxlines && e_strcncmp(b->bf[i].s+bg, "SEE ALSO", 8); i++);
   if(i < b->mxlines)
   for(bg = 0, i++; i < b->mxlines && b->bf[i].len > 0 && bg >= 0; i++)
   {  bg = 0;
      while(b->bf[i].s[bg])
	 {  for(; isspace(b->bf[i].s[bg]); bg++);
	    if(!b->bf[i].s[bg]) continue;
	    for(j = bg+1; b->bf[i].s[j] && b->bf[i].s[j] != ',' && b->bf[i].s[j] != '.'
			&& b->bf[i].s[j] != ' ' && b->bf[i].s[j] != '('; j++);
	    if(b->bf[i].s[j] != '(') {  bg = -1;  break;  }
	    for(j++; b->bf[i].s[j] && b->bf[i].s[j] != ',' 
							&& b->bf[i].s[j] != '.'; j++);
	    cc = HFB;
	    e_ins_nchar(b, f->s, &cc, bg, i, 1);
	    cc = HED;
	    e_ins_nchar(b, f->s, &cc, ++j, i, 1);
	    j++;
	    if(b->bf[i].s[j]) j++;
	    bg = j;
	 }
   }
   b->b.x = b->b.y = 0;
   chmod(tstr, 0600);
   remove(tstr);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   e_schirm(f, 1);
   return(0);
}

int e_funct(f)
     FENSTER *f;
{
   char str[80];
   if(f->ed->hdf && f->ed->hdf->anz > 0) strcpy(str, f->ed->hdf->name[0]);
   else str[0] = '\0';
   if(e_add_arguments(str, "Function", f, 0, AltF, &f->ed->hdf))
   {  f->ed->hdf = e_add_df(str, f->ed->hdf);
      e_ed_man(str, f);
   }
   return(0);
}

struct dirfile *e_make_funct(man)
     char *man;
{
   struct dirfile *df = NULL, *dout = MALLOC(sizeof(struct dirfile));
   char sustr[250], subpath[250], manpath[250];
   int ret = 0, n, i = 0, j, k, l = 0;
#ifdef CATMAN
   struct dirfile *dd = NULL;
   int m;
#endif
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   dout->anz = 0;
   dout->name = NULL;
#ifdef CATMAN
   if(getenv("CATMAN")) strcpy(manpath, getenv("CATMAN"));
   else manpath[0] = '\0';
#else
   manpath[0] = '\0';
   if(getenv("MANPATH")) strcpy(manpath, getenv("MANPATH"));
   if(manpath[0] == '\0') strcpy(manpath, "/usr/man:/usr/local/man");
#endif
   while(manpath[i])
   {  for(n = 0; (subpath[n] = manpath[i]) && manpath[i] != PTHD; i++, n++);
      subpath[n] = '\0';
      if(manpath[i]) i++;
#ifdef CATMAN
      sprintf(sustr, "%s/C/*_man", subpath);
      dd = e_find_dir(sustr);
      for(m = 0; m < dd->anz; m++)  {
	 sprintf(sustr, "%s/C/%s/cat%s/*", subpath, dd->name[m], man);
#else
      sprintf(sustr, "%s/man%s/*", subpath, man);
#endif
      df = e_find_files(sustr, 0);
      if(!df->anz) {  freedf(df);  continue;  }
      for(j = 0; j < df->anz; j++)
      {  for(k = 0; *(df->name[j]+k) != '\0' && *(df->name[j]+k) != '.'; k++);
	 if(*(df->name[j]+k))
	 {  df->name[j] = REALLOC(df->name[j],
				(l = strlen(df->name[j])+2)*sizeof(char));
	    *(df->name[j]+k) = '(';
	    *(df->name[j]+l-2) = ')';
	    *(df->name[j]+l-1) = '\0';
	 }
      }
      if(!dout->name) dout->name = MALLOC(df->anz * sizeof(char *));
      else dout->name = REALLOC(dout->name,
				(df->anz+dout->anz)*sizeof(char *));
      for(j = 0; j < df->anz; j++)
      {  for(k = 0; k < dout->anz; k++)
	 {  if(!(ret = strcmp(df->name[j], dout->name[k])))
	    {  FREE(df->name[j]);  break;  }
	    else if(ret < 0) break;
	 }
	 if(!ret && dout->anz) continue;
	 for(l = dout->anz; l > k; l--) dout->name[l] = dout->name[l-1];
	 dout->name[k] = df->name[j];
	 dout->anz++;
      }
      FREE(df);
#ifdef CATMAN
   }   freedf(dd);
#endif
}
#ifdef XWINDOW
if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
return(dout);
}

#ifdef PROG
extern struct dirfile **e_p_df;
#endif

int e_data_first(sw, cn, nstr)
     int sw;
     ECNT *cn;
     char *nstr;
{
    extern char *e_hlp_str[];
    extern WOPT *gblst, *oblst;
    FENSTER *f;
    int i, j;
    struct dirfile *df = NULL;
    FLWND *fw;
    if(cn->mxedt >= MAXEDT)
    {   e_error(e_msg[3], 0, cn->fb);
	return(-1);
    }
    for (j = 1; j <= MAXEDT; j++)
    {   for (i = 1; i <= cn->mxedt && cn->edt[i] != j; i++);
	if( i > cn->mxedt) break;
    }
    cn->curedt=j;
    (cn->mxedt)++;
    cn->edt[cn->mxedt]=j;

    if( (f = (FENSTER *) MALLOC(sizeof(FENSTER))) == NULL)
			e_error(e_msg[0], 1, cn->fb);
    if( (fw = (FLWND *) MALLOC(sizeof(FLWND))) == NULL)
			e_error(e_msg[0], 1, cn->fb);
    f->fb = cn->fb;
    cn->f[cn->mxedt] = f;
    f->a = e_s_pkt(22, 3);
    f->e = e_s_pkt(f->a.x+35, f->a.y+18);
    f->winnum = cn->curedt;
    f->dtmd = 'D';
    f->ins = sw;
    f->save = 0;
    f->zoom = 0;
    f->ed = cn;
    f->c_sw = NULL;
    f->c_st = NULL;
    f->pic = NULL;
    if(!nstr) f->dirct = NULL;
    else
    {	f->dirct = MALLOC(strlen(nstr)+1);
    	strcpy(f->dirct, nstr);
    }
#ifdef XWINDOW
    if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
    if(sw == 1)
    {	f->datnam = "Function-Index";
	df = e_make_funct(nstr);
    }
    else if(sw == 2)
    {	f->datnam = "Grep";
	df = e_search_files(nstr, f->ed->fd.file, 
			f->ed->fd.search, NULL, f->ed->fd.sw);
    }
    else if(sw == 3)
    {	f->datnam = "Find";
	df = e_search_files(nstr, f->ed->fd.file, 
			f->ed->fd.search, NULL, f->ed->fd.sw);
    }
    else if(sw == 7)
    {	f->datnam = "Windows";
	df = e_make_win_list(f);
    }
#ifdef PROG
    else if(sw == 4)
    {	f->datnam = "Project";
	df = e_p_df[0];
    }
    else if(sw == 5)
    {	f->datnam = "Variables";
	df = e_p_df[1];
    }
    else if(sw == 6)
    {	f->datnam = "Install";
	df = e_p_df[2];
    }
#endif
    f->hlp_str = e_hlp_str[6+MENOPT+sw];
    if(sw < 4) {  f->blst = gblst;  f->nblst = 4;  }
    else {  f->blst = oblst;  f->nblst = 4;  }
#ifdef XWINDOW
    if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
    f->b = (BUFFER *)fw;
    fw->df = df;

    fw->mxa = f->a.x; fw->mxe = f->e.x; fw->mya = f->a.y; fw->mye = f->e.y;
    fw->xa = f->a.x + 3; fw->xe = f->e.x - 13; fw->ya = f->a.y + 3;
    fw->ye = f->e.y - 1; fw->f = f;
    fw->ia = fw->nf = fw->nxfo = fw->nyfo = 0;
    fw->srcha = fw->ja = 0;

    if(cn->mxedt > 1 && (f->ins < 5 || f->ins == 7)) 
			e_ed_rahmen(cn->f[cn->mxedt-1], 0);
    e_firstl(f, 1);
    e_data_schirm(f, 1);
    return(0);
}

int e_data_schirm(f, sw)
     FENSTER *f;
     int sw;
{
    int i, j;
    FLWND *fw = (FLWND *) f->b;

    for(j = f->a.y+1; j < f->e.y; j++)
    for(i = f->a.x+1; i < f->e.x; i++) 
	e_pr_char(i, j, ' ', f->fb->nt.fb);

    if(f->e.x - f->a.x > 25)
    { if(f->ins < 4 || f->ins == 7)
	e_pr_str((f->e.x - 9), f->e.y - 4, "Show", f->fb->nz.fb, 0, -1,
					      f->fb->ns.fb, f->fb->nt.fb);
      else if(f->ins > 3)
      {	e_pr_str((f->e.x - 9), f->e.y - 8, "Add", f->fb->nz.fb, 0, -1,
					      f->fb->ns.fb, f->fb->nt.fb);
	e_pr_str((f->e.x - 9), f->e.y - 6, "Edit", f->fb->nz.fb, 0, -1,
					      f->fb->ns.fb, f->fb->nt.fb);
	e_pr_str((f->e.x - 9), f->e.y - 4, "Delete", f->fb->nz.fb, 0, -1,
					      f->fb->ns.fb, f->fb->nt.fb);
	if(f->ins == 4 && f->a.y < f->e.y - 10)
	   e_pr_str((f->e.x - 9), f->e.y - 10, "Options", f->fb->nz.fb, 
					0, -1, f->fb->ns.fb, f->fb->nt.fb);
      }
	e_pr_str((f->e.x - 9), f->e.y - 2, "Cancel", f->fb->nz.fb, -1, -1,
					      f->fb->ns.fb, f->fb->nt.fb);
    }

    if(f->e.x - f->a.x > 25) { fw->xa = f->a.x + 3;  fw->xe = f->e.x - 13;  }
    else {  fw->xa = f->a.x + 3;  fw->xe = f->e.x - 2;  }
    fw->mxa = f->a.x; fw->mxe = f->e.x; fw->mya = f->a.y; fw->mye = f->e.y;
    fw->xa = f->a.x + 3; fw->ya = f->a.y + 3; fw->ye = f->e.y - 1;
    if(f->ins == 4) fw->df = e_p_df[0];
    if(f->ins == 1)
	e_pr_str(fw->xa, f->a.y + 2, "Functions:", f->fb->nt.fb, 0, 1,
			       f->fb->nsnt.fb, f->fb->nt.fb);
    else if(f->ins == 3)
	e_pr_str(fw->xa, f->a.y + 2, "Directories:", f->fb->nt.fb, 0, 1,
			       f->fb->nsnt.fb, f->fb->nt.fb);
    e_mouse_leiste(fw->xe, fw->ya, fw->ye-fw->ya, 0, fw->f->fb->em.fb);
    e_mouse_leiste(fw->xa, fw->ye, fw->xe-fw->xa, 1, fw->f->fb->em.fb);
    e_pr_file_window(fw, 0, 1, f->fb->ft.fb, f->fb->fz.fb, f->fb->frft.fb);
    return(0);
}

int e_data_eingabe(cn)
     ECNT *cn;
{
    FENSTER *f = cn->f[cn->mxedt];
    FLWND *fw = (FLWND *) f->b;
    int c = AltF;
    fk_cursor(0);
    if(f->ins == 7)
    {  freedf(fw->df);  fw->df = e_make_win_list(f);  }
    while (c != ESC)
    {	if(f->dtmd != 'D') return(0);
	if(f->ins == 4) fw->df = e_p_df[0];
	if(c == AltF) c = e_file_window(0, fw, f->fb->ft.fb, f->fb->fz.fb);
#if  MOUSE
	if(c == MBKEY) c = e_data_ein_mouse(f);
#endif
	if (((c == CR || c == AltS) && (f->ins < 4 || f->ins == 7)) || 
	    ((c == AltA || c == EINFG) && (f->ins > 3 && f->ins < 7)))
	{   if(f->ins == 1) e_ed_man(fw->df->name[fw->nf], f);
	    else if(f->ins == 2)
	    {	e_edit(f->ed, fw->df->name[fw->nf]);
		e_rep_search(f->ed->f[f->ed->mxedt]);
	    }
	    else if(f->ins == 3) e_file_first(0, f->ed, fw->df->name[fw->nf]);
	    else if(f->ins == 7) e_switch_window(f->ed->edt[fw->df->anz-fw->nf], f);
#ifdef PROG
	    else if(f->ins == 4)
	    {	e_file_first(5, f->ed, NULL);
		while(e_file_eingabe(f->ed) != ESC);
		e_p_df[f->ins-4] = fw->df; c = AltF;
		f->save = 1;
	    }
	    else if(f->ins > 4 && f->ins < 7)
	    {	e_p_add_df(fw, f->ins); e_p_df[f->ins-4] = fw->df; c = AltF;   }
#endif
	    if(f->ins < 4 || f->ins == 7) return(0);
	}
#ifdef PROG
	else if(f->ins > 3 && f->ins < 7 && (c == AltD || c == ENTF))
	{   e_p_del_df(fw, f->ins);  c = AltF;  f->save = 1;  }
	else if(f->ins > 3 && f->ins < 7 && (c == AltE || c == CR))
	{   e_p_edit_df(fw, f->ins);  c = AltF;  f->save = 1;  }
	else if(f->ins == 4 && c == AltO)
	{   e_project_options(f);  c = AltF;  }
#endif
	else if(c != AltF)
	{   if(c == AltBl) c = ESC;
	    else if(c == ESC) c = (f->ed->edopt & 1) ? CF4 : AF3;
	    if(f->ins == 7 && ((!(f->ed->edopt & 1) && c == AF3)
		  || ((f->ed->edopt & 1) && c == CF4))) e_close_window(f);
	    if(f->ins == 4 && ((!(f->ed->edopt & 1) && c == AF3)
		  || ((f->ed->edopt & 1) && c == CF4)))
	    {	FLWND *fw = (FLWND *)f->ed->f[f->ed->mxedt]->b;
		fw->df = NULL;
		e_close_window(f->ed->f[f->ed->mxedt]);
		return(0);
	    }
	    if(f->ins == 4 && (!e_tst_dfkt(f, c) || !e_prog_switch(f, c)))
								return(0);
	    if(f->ins > 4 && ((!(f->ed->edopt & 1) && c == AF3)
		  || ((f->ed->edopt & 1) && c == CF4))) return(c);
	    else if((f->ins < 4 || f->ins == 7) && !e_tst_dfkt(f, c)) return(0);
	    else c = AltF;
	}
    }
    return((f->ed->edopt & 1) ? CF4 : AF3);
}

int e_get_funct_in(nstr, f)
     char *nstr;
     FENSTER *f;
{
    return(e_data_first(1, f->ed, nstr));
}


int e_funct_in(f)
     FENSTER *f;
{
    int n, xa = 37, ya = 2, num = 8;
    OPTK *opt = MALLOC(num * sizeof(OPTK));
    char nstr[2];

    opt[0].t = "User Commands";       opt[0].x = 0;  opt[0].o = 'U';
    opt[1].t = "System Calls";        opt[1].x = 0;  opt[1].o = 'S';
    opt[2].t = "C-Lib.-Functions";    opt[2].x = 0;  opt[2].o = 'C';
    opt[3].t = "Devices & Netw. I.";  opt[3].x = 0;  opt[3].o = 'D';
    opt[4].t = "File Formats";        opt[4].x = 0;  opt[4].o = 'F';
    opt[5].t = "Games & Demos";       opt[5].x = 0;  opt[5].o = 'G';
    opt[6].t = "Environment, ...";    opt[6].x = 0;  opt[6].o = 'E';
    opt[7].t = "Maintenance Com.";    opt[7].x = 0;  opt[7].o = 'M';

    n = e_opt_sec_box(xa, ya, num, opt, f, 1);

    FREE(opt);
    if(n < 0) return(ESC);

    nstr[0] = '1' + n;  nstr[1] = '\0';
    return(e_get_funct_in(nstr, f));
}


#endif
