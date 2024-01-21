/* we_opt.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"
#ifdef DOS
#include <dos.h>
#include <alloc.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif

#define E_HLP_NUM 27
char *e_hlp_str[E_HLP_NUM];

extern char *info_file;
#ifdef DEBUGGER
extern int e_deb_type;
#endif
#ifndef DJGPP
extern FARBE *u_fb, *x_fb;
#endif
#define MAGIC 134

/*    Ueber WE      */

int e_about_WE(f)
     FENSTER *f;
{
   PIC *pic = NULL;
   int xa = 10, ya = 4, xe = xa + 50, ye = ya + 13;
   char tmp[40];
   fk_cursor(0);
   pic = e_std_kst(xa, ya, xe, ye, NULL, 1, f->fb->nr.fb, f->fb->nt.fb, f->fb->ne.fb);
   if(pic == NULL) {  e_error(e_msg[0], 1, f->fb); return(ESC);  }
   
   sprintf(tmp, "           %s           ", VERSION);
#ifdef UNIX
   if((e_we_sw & 1) && (e_we_sw & 2))
   {  e_pr_str(xa+7, ya+3, "  XWindow Programming Environment  ", f->fb->et.fb, 0, 0, 0, 0);
      e_pr_str(xa+7, ya+4, "             ( XWPE )              ", f->fb->et.fb, 0, 0, 0, 0);
   }
   else if((e_we_sw & 2))
   {  e_pr_str(xa+7, ya+3, "   Window Programming Environment  ", f->fb->et.fb, 0, 0, 0, 0);
      e_pr_str(xa+7, ya+4, "              ( WPE )              ", f->fb->et.fb, 0, 0, 0, 0);
   }
   else if((e_we_sw & 1))
   
   {  e_pr_str(xa+7, ya+3, "          XWindow Editor           ", f->fb->et.fb, 0, 0, 0, 0);
      e_pr_str(xa+7, ya+4, "              ( XWE )              ", f->fb->et.fb, 0, 0, 0, 0);
   }
   else
#endif
   {  e_pr_str(xa+7, ya+3, "           Window Editor           ", f->fb->et.fb, 0, 0, 0, 0);
      e_pr_str(xa+7, ya+4, "               ( WE )              ", f->fb->et.fb, 0, 0, 0, 0);
   }
   e_pr_str(xa+7, ya+5, tmp, f->fb->et.fb, 0, 0, 0, 0);
   e_pr_str(xa+2, ya+8, "Copyright (C) 1993 Fred Kruse", f->fb->nt.fb, 0, 0, 0, 0);
   e_pr_str(xa+2, ya+9, "This Sofware comes with ABSOLUTELY NO WARRANTY;", f->fb->nt.fb, 0, 0, 0, 0);
   e_pr_str(xa+2, ya+10, "This is free software, and you are welcome to", f->fb->nt.fb, 0, 0, 0, 0);
   e_pr_str(xa+2, ya+11, "redistribute it under certain conditions;", f->fb->nt.fb, 0, 0, 0, 0);
   e_pr_str(xa+2, ya+12, "See \'Help\\Editor\\GNU Pub...\' for details.", f->fb->nt.fb, 0, 0, 0, 0);
#if  MOUSE
   while(e_mshit() != 0);
   e_getch();
   while(e_mshit() != 0);
#else
   e_getch();
#endif
   e_close_view(pic, 1);
   return(0);
}

/*    Alles loeschen    */

int e_clear_desk(f)
     FENSTER *f;
{
   int i;
   ECNT *cn = f->ed;
#if  MOUSE
   int g[4]; /*  = { 2, 0, 0, 0, };  */
   
   g[0] = 2;
#endif
   fk_cursor(0);
   for(i = cn->mxedt; i > 0; i--)
   {  f = cn->f[cn->mxedt];
      if( e_close_window(f) == ESC ) return(ESC);
   }
   cn->mxedt = 0;
#if  MOUSE
   fk_mouse(g);
#endif
   e_ini_desk(cn);
#if  MOUSE
   g[0] = 1;
   fk_mouse(g);
#endif
   return(0);
}

/*    Alles neu schreiben   */

int e_repaint_desk(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   int i, j, g[4];
#ifdef XWINDOW
   extern PIC *e_X_l_pic;
   PIC *sv_pic = NULL, *nw_pic = NULL;
   if(e_we_sw & 1)
   {  if(e_X_l_pic && e_X_l_pic != cn->f[cn->mxedt]->pic)
      {  sv_pic = e_X_l_pic;
	 nw_pic = e_open_view(e_X_l_pic->a.x, e_X_l_pic->a.y,
				e_X_l_pic->e.x, e_X_l_pic->e.y, 0, 2);
      }
      e_ini_size();
   }
#endif
   if(cn->mxedt < 1)
   {  e_cls(f->fb->df.fb, f->fb->dc);
      e_ini_desk(f->ed);
#ifdef XWINDOW
      if((e_we_sw & 1) && nw_pic)
      {  e_close_view(nw_pic, 1);
	 e_X_l_pic = sv_pic;
      }
#endif
      return(0);
   }
   for (i = 1; i <= cn->mxedt; i++)
   {  FREE(cn->f[i]->pic->p);
      FREE(cn->f[i]->pic);
      cn->edt[i] = i;
      cn->f[i]->winnum = i;
      for(j = i-1; j > 0 && (!strcmp(cn->f[j]->datnam, "Messages")
			|| cn->f[j]->dtmd <= 'Z'
			|| !strcmp(cn->f[j]->datnam, "Watches")); j--);
#ifdef PROG
      if(e_we_sw & 2 && cn->f[i]->dtmd > 'Z')
      {  if(!strcmp(cn->f[i]->datnam, "Messages")
				|| !strcmp(cn->f[i]->datnam, "Watches"))
	 {  cn->f[i]->a = e_s_pkt(0, 2*MAXSLNS/3 + 1);
	    cn->f[i]->e = e_s_pkt(MAXSCOL-1, MAXSLNS-2);
	 }
	 else
	 {  if(j < 1)
	    {  cn->f[i]->a = e_s_pkt(0, 1);
	       cn->f[i]->e = e_s_pkt(MAXSCOL-1, 2*MAXSLNS/3);
	    }
	    else
	    {  cn->f[i]->a = e_s_pkt(cn->f[j]->a.x+1, cn->f[j]->a.y+1);
	       cn->f[i]->e = e_s_pkt(cn->f[j]->e.x, cn->f[j]->e.y);
	    }
	 }
      }
      else if(cn->f[i]->dtmd > 'Z')
#else
      if(cn->f[i]->dtmd > 'Z')
#endif
      {  if(j < 1)
	 {  cn->f[i]->a = e_s_pkt(0, 1);
	    cn->f[i]->e = e_s_pkt(MAXSCOL-1, MAXSLNS-2);
	 }
	 else
	 {  cn->f[i]->a = e_s_pkt(cn->f[j]->a.x+1, cn->f[j]->a.y+1);
	    cn->f[i]->e = e_s_pkt(cn->f[j]->e.x, cn->f[j]->e.y);
	 }
      }
      if(cn->f[i]->e.x - cn->f[i]->a.x < 26)
      cn->f[i]->a.x = cn->f[i]->e.x - 26;
      if(cn->f[i]->e.y - cn->f[i]->a.y < 3)
      cn->f[i]->a.y = cn->f[i]->e.y - 3;
   }
   cn->curedt = cn->mxedt;
   ini_repaint(cn);
   e_abs_refr();
   for ( i = 1; i < cn->mxedt; i++)
   {  e_firstl(cn->f[i], 0);
      e_schirm(cn->f[i], 0);
   }
   e_firstl(cn->f[i], 1);
   e_schirm(cn->f[i], 1);
#ifdef XWINDOW
   if((e_we_sw & 1) && nw_pic)
   {  e_close_view(nw_pic, 1);
      e_X_l_pic = sv_pic;
   }
#endif
#if  MOUSE
   g[0] = 2; fk_mouse(g);
#endif
   end_repaint();
   e_cursor(cn->f[i], 1);
#if  MOUSE
   g[0] = 0; fk_mouse(g);
   g[0] = 1; fk_mouse(g);
#endif
   return(0);
}

/*    System-Informationen schreiben   */

int e_sys_info(f)
     FENSTER *f;
{
/*    SCHIRM *s = f->ed->e[f->ed->mxedt]->s;  
    BUFFER *b = f->ed->e[f->ed->mxedt]->b;        */
   PIC *pic = NULL;
   char tmp[80];
#ifdef DOS
   int xa = 10, ya = 5, xe = xa + 60, ye = ya + 12;
#else
   int xa = 10, ya = 5, xe = xa + 60, ye = ya + 8;
#endif
   fk_cursor(0);
   pic = e_std_kst(xa, ya, xe, ye, " Information ", 1, f->fb->nr.fb, f->fb->nt.fb, f->fb->ne.fb);
   if(pic == NULL) {  e_error(e_msg[0], 1, f->fb); return(ESC);  }
/*    e_pr_str((xa+xe-13)/2, ya, " Information ", f->fb->nr.fb, 0, 0, 0, 0);  */
   e_pr_str(xa+3, ya+2, " Current File: ", f->fb->nt.fb, 0, 0, 0, 0);
   e_pr_str(xa+3, ya+4, " Current Directory: ", f->fb->nt.fb, 0, 0, 0, 0);
   e_pr_str(xa+3, ya+6, " Number of Files: ", f->fb->nt.fb, 0, 0, 0, 0);
#ifdef DOS
   e_pr_str(xa+3, ya+8, " Available Memory: ", f->fb->nt.fb, 0, 0, 0, 0);
   e_pr_str(xa+3, ya+10, " Left Memory: ", f->fb->nt.fb, 0, 0, 0, 0);
#endif
   if(strcmp(f->datnam, "Clipboard") != 0)
   if(strcmp(f->dirct, f->ed->dirct) == 0)
   e_pr_str(xa+23, ya+2, f->datnam, f->fb->nt.fb, 0, 0, 0, 0);
   else
   {  strcpy(tmp, f->dirct);
      strcat(tmp, DIRS);
      strcat(tmp, f->datnam);
      e_pr_str(xa+23, ya+2, tmp, f->fb->nt.fb, 0, 0, 0, 0);
   }
   e_pr_str(xa+23, ya+4, f->ed->dirct, f->fb->nt.fb, 0, 0, 0, 0);
   e_pr_str(xa+23, ya+6,
         e_numtozif(f->ed->mxedt, e_anz_zif(f->ed->mxedt), tmp),
                                         f->fb->nt.fb, 0, 0, 0, 0);
#ifdef DOS
   e_pr_str(xa+23, ya+8,
         e_numtozif((c = biosmemory()), (n = e_anz_zif(c)), tmp),
                                         f->fb->nt.fb, 0, 0, 0, 0);
   e_pr_str(xa+24+n, ya+8, "KByte", f->fb->nt.fb, 0, 0, 0, 0);
   e_pr_str(xa+23, ya+10,
         e_numtozif((c = farcoreleft()/1024), (n = e_anz_zif(c)), tmp),
                                         f->fb->nt.fb, 0, 0, 0, 0);
   e_pr_str(xa+24+n, ya+10, "KByte", f->fb->nt.fb, 0, 0, 0, 0);
#endif
#if  MOUSE
   while(e_mshit() != 0);
   e_getch();
   while(e_mshit() != 0);
#else
   e_getch();
#endif
   e_close_view(pic, 1);
   return(0);
}

/*   Farbeinstellungen  */

int e_ad_colors(f)
     FENSTER *f;
{
   int n, xa = 48, ya = 2, num = 4;
   OPTK *opt = MALLOC(num * sizeof(OPTK));
   
   opt[0].t = "Editor Colors";     opt[0].x = 0;  opt[0].o = 'E';
   opt[1].t = "Desk Colors";       opt[1].x = 0;  opt[1].o = 'D';
   opt[2].t = "Option Colors";     opt[2].x = 0;  opt[2].o = 'O';
   opt[3].t = "Progr. Colors";     opt[3].x = 0;  opt[3].o = 'P';
   
   n = e_opt_sec_box(xa, ya, num, opt, f, 1);
   
   FREE(opt);
   if(n < 0) return(ESC);
   
   return(e_ad_colors_md(f, n));
}

int e_ad_colors_md(f, md)
     FENSTER *f;
     int md;
{
   int sw = 0, xa = 0, ya = 1, xe = xa + 79, ye = ya + 22;
   PIC *pic;
   pic = e_std_kst(xa, ya, xe, ye, "Adjust Colors", 1, f->fb->er.fb, f->fb->et.fb, f->fb->es.fb);
   if(pic == NULL) {  e_error(e_msg[0], 1, f->fb); return(ESC);  }
   sw = e_dif_colors(sw, xe-13, ya+1, f, md);
   e_close_view(pic, 1);
   e_repaint_desk(f);
   return(sw);
}

/*   Farbeinstellungen vornehmen  */

int e_dif_colors(sw, xa, ya, f, md)
     int sw;
     int xa;
     int ya;
     FENSTER *f;
     int md;
{
   COLOR *frb = &(f->fb->er);
   int c = 0, bg, num;
   if(md == 1) {  bg = 11;  num = 5;  }
   else if(md == 2) {  bg = 16;  num = 15;  }
   else if(md == 3) {  bg = 32;  num = 5;  }
   else {  bg = 0;  num = 11;  }
   while(c != ESC && c > -2)
   {  e_pr_dif_colors(sw, xa, ya, f, 1, md);
      e_pr_col_kasten(xa-28, ya+1, frb[sw+bg].f, frb[sw+bg].b , f, 0);
      e_pr_ed_beispiel(1, 2, f, sw, md);
#if  MOUSE
      if((c = e_getch()) == -1) c = e_opt_cw_mouse(xa, ya, md);
#else
      c = e_getch();
#endif
      if(c >= 375 && c <= 393) sw = c-375;
      else if(c == 326) sw = 0;
      else if(c == 334) sw = num-1;
      else if(c == 327) sw = (sw == 0) ? num-1 : sw-1;
      else if(c == 335) sw = (sw == num-1) ? 0 : sw+1;
      else if(c == CR || c == 285 || c == 330)
      {  e_pr_dif_colors(sw, xa, ya, f, 0, md);
	 *(frb+sw+bg) = e_n_clr(e_frb_menue(sw, xa-28, ya+1, f, md));
      }
   }
   return(c);
}

char *text[] = {  "Border", "Bord. Bt.", "Text", "Txt Mrk.1", "Txt Mrk.2",
		  "Scrollbar", "Help Hdr.", "Help Btt.", "Help Mrk.",
		  "Breakpnt.", "Stop Brk.",
		  "Border", "Bord. Bt.", "Text", "Txt Mrk.", "Backgrnd.",
		  "Border", "Bord. Bt.", "Text", "Text Sw.",
		  "Write",  "Wrt. Mrk.", "Data", "Data M.A", "Data M.P",
		  "Switch", "Swtch. S.", "Swtch. A.", "Button",
		  "Bttn. Sw.", "Bttn. Ak.",
		  "Text", "Res. Wrd.", "Constants", "Pre-Proc.", "Comments"
	       };

/*   Farbkasten schreiben  */

void e_pr_dif_colors(sw, xa, ya, f, sw2, md)
     int sw;
     int xa;
     int ya;
     FENSTER *f;
     int sw2;
     int md;
{
   int i, rfrb, cfrb, xe = xa + 12, ye, bg;
   char *header;
   if(md == 1) {  ye = ya + 6;  bg = 11;  header = "Desk";  }
   else if(md == 2) {  ye = ya + 16;  bg = 16;  header = "Options";  }
   else if(md == 3) {  ye = ya + 6;  bg = 31;  header = "C-Prog.";  }
   else {  ye = ya + 12;  bg = 0;  header = "Editor";  }
   rfrb = sw2 == 0 ? f->fb->nt.fb : f->fb->fs.fb;
   e_std_rahmen(xa, ya, xe, ye, header, 0, rfrb, 0);
   for(i = 0; i < ye-ya-1; i++)
   {  cfrb = i == sw ? f->fb->fz.fb : f->fb->ft.fb;
      e_pr_str_wsd(xa+2, ya+1+i,  text[i+bg], cfrb, 0, 0, 0, xa+1, xe-1);
   }
}

/*    Farb-Menue  */

int e_frb_x_menue(sw, xa, ya, f, md)
     int sw;
     int xa;
     int ya;
     FENSTER *f;
     int md;
{
   COLOR *frb = &(f->fb->er);
   int c = 1, fsv = frb[sw].fb, x, y;
   if(md == 1) sw += 11;
   else if(md == 2) sw += 16;
   else if(md == 3) sw += 32;
   y = frb[sw].b;
   x = frb[sw].f;
   do
   {  if(c == CRI && y < 7) y++;
      else if(c == CLE && y > 0) y--;
      else if(c == CUP && x > 0) x--;
      else if(c == CDO && x < 15) x++;
      else if(c >= 1000 && c < 1256)
      {  x = (c-1000)/16;  y = (c-1000)%16;  }
      e_pr_x_col_kasten(xa, ya, x, y, f, 1);
      frb[sw] = e_s_clr(x, y);
      e_pr_ed_beispiel(1, 2, f, sw, md);
#if  MOUSE
      if((c=e_getch()) == -1) c = e_opt_ck_mouse(xa, ya, md);
#else
      c = e_getch();
#endif
   }
   while (c != ESC && c != CR && c > -2);
   if(c == ESC || c < -1) frb[sw] = e_n_clr(fsv);
   return(frb[sw].fb);
}

/*   Farbkasten auf Schirm schreiben  */

void e_pr_x_col_kasten(xa, ya, x, y, f, sw)
     int xa;
     int ya;
     int x;
     int y;
     FENSTER *f;
     int sw;
{
   int i, j, rfrb, ffrb, xe = xa + 25, ye = ya + 18;
   rfrb = sw == 0 ? f->fb->nt.fb : f->fb->fs.fb;
   ffrb = rfrb % 16;
   e_std_rahmen(xa-2, ya-1, xe, ye, "Colors", 0, rfrb, 0);
/*     e_pr_str((xa+xe-8)/2, ya-1, "Colors", rfrb, 0, 1, 
                                        f->fb->ms.f+16*(rfrb/16), 0);
*/
   for (j = 0; j < 8; j++)
   for (i = 0; i < 16; i++)
   {  e_pr_char(3*j+xa, i+ya+1, ' ', 16*j+i);
      e_pr_char(3*j+xa+1, i+ya+1, 'x', 16*j+i);
      e_pr_char(3*j+xa+2, i+ya+1, ' ', 16*j+i);
   }
   for (i = 0; i < 18; i++)
   {  e_pr_char(xa-1, i+ya, ' ', rfrb);
      e_pr_char(xe-1, i+ya, ' ', rfrb);
   }
   for (j = 0; j < 25; j++)
   {  e_pr_char(j+xa-1, ya, ' ', rfrb);
      e_pr_char(j+xa-1, ye-1, ' ', rfrb);
   }
#ifdef NEWSTYLE
   if(!(e_we_sw & 1)) {
#endif
      for (i = 0; i < 3; i++)
      {  e_pr_char(3*y+xa+i, x+ya, RE5, x>0 ? 16*y+ffrb : rfrb );
	 e_pr_char(3*y+xa+i, x+ya+2, RE5, x<15 ? 16*y+ffrb : rfrb );
      }
      e_pr_char(3*y+xa-1, x+ya+1, RE6, y<1 ? rfrb  : 16*(y-1)+ffrb);
      e_pr_char(3*y+xa+3, x+ya+1, RE6, y>6 ? rfrb  : 16*(y+1)+ffrb);
      e_pr_char(3*y+xa-1, x+ya, RE1, (y<1 || x<1) ? rfrb  : 16*(y-1)+ffrb);
      e_pr_char(3*y+xa+3, x+ya, RE2, (y>6 || x<1) ? rfrb  : 16*(y+1)+ffrb);
      e_pr_char(3*y+xa-1, x+ya+2, RE3, (y<1 || x>14) ? rfrb  : 16*(y-1)+ffrb);
      e_pr_char(3*y+xa+3, x+ya+2, RE4, (y>6 || x>14) ? rfrb  : 16*(y+1)+ffrb);
#ifdef NEWSTYLE
   }
   else e_make_xrect(3*y+xa, x+ya+1, 3*y+xa+2, x+ya+1, 1);
#endif
}

/*    Farb-Beispiel auf Schirm schreiben   */

void e_pr_ed_beispiel(xa, ya, f, sw, md)
     int xa;
     int ya;
     FENSTER *f;
     int sw;
     int md;
{
   COLOR *frb = &(f->fb->er);
   FARBE *fb = f->fb;
   int i, j, xe = xa+31, ye = ya+19;
   
#ifdef DOS
   frb[sw] = e_s_clr(frb[sw].f, frb[sw].b + 8);
#else
   frb[sw] = e_s_clr(frb[sw].f, frb[sw].b);
#endif
   if(md == 1)
   {  e_blk(xe-xa+1, xa+1, ya, fb->mt.fb);
      e_pr_str_wsd(xa+5, ya,  "Edit", fb->mt.fb, 0, 1, f->fb->ms.fb,
								xa+3, xa+11);
      e_pr_str_wsd(xa+18, ya,  "Options", fb->mz.fb, 0, 0, f->fb->ms.fb,
								xa+16, xa+27);
      for(i = ya+1; i < ye; i++)
      for(j = xa+1; j <= xe+1; j++)
      {  e_pr_char(j, i, fb->dc, fb->df.fb);
	 e_pr_char(j, i, fb->dc, fb->df.fb);
      }
      e_std_rahmen(xa+17, ya+1, xa+26, ya+3, NULL, 0, fb->mr.fb, 0);
      e_pr_str(xa+19, ya+2, "Colors", fb->mt.fb, 0, 1, fb->ms.fb, 0);
      e_blk(xe-xa+1, xa+1, ye, fb->mt.fb);
      e_pr_str_wsd(xa+4, ye, "Alt-F3 Close Window",
                                   fb->mt.fb, 0, 6, fb->ms.fb, xa+2, xa+25);
   }
   else if(md == 2)
   {  e_std_rahmen(xa, ya, xe, ye, "Message", 1, fb->nr.fb, f->fb->ne.fb);
      for(i = ya+1; i < ye; i++) e_blk(xe-xa-1, xa+1, i, fb->nt.fb);
      e_pr_str(xa+4, ya+2, "Name:", f->fb->nt.fb, 0, 1,
				       f->fb->nsnt.fb, f->fb->nt.fb);
      e_pr_str(xa+5, ya+3, "Active Write-Line ", fb->fa.fb, 0, 0, 0, 0);
      e_pr_str(xa+4, ya+5, "Name:", f->fb->nt.fb, 0, 1,
				       f->fb->nsnt.fb, f->fb->nt.fb);
      e_pr_str(xa+5, ya+6, "Passive Write-Line", fb->fr.fb, 0, 0, 0, 0);
      e_pr_str(xa+4, ya+8, "Data:", f->fb->nt.fb, 0, 1,
				       f->fb->nsnt.fb, f->fb->nt.fb);
      e_pr_str(xa+5, ya+9, "Active Marked ", f->fb->fz.fb, 0, 0, 0, 0);
      e_pr_str(xa+5, ya+10, "Passive Marked", f->fb->frft.fb, 0, 0, 0, 0);
      e_pr_str(xa+5, ya+11, "Data Text     ", f->fb->ft.fb, 0, 0, 0, 0);
      e_pr_str(xa+4, ya+13, "Switches:", f->fb->nt.fb, 0, 1,
				       f->fb->nsnt.fb, f->fb->nt.fb);
      e_pr_str(xa+5, ya+14, "[X] Active Switch ", f->fb->fsm.fb, 0, 0, 0, 0);
      e_pr_str(xa+5, ya+15, "[ ] Passive Switch", f->fb->fs.fb, 4, 1,
				       f->fb->nsft.fb, f->fb->fs.fb);
      e_pr_str(xa+6 , ye-2, "Button", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
      e_pr_str(xe-12 , ye-2, "Active", f->fb->nm.fb, 0, -1,
                                              f->fb->nm.fb, f->fb->nt.fb);
#ifdef NEWSTYLE
      if(e_we_sw & 1)
      {  e_make_xrect(xa+4, ya+3, xa+23, ya+3, 1);
	 e_make_xrect(xa+4, ya+6, xa+23, ya+6, 1);
	 e_make_xrect(xa+4, ya+9, xa+19, ya+11, 1);
	 e_make_xrect_abs(xa+4, ya+9, xa+19, ya+9, 0);
	 e_make_xrect(xa+4, ya+14, xa+23, ya+15, 1);
	 e_make_xrect_abs(xa+4, ya+14, xa+23, ya+14, 0);
      }
#endif
   }
   else
   {  e_std_rahmen(xa, ya, xe, ye, "Filename", 1, fb->er.fb, fb->es.fb);
      e_mouse_leiste(xe, ya+1, ye-ya-1, 0, fb->em.fb);
      e_mouse_leiste(xa+20, ye, 11, 1, fb->em.fb);
      e_pr_char(xe-3, ya, WZN, fb->es.fb);
#ifdef NEWSTYLE
      if(!(e_we_sw & 1)) {
#endif
	 e_pr_char(xe-4, ya, '[', fb->er.fb);
	 e_pr_char(xe-2, ya, ']', fb->er.fb);
#ifdef NEWSTYLE
      }  else e_make_xrect(xe-4, ya, xe-2, ya, 0);
#endif
      for(i = ya+1; i < ye; i++) e_blk(xe-xa-1, xa+1, i, fb->et.fb);
      if(md == 3)
      {  e_pr_str(xa+4, ya+3, "#Preprozessor Comands", fb->cp.fb, 0, 0, 0, 0);
	 e_pr_str(xa+4, ya+5, "This are C-Text Colors", fb->ct.fb, 0, 0, 0, 0);
	 e_pr_str(xa+4, ya+7, "int char {} [] ; ,", fb->cr.fb, 0, 0, 0, 0);
	 e_pr_str(xa+4, ya+9, "\"Constants\" 12 0x13", fb->ck.fb, 0, 0, 0, 0);
	 e_pr_str(xa+4, ya+11, "/*   Comments    */", fb->cc.fb, 0, 0, 0, 0);
      }
      else
      {  e_pr_str(xa+4, ya+3, "This are the Editor Colors", fb->et.fb, 0, 0, 0, 0);
	 e_pr_str(xa+4, ya+4, "And this is a marked Line", fb->ez.fb, 0, 0, 0, 0);
	 e_pr_str(xa+4, ya+5, "This is a found word", fb->et.fb, 0, 0, 0, 0);
	 e_pr_str(xa+14, ya+5, "found", fb->ek.fb, 0, 0, 0, 0);
	 e_pr_str(xa+4, ya+8, "Help Header", fb->hh.fb, 0, 0, 0, 0);
	 e_pr_str(xa+4, ya+9, "This is a marked Word", fb->et.fb, 0, 0, 0, 0);
	 e_pr_str(xa+14, ya+9, "marked", fb->hm.fb, 0, 0, 0, 0);
	 e_pr_str(xa+4, ya+10, "in the Help File", fb->et.fb, 0, 0, 0, 0);
	 e_pr_str(xa+4, ya+11, "Help Button", fb->hb.fb, 0, 0, 0, 0);
	 e_pr_str(xa+4, ya+14, "This is a Breakpoint", fb->db.fb, 0, 0, 0, 0);
	 e_pr_str(xa+4, ya+15, "Stop at Breakpoint", fb->dy.fb, 0, 0, 0, 0);
      }
   }
#ifdef DOS
   frb[sw] = e_s_clr(frb[sw].f, frb[sw].b - 8);
#else
   frb[sw] = e_s_clr(frb[sw].f, frb[sw].b);
#endif
}

/*    Schirm mit Standard - Farbeinstellung schreiben   */
/*
int e_std_colors(FENSTER *f)
{
    e_ini_farbe(f->fb);
    e_repaint_desk(f);
    return(0);
} 
*/
/*    Schalter - Optionen    */

#ifdef UHR
#define UNUM 5
#else
#define UNUM 4
#endif
/*
int e_opt_switches(FENSTER *f)
{
    extern struct EXT uhr;
#if  MOUSE
    extern struct mouse e_mouse;
#endif
    extern WOPT *blst;
    ECNT *cn = f->ed;
    PIC *pic = NULL;
#ifdef WEUNDO
    int xa = 50, ya = 3, xe = xa + 24, ye = ya + UNUM+1;
#else
    int xa = 50, ya = 3, xe = xa + 16, ye = ya + UNUM;
#endif
    int n = 1, c = 0, col;
#if  MOUSE
    while(e_mshit() != 0);
#endif
    fk_cursor(0);
    pic = e_std_kst(xa, ya, xe, ye, "Switches", 1, f->fb->mr.fb, f->fb->ms.fb);
    if(pic == NULL) { e_error(e_msg[0], 1, f->fb); return(ESC); }
    while(c != ESC && c > -2)
    {  col = n == 1 ? f->fb->mz.fb : f->fb->mt.fb;
       if(cn->dtmd == 'b' )
           e_pr_str(xa+2, ya+1, "Binary File", col, 0, 0, 0, 0);
       else
           e_pr_str(xa+2, ya+1, "ASCII File ", col, 0, 0, 0, 0);
       col = n == 2 ? f->fb->mz.fb : f->fb->mt.fb;
       if(cn->dtmd == 's' )
           e_pr_str(xa+2, ya+2, "Show End-Mark", col, 0, 0, 0, 0);
       else
           e_pr_str(xa+2, ya+2, "Hide End-Mark", col, 0, 0, 0, 0);
       col = n == 3 ? f->fb->mz.fb : f->fb->mt.fb;
#ifdef WEUNDO
       if((cn->autosv & 1) == 0 )
	   e_pr_str(xa+2, ya+3, "Autosave Options  Off", col, 0, 0, 0, 0);
       else
           e_pr_str(xa+2, ya+3, "Autosave Options   On", col, 0, 0, 0, 0);
       col = n == 4 ? f->fb->mz.fb : f->fb->mt.fb;
       if((cn->autosv & 2) == 0 )
           e_pr_str(xa+2, ya+4, "Autosave Changes  Off", col, 0, 0, 0, 0);
       else
           e_pr_str(xa+2, ya+4, "Autosave Changes   On", col, 0, 0, 0, 0);
       col = n == 5 ? f->fb->mz.fb : f->fb->mt.fb;
#ifdef UHR
       if(uhr.sw == 0 )
           e_pr_str(xa+2, ya+5, "Clock     Off", col, 0, 0, 0, 0);
       else
           e_pr_str(xa+2, ya+5, "Clock      On", col, 0, 0, 0, 0);
#endif
       c = e_getch();
       if(c == 326) n = 1;
       else if(c == 334) n = UNUM;
       else if(c == 327) n = (n == 1) ? UNUM : n - 1;
       else if(c == 335) n = (n == UNUM) ? 1 : n + 1;
#else
       if(cn->autosv == 0 )
           e_pr_str(xa+2, ya+3, "Autosave  Off", col, 0, 0, 0, 0);
       else
           e_pr_str(xa+2, ya+3, "Autosave   On", col, 0, 0, 0, 0);
       col = n == 4 ? f->fb->mz.fb : f->fb->mt.fb;
#ifdef UHR
       if(uhr.sw == 0 )
           e_pr_str(xa+2, ya+4, "Clock     Off", col, 0, 0, 0, 0);
       else
           e_pr_str(xa+2, ya+4, "Clock      On", col, 0, 0, 0, 0);
#endif
       c = e_getch();
       if(c == 326 || c == CtrlA) n = 1;
       else if(c == 334 || c == CtrlE) n = UNUM-1;
       else if(c == 327 || c == CtrlP) n = (n == 1) ? UNUM-1 : n - 1;
       else if(c == 335 || c == CtrlN) n = (n == UNUM-1) ? 1 : n + 1;
#endif
#if  MOUSE
       else if(c == -1)
       {  while(e_mshit() != 0);
          if(e_mouse.x < xa || e_mouse.x > xe ||
                    e_mouse.y < ya || e_mouse.y > ye) c = ESC;
          else if(e_mouse.y == ya && e_mouse.x == xa+3) c = ESC;
          else if(e_mouse.x > xa && e_mouse.x < xe &&
                    e_mouse.y > ya && e_mouse.y < ye) 
          {  if(e_mouse.y - ya == n)  c = CR;
             else n = e_mouse.y - ya;
          }
       }
#endif

       if(c == CR)
       {  if(n == 1 && cn->dtmd == 'b') cn->dtmd = 'n';
          else if(n == 1) cn->dtmd = 'b';
          else if(n == 2 && cn->dtmd == 'n') { cn->dtmd = 's'; f->dtmd = 's'; }
          else if(n == 2 && cn->dtmd == 's') { cn->dtmd = 'n'; f->dtmd = 'n'; }
#ifdef WEUNDO
          else if(n == 3) cn->autosv = 
		(cn->autosv & 1) ? (cn->autosv & ~1) : cn->autosv | 1;
	  else if(n == 4) cn->autosv =
		(cn->autosv & 2) ? (cn->autosv & ~2) : cn->autosv | 2;
#ifdef UHR
          else if(n == 5) uhr.sw = !(uhr.sw);
#endif
#else
          else if(n == 3) cn->autosv = !(cn->autosv);
#ifdef UHR
          else if(n == 4) uhr.sw = !(uhr.sw);
#endif
#endif
       }
    }
    e_close_view(pic, 1);          
    if(uhr.sw == 0)
    e_pr_ul(f->fb);
    return(0);
}
*/
/*    Ziffer - OPtionen      */
/*
int e_opt_numbers(FENSTER *f)
{
#if  MOUSE
    extern struct mouse e_mouse;
#endif
    ECNT *cn = f->ed;
    PIC *pic = NULL;
#ifdef WEUNDO
    int xa = 50, ya = 3, xe = xa + 16, ye = ya + UNUM + 1;
#else
    int xa = 50, ya = 3, xe = xa + 16, ye = ya + UNUM - 1;
#endif
    int n = 1, c = 0, num, col;
#if  MOUSE
    while(e_mshit() != 0);
#endif
    fk_cursor(0);
    pic = e_std_kst(xa, ya, xe, ye, "Numbers", 1, f->fb->mr.fb, f->fb->ms.fb);
    if(pic == NULL) { e_error(e_msg[0], 1, f->fb); return(ESC); }
    while(c != ESC && c > -2)
    {  col = n == 1 ? f->fb->mz.fb : f->fb->mt.fb;
       e_pr_str(xa+2, ya+1, "Max. Columes", col, 0, 0, 0, 0);
       col = n == 2 ? f->fb->mz.fb : f->fb->mt.fb;
       e_pr_str(xa+2, ya+2, "Tabstops", col, 0, 0, 0, 0);
#ifdef WEUNDO
       col = n == 3 ? f->fb->mz.fb : f->fb->mt.fb;
       e_pr_str(xa+2, ya+3, "Max. Changes", col, 0, 0, 0, 0);
       col = n == 4 ? f->fb->mz.fb : f->fb->mt.fb;
       e_pr_str(xa+2, ya+4, "Num. of Undo", col, 0, 0, 0, 0);
#ifdef UHR
       col = n == 5 ? f->fb->mz.fb : f->fb->mt.fb;
       e_pr_str(xa+2, ya+5, "Time", col, 0, 0, 0, 0);
#endif
       c = e_getch();
       if(c == 326 || c == CtrlA) n = 1;
       else if(c == 334 || c == CtrlE) n = UNUM;
       else if(c == 327 || c == CtrlP) n = (n == 1) ? UNUM : n - 1;
       else if(c == 335 || c == CtrlN) n = (n == UNUM) ? 1 : n + 1;
#else
#ifdef UHR
       col = n == 3 ? f->fb->mz.fb : f->fb->mt.fb;
       e_pr_str(xa+2, ya+3, "Time", col, 0, 0, 0, 0);
#endif
       c = e_getch();
       if(c == 326 || c == CtrlA) n = 1;
       else if(c == 334 || c == CtrlE) n = UNUM-2;
       else if(c == 327 || c == CtrlP) n = (n == 1) ? UNUM-2 : n - 1;
       else if(c == 335 || c == CtrlN) n = (n == UNUM-2) ? 1 : n + 1;
#endif
#if  MOUSE
       else if(c == -1)
       {  while(e_mshit() != 0);
          if(e_mouse.x < xa || e_mouse.x > xe ||
                    e_mouse.y < ya || e_mouse.y > ye) c = ESC;
          else if(e_mouse.y == ya && e_mouse.x == xa+3) c = ESC;
          else if(e_mouse.x > xa && e_mouse.x < xe &&
                    e_mouse.y > ya && e_mouse.y < ye) 
          {  if(e_mouse.y - ya == n)  c = CR;
	     else n = e_mouse.y - ya;
          }
       }
#endif

       if(c == CR)
       {  if(n == 1 
             && (num = e_num_kst("Max. Colums:", cn->maxcol, 160, f)) > -1)
                                                             cn->maxcol = num;
          else if(n == 2 
               && (num = e_num_kst("Tabstops:", cn->tabn, 12, f)) > -1)
          {  cn->tabn = num;  
	     cn->tabs = REALLOC(cn->tabs, (cn->tabn+1)*sizeof(char));
             e_blktostr(cn->tabs, cn->tabn);  
          }
#ifdef WEUNDO
          else if(n == 3
             && (num = e_num_kst("Max. Changes:", cn->maxchg, 9999, f)) > -1)
                                       cn->maxchg = num > 0 ? num : 1;
          else if(n == 4
	     && (num = e_num_kst("Num. of Undo:", cn->numundo, 999, f)) > -1)
                                       cn->numundo = num > 0 ? num : 1;
#ifdef UHR
          else if(n == 5) e_set_time(); 
#endif
#else
#ifdef UHR
          else if(n == 3) e_set_time(); 
#endif
#endif
       }
    }
    e_close_view(pic, 1);          
    return(0);
}
*/
/*   Zeit - Einstellen    */
/*
void e_set_time()
{
    extern struct EXT uhr;
    FENSTER *f = uhr.cn->f[uhr.cn->mxedt];
#ifdef UHR
    struct time tm;
#if MOUSE
    extern struct mouse e_mouse;
#endif
    int col = uhr.cn->fb->nt.fb, col1 = uhr.cn->fb->fr.fb, col2 = uhr.cn->fb->fz.fb;
    char hr[3], min[3], sec[3];
    int nhr, nmin, nsec;
    PIC *pic = NULL;
    int c = 255, n = 1;
    int xa = 35, ya = 5, xe = xa +18, ye = ya + 2;
    pic = e_std_kst(xa, ya, xe, ye, 1, f->fb->nr.fb, f->fb->ne.fb);
    if(pic == NULL) { e_error(e_msg[0], 1, f->fb); return; }
    gettime(&tm);
    nhr = tm.ti_hour; nmin = tm.ti_min; nsec = tm.ti_sec;
    e_pr_str(xa+2, ya+1, "Time:", col, 0, 0, 0, 0);
    e_pr_char(xa+11, ya+1, ':', col);
    e_pr_char(xa+14, ya+1, ':', col);
#if  MOUSE
    while(e_mshit() != 0);
    while ( c != CR && c != ESC && c != -2 &&
            (c != -1 || e_mouse.y != ya || e_mouse.x != xa+3) )
#else
    while ( c != CR && c != ESC )
#endif
    {
        e_pr_zstring(e_numtozif(nhr, 2, hr), xa+9, ya+1, 2, col1);
        e_pr_zstring(e_numtozif(nmin, 2, min), xa+12, ya+1, 2, col1);
        e_pr_zstring(e_numtozif(nsec, 2, sec), xa+15, ya+1, 2, col1);
        if(n == 1) c =  e_schreib_zif(&nhr, xa+9, ya+1, 2, col1, col2);
        else if(n == 2) c =  e_schreib_zif(&nmin, xa+12, ya+1, 2, col1, col2);
        else if(n == 3) c =  e_schreib_zif(&nsec, xa+15, ya+1, 2, col1, col2);
#if  MOUSE
        if(c == -1 && e_mouse.y == ya)
        {  if(e_mouse.x == xa+9 || e_mouse.x == xa+10) n = 1;
           else if(e_mouse.x == xa+12 || e_mouse.x == xa+13) n = 2;
           else if(e_mouse.x == xa+15 || e_mouse.x == xa+16) n = 3;
        }
#endif
        if(c == '\t') n++;
        if(n > 3) n = 1;
    }
    if( c != -2 && c != ESC ) 
    {   tm.ti_hour = nhr; tm.ti_min = nmin; tm.ti_sec = nsec;
        tm.ti_hund = 0;
        settime(&tm);
    }
#if  MOUSE
    while(e_mshit() != 0);
#endif
    e_close_view(pic, 1);
#else 
    e_error(e_msg[14], 0, f->fb);
#endif
}
*/
/*   Uhr auf Schirm schreiben   */
/*
void e_uhr()
{
#ifdef UHR
    extern struct EXT uhr;
    struct time tm;
    int col = uhr.cn->fb->mt.fb;
    gettime(&tm);
    e_pr_char(71, 24, '0' + tm.ti_hour/10, col);
    e_pr_char(72, 24, '0' + tm.ti_hour%10, col);
    e_pr_char(73, 24, ':', col);
    e_pr_char(74, 24, '0' + tm.ti_min/10, col);
    e_pr_char(75, 24, '0' + tm.ti_min%10, col);
    e_pr_char(76, 24, ':', col);
    e_pr_char(77, 24, '0' + tm.ti_sec/10, col);
    e_pr_char(78, 24, '0' + tm.ti_sec%10, col);
#endif
}
*/
/*   Save - Optionen - Menue   */

int e_opt_save(f)
     FENSTER *f;
{
   int ret;
   char tmp[256];
   strcpy(tmp, f->ed->optfile);
   ret = e_add_arguments(tmp, "Save Option File", f, 0, AltS, NULL);
   if(ret)
   {  f->ed->optfile = REALLOC(f->ed->optfile, (strlen(tmp)+1)*sizeof(char));
      strcpy(f->ed->optfile, tmp);
      e_save_opt(f);
   }
   return(ret);
}

/*
int e_opt_save(FENSTER *f)
{
#if  MOUSE
    extern struct mouse e_mouse;
#endif
    char filen[80];
    int c = 0, xa = 40, ya = 4, xe = xa + 34, ye = ya + 7;
    PIC *pic = e_std_kst(xa, ya, xe, ye, "Save Options", 1, f->fb->nr.fb, f->fb->ne.fb);
    if(pic == NULL) { e_error(e_msg[0], 0, f->fb);  return(-1);  }
    e_pr_str(xa + 5, ye - 2, "Save", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
    e_pr_str(xe - 10, ye - 2, "Cancel", f->fb->nz.fb, -1, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
    e_pr_str(xa + 2, ya + 2, "Option-File:", f->fb->nt.fb, 0, 0,
                                            f->fb->nsnt.fb, f->fb->nt.fb);
    strcpy(filen, f->ed->optfile);
    fk_cursor(1);
    while(c != ESC && c != CR && toupper(c) != 'S')
    {  c = e_schreib_leiste(filen, xa+2, ya+3, 30, 80,
					    f->fb->fr.fb, f->fb->fz.fb);
#if  MOUSE
       if(c == -1)
       {   if(e_mouse.y == ya && e_mouse.x == xa+3) c = CR;
           else if(e_mouse.y == ye - 2 && e_mouse.x >= xa+4 
                    && e_mouse.x <= xa+9) c = CR;
           else if(e_mouse.y == ye - 2 && e_mouse.x >= xe-11 
                    && e_mouse.x <= xe-4) c = ESC;
       }
       else if(c < -1) c = ESC;
#endif
    }
    if(c != ESC)
    {  f->ed->optfile = REALLOC(f->ed->optfile, (strlen(filen)+1)*sizeof(char));
       strcpy(f->ed->optfile, filen);
       e_save_opt(f);
    }
    e_close_view(pic, 1);
    return(c);
}
*/
/*   Optionen sichern     */

int e_save_opt(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   FILE *fp;
   int magic = MAGIC, j, n, *num, *it;
   char *ch, **str;
   COLOR *cl;
#ifndef XWINDOW
   FARBE *u_fb = cn->fb;
#endif
#ifdef PROG
   int i, len;
#endif
   ch = MALLOC((strlen(cn->optfile)+1)*sizeof(char));
   strcpy(ch, cn->optfile);
   for(j = strlen(ch); j > 0 && ch[j] != DIRC; j--);
   ch[j] = '\0';
   if(access(ch, 0)) mkdir(ch, 0700);
   FREE(ch);
   if((fp = fopen(cn->optfile, "wb")) == NULL)
   {  e_error(e_msg[15], 0, f->fb);
      return(-1);
   }
   if(fwrite("xwpe-", 5, 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   if(fwrite(VERSION, sizeof(VERSION), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   if(fwrite(&magic, sizeof(int), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
#ifdef PROG
#ifdef DEBUGGER
   n = 4;
#else                    /*  Anzahl Hauptbloecke   */
   n = 3;
#endif
#else
   n = 2;
#endif
   if(fwrite(&n, sizeof(int), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   n = 4;                      /*   Anzahl Unterbloecke   */
   if(fwrite(&n, sizeof(int), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   num = MALLOC(n * sizeof(int));
   num[0] = 3;			/*   Anzahl Character-Konstanten in cn  */
#ifdef WEUNDO
   num[1] = 5;			/*   Anzahl Integer-Konstanten in cn  */
#else
   num[1] = 3;			/*   Anzahl Integer-Konstanten in cn  */
#endif
#ifdef UNIX
   num[1]++;
#endif
   num[2] = 0;			/*   Anzahl Double-Konstanten in cn  */
   num[3] = 1;			/*   Anzahl String-Konstanten in cn  */
   for(j = 0; j < n; j++)
   if(fwrite(num+j, sizeof(int), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   for(ch = &cn->dtmd, j = 0; j < num[0]; j++)
   if(fwrite(ch+j, sizeof(char), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   for(it = &cn->maxcol, j = 0; j < num[1]; j++)
   if(fwrite(it+j, sizeof(int), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   len = strlen(info_file) + 1;
   if(fwrite(&len, sizeof(int), 1, fp) != 1
    	    || fwrite(info_file, len, 1, fp) != 1)
   FREE(num);                /*   Ende des ersten Blocks   */
#ifndef DJGPP
   n = 4;                      /*   Anzahl Unterbloecke   */
#else
   n = 2;                      /*   Anzahl Unterbloecke   */
#endif
   if(fwrite(&n, sizeof(int), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   num = MALLOC(n * sizeof(int));
   num[0] = 38;		/*   Anzahl Color-Konstanten in u_fb  */
   num[1] = 2;			/*   Anzahl Character-Konstanten in u_fb  */
#ifndef DJGPP
   num[2] = 38;		/*   Anzahl Color-Konstanten in x_fb  */
   num[3] = 2;			/*   Anzahl Character-Konstanten in x_fb  */
#endif
   for(j = 0; j < n; j++)
   if(fwrite(num+j, sizeof(int), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   for(cl = (COLOR *) u_fb, j = 0; j < num[0]; j++)
   if(fwrite(cl+j, sizeof(COLOR), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   for(ch = (char *) &u_fb->dc, j = 0; j < num[1]; j++)
   if(fwrite(ch+j, sizeof(char), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
#ifndef DJGPP
   for(cl = (COLOR *) x_fb, j = 0; j < num[2]; j++)
   if(fwrite(cl+j, sizeof(COLOR), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   for(ch = (char *) &x_fb->dc, j = 0; j < num[3]; j++)
   if(fwrite(ch+j, sizeof(char), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
#endif
   FREE(num);                /*   Ende des zweiten Blocks   */
#ifdef PROG
   n = 1;                      /*   Anzahl Unterbloecke   */
   if(fwrite(&n, sizeof(int), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   if(fwrite(&e_prog, sizeof(struct e_prog), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   len = strlen(e_prog.arguments) + 1;
   if(fwrite(&len, sizeof(int), 1, fp) != 1
    	    || fwrite(e_prog.arguments, len, 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   len = strlen(e_prog.project) + 1;
   if(fwrite(&len, sizeof(int), 1, fp) != 1
    	    || fwrite(e_prog.project, len, 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   len = strlen(e_prog.exedir) + 1;
   if(fwrite(&len, sizeof(int), 1, fp) != 1
    	    || fwrite(e_prog.exedir, len, 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   len = strlen(e_prog.sys_include) + 1;
   if(fwrite(&len, sizeof(int), 1, fp) != 1
    	    || fwrite(e_prog.sys_include, len, 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   for(i = 0; i < e_prog.num; i++)
   {  n = 7;                      /*   Anzahl Unterbloecke   */
      if(fwrite(&n, sizeof(int), 1, fp) != 1)
      {  e_error(e_msg[16], 0, f->fb); return(-1);  }
      for(str = &e_prog.comp[i]->language, j = 0; j < n; j++)
      {  len = strlen(*(str+j)) + 1;
	 if(fwrite(&len, sizeof(int), 1, fp) != 1
             || fwrite(*(str+j), len, 1, fp) != 1)
	 {  e_error(e_msg[16], 0, f->fb); return(-1);  }
      }
      if(fwrite(&e_prog.comp[i]->key, sizeof(char), 1, fp) != 1)
      {  e_error(e_msg[16], 0, f->fb); return(-1);  }
      if(fwrite(&e_prog.comp[i]->comp_sw, sizeof(int), 1, fp) != 1)
      {  e_error(e_msg[16], 0, f->fb); return(-1);  }
      if(fwrite(&e_prog.comp[i]->x, sizeof(int), 1, fp) != 1)
      {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   }
#endif
#ifdef DEBUGGER
   n = 1;                      /*   Anzahl Unterbloecke   */
   if(fwrite(&n, sizeof(int), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
   if(fwrite(&e_deb_type, sizeof(int), 1, fp) != 1)
   {  e_error(e_msg[16], 0, f->fb); return(-1);  }
#endif
   fclose(fp);
   return(0);
}

/*   Optionen einlesen   */

int e_read_cl(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   FILE *fp = fopen(cn->optfile, "rb");
   int j, nb, n, *num, ntmp;
   char tmp[80], *ch;
   COLOR *cl;
#ifdef DJGPP
   FARBE *u_fb = cn->fb;
#endif
   if(fp == NULL) return(-3);
   
   if(fread(tmp, sizeof(VERSION) + 5, 1, fp) != 1) return(-2);
   if(fread(&n, sizeof(int), 1, fp) != 1) return(-2);
   if(n != MAGIC) return(-1);
   if(fread(&nb, sizeof(int), 1, fp) != 1) return(-2);
   if(nb < 1) return(0);
   if(fread(&n, sizeof(int), 1, fp) != 1) return(-2);
   num = MALLOC(n * sizeof(int));
   for(j = 0; j < n; j++)
   if(fread(num+j, sizeof(int), 1, fp) != 1) return(-2);
   for(j = 0; n > 0 && j < num[0]; j++)
   if(fread(&ntmp, sizeof(char), 1, fp) != 1) return(-2);
   for(j = 0; n > 1 && j < num[1]; j++)
   if(fread(&ntmp, sizeof(int), 1, fp) != 1) return(-2);
   FREE(num);                /*   Ende des ersten Blocks   */
   if(nb < 2) return(0);
   if(fread(&n, sizeof(int), 1, fp) != 1) return(-2);
   num = MALLOC(n * sizeof(int));
   for(j = 0; j < n; j++)
   if(fread(num+j, sizeof(int), 1, fp) != 1) return(-2);
   for(cl = (COLOR *) u_fb, j = 0; n > 0 && j < num[0]; j++)
   if(fread(cl+j, sizeof(COLOR), 1, fp) != 1) return(-2);
   for(ch = (char *) &u_fb->dc, j = 0; n > 1 && j < num[1]; j++)
   if(fread(ch+j, sizeof(char), 1, fp) != 1) return(-2);
#ifndef DJGPP
   for(cl = (COLOR *) x_fb, j = 0; n > 2 && j < num[2]; j++)
   if(fread(cl+j, sizeof(COLOR), 1, fp) != 1) return(-2);
   for(ch = (char *) &x_fb->dc, j = 0; n > 3 && j < num[3]; j++)
   if(fread(ch+j, sizeof(char), 1, fp) != 1) return(-2);
#endif
   FREE(num);                /*   Ende des zweiten Blocks   */
   fclose(fp);
   return(0);
}

int e_opt_read(f, sw)
     FENSTER *f;
     int sw;
{
   ECNT *cn = f->ed;
   FILE *fp = fopen(cn->optfile, "rb");
   int j, nb, n, *num, *it;
   char tmp[256], *ch, **str;
   COLOR *cl;
#ifdef DJGPP
   FARBE *u_fb = cn->fb;
#endif
#ifdef PROG
   struct e_prog tmp_prog;
   int i, len;
#endif
   if(fp == NULL)
   {  char *file = e_mkfilename(cn->libdrct, SYSOPTFILE);
      fp = fopen(file, "rb");
      FREE(file);
   }
   if(fp == NULL) return(0);
   
   if(fread(tmp, sizeof(VERSION) + 5, 1, fp) != 1) return(-2);
   if(fread(&n, sizeof(int), 1, fp) != 1) return(-2);
   if(n != MAGIC) return(-1);
   if(fread(&nb, sizeof(int), 1, fp) != 1) return(-2);
   if(nb < 1) return(0);
   if(fread(&n, sizeof(int), 1, fp) != 1) return(-2);
   num = MALLOC(n * sizeof(int));
   for(j = 0; j < n; j++)
   if(fread(num+j, sizeof(int), 1, fp) != 1) return(-2);
   for(ch = &cn->dtmd, j = 0; n > 0 && j < num[0]; j++)
   if(fread(ch+j, sizeof(char), 1, fp) != 1) return(-2);
   for(it = &cn->maxcol, j = 0; n > 1 && j < num[1]; j++)
   if(fread(it+j, sizeof(int), 1, fp) != 1) return(-2);
   if(fread(&len, sizeof(int), 1, fp) != 1
    	    || fread(tmp, len, 1, fp) != 1) return(-2);
            info_file = e_make_string(info_file, tmp);
   FREE(num);                /*   Ende des ersten Blocks   */
   if(nb < 2) return(0);
   if(fread(&n, sizeof(int), 1, fp) != 1) return(-2);
   num = MALLOC(n * sizeof(int));
   for(j = 0; j < n; j++)
   if(fread(num+j, sizeof(int), 1, fp) != 1) return(-2);
   for(cl = (COLOR *) u_fb, j = 0; n > 0 && j < num[0]; j++)
   if(fread(cl+j, sizeof(COLOR), 1, fp) != 1) return(-2);
   for(ch = (char *) &u_fb->dc, j = 0; n > 1 && j < num[1]; j++)
   if(fread(ch+j, sizeof(char), 1, fp) != 1) return(-2);
#ifndef DJGPP
   for(cl = (COLOR *) x_fb, j = 0; n > 2 && j < num[2]; j++)
   if(fread(cl+j, sizeof(COLOR), 1, fp) != 1) return(-2);
   for(ch = (char *) &x_fb->dc, j = 0; n > 3 && j < num[3]; j++)
   if(fread(ch+j, sizeof(char), 1, fp) != 1) return(-2);
#endif
   FREE(num);                /*   Ende des zweiten Blocks   */
#ifdef PROG
   if(nb < 3) return(0);
   if(fread(&n, sizeof(int), 1, fp) != 1) return(-2);
   if(n < 1) return(0);
   if(fread(&tmp_prog, sizeof(struct e_prog), 1, fp) != 1) return(-2);
/*    if(e_we_sw & 2)   */
   {  for(i = 0; i < e_prog.num; i++)
      {  FREE(e_prog.comp[i]->language);
	 FREE(e_prog.comp[i]->compiler);
	 FREE(e_prog.comp[i]->comp_str);
	 FREE(e_prog.comp[i]->libraries);
	 FREE(e_prog.comp[i]->exe_name);
	 FREE(e_prog.comp[i]->filepostfix);
	 FREE(e_prog.comp[i]->intstr);
	 FREE(e_prog.comp[i]);
      }
      FREE(e_prog.comp);;
      if(e_prog.arguments) FREE(e_prog.arguments);
      if(e_prog.project) FREE(e_prog.project);
      if(e_prog.exedir) FREE(e_prog.exedir);
      if(e_prog.sys_include) FREE(e_prog.sys_include);
   }
   e_prog = tmp_prog;
   if(fread(&len, sizeof(int), 1, fp) != 1
    	    || fread(tmp, len, 1, fp) != 1) return(-2);
   e_prog.arguments = e_make_string(NULL, tmp);
   if(fread(&len, sizeof(int), 1, fp) != 1
    	    || fread(tmp, len, 1, fp) != 1) return(-2);
   e_prog.project = e_make_string(NULL, tmp);
   if(fread(&len, sizeof(int), 1, fp) != 1
    	    || fread(tmp, len, 1, fp) != 1) return(-2);
   e_prog.exedir = e_make_string(NULL, tmp);
   if(fread(&len, sizeof(int), 1, fp) != 1
    	    || fread(tmp, len, 1, fp) != 1) return(-2);
   e_prog.sys_include = e_make_string(NULL, tmp);
   e_prog.comp = MALLOC(e_prog.num * sizeof(struct e_s_prog *));
   for(i = 0; i < e_prog.num; i++)
   e_prog.comp[i] = MALLOC(sizeof(struct e_s_prog));
   for(i = 0; i < e_prog.num; i++)
   {  if(fread(&n, sizeof(int), 1, fp) != 1) return(-2);
      for(str = &e_prog.comp[i]->language, j = 0; j < n; j++)
      {  if(fread(&len, sizeof(int), 1, fp) != 1
    	    || fread(tmp, len, 1, fp) != 1) return(-2);
	 str[j] = e_make_string(NULL, tmp);
      }
      if(fread(&e_prog.comp[i]->key, sizeof(char), 1, fp) != 1) return(-2);
      if(fread(&e_prog.comp[i]->comp_sw, sizeof(int), 1, fp) != 1) return(-2);
      if(fread(&e_prog.comp[i]->x, sizeof(int), 1, fp) != 1) return(-2);
   }
#endif
#ifdef DEBUGGER
   if(nb < 4) return(0);
   if(fread(&n, sizeof(int), 1, fp) != 1) return(-2);
   if(n < 1) return(0);
   if(fread(&e_deb_type, sizeof(int), 1, fp) != 1) return(-2);
#endif
   fclose(fp);
   return(0);
}

/*  Fenster zur Eingabe einer Textzeile    */

int e_add_arguments(str, head, f, n, sw, df)
     char *str;
     char *head;
     FENSTER *f;
     int n;
     int sw;
     struct dirfile **df;
{
   int ret;
   char *tmp = MALLOC((strlen(head)+2) * sizeof(char));
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o || !tmp) return(-1);
   o->xa = 20;  o->ya = 4;  o->xe = 57;  o->ye = 11;
   o->bgsw = 0;
   o->name = head;
   o->crsw = AltO;
   sprintf(tmp, "%s:", head);
   e_add_wrstr(4, 2, 4, 3, 30, 128, n, sw, tmp, str, df, o);
   e_add_bttstr(7, 5, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(24, 5, -1, ESC, "Cancel", NULL, o);
   FREE(tmp);
   ret = e_opt_kst(o);
   if(ret != ESC) strcpy(str, o->wstr[0]->txt);
   freeostr(o);
   return(ret == ESC ? 0 : 1);
}
/*
{
#if  MOUSE
    extern struct mouse e_mouse;
#endif
    char tmp[80];
    int c = 0, xa = 40, ya = 4, xe = xa + 34, ye = ya + 7;
    PIC *pic = e_std_kst(xa, ya, xe, ye, head, 1, f->fb->nr.fb, f->fb->ne.fb);
    if(pic == NULL) { e_error(e_msg[0], 0, f->fb);  return(-1);  }
    e_pr_str(xa + 5, ye - 2, "Ok", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
    e_pr_str(xe - 10, ye - 2, "Cancel", f->fb->nz.fb, -1, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
    strcpy(tmp, head);
    e_pr_str(xa + 2, ya + 2, strcat(tmp, ":"), f->fb->nt.fb, 0, 0,
                                            f->fb->nsnt.fb, f->fb->nt.fb);
    fk_cursor(1);
    while(c != ESC && c != CR && toupper(c) != 'O')
    {  c = e_schreib_leiste(str, xa+2, ya+3, 30, 80,
                                            f->fb->fr.fb, f->fb->fz.fb);
#if  MOUSE
       if(c == -1)
       {   if(e_mouse.y == ya && e_mouse.x == xa+3) c = ESC;
           else if(e_mouse.y == ye - 2 && e_mouse.x >= xa+4 
                    && e_mouse.x <= xa+9) c = CR;
           else if(e_mouse.y == ye - 2 && e_mouse.x >= xe-11 
                    && e_mouse.x <= xe-4) c = ESC;
       }
       else if(c < -1) c = ESC;
#endif
    }
    e_close_view(pic, 1);
    return(c != ESC ? 1 : 0);
}
*/
W_O_TXTSTR **e_add_txtstr(x, y, txt, o)
     int x;
     int y;
     char *txt;
     W_OPTSTR *o;
{
   if(o->tn == 0) o->tstr = MALLOC(1);
   (o->tn)++;
   if(!(o->tstr = REALLOC(o->tstr, o->tn * sizeof(W_O_TXTSTR *))))
   return(NULL);
   if(!(o->tstr[o->tn-1] = MALLOC(sizeof(W_O_TXTSTR)))) return(NULL);
   if(!(o->tstr[o->tn-1]->txt = MALLOC((strlen(txt)+1) * sizeof(char))))
   return(NULL);
   o->tstr[o->tn-1]->x = x;
   o->tstr[o->tn-1]->y = y;
   strcpy(o->tstr[o->tn-1]->txt, txt);
   return(o->tstr);
}

W_O_WRSTR **e_add_wrstr(xt, yt, xw, yw, nw, wmx, nc, sw, header, txt, df, o)
     int xt;
     int yt;
     int xw;
     int yw;
     int nw;
     int wmx;
     int nc;
     int sw;
     char *header;
     char *txt;
     struct dirfile **df;
     W_OPTSTR *o;
{
   if(o->wn == 0) o->wstr = MALLOC(1);
   (o->wn)++;
   if(!(o->wstr = REALLOC(o->wstr, o->wn * sizeof(W_O_WRSTR *))))
   return(NULL);
   if(!(o->wstr[o->wn-1] = MALLOC(sizeof(W_O_WRSTR)))) return(NULL);
   if(!(o->wstr[o->wn-1]->txt = MALLOC((wmx+1) * sizeof(char))))
   return(NULL);
   if(!(o->wstr[o->wn-1]->header = MALLOC((strlen(header)+1) * sizeof(char))))
   return(NULL);
   o->wstr[o->wn-1]->xt = xt;
   o->wstr[o->wn-1]->yt = yt;
   o->wstr[o->wn-1]->xw = xw;
   o->wstr[o->wn-1]->yw = yw;
   o->wstr[o->wn-1]->nw = nw;
   o->wstr[o->wn-1]->wmx = wmx;
   o->wstr[o->wn-1]->nc = nc;
   o->wstr[o->wn-1]->sw = sw;
   o->wstr[o->wn-1]->df = df;
   strcpy(o->wstr[o->wn-1]->header, header);
   strcpy(o->wstr[o->wn-1]->txt, txt);
   return(o->wstr);
}

W_O_NUMSTR **e_add_numstr(xt, yt, xw, yw, nw, wmx, nc, sw, header, num, o)
     int xt;
     int yt;
     int xw;
     int yw;
     int nw;
     int wmx;
     int nc;
     int sw;
     char *header;
     int num;
     W_OPTSTR *o;
{
   if(o->nn == 0) o->nstr = MALLOC(1);
   (o->nn)++;
   if(!(o->nstr = REALLOC(o->nstr, o->nn * sizeof(W_O_NUMSTR *))))
   return(NULL);
   if(!(o->nstr[o->nn-1] = MALLOC(sizeof(W_O_NUMSTR)))) return(NULL);
   if(!(o->nstr[o->nn-1]->header = MALLOC((strlen(header)+1) * sizeof(char))))
   return(NULL);
   o->nstr[o->nn-1]->xt = xt;
   o->nstr[o->nn-1]->yt = yt;
   o->nstr[o->nn-1]->xw = xw;
   o->nstr[o->nn-1]->yw = yw;
   o->nstr[o->nn-1]->nw = nw;
   o->nstr[o->nn-1]->wmx = wmx;
   o->nstr[o->nn-1]->nc = nc;
   o->nstr[o->nn-1]->sw = sw;
   o->nstr[o->nn-1]->num = num;
   strcpy(o->nstr[o->nn-1]->header, header);
   return(o->nstr);
}

W_O_SSWSTR **e_add_sswstr(x, y, nc, sw, num, header, o)
     int x;
     int y;
     int nc;
     int sw;
     int num;
     char *header;
     W_OPTSTR *o;
{
   if(o->sn == 0) o->sstr = MALLOC(1);
   (o->sn)++;
   if(!(o->sstr = REALLOC(o->sstr, o->sn * sizeof(W_O_SSWSTR *))))
   return(NULL);
   if(!(o->sstr[o->sn-1] = MALLOC(sizeof(W_O_SSWSTR)))) return(NULL);
   if(!(o->sstr[o->sn-1]->header = MALLOC((strlen(header)+1) * sizeof(char))))
   return(NULL);
   o->sstr[o->sn-1]->x = x;
   o->sstr[o->sn-1]->y = y;
   o->sstr[o->sn-1]->nc = nc;
   o->sstr[o->sn-1]->sw = sw;
   o->sstr[o->sn-1]->num = num;
   strcpy(o->sstr[o->sn-1]->header, header);
   return(o->sstr);
}

W_O_SPSWSTR **e_add_spswstr(n, x, y, nc, sw, header, o)
     int n;
     int x;
     int y;
     int nc;
     int sw;
     char *header;
     W_OPTSTR *o;
{
   if(n >= o->pn) return(NULL);
   if(n < 0) n = 0;
   if(o->pstr[n]->np == 0) o->pstr[n]->ps = MALLOC(1);
   (o->pstr[n]->np)++;
   if(!(o->pstr[n]->ps = REALLOC(o->pstr[n]->ps, o->pstr[n]->np * sizeof(W_O_SPSWSTR *))))
   return(NULL);
   if(!(o->pstr[n]->ps[o->pstr[n]->np-1] = MALLOC(sizeof(W_O_SPSWSTR)))) return(NULL);
   if(!(o->pstr[n]->ps[o->pstr[n]->np-1]->header = MALLOC((strlen(header)+1) * sizeof(char))))
   return(NULL);
   o->pstr[n]->ps[o->pstr[n]->np-1]->x = x;
   o->pstr[n]->ps[o->pstr[n]->np-1]->y = y;
   o->pstr[n]->ps[o->pstr[n]->np-1]->nc = nc;
   o->pstr[n]->ps[o->pstr[n]->np-1]->sw = sw;
   strcpy(o->pstr[n]->ps[o->pstr[n]->np-1]->header, header);
   return(o->pstr[n]->ps);
}

W_O_PSWSTR **e_add_pswstr(n, x, y, nc, sw, num, header, o)
     int n;
     int x;
     int y;
     int nc;
     int sw;
     int num;
     char *header;
     W_OPTSTR *o;
{
   if(o->pn == 0) o->pstr = MALLOC(1);
   if(n >= o->pn)
   {  n = o->pn;
      (o->pn)++;
      if(!(o->pstr = REALLOC(o->pstr, o->pn * sizeof(W_O_PSWSTR *))))
      return(NULL);
      if(!(o->pstr[o->pn-1] = MALLOC(sizeof(W_O_PSWSTR)))) return(NULL);
      o->pstr[o->pn-1]->np = 0;
   }
   if(!e_add_spswstr(n, x, y, nc, sw, header, o)) return(NULL);
   o->pstr[o->pn-1]->num = num;
   return(o->pstr);
}

W_O_BTTSTR **e_add_bttstr(x, y, nc, sw, header, fkt, o)
     int x;
     int y;
     int nc;
     int sw;
     char *header;
     int (*fkt)();
     W_OPTSTR *o;
{
   if(o->bn == 0) o->bstr = MALLOC(1);
   (o->bn)++;
   if(!(o->bstr = REALLOC(o->bstr, o->bn * sizeof(W_O_BTTSTR *))))
   return(NULL);
   if(!(o->bstr[o->bn-1] = MALLOC(sizeof(W_O_BTTSTR)))) return(NULL);
   if(!(o->bstr[o->bn-1]->header = MALLOC((strlen(header)+1) * sizeof(char))))
   return(NULL);
   o->bstr[o->bn-1]->x = x;
   o->bstr[o->bn-1]->y = y;
   o->bstr[o->bn-1]->nc = nc;
   o->bstr[o->bn-1]->sw = sw;
   o->bstr[o->bn-1]->fkt = fkt;
   strcpy(o->bstr[o->bn-1]->header, header);
   return(o->bstr);
}

int freeostr(o)
     W_OPTSTR *o;
{
   int i, j;
   if(!o) return(0);
   for(i = 0; i < o->tn; i++)
   {  FREE(o->tstr[i]->txt);  FREE(o->tstr[i]);  }
   if(o->tn) FREE(o->tstr);
   for(i = 0; i < o->wn; i++)
   {  FREE(o->wstr[i]->txt);  FREE(o->wstr[i]->header);  FREE(o->wstr[i]);  }
   if(o->wn) FREE(o->wstr);
   for(i = 0; i < o->nn; i++)
   {  FREE(o->nstr[i]->header);  FREE(o->nstr[i]);  }
   if(o->nn) FREE(o->nstr);
   for(i = 0; i < o->pn; i++)
   {  for(j = 0; j < o->pstr[i]->np; j++)
      {  FREE(o->pstr[i]->ps[j]->header);  FREE(o->pstr[i]->ps[j]);  }
      if(o->pstr[i]->np) FREE(o->pstr[i]->ps);
      FREE(o->pstr[i]);
   }
   if(o->pn) FREE(o->pstr);
   for(i = 0; i < o->bn; i++)
   {  FREE(o->bstr[i]->header);  FREE(o->bstr[i]);  }
   if(o->bn) FREE(o->bstr);
   FREE(o);
   return(0);
}

W_OPTSTR *e_init_opt_kst(f)
     FENSTER *f;
{
   W_OPTSTR *o = MALLOC(sizeof(W_OPTSTR));
   if(!o) return(NULL);
   o->frt = f->fb->nr.fb;
   o->frs = f->fb->ne.fb;
   o->ftt = f->fb->nt.fb;
   o->fts = f->fb->nsnt.fb;
   o->fst = f->fb->fs.fb;
   o->fss = f->fb->nsft.fb;
   o->fsa = f->fb->fsm.fb;
   o->fwt = f->fb->fr.fb;
   o->fws = f->fb->fa.fb;
   o->fbt = f->fb->nz.fb;
   o->fbs = f->fb->ns.fb;
   o->fbz = f->fb->nm.fb;
   o->tn = o->sn = o->pn = o->bn = o->wn = o->nn = 0;
   o->f = f;
   o->pic = NULL;
   return(o);
}

int e_opt_move(o)
     W_OPTSTR *o;
{
   int xa = o->xa, ya = o->ya, xe = o->xe, ye = o->ye;
   int c = 0;
   PIC *pic;
   e_std_rahmen(o->xa, o->ya, o->xe, o->ye, o->name, 0, o->frt, o->frs);
#ifndef NEWSTYLE
#if !defined(DJGPP) && !defined(DOS)
   if(!(e_we_sw & 1)) pic = e_open_view(o->xa, o->ya, o->xe, o->ye, 0, 2);
   else 
#endif
   {  pic = e_open_view(o->xa, o->ya, o->xe-2, o->ye-1, 0, 2);
      e_close_view(pic, 2);
   }
#else
   pic = e_open_view(o->xa, o->ya, o->xe, o->ye, 0, 2);
#endif
   while ((c = e_getch()) != ESC && c != CR)
   {  switch(c)
      {  case CLE:
	    if(xa > 0) {  xa--; xe--;  }
	    break;
	 case CRI:
	    if(xe < MAXSCOL-1) {  xa++; xe++;  }
	    break;
	 case CUP:
	    if(ya > 1) {  ya--; ye--;  }
	    break;
	 case CDO:
	    if(ye < MAXSLNS-2) {  ya++; ye++;  }
	    break;
      }
      if( xa != o->xa || ya != o->ya || xe != o->xe || ye != o->ye)
      {
	 o->xa = xa;
	 o->ya = ya;
	 o->xe = xe;
	 o->ye = ye;
	 o->pic = e_change_pic(o->xa, o->ya, o->xe, o->ye, o->pic, 1, o->frt);
	 if(o->pic == NULL)  e_error(e_msg[0], 1, o->f->fb);
	 pic->a.x = o->xa;  pic->a.y = o->ya;
	 pic->e.x = o->xe;  pic->e.y = o->ye;
	 e_close_view(pic, 2);
      }
   }
   pic->a.x = o->xa;  pic->a.y = o->ya;
   pic->e.x = o->xe;  pic->e.y = o->ye;
   e_close_view(pic, 1);
   e_std_rahmen(o->xa, o->ya, o->xe, o->ye, o->name, 1, o->frt, o->frs);
   return(c);
}

int e_get_sw_cmp(xin, yin, x, y, xmin, ymin, c)
     int xin;
     int yin;
     int x;
     int y;
     int xmin;
     int ymin;
     int c;
{
   return
   (
    ( c == 0 && yin == y && (xin-1 <= x && xin+xmin >= x))  ||
    ((c == CDO || c == BDO) && yin > y && (yin < ymin ||
	(yin == ymin && xin <= x && xin > xmin)) ) ||
    ((c == CUP || c == BUP) && yin < y && (yin > ymin ||
	(yin == ymin && xin <=x && xin > xmin)) ) ||
    ((c == CLE || c == CCLE) && yin == y && xin < x && xin > xmin) ||
    ((c == CRI || c == CCRI) && yin == y && xin > x && xin < xmin) );
}

int e_get_opt_sw(c, x, y, o)
     int c;
     int x;
     int y;
     W_OPTSTR *o;
{
   int i, j, xmin, ymin, ret = 0;
   if(c != 0 && c != CUP && c != CDO && c != CLE && c != CRI &&
	c != BUP && c != BDO && c != CCLE && c != CCRI) return(c);
   
   xmin = (c == CRI || c == CCRI) ? o->xe : o->xa;
   ymin = (c == CUP || c == BUP) ? o->ya : o->ye;
   x -= o->xa;  xmin -= o->xa;
   y -= o->ya;  ymin -= o->ya;
   for(i = 0; i < o->wn; i++)
   {  if(e_get_sw_cmp(o->wstr[i]->xw, o->wstr[i]->yw, x, y,
			c ? xmin : o->wstr[i]->nw, ymin, c))
      {  xmin = o->wstr[i]->xw;  ymin = o->wstr[i]->yw;
	 ret = o->wstr[i]->sw;
      }
   }
   for(i = 0; i < o->nn; i++)
   {  if(e_get_sw_cmp(o->nstr[i]->xw, o->nstr[i]->yw, x, y,
			c ? xmin : o->nstr[i]->nw, ymin, c))
      {  xmin = o->nstr[i]->xw;  ymin = o->nstr[i]->yw;
	 ret = o->nstr[i]->sw;
      }
   }
   for(i = 0; i < o->sn; i++)
   {  if(e_get_sw_cmp(o->sstr[i]->x, o->sstr[i]->y, x, y,
		c ? xmin : 2, ymin, c))
      {  xmin = o->sstr[i]->x;  ymin = o->sstr[i]->y;
	 ret = o->sstr[i]->sw;
      }
   }
   for(i = 0; i < o->pn; i++)
   for(j = 0; j < o->pstr[i]->np; j++)
   {  if(e_get_sw_cmp(o->pstr[i]->ps[j]->x, o->pstr[i]->ps[j]->y, x, y,
		c ? xmin : 2, ymin, c))
      {  xmin = o->pstr[i]->ps[j]->x;  ymin = o->pstr[i]->ps[j]->y;
	 ret = o->pstr[i]->ps[j]->sw;
      }
   }
   for(i = 0; i < o->bn; i++)
   {  if(e_get_sw_cmp(o->bstr[i]->x, o->bstr[i]->y, x, y,
		c ? xmin : strlen(o->bstr[i]->header), ymin, c))
      {  xmin = o->bstr[i]->x;  ymin = o->bstr[i]->y;
	 ret = o->bstr[i]->sw;
      }
   }
   return(!ret ? c : ret);
}

int e_opt_kst(o)
     W_OPTSTR *o;
{
   int ret = 0, csv, sw = 1, i, j, num, cold, c = o->bgsw;
   char *tmp;
   fk_cursor(0);
   o->pic = e_std_kst(o->xa, o->ya, o->xe, o->ye, o->name, 1, o->frt, o->ftt, o->frs);
   if(o->pic == NULL) {  e_error(e_msg[0], 0, o->f->fb); return(-1);  }
   if(!c) c = e_get_opt_sw(CDO, 0, 0, o);
   for(i = 0; i < o->tn; i++)
   e_pr_str(o->xa+o->tstr[i]->x, o->ya+o->tstr[i]->y, o->tstr[i]->txt,
						o->ftt, -1, 0, 0, 0);
   for(i = 0; i < o->wn; i++)
   {  e_pr_str(o->xa+o->wstr[i]->xt, o->ya+o->wstr[i]->yt, o->wstr[i]->header,
					o->ftt, o->wstr[i]->nc, 1, o->fts, 0);
      if(!o->wstr[i]->df)
      e_schr_nchar(o->wstr[i]->txt, o->xa+o->wstr[i]->xw,
		    o->ya+o->wstr[i]->yw, 0, o->wstr[i]->nw, o->fwt);
      else
      e_schr_nchar_wsv(o->wstr[i]->txt, o->xa+o->wstr[i]->xw,
		    o->ya+o->wstr[i]->yw, 0, o->wstr[i]->nw, o->fwt, o->fws);
   }
   for(i = 0; i < o->nn; i++)
   {  e_pr_str(o->xa+o->nstr[i]->xt, o->ya+o->nstr[i]->yt, o->nstr[i]->header,
					o->ftt, o->nstr[i]->nc, 1, o->fts, 0);
      e_schr_nzif(o->nstr[i]->num, o->xa+o->nstr[i]->xw, o->ya+o->nstr[i]->yw,
						  o->nstr[i]->nw, o->fwt);
   }
   for(i = 0; i < o->sn; i++)
   {  e_pr_str(o->xa+o->sstr[i]->x+4, o->ya+o->sstr[i]->y, o->sstr[i]->header,
					o->fst, o->sstr[i]->nc, 1, o->fss, 0);
#ifdef NEWSTYLE
      if(e_we_sw & 1)
      e_pr_str(o->xa+o->sstr[i]->x, o->ya+o->sstr[i]->y, "   ",
						   o->fst, -1, 1, 0, 0);
      else
#endif
      e_pr_str(o->xa+o->sstr[i]->x, o->ya+o->sstr[i]->y, "[ ]", o->fst, -1, 1, 0, 0);
   }
   for(i = 0; i < o->pn; i++)
   {  for(j = 0; j < o->pstr[i]->np; j++)
      {  e_pr_str(o->xa+o->pstr[i]->ps[j]->x, o->ya+o->pstr[i]->ps[j]->y, "[ ] ",
							o->fst, -1, 1, 0, 0);
	 e_pr_str(o->xa+o->pstr[i]->ps[j]->x+4, o->ya+o->pstr[i]->ps[j]->y,
		o->pstr[i]->ps[j]->header, o->fst, o->pstr[i]->ps[j]->nc,
							     1, o->fss, 0);
#ifdef NEWSTYLE
	 if(e_we_sw & 1)
	 e_pr_str(o->xa+o->pstr[i]->ps[j]->x,
		 o->ya+o->pstr[i]->ps[j]->y, "   ", o->fst, -1, 1, 0, 0);
	 else
#endif
	 e_pr_str(o->xa+o->pstr[i]->ps[j]->x, o->ya+o->pstr[i]->ps[j]->y, "[ ]", o->fst, -1, 1, 0, 0);
      }
   }
   for(i = 0; i < o->bn; i++)
   {  e_pr_str(o->xa+o->bstr[i]->x, o->ya+o->bstr[i]->y, o->bstr[i]->header,
				o->fbt, o->bstr[i]->nc, -1, o->fbs, o->ftt);
   }
   cold = c;
   while (c != ESC || sw)
   {
#ifdef NEWSTYLE
      if(e_we_sw & 1)
      {  for(i = 0; i < o->sn; i++)
	 {  if(o->sstr[i]->num)
	    e_make_xrect_abs(o->xa+o->sstr[i]->x, o->ya+o->sstr[i]->y,
			o->xa+o->sstr[i]->x + 2, o->ya+o->sstr[i]->y, 1);
	    else
	    e_make_xrect_abs(o->xa+o->sstr[i]->x, o->ya+o->sstr[i]->y,
			o->xa+o->sstr[i]->x + 2, o->ya+o->sstr[i]->y, 0);
	 }
	 for(i = 0; i < o->pn; i++)
	 for(j = 0; j < o->pstr[i]->np; j++)
	 {  if(o->pstr[i]->num == j)
	    e_make_xrect_abs(o->xa+o->pstr[i]->ps[j]->x, o->ya+o->pstr[i]->ps[j]->y,
			o->xa+o->pstr[i]->ps[j]->x + 2, o->ya+o->pstr[i]->ps[j]->y, 1);
	    else
	    e_make_xrect_abs(o->xa+o->pstr[i]->ps[j]->x, o->ya+o->pstr[i]->ps[j]->y,
			o->xa+o->pstr[i]->ps[j]->x+2, o->ya+o->pstr[i]->ps[j]->y, 0);
	 }
      }
      else
#endif
      {  for(i = 0; i < o->sn; i++)
	 {  if(o->sstr[i]->num)
	    e_pr_char(o->xa+o->sstr[i]->x+1, o->ya+o->sstr[i]->y, 'X', o->fst);
	    else
	    e_pr_char(o->xa+o->sstr[i]->x+1, o->ya+o->sstr[i]->y, ' ', o->fst);
	 }
	 for(i = 0; i < o->pn; i++)
	 for(j = 0; j < o->pstr[i]->np; j++)
	 {  if(o->pstr[i]->num == j)
	    e_pr_char(o->xa+o->pstr[i]->ps[j]->x+1, o->ya+o->pstr[i]->ps[j]->y, SWSYM, o->fst);
	    else
	    e_pr_char(o->xa+o->pstr[i]->ps[j]->x+1, o->ya+o->pstr[i]->ps[j]->y, ' ', o->fst);
	 }
      }
      if((c == AF2 && !(o->f->ed->edopt & 1))
		|| (o->f->ed->edopt & 1 && c == CtrlL))
      {  e_opt_move(o);  c = cold;  continue;  }
      for(i = 0; i < o->wn; i++)
      if(o->wstr[i]->sw == c || (o->wstr[i]->nc >= 0 &&
		toupper(c) == o->wstr[i]->header[o->wstr[i]->nc]))
      {  cold = c;
	 tmp = MALLOC((o->wstr[i]->wmx + 1) * sizeof(char));
	 strcpy(tmp, o->wstr[i]->txt);
#if  MOUSE
	 if(!o->wstr[i]->df && (c = e_schreib_leiste(tmp,
		     o->xa+o->wstr[i]->xw, o->ya+o->wstr[i]->yw,
		     o->wstr[i]->nw, o->wstr[i]->wmx, o->fwt, o->fws)) < 0)
	     c = e_opt_mouse(o);
	 else if(o->wstr[i]->df && (c = e_schr_lst_wsv(tmp,
		     o->xa+o->wstr[i]->xw, o->ya+o->wstr[i]->yw,
		     o->wstr[i]->nw, o->wstr[i]->wmx, o->fwt, o->fws,
		     o->wstr[i]->df, o->f)) < 0)
	     c = e_opt_mouse(o);
#else
	 if(!o->wstr[i]->df)
		c = e_schreib_leiste(tmp,
		     o->xa+o->wstr[i]->xw, o->ya+o->wstr[i]->yw,
		     o->wstr[i]->nw, o->wstr[i]->wmx, o->fwt, o->fws);
	 else if(o->wstr[i]->df)
		c = e_schr_lst_wsv(tmp,
		     o->xa+o->wstr[i]->xw, o->ya+o->wstr[i]->yw,
		     o->wstr[i]->nw, o->wstr[i]->wmx, o->fwt, o->fws,
		     o->wstr[i]->df, o->f);
#endif
	 if(c != ESC) strcpy(o->wstr[i]->txt, tmp);
	 csv = c;
	 if(!o->wstr[i]->df)
	 e_schr_nchar(o->wstr[i]->txt, o->xa+o->wstr[i]->xw,
		    o->ya+o->wstr[i]->yw, 0, o->wstr[i]->nw, o->fwt);
	 else
	 e_schr_nchar_wsv(o->wstr[i]->txt, o->xa+o->wstr[i]->xw,
		    o->ya+o->wstr[i]->yw, 0, o->wstr[i]->nw, o->fwt, o->fws);
	 if((c = e_get_opt_sw(c, o->xa+o->wstr[i]->xw,
				o->ya+o->wstr[i]->yw, o)) != csv)  sw = 1;
	 else sw = 0;
/*
	      if(c == ESC) sw = 1;
	      else if(c == CR)  {  sw = 1;  c = o->crsw;  }
*/
	 if(c == CR)  c = o->crsw;
	 else if(c == ESC) ret = ESC;
	 free(tmp);
	 fk_cursor(0);
	 break;
      }
      if(i < o->wn) continue;
      for(i = 0; i < o->nn; i++)
      if(o->nstr[i]->sw == c || (o->nstr[i]->nc >= 0 &&
		toupper(c) == o->nstr[i]->header[o->nstr[i]->nc]))
      {  cold = c;
	 num = o->nstr[i]->num;
#if MOUSE
	 if((c = e_schreib_zif(&num, o->xa+o->nstr[i]->xw, o->ya+o->nstr[i]->yw,
			o->nstr[i]->nw, o->fwt, o->fws)) < 0)
	 c = e_opt_mouse(o);
#else
	 c = e_schreib_zif(&num, o->xa+o->nstr[i]->xw, o->ya+o->nstr[i]->yw,
			o->nstr[i]->nw, o->fwt, o->fws);
#endif
	 if(c != ESC) o->nstr[i]->num = num;
	 csv = c;
	 if((c = e_get_opt_sw(c, o->xa+o->nstr[i]->xw,
				o->ya+o->nstr[i]->yw, o)) != csv)  sw = 1;
	 else sw = 0;
	 if(c != cold) e_schr_nzif(o->nstr[i]->num, o->xa+o->nstr[i]->xw,
				o->ya+o->nstr[i]->yw, o->nstr[i]->nw, o->fwt);
/*
	      if(c == ESC) sw = 1;
	      else if(c == CR)  {  sw = 1;  c = o->crsw;  }
*/
	 if(c == CR)  c = o->crsw;
	 else if(c == ESC) ret = ESC;
	 fk_cursor(0);
	 break;
      }
      if(i < o->nn) continue;
      for(i = 0; i < o->sn; i++)
      if(o->sstr[i]->sw == c || (o->sstr[i]->nc >= 0 &&
		toupper(c) == o->sstr[i]->header[o->sstr[i]->nc]))
      {  if(!sw)
	 {  o->sstr[i]->num = !o->sstr[i]->num;
	    sw = 1;
	    c = cold;
	    break;
	 }
	 cold = c;
	 e_pr_str(o->xa+o->sstr[i]->x+4, o->ya+o->sstr[i]->y, o->sstr[i]->header,
					o->fsa, o->sstr[i]->nc, 1, o->fsa, 0);
#if MOUSE
	 if((c = e_getch()) < 0) c = e_opt_mouse(o);
#else
	 c = e_getch();
#endif
	 if(c == CR) {  sw = 0;  c = cold;  break;  }
	 else if(c == ESC)  sw = 1;
	 else
	 {  csv = c;
	    if((c = e_get_opt_sw(c, o->xa+o->sstr[i]->x,
				o->ya+o->sstr[i]->y, o)) != csv)  sw = 1;
	    else sw = 0;
	 }
	 if(c != cold)
	 e_pr_str(o->xa+o->sstr[i]->x+4, o->ya+o->sstr[i]->y, o->sstr[i]->header,
					o->fst, o->sstr[i]->nc, 1, o->fss, 0);
	 break;
      }
      if(i < o->sn) continue;
      for(i = 0; i < o->pn; i++)
      {  for(j = 0; j < o->pstr[i]->np; j++)
	 if(o->pstr[i]->ps[j]->sw == c || (o->pstr[i]->ps[j]->nc >= 0 &&
	    toupper(c) == o->pstr[i]->ps[j]->header[o->pstr[i]->ps[j]->nc]))
	 {  if(!sw)
	    {  o->pstr[i]->num = j;
	       sw = 1;
	       c = cold;
	       break;
	    }
	    cold = c;
	    e_pr_str(o->xa+o->pstr[i]->ps[j]->x+4, o->ya+o->pstr[i]->ps[j]->y, o->pstr[i]->ps[j]->header,
				o->fsa, o->pstr[i]->ps[j]->nc, 1, o->fsa, 0);
#if MOUSE
	    if((c = e_getch()) < 0) c = e_opt_mouse(o);
#else
	    c = e_getch();
#endif
	    if(c == CR)  {  sw = 0;  c = cold;  break;  }
	    else if(c == ESC)  sw = 1;
	    {  csv = c;
	       if((c = e_get_opt_sw(c, o->xa+o->pstr[i]->ps[j]->x,
				o->ya+o->pstr[i]->ps[j]->y, o)) != csv)  sw = 1;
	       else sw = 0;
	    }
	    if(c != cold)
	    e_pr_str(o->xa+o->pstr[i]->ps[j]->x+4, o->ya+o->pstr[i]->ps[j]->y, o->pstr[i]->ps[j]->header,
				o->fst, o->pstr[i]->ps[j]->nc, 1, o->fss, 0);
	    break;
	 }
	 if(j < o->pstr[i]->np) break;
      }
      if(i < o->pn) continue;
      for(i = 0; i < o->bn; i++)
      if(o->bstr[i]->sw == c || (o->bstr[i]->nc >= 0 &&
		toupper(c) == o->bstr[i]->header[o->bstr[i]->nc]))
      {  e_pr_str(o->xa+o->bstr[i]->x, o->ya+o->bstr[i]->y, o->bstr[i]->header,
				o->fbz, o->bstr[i]->nc, -1, o->fbz, o->ftt);
	 if(!sw)
	 {  if(o->bstr[i]->fkt != NULL)
	    {  if((ret = o->bstr[i]->fkt(o->f)) > 0) c = ESC;
	       else
	       {  c = cold;
		  e_pr_str(o->xa+o->bstr[i]->x, o->ya+o->bstr[i]->y, o->bstr[i]->header,
				o->fbt, o->bstr[i]->nc, -1, o->fbs,  o->ftt);
	       }
	    }
	    else {  ret = o->bstr[i]->sw;  c = ESC;  }
	    break;
	 }
	 cold = c;
#if MOUSE
	 if((c = e_getch()) < 0) c = e_opt_mouse(o);
#else
	 c = e_getch();
#endif
	 if(c == CR) {  ret = c = o->bstr[i]->sw;  sw = 0;  break;  }
	 else if(c == ESC) {  sw = 0;  ret = ESC;  break;  }
	 csv = c;
	 if((c = e_get_opt_sw(c, o->xa+o->bstr[i]->x,
				o->ya+o->bstr[i]->y, o)) != csv)  sw = 1;
	 else sw = 0;
	 if(c != cold)
	 e_pr_str(o->xa+o->bstr[i]->x, o->ya+o->bstr[i]->y, o->bstr[i]->header,
				o->fbt, o->bstr[i]->nc, -1, o->fbs,  o->ftt);
	 break;
      }
      if(i < o->bn) continue;
      
      c = cold;
      sw = 1;
   }
   e_close_view(o->pic, 1);
   return(ret);
}

int e_edt_options(f)
     FENSTER *f;
{
   extern int b_dif;
   int i, ret, mdsv = f->ed->dtmd, edopt = f->ed->edopt;
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o) return(-1);
   o->xa = 15;  o->ya = 3;  o->xe = 63;  o->ye = 21;
   o->bgsw = AltO;
   o->name = "Editor-Options";
   o->crsw = AltO;
   e_add_txtstr(3, 2, "Data:", o);
   e_add_txtstr(3, 6, "Display:", o);
   e_add_txtstr(24, 2, "Autosave:", o);
   e_add_txtstr(24, 6, "Keys:", o);
   e_add_txtstr(24, 10, "Auto-Indent:", o);
   e_add_numstr(3, 10, 18, 10, 3, 100, 0, AltM, "Max. Colums:", f->ed->maxcol, o);
   e_add_numstr(3, 11, 19, 11, 2, 100, 0, AltT, "Tabstops:", f->ed->tabn, o);
   e_add_numstr(3, 12, 18, 12, 3, 1000, 2, AltX, "MaX. Changes:", f->ed->maxchg, o);
   e_add_numstr(3, 13, 19, 13, 2, 100, 0, AltN, "Num. Undo:", f->ed->numundo, o);
   e_add_numstr(3, 14, 19, 14, 2, 100, 5, AltI, "Auto Ind. Col.:", b_dif, o);
   e_add_sswstr(4, 7, 0, AltS, f->ed->dtmd == 's' ? 1 : 0, "Show Endmark", o);
   e_add_sswstr(25, 3, 1, AltP, f->ed->autosv & 1, "OPtions         ", o);
   e_add_sswstr(25, 4, 1, AltH, f->ed->autosv & 2 ? 1 : 0, "CHanges         ", o);
   e_add_pswstr(0, 4, 3, 0, AltA, 0, "ASCII       ", o);
   e_add_pswstr(0, 4, 4, 0, AltB, f->ed->dtmd == 'b' ? 1 : 0, "Binary      ", o);
   e_add_pswstr(1, 25, 7, 1, AltL, 0, "OLd-Style       ", o);
   e_add_pswstr(1, 25, 8, 0, AltC, f->ed->edopt & 1, "CUA-Style       ", o);
   e_add_pswstr(2, 25, 11, 3, AltY, 0, "OnlY Source-Text", o);
   e_add_pswstr(2, 25, 12, 3, AltW, 0, "AllWays         ", o);
   e_add_pswstr(2, 25, 13, 2, AltV, f->ed->edopt & 256 ? 1 :
			f->ed->edopt & 128 ? 0 : 2, "NeVer           ", o);
   e_add_bttstr(12, 16, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(31, 16, -1, ESC, "Cancel", NULL, o);
   ret = e_opt_kst(o);
   if(ret != ESC)
   {  if(o->pstr[0]->num) f->ed->dtmd = 'b';
      else if(o->sstr[0]->num) f->ed->dtmd = 's';
      else f->ed->dtmd = 'n';
      f->ed->autosv = o->sstr[1]->num + (o->sstr[2]->num << 1);
      f->ed->maxcol = o->nstr[0]->num;
      f->ed->tabn = o->nstr[1]->num;
      f->ed->maxchg = o->nstr[2]->num;
      f->ed->numundo = o->nstr[3]->num;
      f->ed->edopt = ((f->ed->edopt & ~385) + o->pstr[1]->num)
	  + (o->pstr[2]->num == 0 ? 128 : 0) 
	  + (o->pstr[2]->num == 1 ? 256 : 0);
      if(edopt != f->ed->edopt)
      {  e_switch_blst(f->ed);
         for(i = 0; i <= f->ed->mxedt; i++)
	   if((f->ed->edopt & 256) || ((f->ed->edopt & 128) 
				&& f->ed->f[i]->c_st)) f->ed->f[i]->flg = 1;
	   else f->ed->f[i]->flg = 0;
	 e_repaint_desk(f);
      }
      if(f->ed->dtmd != mdsv)
      {  for(i = 0; i <= f->ed->mxedt; i++)
	 if(f->ed->f[i]->dtmd == mdsv) f->ed->f[i]->dtmd = f->ed->dtmd;
	 e_repaint_desk(f);
      }
   }
   freeostr(o);
   return(0);
}


int e_read_colors(f)
     FENSTER *f;
{
   int ret;
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o) return(-1);
   o->xa = 20;  o->ya = 4;  o->xe = 52;  o->ye = 14;
   o->bgsw = AltO;
   o->name = "Read Colors";
   o->crsw = AltO;
   e_add_wrstr(4, 5, 4, 6, 25, 128, 7, AltF, "Option-File:", f->ed->optfile, NULL, o);
   e_add_pswstr(0, 5, 2, 0, AltS, 0, "Standard Colors", o);
   e_add_pswstr(0, 5, 3, 0, AltR, 0, "Read from File ", o);
   e_add_bttstr(6, 8, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(21, 8, -1, ESC, "Cancel", NULL, o);
   ret = e_opt_kst(o);
   if(ret != ESC)
   {  if(!o->pstr[0]->num) e_ini_farbe(f->fb, 1);
      else {  strcpy(f->ed->optfile, o->wstr[0]->txt); e_read_cl(f);  }
      e_repaint_desk(f);
   }
   freeostr(o);
   return(0);
}

int e_read_help_str(cn)
     ECNT *cn;
{
   FILE *fp;
   char str[128];
   int i, len;
   sprintf(str, "%s/we_hlp_str", cn->libdrct);
   for(i = 0; i < E_HLP_NUM; i++)
   {  e_hlp_str[i] = MALLOC(sizeof(char));
      *e_hlp_str[i] = '\0';
   }
   if(!(fp = fopen(str, "rb"))) return(-1);
   for(i = 0; i < E_HLP_NUM && fgets(str, 128, fp); i++)
   {
#ifndef PROG
      if(i == 11)
      {  int j;
         for(j = 0; j < 3; j++) fgets(str, 128, fp);
      }
#elif !defined(DEBUGGER)
      if(i == 12) fgets(str, 128, fp);
#endif
      if(str[len=strlen(str)-1] == '\n') str[len] = '\0';
      else len++;
      e_hlp_str[i] = REALLOC(e_hlp_str[i], (len+1)*sizeof(char));
      strcpy(e_hlp_str[i], str);
   }
   fclose(fp);
   return(i == E_HLP_NUM ? 0 : -2);
}

