/* we_e_aus.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"

/*
      Einfaerben des ganzen Schirms mit einer Farbe und einem Character   */

void e_cls(frb, chr)
     int frb;
     int chr;
{
   int i, j;
   for (j = 0; j < MAXSLNS; j++)
   for (i = 0; i < MAXSCOL; i++)
   e_pr_char(i, j, chr, frb);
}

/*
	  Schreiben eines Text-Strings       */

int e_puts(s, xa, ya, frb)
     char *s;
     int xa;
     int ya;
     int frb;
{
   int i;
   if(xa >= MAXSCOL || ya > MAXSLNS) return(-1);
   for(i = 0; s[i] != '\0' && i < 2000; i++)
   e_pr_char(i + xa, ya, s[i], frb);
   return(0);
}
/*
	  Schreiben eines Text-Strings       */

void e_pr_str(x, y, str, col, b2, n2, col2, col3)
     int x;
     int y;
     char *str;
     int col;
     int b2;
     int n2;
     int col2;
     int col3;
{
   int i, sw = 0;
   if(n2 < 0) {  sw = 1; n2 = -n2;  }
   e_pr_char(x-1, y, 32, col);
   for (i=0; *(str+i) != '\0'; i++)
   {  if(i>=b2 && i<b2+n2)
      e_pr_char(x+i, y, *(str+i), col2);
      else
      e_pr_char(x+i, y, *(str+i), col);
   }
   e_pr_char(x+i, y, 32, col);
#if defined(DOS) || defined(DJGPP)
   if(sw == 1)
#else
   if(sw == 1 && e_we_sw & 1)
#endif
#ifdef NEWSTYLE
   e_make_xrect(x-1, y, x+i, y, 0);
#else
   {  i++;
      e_pr_char(x+i, y, SCR, col3);
      for (; i >= 0; i--)
      e_pr_char(x+i+MAXSCOL, y, SCD, col3);
   }
#endif
}

int e_pr_str_wsd(x, y, str, col, b2, n2, col2, bg, nd)
     int x;
     int y;
     char *str;
     int col;
     int b2;
     int n2;
     int col2;
     int bg;
     int nd;
{
   int i;
   for(i = bg; i < x; i++) e_pr_char(i, y, ' ', col);
   for(i = 0; str[i] && i <= nd-x; i++)
   {  if(i>=b2 && i<b2+n2)
      e_pr_char(i+x, y, *(str+i), col2);
      else
      e_pr_char(i+x, y, *(str+i), col);
   }
   for(i+=x; i <= nd; i++) e_pr_char(i, y, ' ', col);
#ifdef NEWSTYLE
   if(e_we_sw & 1) e_make_xrect(bg, y, nd, y, 0);
#endif
   return(0);
}

int e_pr_str_scan(x, y, str, col, b2, n2, col2, bg, nd)
     int x;
     int y;
     char *str;
     int col;
     int b2;
     int n2;
     int col2;
     int bg;
     int nd;
{
   char txt[30], *pt;
   if(e_we_sw & 1)
   return(e_pr_str_wsd(x, y, str, col, b2, n2, col2, bg, nd));
   strcpy(txt, str);
   if((pt = strstr(txt, "Alt")) != NULL)
   {  pt[0] = 'E'; pt[1] = 'S'; pt[2] = 'C';  }
   else if(!strncmp(txt, "^F4 Close", 9)) strcpy(txt, "ESC X Cl.");
   return(e_pr_str_wsd(x, y, txt, col, b2, n2, col2, bg, nd));
}
/*
       String aus Zahlen schreiben   */

int e_pr_zstring(s, x, y, n, fb)
     char *s;
     int x;
     int y;
     int n;
     int fb;
{
   int i, len = strlen(s);
   for(i = 0; i < n - len; i++)
   e_pr_char(x+i, y, ' ', fb);
   for(; i < n; i++)
   e_pr_char(x+i, y, s[i-n+len], fb);
#ifdef NEWSTYLE
   if(e_we_sw & 1) e_make_xrect(x, y, x+n-1, y, 1);
#endif
   return(len);
}

/*   N Zeichen auf Schirm schreiben   */

int e_schr_nchar(s, x, y, n, max, frb)
     char *s;
     int x;
     int y;
     int n;
     int max;
     int frb;
{
   int i, j;
   e_pr_char(x, y, ' ', frb);
   for(i = 1, j = 0; i < max-1 && s[n+j] !='\0'; i++, j++)
#ifdef DOS
   e_pr_char(x+i, y, s[n+j], frb);
#else
   {  if((unsigned char) s[n+j] < ' ')
      {  e_pr_char(x+i, y, '^', frb);
	 i++;
	 e_pr_char(x+i, y, s[n+j] + '@', frb);
      }
      else  e_pr_char(x+i, y, s[n+j], frb);
   }
#endif
   for(; i < max; i++)
   e_pr_char(x+i, y, ' ', frb);
#ifdef NEWSTYLE
   e_make_xrect(x, y, x+max-1, y, 1);
#endif
   return(i);
}
/*
	  Schreiben eines Text-Strings       */

void e_pr_nstr(x, y, n, str, col, col2)
     int x;
     int y;
     int n;
     char *str;
     int col;
     int col2;
{
   int i;
   e_pr_char(x-1, y, ' ', col);
   for (i=0; *(str+i) != '\0' && i < n-1; i++)
   e_pr_char(x+i, y, *(str+i), col);
   if(i < n-1)  e_pr_char(x+i, y, ' ', col);
   for (i++; i < n-1; i++)
   e_pr_char(x+i, y, ' ', col2);
}
/*
	 Zifferneingabe    */

int e_schreib_zif(num, x, y, max, ft, fs)
     int *num;
     int x;
     int y;
     int max;
     int ft;
     int fs;
{
#if  MOUSE
   extern struct mouse e_mouse;
#endif
   int c, i, jc = max-1, ntmp = *num, nnum = e_anz_zif(ntmp);
   int first = 1;
   char *s = MALLOC((max+1)*sizeof(char));
   e_pr_zstring(e_numtozif(ntmp, max, s), x, y, max, fs);
   fk_locate(x+jc, y);
   fk_cursor(1);
   while ((c = e_getch()) != ESC)
   {
#if  MOUSE
      if(c < 0)
      {  if(e_mouse.y == y && e_mouse.x >= x && e_mouse.x < x+max)
	 {  jc = e_mouse.x-x > max-nnum ? e_mouse.x-x : max-nnum;  }
	 else
	 {  if(c > -2) *num = e_ziftonum(s);
	    FREE(s);
	    return(c);
	 }
      }
#endif
      if (c == 332) {  if(jc < max-1) jc++;  }
      else if (c == 330) {  if(jc > max-nnum) jc--;  }
      else if (c == 326) jc = max-nnum;
      else if (c == 334) jc = max-1;
      else if (c == DC)
      {  if(jc > max-nnum)
	 {  for( i = jc-1; i > 0; i--)
	    s[i] = s[i-1];
	    s[0] = 32;
	    nnum--;
	 }
      }
      else if (c == ENTF)
      {  if(jc < max)
	 {  for( i = jc; i > 0; i--)
	    s[i] = s[i-1];
	    s[0] = 32;
	    nnum--;
	 }
      }
      else if (first == 1 && c >='0' && c <= '9')
      {  for(i = 0; i < max - 1; i++) s[i] = 32;
	 s[i] = c;
	 s[max] = '\0';
	 nnum = 1;
      }
      else if (c >='0' && c <= '9')
      {  for( i = 0; i < jc; i++)
	 s[i] = s[i+1];
	 s[jc] = c;
	 if(nnum < max) nnum++;
      }
      else if(c > 0 && (c < 32 || c > 255))
      {  if(c != ESC) *num = e_ziftonum(s);
	 FREE(s);
	 return(c);
      }
      if(jc > max -1) jc = max-1;
      else if(jc < max - nnum) jc = max-nnum;
      
      e_pr_zstring(s, x, y, max, ft);
      fk_locate(x+jc, y);
      first = 0;
   }
   return(c);
}

/*    Zeichen in einer Zeile schreiben    */

int e_schreib_leiste(s, x, y, n, max, ft, fs)
     char *s;
     int x;
     int y;
     int n;
     int max;
     int ft;
     int fs;
{
#if  MOUSE
   extern struct mouse e_mouse;
#endif
   int c, i, ja = 0, jc, l = strlen(s);
#ifndef DOS
   int jd;
#endif
   int sond = 0, first = 1;
   unsigned char *tmp = MALLOC(max+1);
   fk_cursor(1);
   strcpy(tmp, s);
   jc = l;
#ifdef DOS
   if(jc > n-2) jc = n-3;
#else
   for(jd = 0, i = ja; tmp[i] && i <= n-3 + jd; i++) if(tmp[i] < ' ') jd++;
   if(jc + jd > n-2) jc = n-3-jd;
#endif
   e_schr_nchar(tmp, x, y, 0, n, fs);
   e_pr_char(x, y, ' ', ft);
   e_pr_char(x+n-1, y, ' ', ft);
#ifdef DOS
   fk_locate(x+jc+1, y);
#else
   fk_locate(x+jc+jd+1, y);
#endif
#ifdef NEWSTYLE
   e_make_xrect(x, y, x+n-1, y, 1);
#endif
   while ((c = e_getch()) != ESC)
   {
#if  MOUSE
      if(c < 0)
      {  if(e_mouse.y == y && e_mouse.x > x && e_mouse.x < x+n)
	 {  jc = e_mouse.x-x-1 < l ? e_mouse.x-x-1 : l;
	    if(c == -2)
	    {  extern struct EXT h_error;
	       int len = h_error.cn->f[0]->b->bf[0].len;
#ifdef XWINDOW
	       if (bioskey(2) & 8)
	       e_cp_X_to_buffer(h_error.cn->f[h_error.cn->mxedt]);
#endif
	       while(e_mshit());
	       if(first == 1)
	       {  tmp[0] = '\0';
		  l = 0;
		  jc = 0;
	       }
	       if(len + l > max) len = max - l;
	       for( i = l; i >= jc; i--)
	       tmp[i+len] = tmp[i];
	       for( i = jc; i < jc + len; i++)
	       tmp[i] = h_error.cn->f[0]->b->bf[0].s[i-jc];
	       jc += len;
	       l += len;
	    }
	 }
	 else
	 {  if(c > -4) strcpy(s, tmp);
	    FREE(tmp);
	    fk_cursor(0);
	    return(c);
	 }
      }
#endif
      if (c == CRI || (!sond && c == CtrlF)) {  if(jc < l) jc++;  }
      else if (c == CLE || (!sond && c == CtrlB)) {  if(jc > 0) jc--;  }
      else if (c == CCLE && jc > 0) jc = e_su_rblk(jc, tmp);
      else if (c == CCRI && jc < l) jc = e_su_lblk(jc, tmp);
      else if (c == POS1 || (!sond && c == CtrlA)) jc = 0;
      else if (c == ENDE || (!sond && c == CtrlE)) jc = l;
      else if (c == AltJ || c == F9) sond = !sond;
#ifndef DOS
      else if (c == PASTE || c == ShiftEin || c == CtrlV || c == AltEin)
#else
      else if (c == ShiftEin || c == CtrlV)
#endif
      {  extern struct EXT h_error;
	 int len = h_error.cn->f[0]->b->bf[0].len;
#ifdef XWINDOW
	 if ((e_we_sw & 1) && c == AltEin)
	 e_cp_X_to_buffer(h_error.cn->f[h_error.cn->mxedt]);
#endif
	 if(first == 1)
	 {  tmp[0] = '\0';
	    l = 0;
	    jc = 0;
	 }
	 if(len + l > max) len = max - l;
	 for( i = l; i >= jc; i--)
	 tmp[i+len] = tmp[i];
	 for( i = jc; i < jc + len; i++)
	 tmp[i] = h_error.cn->f[0]->b->bf[0].s[i-jc];
	 jc += len;
	 l += len;
      }
      else if (c == DC && !sond)
      {  if(jc > 0)
	 {  for( i = jc-1; i < l; i++)
	    tmp[i] = tmp[i+1];
	    jc--;
	    l--;
	 }
      }
      else if (c == ENTF || (!sond && c == CtrlD))
      {  if(jc < l)
	 {  for( i = jc; i < l; i++)
	    tmp[i] = tmp[i+1];
	    l--;
	 }
      }
      else if (!sond && c == CtrlT)
      {  c = e_su_lblk(jc, tmp) - jc;
	 {  for( i = jc; i <= l-c; i++)
	    tmp[i] = tmp[i+c];
	    l -= c;
	 }
      }
      else if (!sond && c == CtrlO)
      {  if((c = e_getch()) != CtrlT && toupper(c) != 'T') continue;
	 c = jc - e_su_rblk(jc, tmp);
	 {  for( i = jc-c; i <= l-c; i++)
	    tmp[i] = tmp[i+c];
	    jc -= c;
	    l -= c;
	 }
      }
      else if (first == 1 && ((c != CR && c != CtrlP && c != CtrlN)
					|| sond) && c > 0 && c < 0x7f)
      {  tmp[0] = c;
	 tmp[1] = '\0';
	 l = 1;
	 jc = 1;
      }
      else if(((c != CR && c != CtrlP && c != CtrlN) || sond)
					&& c > 0 && c < 0xff)
      {  if(l < max)
	 {  for( i = l; i >= jc; i--)
	    tmp[i+1] = tmp[i];
	    tmp[jc] = c;
	    l++;
	    jc++;
	 }
      }
      else if(c > 0)
      {  if(c != ESC) strcpy(s, tmp);
	 FREE(tmp);
	 fk_cursor(0);
	 if(c == CtrlP) c = CUP;
	 else if(c == CtrlN) c = CDO;
	 return(c);
      }
#ifdef DOS
      if(jc-ja > n-3) ja=jc-n+3 ;
#else
      for(jd = 0, i = ja; i < jc; i++) if(tmp[i] < ' ') jd++;
      if(jc+jd-ja > n-3) ja=jc+jd-n+3 ;
#endif
      else if(jc-ja < 0) ja = jc;
      
      e_schr_nchar(tmp, x, y, ja, n, ft);
      
      e_pr_char(x, y, ja > 0 ? MCL : ' ', ft);
      e_pr_char(x+n-1, y, l-ja > n-2 ? MCR : ' ', ft);
#ifdef DOS
      fk_locate(x+jc-ja+1, y);
#else
      fk_locate(x+jc+jd-ja+1, y);
#endif
      first = 0;
#ifdef NEWSTYLE
      e_make_xrect(x, y, x+n-1, y, 1);
#endif
   }
   fk_cursor(0);
   return(c);
}

int e_schr_nzif(num, x, y, max, col)
     int num;
     int x;
     int y;
     int max;
     int col;
{
   char *str = MALLOC((max+1)*sizeof(char));
   int i, nt;
   for(i = 0, nt = 1; i < max; i++) nt *= 10;
   if(num >= nt) num = nt - 1;
   e_pr_zstring(e_numtozif(num, max, str), x, y, max, col);
   FREE(str);
   return(0);
}


