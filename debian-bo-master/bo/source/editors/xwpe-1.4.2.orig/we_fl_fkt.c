/* we_fl_nw.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"
#include "xkeys.h"
#include "makro.h"
#ifdef UNIX
#include <sys/types.h>
#include <sys/stat.h>
#endif
#ifdef DEFPGC
#define putc(c, fp) fputc(c, fp)
#endif

int e_read_help();
char *e_strcstr();
int e_strccmp();
int e_read_info();
int e_switch_screen();

char *info_file = NULL;
char *e_tmp_dir = NULL;

char *e_getedpath(fl)
     char *fl;
{
   char *pth;
   int n = strlen(fl);
   for(n--; n >= 0 && fl[n] != DIRC; n--);
#ifdef UNIX
   if(n < 0)
#ifdef DJGPP
   {  if(!(fl = getenv("COMPILER_PATH"))) fl = DESTDIR;
#else
   {  fl = DESTDIR;
#endif
      n = strlen(fl);
   }
#else
   if(n < 0) n = 0;
#endif
   pth = MALLOC((n+1)*sizeof(char));
   pth[n] = '\0';
   for(n--; n >= 0; n--) pth[n] = fl[n];
   return(pth);
}

/*   Vollstaendigen Dateinamen erstellen   */

char *e_mkfilename(dr, fn)
     char *dr;
     char *fn;
{
   char *fl;
   if(!fn) return(NULL);
   if(!dr)
   {  fl = MALLOC(strlen(fn) + 1);
      strcpy(fl, fn);
      return(fl);
   }
   fl = MALLOC(strlen(dr) + strlen(fn) + 2);
   strcpy(fl, dr);
#ifdef DJGPP
   if(dr[0] != '\0' && dr[strlen(dr)-1] != DIRC 
      && dr[strlen(dr)-1] != '/') strcat(fl, DIRS);
#else
   if(dr[0] != '\0' && dr[strlen(dr)-1] != DIRC) strcat(fl, DIRS);
#endif
   strcat(fl, fn);
   return(fl);
}
/*
               Datei - Einlese Routine     */

PUNKT e_readin(i, j, fp, b, sw)
     int i;
     int j;
     FILE *fp;
     BUFFER *b;
     int sw;
{
   PUNKT pkt;
   int ii, k, n = 1, kn = 0, kl = 0, hb = 0;
   char cc, c = 0;
   extern char *e_k_key;
   if(sw == 'b') return(e_readbin(i, j, fp, b));
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pt_tmp(WTCUR);
#endif
   if(sw == 'k') kl = strlen(e_k_key);
   if(i == 0) e_new_line(j, b);
   while (n == 1 && c != EOF)
   {  for(; i < b->mx.x; i++)
      {  if(hb != 2) n = fread(&c, sizeof(char), 1, fp);
	 else {  c = cc;  hb = 0;  }
#if defined(DJGPP) || defined(DOS)
	 if(n == 1 && c == WR && i == 0) {  i--; continue;  }
#endif
	 if(sw == 'k' && c > 32 && c < 127)
	 {  c -= (e_k_key[kn++] - 32);
	    if(c <= 32) c += 94;
	    if(kn >= kl) kn = 0;
	 }
/*
	     else if(sw == 'm' && c == CtrlH)
	     {    i -= 2;   continue;   }
*/
	 else if(sw == 'h' && c == CtrlH)
	 {  if(!hb)
	    {  *(b->bf[j].s+i-1) = HBB;
	       hb = 1;
	    }
	    else i--;
	    n = fread(&c, sizeof(char), 1, fp);
	 }
	 else if(sw == 'h' && hb == 1)
	 {  n = fread(&cc, sizeof(char), 1, fp);
	    if(cc != HBS)
	    {  *(b->bf[j].s+i) = HED;
	       i++;
	       hb = 2;
	    }
	    else n = fread(&c, sizeof(char), 1, fp);
	 }
	 if(n != 1)
	 {  *(b->bf[j].s+i) = '\0';
	    break;
	 }
	 *(b->bf[j].s+i) = c;
#ifdef UNIX
	 if(c == EOF || c== CR || c == WR) break;
#else
	 if(c == EOF || c== CR || c == WR || c == '\0') break;
#endif
      }
      if (c == EOF || n != 1)
      {  if(i == 0 && j > 0)
	 {  FARFREE(b->bf[j].s);
	    for(k = j; k < b->mxlines; k++) b->bf[k] = b->bf[k+1];
	    (b->mxlines)--;
	 }
	 else
	 {  *(b->bf[j].s+i) = WR;
	    *(b->bf[j].s+i+1) = '\0';
	    j++;
	 }
      }
      else if(i >= b->mx.x)
      {  for(ii = i-1; *(b->bf[j].s+ii) != ' ' && *(b->bf[j].s+ii) != '-' && ii > 0; ii--);
	 if(ii <= 1) ii = i - 1;
	 e_new_line(j+1, b);
	 for(k = ii+1; k <= i; k++)
	 {  if(*(b->bf[j].s+k) != '\0') *(b->bf[j+1].s+k-ii-1) = *(b->bf[j].s+k);
	    else *(b->bf[j+1].s+k-ii-1) = ' ';
	 }
	 i = i - ii - 1;
	 *(b->bf[j].s+ii+1) = '\0';
	 j++;
	 if(i < 0) i = 0;
      }
      else if(c == CR || c == WR)
      {  *(b->bf[j].s+i) = WR;
	 *(b->bf[j].s+i+1) = '\0';
	 e_new_line(++j, b);
/*
#if defined(DOS) || defined(DJGPP)
             c = getc(fp);
#endif
*/
	 i = 0;
      }
      else
      {  *(b->bf[j].s+i) = ' ';
	 i++;
	 *(b->bf[j].s+i) = '\0';
      }
      b->bf[j-1].len = e_str_len(b->bf[j-1].s);
      b->bf[j-1].nrc = e_str_nrc(b->bf[j-1].s);
      e_pr_mtul("READING LINE", j, b->fb->mt.fb);
   }
   pkt.x = i;  pkt.y = j;
   e_pr_ul(b->fb);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pt_bak();
#endif
   return(pkt);
}

/*   Einlesen einer binaeren Datei   */

PUNKT e_readbin(i, j, fp, b)
     int i;
     int j;
     FILE *fp;
     BUFFER *b;
{
   PUNKT pkt;
   int c;
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pt_tmp(WTCUR);
#endif
   if(i == 0) e_new_line(j, b);
   c = fread(b->bf[j].s+i, sizeof(char), b->mx.x - i, fp);
   b->bf[j].len = i + c;
   if(c == b->mx.x - i)
   {  for(c = b->mx.x, j++; c == b->mx.x; j++)
      {  e_new_line(j, b);
	 c = fread(b->bf[j].s, sizeof(char), b->mx.x, fp);
	 b->bf[j].len = b->bf[j].nrc = c;
	 e_pr_mtul("READING LINE", j, b->fb->mt.fb);
      }
   }
   pkt.x = c;  pkt.y = j;
   e_pr_ul(b->fb);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pt_bak();
#endif
   return(pkt);
}

/*   Neues Edit-Fenster oeffnen   */

int e_new(f)
     FENSTER *f;
{
   e_edit(f->ed, "");
   return(0);
}

/*   Datei speichern     */

int e_crypt_save(f)
     FENSTER *f;
{
   extern char *e_k_key;
   char str[80];
   if(!e_k_key)
   {  e_k_key = MALLOC(1);  e_k_key[0] = '\0';  }
   strcpy(str, e_k_key);
   if(!e_add_arguments(str, "Password", f, 0, AltP, NULL)) return(-1);
   
   e_k_key = REALLOC(e_k_key, strlen(str) + 1);
   if(e_k_key) strcpy(e_k_key, str);
   if(!e_k_key || e_k_key[0] == '\0') f->dtmd = 'n';
   else f->dtmd = 'k';
   return(e_m_save(f));
}

int e_m_save(f)
     FENSTER *f;
{
   int ret = e_save(f);
   if(ret != ESC) e_message(0, e_msg[20], f);
   return(ret);
}

int e_save(f)
     FENSTER *f;
{
   BUFFER *b = f->ed->f[f->ed->mxedt]->b;
   int ret;
   if(f->dtmd <= 'Z') return(0);
   for(ret = f->ed->mxedt; ret > 0 && f->ed->f[ret] != f; ret--);
   if(!ret) return(0);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pt_tmp(WTCUR);
#endif
   b = f->ed->f[ret]->b;
   ret = e_write(0, 0, b->mx.x, b->mxlines-1, f);
   if(ret != ESC) f->save = 0;
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pt_bak();
#endif
   return(ret);
}

/*     Alle Fenster speichern     */

int e_saveall(f)
     FENSTER *f;
{
   int i, ret = 0;
   ECNT *cn = f->ed;
   for(i = cn->mxedt; i > 0; i--)
   {  if(cn->f[i]->save != 0 && cn->f[i]->ins != 8)
      if(e_save(cn->f[i]) == ESC) ret = ESC;
   }
   if(ret != ESC) e_message(0, e_msg[21], f);
   return(ret);
}

/*     Edit - Sitzung beenden     */

int e_quit(f)
     FENSTER *f;
{
   int i;
   char tmp[128];
#if  MOUSE
   int g[4];
#endif
   ECNT *cn = f->ed;
#ifdef DEBUGGER
   extern int rfildes[], e_d_swtch;
   if(e_d_swtch) {  write(rfildes[1], "q\ny\n", 4);  e_d_swtch = 0;  }
#endif
   for(i = cn->mxedt; i > 0; i--)
   {  if( e_close_window(cn->f[cn->mxedt]) == ESC ) return(ESC);
   }
   if(cn->autosv != 0) e_save_opt(f);
#ifdef SWAP
   if(cn->sp->fp)  {  fclose(cn->sp->fp);  remove(cn->swpfile);  }
   if(cn->sp->fp2)  {  fclose(cn->sp->fp2);  remove(cn->swp2file);  }
#endif
#ifdef UNIX
   if(e_quit_wastebasket(cn->f[0])) return(0);
   sprintf(tmp, "rm -rf %s&", e_tmp_dir);
   system(tmp);
#endif
#if  MOUSE
   g[0] = 2;
   fk_mouse(g);
#endif
   e_cls(cn->fb->ws, ' ');
   fk_locate(0, 0);
   fk_cursor(1);
   e_refresh();
   exit(0);
   return(0);
}

/*   Datei auf Platte schreiben   */

int e_write(xa, ya, xe, ye, f)
     int xa;
     int ya;
     int xe;
     int ye;
     FENSTER *f;
{
   BUFFER *b;  /*   = f->ed->e[f->ed->mxedt]->b;   */
   int i = xa, j, kn = 0, kl;
   extern char *e_k_key;
   char c, tmp[80], *ptmp;
   FILE *fp;
#ifdef SWAP
   char *tstr;   /*  = MALLOC(b->mx.x+1);  */
#endif
   for(j = f->ed->mxedt; j > 0 && f->ed->f[j] != f; j--);
   b = f->ed->f[j]->b;
#ifdef SWAP
   tstr = MALLOC(b->mx.x+1);
#endif
   if(f->ins == 8) return(ESC);
   ptmp = e_mkfilename(f->dirct, f->datnam);
   if(access(ptmp, 0) == 0)
   {  if(access(e_new_qual(ptmp, "bak", tmp), 0) == 0) remove(tmp);
      rename(ptmp, tmp);
   }
   if( (fp = fopen(ptmp, "wb")) == NULL)
   {  e_error(e_msg[6], 0, f->fb);
      FREE(ptmp);
      return(ESC);
   }
#ifdef UNIX
   if(f->filemode >= 0) chmod(ptmp, f->filemode);
#endif
#ifndef SWAP
   if(f->dtmd == 'k')
   {  kl = strlen(e_k_key);
      for (j = ya; j < ye && j < b->mxlines; j++)
      {  if(b->bf[j].s != NULL)
	 for (; *(b->bf[j].s+i)!= '\0' && i <= b->bf[j].len
					     && i < b->mx.x ; i++)
	 {  if((c = *(b->bf[j].s+i)) > 32 && c < 127)
	    {  c += (e_k_key[kn++] - 32);
	       if(c == 127 || c < 0) c -= 94;
	       if(kn >= kl) kn = 0;
	    }
	    putc(c, fp);
	 }
	 i = 0;
	 e_pr_mtul("WRITING LINE", j, b->fb->mt.fb);
      }
      if(b->bf[j].s != NULL)
      for (i = 0; i < xe && i <= b->bf[j].len
			&& *(b->bf[j].s+i)!= '\0' && i < b->mx.x ; i++)
      {  if((c = *(b->bf[j].s+i)) > 32 && c < 127)
	 {  c += (e_k_key[kn++] - 32);
	    if(c == 127 || c < 0) c -= 94;
	    if(kn >= kl) kn = 0;
	 }
	 putc(c, fp);
      }
   }
   else if(f->dtmd == 'b')
   {  for (j = ya; j < ye && j < b->mxlines; j++)
      {  if(b->bf[j].s != NULL)
	 for (; i < b->bf[j].len; i++)
	 {  putc(*(b->bf[j].s+i), fp);
	 }
	 i = 0;
	 e_pr_mtul("WRITING LINE", j, b->fb->mt.fb);
      }
      if(b->bf[j].s != NULL)
      for (i = 0; i < xe && i < b->bf[j].len; i++)
      {  putc(*(b->bf[j].s+i), fp);
      }
   }
   else
   {  for (j = ya; j < ye && j < b->mxlines; j++)
      {  if(b->bf[j].s != NULL)
	 for (; i <= b->bf[j].len && i < b->mx.x
					&& *(b->bf[j].s+i)!= '\0' ; i++)
	 {  if(*(b->bf[j].s+i) == WR)
	    {
#if defined(DOS) || defined(DJGPP)
	       putc(CR, fp);
#endif
	       putc(WR, fp);
	       break;
	    }
	    else
	    putc(*(b->bf[j].s+i), fp);
	    e_pr_mtul("WRITING LINE", j, b->fb->mt.fb);
	 }
	 i = 0;
      }
      if(b->bf[j].s != NULL)
      for (i = 0; i < xe && *(b->bf[j].s+i)!= '\0' && i <= b->bf[j].len
					     && i < b->mx.x ; i++)
      {  if(*(b->bf[j].s+i) == WR)
	 {
#if defined(DOS) || defined(DJGPP)
	    putc(CR, fp);
#endif
	    putc(WR, fp);
	    break;
	 }
	 else
	 putc(*(b->bf[j].s+i), fp);
      }
   }
#else
   if(b->bf == NULL) e_swap_in_b(b);
   if(f->dtmd == 'k')
   {  kl = strlen(e_k_key);
      for (j = ya; j < ye && j < b->mxlines; j++)
      {  if(b->bf[j].s == NULL)
	 {  b->bf[j].s = tstr;
	    if(fseek(b->sp->fp, b->sp->pos[b->bf[j].sp], SEEK_SET))
	    e_error(e_msg[4], 1, b->fb);
	    if(fread(b->bf[j].s, b->bf[j].nrc, 1, b->sp->fp) != 1)
	    e_error(e_msg[4], 1, b->fb);
	 }
	 for (; *(b->bf[j].s+i)!= '\0' && i <= b->bf[j].len
					     && i < b->mx.x ; i++)
	 {  if((c = *(b->bf[j].s+i)) > 32 && c < 127)
	    {  c += (e_k_key[kn++] - 32);
	       if(c == 127 || c < 0) c -= 94;
	       if(kn >= kl) kn = 0;
	    }
	    putc(c, fp);
	 }
	 if(b->bf[j].s == tstr) b->bf[j].s = NULL;
	 i = 0;
	 e_pr_mtul("WRITING LINE", j, b->fb->mt.fb);
      }
      if(b->bf[j].s == NULL)
      {  b->bf[j].s = tstr;
	 if(fseek(b->sp->fp, b->sp->pos[b->bf[j].sp], SEEK_SET))
	 e_error(e_msg[4], 1, b->fb);
	 if(fread(b->bf[j].s, b->bf[j].nrc, 1, b->sp->fp) != 1)
	 e_error(e_msg[4], 1, b->fb);
      }
      for (i = 0; i < xe && i <= b->bf[j].len
			&& *(b->bf[j].s+i)!= '\0' && i < b->mx.x ; i++)
      {  if((c = *(b->bf[j].s+i)) > 32 && c < 127)
	 {  c += (e_k_key[kn++] - 32);
	    if(c == 127 || c < 0) c -= 94;
	    if(kn >= kl) kn = 0;
	 }
	 putc(c, fp);
      }
      if(b->bf[j].s == tstr) b->bf[j].s = NULL;
   }
   else if(f->dtmd == 'b')
   {  for (j = ya; j < ye && j < b->mxlines; j++)
      {  if(b->bf[j].s == NULL)
	 {  b->bf[j].s = tstr;
	    if(fseek(b->sp->fp, b->sp->pos[b->bf[j].sp], SEEK_SET))
	    e_error(e_msg[4], 1, b->fb);
	    if(fread(b->bf[j].s, b->bf[j].nrc, 1, b->sp->fp) != 1)
	    e_error(e_msg[4], 1, b->fb);
	 }
	 for (; i < b->bf[j].len; i++)
	 {  putc(*(b->bf[j].s+i), fp);
	 }
	 if(b->bf[j].s == tstr) b->bf[j].s = NULL;
	 i = 0;
	 e_pr_mtul("WRITING LINE", j, b->fb->mt.fb);
      }
      if(b->bf[j].s == NULL)
      {  b->bf[j].s = tstr;
	 if(fseek(b->sp->fp, b->sp->pos[b->bf[j].sp], SEEK_SET))
	 e_error(e_msg[4], 1, b->fb);
	 if(fread(b->bf[j].s, b->bf[j].nrc, 1, b->sp->fp) != 1)
	 e_error(e_msg[4], 1, b->fb);
      }
      for (i = 0; i < xe && i < b->bf[j].len; i++)
      {  putc(*(b->bf[j].s+i), fp);
      }
      if(b->bf[j].s == tstr) b->bf[j].s = NULL;
   }
   else
   {  for (j = ya; j < ye && j < b->mxlines; j++)
      {  if(b->bf[j].s == NULL)
	 {  b->bf[j].s = tstr;
	    if(fseek(b->sp->fp, b->sp->pos[b->bf[j].sp], SEEK_SET))
	    e_error(e_msg[4], 1, b->fb);
	    if(fread(b->bf[j].s, b->bf[j].nrc, 1, b->sp->fp) != 1)
	    e_error(e_msg[4], 1, b->fb);
	 }
	 for (; *(b->bf[j].s+i)!= '\0' && i <= b->bf[j].len
					     && i < b->mx.x ; i++)
	 {  if(*(b->bf[j].s+i) == WR)
	    {
#if defined(DOS) || defined(DJGPP)
	       putc(CR, fp);
#endif
	       putc(WR, fp);
	       break;
	    }
	    else
	    putc(*(b->bf[j].s+i), fp);
	    e_pr_mtul("WRITING LINE", j, b->fb->mt.fb);
	 }
	 if(b->bf[j].s == tstr) b->bf[j].s = NULL;
	 i = 0;
      }
      if(b->bf[j].s == NULL)
      {  b->bf[j].s = tstr;
	 if(fseek(b->sp->fp, b->sp->pos[b->bf[j].sp], SEEK_SET))
	 e_error(e_msg[4], 1, b->fb);
	 if(fread(b->bf[j].s, b->bf[j].nrc, 1, b->sp->fp) != 1)
	 e_error(e_msg[4], 1, b->fb);
      }
      for (i = 0; i < xe && *(b->bf[j].s+i)!= '\0' && i <= b->bf[j].len
					     && i < b->mx.x ; i++)
      {  if(*(b->bf[j].s+i) == WR)
	 {
#if defined(DOS) || defined(DJGPP)
	    putc(CR, fp);
#endif
	    putc(WR, fp);
	    break;
	 }
	 else
	 putc(*(b->bf[j].s+i), fp);
      }
      if(b->bf[j].s == tstr) b->bf[j].s = NULL;
   }
   FREE(tstr);
#endif
   FREE(ptmp);
   fclose(fp);
   e_pr_ul(b->fb);
   return(0);
}

/*   Neuen Qualifier an Dateinamen anhaengen   */

char *e_new_qual(s, ns, sb)
     char *s;
     char *ns;
     char *sb;
{
   int i, j;
   for(i = strlen(s); i >= 0 && s[i] != '.' && s[i] != DIRC; i--);
   if(i < 0 || s[i] == DIRC) strcpy(sb, s);
#if !defined(DJGPP) && !defined(DOS)
   else if(i == 0 || s[i-1] == DIRC) strcpy(sb, s);
#endif
   else
   {  for(j = 0; j < i; j++)  sb[j] = s[j];
      sb[j] = '\0';
   }
   strcat(sb, ".");
   strcat(sb, ns);
   return(sb);
}

/*    File/Directory-Struktur freigeben   */

int freedf(df)
     struct dirfile *df;
{
   if (df == NULL) return(-1);
   if(df->name)
   {  for(; df->anz > 0; (df->anz)--)
      if(*(df->name+df->anz-1)) FREE(*(df->name+df->anz-1));
      FREE(df->name);
   }
   FREE(df);
   df = NULL;
   return(0);
}
/*
	     Datei - Pfad erstellen    */

#ifndef VERSION14
char *e_mk_path(pth, cd, dd, n)
     char *pth;
     struct dirfile *cd;
     struct dirfile *dd;
     int n;
{
#ifdef UNIX
   int i = 0, k = 0, j = 0;
   if(!strcmp(cd->name[0], "Wastebasket"))
   {  e_ret_wastefile("", pth);
      strcat(pth, DIRS);
      i = strlen(pth);
      k++;
   }
#ifdef DJGPP
   else k = 1;
#endif
#else
   int i = 0, k = 1, j = 0;
#endif
   for (; k <= n && k < cd->anz; i++, j++)
   {  if( (*(pth+i) = *(*(cd->name+k)+j)) == '\0' || *(pth+i) == DIRC )
      {  *(pth+i) = DIRC;
	 k++;
	 j = -1;
      }
   }
   if(n >= k)
   for (j = 0; (*(pth+i) = *(*(dd->name+n-cd->anz)+j)) != '\0';
								  i++, j++);
#if defined(UNIX) && !defined(DJGPP)
   if(n == 0 || n >= k) i++;
#else
   if(n == 1 || n >= k) i++;
#endif
     *(pth+i-1) = '\0';
   return (pth);
}
#endif
#ifndef DOS
int e_file_window(sw, fw, ft, fz)
     int sw;
     FLWND *fw;
     int ft;
     int fz;
{
   int i, c, nsu = 0;
   int len = 0;
   char sustr[18];
   if(fw->df->anz > 0)
   {  if(fw->nf >= fw->df->anz) fw->nf = fw->df->anz-1;
      len = strlen(*(fw->df->name+fw->nf));
   }
   e_mouse_leiste(fw->xe, fw->ya, fw->ye-fw->ya, 0, fw->f->fb->em.fb);
   e_mouse_leiste(fw->xa, fw->ye, fw->xe-fw->xa, 1, fw->f->fb->em.fb);
   while (1)
   {  e_pr_file_window(fw, 1, 1, ft, fz, 0);
#if  MOUSE
      if((c = e_getch()) < 0) c = fl_wnd_mouse(sw, c, fw);
#else
      c = e_getch();
#endif
      if(fw->df->anz <= 0) return(ESC);
      if (c == CUP || c == CtrlP)
      {  if(fw->nf <= 0)  return(c);
	 else  fw->nf--;
	 if(fw->srcha < 0) nsu = 0;
      }
      else if ((c == CDO || c == CtrlN) && fw->nf < fw->df->anz-1)
      {  fw->nf++;
	 if(fw->srcha < 0) nsu = 0;
      }
      else if (c == CLE || c == CtrlB)
      {  if(fw->ja <= 0)  return(c);
	 else  fw->ja--;
      }
      else if (c == CRI || c == CtrlF)
      {  if(fw->ja > len-2)  return(c);
	 else  fw->ja++;
      }
      else if (c == BUP)
      {  if(fw->nf <= 0)  return(c);
	 if((fw->nf -= (fw->ye-fw->ya-1)) < 0) fw->nf = 0;
	 if(fw->srcha < 0) nsu = 0;
      }
      else if (c == BDO)
      {  if((fw->nf += (fw->ye-fw->ya-1)) > fw->df->anz-1)
	 fw->nf = fw->df->anz-1;
	 if(fw->srcha < 0) nsu = 0;
      }
      else if (c == POS1 || c == CtrlA || c == CBUP)
      {  fw->nf = 0; nsu = 0;  fw->ja = fw->srcha > 0 ? fw->srcha : 0;  }
      else if (c == ENDE || c == CtrlE || c == CBDO)
      fw->nf = fw->df->anz-1;
      else if (c >= 32 && c < 127)
      {  sustr[nsu] = c;  sustr[++nsu] = '\0';
	 if(fw->srcha < 0)
	 {  for(i = nsu > 1 ? fw->nf : fw->nf+1; i < fw->df->anz; i++)
	    {  for(c = 0; *(fw->df->name[i]+c)
			&& ( *(fw->df->name[i]+c) <= 32
			  || *(fw->df->name[i]+c) >= 127); c++);
#ifdef UNIX
	       if(!(e_we_sw & 1) && *(fw->df->name[i]+c-1) == 32) c += 3;
#endif
	       if(strncmp(fw->df->name[i]+c, sustr, nsu) >= 0) break;
	    }
	 }
	 else
	 {  for(i = 0; i < fw->df->anz
		  && strncmp(fw->df->name[i]+fw->srcha, sustr, nsu) < 0; i++);
	 }
	 fw->nf = i < fw->df->anz ? i : fw->df->anz-1;
      }
      else if (c == CCLE) return(c);
      else if (c == CCRI) return(c);
      else if(c) {  nsu = 0;  return(c);  }
      else if(fw->srcha < 0) nsu = 0;
      if(fw->nf > fw->df->anz-1) fw->nf = fw->df->anz-1;
      len=strlen(*(fw->df->name+fw->nf));
      if(fw->ja >= len) fw->ja = !len ? len : len-1;
      if(fw->nf-fw->ia >= fw->ye - fw->ya) fw->ia = fw->nf+fw->ya-fw->ye+1;
      else if(fw->nf-fw->ia < 0) fw->ia = fw->nf;
   }
   return(0);
}

int e_pr_file_window(fw, c, sw, ft, fz, fs)
     FLWND *fw;
     int c;
     int sw;
     int ft;
     int fz;
     int fs;
{
#ifdef NEWSTYLE
   int xrt = 0;
#endif
   int i = fw->ia, len;
   if(fw->df != NULL)
   {  for(; i < fw->df->anz && i-fw->ia < fw->ye-fw->ya; i++)
      {  if((len=strlen(*(fw->df->name+i))) < 0) len = 0;
	 if(i == fw->nf && c && len >= fw->ja)
	 {  e_pr_nstr(fw->xa+1, fw->ya+i-fw->ia, fw->xe-fw->xa,
		*(fw->df->name+i)+fw->ja, fz, fz);
#ifdef NEWSTYLE
	    xrt = 1;
#endif
	 }
	 else if(i == fw->nf && len >= fw->ja)
	 e_pr_nstr(fw->xa+1, fw->ya+i-fw->ia, fw->xe-fw->xa,
		*(fw->df->name+i)+fw->ja, fs, fs);
	 else if(len >= fw->ja)
	 e_pr_nstr(fw->xa+1, fw->ya+i-fw->ia, fw->xe-fw->xa,
		*(fw->df->name+i)+fw->ja, ft, ft);
	 else if(len < fw->ja)
	 e_blk(fw->xe-fw->xa-1, fw->xa+1, fw->ya+i-fw->ia, ft);
/*
      if((len -= fw->ja) < 0) len = 0;
      e_blk(fw->xe-fw->xa-len-1, fw->xa+len+1, fw->ya+i-fw->ia, 
						ft);
*/
	 if(sw)
	 {  fw->nyfo = e_lst_zeichen(fw->xe, fw->ya, fw->ye-fw->ya, 0,
			fw->f->fb->em.fb, fw->df->anz, fw->nyfo, fw->nf);
	    fw->nxfo = e_lst_zeichen(fw->xa, fw->ye, fw->xe-fw->xa, 1,
			fw->f->fb->em.fb, len, fw->nxfo, fw->ja);
	 }
      }
   }
   for(; i-fw->ia < fw->ye-fw->ya; i++)
   e_blk(fw->xe-fw->xa, fw->xa, fw->ya+i-fw->ia, ft);
#ifdef NEWSTYLE
   e_make_xrect(fw->xa, fw->ya, fw->xe-1, fw->ye-1, 1);
   if(xrt) e_make_xrect_abs(fw->xa, fw->ya+fw->nf-fw->ia,
				fw->xe-1, fw->ya+fw->nf-fw->ia, 0);
#endif
   return(0);
}

struct dirfile *e_ext_dirfile(df, sw)
     struct dirfile *df;
     int sw;
{
   struct dirfile *edf = MALLOC(sizeof(struct dirfile));
   char **name, **ename, *stmp, str[256];
   int ntmp, n, i, *num;
   if(!edf) return(NULL);
   if(!(edf->name = MALLOC(df->anz * sizeof(char *))))
   {  FREE(edf); return(NULL);  }
   if(!(num = MALLOC(df->anz * sizeof(int))))
   {  FREE(edf); FREE(num); return(NULL);  }
   edf->anz = df->anz;
   for(i = 0; i < df->anz; i++)
   {  e_file_info(*(df->name+i), str, num+i, sw);
      if(!(*(edf->name+i) = MALLOC((strlen(str)+1)*sizeof(char)))) break;
      strcpy(*(edf->name+i), str);
   }
   if(sw & 3)
   {  ename = MALLOC(df->anz * sizeof(char *));
      name = MALLOC(df->anz * sizeof(char *));
      for (i = 0; i < df->anz; i++)
      {  for (ntmp = num[i], n = i; n > 0 && ntmp > num[n-1]; n--)
	 {  *(ename + n) = *(ename + n - 1);
	    *(name + n) = *(name + n - 1);
	    num[n] = num[n-1];
	 }
	 *(ename + n) = *(edf->name + i);
	 *(name + n) = *(df->name + i);
	 num[n] = ntmp;
      }
      FREE(edf->name);
      FREE(df->name);
      edf->name = ename;
      df->name = name;
   }
   FREE(num);
   if(i != edf->anz) {  edf->anz = i;  freedf(edf);  return(NULL);  }
   if(sw & 4)
   {  for (i = 0; i < (df->anz)/2; i++)
      {  stmp = edf->name[i];
	 edf->name[i] = edf->name[edf->anz-i-1];
	 edf->name[edf->anz-i-1] = stmp;
	 stmp = df->name[i];
	 df->name[i] = df->name[df->anz-i-1];
	 df->name[df->anz-i-1] = stmp;
      }
   }
   return(edf);
}

struct dirfile *e_ext_dirdir(cd, dd)
     struct dirfile *cd;
     struct dirfile *dd;
{
   extern char *ctree[5];
   struct dirfile *edf = MALLOC(sizeof(struct dirfile));
   char str[256];
   int i, j;
   if(!edf) return(NULL);
   if(!(edf->name = MALLOC((cd->anz+dd->anz) * sizeof(char *))))
   {  FREE(edf); return(NULL);  }
   for(i = 0; i < cd->anz; i++)
   {
      if(!i)
#if !defined(DJGPP) && !defined(VERSION14)
      {  if(*cd->name[0] == DIRC && *(cd->name[0]+1) == '\0')
	 strcpy(str, "Root");
	 else strcpy(str, *(cd->name+i));
#else
      {  strcpy(str, *(cd->name+i));
#endif
      }
      else
      {  for(str[0] = '\0', j = 0; j < i-1; j++) strcat(str, "  ");
	 if(i == cd->anz-1 && dd->anz < 1) strcat(str, ctree[1]);
	 else if(i == cd->anz-1) strcat(str, ctree[2]);
	 else strcat(str, ctree[0]);
	 strcat(str, *(cd->name+i));
      }
      if(!(*(edf->name+i) = MALLOC((strlen(str)+1)*sizeof(char)))) break;
      strcpy(*(edf->name+i), str);
   }
   if(i != cd->anz) {  edf->anz = i;  freedf(edf);  return(NULL);  }
   for(; i < cd->anz + dd->anz; i++)
   {  for(str[0] = '\0', j = 0; j < cd->anz-2; j++) strcat(str, "  ");
      strcat(str, " ");
      if(i == cd->anz+dd->anz-1) strcat(str, ctree[4]);
      else strcat(str, ctree[3]);
      strcat(str, *(dd->name+i-cd->anz));
      if(!(*(edf->name+i) = MALLOC((strlen(str)+1)*sizeof(char)))) break;
      strcpy(*(edf->name+i), str);
   }
   if(i != (cd->anz+dd->anz)) {  edf->anz = i;  freedf(edf);  return(NULL);  }
   edf->anz = i;
   return(edf);
}
#endif

struct help_ud {  struct help_ud *next;  char *str, *nstr, *pstr, *file;
        	  int x, y, sw;
               } *ud_help;

typedef struct {  FILE *fp;  int sw;  } IFILE;

#define e_i_fgets(s, n, p) fgets(s, n, p->fp)

int e_mkdir_path(path)
     char *path;
{
   int i, len;
   char *tmp = MALLOC(((len=strlen(path))+1)*sizeof(char));
   if(!tmp) return(-1);
   strcpy(tmp, path);
   for(i = len; i > 0 && tmp[i] != DIRC; i--);
   if(i > 0)
   {  tmp[i] = '\0';
      if(access(tmp, 0))
      {  e_mkdir_path(tmp);
         mkdir(tmp, 0700);
      }
   }
   FREE(tmp);
   return(0);
}

IFILE *e_i_fopen(path, stat)
     char *path;
     char *stat;
{
   extern char *e_tmp_dir;
   char *tmp, *tmp2, *command;
   int len;
   IFILE *fp = MALLOC(sizeof(IFILE));
   if(!fp) return(NULL);
   if(!path) {  FREE(fp); return(NULL);  }
   if((fp->fp = fopen(path, stat))) {  fp->sw = 0;  return(fp);  }
   if(!(tmp2 = MALLOC((strlen(path)+11)*sizeof(char)))) 
   {  FREE(fp);  return(NULL);  }
   strcpy(tmp2, path);
   strcat(tmp2, ".info");
   if((fp->fp = fopen(tmp2, stat))) 
   {  FREE(tmp2);  fp->sw = 0;  return(fp);  }
   strcpy(tmp2, path);
   strcat(tmp2, ".gz");
   if(access(tmp2, 0))
   {  strcpy(tmp2, path);
      strcat(tmp2, ".Z");
      if(access(tmp2, 0))
      {  strcpy(tmp2, path);
	 strcat(tmp2, ".info.gz");
	 if(access(tmp2, 0))
	 {  strcpy(tmp2, path);
	    strcat(tmp2, ".info.Z");
	    if(access(tmp2, 0))
	    {  FREE(fp); FREE(tmp2); return(NULL);  }
	 }
      }
   }
   if(!(tmp = MALLOC((strlen(path)+(len=strlen(e_tmp_dir))+2)*sizeof(char)))) 
   {  FREE(fp);  FREE(tmp2);  return(NULL);  }
   strcpy(tmp, e_tmp_dir);
   if(path[0] != DIRC) {  tmp[len] = DIRC;  tmp[len+1] = '\0';  }
   strcat(tmp, path);
   if((fp->fp = fopen(tmp, stat)))
   {  fp->sw = 1;  FREE(tmp);  FREE(tmp2);  return(fp);  }
   command = MALLOC((strlen(tmp) + strlen(tmp2) + 14) * sizeof(char));
   if(!command) {  FREE(fp);  FREE(tmp);  FREE(tmp2);  return(NULL);  }
   e_mkdir_path(tmp);
   sprintf(command, "gunzip < %s > %s", tmp2, tmp);
   FREE(tmp2);
   system(command);
   FREE(command);  
   fp->fp = fopen(tmp, stat);
   FREE(tmp);
   if(!fp->fp) {  FREE(fp);  return(NULL);  }
   fp->sw = 1;
   return(fp);  
}

int e_i_fclose(fp)
     IFILE *fp;
{
   int ret = fclose(fp->fp);
   FREE(fp);
   return(ret);
}

int e_read_help(str, f, sw)
     char *str;
     FENSTER *f;
     int sw;
{
   IFILE *fp;
   char *ptmp, tstr[256];
   int i;
   ptmp = e_mkfilename(f->ed->libdrct, WE_HELP_ED);
   fp = e_i_fopen(ptmp, "rb");
   FREE(ptmp);
   if(!fp) return(1);
   e_close_buffer(f->b);
   if( (f->b = (BUFFER *) MALLOC(sizeof(BUFFER))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   if( (f->b->bf = (STRING *) MALLOC(MAXLINES*sizeof(STRING))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   f->b->f = f;
   f->b->b = e_s_pkt(0, 0);
   f->b->mx = e_s_pkt(f->ed->maxcol, MAXLINES);
   f->b->mxlines = 0;
   f->b->fb = f->fb;
   f->b->cn = f->ed;
#ifdef SWAP
   f->b->sp = f->ed->sp;
   f->b->pos = f->ed->curedt;
#endif
#ifdef WEUNDO
   f->b->ud = NULL;
#endif
   e_new_line(0, f->b);
   if(str && str[0])
   {  while((ptmp = e_i_fgets(tstr, 256, fp)) && !e_strcstr(tstr, str));
      if(ptmp && !sw)
      {  strcpy(f->b->bf[f->b->mxlines-1].s, tstr);
	 f->b->bf[f->b->mxlines-1].len = e_str_len(f->b->bf[f->b->mxlines-1].s);
	 f->b->bf[f->b->mxlines-1].nrc = e_str_nrc(f->b->bf[f->b->mxlines-1].s);
      }
   }
   if(sw)
   {  char *tp = NULL, tmp[128], ts[2];
      ts[0] = HHD; ts[1] = '\0';
      while((ptmp = e_i_fgets(tstr, 256, fp)) && !(tp = strstr(tstr, ts)));
      if(!str || !str[0])
         while((ptmp = e_i_fgets(tstr, 256, fp)) && !(tp = strstr(tstr, ts)));
      if(!ptmp)
      {  e_i_fclose(fp);
	 return(e_error("No Next Page", 0, f->fb));
      }
      else
      {  for(i = 0; (tmp[i] = tp[i]) && tp[i] != HED; i++);
	 tmp[i] = HED;  tmp[i+1] = '\0';
	 if((ud_help->str = MALLOC((strlen(tmp)+1)*sizeof(char))) != NULL)
	    strcpy(ud_help->str, tmp);
         strcpy(f->b->bf[f->b->mxlines-1].s, tstr);
	 f->b->bf[f->b->mxlines-1].len = e_str_len(f->b->bf[f->b->mxlines-1].s);
	 f->b->bf[f->b->mxlines-1].nrc = e_str_nrc(f->b->bf[f->b->mxlines-1].s);
      }
   }
   while(e_i_fgets(tstr, 256, fp))
   {  for(i = 0; tstr[i]; i++)
      if(tstr[i] == HFE)  {  e_i_fclose(fp);  return(0);  }
      e_new_line(f->b->mxlines, f->b);
      strcpy(f->b->bf[f->b->mxlines-1].s, tstr);
      f->b->bf[f->b->mxlines-1].len = e_str_len(f->b->bf[f->b->mxlines-1].s);
      f->b->bf[f->b->mxlines-1].nrc = e_str_nrc(f->b->bf[f->b->mxlines-1].s);
   }
   e_i_fclose(fp);
   return(2);
}

int e_help_ret(f)
     FENSTER *f;
{
   BUFFER *b = f->b;
   int i, j;
   char str[126];
   struct help_ud *next;
   for(i = b->b.x; i >= 0 && b->bf[b->b.y].s[i] != HED; i--)
   {  if(b->bf[b->b.y].s[i] == HBG)
      {  str[0] = HHD;
	 for(j = i+1; j < b->bf[b->b.y].len
			&& (str[j-i] = b->bf[b->b.y].s[j]) != HED; j++);
	 str[j-i+1] = '\0';
	 if((next = MALLOC(sizeof(struct help_ud))) != NULL)
	 {  next->str = MALLOC((strlen(str)+1) * sizeof(char));
	    if(next->str) strcpy(next->str, str);
            if(ud_help && ud_help->file)
            {  next->file = MALLOC((strlen(ud_help->file)+1) * sizeof(char));
               if(next->file) strcpy(next->file, ud_help->file);
            }
            else next->file = NULL;
            next->nstr = next->pstr = NULL;
            next->x = b->b.x;
            next->y = b->b.y;
            next->sw = ud_help ? ud_help->sw : 0;
	    next->next = ud_help;
	    ud_help = next;
	 }
         if(ud_help->sw) e_read_info(str, f, ud_help->file);
	 else e_read_help(str, f, 0);
	 b = f->b; b->b.x = b->b.y = 0;
	 e_cursor(f, 1);
	 e_schirm(f, 1);
	 return(0);
      }
      else if(b->bf[b->b.y].s[i] == HNF)
      {  for(i++; i < b->bf[b->b.y].len && b->bf[b->b.y].s[i] != HED; i++);
	 for(i++, j = 0; j+i < b->bf[b->b.y].len
			&& (str[j] = b->bf[b->b.y].s[j+i]) != HED; j++);
	 str[j] = '\0';
	 if((next = MALLOC(sizeof(struct help_ud))) != NULL)
	 {  next->str = MALLOC(4 * sizeof(char));
	    if(next->str) strcpy(next->str, "Top");
            next->file = MALLOC((strlen(str)+1) * sizeof(char));
            if(next->file) strcpy(next->file, str);
            next->sw = 1;
	    next->next = ud_help;
            next->x = b->b.x;
            next->y = b->b.y;
            next->nstr = next->pstr = NULL;
	    ud_help = next;
	 }
         e_read_info("Top", f, ud_help->file);
	 b = f->b; b->b.x = b->b.y = 0;
	 e_cursor(f, 1);
	 e_schirm(f, 1);
	 return(0);
      }
      else if(b->bf[b->b.y].s[i] == HFB)
      {  for(j = i+1; j < b->bf[b->b.y].len
			&& (str[j-i-1] = b->bf[b->b.y].s[j]) != HED; j++);
	 str[j-i-1] = '\0';
	 return(e_ed_man(str, f));
      }
      else if(b->bf[b->b.y].s[i] == HHD) return(e_help_last(f));
   }
   return(1);
}

int e_help_last(f)
     FENSTER *f;
{
   struct help_ud *last = ud_help;
   if(!last) return(1);
   if(last->sw)
   {  if(last->next == NULL) e_read_info(NULL, f, NULL);
      else e_read_info(last->next->str, f, last->next->file);
   }
   else
   {  if(last->next == NULL) e_read_help(NULL, f, 0);
      else e_read_help(last->next->str, f, 0);
   }
   f->b->b.x = last->x;  
   f->b->b.y = last->y;
   e_cursor(f, 1);
   e_schirm(f, 1);
   ud_help = last->next;
   FREE(last->str);
   FREE(last);
   return(0);
}

int e_help_next(f, sw)
     FENSTER *f;
     int sw;
{
   struct help_ud *last = ud_help;
   if(last && last->sw)
   {  if(sw && last->nstr)
      {  if(last->str) FREE(last->str);
         if(last->pstr) FREE(last->pstr);
         last->str = last->nstr;
         last->nstr = NULL;
      }
      else if(!sw && last->pstr)
      {  if(last->str) FREE(last->str);
         if(last->nstr) FREE(last->nstr);
         last->str = last->pstr;
         last->pstr = NULL;
      }
      else
         return(e_error(sw ? "No Next Page" : "No Previous Page", 0, f->fb));
      e_read_info(last->str, f, last->file);
      f->b->b.x = f->b->b.y = 0;
      e_cursor(f, 1);
      e_schirm(f, 1);
      return(0);
   }
   else
   {  if(sw)
      {  if((last = MALLOC(sizeof(struct help_ud))) != NULL)
	 {  last->str = NULL;
            last->file = NULL;
	    last->x = last->y = 0;
	    last->nstr = last->pstr = NULL;
	    last->sw = 0;
	    last->next = ud_help;
	    ud_help = last;
	    e_read_help(ud_help->next ? ud_help->next->str : NULL, f, 1);
            f->b->b.x = f->b->b.y = 0;
            e_cursor(f, 1);
            e_schirm(f, 1);
            return(0);
	 }
      }
      else return(e_help_last(f));
   }
   return(e_error(sw ? "No Next Page" : "No Previous Page", 0, f->fb));
}

int e_help_free(f)
     FENSTER *f;
{
   struct help_ud *next, *last = ud_help;
   while(last)
   {  next = last->next;
      if(last->str) FREE(last->str);
      if(last->pstr) FREE(last->pstr);
      if(last->nstr) FREE(last->nstr);
      if(last->file) FREE(last->file);
      FREE(last);
      last = next;
   }
   ud_help = NULL;
   return(0);
}

int e_help_comp(f)
     FENSTER *f;
{
   BUFFER *b = f->b;
   int i, j, k, hn = 0, sn = 0;
   char **swtch = MALLOC(sizeof(char *));
   char **hdrs = MALLOC(sizeof(char *));
   char str[256];
   if(f->dtmd != 'h') return(1);
   for(j = 0; j < b->mxlines; j++)
   {  for(i = 0; i < b->bf[j].len; i++)
      {  if(b->bf[j].s[i] == HBG)
	 {  for(k = i+1; k < b->bf[j].len
		  && (str[k-i-1] = b->bf[j].s[k]) != HED
		  && str[k-i-1] != HBG && str[k-i-1] != HHD
		  && str[k-i-1] != HFE ; k++);
	    if(str[k-i-1] != HED)
	    {  e_error("Error in Switch!", 0, f->fb);
	       b->b.x = k;  b->b.y = j;
	       e_cursor(f, 1);
	       return(2);
	    }
	    str[k-i-1] = '\0';
	    sn++;
	    swtch = REALLOC(swtch, sn * sizeof(char *));
	    swtch[sn-1] = MALLOC((strlen(str) + 1) + sizeof(char));
	    strcpy(swtch[sn-1], str);
	 }
      }
   }
   for(j = 0; j < b->mxlines; j++)
   {  for(i = 0; i < b->bf[j].len; i++)
      {  if(b->bf[j].s[i] == HHD)
	 {  for(k = i+1; k < b->bf[j].len
		  && (str[k-i-1] = b->bf[j].s[k]) != HED
		  && str[k-i-1] != HBG && str[k-i-1] != HHD
		  && str[k-i-1] != HFE ; k++);
	    if(str[k-i-1] != HED)
	    {  e_error("Error in Header!", 0, f->fb);
	       b->b.x = k;  b->b.y = j;
	       e_cursor(f, 1);
	       return(2);
	    }
	    str[k-i-1] = '\0';
	    hn++;
	    hdrs = REALLOC(hdrs, hn * sizeof(char *));
	    hdrs[hn-1] = MALLOC((strlen(str) + 1) + sizeof(char));
	    strcpy(hdrs[hn-1], str);
	 }
      }
   }
   for(j = 0; j < sn; j++)
   {  for(i = 0; i < hn; i++) if(!e_strccmp(swtch[j], hdrs[i])) break;
      if(i >= hn)
      {  sprintf(str, "Switch \'%s\' has no Header!", swtch[j]);
	 e_error(str, 0, f->fb);
	 goto ende;
      }
   }
   for(j = 0; j < hn; j++)
   {  for(i = 0; i < sn; i++) if(!e_strccmp(swtch[i], hdrs[j])) break;
      if(i >= sn)
      {  sprintf(str, "No Jump to Header \'%s\'!", hdrs[j]);
	 e_error(str, 0, f->fb);
	 goto ende;
      }
   }
   ende:
    for(i = 0; i < sn; i++) FREE(swtch[i]);
   for(i = 0; i < hn; i++) FREE(hdrs[i]);
   FREE(swtch);
   FREE(hdrs);
   return(0);
}

/*
          Hilfe-Fenster    */

int e_help(f)
     FENSTER *f;
{
   extern char *e_hlp;
   e_hlp = NULL;
   return(e_help_loc(f, 0));
}

int e_info(f)
     FENSTER *f;
{
   extern char *e_hlp;
   e_hlp = NULL;
   return(e_help_loc(f, 1));
}

#define IFE 31

int e_mk_info_button(str)
     char *str;
{
   int i, bg, nd, len = strlen(str);
   if(str[0] == '\n' || str[0] == '\0') return(1);
   for(bg = 0; str[bg] && isspace(str[bg]); bg++);
   for(nd = bg; str[nd] && (str[nd] != ':' || 
      (!isspace(str[nd+1]) && (str[nd+1] != ':' || 
       (!isspace(str[nd+2]) && str[nd+2] != '.')))); nd++);
   if(!str[nd]) return(-1);
   if(str[nd+1] != ':')
   {  for(nd++; str[nd] && isspace(str[nd]); nd++);
      if(str[nd] == '(')
      {  for(i = len; i >= bg; i--) str[i+1] = str[i];
         str[bg] = HNF;
         str[nd+1] = HED;
         for(nd++; str[nd] && str[nd] && str[nd] != ')'; nd++);
         if(str[nd]) str[nd] = HED;
         return(0);
      }
      for(; str[nd] && (str[nd] != '.' || isalnum1(str[nd+1])); nd++);
      if(!str[nd]) return(-1);
      for(bg = nd; str[bg] != ':' || !isspace(str[bg+1]); bg--);
      for(bg++; isspace(str[bg]); bg++);
   }
   for(i = len; i >= nd; i--) str[i+2] = str[i];
   str[nd+1] = HED;
   for(; i >= bg; i--) str[i+1] = str[i];
   str[bg] = HBG;
   return(0);
}

int e_mk_info_mrk(str)
     char *str;
{
   int bg, nd = -1;
   do
   {  for(bg = nd+1; str[bg] && str[bg] != '`'; bg++);
      for(nd = bg; str[nd] && str[nd] != '\''; nd++);
      if(str[nd]) {  str[bg] = HBB;  str[nd] = HED;  }
   } while(str[nd]);
   return(0);
}

IFILE *e_info_jump(str, path, fp)
     char *str;
     char **path;
     IFILE *fp;
{
   IFILE *fpn;
   int i, j, n, anz = 0;
   char *ptmp, *fstr, tstr[256], nfl[128];
   struct FL_INFO{  char *name; int line;  } **files = MALLOC(1);
   while(e_i_fgets(tstr, 256, fp) && tstr[0] != IFE)
   {  for(i = 0; (nfl[i] = tstr[i]) && tstr[i] != ':'; i++);
      if(!tstr[i]) continue;
      nfl[i] = '\0';
      anz++;
      files = REALLOC(files, anz * sizeof(struct FL_INFO *));
      files[anz-1] = MALLOC(sizeof(struct FL_INFO));
      files[anz-1]->name = MALLOC((strlen(nfl)+1)*sizeof(char));
      strcpy(files[anz-1]->name, nfl);
      files[anz-1]->line = atoi(tstr+i+1);
   }
   i = (strlen(str) + 6);
   fstr = MALLOC((i+1) * sizeof(char));
   strcat(strcpy(fstr, "Node: "), str);
   if(fstr[n = strlen(fstr)-1] == HED) {  fstr[n] = '\0';  i--;  }
   while((ptmp = e_i_fgets(tstr, 256, fp)) && strncmp(tstr, fstr, i));
   if(ptmp)
   {  n = atoi(tstr+i+1);
      for(i = 1; i < anz && n >= files[i]->line; i++);
#ifdef DJGPP
      if(files[i-1]->name[0] == DIRC || files[i-1]->name[0] == '/')
#else
      if(files[i-1]->name[0] == DIRC)
#endif
      {  ptmp = MALLOC((strlen(files[i-1]->name)+1)*sizeof(char));
         strcpy(ptmp, files[i-1]->name);
      }
      else
#ifdef DJGPP
      {  for(n = strlen(*path)-1; n >= 0 && *(*path+n) != DIRC 
            && *(*path+n) != '/'; n--);
#else
      {  for(n = strlen(*path)-1; n >= 0 && *(*path+n) != DIRC; n--);
#endif
         n++;
         ptmp = MALLOC((strlen(files[i-1]->name)+n+1)*sizeof(char));
         for(j = 0; j < n; j++) ptmp[j] = *(*path+j);
         ptmp[j] = '\0';
         strcat(ptmp, files[i-1]->name);
      }
      if((fpn = e_i_fopen(ptmp, "rb")) != NULL)
      {  e_i_fclose(fp);
         fp = fpn;
         FREE(*path);
         *path = ptmp;
      }
      else FREE(ptmp);
   }
   FREE(fstr);
   for(i = 0; i < anz; i++)
   {  FREE(files[i]->name);
      FREE(files[i]);
   }
   FREE(files);
   return(fp);
}

char *e_mk_info_pt(str, node)
     char *str;
     char *node;
{
   int i;
   char *ptmp, tmp[128];
   do
   {  if(!(ptmp = strstr(str, node))) return(NULL);
      while(*ptmp && *ptmp != ',' && *ptmp != ':') ptmp++;
      if(!*ptmp) return(NULL);
   } while (*ptmp == ',');
   for(ptmp++; isspace(*ptmp); ptmp++);
   for(i = 0; (tmp[i] = ptmp[i]) && ptmp[i] != ',' && ptmp[i] != '\n'; i++);
   tmp[i] = '\0';
   if(i == 0) return(NULL);
   ptmp = MALLOC((strlen(tmp)+1)*sizeof(char));
   strcpy(ptmp, tmp);
   return(ptmp);
}

char *e_mk_info_path(path, file)
     char *path;
     char *file;
{
   int n = 0;
   char *tp;
   if(!info_file) return(NULL);
#ifdef DJGPP
   if(file[0] == DIRC || file[0] == '/')
#else
   if(file[0] == DIRC)
#endif
   {  if(path && !strcmp(path, file)) {  FREE(path);  return(NULL);  }
      else if(path) FREE(path);
      if(!(path = MALLOC((strlen(file) + 1) * sizeof(char)))) return(NULL);;
      return(strcpy(path, file));
   }
   if(path)
   {  n = strlen(path) - strlen(file) - 1;
      if(n > 0) path[n] = '\0';
      else if(n == 0) path[n+1] = '\0';
      n = strlen(path);
      for(tp = info_file; (tp = strstr(tp, path)) != NULL
         	&& tp[n] && tp[n] != PTHD; tp++);
      FREE(path);
      if(!tp) return(NULL);
      for(n = 0; info_file+n <= tp || (tp[n] && tp[n] != PTHD); n++);
      if(!tp[n]) return(NULL);
      n++;
   }
   tp = info_file + n;
   for(n = 0; tp[n] && tp[n] != PTHD; n++);
   if(!(path = MALLOC((strlen(file) + n + 2) * sizeof(char)))) return(NULL);
   for(n = 0; (path[n] = tp[n]) && tp[n] != PTHD; n++);
   if(n > 1)
   {  path[n] = DIRC;  n++;  }
   path[n] = '\0';
   return(strcat(path, file));
}

int e_read_info(str, f, file)
     char *str;
     FENSTER *f;
     char *file;
{
   IFILE *fp = NULL;
   char *path = NULL, *ptmp, tstr[256], fstr[128];
   int i, len, sw = 0, bsw = 0;
   if(!str) str = "Top";
   if(str[0] == HHD) str++;
   strcat(strcpy(fstr, "Node: "), str);
   len = strlen(fstr);
   if(fstr[len-1] == HED) fstr[len-1] = '\0';
   if(!file || !file[0]) file = "dir";
   do
   {  path = e_mk_info_path(path, file);
      fp = e_i_fopen(path, "rb");
   } while(!fp && path);
   if(!fp) return(1);
   e_close_buffer(f->b);
   if( (f->b = (BUFFER *) MALLOC(sizeof(BUFFER))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   if( (f->b->bf = (STRING *) MALLOC(MAXLINES*sizeof(STRING))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   f->b->f = f;
   f->b->b = e_s_pkt(0, 0);
   f->b->mx = e_s_pkt(f->ed->maxcol, MAXLINES);
   f->b->mxlines = 0;
   f->b->fb = f->fb;
   f->b->cn = f->ed;
#ifdef SWAP
   f->b->sp = f->ed->sp;
   f->b->pos = f->ed->curedt;
#endif
#ifdef WEUNDO
   f->b->ud = NULL;
#endif
   e_new_line(0, f->b);
   while((ptmp = e_i_fgets(tstr, 256, fp)) != NULL)
   {  if(!strncmp(tstr, "Indirect:", 9)) fp = e_info_jump(str, &path, fp);
      else if(!strncmp(tstr, "File:", 5) && strstr(tstr, fstr)) break;
   }
   FREE(path);
   if(ptmp)
   {  ud_help->nstr = e_mk_info_pt(tstr, "Next");
      ud_help->pstr = e_mk_info_pt(tstr, "Prev");
   }
   if(!strcmp(file, "dir"))
      while((ptmp = e_i_fgets(tstr, 256, fp)) 
         && e_strcncmp(tstr, "* Menu:", 7));
   else while((ptmp = e_i_fgets(tstr, 256, fp)) && tstr[0] == '\n');
   if(ptmp)
   {  if(!sw && !e_strcncmp(tstr, "* Menu:", 7)) sw = 1;
      for(i = len = strlen(tstr); i >= 0; i--) tstr[i+1] = tstr[i];
      tstr[0] = HHD;  tstr[len+1] = HED;  tstr[len+1] = '\0';
      strcpy(f->b->bf[f->b->mxlines-1].s, tstr);
      f->b->bf[f->b->mxlines-1].len = e_str_len(f->b->bf[f->b->mxlines-1].s);
      f->b->bf[f->b->mxlines-1].nrc = e_str_nrc(f->b->bf[f->b->mxlines-1].s);
   }
   while(e_i_fgets(tstr, 256, fp))
   {  for(i = 0; tstr[i]; i++)
      if(tstr[i] == IFE)  {  e_i_fclose(fp);  return(0);  }
      if(bsw == 1) 
      {  bsw = e_mk_info_button(tstr);
         for(ptmp = tstr; (ptmp = e_strcstr(ptmp, "*note")); ptmp += 5)
            bsw = e_mk_info_button(ptmp+5);
      }
      else if(!sw && !e_strcncmp(tstr, "* Menu:", 7)) sw = 1;
      else if((ptmp = e_strcstr(tstr, "*note")))
      {  bsw = e_mk_info_button(ptmp+5);
         for(ptmp += 5; (ptmp = e_strcstr(ptmp, "*note")); ptmp += 5)
            bsw = e_mk_info_button(ptmp+5);
      }
      else if(sw && tstr[0] == '*') 
      {  bsw = e_mk_info_button(tstr+1);
         for(ptmp = tstr+1; (ptmp = e_strcstr(ptmp, "*note")); ptmp += 5)
            bsw = e_mk_info_button(ptmp+5);
      }
      e_mk_info_mrk(tstr);
      e_new_line(f->b->mxlines, f->b);
      strcpy(f->b->bf[f->b->mxlines-1].s, tstr);
      f->b->bf[f->b->mxlines-1].len = e_str_len(f->b->bf[f->b->mxlines-1].s);
      f->b->bf[f->b->mxlines-1].nrc = e_str_nrc(f->b->bf[f->b->mxlines-1].s);
   }
   e_i_fclose(fp);
   return(2);
}

int e_help_loc(f, sw)
     FENSTER *f;
     int sw;
{
   extern char *e_hlp;
   int i;
   char *tmp = NULL;
   struct help_ud *next;
   if(!sw) tmp = e_hlp;
   for(i = f->ed->mxedt; i >= 0; i--)
   {  if(!strcmp(f->ed->f[i]->datnam, "Help"))
      {  e_switch_window(f->ed->edt[i], f);
         if(ud_help && sw != ud_help->sw)
         {  e_close_window(f->ed->f[f->ed->mxedt]);
            i = -1;
         }
         break;
      }
   }
   if(i < 0) e_edit(f->ed, "Help");
   if((tmp || sw) && (next = MALLOC(sizeof(struct help_ud))))
   {  if(tmp)
      {  next->str = MALLOC((strlen(tmp)+1) * sizeof(char));
         if(next->str) strcpy(next->str, tmp);
      }
      else next->str = NULL;
      next->file = NULL;
      next->x = next->y = 0;
      next->nstr = next->pstr = NULL;
      next->sw = sw;
      next->next = ud_help;
      ud_help = next;
   }
   if(sw) e_read_info(NULL, f->ed->f[f->ed->mxedt], NULL);
   else e_read_help(tmp, f->ed->f[f->ed->mxedt], 0);
   e_schirm(f->ed->f[f->ed->mxedt], 1);
   return(0);
}

int e_help_options(f)
     FENSTER *f;
{
   char str[128];
   if(!info_file)
   {  info_file = MALLOC(1);  info_file[0] = '\0';  }
   strcpy(str, info_file);
   if(e_add_arguments(str, "Info-Path", f, 0 , AltI, NULL))
   {  info_file = REALLOC(info_file, strlen(str) + 1);
      strcpy(info_file, str);
   }
   return(0);
}

int e_hp_next(f)
     FENSTER *f;
{
   int i;
   for(i = f->ed->mxedt; i >= 0; i--)
   {  if(!strcmp(f->ed->f[i]->datnam, "Help"))
      {  e_switch_window(f->ed->edt[i], f);
         f = f->ed->f[f->ed->mxedt];
         break;
      }
   }
   if(i < 0) return(e_help_loc(f, 0));
   else return(e_help_next(f, 1));
}   

int e_hp_prev(f)
     FENSTER *f;
{
   int i;
   for(i = f->ed->mxedt; i >= 0; i--)
   {  if(!strcmp(f->ed->f[i]->datnam, "Help"))
      {  e_switch_window(f->ed->edt[i], f);
         f = f->ed->f[f->ed->mxedt];
         break;
      }
   }
   if(i < 0) return(e_help_loc(f, 0));
   else return(e_help_next(f, 0));
}   

int e_hp_back(f)
     FENSTER *f;
{
   int i;
   for(i = f->ed->mxedt; i >= 0; i--)
   {  if(!strcmp(f->ed->f[i]->datnam, "Help"))
      {  e_switch_window(f->ed->edt[i], f);
         f = f->ed->f[f->ed->mxedt];
         break;
      }
   }
   if(i < 0) return(e_help_loc(f, 0));
   else return(e_help_last(f));
}   

int e_hp_ret(f)
     FENSTER *f;
{
   int i;
   for(i = f->ed->mxedt; i >= 0; i--)
   {  if(!strcmp(f->ed->f[i]->datnam, "Help"))
      {  e_switch_window(f->ed->edt[i], f);
         f = f->ed->f[f->ed->mxedt];
         break;
      }
   }
   if(i < 0) return(e_help_loc(f, 0));
   else return(e_help_ret(f));
}   



