/* we_fl_gr.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"
#ifdef DOS
#include <dir.h>
#include <dos.h>
#include <bios.h>

/*    Files innerhalb einer Directory finden   */
char *ctree[5] = {  "\xc0\xc4\xc2",
		    "\xc0\xc4\xc4",
		    "\xc0\xc2\xc4",
		    "\xc3\xc4\xc4",
		    "\xc0\xc4\xc4"  };

/*    Files innerhalb einer Directory finden   */

int e_manager(FENSTER *f)
{
   return(e_ed_manager(0, f));
}

int e_ed_manager(int sw, FENSTER *f)
{
   extern struct EXT h_error;
   SCHIRM *s = f->ed->f[f->ed->mxedt]->s;
   BUFFER *b = f->ed->f[f->ed->mxedt]->b;
   FILE *fp;
#if  MOUSE
   struct mouse *m;
#endif
   struct dirfile *df, *dd, *cd;
   char filen[80];
   static char rdfile[80] = "*.*";
   char *ctree[5] = {  "\xc0\xc4\xc2",
		       "\xc0\xc4\xc4",
		       "\xc0\xc2\xc4",
		       " \xc3\xc4",
		       " \xc0\xc4"  };
   PIC *pic = NULL;
   PIC *outp = NULL;
   int xa = 7, ya = 3, xe = xa+65, ye = ya+18;
   int c = 304, nf = 0, nfo = 0, ia = 0, i, j, len;
   int ica = 0, ic, nc, nco, ncoo=0, cfrb, cbg;
   int g[4], cold = c, ntmp;
   char dirct[80], ftmp[80], *dtp, *ftp;
   h_error.sw = 0;
   harderr(e_d_handler);
   pic = e_std_kst(xa, ya, xe, ye, 1, f->fb->nr.fb, f->fb->ne.fb);
   if(pic == NULL) {  e_error("Not Enough Memory (2)", 0, f->fb);  return(ESC);  }
   e_pr_str((xa+xe-11)/2, ya, "File-Manager", f->fb->nr.fb, 0, 0, 0, 0);
   
   if(sw != 2)
   e_pr_str((xe - 12), ya + 2, "Edit", f->fb->nz.fb, 0, -1,
					      f->fb->ns.fb, f->fb->nt.fb);
   if(sw != 1)
   e_pr_str((xe - 12), ya + 6, "Save", f->fb->nz.fb, 0, -1,
					      f->fb->ns.fb, f->fb->nt.fb);
   if(sw == 0)
   {  e_pr_str((xe - 12), ya + 4, "Replace", f->fb->nz.fb, 0, -1,
					      f->fb->ns.fb, f->fb->nt.fb);
      e_pr_str((xe - 12), ya + 8, "Execute", f->fb->nz.fb, 1, -1,
					      f->fb->ns.fb, f->fb->nt.fb);
      e_pr_str((xe - 12), ya + 12, "Dos", f->fb->nz.fb, 1, -1,
					      f->fb->ns.fb, f->fb->nt.fb);
   }
   e_pr_str((xe - 12), ya + 10, "Change Dir", f->fb->nz.fb, 0, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
   e_pr_str((xe - 12), ya + 14, "Cancel", f->fb->nz.fb, -1, -1,
                                              f->fb->ns.fb, f->fb->nt.fb);
   
   e_pr_str((xe - 32), ya + 2, "Name:", f->fb->nt.fb, 0, 1,
			       f->fb->nsnt.fb, f->fb->nt.fb);
   e_pr_str((xe - 32), ya + 5, "Files:", f->fb->nt.fb, 0, 1,
			       f->fb->nsnt.fb, f->fb->nt.fb);
   e_mouse_leiste(xe-17, ya+6, ye-ya-8, 0, f->fb->em.fb);
   
   e_pr_str((xa + 3), ya + 2, "Directory:", f->fb->nt.fb, 0, 1,
			       f->fb->nsnt.fb, f->fb->nt.fb);
   e_blk(25, xa+3, ya+3, f->fb->fr.fb);
   e_schr_nchar(getcwd(dirct, 80), xa+4, ya+3, 0, 23, f->fb->fr.fb);
   e_pr_str((xa + 3), ya + 5, "DirTree:", f->fb->nt.fb, 3, 1,
			       f->fb->nsnt.fb, f->fb->nt.fb);
   e_mouse_leiste(xa + 28, ya+6, ye-ya-8, 0, f->fb->em.fb);
   
   cd = e_mk_cur_dir();
   nco = nc = cd->anz - 1;
   dd = e_find_dir(SUDIR);
   df = e_find_files(rdfile);
   
   while(c != ESC)
   {  for(i = ia; i < df->anz && i-ia+ya+8 < ye; i++)
      {  if(i == nf && c == 288)
	 e_pr_nstr((xe - 31), ya+i-ia+6, 14, *(df->name+i),
                                                f->fb->fz.fb, f->fb->fz.fb);
	 else
	 e_pr_nstr((xe - 31), ya+i-ia+6, 14, *(df->name+i),
                                                f->fb->ft.fb, f->fb->ft.fb);
/*              e_pr_str((xe - 31), ya+i-ia+6, *(df->name+i), f->fb->ft.fb,
                                                             0, 0, 0, 0); */
	 len=strlen(*(df->name+i));
	 e_blk(14-len, xe-31+len, ya+6+i-ia, f->fb->ft.fb);
      }
      for(; i-ia+ya+8 < ye; i++)
      e_blk(15, xe-32, ya+6+i, f->fb->ft.fb);
      
      for(i = ica; i < cd->anz && i-ica+ya+8 < ye; i++)
      {  e_blk(i*2, xa+3, ya+6+i-ica, f->fb->ft.fb);
	 if(i == nc && c == 275) cfrb = f->fb->fz.fb;
	 else if(i == nc) cfrb = f->fb->frft.fb;
	 else  cfrb = f->fb->ft.fb;
	 if (i == 0) cbg = 4;
	 else  cbg = (i-1)*2 + 8;
	 if(i== cd->anz-1 && dd->anz < 1)
	 e_pr_str((xa+cbg-4), ya+i-ica+6, ctree[1], f->fb->ft.fb,
                                                                 0, 0, 0, 0);
	 else if(i != 0)
	 e_pr_str((xa+cbg-4), ya+i-ica+6, ctree[0], f->fb->ft.fb,
								 0, 0, 0, 0);
	 e_pr_nstr((xa+cbg), ya+i-ica+6, 29-cbg, *(cd->name+i), cfrb, f->fb->ft.fb);
	 
	 len=strlen(*(cd->name+i));
      }
      if(cd->anz == 1) cbg = 8;
      else if(ica < cd->anz) cbg += 2;
      for(j = i; j < dd->anz + cd->anz && j-ica+ya+8 < ye; j++)
      {  e_blk(cbg-4, xa+3, ya+6+j-ica, f->fb->ft.fb);
	 if(j == nc && c == 275) cfrb = f->fb->fz.fb;
	 else if(j == nc) cfrb = f->fb->frft.fb;
	 else  cfrb = f->fb->ft.fb;
	 if( dd->anz == 1)
	 e_pr_str((xa+cbg-4), ya+j-ica+6, ctree[1], f->fb->ft.fb,
                                                                 0, 0, 0, 0);
	 else if(j == dd->anz + cd->anz - 1)
	 e_pr_str((xa+cbg-4), ya+j-ica+6, ctree[4], f->fb->ft.fb,
                                                                 0, 0, 0, 0);
	 else if(j== cd->anz)
	 e_pr_str((xa+cbg-4), ya+j-ica+6, ctree[2], f->fb->ft.fb,
                                                                 0, 0, 0, 0);
	 else
	 e_pr_str((xa+cbg-4), ya+j-ica+6, ctree[3], f->fb->ft.fb,
                                                                 0, 0, 0, 0);
	 e_pr_nstr((xa+cbg), ya+j-ica+6, 29-cbg, *(dd->name+j-cd->anz),
                                                         cfrb, f->fb->ft.fb);
	 len=strlen(*(dd->name+j-cd->anz));
      }
      for(i=j; i-ica+ya+8 < ye; i++)
      e_blk(25, xa+3, ya+6+i, f->fb->ft.fb);
      
      if(df->anz > 0)
      e_pr_nstr((xa+4), ye-1, xe-xa-12,
               e_file_info(*(df->name+nf), ftmp), f->fb->ft.fb, f->fb->ft.fb);
      
      switch(c)
      {  case 304:
	    cold = c;
	    fk_cursor(1);
	    c = e_schreib_leiste(rdfile, xe-32, ya+3, 15, 80,
                                            f->fb->fr.fb, f->fb->fz.fb);
	    strupr(rdfile);
#if  MOUSE
	    if(c == -1)
	    {  ntmp = nf;
	       c = e_mng_mouse(xa, ya, xe, ye, ia, df->anz, &ntmp, cold);
	       if(c == 288) nf = ntmp;
	    }
#endif
	    if(c == 330) c = 287;
	    else if(c == 335) c = 288;
	    else if(c == 279) break;
	    else if(c != ESC && c != 287 && c != 275 && c != 300
		      && ( strstr(rdfile,"*") || strstr(rdfile,"?") ) )               {  freedf(df);
											 df = e_find_files(rdfile);
											 nf = 0;
											 c = 288;
										      }
	    else
	    {  strcpy(filen, rdfile);
	       if(c == CR) c = (sw == 2) ? 286 : 273;
	    }
	    if (c != 304)
	    e_schr_nchar(rdfile, xe-31, ya+3, 0, 13, f->fb->fr.fb);
	    fk_cursor(0);
	    break;
	 case 300:
	    if(sw != 0) {  c = cold;  break;  }
/*
               if(cold == 304 && ( strstr(filen,"*") || strstr(filen,"?") ) )
               {   c = 288;  break;  }
*/
	 case 279:
	    if(sw != 0) {  c = cold;  break;  }
	    outp = e_open_view(0,0,MAXSCOL-1,MAXSLNS-1,f->fb->ws,1);
	    fk_locate(0,0);
	    fk_cursor(1);
	    g[0] = 2;
#if  MOUSE
	    fk_mouse(g);
#endif
	    if (c == 279) system("");
	    else
	    {  if(cold == 288) strcpy(filen, *(df->name + nf));
	       printf("Execute Command: %s\n", filen);
	       if(system(filen))
	       {
		  e_error("Command not found!",0,f->fb);
	       }
	       else
	       {  printf("Strike Any Key: ");
		  fk_getch();
	       }
	    }
	    e_close_view(outp, 1);
	    c = cold;
	    fk_cursor(0);
#if  MOUSE
	    g[0] = 1;
	    fk_mouse(g);
#endif
	    break;
	 case 287:
	    cold = c;
	    fk_cursor(1);
	    c = e_schreib_leiste(dirct, xa+4, ya+3, 23, 80,
					    f->fb->fr.fb, f->fb->fz.fb);
	    strupr(dirct);
#if  MOUSE
	    if(c == -1)
	    {  ntmp = nc;
	       c = e_mng_mouse(xa, ya, xe, ye, ica, cd->anz+dd->anz-1, &ntmp, cold);
	       if(c == 275) nc = ntmp;
	    }
#endif
	    if(c == 332) c = 304;
	    else if(c == 335) c = 275;
	    else if(c== CR) c = 301;
	    if (c != 287)
	    e_schr_nchar(dirct, xa+4, ya+3, 0, 23, f->fb->fr.fb);
	    fk_cursor(0);
	    break;
	 case 275:
	    cold = c;
#if  MOUSE
	    if((c = e_getch()) == -1)
	    {  ntmp = nc;
	       c = e_mng_mouse(xa, ya, xe, ye, ica, cd->anz+dd->anz, &ntmp, cold);
	       if(c == 275) nc = ntmp;
	    }
#else
	    c = e_getch();
#endif
	    if (c == 327)
	    {  if(nc <= 0)  c = 287;
	       else  {  nc--; c = 275;  }
	    }
	    else if (c == 335 && nc < cd->anz+dd->anz-1)
	    {  nc++; c = 275;  }
	    else if (c == 332) c = 288;
	    else if (c == 328) {  if((nc-=(ye-ya-9)) < 0) nc =0; c = 275;  }
	    else if (c == 336) {  if((nc+=(ye-ya-9)) > cd->anz+dd->anz-1)
				  nc = cd->anz+dd->anz-1; c = 275;  }
	    else if (c == 326) {  nc = 0; c = 275;  }
	    else if (c == 334) {  nc = cd->anz+dd->anz-1; c = 275;  }
	    else if (c == 301 || (c == CR && nc != nco))
	    {  c = 275;
	       if(nc == 0)
	       {  cd->anz = 1;
		  nco = nc;
		  freedf(dd);
		  dd = e_mk_drives();
		  for(nc = 1; **(dd->name+nc-1) != *dirct
				   && nc <= dd->anz; nc++);
		  nco = nc;
	       }
	       else
	       {  if(cd->anz == 1)
		  {  h_error.sw = 0;
		     fk_setdisk(**(dd->name+nc-1)-'A');
		     if(h_error.sw == 0) getcwd(dirct, 80);
		     if(h_error.sw != 0)
		     {  fk_setdisk(**(dd->name+nco-1)-'A');  nc = nco;  break;  }
		  }
		  else
		  e_mk_path(dirct, cd, dd, nc);
		  e_schr_nchar(dirct, xa+4, ya+3, 0, 23, f->fb->fr.fb);
		  c = 301;
	       }
	    }
	    else if(c == CR) c = 275;
	    if(nc > cd->anz+dd->anz-1) nc = cd->anz+dd->anz-1;
	    ncoo = e_lst_zeichen(xa+28, ya+6, ye-ya-8, 0,
                                  f->fb->em.fb, cd->anz+dd->anz, ncoo, nc);
	    break;
	 case 301:
	    chdir(dirct);
	    freedf(df);
	    freedf(cd);
	    freedf(dd);
	    if((f->ed->dirct = REALLOC(f->ed->dirct,
                                         strlen(dirct)+1)) == NULL)
	    e_error("Not Enough Memory (2)", 0, f->fb);
	    else
	    strcpy(f->ed->dirct, dirct);
	    cd = e_mk_cur_dir();
	    nco = nc = cd->anz - 1;
	    dd = e_find_dir(SUDIR);
	    df = e_find_files(rdfile);
	    nf = ia = 0;
	    ica = 0;
	    c = cold;
	    break;
	 case 288:
	    if (df->anz < 1) {  c = cold; break;  }
	    cold = c;
#if  MOUSE
	    if((c = e_getch()) == -1)
	    {  ntmp = nf;
	       c = e_mng_mouse(xa, ya, xe, ye, ia, df->anz, &ntmp, cold);
	       if(c == 288) nf = ntmp;
	    }
#else
	    c = e_getch();
#endif
	    if (c == 327)
	    {  if(nf <= 0)  c = 304;
	       else  {  nf--; c = 288;  }
	    }
	    else if (c == 335 && nf < df->anz-1)
	    {  nf++; c = 288;  }
	    else if (c == 330) c = 275;
	    else if (c == 328) {  if((nf-=(ye-ya-9)) < 0) nf = 0; c = 288;  }
	    else if (c == 336) {  if((nf+=(ye-ya-9)) > df->anz-1)
				  nf = df->anz-1; c = 288;  }
	    else if (c == 326) {  nf = 0; c = 288;  }
	    else if (c == 334) {  nf = df->anz-1; c = 288;  }
	    else if (c == CR || c == 273)
	    {  strcpy(filen, *(df->name + nf));
	       c = 273;
	    }
	    else if (c == 286 || c == 274)
	    strcpy(filen, *(df->name + nf));
	    nfo = e_lst_zeichen(xe-17, ya+6, ye-ya-8, 0, f->fb->em.fb,
                                                       df->anz, nfo, nf);
	    if(nf > df->anz-1) nf = df->anz-1;
	    break;
	 case 286:
	    if(sw == 1 || f->ins == 8) {  c = cold;  break;  }
	    freedf(df);
	    freedf(dd);
	    freedf(cd);
	    e_close_view(pic, 1);
	    if( !access(filen, 0) )
	    {  strcpy(ftmp, "File ");
	       strcat(ftmp, filen);
	       strcat(ftmp, " exist !");
	       c = e_message(7, ftmp, "Do You want to overwrite File ?", f->fb);
	       if( c == ESC ) break;
	       else if(c == 'n') {  c = 288; break;  }
	    }
	    if(sw != 0)
	    {  dtp = f->dirct;  ftp = f->datnam;  }
	    else
	    {  FREE(f->dirct);  FREE(f->datnam);  }
	    e_mkeddir(f, filen);
	    if(sw == 0)
	    e_save(f);
	    else
	    {  e_write(s->ka.x, s->ka.y, s->ke.x, s->ke.y, f);
	       FREE(f->dirct);  FREE(f->datnam);
	       f->dirct = dtp;  f->datnam = ftp;
	    }
	    e_ed_rahmen(f, 1);
	    return('s');
	 case 274:
	    if(sw != 0) {  c = cold;  break;  }
	 case 273:
	    if(sw == 2 || h_error.sw != 0) {  c = cold;  break;  }
	    if(strstr(filen,"*") || strstr(filen,"?") )
	    {  c = 288;  break;  }
	    freedf(df);
	    freedf(dd);
	    freedf(cd);
	    e_close_view(pic, 1);
	    if(c == 274 && f->ed->mxedt > 0) e_close_window(f);
	    if(sw == 0)  e_edit(f->ed, filen);
	    else
	    {  if((fp = fopen(filen,"rb")) == NULL)  {  c = cold; break;  }
	       if(access(filen, 2) != 0) f->ins = 8;
	       if(b->b.x != 0)
	       {  e_new_line(b->b.y+1, b);
		  if(*(b->bf[b->b.y].s+b->bf[b->b.y].len) != '\0') (b->bf[b->b.y].len)++;
		  for(i = b->b.x;  i <= b->bf[b->b.y].len; i++)
                     *(b->bf[b->b.y+1].s+i-b->b.x) = *(b->bf[b->b.y].s+i);
		  *(b->bf[b->b.y].s+b->b.x) = '\0';
		  b->bf[b->b.y].len = b->b.x;
		  b->bf[b->b.y+1].len = e_str_len(b->bf[b->b.y+1].s);
		  b->bf[b->b.y+1].nrc = e_str_nrc(b->bf[b->b.y+1].s);
	       }
	       s->ka.x = b->b.x; s->ka.y = b->b.y;
	       s->ke = e_readin(b->b.x, b->b.y, fp, b, f->dtmd);
	       e_schirm(f, 1);
	    }
	    if(h_error.sw == 0) getcwd(ftmp, 80);
	    if( (f->ed->dirct = REALLOC(f->ed->dirct, strlen(ftmp)+1)) == NULL)
	    {  e_error("Kein Speicherplatz", 0, f->fb); c = ESC; break;  }
	    strcpy(f->ed->dirct, ftmp);
	    return('e');
	 default:
	    c = cold;
	    break;
      }
      if(nf-ia+ya+8 >= ye) ia = nf+ya-ye+9;
      else if(nf-ia < 0) ia = nf;
      if(nc-ica+ya+8 >= ye) ica = nc+ya-ye+9;
      else if(nc-ica < 0) ica = nc;
   }
   
   freedf(df);
   freedf(dd);
   freedf(cd);
   e_close_view(pic, 1);
   if(h_error.sw == 0) getcwd(ftmp, 80);
   if( (f->ed->dirct = REALLOC(f->ed->dirct, strlen(ftmp)+1)) == NULL)
   {  e_error("Kein Speicherplatz", 0, f->fb);  }
   else strcpy(f->ed->dirct, ftmp);
   return(ESC);
}

struct dirfile *e_find_files(char *sufile)
{
   struct dirfile *df = MALLOC( sizeof(struct dirfile) );
   struct ffblk drct[1];
   char *tmpst, **tmpz;
   int done, i, maxf = 0;
   
   df->name = NULL;
   done = findfirst(sufile,drct,0);
   for(df->anz = 0; !done; (df->anz)++)
   {  tmpst = MALLOC(strlen(drct->ff_name)+1);
      strcpy(tmpst,drct->ff_name);
      if(maxf == df->anz)
      {  maxf += 40;
	 tmpz = MALLOC(sizeof(char *)*maxf);
	 for(i = 0; i <= maxf - 40; i++)
		*(tmpz + i) = *(df->name + i);
	 if(df->name != NULL) FREE(df->name);
	 df->name = tmpz;
      }
      for (i = df->anz; i > 0 && strcmp(*(df->name+i-1), tmpst) > 0; i--)
	    *(df->name + i) = *(df->name + i - 1);
      *(df->name+i) = tmpst;
      
      done = findnext(drct);
   }
   return(df);
}

/*    Directorys innerhalb einer Directory finden   */

struct dirfile *e_find_dir(char *sufile)
{
   struct dirfile *df  = MALLOC( sizeof(struct dirfile) );
   struct ffblk drct[1], dfff[1];
   char *tmpst, **tmpz;
   int done, i, maxf = 0;
   
   df->name = NULL;
   df->anz = 0;
   findfirst(sufile, dfff, 0);
   done = findfirst(sufile,drct,FA_DIREC);
   while( !done )
   {  if( strcmp(dfff->ff_name,drct->ff_name) )
      {  if( strcmp(drct->ff_name,".") && strcmp(drct->ff_name,"..") )
	 {  tmpst = MALLOC(strlen(drct->ff_name)+1);
	    strcpy(tmpst,drct->ff_name);
	    if(maxf == df->anz)
	    {  maxf += 40;
	       tmpz = MALLOC(sizeof(char *)*maxf);
	       for(i = 0; i <= maxf - 40; i++)
		   *(tmpz + i) = *(df->name + i);
	       if(df->name != NULL) FREE(df->name);
	       df->name = tmpz;
	    }
	    for (i = df->anz; i > 0 && strcmp(*(df->name+i-1), tmpst) > 0; i--)
		*(df->name + i) = *(df->name + i - 1);
	    *(df->name+i) = tmpst;
	    (df->anz)++;
	 }
      }
      else
      findnext(dfff);
      
      done = findnext(drct);
   }
   return(df);
}

/*   Testen der Laufwerksbereitschaft   */

int e_dsk_test(int disk)
{
   extern struct EXT h_error;
   int result;
   char buffer[512];
   
   /*  Hard-Error abfangen  */
   h_error.sw = 0;
   harderr(e_d_handler);
   
   /* Disk-Reset */
   result = biosdisk(0,disk,0,0,0,1,buffer);
   
   /* versucht einen Sektor zu lesen */
   result = biosdisk(2,disk,0,0,1,1,buffer);
   
   /* wertet das Ergebnis aus */
   return (result);
}

/*    Vollstaendige Datei - Informationen     */

char *e_file_info(char *filen, char *str)
{
   struct ffblk finfo[1];
   struct tm {  unsigned sec:5;
		unsigned min:6;
		unsigned hr :5;  } *ftm;
   struct dt {  unsigned day:5;
		unsigned mon:4;
		unsigned yr :7;  } *fdt;
   findfirst(filen, finfo, 0);
   ftm = (struct tm *) &(finfo->ff_ftime);
   fdt = (struct dt *) &(finfo->ff_fdate);
   sprintf(str, "%-13.13s  %c  %6ld   %2.2u.%2.2u.%2.2u   %2.2u.%2.2u",
	 finfo->ff_name, finfo->ff_attrib, finfo->ff_fsize, fdt->day,
	 fdt->mon, fdt->yr+80, ftm->hr, ftm->min);
   return(str);
}

/*   Behandlung von Hardware-Fehlermeldungen   */

static char *err_msg[] = {
   "Disk is Write-Protected",
   "Unknown Unit",
   "Drive not ready",
   "Unknown Command",
   "Data-Error (CRC)",
   "rong Length (Request-Struktur)",
   "Search-Error",
   "Unknown Medium-Typ",
   "Sektor not found",
   "Printer got no Paper",
   "Write-Error",
   "Read-Error",
   "General Error",
   "Unknown Error",
   "Unknown Error",
   "Invallid Disk-Change"
};

int e_h_error(char *msg)
{
   extern struct EXT h_error;
   h_error.sw = 1;
   e_error(msg, 0, h_error.cn->fb);
   return(0);
}

 /* Das Pragma warn -par unterdr?ckt die
    Warnungen des Compilers, die sich auf
    die Nichtbenutzung der Parameter
    errval, bp und si von handler beziehen.
 */

#pragma warn -par

/*   Hardware-Fehlerbehandlung   */

int e_p_handler(int errval,int ax,int bp,int si)
{
   static char msg[200];
   unsigned di;
   int errorno;
   
   di= _DI;
   
   errorno = di & 0x00FF;
   
    /* meldet, um welchen Fehler
       es sich handelt */
   sprintf(msg, "Printer-Error: %s!", err_msg[errorno]);
   
    /* zur?ck zum Programm  */
   e_h_error(msg);
   hardretn(EOF);
   
   return EOF;
}

int e_d_handler(int errval,int ax,int bp,int si)
{
   static char msg[200];
   unsigned di;
   int drive;
   int errorno;
   
   di= _DI;
   
   drive = ax & 0x00FF;
   errorno = di & 0x00FF;
   
    /* meldet, um welchen Fehler
       es sich handelt */
   sprintf(msg, "Error at Drive %c: %s", 'A'+drive, err_msg[errorno]);
   
   e_h_error(msg);
   
   hardretn(EOF);
   
   return 2;
}
#pragma warn +par

/*   Angabe des Drucker Ports   */

e_printer_port(FENSTER *f)
{
   int c = e_num_kst("Lineprinter Number:", f->ed->prnt+1, 9, f) - 1;
   return( c < 0 ? 0 : c);
}

/*   Datei drucken    */

int e_drucke_datei(FENSTER *f)
{
   extern struct EXT h_error;
   BUFFER *b = f->ed->f[f->ed->mxedt]->b;
   int i, j;
#if  MOUSE
   while(e_mshit() != 0);
#endif
   h_error.sw = 0;
   harderr(e_p_handler);
   fputc(ESC, stdprn);
   if(h_error.sw != 0) return(ESC);
   fputc('t', stdprn);
   if(h_error.sw != 0) return(ESC);
   fputc(1, stdprn);
   if(h_error.sw != 0) return(ESC);
   fputc(ESC, stdprn);
   if(h_error.sw != 0) return(ESC);
   fputc('6', stdprn);
   if(h_error.sw != 0) return(ESC);
   for(j = 0; j < b->mxlines && h_error.sw == 0; j++)
   {  for(i = 0; i < b->bf[j].len && h_error.sw == 0; i++)
      putc(*(b->bf[j].s+i), stdprn);
      
      if(h_error.sw != 0) return(ESC);
      putc(10, stdprn);
      if(h_error.sw != 0) return(ESC);
      putc(13, stdprn);
   }
   return(ESC);
}

/*  Struktur bis zur Arbeits-Directory erstellen  */

struct dirfile *e_mk_cur_dir(void)
{
   struct dirfile *df = MALLOC(sizeof(struct dirfile));
   char buf[256];
   char tmp[256];
   char **dftmp;
   int buflen = 256, maxd = 10, i, j, k;
   if (getcwd(buf, buflen) == NULL) return(NULL);
   df->name = MALLOC(sizeof(char *) * maxd);
   df->anz = 1;
   *(df->name) = MALLOC(7*sizeof(char));
   strcpy(*(df->name), "Drives");
   for ( i = 0, j = 0; i < buflen; i++, j++ )
   {  tmp[j] = buf[i];
      if(tmp[j] == DIRC || tmp[j] == '\0')
      {  if(buf[i] == '\0' && j == 0) return(df);
	 if(df->anz == 1) j++;
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

#if defined(DOS) || defined(DJGPP)

/*   Angeschlossene Laufwerke finden   */

struct dirfile *e_mk_drives(void)
{
   struct dirfile *df = MALLOC(sizeof(struct dirfile));
   int drvs, i;
   df->name = MALLOC(sizeof(char *) * 26);
   df->anz = 0;
   for ( i = 0; i < 26; i++)
   {  if(fk_lfw_test(i) >= 0)
      {  *(df->name + df->anz) = MALLOC(2 * sizeof(char));
	 *(*(df->name + df->anz)) = 'A'+i;
	 *(*(df->name + df->anz)+1) = '\0';
	 (df->anz)++;
      }
   }
   return(df);
}

#endif


