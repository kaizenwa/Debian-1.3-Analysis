/* we_unix.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "model.h"		/*   umgekehrt f. D.S.  */
#ifdef UNIX

#include<sys/types.h>
#include<sys/stat.h>
#include<dirent.h>
#ifndef TERMCAP
#ifndef DJGPP
#include<curses.h>
/*  #include<term.h>   */
#endif
#endif
#include <signal.h>

#include "edit.h"		/*   umgekehrt f. D.S.  */
#include "attrb.h"
#ifndef DJGPP
char *schirm = NULL;
#else
unsigned char *schirm = NULL;
#endif
char e_we_sw = 0;
int e_switch_screen();

#ifndef NOANSI
int (*fk_u_locate)(int x, int y);
int (*fk_u_cursor)(int x);
int (*e_u_initscr)(int argc, char *argv[]);
int (*fk_u_putchar)(char c);
int (*u_bioskey)(int sw);
int (*e_frb_u_menue)(int sw, int xa, int ya, FENSTER *f, int md);
COLOR (*e_s_u_clr)(int f, int b);
COLOR (*e_n_u_clr)(int fb);
void (*e_pr_u_col_kasten)(int xa, int ya, int x,
					int y, FENSTER *f, int sw);
int (*fk_mouse)(int g[]);
#else
int (*fk_u_locate)();
int (*fk_u_cursor)();
int (*e_u_initscr)();
int (*fk_u_putchar)();
int (*u_bioskey)();
int (*e_frb_u_menue)();
COLOR (*e_s_u_clr)();
COLOR (*e_n_u_clr)();
void (*e_pr_u_col_kasten)();
int (*fk_mouse)();
#endif
int (*e_u_refresh)();
int (*e_u_getch)();
int (*e_u_sys_ini)();
int (*e_u_sys_end)();

#ifndef DJGPP
FARBE *u_fb, *x_fb;
#endif

#ifndef DJGPP
char MCI, MCA, RD1, RD2, RD3, RD4, RD5, RD6, WBT;
char RE1, RE2, RE3, RE4, RE5, RE6;
#else
unsigned char MCI, MCA, RD1, RD2, RD3, RD4, RD5, RD6, WBT;
#endif
/* int FBESNR = 1, FBNSNT = 1, FBNSFT = 1, FBFRFT = 1, FFESNR = 1, FBWS = 1; */
char *ctree[5];
int MENOPT = 8;
int e_mn_men = 3;
extern int col_num;
extern unsigned int spc[];

int e_ini_unix(argc, argv)
     int argc;
     char **argv;
{
   extern OPT opt[];
   int i;
#ifdef DJGPP
#ifdef PROG
   e_we_sw = 2;
#endif
#else
   for(i = strlen(argv[0])-1; i >= 0 && *(argv[0] + i) != DIRC; i--);
#ifdef XWINDOW
   if(*(argv[0]+i+1) == 'x') e_we_sw = 1;
   else e_we_sw = 0;
#endif
   u_fb = MALLOC(sizeof(FARBE));
   x_fb = MALLOC(sizeof(FARBE));
#ifdef PROG
   if(!strncmp("wpe", (argv[0] + i + e_we_sw + 1), 3)) e_we_sw |= 2;
#endif
#endif
#ifdef XWINDOW
   u_bioskey = x_bioskey;
   if(e_we_sw & 1)
   {  e_s_u_clr = e_s_x_clr;
      e_n_u_clr = e_n_x_clr;
      e_frb_u_menue = e_frb_x_menue;
      e_pr_u_col_kasten = e_pr_x_col_kasten;
      fk_u_cursor = fk_x_cursor;
      fk_u_locate = fk_x_locate;
      e_u_refresh = e_x_refresh;
      e_u_getch = e_x_getch;
      e_u_sys_ini = e_x_sys_ini;
      e_u_sys_end = e_x_sys_end;
      fk_u_putchar = fk_x_putchar;
      fk_mouse = fk_x_mouse;
      MCI = 2;
      MCA = 1;
#ifdef NEWSTYLE
      RD1 = ' ';
      RD2 = ' ';
      RD3 = ' ';
      RD4 = ' ';
      RD5 = ' ';
      RD6 = ' ';
      RE1 = ' ';
      RE2 = ' ';
      RE3 = ' ';
      RE4 = ' ';
      RE5 = ' ';
      RE6 = ' ';
#else
      RD1 = spc[2];
      RD2 = spc[1];
      RD3 = spc[3];
      RD4 = spc[0];
      RD5 = spc[4];
      RD6 = spc[5];
      RD1 = 13;
      RD2 = 12;
      RD3 = 14;
      RD4 = 11;
      RD5 = 18;
      RD6 = 25;
      RE1 = '.';
      RE2 = '.';
      RE3 = '.';
      RE4 = '.';
      RE5 = '.';
      RE6 = ':';
#endif
      WBT = 1;
/*
	ctree[0] = "\x0e\x12\x18";
	ctree[1] = "\x0e\x12\x12";
	ctree[2] = "\x0e\x18\x12";
	ctree[3] = "\x15\x12\x12";;
	ctree[4] = "\x0e\x12\x12";
*/
      ctree[0] = "\016\022\030";
      ctree[1] = "\016\022\022";
      ctree[2] = "\016\030\022";
      ctree[3] = "\025\022\022";
      ctree[4] = "\016\022\022";
   }
   else
#endif
   {  fk_u_cursor = fk_t_cursor;
      fk_u_locate = fk_t_locate;
#ifndef DJGPP
      e_u_refresh = e_t_refresh;
#endif
      e_u_getch = e_t_getch;
      e_u_sys_ini = e_t_sys_ini;
      e_u_sys_end = e_t_sys_end;
      fk_u_putchar = fk_t_putchar;
      fk_mouse = fk_t_mouse;
#ifdef DJGPP
      e_we_sw |= 1;
      e_s_u_clr = e_s_x_clr;
      e_n_u_clr = e_n_x_clr;
      e_pr_u_col_kasten = e_pr_x_col_kasten;
      e_frb_u_menue = e_frb_x_menue;
      u_bioskey = x_bioskey;
      e_t_initscr();
#else
      e_t_initscr();
      if(col_num > 0)
      {   e_pr_u_col_kasten = e_pr_x_col_kasten;
          e_frb_u_menue = e_frb_x_menue;
	  e_s_u_clr = e_s_x_clr;
	  e_n_u_clr = e_n_x_clr;
      }
      else
      {   e_pr_u_col_kasten = e_pr_t_col_kasten;
          e_frb_u_menue = e_frb_t_menue;
	  e_s_u_clr = e_s_t_clr;
	  e_n_u_clr = e_n_t_clr;
      }
#endif
#ifndef DJGPP
      MCI = '+';
      MCA = '0';
      RD1 = '\01';
      RD2 = '\02';
      RD3 = '\03';
      RD4 = '\04';
      RD5 = '\05';
      RD6 = '\06';
      RE1 = '\01';
      RE2 = '\02';
      RE3 = '\03';
      RE4 = '\04';
      RE5 = '\05';
      RE6 = '\06';
      WBT = '\13';
      ctree[0] = "\11\12\07";   /*  07 -> 30  */
      ctree[1] = "\11\12\12";	/*  11 -> 16  */
      ctree[2] = "\11\07\12";   /*  12 -> 22  */
      ctree[3] = "\10\12\12";   /*  10 -> 25  */
      ctree[4] = "\11\12\12";
/*
      RD1 = spc[2];
      RD2 = spc[1];
      RD3 = spc[3];
      RD4 = spc[0];
      RD5 = spc[4];
      RD6 = spc[5];
*//*
      RD1 = '+';
      RD2 = '+';
      RD3 = '+';
      RD4 = '+';
      RD5 = '-';
      RD6 = '|';
*//*
      RE1 = '.';
      RE2 = '.';
      RE3 = '.';
      RE4 = '.';
      RE5 = '.';
      RE6 = ':';
      WBT = '#';
      ctree[0] = "|__";
      ctree[1] = "|__";
      ctree[2] = "|__";
      ctree[3] = "|__";
      ctree[4] = "|__";
*/
#else
      MCI = '\xb1';
      MCA = '\xfe';
      RD1 = '\xc9';
      RD2 = '\xbb';
      RD3 = '\xc8';
      RD4 = '\xbc';
      RD5 = '\xcd';
      RD6 = '\xba';
      WBT = '\xfe';
      ctree[0] = "\xc0\xc4\xc2";
      ctree[1] = "\xc0\xc4\xc4";
      ctree[2] = "\xc0\xc2\xc4";
      ctree[3] = "\xc3\xc4\xc4";
      ctree[4] = "\xc0\xc4\xc4";
#endif
   }
   if(e_we_sw & 2)
#ifndef DEBUGGER
   {  opt[0].x = 2, opt[1].x = 7, opt[2].x = 14; opt[3].x = 21; opt[4].x = 30;
      opt[9].t = opt[7].t; opt[9].x = 74; opt[9].s = opt[7].s; opt[9].as = opt[7].as;
      opt[8].t = opt[6].t; opt[8].x = 65; opt[8].s = opt[6].s; opt[8].as = opt[6].as;
      opt[7].t = opt[5].t; opt[7].x = 55; opt[7].s = opt[5].s; opt[7].as = opt[5].as;
      opt[6].t = "Project"; opt[6].x = 45; opt[6].s = 'P'; opt[6].as = AltP;
      opt[5].t = "Run"; opt[5].x = 38; opt[5].s = 'R'; opt[5].as = AltR;
      MENOPT = 10;
      e_mn_men = 2;
#else
   {  opt[0].x = 2, opt[1].x = 6, opt[2].x = 12; opt[3].x = 18; opt[4].x = 26;
      opt[10].t = opt[7].t; opt[10].x = 74; opt[10].s = opt[7].s; opt[10].as = opt[7].as;
      opt[9].t = opt[6].t; opt[9].x = 65; opt[9].s = opt[6].s; opt[9].as = opt[6].as;
      opt[8].t = opt[5].t; opt[8].x = 56; opt[8].s = opt[5].s; opt[8].as = opt[5].as;
      opt[7].t = "Project"; opt[7].x = 47; opt[7].s = 'P'; opt[7].as = AltP;
      opt[6].t = "Debug"; opt[6].x = 40; opt[6].s = 'D'; opt[6].as = AltD;
      opt[5].t = "Run"; opt[5].x = 34; opt[5].s = 'R'; opt[5].as = AltR;
      MENOPT = 11;
      e_mn_men = 1;
#endif
   }
   return(argc);
}

int e_ini_schirm(argc, argv)
     int argc;
     char **argv;
{
#ifdef XWINDOW
   if(e_we_sw & 1)
   {
/*	FBESNR = cn->fb->es.f+16*cn->fb->nr.b;
	FBNSNT = cn->fb->ns.f+16*cn->fb->nt.b;
	FBNSFT = cn->fb->ns.f+16*cn->fb->ft.b;
	FBFRFT = cn->fb->fr.f+16*cn->fb->ft.b;
	FFESNR = cn->fb->es.f+16*cn->fb->nr.b;
	FBWS = 7;
*/
      return(e_x_initscr(argc, argv));
   }
   else
#endif
   {
/*	FBESNR = cn->fb->es.fb;
	FBNSNT = cn->fb->ns.fb;
	FBNSFT = cn->fb->ns.fb;
	FBFRFT = cn->fb->fr.fb;
	FFESNR = cn->fb->es.fb;
	FBWS = 0;
*/
/*
#ifdef DJGPP
      e_t_initscr();
#endif
*/
      return(argc);
   }
}
#ifndef DJGPP
int e_abs_refr()
{
   extern char *altschirm;
   int i;
   for(i = 0; i < 2 * MAXSCOL * MAXSLNS; i++) altschirm[i] = 0;
   return(0);
}
#endif
int e_tast_sim(c)
     int c;
{
   if(c >= 'A' && c <= 'Z') return(c + 1024 - 'A');
   switch(c)
   {  case 'a':  return(AltA);
      case 'b':  return(AltB);
      case 'c':  return(AltC);
      case 'd':  return(AltD);
      case 'e':  return(AltE);
      case 'f':  return(AltF);
      case 'g':  return(AltG);
      case 'h':  return(AltH);
      case 'i':  return(AltI);
      case 'j':  return(AltJ);
      case 'k':  return(AltK);
      case 'l':  return(AltL);
      case 'm':  return(AltM);
      case 'n':  return(AltN);
      case 'o':  return(AltO);
      case 'p':  return(AltP);
      case 'q':  return(AltQ);
      case 'r':  return(AltR);
      case 's':  return(AltS);
      case 't':  return(AltT);
      case 'u':  return(AltU);
      case 'v':  return(AltV);
      case 'w':  return(AltW);
      case 'x':  return(AltX);
      case 'y':  return(AltY);
      case 'z':  return(AltZ);
      case '1':  return(Alt1);
      case '2':  return(Alt2);
      case '3':  return(Alt3);
      case '4':  return(Alt4);
      case '5':  return(Alt5);
      case '6':  return(Alt6);
      case '7':  return(Alt7);
      case '8':  return(Alt8);
      case '9':  return(Alt9);
      case '0':  return(Alt0);
      case ' ':  return(AltBl);
      case '#':  return(AltSYS);
      case CtrlA:  return(CBUP);
      case CtrlE:  return(CBDO);
      case CtrlB:  return(CCLE);
      case CtrlF:  return(CCRI);
      case CtrlP:  return(CPS1);
      case CtrlN:  return(CEND);
      case CtrlH:  return(AltBS);
      default:  return(0);
   }
}

void e_err_end(sig)
     int sig;
{
/*    psignal(sig, NULL);   */
   printf("Signal: %d\nError Exit!\n", sig);
   e_exit(1);
}

void e_err_save()
{
   extern struct EXT h_error;
   ECNT *cn = h_error.cn;
   int i;
   FENSTER *f;
   BUFFER *b;
   for(i = 0; i <= cn->mxedt; i++)
   {  if(cn->f[i]->dtmd > 'Z')
      {  f = cn->f[i];
	 b = cn->f[i]->b;
	 if(b->mxlines > 1 || b->bf[0].len > 0)
	 {  strcat(f->datnam, ".ESV");
	    printf("Try to save %s!\n", f->datnam);
	    if(!e_save(f)) printf("File %s saved!\n", f->datnam);
	 }
      }
   }
}

#undef exit
void e_exit(n)
     int n;
{
#ifdef DEBUGGER
   extern int e_d_pid;
   if(e_d_pid) kill(e_d_pid, 7);
#endif
   if(!(e_we_sw & 1)) e_endwin();
   e_switch_screen(0);
   if(n != 0)
   {  printf("\nError-Exit!   Code: %d!\n", n);
      e_err_save();
   }
   exit(n);
}

char *e_mkfilepath(dr, fn, fl)
     char *dr;
     char *fn;
     char *fl;
{
   strcpy(fl, dr);
   if(dr[strlen(dr)-1] != DIRC) strcat(fl, DIRS);
   strcat(fl, fn);
   return(fl);
}

int e_compstr(a, b)
     char *a;
     char *b;
{
   int n, k;
   char *ctmp, *cp;
   if(a[0] == '*' && !a[1]) return(0);
   if(!a[0] || !b[0]) return(a[0] - b[0]);
   if(a[0] == '*' && a[1] == '*') return(e_compstr(++a, b));
   for(n = a[0] == '*' ? 2 : 1;
	a[n] != '*' && a[n] != '?' && a[n] != '[' && a[n]; n++);
   if(a[0] == '*')
   {  n--;
      a++;
      if(a[0] == '?')
      {  cp = MALLOC((strlen(a)+1)*sizeof(char));
	 strcpy(cp, a);
	 cp[0] = '*';
	 n = e_compstr(cp, ++b);
	 FREE(cp);
	 return(n);
      }
      else if(a[0] == '[')
      {  while(*b && (n = e_compstr(a, b))) b++;
	 return(n);
      }
      ctmp = MALLOC(n+1);
      for(k = 0; k < n; k++) ctmp[k] = a[k];
      ctmp[n] = '\0';
      cp = strstr(b, ctmp);
      FREE(ctmp);
      if(cp == NULL) return((a[0] - b[0]) ? a[0] - b[0] : -1);
      if(!a[n] && !cp[n]) return(0);
      if(!a[n]) return(e_compstr(a-1, cp+1));
      if(!(k = e_compstr(a+n, cp+n))) return(0);
      return(e_compstr(a-1, cp+1));
   }
   else if(a[0] == '?') {  n--;  a++;  b++;  }
   else if(a[0] == '[')
   {  if(a[1] == '!')
      {  for(k = 2; a[k] && (a[k] != ']' || k == 2) && a[k] != b[0]; k++)
	 if(a[k+1] == '-' && b[0] >= a[k] && b[0] <= a[k+2]) return(-b[0]);
	 if(a[k] != ']') return(-b[0]);
	 n-=(k+1);  a+=(k+1);  b++;
      }
      else
      {  for(k = 1; a[k] && (a[k] != ']' || k == 1) && a[k] != b[0]; k++)
	 if(a[k+1] == '-' && b[0] >= a[k] && b[0] <= a[k+2]) break;
	 if(a[k] == ']' || a[k] == '\0') return(-b[0]);
	 for(; a[k] && (a[k] != ']'); k++);
	 n-=(k+1);  a+=(k+1);  b++;
      }
   }
   if(n <= 0) return(e_compstr(a, b));
   if((k = strncmp(a, b, n)) != 0) return(k);
   return(e_compstr(a+n, b+n));
}

struct dirfile *e_find_files(sufile, sw)
     char *sufile;
     int sw;
{
   char stmp[120], *tmpst, *sfile, *sdir;
   struct dirfile *df = MALLOC( sizeof(struct dirfile) );
   DIR *dirp;
   struct dirent *dp;
   struct stat buf[1];
   int i, n;
#ifdef DJGPP
   int laufw = -1;
   if(sufile[1] == ':')
   {  laufw = getdisk();
      setdisk(sufile[0] - 'A');
      sufile += 2;
   }
#endif
   df->name = NULL;
   df->anz = 0;
   for(n = strlen(sufile); n >= 0 && sufile[n] != DIRC; n--);
   sfile = sufile+n+1;
   if(n <= 0)
   {  sdir = MALLOC(2 * sizeof(char));
      sdir[0] = n ? '.' : DIRC;  sdir[1] = '\0';
   }
   else
   {  sdir = MALLOC((n+1) * sizeof(char));
      for(i = 0; i < n; i++) sdir[i] = sufile[i];
      sdir[n] = '\0';
   }
   if(!(dirp = opendir(sdir))) {  FREE(sdir); return(df);  }
   while((dp = readdir(dirp)) != NULL)
   {  if(!(sw & 1) && dp->d_name[0] == '.' && sfile[0] != '.') continue;
      if(!e_compstr(sfile, dp->d_name))
      {  stat(e_mkfilepath(sdir, dp->d_name, stmp), buf);
#ifdef DJGPP
	 if(((buf->st_mode & S_IFMT) == S_IFREG) && (!(sw & 2) ||
	   ((n = strlen(dp->d_name)) > 3 && ( !strcmp(dp->d_name+n-4, ".exe")
		|| !strcmp(dp->d_name+n-4, ".com")
		|| !strcmp(dp->d_name+n-4, ".bat") )) ))
#else
	 if(((buf->st_mode & S_IFMT) == S_IFREG) && 
            (!(sw & 2) || (buf->st_mode & 0111)))
#endif
	 {  if(df->anz == 0) df->name = MALLOC((df->anz + 1) * sizeof(char *));
	    else df->name = REALLOC(df->name, (df->anz + 1) * sizeof(char *));
	    if(df->name == NULL || !(tmpst = MALLOC(strlen(dp->d_name)+1)))
	    {  df->anz = 0;
	       closedir(dirp);
               FREE(sdir);
	       return(df);
	    }
	    strcpy(tmpst,dp->d_name);
	    for (n = df->anz; n > 0 && strcmp(*(df->name+n-1), tmpst) > 0; n--)
              *(df->name + n) = *(df->name + n - 1);
	    *(df->name+n) = tmpst;
	    (df->anz)++;
	 }
      }
   }
   closedir(dirp);
#ifdef DJGPP
   if(laufw >= 0) setdisk(laufw);
#endif
   FREE(sdir);
   return(df);
}

struct dirfile *e_find_dir(sufile, sw)
     char *sufile;
     int sw;
{
   char stmp[120], *tmpst, *sfile, *sdir;
   struct dirfile *df = MALLOC( sizeof(struct dirfile) );
   DIR *dirp;
   struct dirent *dp;
   struct stat buf[1];
   int i, n;
#ifdef DJGPP
   int laufw = -1;
   if(sufile[1] == ':')
   {  laufw = getdisk();
      setdisk(sufile[0] - 'A');
      sufile += 2;
   }
#endif
   df->name = NULL;
   df->anz = 0;
   for(n = strlen(sufile); n >= 0 && sufile[n] != DIRC; n--);
   sfile = sufile+n+1;
   if(n <= 0)
   {  sdir = MALLOC(2 * sizeof(char));
      sdir[0] = n ? '.' : DIRC;  sdir[1] = '\0';
   }
   else
   {  sdir = MALLOC((n+1) * sizeof(char));
      for(i = 0; i < n; i++) sdir[i] = sufile[i];
      sdir[n] = '\0';
   }
   if(!(dirp = opendir(sdir))) {  FREE(sdir);  return(df);  }
   while((dp = readdir(dirp)) != NULL)
   {  if(!sw && dp->d_name[0] == '.' && sfile[0] != '.') continue;
      if(!e_compstr(sfile, dp->d_name) && strcmp(dp->d_name, ".")
                                             && strcmp(dp->d_name, ".."))
      {  stat(e_mkfilepath(sdir, dp->d_name, stmp), buf);
	 if((buf->st_mode & S_IFMT) == S_IFDIR)
	 {  if(df->anz == 0) df->name = MALLOC((df->anz + 1) * sizeof(char *));
	    else df->name = REALLOC(df->name, (df->anz + 1) * sizeof(char *));
	    if(df->name == NULL || !(tmpst = MALLOC(strlen(dp->d_name)+1)))
	    {  df->anz = 0;
	       closedir(dirp);
               FREE(sdir);
	       return(df);
	    }
	    strcpy(tmpst,dp->d_name);
	    for (n = df->anz; n > 0 && strcmp(*(df->name+n-1), tmpst) > 0; n--)
              *(df->name + n) = *(df->name + n - 1);
	    *(df->name+n) = tmpst;
	    (df->anz)++;
	 }
      }
   }
   closedir(dirp);
#ifdef DJGPP
   if(laufw >= 0) setdisk(laufw);
#endif
   FREE(sdir);
   return(df);
}

#include <time.h>

char *e_file_info(filen, str, num, sw)
     char *filen;
     char *str;
     int *num;
     int sw;
{
   struct tm *ttm;
   struct stat buf[1];
   stat(filen, buf);
   ttm = localtime(&(buf->st_mtime));
   sprintf(str, "%c%c%c%c%c%c%c%c%c%c  %-13s  %6ld  %2.2u.%2.2u.%2.2u  %2.2u.%2.2u",
	 buf->st_mode & 040000 ? 'd' : '-',
         buf->st_mode & 0400 ? 'r' : '-',
         buf->st_mode & 0200 ? 'w' : '-',
         buf->st_mode & 0100 ? 'x' : '-',
         buf->st_mode & 040 ? 'r' : '-',
         buf->st_mode & 020 ? 'w' : '-',
         buf->st_mode & 010 ? 'x' : '-',
         buf->st_mode & 04 ? 'r' : '-',
         buf->st_mode & 02 ? 'w' : '-',
         buf->st_mode & 01 ? 'x' : '-',
	 filen,
         buf->st_size, ttm->tm_mday,
         ttm->tm_mon + 1, ttm->tm_year, ttm->tm_hour, ttm->tm_min);
   if(sw & 1) *num = buf->st_mtime;
   else if(sw & 2) *num = buf->st_size;
   return(str);
}
#ifndef DJGPP
void ini_repaint(cn)
     ECNT *cn;
{
   e_cls(cn->fb->df.fb, cn->fb->dc);
   e_ini_desk(cn);
}

void end_repaint()
{
   e_refresh();
}
#endif
int e_recover(cn)
     ECNT *cn;
{
   struct dirfile *files;
   FENSTER *f = NULL;
   BUFFER *b;
   SCHIRM *s;
   int i;
   files = e_find_files("*.ESV", 1);
   for(i = 0; i < files->anz; i++)
   {  e_edit(cn, files->name[i]);
      f = cn->f[cn->mxedt];
      f->datnam[strlen(f->datnam)-4] = '\0';
      if(!strcmp(f->datnam, BUFFER_NAME))
      {  s = cn->f[cn->mxedt]->s;
	 b = cn->f[cn->mxedt]->b;
	 s->ke.y = b->mxlines - 1;
	 s->ke.x = b->bf[b->mxlines-1].len;
	 e_edt_copy(f);
	 e_close_window(f);
      }
      else f->save = 1;
#ifdef PROG
      if(e_we_sw & 2) e_add_synt_tl(f->datnam, f);
#endif
      if((f->ed->edopt & 256) || ((f->ed->edopt & 128) && f->c_st)) 
         f->flg = 1;
   }
   freedf(files);
   return(0);
}

int e_frb_t_menue(sw, xa, ya, f, md)
     int sw;
     int xa;
     int ya;
     FENSTER *f;
     int md;
{
   COLOR *frb = &(f->fb->er);
   int i, j, y, c=1, fb, fsv;
   if(md == 1) sw += 11;
   else if(md == 2) sw += 16;
   else if(md == 3) sw += 32;
   fsv = fb = frb[sw].fb;
   if(fb == 0) y = 0;
   else for(y = 1, j = fb; j > 1; y++) j /= 2;
   do
   {  if(c == CDO) y = y < 6 ? y + 1 : 0;
      else if(c == CUP) y = y > 0 ? y - 1 : 6;
      if(y == 0) fb = 0;
      else for(i = 1, fb = 1; i < y; i++) fb *= 2;
      frb[sw] = e_n_clr(fb);
      e_pr_t_col_kasten(xa, ya, fb, fb, f, 1);
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

void e_pr_t_col_kasten(xa, ya, x, y, f, sw)
     int xa;
     int ya;
     int x;
     int y;
     FENSTER *f;
     int sw;
{
   int rfrb, xe = xa + 14, ye = ya + 8;
   
   if(x == 0) y = 0;
   else  for(rfrb = x, y = 1; rfrb > 1; y++) rfrb /= 2;
   rfrb = sw == 0 ? f->fb->nt.fb : f->fb->fs.fb;
   e_std_rahmen(xa, ya, xe, ye, "Colors", 0, rfrb, 0);
/*     e_pr_str((xa+xe-8)/2, ya, "Colors", rfrb, 0, 1, 
                                        f->fb->ms.f+16*(rfrb/16), 0);
*/
   e_pr_nstr(xa+2, ya+1, xe-xa-1, "A_NORMAL   ", 0, 0);
   e_pr_nstr(xa+2, ya+2, xe-xa-1, "A_STANDOUT ", A_STANDOUT, A_STANDOUT);
   e_pr_nstr(xa+2, ya+3, xe-xa-1, "A_UNDERLINE", A_UNDERLINE, A_UNDERLINE);
   e_pr_nstr(xa+2, ya+4, xe-xa-1, "A_REVERSE  ", A_REVERSE, A_REVERSE);
   e_pr_nstr(xa+2, ya+5, xe-xa-1, "A_BLINK    ", A_BLINK, A_BLINK);
   e_pr_nstr(xa+2, ya+6, xe-xa-1, "A_DIM      ", A_DIM, A_DIM);
   e_pr_nstr(xa+2, ya+7, xe-xa-1, "A_BOLD     ", A_BOLD, A_BOLD);
   
   fk_locate(xa+4, ya + y + 1);
   
}





#endif

