/* we_term.c                                              */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"
#ifdef UNIX
#ifndef DJGPP

#include <termio.h>
#include <fcntl.h>

/*
#undef NL1
#undef CR1
#undef CR2
#undef CR3
#undef TAB1
#undef BS1
#undef FF1
#undef B50
#undef B75
#undef B110
#undef B134
#undef B150
#undef B200
#undef B300
#undef B600
#undef B1200
#undef B1800
#undef B2400
#undef B4800
#undef B9600
#undef EXTA
#undef EXTB
#undef HUPCL
#undef ECHO
#undef TIOCHPCL
#undef TIOCEXCL
#undef TIOCNXCL

#undef O_SYNC
#define O_SYNC 020

#ifndef TERMCAP
#include<curses.h>
#ifndef DJGPP
#include<term.h>
#endif
#endif
*/
#include<signal.h>
#define KEYFN 42
#define NSPCHR 13

int MAXSLNS = 24;
int MAXSCOL = 80;

char *key_f[KEYFN], *key_key;
char *cur_rc, *cur_vs, *cur_nvs, *cur_vvs, cur_attr;
char *att_no, *att_so, *att_ul, *att_rv, *att_bl, *att_dm, *att_bo;
char *ratt_no, *ratt_so, *ratt_ul, *ratt_rv, *ratt_bl, *ratt_dm, *ratt_bo;
char *beg_scr, *swt_scr, *sav_cur, *res_cur; 
char *altschirm;
char *col_fg, *col_bg, *spc_st, *spc_in, *spc_bg, *spc_nd;

int col_num = 0;
char *sp_chr[NSPCHR];

int cur_x = -1, cur_y = -1;
struct termio otermio, ntermio, ttermio;
/*  TERMINAL *save_term;   */
#ifdef TERMCAP
char area[315];
char *ap = area;
char tcbuf[1024];
char *tc_screen;
char *tgetstr();
int tgetnum();
char *tgoto();
/*  short int colums, lines;   */
#define tigetstr(k) tgetstr(k, &ap)
#else
char *tigetstr();
int tigetnum();
char *tparm(/* ??? */);
int endwin();
int noecho();
#endif
int def_shell_mode();
int initscr(/* ??? */);
int tputs();
int putp();
int setupterm();
int e_switch_screen();

extern int e_term_def;

char *init_key(key)
     char *key;
{
   char *tmp, *keystr;
   tmp = (char *) tigetstr(key);
   if(tmp == NULL || tmp == ((char *) -1)) return(NULL);
   else
   {  keystr = MALLOC(strlen(tmp)+1);
      strcpy(keystr, tmp);
   }
   return(keystr);
}

char *init_kkey(key)
     char *key;
{
   char *tmp;
   int i;
   tmp = init_key(key);
   if(tmp == NULL) return(NULL);
   if(!key_key)
   {  key_key = MALLOC(2); key_key[0] = tmp[1];
      key_key[1] = '\0';  return(tmp);  }
   else
   {  for(i = 0; key_key[i] != '\0'; i++)
      if(key_key[i] == tmp[1]) return(tmp);
      key_key = REALLOC(key_key, i + 2);
      key_key[i] = tmp[1];
      key_key[i + 1] = '\0';
   }
   return(tmp);
}

char *init_spchr(c)
     char c;
{
   int i;
   char *pt = NULL;
   if(!spc_st || !spc_bg || !spc_nd) return(NULL);
   for(i = 0; spc_st[i] && spc_st[i+1] && spc_st[i] != c; i+=2);
   if(spc_st[i] && spc_st[i+1])
   {  pt = MALLOC((strlen(spc_bg)+strlen(spc_nd)+2)*sizeof(char));
      if(pt) sprintf(pt, "%s%c%s", spc_bg, spc_st[i+1], spc_nd);
   }
   return(pt);
}

int init_cursor()
{
/*   int i;     */
#ifndef TERMCAP
   if(!(cur_rc = init_key("cup"))) return(-1);
   if((col_fg = init_key("setf")) && (col_bg = init_key("setb")))
   {  if((col_num = tigetnum("colors")) < 0) col_num = 8;  }
   else
   {  if(col_fg) {  free(col_fg);  col_fg = NULL;   }
      if(col_bg) {  free(col_bg);  col_bg = NULL;   }
   }
   spc_st = init_key("acsc");
   spc_in = init_key("enacs");
   spc_bg = init_key("smacs");
   spc_nd = init_key("rmacs");
   att_no = init_key("sgr0");
   att_so = init_key("smso");
   att_ul = init_key("smul");
   att_rv = init_key("rev");
   att_bl = init_key("blink");
   att_dm = init_key("dim");
   att_bo = init_key("bold");
   ratt_no = init_key("sgr0");
   ratt_so = init_key("rmso");
   ratt_ul = init_key("rmul");
   ratt_rv = init_key("sgr0");
   ratt_bl = init_key("sgr0");
   ratt_dm = init_key("sgr0");
   ratt_bo = init_key("sgr0");
   beg_scr = init_key("smcup");
   swt_scr = init_key("rmcup");
   sav_cur = init_key("sc");
   res_cur = init_key("rc");

   key_f[0] = init_kkey("kf1");
   key_f[1] = init_kkey("kf2");
   key_f[2] = init_kkey("kf3");
   key_f[3] = init_kkey("kf4");
   key_f[4] = init_kkey("kf5");
   key_f[5] = init_kkey("kf6");
   key_f[6] = init_kkey("kf7");
   key_f[7] = init_kkey("kf8");
   key_f[8] = init_kkey("kf9");
   key_f[9] = init_kkey("kf10");
   key_f[10] = init_kkey("kcuu1");
   key_f[11] = init_kkey("kcud1");
   key_f[12] = init_kkey("kcub1");
   key_f[13] = init_kkey("kcuf1");
   if(e_term_def == 1)
   {  key_f[14] = init_kkey("kfnd");
      key_f[15] = init_kkey("kich1");
      if(!(key_f[16] = init_kkey("kdch1"))) key_f[16] = "\33[3~";
      key_f[17] = init_kkey("kslt");
      key_f[18] = init_kkey("kpp");
      key_f[19] = init_kkey("knp");
   }
   else
   {  key_f[14] = init_kkey("kich1");
      key_f[15] = init_kkey("khome");
      key_f[16] = init_kkey("kpp");
/*   	key_f[17] = init_kkey("kdch1");      */
      if(!(key_f[17] = init_kkey("kdch1"))) key_f[17] = "\177";
      key_f[18] = init_kkey("kend");
      key_f[19] = init_kkey("knp");
   }
   key_f[20] = init_kkey("kbs");
   key_f[21] = init_kkey("khlp");
   key_f[22] = init_kkey("kll");
   key_f[23] = init_kkey("kf17");
   key_f[24] = init_kkey("kf18");
   key_f[25] = init_kkey("kf19");
   key_f[26] = init_kkey("kf20");
   key_f[27] = init_kkey("kf21");
   key_f[28] = init_kkey("kf22");
   key_f[29] = init_kkey("kf23");
   key_f[30] = init_kkey("kf24");
   key_f[31] = init_kkey("kf25");
   key_f[32] = init_kkey("kf26");
   key_f[33] = init_kkey("kPRV");
   key_f[34] = init_kkey("kNXT");
   key_f[35] = init_kkey("kLFT");
   key_f[36] = init_kkey("kRIT");
   key_f[37] = init_kkey("kHOM");
   key_f[38] = init_kkey("kri");
   key_f[39] = init_kkey("kEND");
   key_f[40] = init_kkey("kind");
   key_f[41] = init_kkey("kext");

#else

   if(!(cur_rc = init_key("cm"))) return(-1);
   if((col_fg = init_key("Sf")) && (col_bg = init_key("Sb")))
   {  if((col_num = tgetnum("Co")) < 0) col_num = 8;  }
   else
   {  if(col_fg) {  free(col_fg);  col_fg = NULL;   }
      if(col_bg) {  free(col_bg);  col_bg = NULL;   }
   }
   spc_st = init_key("ac");
   spc_in = init_key("eA");
   spc_bg = init_key("as");
   spc_nd = init_key("ae");
   att_no = init_key("me");
   att_so = init_key("so");
   att_ul = init_key("us");
   att_rv = init_key("mr");
   att_bl = init_key("mb");
   att_dm = init_key("mh");
   att_bo = init_key("md");
   ratt_no = init_key("me");
   ratt_so = init_key("se");
   ratt_ul = init_key("ue");
   ratt_rv = init_key("me");
   ratt_bl = init_key("me");
   ratt_dm = init_key("me");
   ratt_bo = init_key("me");
   beg_scr = init_key("ti");
   swt_scr = init_key("te");
   sav_cur = init_key("sc");
   res_cur = init_key("rc");
   
   key_f[10] = init_kkey("ku");
   key_f[11] = init_kkey("kd");
   key_f[12] = init_kkey("kl");
   key_f[13] = init_kkey("kr");
   key_f[14] = init_kkey("kI");
   key_f[15] = init_kkey("kh");
   key_f[16] = init_kkey("kP");
   key_f[17] = init_kkey("kD");
   key_f[18] = init_kkey("@7");
   key_f[19] = init_kkey("kN");
   key_f[20] = init_kkey("kb");
   key_f[21] = init_kkey("%1");
   key_f[22] = init_kkey("kH");
   key_f[0] = init_kkey("k1");
   key_f[1] = init_kkey("k2");
   key_f[2] = init_kkey("k3");
   key_f[3] = init_kkey("k4");
   key_f[4] = init_kkey("k5");
   key_f[5] = init_kkey("k6");
   key_f[6] = init_kkey("k7");
   key_f[7] = init_kkey("k8");
   key_f[8] = init_kkey("k9");
   key_f[9] = init_kkey("k;");
   key_f[23] = init_kkey("F7");
   key_f[24] = init_kkey("F8");
   key_f[25] = init_kkey("F9");
   key_f[26] = init_kkey("FA");
   key_f[27] = init_kkey("FB");
   key_f[28] = init_kkey("FC");
   key_f[29] = init_kkey("FD");
   key_f[30] = init_kkey("FE");
   key_f[31] = init_kkey("FF");
   key_f[32] = init_kkey("FG");
   key_f[33] = init_kkey("%e");
   key_f[34] = init_kkey("%c");
   key_f[35] = init_kkey("#4");
   key_f[36] = init_kkey("%i");
   key_f[37] = init_kkey("#2");
   key_f[38] = init_kkey("kR");
   key_f[39] = init_kkey("*7");
   key_f[40] = init_kkey("kF");
   key_f[41] = init_kkey("@1");
#endif
   sp_chr[0] = "";
   if(!(sp_chr[1] = init_spchr('l'))) sp_chr[1] = "+";
   if(!(sp_chr[2] = init_spchr('k'))) sp_chr[2] = "+";
   if(!(sp_chr[3] = init_spchr('m'))) sp_chr[3] = "+";
   if(!(sp_chr[4] = init_spchr('j'))) sp_chr[4] = "+";
   if(!(sp_chr[5] = init_spchr('q'))) sp_chr[5] = "-";
   if(!(sp_chr[6] = init_spchr('x'))) sp_chr[6] = "|";
   if(!(sp_chr[7] = init_spchr('w'))) sp_chr[7] = "_";
   if(!(sp_chr[8] = init_spchr('t'))) sp_chr[8] = "|";
   if(!(sp_chr[9] = init_spchr('m'))) sp_chr[9] = "|";
   if(!(sp_chr[10] = init_spchr('q'))) sp_chr[10] = "_";
   if(!(sp_chr[11] = init_spchr('`'))) sp_chr[11] = "#";
   if(!(sp_chr[12] = init_spchr('a'))) sp_chr[12] = " ";
   return(0);
}
/*
void e_ini_schirm()
{
    extern struct EXT h_error;
    e_initscr();
}
*/
int e_t_initscr()
{  int ret;
   ret = ioctl(1, TCGETA, &otermio);     /* save old settings */
/*
    if(ret)
    {	printf("Error in Terminal Initialisation Code: %d\n", ret);
    	printf("c_iflag = %o, c_oflag = %o, c_cflag = %o,\n",
		otermio.c_iflag, otermio.c_oflag, otermio.c_cflag);
    	printf("c_lflag = %o, c_line = %o, c_cc = {\"\\%o\\%o\\%o\\%o\\%o\\%o\\%o\\%o\"}\n",
		otermio.c_lflag, otermio.c_line, otermio.c_cc[0], otermio.c_cc[1], 
		otermio.c_cc[2], otermio.c_cc[3], otermio.c_cc[4], otermio.c_cc[5], 
		otermio.c_cc[6], otermio.c_cc[7]);
	exit(1);
    }
*/
/*
#ifndef TERMCAP
   def_shell_mode();
   initscr();
#endif
*/
/*    delwin(stdscr);    */
/*    save_term = (TERMINAL *) MALLOC(sizeof(TERMINAL));   */
   e_begscr();
   schirm = MALLOC(2 * MAXSCOL * MAXSLNS);
   altschirm = MALLOC(2 * MAXSCOL * MAXSLNS);
#if defined(XWINDOW) && defined(NEWSTYLE)
   extbyte = MALLOC(MAXSCOL * MAXSLNS);
#endif
   e_abs_refr();
   if(init_cursor())
   {  printf("Terminal Not in the right mode\n");
      e_exit(1);
   }
   ioctl(0, TCGETA, &ntermio);
   ntermio.c_iflag = 0;            /* setup new settings */
   ntermio.c_oflag = 0;
   ntermio.c_lflag = 0;
   ntermio.c_cc[VMIN] = 1;
   ntermio.c_cc[VTIME] = 0;
#ifndef NOVSWTCH
   ntermio.c_cc[VSWTCH] = 0;
#endif
   ioctl(0, TCSETA, &ntermio);
   if(spc_in) e_putp(spc_in);
   return(0);
}

int svflgs, kbdflgs;

int e_begscr()
{
   int cols, lns;
   kbdflgs = fcntl( 0, F_GETFL, 0 );
#ifndef TERMCAP
/*   noecho();    */
   setupterm((char *)0, 1, (int *)0);
   if((lns = tigetnum("lines")) > 0) MAXSLNS = lns;
   if((cols = tigetnum("cols")) > 0) MAXSCOL = cols;
#else
   if ((tc_screen = getenv("TERM")) == NULL)
   e_exitm("Environment variable TERM not defined!", 1);
   if ((tgetent(tcbuf, tc_screen)) != 1)
   e_exitm("Unknown terminal type!", 1);
   if((lns = tgetnum("li")) > 0) MAXSLNS = lns;
   if((cols = tgetnum("co")) > 0) MAXSCOL = cols;
/*    
    if ((lines=(short)tgetnum("li")) == -1)
		e_exitm("termcap entry incomplete (lines)", 1);
    if ((colums=(short)tgetnum("co")) == -1)
		e_exitm("termcap entry incomplete (colums)", 1);
*/
#endif
/*    *save_term = *cur_term;   */
   
   signal(SIGSEGV, e_exit);
   
   return(0);
}

#define fk_putp(p) ( p ? e_putp(p) : e_putp(att_no) )

int e_endwin()
{
/*
#ifndef TERMCAP
   endwin();
#endif
*/
   fk_putp(ratt_bo);
   ioctl(0, TCSETA, &otermio);
   return(0);
}

int fk_t_cursor(x)
     int x;
{
   return(x);
/*    if(x == 0) putp(cur_nvs);
    else putp(cur_vvs);    */
}

int fk_t_putchar(c)
     char c;
{
   return(fputc(c, stdout));
}

int fk_attrset(a)
     int a;
{
   if(cur_attr == a) return(0);
   switch(cur_attr)
   {  case 0:  fk_putp(ratt_no);  break;
      case 1:  fk_putp(ratt_so);  break;
      case 2:  fk_putp(ratt_ul);  break;
      case 4:  fk_putp(ratt_rv);  break;
      case 8:  fk_putp(ratt_bl);  break;
      case 16:  fk_putp(ratt_dm);  break;
      case 32:  fk_putp(ratt_bo);  break;
   }
   switch(a)
   {  case 0:  fk_putp(att_no);  break;
      case 1:  fk_putp(att_so);  break;
      case 2:  fk_putp(att_ul);  break;
      case 4:  fk_putp(att_rv);  break;
      case 8:  fk_putp(att_bl);  break;
      case 16:  fk_putp(att_dm);  break;
      case 32:  fk_putp(att_bo);  break;
   }
   return(cur_attr = a);
}

void fk_colset(c)
     int c;
{
   int bg;
   if(cur_attr == c) return;
   cur_attr = c;
   bg = c / 16;
#ifdef TERMCAP
   if((c %= 16) >= col_num)
   {  fk_putp(att_bo);
      e_putp(tgoto(col_fg, 0, c % col_num));
      e_putp(tgoto(col_bg, 0, bg));
   }
   else
   {  fk_putp(ratt_bo);
      e_putp(tgoto(col_fg, 0, c));
      e_putp(tgoto(col_bg, 0, bg));
   }
#else
   if((c %= 16) >= col_num)
   {  fk_putp(att_bo);
      e_putp(tparm(col_fg, c % col_num));
      e_putp(tparm(col_bg, bg));
   }
   else
   {  fk_putp(ratt_bo);
      e_putp(tparm(col_fg, c));
      e_putp(tparm(col_bg, bg));
   }
#endif
}

int e_t_refresh()
{
   int x = cur_x, y = cur_y, i, j, c;
   fk_cursor(0);
   for(i = 0; i < MAXSLNS; i++)
   for(j = 0; j < MAXSCOL; j++)
   {  if(i == MAXSLNS-1 && j == MAXSCOL-1) break;
      if(*(schirm + 2*MAXSCOL * i + 2 * j) != *(altschirm + 2*MAXSCOL * i + 2 * j) ||
         *(schirm + 2*MAXSCOL * i + 2 * j + 1) != *(altschirm + 2*MAXSCOL * i + 2 * j + 1) )
#ifndef TERMCAP
      {  if(cur_x != j || cur_y != i) e_putp(tparm(cur_rc, i, j));
#else
      {  if(cur_x != j || cur_y != i) e_putp(tgoto(cur_rc, j, i));
#endif
	 if(cur_x < MAXSCOL)
	 {  cur_x = j + 1;  cur_y = i;  }
	 else
	 {  cur_x = 0;  cur_y = i+1;  }
	 if(col_num <= 0) fk_attrset(e_gt_col(j, i));
	 else fk_colset(e_gt_col(j, i));
         if((c = e_gt_char(j, i)) < NSPCHR) e_putp(sp_chr[c]);
         else fputc(e_gt_char(j, i), stdout);
	 *(altschirm + 2*MAXSCOL * i + 2 * j) = *(schirm + 2*MAXSCOL * i + 2 * j);
	 *(altschirm + 2*MAXSCOL * i + 2 * j + 1) = *(schirm + 2*MAXSCOL * i + 2 * j + 1);
      }
   }
   fk_cursor(1);
   fk_locate(x, y);
   return(0);
}

int e_t_sys_ini()
{
   e_refresh();
   ioctl(0, TCGETA, &ttermio);
   svflgs = fcntl( 0, F_GETFL, 0 );
   e_endwin();
   return(0);   
}

int e_t_sys_end()
{
   ioctl(0, TCSETA, &ttermio);
   fcntl( 0, F_SETFL, svflgs );
   e_abs_refr();
   fk_locate(0, 0);
   return(0);
}

int e_t_kbhit()
{
   int ret;
   char kbdflgs, c;
   e_refresh();
   kbdflgs = fcntl(0, F_GETFL, 0 );
   fcntl( 0, F_SETFL, kbdflgs | O_NONBLOCK);
   ret = read(0, &c, 1);
   fcntl(0, F_SETFL, kbdflgs & ~O_NONBLOCK);
   return(ret == 1 ? c : 0);
}

extern char *key_key, *key_f[];

int e_t_getch()
{  int c, c2;
   e_refresh();
   if((c =fk_getch()) != ESC)
   {  if(key_f[20] && c == *key_f[20]) return(DC);
      else if(key_f[17] && c == *key_f[17]) return(ENTF);
      else return(c);
   }
   else if((c = fk_getch()) == CR) return(ESC);
   if(c == ESC)
   {  if((c = fk_getch()) == ESC) return(ESC);
      if((c2 = e_find_key(c, 1, 1))) return (c2);
   }
   if((c2 = e_find_key((char)c, 1, 0))) return (c2);
   return(e_tast_sim(c));
}

char e_key_save[10];

int e_find_key(c, j, sw)
     int c;
     int j;
     int sw;
{
   int i, k;
   e_key_save[j] = c;
   e_key_save[j+1] = '\0';
   for(i = 0; i < KEYFN; i++)
   {  if(key_f[i] == NULL) continue;
      for(k = 1; k <= j && e_key_save[k] == *(key_f[i] + k); k++);
      if(k > j)
      {  if(*(key_f[i] + k) != '\0') return(e_find_key(fk_getch(), j+1, sw));
	 else if(sw)
	 {  switch(i)
	    {  case 0:  return(AF1);
	       case 1:  return(AF2);
	       case 2:  return(AF3);
	       case 3:  return(AF4);
	       case 4:  return(AF5);
	       case 5:  return(AF6);
	       case 6:  return(AF7);
	       case 7:  return(AF8);
	       case 8:  return(AF9);
	       case 9:  return(AF10);
	       case 10:  return(BUP);
	       case 11:  return(BDO);
	       case 12:  return(CCLE);
	       case 13:  return(CCRI);
	       case 14:  return(EINFG);
	       case 15:  return(CPS1);
	       case 16:  return(CBUP);
	       case 17:  return(ENTF);
	       case 18:  return(CEND);
	       case 19:  return(CBDO);
	       case 20:  return(AltBS);
	       case 21:  return(AF1);
	       case 22:  return(CEND);
	       case 33:  return(BUP+512);
	       case 34:  return(BDO+512);
	       case 35:  return(CCLE+512);
	       case 36:  return(CCRI+512);
	       case 37:  return(CPS1+512);
	       case 38:  return(CBUP+512);
	       case 39:  return(CEND+512);
	       case 40:  return(CBDO+512);
	       case 41:  return(CEND);
	       default:  return(0);
	    }
	 }
	 else
	 {  switch(i)
	    {  case 0:  return(F1);
	       case 1:  return(F2);
	       case 2:  return(F3);
	       case 3:  return(F4);
	       case 4:  return(F5);
	       case 5:  return(F6);
	       case 6:  return(F7);
	       case 7:  return(F8);
	       case 8:  return(F9);
	       case 9:  return(F10);
	       case 10:  return(CUP);
	       case 11:  return(CDO);
	       case 12:  return(CLE);
	       case 13:  return(CRI);
	       case 14:  return(EINFG);
	       case 15:  return(POS1);
	       case 16:  return(BUP);
	       case 17:  return(ENTF);
	       case 18:  return(ENDE);
	       case 19:  return(BDO);
	       case 20:  return(DC);
	       case 21:  return(HELP);
	       case 22:  return(ENDE);
	       case 23:  return(SF1);
	       case 24:  return(SF2);
	       case 25:  return(SF3);
	       case 26:  return(SF4);
	       case 27:  return(SF5);
	       case 28:  return(SF6);
	       case 29:  return(SF7);
	       case 30:  return(SF8);
	       case 31:  return(SF9);
	       case 32:  return(SF10);
	       case 33:  return(CUP+512);
	       case 34:  return(CDO+512);
	       case 35:  return(CLE+512);
	       case 36:  return(CRI+512);
	       case 37:  return(POS1+512);
	       case 38:  return(BUP+512);
	       case 39:  return(ENDE+512);
	       case 40:  return(BDO+512);
	       case 41:  return(ENDE);
	       default:  return(0);
	    }
	 }
      }
   }
   return(0);
}

int fk_t_locate(x, y)
     int x;
     int y;
{
   if(col_num > 0) 
   {   fk_colset(e_gt_col(cur_x, cur_y));
       fputc(e_gt_char(cur_x, cur_y), stdout);
   }
   cur_x = x;
   cur_y = y;
#ifndef TERMCAP
   e_putp(tparm(cur_rc, y, x));
#else
   e_putp(tgoto(cur_rc, x, y));
#endif
   fflush(stdout);
   return(y);
}

int fk_t_mouse(g)
     int *g;
{
   return(0);
}

#undef exit
int e_exitm(s, n)
     char *s;
     int n;
{
   e_endwin();
   if(n != 0) printf("\n%s\n", s);
   exit(n);
}

int e_switch_screen(sw)
     int sw;
{
   static int sav_sw = -1;
   if((e_we_sw & 1) || sw == sav_sw) return(0);
   sav_sw = sw;
   if(sw && beg_scr) 
   {  fflush(stdout);
      if(sav_cur) e_putp(sav_cur);
      e_putp(beg_scr);
/*      fflush(stdout);    */
   }
   else if(!sw && swt_scr)
   {  e_putp(swt_scr);
      if(res_cur) e_putp(res_cur);
      fflush(stdout);
   }
   else return(-1);
   return(0);
}

int e_deb_out(f)
     FENSTER *f;
{
   if(!swt_scr || !beg_scr)
      return(e_error("Your terminal don\'t use begin/end cup", 0, f->fb));
   e_d_switch_out(1);
   getchar();
   e_d_switch_out(0);
   return(0);
}

int e_d_switch_screen(sw)
     int sw;
{
   if(e_we_sw & 1) return(0);
#ifdef DEBUGGER
   if(!sw) e_g_sys_ini();
   else e_g_sys_end();
#endif
   return(e_switch_screen(sw));
}

int e_d_switch_out(sw)
     int sw;
{
   int i, j;
   static int save_sw = 32000;
   if(e_we_sw & 1) return(sw);
   if(save_sw == sw) return(0);
   save_sw = sw;
/*
   if(e_d_out == NULL)
   {  e_d_out = MALLOC(MAXOUT + 1);
      e_d_out_cur = 0;
   }
*/
   if(sw && e_d_switch_screen(0))
#ifndef TERMCAP
   {  e_putp(tparm(cur_rc, 0, 0));
#else
   {  e_putp(tgoto(cur_rc, 0, 0));
#endif
      e_putp(att_no);
      for(i = 0; i < MAXSLNS; i++)
	 for(j = 0; j < MAXSCOL; j++)
	 e_d_putchar(' ');
#ifndef TERMCAP
      e_putp(tparm(cur_rc, 0, 0));
#else
      e_putp(tgoto(cur_rc, 0, 0));
#endif
/*
      for(i = 0; i < e_d_out_cur; i++)
      {  if(e_d_out[i] == '\n')  e_d_putchar(CR);
	 e_d_putchar(e_d_out[i] & 255);
      }
*/
      fflush(stdout);
   }
   else if(!sw)
   {  e_d_switch_screen(1);
      e_abs_refr();
      e_refresh();
   }
   return(sw);
}
#endif  /*  is not DJGPP  */

int e_s_sys_ini()
{
   e_sys_ini();
   return(e_switch_screen(0));
}

int e_s_sys_end()
{
   e_switch_screen(1);
   return(e_sys_end());
}

#endif  /*  Is UNIX       */


