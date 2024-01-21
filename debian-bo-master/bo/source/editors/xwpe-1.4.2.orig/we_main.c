/* we_main.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

/*
                       Window - Editor (WE)
                           Version 1.0
                                       Copyright by F.Kruse       */

#include "edit.h"
#include <signal.h>
#ifdef UNIX
#include "attrb.h"
#include <sys/types.h>
#include <sys/stat.h>
#endif

int fk__cursor = 0;
#if MOUSE
int fk__mouse_stat = 1;
struct mouse e_mouse = {  0, 0, 0  };
#endif
#ifdef DOS
char far *schirm = (char far *)0xb8000000;
#endif
struct EXT uhr, h_error;
#ifdef DJGPP
#include<dpmi.h>
int e_ctrl_break();
int e_d_handler(_go32_dpmi_registers *r);
int harderr(int (*hnd)(_go32_dpmi_registers *r));
#endif
#ifndef DJGPP
extern FARBE *u_fb, *x_fb;
#endif
extern char *info_file;
extern char *e_tmp_dir;
int e_switch_screen();
int e_read_help_str();

char *e_msg[] = {  "Not Enough Memory",
		   "Option-File got the Wrong Version",
		   "Can\'t read Option-File",
#ifndef UNIX
		   "More Than Nine Windows",
#else
		   "More Than 35 Windows",
#endif
		   "Read Error in Swap-File",
		   "String NOT found",      /*  Number 5  */
		   "Can\'t open File %s",
		   "\nStrike Any Key: \n",
		   "More than nine Windows",
		   "Can\'t close File",
		   "No more Undo",       /*   Number 10   */
		   "Execute Command: %s\n",
		   "Command not found",
		   "\nStrike <Return> to continue\n",
		   "Not Installed",
		   "Can\'t open Option-File",  /*  Number 15   */
		   "Can\'t write to Option-File",
		   "Write Error in Swap-File",
		   "Internal Swap-Error",
		   "Can\'t open Swap-File",
		   "File saved",              /*  Number 20   */
		   "All Files saved",
		   "Can\'t Print File",
		   "No more Redo"
		};

OPT opt[] = {  "#",        3, '#', AltSYS,
	       "File",    10, 'F', AltF,
	       "Edit",    19, 'E', AltE,
	       "Search",  28, 'S', AltS,
	       "Block",   39, 'B', AltB,
	       "Options", 49, 'O', AltO,
	       "Window",  61, 'W', AltW,
	       "Help",    72, 'H', AltH
#ifdef PROG
	       , NULL,  0, 0, 0
	       , NULL,  0, 0, 0
#endif
#ifdef DEBUGGER
	       , NULL, 0, 0, 0
#endif
	    };

WOPT eblst_o[] = {  "F1 Help",  0, 0, 2, F1,
		    "F2 Save",  9, 0, 2, F2,
		    "F3 Files", 18, 0, 2,  F3,
		    "Alt-F3 Close W.", 28, 0, 6, AF3,
		    "F4 Search", 45, 0, 2, F4,
		    "^L S.Again", 56, 0, 2, CF4,
		    "Alt-X Quit",  68, 0, 5, AltX  };

WOPT eblst_u[] = {  "F1 Help",  0, 0, 2, F1,
		    "Alt-F2 Save",  9, 0, 6, AF2,
		    "F2 Files", 22, 0, 2,  F2,
		    "^F4 Close ", 32, 0, 3, CF4,
		    "Alt-F3 Srch", 44, 0, 6, AF3,
		    "F3 S.Ag.", 57, 0, 2, F3,
		    "Alt-F4 Quit",  67, 0, 6, AF4  };

WOPT fblst_o[] = {  "F1 Help",  0, 0, 2, F1,
		    "Edit",  9, 0, 1, AltE,
		    "RePlace", 15, 2, 1,  AltP,
		    "Move", 24, 0, 1, AltM,
		    "DUplicate", 30, 1, 1, AltU,
		    "Remove", 41, 0, 1, AltR,
		    "Alt-F3 Close W.", 49, 0, 6, AF3,
		    "Alt-X Quit",  66, 0, 5, AltX  };

WOPT fblst_u[] = {  "F1 Help",  0, 0, 2, F1,
		    "Edit",  9, 0, 1, AltE,
		    "RePlace", 15, 2, 1,  AltP,
		    "Move", 24, 0, 1, AltM,
		    "DUplicate", 30, 1, 1, AltU,
		    "Remove", 41, 0, 1, AltR,
		    "^F4 Close W.", 49, 0, 3, CF4,
		    "Alt-F4 Quit",  63, 0, 6, AF4  };

WOPT mblst_o[] = {  "F1 Help",  0, 0, 2, F1,
		    "PreV. Err.",  9, 3, 1, AltV,
		    "NexT Err.", 21, 3, 1,  AltT,
		    "Compile", 32, 0, 1, AltC,
		    "Make", 41, 0, 1, AltM,
		    "RUn", 47, 1, 1, AltU,
		    "Alt-F3 Close", 52, 0, 6, AF3,
		    "Alt-X Quit",  66, 0, 5, AltX  };

WOPT mblst_u[] = {  "F1 Help",  0, 0, 2, F1,
		    "PreV. Err.",  9, 3, 1, AltV,
		    "NexT Err.", 21, 3, 1,  AltT,
		    "Compile", 32, 0, 1, AltC,
		    "Make", 41, 0, 1, AltM,
		    "RUn", 47, 1, 1, AltU,
		    "^F4 Close", 52, 0, 3, CF4,
		    "Alt-F4 Quit",  63, 0, 6, AF4  };

WOPT dblst_o[] = {  "F1 Help",  0, 0, 2, F1,
		    "^F7 Make Watch",  9, 0, 3, CF7,
		    "F7 Step", 25, 0, 2,  F7,
		    "F8 Next", 34, 0, 2, F8,
		    "^F10 Run", 43, 0, 4, F10,
		    "Alt-F3 Close", 53, 0, 6, AF3,
		    "Alt-X Quit",  67, 0, 5, AltX  };

WOPT dblst_u[] = {  "F1 Help",  0, 0, 2, F1,
		    "^F5 Make Watch",  9, 0, 3, CF7,
		    "F7 Step", 25, 0, 2,  F7,
		    "F8 Next", 34, 0, 2, F8,
		    "^F10 Run", 43, 0, 4, F10,
		    "^F4 Close", 53, 0, 3, CF4,
		    "Alt-F4 Quit",  64, 0, 6, AF4  };

WOPT xblst_o[] = {  "F1 Help",  0, 0, 2, F1,
		    "Execute",  19, 0, 1, AltE,
		    "Alt-F3 Close W.", 38, 0, 6, AF3,
		    "Alt-X Quit",  66, 0, 5, AltX  };

WOPT xblst_u[] = {  "F1 Help",  0, 0, 2, F1,
		    "Execute",  19, 0, 1, AltE,
		    "^F4 Close W.", 38, 0, 3, CF4,
		    "Alt-F4 Quit",  63, 0, 6, AF4  };

WOPT wblst_o[] = {  "F1 Help",  0, 0, 2, F1,
		    "Write",  19, 0, 1, AltW,
		    "Alt-F3 Close W.", 38, 0, 6, AF3,
		    "Alt-X Quit",  66, 0, 5, AltX  };

WOPT wblst_u[] = {  "F1 Help",  0, 0, 2, F1,
		    "Write",  19, 0, 1, AltW,
		    "^F4 Close W.", 38, 0, 3, CF4,
		    "Alt-F4 Quit",  63, 0, 6, AF4  };

WOPT rblst_o[] = {  "F1 Help",  0, 0, 2, F1,
		    "Read",  19, 0, 1, AltR,
		    "Alt-F3 Close W.", 38, 0, 6, AF3,
		    "Alt-X Quit",  66, 0, 5, AltX  };

WOPT rblst_u[] = {  "F1 Help",  0, 0, 2, F1,
		    "Read",  19, 0, 1, AltR,
		    "^F4 Close W.", 38, 0, 3, CF4,
		    "Alt-F4 Quit",  63, 0, 6, AF4  };

WOPT ablst_o[] = {  "F1 Help",  0, 0, 2, F1,
		    "Add",  19, 0, 1, AltA,
		    "Alt-F3 Close W.", 38, 0, 6, AF3,
		    "Alt-X Quit",  66, 0, 5, AltX  };

WOPT ablst_u[] = {  "F1 Help",  0, 0, 2, F1,
		    "Add",  19, 0, 1, AltA,
		    "^F4 Close W.", 38, 0, 3, CF4,
		    "Alt-F4 Quit",  63, 0, 6, AF4  };

WOPT sblst_o[] = {  "F1 Help",  0, 0, 2, F1,
		    "Save",  14, 0, 1, AltS,
		    "Save CrYpt",  25, 7, 1, AltY,
		    "Alt-F3 Close W.", 42, 0, 6, AF3,
		    "Alt-X Quit",  66, 0, 5, AltX  };

WOPT sblst_u[] = {  "F1 Help",  0, 0, 2, F1,
		    "Save",  14, 0, 1, AltS,
		    "Save CrYpt",  25, 7, 1, AltY,
		    "^F4 Close W.", 42, 0, 3, CF4,
		    "Alt-F4 Quit",  66, 0, 6, AF4  };

WOPT hblst_o[] = {  "F1 Help",  0, 0, 2, F1,
		    "<BSP> Back",  9, 0, 5, DC,
		    "<CR> Goto",  21, 0, 4, CR,
		    " PreVious",  32, 4, 1, AltV,
		    " NexT",  45, 4, 1, AltT,
		    "Alt-F3 Close", 54, 0, 6, AF3,
		    "Alt-X Quit",  68, 0, 5, AltX  };

WOPT hblst_u[] = {  "F1 Help",  0, 0, 2, F1,
		    "<BSP> Back",  9, 0, 5, DC,
		    "<CR> Goto",  21, 0, 4, CR,
		    " PreVious",  32, 4, 1, AltV,
		    " NexT",  45, 4, 1, AltT,
		    "^F4 Close", 54, 0, 3, CF4,
		    "Alt-F4 Quit",  65, 0, 6, AF4  };

WOPT gblst_o[] = {  "F1 Help",  0, 0, 2, F1,
		    "Show",  19, 0, 1, AltS,
		    "Alt-F3 Close W.", 38, 0, 6, AF3,
		    "Alt-X Quit",  66, 0, 5, AltX  };

WOPT gblst_u[] = {  "F1 Help",  0, 0, 2, F1,
		    "Show",  19, 0, 1, AltS,
		    "^F4 Close W.", 38, 0, 3, CF4,
		    "Alt-F4 Quit",  63, 0, 6, AF4  };

WOPT oblst_o[] = {  "F1 Help",  0, 0, 2, F1,
		    "Add",  14, 0, 1, AltA,
		    "Delete",  25, 0, 1, AltD,
		    "Alt-F3 Close W.", 42, 0, 6, AF3,
		    "Alt-X Quit",  66, 0, 5, AltX  };

WOPT oblst_u[] = {  "F1 Help",  0, 0, 2, F1,
		    "Add",  14, 0, 1, AltA,
		    "Delete",  25, 0, 1, AltD,
		    "^F4 Close W.", 42, 0, 3, CF4,
		    "Alt-F4 Quit",  63, 0, 6, AF4  };

WOPT *eblst, *fblst, *mblst, *dblst, *xblst, *wblst, *rblst;
WOPT *ablst, *sblst, *hblst, *gblst, *oblst;

extern char *e_hlp_str[];

char *e_hlp, *user_shell;
WOPT *blst;
int nblst = 7;

#ifdef UNIX
int e_term_def = 0;
#endif

char *e_k_key = NULL;
extern int col_num;

/*
#include <termio.h>
struct termio tsttermio;
*/

void main(argc, argv)
     int argc;
     char **argv;
{
   FARBE *fb;
   ECNT *cn;
   int i, err = 0, g[4];
   int so = 0, sf = 0, sd = 1;
   char *tp;
/*
    i = ioctl(0, TCGETA, &tsttermio);
    printf("TCGETA = 0%o\n", TCGETA);
    getchar();
    if(i)
    {	printf("Error in Terminal Initialisation (1) Code: %d\n", i);
    	printf("c_iflag = %o, c_oflag = %o, c_cflag = %o,\n",
		tsttermio.c_iflag, tsttermio.c_oflag, tsttermio.c_cflag);
    	printf("c_lflag = %o, c_line = %o, c_cc = {\"\\%o\\%o\\%o\\%o\\%o\\%o\\%o\\%o\"}\n",
		tsttermio.c_lflag, tsttermio.c_line, tsttermio.c_cc[0], tsttermio.c_cc[1], 
		tsttermio.c_cc[2], tsttermio.c_cc[3], tsttermio.c_cc[4], tsttermio.c_cc[5], 
		tsttermio.c_cc[6], tsttermio.c_cc[7]);
	exit(1);
    }
*/
#ifdef UNIX
/*
    signal(SIGSEGV, e_err_end);
    signal(SIGBUS, e_err_end);
*/
   for(i = 0; i < 12; i++) signal(i, e_err_end);
   e_ini_unix(argc, argv);
   e_switch_screen(1);
#endif
   signal(SIGINT, e_int_hnd);
#ifdef DJGPP
   e_ctrl_break();
   harderr(e_d_handler);
#endif
#ifdef DJGPP
   if( (fb = (FARBE *) MALLOC(sizeof(FARBE))) == NULL)
   e_error(e_msg[0], 2, fb);
#else
   if(e_we_sw & 1) fb = x_fb;
   else	     fb = u_fb;
#endif
   e_ini_farbe(fb, 0);
   if( (cn = (ECNT *) MALLOC(sizeof(ECNT))) == NULL)
   e_error(e_msg[0], 2, fb);
   cn->mxedt = -1;
   cn->curedt = 0;
   cn->edt[0] = 0;
   cn->fb = fb;
   cn->dirct = MALLOC(80*sizeof(char));
   getcwd(cn->dirct, 80);
   strcpy(cn->fd.search, "");
   strcpy(cn->fd.replace, "");
   strcpy(cn->fd.file, SUDIR);
   strcpy(cn->fd.dirct, cn->dirct);
   cn->fd.sw = 16;
   cn->fd.sn = 0;
   cn->fd.rn = 0;
   cn->sdf = cn->rdf = cn->fdf = cn->ddf = cn->wdf = cn->hdf = cn->shdf = NULL;
   uhr.cn = h_error.cn = cn;
   cn->edtdrct = e_getedpath(argv[0]);
   cn->libdrct = MALLOC(((i=strlen(cn->edtdrct))
			+strlen(WPE_LIB_DIR)+1) * sizeof(char));
   strcpy(cn->libdrct, cn->edtdrct);
#ifdef DJGPP
   for(; i >= 0 && cn->edtdrct[i] != DIRC && cn->edtdrct[i] != '/'; i--);
#else
   for(; i >= 0 && cn->edtdrct[i] != DIRC; i--);
#endif
   cn->libdrct[i+1] = '\0';
   strcat(cn->libdrct, WPE_LIB_DIR);
   
                       /*   Standard Einstellungen    */
   cn->dtmd = 'n';
   cn->autosv = 0;
   cn->prnt= 0;
   cn->maxchg = 999;
   cn->numundo = 10;
   cn->maxcol = MAXCOLUM;
   cn->tabn = 8;
#ifdef DOS
   cn->tabs = MALLOC((cn->tabn+1)*sizeof(char));
   cn->uhr = 1;
#else
   cn->flopt = 010150;
#ifdef CUAKEYS
   cn->edopt = 153;
#else
   cn->edopt = 152;
#endif
   cn->tabs = MALLOC((cn->tabn+1)*sizeof(unsigned));
#endif
   info_file = MALLOC((strlen(INFO_FILE)+1)*sizeof(char));
   strcpy(info_file, INFO_FILE);
#ifdef SWAP
   e_swap_init(cn);
#endif
   strcpy(cn->tabs,"        ");
   e_read_help_str(cn);
   e_hlp = e_hlp_str[0];
#ifdef DJGPP
   if(!(user_shell = getenv("COMSPEC"))) user_shell = "command.com";
#else
   if(!(user_shell = getenv("SHELL"))) user_shell = DEF_SHELL;
#endif
   e_tmp_dir = MALLOC(128);
   sprintf(e_tmp_dir, "/tmp/we_%u", (unsigned) getpid());
   e_tmp_dir = REALLOC(e_tmp_dir, (strlen(e_tmp_dir)+1)*sizeof(char));
   mkdir(e_tmp_dir, 0700);
   e_edit(cn, "");     /*       Clipboard         */
   
   for(i = 1; i < argc; i++)
   {  if(*argv[i] == '-')
      {  if(*(argv[i]+1) == 's' && *(argv[i]+2) == 'o') so = 1;
	 else if(*(argv[i]+1) == 's' && *(argv[i]+2) == 'c') sf = 1;
	 else if(*(argv[i]+1) == 'o' && *(argv[i]+2) == 'f')
	 {  sd = 0;
	    cn->optfile = MALLOC((strlen(argv[i+1])+1)*sizeof(char));
	    strcpy(cn->optfile, argv[i+1]);
	 }
#ifdef UNIX
	 else if(!strcmp(argv[i], "-vttoat")) e_term_def = 1;
#endif
      }
   }
#ifdef SWAP
#ifdef UNIX
   cn->swpfile = e_mkfilename("/tmp", ".WE_SWP_1.SWP");
   cn->swp2file = e_mkfilename("/tmp", ".WE_SWP_2.SWP");
#else
   cn->swpfile = e_mkfilename(cn->edtdrct, "WE_SWP_1.SWP");
   cn->swp2file = e_mkfilename(cn->edtdrct, "WE_SWP_2.SWP");
#endif
#endif
#ifdef PROG
/*     if(e_we_sw & 2) */  e_ini_prog(cn);
#endif
#ifndef DJGPP
   if(sd != 0)  cn->optfile = e_mkfilename(getenv("HOME"), OPTFILE);
#else
   if(sd != 0)  cn->optfile = e_mkfilename(cn->libdrct, OPTFILE);
/*
   if(sd != 0)
   {  char *etmp = getenv("COMPILER_PATH");
      if(!etmp) cn->optfile = e_mkfilename("c:\\", OPTFILE);
      else cn->optfile = e_mkfilename(etmp, OPTFILE);
   }
*/
#endif
   if(so == 0)  err = e_opt_read(cn->f[cn->mxedt], sf);
#ifdef UNIX
   argc = e_ini_schirm(argc, argv);
#else
   uhr.sw = cn->uhr;
   e_ini_schirm();
#endif
   if((tp = getenv("INFOPATH")) != NULL)
            info_file = e_make_string(info_file, tp);
   if(cn->edopt & 1) blst = eblst_u;
   else blst = eblst_o;
   e_ini_desk(cn);
   cn->f[0]->blst = eblst;
#if MOUSE
#ifdef DJGPP
   e_refresh();
#endif
   g[0] = 4; g[2] = 0; g[3] = 0;
   fk_mouse(g);
   g[0] = 1;
   fk_mouse(g);
#endif
   if(err < 0) e_error(e_msg[-err], 0, cn->fb);
   if(e_we_sw & 2) e_read_pr_opt(cn);
#ifdef UNIX
   for(i = 1; i < argc; i++) if(!strcmp(argv[i], "-r")) e_recover(cn);
#endif
   for(i = 1; i < argc; i++)
   {  if(*argv[i] == '-')
      {  if(*(argv[i]+1) == 'm') cn->dtmd = *(argv[i]+2);
	 if(*(argv[i]+1) == 'n')
	 {  if(*(argv[i]+2) == 'c') cn->maxcol = atoi(argv[i+1]);
	    else if(*(argv[i]+2) == 'u') cn->numundo = atoi(argv[i+1]);
	    else if(*(argv[i]+2) == 'm') cn->maxchg = atoi(argv[i+1]);
	    else i--;
	    i++;
	 }
	 else if(*(argv[i]+1) == 'o' && *(argv[i]+2) == 'f') i++;
#ifdef UNIX
#ifdef PROG
	 else if(*(argv[i]+1) == 'p' && *(argv[i]+2) == 'm') e_we_sw |= 8;
	 else if(*(argv[i]+1) == 'p' && *(argv[i]+2) == 't')
	 e_s_prog.comp_sw = 1;
	 else if(*(argv[i]+1) == 'p' && *(argv[i]+2) == 'e')
	 e_s_prog.comp_sw = 2;
#endif
	 else if(*(argv[i]+1) == 'u' && *(argv[i]+2) == 'm') cn->dtmd = 'm';
#endif
	 else if(*(argv[i]+1) == 'k' && *(argv[i]+2) == '\0')
	 {  cn->dtmd = 'k';  e_k_key = argv[i+1];  i++;  }
	 continue;
      }
      else  e_edit(cn, argv[i]);
   }
#ifdef DOS
   ctrlbrk(e_ctrl_break);
   if(cn->mxedt == 0) e_edit(cn, "");
   do
   {
      i = e_eingabe(cn);
      if(i == AltX) i = e_quit(cn->f[cn->mxedt]);
   }
   while ( i != AltX );
   exit(0);
}
#else
   if(cn->mxedt == 0) e_manager(cn->f[cn->mxedt]);
   do
   {
      if(cn->f[cn->mxedt]->dtmd == 'F') i = e_file_eingabe(cn);
      else if(cn->f[cn->mxedt]->dtmd == 'D') i = e_data_eingabe(cn);
      else i = e_eingabe(cn);
      if(i == AltX) i = e_quit(cn->f[cn->mxedt]);
   }
   while ( i != AltX );
   exit(0);
}
#endif

int e_switch_blst(cn)
     ECNT *cn;
{
   int i;
   FENSTER *f;
   if(cn->edopt & 1)
   {  for(i = 0; i <= cn->mxedt; i++)
      {  f = cn->f[i];
	 if(f->blst == eblst_o) f->blst = eblst_u;
	 else if(f->blst == fblst_o) f->blst= fblst_u;
	 else if(f->blst == mblst_o) f->blst= mblst_u;
	 else if(f->blst == dblst_o) f->blst= dblst_u;
	 else if(f->blst == xblst_o) f->blst= xblst_u;
	 else if(f->blst == wblst_o) f->blst= wblst_u;
	 else if(f->blst == rblst_o) f->blst= rblst_u;
	 else if(f->blst == ablst_o) f->blst= ablst_u;
	 else if(f->blst == sblst_o) f->blst= sblst_u;
	 else if(f->blst == hblst_o) f->blst= hblst_u;
	 else if(f->blst == gblst_o) f->blst= gblst_u;
	 else if(f->blst == oblst_o) f->blst= oblst_u;
      }
   }
   else
   {  for(i = 0; i <= cn->mxedt; i++)
      {  f = cn->f[i];
	 if(f->blst == eblst_u) f->blst= eblst_o;
	 else if(f->blst == fblst_u) f->blst= fblst_o;
	 else if(f->blst == mblst_u) f->blst= mblst_o;
	 else if(f->blst == dblst_u) f->blst= dblst_o;
	 else if(f->blst == xblst_u) f->blst= xblst_o;
	 else if(f->blst == wblst_u) f->blst= wblst_o;
	 else if(f->blst == rblst_u) f->blst= rblst_o;
	 else if(f->blst == ablst_u) f->blst= ablst_o;
	 else if(f->blst == sblst_u) f->blst= sblst_o;
	 else if(f->blst == hblst_u) f->blst= hblst_o;
	 else if(f->blst == gblst_u) f->blst= gblst_o;
	 else if(f->blst == oblst_u) f->blst= oblst_o;
      }
   }
   return(0);
}

void e_ini_desk(cn)
     ECNT *cn;
{
   extern int e_mn_men;
   int i;
   if(cn->edopt & 1)
   {  eblst = eblst_u; fblst = fblst_u; mblst = mblst_u; dblst = dblst_u;
      xblst = xblst_u; wblst = wblst_u; rblst = rblst_u; ablst = ablst_u;
      sblst = sblst_u; hblst = hblst_u; gblst = gblst_u; oblst = oblst_u;
   }
   else
   {  eblst = eblst_o; fblst = fblst_o; mblst = mblst_o; dblst = dblst_o;
      xblst = xblst_o; wblst = wblst_o; rblst = rblst_o; ablst = ablst_o;
      sblst = sblst_o; hblst = hblst_o; gblst = gblst_o; oblst = oblst_o;
   }
   e_cls(cn->fb->df.fb, cn->fb->dc);
   e_blk(MAXSCOL, 0, 0, cn->fb->mt.fb);
   for (i = 0; i < MENOPT; ++i)
   {  e_pr_str_wsd(opt[i].x, 0, opt[i].t, cn->fb->mt.fb, 0, 1,
		cn->fb->ms.fb, ( i == 0 ? 0 : opt[i].x-e_mn_men),
		(i == MENOPT-1) ? MAXSCOL-1 : opt[i+1].x-e_mn_men-1);
   }
   e_pr_uul(cn->fb);
}

void e_ini_farbe(fb, sw)
     FARBE *fb;
     int sw;
{
#if defined(DOS) || defined(DJGPP)
   fb->er = e_s_x_clr(15,1);   /*  Editoren Rahmen         */
   fb->et = e_s_x_clr(14,1);   /*  Editoren Text           */
   fb->ez = e_s_x_clr(1,7);    /*  Editoren Text markiert  */
   fb->es = e_s_x_clr(10,1);   /*  Editoren Fenster-Button */
   fb->ek = e_s_x_clr(14,3);   /*  Editoren Markierung (find) */
   fb->em = e_s_x_clr(1,3);    /*  Mausleiste              */
   fb->mr = e_s_x_clr(0,7);    /*  Menu Rahmen             */
   fb->mt = e_s_x_clr(0,7);    /*  Menu Text               */
   fb->mz = e_s_x_clr(0,2);    /*  Menu Text markiert      */
   fb->ms = e_s_x_clr(4,7);    /*  Menu-Schalter           */
   fb->nr = e_s_x_clr(15,7);   /*  Optionen Rahmen         */
   fb->nt = e_s_x_clr(0,7);    /*  Optionen Text           */
   fb->nsnt = e_s_x_clr(14,7); /*  Optionen Text-Schalter  */
   fb->ne = e_s_x_clr(10, 7);  /*  Optionen Fenster-Button */
   fb->fr = e_s_x_clr(15,1);   /*  Schreibleiste	passiv	  */
   fb->fa = e_s_x_clr(15,2);   /*  Schreibleiste	aktiv	  */
   fb->ft = e_s_x_clr(0,3);    /*  Daten Text		  */
   fb->fz = e_s_x_clr(15,2);   /*  Daten aktiv markiert	  */
   fb->frft = e_s_x_clr(15,3); /*  Daten passiv markiert	  */
   fb->fs = e_s_x_clr(0,3);    /*  Schalter Text		  */
   fb->nsft = e_s_x_clr(14,3); /*  Schalter Schalter	  */
   fb->fsm = e_s_x_clr(14,3);  /*  Schalter aktiv	  */
   fb->nz = e_s_x_clr(15,2);   /*  Button Text             */
   fb->ns = e_s_x_clr(14,2);   /*  Button Schalter         */
   fb->nm = e_s_x_clr(14,2);   /*  Button Text markiert    */
   fb->hh = e_s_x_clr(9,3);    /*  Help Header		  */
   fb->hb = e_s_x_clr(14,2);   /*  Help Button		  */
   fb->hm = e_s_x_clr(10,1);   /*  Help markiert		  */
   fb->df = e_s_x_clr(7,0);    /*  Hintergrund		  */
   fb->of = e_s_x_clr(8,0);    /*  leer			  */
   fb->db = e_s_x_clr(0,4);    /*  Breakpoint		  	  */
   fb->dy = e_s_x_clr(9,3);    /*  Debugger Stop		  */
   fb->ct = e_s_x_clr(14, 1);  /*  C-Prog. Text                 */
   fb->cr = e_s_x_clr(15, 1);  /*  C-Prog. res. Worte           */
   fb->ck = e_s_x_clr(11, 1);  /*  C-Prog. Konstanten           */
   fb->cp = e_s_x_clr(10, 1);  /*  C-Prog. Preprozessor         */
   fb->cc = e_s_x_clr( 7, 1);  /*  C-Prog. Kommentare           */
   fb->dc = 0xb0;
   fb->ws = 7;
#else
   if(!sw || (e_we_sw & 1))
   {
#ifdef NEWSTYLE
      
      x_fb->er = e_s_x_clr(15,7);   /*  Editoren Rahmen         */
      x_fb->et = e_s_x_clr(0,7);    /*  Editoren Text           */
      x_fb->ez = e_s_x_clr(0,3);    /*  Editoren Text markiert  */
      x_fb->es = e_s_x_clr(2,7);    /*  Editoren Fenster-Button */
      x_fb->ek = e_s_x_clr(9,7);    /*  Editoren Markierung (find) */
      x_fb->em = e_s_x_clr(1,3);    /*  Mausleiste              */
      x_fb->mr = e_s_x_clr(0,7);    /*  Menu Rahmen             */
      x_fb->mt = e_s_x_clr(0,7);    /*  Menu Text               */
      x_fb->mz = e_s_x_clr(7,6);    /*  Menu Text markiert      */
      x_fb->ms = e_s_x_clr(4,7);    /*  Menu-Schalter           */
      x_fb->nr = e_s_x_clr(15,7);   /*  Optionen Rahmen         */
      x_fb->nt = e_s_x_clr(0,7);    /*  Optionen Text           */
      x_fb->nsnt = e_s_x_clr(14,7); /*  Optionen Text-Schalter  */
      x_fb->ne = e_s_x_clr(4, 7);   /*  Optionen Fenster-Button */
      x_fb->fr = e_s_x_clr(15,1);   /*  Schreibleiste	passiv	  */
      x_fb->fa = e_s_x_clr(15,2);   /*  Schreibleiste	aktiv	  */
      x_fb->ft = e_s_x_clr(0,3);    /*  Daten Text		  */
      x_fb->fz = e_s_x_clr(15,2);   /*  Daten aktiv markiert	  */
      x_fb->frft = e_s_x_clr(15,3); /*  Daten passiv markiert	  */
      x_fb->fs = e_s_x_clr(0,3);    /*  Schalter Text		  */
      x_fb->nsft = e_s_x_clr(14,3); /*  Schalter Schalter	  */
      x_fb->fsm = e_s_x_clr(14,3);  /*  Schalter aktiv	  */
      x_fb->nz = e_s_x_clr(15,1);   /*  Button Text             */
      x_fb->ns = e_s_x_clr(11,1);   /*  Button Schalter         */
      x_fb->nm = e_s_x_clr(11,1);   /*  Button Text markiert    */
      x_fb->hh = e_s_x_clr(9,3);    /*  Help Header		  */
      x_fb->hb = e_s_x_clr(11,1);   /*  Help Button		  */
      x_fb->hm = e_s_x_clr(4,7);    /*  Help markiert		  */
      x_fb->df = e_s_x_clr(7,0);    /*  Hintergrund		  */
      x_fb->of = e_s_x_clr(8,0);    /*  leer			  */
      x_fb->db = e_s_x_clr(0,4);    /*  Breakpoint		  */
      x_fb->dy = e_s_x_clr(9,3);    /*  Debugger Stop		  */
      x_fb->ct = e_s_x_clr(0, 7);   /*  C-Prog. Text            */
      x_fb->cr = e_s_x_clr(1, 7);   /*  C-Prog. res. Worte      */
      x_fb->ck = e_s_x_clr(6, 7);   /*  C-Prog. Konstanten      */
      x_fb->cp = e_s_x_clr(2, 7);   /*  C-Prog. Preprozessor    */
      x_fb->cc = e_s_x_clr(8, 7);   /*  C-Prog. Kommentare      */
      x_fb->dc = 0x02;
      x_fb->ws = 7;
#else
      x_fb->er = e_s_x_clr(15,1);   /*  Editoren Rahmen         */
      x_fb->et = e_s_x_clr(14,1);   /*  Editoren Text           */
      x_fb->ez = e_s_x_clr(1,7);    /*  Editoren Text markiert  */
      x_fb->es = e_s_x_clr(10,1);   /*  Editoren Fenster-Button */
      x_fb->ek = e_s_x_clr(14,3);   /*  Editoren Markierung (find) */
      x_fb->em = e_s_x_clr(1,3);    /*  Mausleiste              */
      x_fb->mr = e_s_x_clr(0,7);    /*  Menu Rahmen             */
      x_fb->mt = e_s_x_clr(0,7);    /*  Menu Text               */
      x_fb->mz = e_s_x_clr(0,2);    /*  Menu Text markiert      */
      x_fb->ms = e_s_x_clr(4,7);    /*  Menu-Schalter           */
      x_fb->nr = e_s_x_clr(15,7);   /*  Optionen Rahmen         */
      x_fb->nt = e_s_x_clr(0,7);    /*  Optionen Text           */
      x_fb->nsnt = e_s_x_clr(14,7); /*  Optionen Text-Schalter  */
      x_fb->ne = e_s_x_clr(4, 7);   /*  Optionen Fenster-Button */
      x_fb->fr = e_s_x_clr(15,1);   /*  Schreibleiste	passiv	  */
      x_fb->fa = e_s_x_clr(15,2);   /*  Schreibleiste	aktiv	  */
      x_fb->ft = e_s_x_clr(0,3);    /*  Daten Text		  */
      x_fb->fz = e_s_x_clr(15,2);   /*  Daten aktiv markiert	  */
      x_fb->frft = e_s_x_clr(15,3); /*  Daten passiv markiert	  */
      x_fb->fs = e_s_x_clr(0,3);    /*  Schalter Text		  */
      x_fb->nsft = e_s_x_clr(14,3); /*  Schalter Schalter	  */
      x_fb->fsm = e_s_x_clr(14,3);  /*  Schalter aktiv	  */
      x_fb->nz = e_s_x_clr(15,2);   /*  Button Text             */
      x_fb->ns = e_s_x_clr(14,2);   /*  Button Schalter         */
      x_fb->nm = e_s_x_clr(14,2);   /*  Button Text markiert    */
      x_fb->hh = e_s_x_clr(9,3);    /*  Help Header		  */
      x_fb->hb = e_s_x_clr(14,2);   /*  Help Button		  */
      x_fb->hm = e_s_x_clr(10,1);   /*  Help markiert		  */
      x_fb->df = e_s_x_clr(7,0);    /*  Hintergrund		  */
      x_fb->of = e_s_x_clr(8,0);    /*  leer			  */
      x_fb->db = e_s_x_clr(0,4);    /*  Breakpoint		  */
      x_fb->dy = e_s_x_clr(9,3);    /*  Debugger Stop		  */
      x_fb->ct = e_s_x_clr(14, 1);  /*  C-Prog. Text            */
      x_fb->cr = e_s_x_clr(15, 1);  /*  C-Prog. res. Worte      */
      x_fb->ck = e_s_x_clr(11, 1);  /*  C-Prog. Konstanten      */
      x_fb->cp = e_s_x_clr(10, 1);  /*  C-Prog. Preprozessor    */
      x_fb->cc = e_s_x_clr( 7, 1);  /*  C-Prog. Kommentare      */
      x_fb->dc = 0x02;
      x_fb->ws = 7;
#endif
   }
   if(!sw || !(e_we_sw & 1))
   {  if(col_num)
      {  u_fb->er = e_s_x_clr(15,4);   /*  Editoren Rahmen         */
	 u_fb->et = e_s_x_clr(11,4);   /*  Editoren Text           */
	 u_fb->ez = e_s_x_clr(4,7);    /*  Editoren Text markiert  */
	 u_fb->es = e_s_x_clr(10,4);   /*  Editoren Fenster-Button */
	 u_fb->ek = e_s_x_clr(11,6);   /*  Editoren Markierung (find) */
	 u_fb->em = e_s_x_clr(4,6);    /*  Mausleiste              */
	 u_fb->mr = e_s_x_clr(0,7);    /*  Menu Rahmen             */
	 u_fb->mt = e_s_x_clr(0,7);    /*  Menu Text               */
	 u_fb->mz = e_s_x_clr(0,2);    /*  Menu Text markiert      */
	 u_fb->ms = e_s_x_clr(1,7);    /*  Menu-Schalter           */
	 u_fb->nr = e_s_x_clr(15,7);   /*  Optionen Rahmen         */
	 u_fb->nt = e_s_x_clr(0,7);    /*  Optionen Text           */
	 u_fb->nsnt = e_s_x_clr(11,7); /*  Optionen Text-Schalter  */
	 u_fb->ne = e_s_x_clr(1, 7);   /*  Optionen Fenster-Button */
	 u_fb->fr = e_s_x_clr(15,4);   /*  Schreibleiste	passiv	  */
	 u_fb->fa = e_s_x_clr(15,2);   /*  Schreibleiste	aktiv	  */
	 u_fb->ft = e_s_x_clr(0,6);    /*  Daten Text		  */
	 u_fb->fz = e_s_x_clr(15,2);   /*  Daten aktiv markiert	  */
	 u_fb->frft = e_s_x_clr(15,6); /*  Daten passiv markiert	  */
	 u_fb->fs = e_s_x_clr(0,6);    /*  Schalter Text		  */
	 u_fb->nsft = e_s_x_clr(14,6); /*  Schalter Schalter	  */
	 u_fb->fsm = e_s_x_clr(14,6);  /*  Schalter aktiv	  */
	 u_fb->nz = e_s_x_clr(15,2);   /*  Button Text             */
	 u_fb->ns = e_s_x_clr(11,2);   /*  Button Schalter         */
	 u_fb->nm = e_s_x_clr(11,2);   /*  Button Text markiert    */
	 u_fb->hh = e_s_x_clr(12,6);    /*  Help Header		  */
	 u_fb->hb = e_s_x_clr(11,2);   /*  Help Button		  */
	 u_fb->hm = e_s_x_clr(10,4);   /*  Help markiert		  */
	 u_fb->df = e_s_x_clr(7,0);    /*  Hintergrund		  */
	 u_fb->of = e_s_x_clr(8,0);    /*  leer			  */
	 u_fb->db = e_s_x_clr(0,1);    /*  Breakpoint		  */
	 u_fb->dy = e_s_x_clr(12,6);    /*  Debugger Stop		  */
	 u_fb->ct = e_s_x_clr(11, 4);  /*  C-Prog. Text            */
	 u_fb->cr = e_s_x_clr(15, 4);  /*  C-Prog. res. Worte      */
	 u_fb->ck = e_s_x_clr(14, 4);  /*  C-Prog. Konstanten      */
	 u_fb->cp = e_s_x_clr(10, 4);  /*  C-Prog. Preprozessor    */
	 u_fb->cc = e_s_x_clr( 7, 4);  /*  C-Prog. Kommentare      */
	 u_fb->dc = ' ';
	 u_fb->ws = 7;
      }
      else
      {  u_fb->er = e_n_t_clr(0);
	 u_fb->et = e_n_t_clr(0);
	 u_fb->ez = e_n_t_clr(A_REVERSE);
	 u_fb->es = e_n_t_clr(0);
	 u_fb->ek = e_n_t_clr(A_UNDERLINE);
	 u_fb->em = e_n_t_clr(A_STANDOUT);
	 u_fb->mr = e_n_t_clr(A_STANDOUT);
	 u_fb->mt = e_n_t_clr(A_STANDOUT);
	 u_fb->mz = e_n_t_clr(0);
	 u_fb->ms = e_n_t_clr(0);
	 u_fb->nr = e_n_t_clr(A_STANDOUT);
	 u_fb->nt = e_n_t_clr(A_REVERSE);
	 u_fb->nsnt = e_n_t_clr(A_BOLD);
	 u_fb->ne = e_n_t_clr(0);
	 u_fb->fr = e_n_t_clr(0);
	 u_fb->fa = e_n_t_clr(A_REVERSE);
	 u_fb->ft = e_n_t_clr(0);
	 u_fb->fz = e_n_t_clr(A_STANDOUT);
	 u_fb->frft = e_n_t_clr(0);
	 u_fb->fs = e_n_t_clr(0);
	 u_fb->nsft = e_n_t_clr(A_BOLD);
	 u_fb->fsm = e_n_t_clr(A_BOLD);
	 u_fb->nz = e_n_t_clr(0);
	 u_fb->ns = e_n_t_clr(A_BOLD);
	 u_fb->nm = e_n_t_clr(A_BOLD);
	 u_fb->hh = e_n_t_clr(A_REVERSE);
	 u_fb->hb = e_n_t_clr(A_REVERSE);
	 u_fb->hm = e_n_t_clr(A_BOLD);
	 u_fb->of = e_n_t_clr(A_STANDOUT);
	 u_fb->df = e_n_t_clr(0);
	 u_fb->db = e_n_t_clr(A_STANDOUT);
	 u_fb->dy = e_n_t_clr(A_STANDOUT);
	 u_fb->ct = e_n_t_clr(0);
	 u_fb->cr = e_n_t_clr(0);
	 u_fb->ck = e_n_t_clr(0);
	 u_fb->cp = e_n_t_clr(0);
	 u_fb->cc = e_n_t_clr(0);
	 u_fb->dc = 0x20;
	 u_fb->ws = 0;
      }
   }
#endif
}

void e_int_hnd(c)
     int c;
{
   signal(SIGINT, e_int_hnd);
   return;
}




