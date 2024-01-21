#define NEWCOMP

/* we_prog.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#undef TESTOUT

#include "edit.h"

#ifdef PROG

#undef exit

#include "xkeys.h"

#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>

#define NSTR ""

int e_run_sh();
int e_make_library();
int e_p_exec();
struct dirfile **e_make_prj_opt();
int e_s_sys_ini();
int e_s_sys_end();

int wfildes[2], efildes[2];
char *wfile = NULL, *efile = NULL;

struct e_s_prog e_s_prog = {  NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, 0  };

struct e_prog e_prog = {  0, 0, NULL, NULL, NULL, NULL, NULL  };

struct ERR_LI  {  char *file, *text, *srch;  int x, y, line;  } *err_li = NULL;
int err_no, err_num;

int e__project = 0, e_argc, e_p_l_comp;
char **e_arg = NULL;
char *e__arguments = NULL;
M_TIME last_time;
char e_get_buffer[128], library[80];
int e_get_pointer = 0;
int e_save_pid;
struct dirfile **e_p_df;

extern BUFFER *e_p_m_buffer;
extern char *e_tmp_dir;
#ifdef DEBUGGER
extern int e_d_swtch;
#endif

char *gnu_intstr = "${?*:warning:}${FILE}:${LINE}:* before `${COLUMN=BEFORE}\'*";
char *cc_intstr = "${?*:warning:}\"${FILE}\", line ${LINE}:* at or near * \"${COLUMN=AFTER}\"";
char *pc_intstr = "${?0:e}${?0:w}${?0:s}*:*:* * ${FILE}:\n\n* ${LINE}  ${CMPTEXT}\n*-------\
${COLUMN=PREVIOUS?^+14}\n[EWew] * line*";


char *e_p_msg[] = {  "Not a C - File",
		     "Can\'t open Pipe",
		     "Error in Process",
		     "Error in Pipe\n Pipe Nr.: %d\n",
		     "Error at Command: ",
		     "Return-Code: %d",      /*   Number 5   */
		     "%s is not a C - File",
		     "Unknown Compiler",
		     "No Compiler specified",
		     "No Files to compile"
		  };


#ifdef FREETEST

void test_free(void *p)
{
   char *tp = (char *) p;
   tp[0] = '\0';
   free(tp);
}
/*
void *test_realloc(void *p, size_t len)
{
   char *tp = (char *) p;
   return(realloc(tp, len));
}
*/
#endif
int e_prog_switch(f, c)
     FENSTER *f;
     int c;
{
   switch(c)
   {  case AltU:
      case CF9:
	 e_run(f);
	 break;
      case AltM:   /*  Alt M  Make */
      case F9:
	 e_make(f);
	 break;
      case AltC:   /*  Alt C  Compile */
      case AF9:
	 e_compile(f);
	 break;
      case AltL:   /*  Alt L  InstaLl */
	 e_install(f);
	 break;
      case AltA:   /*  Alt A  Execute MAke */
	 e_exec_make(f);
	 break;
      case AltT:   /*  Alt T  NexT Error */
      case AF8:
	 e_next_error(f);
	 break;
      case AltV:   /*  Alt V  PreVious Error  */
      case AF7:
	 e_previous_error(f);
	 break;
#ifdef DEBUGGER
      case CtrlG:   /*  Ctrl G DebuG - Modus */
	 e_deb_inp(f);
	 break;
      default:
	 return(e_debug_switch(f, c));
#else
      default:
	 return(c);
#endif
   }
   return(0);
}

int e_compile(f)
     FENSTER *f;
{
   int ret;
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   efildes[0] = efildes[1] = -1;
   wfildes[0] = wfildes[1] = -1;
   ret = e_comp(f);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   return(ret);
}

int e_make(f)
     FENSTER *f;
{
   int ret;
   ret = e_p_make(f);
   return(ret);
}

int e_p_make(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   char ostr[128], estr[128], mstr[80];
   int len, i, file = -1;
   struct stat cbuf[1], obuf[1];
   PIC *pic = NULL;
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   efildes[0] = efildes[1] = -1;
   wfildes[0] = wfildes[1] = -1;
   if(e_comp(f))
   {
#ifdef XWINDOW
      if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
      return(-1);
   }
   f = cn->f[cn->mxedt-1];
   if(!e__project)
   {  e_arg = MALLOC(6 * sizeof(char *));
      e_argc = e_make_arg(&e_arg, e_s_prog.libraries);
      e_arg[1] = MALLOC(3);
      strcpy(e_arg[1], "-o");
      strcpy(mstr, f->datnam);
      for(i = strlen(mstr); i >= 0 && mstr[i] != '.'; i--);
      if(i >= 0) mstr[i] = '\0';
      len = strlen(e_prog.exedir) - 1;
      if(e_s_prog.exe_name && e_s_prog.exe_name[0])
      {  if(e_prog.exedir[len] == DIRC)
	 sprintf(estr, "%s%s.e", e_prog.exedir, e_s_prog.exe_name);
	 else sprintf(estr, "%s%c%s.e", e_prog.exedir, DIRC, e_s_prog.exe_name);
      }
      else
      {  if(e_prog.exedir[len] == DIRC)
	 sprintf(estr, "%s%s.e", e_prog.exedir, mstr);
	 else sprintf(estr, "%s%c%s.e", e_prog.exedir, DIRC, mstr);
      }
      if(e_prog.exedir[len] == DIRC)
      sprintf(ostr, "%s%s.o", e_prog.exedir, mstr);
      else sprintf(ostr, "%s%c%s.o", e_prog.exedir, DIRC, mstr);
      e_argc = e_add_arg(&e_arg, estr, 2, e_argc);
      e_argc = e_add_arg(&e_arg, ostr, 3, e_argc);
      stat(ostr, cbuf);
      if(!stat(estr, obuf) &&
	       obuf->st_mtime >= cbuf->st_mtime) goto make_ende;
   }
   else
   {  if(!stat(e_s_prog.exe_name, obuf) && !e_p_l_comp &&
	       obuf->st_mtime >= last_time) goto make_ende;
   }
#ifdef DEBUGGER
   if(e_d_swtch > 0) e_d_quit(f);
#endif
   if(!e_p_mess_win("Linking", e_argc, e_arg, &pic, f))
   {  e_sys_ini();
      file = e_exec_inf(f, e_arg, e_argc);
      e_sys_end();
   }
   else file = 0;
   make_ende:
    if(!e__project) for(i = 0; i < e_argc - 1; i++) FREE(e_arg[i]);
   FREE(e_arg);
   if(file != 0) i = e_p_exec(file, f, pic);
   else
   {  i = ESC;  if(pic) e_close_view(pic, 1);  }
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   return(i);
}

int e_run(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   BUFFER *b;
   SCHIRM *s;
   char estr[256];
   int len, ret;
   efildes[0] = efildes[1] = -1;
   wfildes[0] = wfildes[1] = -1;
   if(!e_run_sh(f)) return(0);
   if(e_p_make(f)) return(-1);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   f = cn->f[cn->mxedt-1];
#ifdef DEBUGGER
   if(e_d_swtch > 0) e_d_quit(f);
#endif
   if(e_s_prog.exe_name && e_s_prog.exe_name[0])
#ifndef DJGPP
   {  if(e_s_prog.exe_name[0] == DIRC)
         sprintf(estr, "%s ", e_s_prog.exe_name);
      else if(e_prog.exedir[strlen(e_prog.exedir)-1] == DIRC)
         sprintf(estr, "%s%s ", e_prog.exedir, e_s_prog.exe_name);
      else sprintf(estr, "%s%c%s ", e_prog.exedir, DIRC, e_s_prog.exe_name);
#else
   {  if(e_s_prog.exe_name[0] == DIRC)
         sprintf(estr, "go32 %s ", e_s_prog.exe_name);
      else if(e_prog.exedir[strlen(e_prog.exedir)-1] == DIRC)
         sprintf(estr, "go32 %s%s ", e_prog.exedir, e_s_prog.exe_name);
      else sprintf(estr, "go32 %s%c%s ", e_prog.exedir, DIRC, e_s_prog.exe_name);
#endif
   }
   else if(!e__project)
#ifndef DJGPP
   {  if(f->datnam[0] == DIRC)
         sprintf(estr, "%s", f->datnam);
      else if(e_prog.exedir[strlen(e_prog.exedir)-1] == DIRC)
         sprintf(estr, "%s%s", e_prog.exedir, f->datnam);
      else sprintf(estr, "%s%c%s", e_prog.exedir, DIRC, f->datnam);
#else
   {  if(f->datnam[0] == DIRC)
         sprintf(estr, "go32 %s", f->datnam);
      else if(e_prog.exedir[strlen(e_prog.exedir)-1] == DIRC)
         sprintf(estr, "go32 %s%s", e_prog.exedir, f->datnam);
      else sprintf(estr, "go32 %s%c%s", e_prog.exedir, DIRC, f->datnam);
#endif
      for(len = strlen(estr); len > 0 && estr[len] != '.'; len--);
      estr[len] = '\0';
      strcat(estr, ".e ");
   }
#ifndef DJGPP
   else
   {  if(e_prog.exedir[strlen(e_prog.exedir)-1] == DIRC)
      sprintf(estr, "%sa.out ", e_prog.exedir);
      else sprintf(estr, "%s%ca.out ", e_prog.exedir, DIRC);
   }
#else
   else
   {  if(e_prog.exedir[strlen(e_prog.exedir)-1] == DIRC)
      sprintf(estr, "go32 %sa.out ", e_prog.exedir);
      else sprintf(estr, "go32 %s%ca.out ", e_prog.exedir, DIRC);
   }
#endif
   if(e_prog.arguments) strcat(estr, e_prog.arguments);
#ifdef XWINDOW
   if(e_we_sw & 1)  ret = e_x_system(estr);
   else
#endif
   ret = e_system(estr, cn);
   f = cn->f[cn->mxedt];
   b = cn->f[cn->mxedt]->b;
   s = cn->f[cn->mxedt]->s;
   e_new_line(b->mxlines, b);
   sprintf((char *)b->bf[b->mxlines-1].s, e_p_msg[5], ret);
   b->b.x = b->bf[b->mxlines-1].len =
	b->bf[b->mxlines-1].nrc = strlen((char *)b->bf[b->mxlines-1].s);
   b->b.y = b->mxlines-1;
   e_cursor(f, 1);
   e_schirm(f, 1);
   e_refresh();
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   return(0);
}

int e_comp(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   PIC *pic = NULL;
   char **arg = NULL, fstr[128], ostr[128];
   int i, file = -1, len, argc;
#ifdef CHECKHEADER
   struct stat obuf[1];
#else
   struct stat cbuf[1], obuf[1];
#endif
#ifdef DEBUGGER
   if(e_d_swtch > 0)
   {  i = e_message(1,
	    "The Debugger is Running\nDo You want to Quit Debugging ?",f);
      if(i == 'Y') e_d_quit(f);
      else return(-1);
#ifdef XWINDOW
      if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   }
#endif
   if(e_prog.project[0] && !access(e_prog.project, 0)) e__project = 1;
   else  e__project = 0;
   if(e__project) return(e_c_project(f));
   for(i = cn->mxedt; i > 0; i--)
   if(e_check_c_file(cn->f[i]->datnam)) break;
   if(i == 0)
   {  sprintf(ostr, e_p_msg[6], f->datnam);
      e_error(ostr, 0, f->fb);
      return(ESC);
   }
   else if(cn->f[i]->save) e_save(cn->f[i]);
   f = cn->f[i];
   e_switch_window(cn->edt[i], cn->f[cn->mxedt]);
   if(e_new_message(f)) return(ESC);
   argc = e_make_arg(&arg, e_s_prog.comp_str);
   arg[1] = MALLOC(3);
   strcpy(arg[1], "-c");
   len = strlen(f->dirct) - 1;
   if(!strcmp(f->ed->dirct, f->dirct)) strcpy(fstr, f->datnam);
   if(f->dirct[len] == DIRC) sprintf(fstr, "%s%s", f->dirct, f->datnam);
   else sprintf(fstr, "%s%c%s", f->dirct, DIRC, f->datnam);
   argc = e_add_arg(&arg, fstr, argc, argc);
   if(e_prog.exedir[strlen(e_prog.exedir)-1] == DIRC)
   sprintf(ostr, "%s%s", e_prog.exedir, f->datnam);
   else sprintf(ostr, "%s%c%s", e_prog.exedir, DIRC, f->datnam);
   for(len = strlen(ostr); len > 0 && ostr[len] != '.'; len --);
   ostr[len] = '\0';
   strcat(ostr, ".o");
#ifndef NOOPTFILEDIR
   argc = e_add_arg(&arg, "-o", argc, argc);
   argc = e_add_arg(&arg, ostr, argc, argc);
#endif
   e_sys_ini();
#ifdef CHECKHEADER
   if((i = stat(ostr, obuf)) != 0) e_check_header(fstr, (M_TIME) 0, cn, 0);
   if((i || e_check_header(fstr, obuf->st_mtime, cn, 0)))
#else
   stat(f->datnam, cbuf);
   if((stat(ostr, obuf) || obuf->st_mtime < cbuf->st_mtime))
#endif
   {  remove(ostr);
      if(!e_p_mess_win("Compiling", argc, arg, &pic, f) &&
		(file = e_exec_inf(f, arg, argc)) == 0)
      {  e_sys_end();  e_free_arg(arg, argc);
	 if(pic) e_close_view(pic, 1);   return(ESC);
      }
   }
   e_sys_end();
   e_free_arg(arg, argc);
   i = e_p_exec(file, f, pic);
   return(i);
}
#ifndef DJGPP
int e_exec_inf(f, argv, n)
     FENSTER *f;
     char **argv;
     int n;
{
   int pid;
   char tstr[128];
#ifdef DEBUGGER
   if(e_d_swtch > 0) e_d_quit(f);
#endif
   fflush(stdout);
/*
   if(pipe(efildes))
      {  e_error(e_p_msg[1], 0, f->fb); return(0); }
*/
   sprintf(tstr, "%s/we_111", e_tmp_dir);
   if((efildes[1] = creat(tstr, 0777)) < 0)
   {  e_error(e_p_msg[1], 0, f->fb); return(0);  }
   if((efildes[0] = open(tstr, O_RDONLY)) < 0 )
   {  e_error(e_p_msg[1], 0, f->fb); return(0);  }
   efile = MALLOC((strlen(tstr)+1)*sizeof(char));
   strcpy(efile, tstr);
   sprintf(tstr, "%s/we_112", e_tmp_dir);
   if((wfildes[1] = creat(tstr, 0777)) < 0)
   {  e_error(e_p_msg[1], 0, f->fb); return(0);  }
   if((wfildes[0] = open(tstr, O_RDONLY)) < 0 )
   {  e_error(e_p_msg[1], 0, f->fb); return(0);  }
   wfile = MALLOC((strlen(tstr)+1)*sizeof(char));
   strcpy(wfile, tstr);
   
   if((e_save_pid = pid = fork()) > 0) return(efildes[1]);
   else if(pid < 0)
   {  e_error(e_p_msg[2], 0, f->fb); return(0);  }
   
   close(2);   /*  neuer prozess   */
   if(fcntl(efildes[1], F_DUPFD, 2) != 2)
   {  fprintf(stderr, e_p_msg[3], efildes[1]);
      exit(1);
   }
   close(1);
   if(fcntl(wfildes[1], F_DUPFD, 1) != 1)
   {  fprintf(stderr, e_p_msg[3], wfildes[1]);
      exit(1);
   }
   e_print_arg(stderr, "", argv, n);
/*   alarm(60);    */
   execvp(argv[0], argv);
   e_print_arg(stderr, e_p_msg[4], argv, n);
   exit(1);
}
#endif

int e_print_arg(fp, s, argv, n)
     FILE *fp;
     char *s;
     char **argv;
     int n;
{
   int i;
   fprintf(fp,"%s", s);
   for(i = 0; i < n && argv[i] != NULL; i++) fprintf(fp," %s", argv[i]);
   fprintf(fp,"\n");
   return(n);
}


int e_p_exec(file, f, pic)
     int file;
     FENSTER *f;
     PIC *pic;
{
   ECNT *cn = f->ed;
   BUFFER *b = cn->f[cn->mxedt]->b;
   int ret = 0, i = 0, is, j, k, l = 0, m = 0, fd, stat_loc;
   char str[128];
   f = cn->f[cn->mxedt];
   nochmal:
   while((ret = wait(&stat_loc)) >= 0 && ret != e_save_pid);
   ret = 0;
   if((stat_loc & 0xff) != 0177 || ((stat_loc & 0xff00) >> 8) !=  SIGALRM)
   {  /*	kill(e_save_pid, SIGKILL);   */
      if(efildes[1] >= 0) close(efildes[1]);
      if(wfildes[1] >= 0) close(wfildes[1]);
   }
   if(!b->mxlines || b->bf[b->mxlines-1].len != 0)
   e_new_line(b->mxlines, b);
   for(is = b->mxlines-1, fd = efildes[0]; fd > 0; fd = wfildes[0])
   {  for(j = 0, i = b->mxlines-1; e_line_read(fd, str, 128) == 0; )
      {  k = 0;
	 do
	 {  if(j == 0) e_new_line(i, b);
	    for(; j < f->e.x - f->a.x - 2 && str[k] != '\n'
		    && str[k] != '\0'; j++, k++) *(b->bf[i].s+j) = str[k];
	    if(str[k] != '\0')
	    {  if(str[k] != '\n')
	       {  for(l = j-1; l > 0 && !isspace(b->bf[i].s[l]); l--);
		  if(l > 0)
		  {  e_new_line(i+1, b);
		     for(m = l; m < j; m++)
		     {  b->bf[i+1].s[m-l] = b->bf[i].s[m];
			b->bf[i].s[m] = ' ';
		     }
		  }
		  *(b->bf[i].s+j) = '\\';
		  j++;
	       }
	       *(b->bf[i].s+j) = '\n';
	       *(b->bf[i].s+j+1) = '\0';
	       b->bf[i].len = j;
	       b->bf[i].nrc = j + 1;
	       if(str[k] != '\n') {  i++;  j = m - l;  }
	       else j = 0;
	    }
	 }
	 while(str[k] != '\n' && str[k] != '\0');
	 if(str[k] != '\0')
	 {  b->b.y = i;
	    e_cursor(f, 1);
	    e_schirm(f, 1);
#ifdef XWINDOW
	    if(e_we_sw & 1) ret = e_x_change(pic);
#endif
	    e_refresh();
	    fflush(stdout);
	    i++;
	    j = 0;
	 }
      }
      if(fd == wfildes[0]) break;
   }
   if((stat_loc & 0xff) == 0177 && ((stat_loc & 0xff00) >> 8) ==  SIGALRM)
   goto nochmal;
   b->b.y = b->mxlines-1;
   if(efildes[0] >= 0) close(efildes[0]);
   if(wfildes[0] >= 0) close(wfildes[0]);
   if(wfile) {  remove(wfile);  FREE(wfile);  wfile = NULL;  }
   if(efile) {  remove(efile);  FREE(efile);  efile = NULL;  }
   efildes[0] = efildes[1] = -1;
   wfildes[0] = wfildes[1] = -1;
   if(pic) e_close_view(pic, 1);
   if(ret || (b->mxlines - is > 2 && (i = e_make_error_list(f))))
   {  if(i != -2 && !ret) e_show_error(err_no = 0, f);
      return(-1);
   }
   strcpy((char *)b->bf[b->mxlines-1].s, "Success");
   b->bf[b->mxlines-1].len = 7;
   b->bf[b->mxlines-1].nrc = 7;
   e_cursor(f, 1);
   e_schirm(f, 1);
   e_refresh();
   return(0);
}

int e_show_error(n, f)
     int n;
     FENSTER *f;
{
   ECNT *cn = f->ed;
   BUFFER *b = cn->f[cn->mxedt]->b;
   int i, j, bg = 0;
   char *filename;
   unsigned char *cp;
   if(!err_li || n >= err_num || n < 0) return(1);
   f = cn->f[cn->mxedt];
   if(err_li[n].file[0] == '.' && err_li[n].file[1] == DIRC) bg = 2;
#ifdef DJGPP
   if(err_li[n].file[0] == DIRC || err_li[n].file[0] == '/' || 
	(err_li[n].file[1] == ':' && (err_li[n].file[2] == DIRC ||
        err_li[n].file[2] == '/')))
#else
   if(err_li[n].file[0] == DIRC)
#endif
   filename = e_mkfilename(f->dirct, f->datnam);
   else filename = f->datnam;
   if(strcmp(err_li[n].file+bg, filename))
   {  for(i = cn->mxedt - 1; i > 0; i--)
      {  if(filename != cn->f[i+1]->datnam)
	 {  FREE(filename);
	    filename = e_mkfilename(cn->f[i]->dirct, cn->f[i]->datnam);
	 }
	 else filename = cn->f[i]->datnam;
	 if(!strcmp(err_li[n].file+bg, filename))
	 {  if(filename != cn->f[i]->datnam) FREE(filename);
            e_switch_window(cn->edt[i], cn->f[cn->mxedt]);
            break;  
         }
      }
      if(i <= 0)
      {  if(filename != cn->f[i+1]->datnam) FREE(filename); 
         if(e_edit(cn, err_li[n].file))return(ESC);
      }
   }
   else if(filename != f->datnam) FREE(filename);
   e_pr_str_wsd(1, MAXSLNS - 1, err_li[n].text, f->fb->mt.fb, -1, 0,
						f->fb->mt.fb, 1, MAXSCOL-2);
/*   e_pr_nstr(2, MAXSLNS - 1, MAXSCOL-2, err_li[n].text,
                                                f->fb->mt.fb, f->fb->mt.fb); */
   b = cn->f[cn->mxedt]->b;
   b->b.y = err_li[n].line > b->mxlines ? b->mxlines - 1 : err_li[n].line - 1;
   if(!err_li[n].srch)
   {  for(i = j = 0; i + j < err_li[n].x && i < b->bf[b->b.y].len; i++)
      {  if(*(b->bf[b->b.y].s + i) == TAB)
	 j += (f->ed->tabn - ((j + i) % f->ed->tabn) - 1);
#ifdef UNIX
	 else if(((unsigned char) *(b->bf[b->b.y].s + i)) > 126)
	 {  j++;
	    if(((unsigned char) *(b->bf[b->b.y].s + i)) < 128 + ' ') j++;
	 }
	 else if(*(b->bf[b->b.y].s + i) < ' ') j++;
#endif
      }
      b->b.x = i;
   }
   else
   {  cp = strstr(b->bf[b->b.y].s, err_li[n].srch+1);
      for(i = 0; b->bf[b->b.y].s + i < cp; i++);
      if(err_li[n].srch[0] == 'B')
      {  for(i--; i >= 0 && isspace(b->bf[b->b.y].s[i]); i--);
	 if(i < 0 && b->b.y > 0)
	 {  (b->b.y)--;  i = b->bf[b->b.y].len+1;  }
	 else i++;
      }
/*      else if(err_li[n].x < -1) i++;    */
      b->b.x = i +  err_li[n].x;
   }
   e_cursor(cn->f[cn->mxedt],1);
   return(0);
}

int e_pure_bin(str, ch)
     char *str;
     int ch;
{
   int i;
   for(i = 0; isspace(str[i]); i++);
   for(; str[i] && str[i] != ch; i++);
#ifdef DJGPP
   for(; i >= 0 && str[i] != DIRC && str[i] != '/' && str[i] != ':'; i--);
#else
   for(; i >= 0 && str[i] != DIRC; i--);
#endif
   return(i+1);
}

int e_make_error_list(f)
     FENSTER *f;
{
   char file[256];
   ECNT *cn = f->ed;
   BUFFER *b = cn->f[cn->mxedt]->b;
   int i, j, k = 0, ret = 0;
   char *spt;
   if(err_li)
   {  for(i = 0; i < err_num; i++)
      {  if(err_li[i].file) FREE(err_li[i].file);
	 if(err_li[i].text) FREE(err_li[i].text);
	 if(err_li[i].srch) FREE(err_li[i].srch);
      }
      FREE(err_li);
   }
   err_li = MALLOC(sizeof(struct ERR_LI) * b->mxlines);
   err_num = 0;
   for(i = 0; i < b->mxlines; i++)
   {  if(!strncmp((char *)b->bf[i].s, "Error at Command:", 17)) 
						return(!ret ? -2 : ret);
      if((!strncmp((char *)b->bf[i].s, "ld", 2)
		&& (b->bf[i].s[2] == ' '  || b->bf[i].s[2] == ':'))
		|| !strncmp((char *)b->bf[i].s, "collect:", 8)) ret = -2;
      else if(!strncmp((char *)b->bf[i].s, "makefile:", 9)
      		|| !strncmp((char *)b->bf[i].s, "Makefile:", 9))
      {  err_li[k].file = MALLOC(9);
	 for(j = 0; j < 8; j++) err_li[k].file[j] = b->bf[i].s[j];
	 err_li[k].file[8] = '\0';
	 err_li[k].line = atoi((char *)b->bf[i].s+9);
	 err_li[k].y = i;
	 err_li[k].x = 0;
	 err_li[k].srch = NULL;
	 err_li[k].text = MALLOC(strlen((char *)b->bf[i].s) + 1);
	 strcpy(err_li[k].text, (char *)b->bf[i].s);
	 err_li[k].text[b->bf[i].len] = '\0';
	 k++;
	 err_num++;
	 ret = -1;
	 continue;
      }
      else if(!strncmp((char *)b->bf[i].s, "make:", 5)
      		&& ( (spt = strstr((char *)b->bf[i].s, "makefile"))
      		||   (spt = strstr((char *)b->bf[i].s, "Makefile")) )
	 	&& (err_li[k].line = atoi(spt+14)) > 0 )
      {  err_li[k].file = MALLOC(9);
	 for(j = 0; j < 8; j++) err_li[k].file[j] = spt[j];
	 err_li[k].file[8] = '\0';
	 err_li[k].y = i;
	 err_li[k].x = 0;
	 err_li[k].srch = NULL;
	 err_li[k].text = MALLOC(strlen((char *)b->bf[i].s) + 1);
	 strcpy(err_li[k].text, (char *)b->bf[i].s);
	 err_li[k].text[b->bf[i].len] = '\0';
	 k++;
	 err_num++;
	 continue;
      }
#ifdef NEWCOMP
      else
      {  char *tststr = e_s_prog.comp_sw ? e_s_prog.intstr : gnu_intstr;
	 if(!(ret = e_p_cmp_mess(tststr, b, &i, &k, ret)))
	 {  int ip, in;
	    ip = e_pure_bin(e_s_prog.compiler, ' ');
	    in = e_pure_bin(b->bf[i].s, ':');
	    sprintf(file, "%s:", e_s_prog.compiler+ip);
	    if(!strncmp(file, b->bf[i].s+in, strlen(file))) ret = -2;
	    else if(!strncmp("ld:", b->bf[i].s+in, 3)) ret = -2;
	    else if(!strncmp("as:", b->bf[i].s+in, 3)) ret = -2;
	 }
/*	 e_pr_char(0, MAXSLNS-1, -ret + '0', 7);    */
      }
   }
   if(!(f->ed->edopt & 48) && ret == -1) ret = 0;
/*	 e_pr_char(0, 0, -ret + '0', 7);       */
   return(ret);
}
#else
      else if(!strncmp((char *)b->bf[i].s, "Error on line ", 14)
		|| !strncmp((char *)b->bf[i].s, "Warning on line ", 16) )
      {  if(*b->bf[i].s == 'E') bg = 14;
	 else bg = 16;
	 if((err_li[k].line = atoi(b->bf[i].s+bg)) <= 0) continue;
	 if(*b->bf[i].s == 'E') ret = -1;
	 else if(f->ed->edopt & 32) ret = -1;
#ifndef DJGPP
	 for(j = bg; b->bf[i].s[j] != '\0' && b->bf[i].s[j] != ':'; j++);
#else
	 for(j = bg; b->bf[i].s[j] != '\0' && (b->bf[i].s[j] != ':'
					|| b->bf[i].s[j-2] == ' '); j++);
#endif
	 for(bg = j; bg > 0 && b->bf[i].s[bg-1] != ' '; bg--);
	 err_li[k].file = MALLOC((j - bg + 1)*sizeof(char));
#ifndef DJGPP
	 for(j = bg; (err_li[k].file[j-bg] = b->bf[i].s[j]) != '\0'
					&& b->bf[i].s[j] != ':'; j++);
#else
	 for(j = bg; (err_li[k].file[j-bg] = b->bf[i].s[j]) != '\0'
		&& (b->bf[i].s[j] != ':' || b->bf[i].s[j-2] == ' '); j++);
#endif
	 err_li[k].file[j-bg] = '\0';
	 err_li[k].x = 0;
	 err_li[k].srch = NULL;
	 err_li[k].y = i;
	 err_li[k].text = MALLOC(strlen((char *)b->bf[i].s) + 1);
	 strcpy(err_li[k].text, (char *)b->bf[i].s);
	 err_li[k].text[b->bf[i].len] = '\0';
	 k++;
	 err_num++;
	 continue;
      }
      if(e_s_prog.comp_sw == 2)
      {
	 int x;
	 if(b->bf[i].s[b->bf[i].len-1] == ':')
	 {  bg = 0;
	    for(j = b->bf[i].len - 2; j >= 0 && b->bf[i].s[j] != ' '; j--);
	    if(j < 0 && i > 0 && b->bf[i-1].s[b->bf[i-1].len-1] == '\\')
	    {  for(j = b->bf[i-1].len - 2; j >= 0 && b->bf[i-1].s[j] != ' '; j--);
	       for(j++; (file[bg] = b->bf[i-1].s[j]) != '\\'; j++, bg++);
	       j = -1;
	    }
	    for(j++; (file[bg] = b->bf[i].s[j]) != ':'; j++, bg++);
	    file[bg] = '\0';
#ifndef DJGPP
	    bg = (file[0] == '.' && file[1] == DIRC) ? 2 : 0;
#else
	    bg = (file[0] == '.' && (file[1] == DIRC || file[1] == '/') ? 2 : 0;
#endif
	 }
	 else
	 {  for(j = 1; b->bf[i].s[j] == ' ' || b->bf[i].s[j] == '\t'; j++);
	    if(b->bf[i].s[j] >= '0' && b->bf[i].s[j] <= '9')
	    {  line = atoi(b->bf[i].s + j);
	       for(j++; b->bf[i].s[j] >= '0' && b->bf[i].s[j] <= '9'; j++);
	       x = j;
	    }
	    else if(b->bf[i].s[j] == '-' || b->bf[i].s[j] == '^')
	    {  for(err_li[k].x = j; b->bf[i].s[j] == '-'; j++);
	       err_li[k].x = j - x - 2;
	       err_li[k].srch = NULL;
	       err_li[k].y = i;
	       err_li[k].line = line;
	       err_li[k].file = MALLOC(strlen(file+bg) + 1);
	       strcpy(err_li[k].file, file+bg);
	       err_li[k].text = MALLOC(strlen((char *)b->bf[i].s) + 1);
	       strcpy(err_li[k].text, (char *)b->bf[i].s);
	       err_li[k].text[b->bf[i].len] = '\0';
	       k++;
	       err_num++;
	       if(!ret && b->bf[i].s[0] == 'E') ret = -1;
	       else if(!ret && f->ed->edopt & 32
				 && b->bf[i].s[0] == 'W') ret = -1;
	    }
	 }
      }
      else
      {  if(e_s_prog.comp_sw == 0)
	 {  bg = 0; chr[0] = ':';  chr[1] = ' ';  }
	 else
	 {  bg = 1; chr[0] = '\"';  chr[1] = ',';  }
#ifdef DJGPP
	 for(j = 0; ( (file[j] = b->bf[i].s[j + bg]) != chr[0] || j < 2 )
		&& file[j] != ' ' && file[j] != '\0'; j++);
#else
	 for(j = 0; (file[j] = b->bf[i].s[j + bg]) != chr[0]
		&& file[j] != ' ' && file[j] != '\0'; j++);
#endif
	 if(file[j] == chr[0] &&
		((e_s_prog.comp_sw == 0 && b->bf[i].s[j+bg+1] != chr[1] ) ||
		 (e_s_prog.comp_sw == 1 && b->bf[i].s[j+bg+1] == chr[1] )) &&
			(i == 0 || b->bf[i-1].s[b->bf[i-1].len-1] != '\\'))
	 {  file[j] = '\0';
	    for(bg = j; bg > 0 && file[bg-1] != ' '; bg--);
	    if(file[bg] == '.' && (file[bg+1] == DIRC || file[bg+1] == '/')) bg += 2;
	    if(!(e_s_prog.comp_sw & 1))
	    err_li[k].line = atoi((char *)b->bf[i].s+j+1);
	    else
	    err_li[k].line = atoi((char *)b->bf[i].s+j+9);
	    
	    if(err_li[k].line <= 0) continue;
	    
	    err_li[k].file = MALLOC(strlen(file+bg) + 1);
	    strcpy(err_li[k].file, file+bg);
	    err_li[k].x = 0;
	    err_li[k].srch = NULL;
	    err_li[k].y = i;
	    err_li[k].text = MALLOC(strlen((char *)b->bf[i].s) + 1);
	    strcpy(err_li[k].text, (char *)b->bf[i].s);
	    err_li[k].text[b->bf[i].len] = '\0';
	    k++;
	    err_num++;
	    if(!ret)
	    {  for(j++; b->bf[i].s[j] != ':' && b->bf[i].s[j] != '\0'; j++);
	       if(f->ed->edopt & 32
	       || strncmp((char *)(b->bf[i].s+j+2), "warning:", 8)) ret = -1;
	    }
	 }
      }
   }
   if(!(f->ed->edopt & 48) && ret == -1) ret = 0;
/*	 e_pr_char(0, 0, -ret + '0', 7);     */
   return(ret);
}
#endif

int e_previous_error(f)
     FENSTER *f;
{
   if(err_no > 0)  return(e_show_error(--err_no, f));
   e_pr_uul(f->fb);
   return(0);
}

int e_next_error(f)
     FENSTER *f;
{
   if(err_no < err_num - 1)  return(e_show_error(++err_no, f));
   e_pr_uul(f->fb);
   return(0);
}

int e_cur_error(y, f)
     int y;
     FENSTER *f;
{
   int i;
   if(err_num)
   {  for(i = 1; i < err_num && err_li[i].y <= y; i++);
      return(e_show_error(err_no = i - 1, f));
   }
   e_pr_uul(f->fb);
   return(0);
}

int e_d_car_ret(f)
     FENSTER *f;
{
   
   if(!strcmp(f->datnam, "Messages"))
   return(e_cur_error(f->ed->f[f->ed->mxedt]->b->b.y, f));
#ifdef DEBUGGER
   if(!strcmp(f->datnam, "Watches")) return(e_edit_watches(f));
   if(!strcmp(f->datnam, "Stack")) return(e_make_stack(f));
#endif
   return(0);
}

int e_line_read(n, s, max)
     int n;
     char *s;
     int max;
{
   int i, ret = 0;
   for(i = 0; i < max - 1; i++)
     if((ret = read(n, s + i, 1)) != 1 || s[i] == '\n'|| s[i] == '\0') break;
   if(ret != 1 && i == 0) return(-1);
   if(i == max - 1) i--;
   s[i+1] = '\0';
   return(0);
}

int e_arguments(f)
     FENSTER *f;
{
   char str[80];
   if(!e_prog.arguments)
   {  e_prog.arguments = MALLOC(1);  e_prog.arguments[0] = '\0';  }
   strcpy(str, e_prog.arguments);
   if(e_add_arguments(str, "Arguments", f, 0 , AltA, NULL))
   {  e_prog.arguments = REALLOC(e_prog.arguments, strlen(str) + 1);
      strcpy(e_prog.arguments, str);
   }
   return(0);
}

int e_check_c_file(name)
     char *name;
{
   int i, len;
   for(len = strlen(name); len >= 0 && name[len] != '.'; len--);
   if(len >= 0)
   {  for(i = 0; i < e_prog.num; i++)
      if(!strcmp(e_prog.comp[i]->filepostfix, (name+len)))
      {  e_copy_prog(&e_s_prog, e_prog.comp[i]);
	 return(i+1);
      }
   }
   return(0);
/*
    if(name[(len = strlen(name)) - 2] == '.' &&
				name[len - 1] == 'c') return(1);
    else if(name[len - 3] == '.' &&
	name[len - 2] == 'c' && name[len - 1] == 'c') return(2);
    else return(0);
*/
}

#ifdef CHECKHEADER

int e_check_header(file, otime, cn, sw)
     char *file;
     M_TIME otime;
     ECNT *cn;
     int sw;
{
   struct stat cbuf[1];
   FILE *fp;
   char *p, str[120], str2[120];
   int i;
   for(i = cn->mxedt; i > 0; i--)
   {  if(file[0] == DIRC)
      p = e_mkfilename(cn->f[i]->dirct, cn->f[i]->datnam);
      else p = cn->f[i]->datnam;
      if(!strcmp(p, file) && cn->f[i]->save)
      {  e_save(cn->f[i]);  if(p != cn->f[i]->datnam) FREE(p);  break;  }
      if(p != cn->f[i]->datnam) FREE(p);
   }
   if((fp = fopen(file, "r")) == NULL) return(sw);
/*  {   sprintf(str, e_msg[6], file);
        e_error(str, 0, cn->fb); 
        return(sw); 
    }
*/
   stat(file, cbuf);
   if(otime < cbuf->st_mtime) sw++;
   while(fgets(str, 120, fp))
   {  for(p = str; isspace(*p); p++);
      if(*p == '/' && *(p+1) == '*')
      {  p++;
	 do
	 {  for(p++; *p && *p != '*'; p++);
	    if(!*p && !fgets((p = str), 120, fp)) break;
	 } while (p != NULL && (*p != '*' || *(p+1) != '/'));
	 if(!p) break;
	 for(p += 2; isspace(*p); p++);
      }
      if(*p == '#')
      {  for(p++; isspace(*p); p++);
	 if(!strncmp(p, "include", 7))
	 {  for(p += 8; isspace(*p); p++);
	    if(*p == '\"')
	    {  for(p++, i = 0; p[i] != '\"' && p[i] != '\0'
				&& p[i] != '\n'; i++)  str2[i] = p[i];
	       str2[i] = '\0';
	       sw = e_check_header(str2, otime, cn, sw);
	    }
	 }
      }
   }
   fclose(fp);
   return(sw);
}
/*
char *e_fgets(char *s, int n, FILE *stream)
{
    char *b;
    int i;
    for(; e_get_buffer[e_get_pointer] == ' ' 
		|| e_get_buffer[e_get_pointer] == '\t'; e_get_pointer++);
    if(e_get_buffer[e_get_pointer] == '\0' 
			|| e_get_buffer[e_get_pointer] == '\n')  
    {  e_get_buffer[0] = '\0';  e_get_pointer = 0;  }
    if(e_get_buffer[0] == '\0')
    {	if(!(b = fgets(e_get_buffer, n, stream)))  return(b);
	else e_get_pointer = 0;
	for(; e_get_buffer[e_get_pointer] == ' ' 
		|| e_get_buffer[e_get_pointer] == '\t'; e_get_pointer++);
    }
    for(i = 0; (s[i] = e_get_buffer[e_get_pointer]) != '\0' 
			&& s[i] != '\n' && s[i] != ' '; i++, e_get_pointer++);
    if(s[i] == '\0' || s[i] == '\n')  
    {  e_get_buffer[0] = '\0';  e_get_pointer = 0;  }
    s[i] = '\0';
    return(s);
}

char *e_gets(char *s, int n, FILE *stream)
{
    char *b;
    int i;
    for(i = 0; (s[i] = e_get_buffer[e_get_pointer]) != '\0' 
			&& s[i] != '\n'; i++, e_get_pointer++);
    if(e_get_buffer[e_get_pointer] == '\0' 
			|| e_get_buffer[e_get_pointer] == '\n')  
    {  e_get_buffer[0] = '\0';  e_get_pointer = 0;  }
    if(e_get_buffer[0] == '\0')
    {	if(!(b = fgets(e_get_buffer, n, stream)))  return(b);
	else e_get_pointer = 0;
	for(; e_get_buffer[e_get_pointer] == ' ' 
		|| e_get_buffer[e_get_pointer] == '\t'; e_get_pointer++);
    }
    for(i = 0; (s[i] = e_get_buffer[e_get_pointer]) != '\0' 
			&& s[i] != '\n'; i++, e_get_pointer++);
    if(s[i] == '\0' || s[i] == '\n')  
    {  e_get_buffer[0] = '\0';  e_get_pointer = 0;  }
    s[i] = '\0';
    return(s);
}
*/
#endif

char *e_cat_string(p, str)
     char *p;
     char *str;
{
   if(str == NULL) return(p = NULL);
   if(p == NULL)
   {  if((p = MALLOC(strlen(str)+1)) == NULL) return(NULL);
      p[0] = '\0';
   }
   else if((p = REALLOC(p, strlen(p) + strlen(str)+2)) == NULL) return(NULL);
   strcat(p, " ");
   strcat(p, str);
   return(p);
}

int e_make_arg(arg, str)
     char ***arg;
     char *str;
{
   int fl = 0, i, j;
   char tmp[128], *p = tmp;
   if(!(*arg)) *arg = (char **) MALLOC(4*sizeof(char *));
   else *arg = (char **) REALLOC(*arg, 4*sizeof(char *));
   (*arg)[0] = MALLOC(strlen(e_s_prog.compiler) + 1);
   strcpy((*arg)[0], e_s_prog.compiler);
   if(!str)
   {  (*arg)[1] = NULL;
      (*arg)[2] = NULL;
      return(2);
   }
   strcpy(tmp, str);
   for(j = 2, i = 0; p[i] != '\0'; j++)
   {  for(; p[i] != '\0' && p[i] != ' '; i++);
      if(p[i] == '\0') fl = !fl;
      p[i] = '\0';
      (*arg)[j] = MALLOC(strlen(p) + 1);
      strcpy((*arg)[j], p);
      *arg = (char **) REALLOC(*arg, (j + 3)*sizeof(char *));
      if(fl) {  j++;  break;  }
      p += (i + 1);
      i = 0;
   }
   (*arg)[j] = NULL;
   return(j);
}

int e_add_arg(arg, str, n, argc)
     char ***arg;
     char *str;
     int n;
     int argc;
{
   int i;
   argc++;
   *arg = (char **) REALLOC(*arg, (argc+1)*sizeof(char *));
   for(i = argc; i > n; i--) (*arg)[i] = (*arg)[i-1];
   (*arg)[n] = MALLOC(strlen(str) + 1);
   strcpy((*arg)[n], str);
   return(argc);
}

int e_ini_prog(cn)
     ECNT *cn;
{
   int i;
#ifdef DJGPP
   e_prog.num = 3;
#else
   e_prog.num = 4;
#endif
   e_prog.n = 0;
   e_prog.arguments = e_make_string(e_prog.arguments, "");
   e_prog.project = e_make_string(e_prog.project, "project.prj");
   e_prog.exedir = e_make_string(e_prog.exedir, ".");
   e_prog.sys_include = e_make_string(e_prog.sys_include,
			 "/usr/include:/usr/5include:/usr/include/X11");
   if(e_prog.comp == NULL)
   e_prog.comp = MALLOC(e_prog.num * sizeof(struct e_s_prog *));
   else
   e_prog.comp = REALLOC(e_prog.comp, e_prog.num * sizeof(struct e_s_prog *));
   for(i = 0; i < e_prog.num; i++)
   e_prog.comp[i] = MALLOC(sizeof(struct e_s_prog));
   e_prog.comp[0]->compiler = e_make_string(NULL, "gcc");
   e_prog.comp[0]->language = e_make_string(NULL, "C");
   e_prog.comp[0]->filepostfix = e_make_string(NULL, ".c");
   e_prog.comp[0]->key = 'C';
   e_prog.comp[0]->x = 0;
   e_prog.comp[0]->intstr = e_make_string(NULL, cc_intstr);
#ifdef DJGPP
   e_prog.comp[0]->libraries = e_make_string(NULL, "");
   e_prog.comp[1]->libraries = e_make_string(NULL, "-lgpl -lm");
   e_prog.comp[2]->libraries = e_make_string(NULL, "");
   e_prog.comp[1]->compiler = e_make_string(NULL, "gcc");
   e_prog.comp[2]->compiler = e_make_string(NULL, "gfc");
#else
   e_prog.comp[2]->compiler = e_make_string(NULL, "f77");
   e_prog.comp[1]->compiler = e_make_string(NULL, "g++");
#endif
   e_prog.comp[1]->language = e_make_string(NULL, "C++");
   e_prog.comp[1]->filepostfix = e_make_string(NULL, ".cc");
   e_prog.comp[1]->key = '+';
   e_prog.comp[1]->x = 1;
   e_prog.comp[1]->intstr = e_make_string(NULL, cc_intstr);
   e_prog.comp[2]->language = e_make_string(NULL, "Fortran");
   e_prog.comp[2]->filepostfix = e_make_string(NULL, ".f");
   e_prog.comp[2]->key = 'F';
   e_prog.comp[2]->x = 0;
   e_prog.comp[2]->intstr = e_make_string(NULL, cc_intstr);
#ifndef DJGPP
   e_prog.comp[3]->compiler = e_make_string(NULL, "pc");
   e_prog.comp[3]->language = e_make_string(NULL, "Pascal");
   e_prog.comp[3]->filepostfix = e_make_string(NULL, ".p");
   e_prog.comp[3]->key = 'P';
   e_prog.comp[3]->x = 0;
   e_prog.comp[3]->intstr = e_make_string(NULL, pc_intstr);
#endif
   for(i = 0; i < e_prog.num; i++)
   {  e_prog.comp[i]->comp_str = e_make_string(NULL, "-g");
#ifndef DJGPP
      e_prog.comp[i]->libraries = e_make_string(NULL, "");
#endif
      e_prog.comp[i]->exe_name = e_make_string(NULL, "");
      e_prog.comp[i]->comp_sw = i < 2 ? 0 : 1;
   }
   e_copy_prog(&e_s_prog, e_prog.comp[0]);
   return(0);
}

int e_copy_prog(out, in)
     struct e_s_prog *out;
     struct e_s_prog *in;
{
   out->language = e_make_string(out->language, in->language);
   out->filepostfix = e_make_string(out->filepostfix, in->filepostfix);
   out->compiler = e_make_string(out->compiler, in->compiler);
   out->comp_str = e_make_string(out->comp_str, in->comp_str);
   out->libraries = e_make_string(out->libraries, in->libraries);
   out->exe_name = e_make_string(out->exe_name, in->exe_name);
   out->intstr = e_make_string(out->intstr, in->intstr);
   out->key = in->key;
   out->comp_sw = in->comp_sw;
   return(0);
}

int e_prj_ob_btt(f, sw)
     FENSTER *f;
     int sw;
{
   FLWND *fw;
   e_data_first(sw+4, f->ed, f->ed->dirct);
   if(sw > 0)
   {  if(!(f->ed->edopt & 1)) while(e_data_eingabe(f->ed) != AF3);
      else while(e_data_eingabe(f->ed) != CF4);
      fw = (FLWND *)f->ed->f[f->ed->mxedt]->b;
      fw->df = NULL;
      e_close_window(f->ed->f[f->ed->mxedt]);
   }
   return(0);
}

int e_prj_ob_file(f)
     FENSTER *f;
{
   return(e_prj_ob_btt(f, 0));
}

int e_prj_ob_varb(f)
     FENSTER *f;
{
   return(e_prj_ob_btt(f, 1));
}

int e_prj_ob_inst(f)
     FENSTER *f;
{
   return(e_prj_ob_btt(f, 2));
}

int e_prj_ob_svas(f)
     FENSTER *f;
{
   return(e_project_name(f) ? 0 : AltS);
}

int e_project_options(f)
     FENSTER *f;
{
   int ret;
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o) return(-1);
   if(!(e_make_prj_opt(f))) {  freeostr(o);  return(-1);  }
   o->xa = 8;  o->ya = 2;  o->xe = 68;  o->ye = 22;
   o->bgsw = 0;
   o->name = "Project-Options";
   o->crsw = AltS;
   e_add_txtstr(4, 12, "Compiler-Style:", o);
   e_add_wrstr(4, 2, 22, 2, 36, 128, 0, AltC, "Compiler:", e_s_prog.compiler, NULL, o);
   e_add_wrstr(4, 4, 22, 4, 36, 256, 3, AltP, "ComPiler-Options:", e_s_prog.comp_str, NULL, o);
   e_add_wrstr(4, 6, 22, 6, 36, 256, 0, AltL, "Loader-Options:", e_s_prog.libraries, NULL, o);
   e_add_wrstr(4, 8, 22, 8, 36, 128, 0, AltE, "Executable:", e_s_prog.exe_name, NULL, o);
   e_add_wrstr(4, 10, 22, 10, 36, 128, 2, AltB, "LiBrary:", library, NULL, o);
   e_add_wrstr(22, 12, 22, 13, 36, 256, 0, AltM, "Message-String:", e_s_prog.intstr, NULL, o);
   e_add_pswstr(0, 5, 13, 0, AltG, 0, "GNU       ", o);
   e_add_pswstr(0, 5, 14, 1, AltT, e_s_prog.comp_sw, "OTher     ", o);
   e_add_bttstr(9, 18, 0, AltS, "Save", NULL, o);
   e_add_bttstr(44, 18, -1, ESC, "Cancel", NULL, o);
   e_add_bttstr(26, 18, 5, AltA, "Save As", e_prj_ob_svas, o);
/*    e_add_bttstr(7, 16, 0, AltF, "Files ...", e_prj_ob_file, o);  */
   e_add_bttstr(12, 16, 0, AltV, "Variables ...", e_prj_ob_varb, o);
   e_add_bttstr(35, 16, 0, AltI, "Install ...", e_prj_ob_inst, o);
   ret = e_opt_kst(o);
   if(ret != ESC)
   {  e_s_prog.compiler = e_make_string(e_s_prog.compiler, o->wstr[0]->txt);
      e_s_prog.comp_str = e_make_string(e_s_prog.comp_str, o->wstr[1]->txt);
      e_s_prog.libraries = e_make_string(e_s_prog.libraries, o->wstr[2]->txt);
      e_s_prog.exe_name = e_make_string(e_s_prog.exe_name, o->wstr[3]->txt);
      e_s_prog.intstr = e_make_string(e_s_prog.intstr, o->wstr[5]->txt);
      strcpy(library, o->wstr[4]->txt);
      e_s_prog.comp_sw = o->pstr[0]->num;
      e_wrt_prj_fl(f);
   }
   freeostr(o);
   if(f->ed->mxedt > 0) e_ed_rahmen(f, 1);
   return(0);
}

int e_run_c_options(f)
     FENSTER *f;
{
   int i, ret;
   char **ct;
   W_OPTSTR *o = e_init_opt_kst(f);
   if(!o) return(-1);
   for(ct = &e_s_prog.language, i = 0; i < 7; i++, ct++)
   {  if(*ct == NULL) {  *ct = malloc(1);  **ct = '\0';  }  }
   o->xa = 8;  o->ya = 2;  o->xe = 68;  o->ye = 22;
   o->bgsw = 0;
   o->name = "Compiler-Options";
   o->crsw = AltO;
   e_add_txtstr(4, 14, "Compiler-Style:", o);
   e_add_wrstr(4, 2, 22, 2, 36, 128, 0, AltC, "Compiler:", e_s_prog.compiler, NULL, o);
   e_add_wrstr(4, 4, 22, 4, 36, 128, 3, AltP, "ComPiler-Options:", e_s_prog.comp_str, NULL, o);
   e_add_wrstr(4, 6, 22, 6, 36, 128, 0, AltL, "Loader-Options:", e_s_prog.libraries, NULL, o);
   e_add_wrstr(4, 8, 22, 8, 36, 128, 0, AltE, "Executable:", e_s_prog.exe_name, NULL, o);
   e_add_wrstr(4, 10, 22, 10, 36, 128, 1, AltA, "LAnguage:", e_s_prog.language, NULL, o);
   e_add_wrstr(4, 12, 22, 12, 36, 128, 0, AltF, "File-Postfix:", e_s_prog.filepostfix, NULL, o);
   e_add_wrstr(22, 14, 22, 15, 36, 128, 0, AltM, "Message-String:", e_s_prog.intstr , NULL, o);
   e_add_pswstr(0, 5, 15, 0, AltG, 0, "GNU      ", o);
   e_add_pswstr(0, 5, 16, 1, AltT, e_s_prog.comp_sw, "OTher    ", o);
   e_add_bttstr(16, 18, 1, AltO, " Ok ", NULL, o);
   e_add_bttstr(37, 18, -1, ESC, "Cancel", NULL, o);
   ret = e_opt_kst(o);
   if(ret != ESC)
   {  e_s_prog.compiler = e_make_string(e_s_prog.compiler, o->wstr[0]->txt);
      e_s_prog.comp_str = e_make_string(e_s_prog.comp_str, o->wstr[1]->txt);
      e_s_prog.libraries = e_make_string(e_s_prog.libraries, o->wstr[2]->txt);
      e_s_prog.exe_name = e_make_string(e_s_prog.exe_name, o->wstr[3]->txt);
      e_s_prog.language = e_make_string(e_s_prog.language, o->wstr[4]->txt);
      e_s_prog.filepostfix = e_make_string(e_s_prog.filepostfix, o->wstr[5]->txt);
      e_s_prog.intstr = e_make_string(e_s_prog.intstr, o->wstr[6]->txt);
      e_s_prog.comp_sw = o->pstr[0]->num;
   }
   freeostr(o);
   return(0);
}

int e_run_options(f)
     FENSTER *f;
{
   int i, n, xa = 48, ya = 2, num = 2 + e_prog.num;
   OPTK *opt = MALLOC(num * sizeof(OPTK));
   char tmp[80];
   
   tmp[0] = '\0';
   opt[0].t = "Add Compiler   ";     opt[0].x = 0;  opt[0].o = 'A';
   opt[1].t = "Remove Compiler";     opt[1].x = 0;  opt[1].o = 'R';
   
   for (i = 0; i < e_prog.num; i++)
   {  opt[i+2].t = e_prog.comp[i]->language;  opt[i+2].x = e_prog.comp[i]->x;
      opt[i+2].o = e_prog.comp[i]->key;
   }
   n = e_opt_sec_box(xa, ya, num, opt, f, 1);
   
   if(n == 0)
   {  if(!e_run_c_options(f))
      {  char **ct;
         e_prog.num++;
	 e_prog.comp = REALLOC(e_prog.comp,
			e_prog.num * sizeof(struct e_s_prog *));
	 e_prog.comp[e_prog.num-1] = MALLOC(sizeof(struct e_s_prog));
         for(ct = &e_prog.comp[e_prog.num-1]->language, i = 0; i < 7; i++, ct++)
         {  *ct = malloc(1);  **ct = '\0';  }
	 e_copy_prog(e_prog.comp[e_prog.num-1], &e_s_prog);
	 for(n = 0; e_prog.comp[e_prog.num-1]->language[n]; n++)
	 {  for(i = 0; i <= e_prog.num
			&& toupper(e_prog.comp[e_prog.num-1]->language[n])
					!= opt[i].o; i++);
	    if(i > e_prog.num) break;
	 }
	 e_prog.comp[e_prog.num-1]->key =
			toupper(e_prog.comp[e_prog.num-1]->language[n]);
	 e_prog.comp[e_prog.num-1]->x = n;
      }
   }
   else if(n == 1)
   {  if(e_add_arguments(tmp, "Remove Compiler", f, 0, AltR, NULL))
      {  for(i = 0; i < e_prog.num
			&& strcmp(e_prog.comp[i]->language, tmp); i++);
	 if(i >= e_prog.num)
	 {  e_error(e_p_msg[7], 0, f->fb); FREE(opt); return(0);  }
	 FREE(e_prog.comp[i]);
	 for(; i < e_prog.num-1; i++) e_prog.comp[i] = e_prog.comp[i+1];
	 e_prog.num--;
      }
   }
   else if(n > 1)
   {  e_copy_prog(&e_s_prog, e_prog.comp[n-2]);
      e_run_c_options(f);
      e_copy_prog(e_prog.comp[n-2], &e_s_prog);
   }
   FREE(opt);
   return(n < 0 ? ESC : 0);
}

int e_project_name(f)
     FENSTER *f;
{
   char str[80];
   if(!e_prog.project)
   {  e_prog.project = MALLOC(1);  e_prog.project[0] = '\0';  }
   strcpy(str, e_prog.project);
   if(e_add_arguments(str, "Project", f, 0, AltP, NULL))
   {  e_prog.project = REALLOC(e_prog.project, strlen(str) + 1);
      strcpy(e_prog.project, str);
      return(0);
   }
   return(ESC);
}

int e_project(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   int i;
   if(!e_project_name(f))
   {  for(i = cn->mxedt; i > 0
	   && (cn->f[i]->dtmd != 'D' || cn->f[i]->ins != 4); i--);
      if(i > 0)
      {  e_switch_window(cn->edt[i], cn->f[cn->mxedt]);
	 e_close_window(cn->f[cn->mxedt]);
      }
      f = cn->f[cn->mxedt];
      e_make_prj_opt(f);
      e_prj_ob_file(f);
      return(0);
   }
   return(ESC);
}

int e_show_project(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   int i;
   for(i = cn->mxedt; i > 0
	   && (cn->f[i]->dtmd != 'D' || cn->f[i]->ins != 4); i--);
   if(i > 0) e_switch_window(cn->edt[i], cn->f[cn->mxedt]);
   else
   {  e_make_prj_opt(f);
      e_prj_ob_file(f);
   }
   return(0);
}

int e_cl_project(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   int i;
   if(!e_prog.project) e_prog.project = MALLOC(sizeof(char));
   else e_prog.project = REALLOC(e_prog.project, sizeof(char));
   e_prog.project[0] = '\0';
   for(i = cn->mxedt; i > 0
	   && (cn->f[i]->dtmd != 'D' || cn->f[i]->ins != 4); i--);
   if(i > 0)
   {  e_switch_window(cn->edt[i], cn->f[cn->mxedt]);
      e_close_window(cn->f[cn->mxedt]);
   }
   return(0);
}

int e_p_add_item(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   int i;
   for(i = cn->mxedt; i > 0
	   && (cn->f[i]->dtmd != 'D' || cn->f[i]->ins != 4); i--);
   if(i > 0) e_switch_window(cn->edt[i], cn->f[cn->mxedt]);
   else
   {  FLWND *fw;
      e_make_prj_opt(f);
      e_prj_ob_file(f);
      fw = (FLWND *) cn->f[cn->mxedt]->b;
      fw->nf = fw->df->anz - 1;
   }
   cn->f[cn->mxedt]->save = 1;
   e_file_first(5, cn, NULL);
   e_file_eingabe(cn);
   return(0);
}

int e_p_del_item(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   int i;
   for(i = cn->mxedt; i > 0
	   && (cn->f[i]->dtmd != 'D' || cn->f[i]->ins != 4); i--);
   if(i > 0) e_switch_window(cn->edt[i], cn->f[cn->mxedt]);
   else return(e_error("No Project-Window", 0, f->fb));
   f = cn->f[cn->mxedt];
   f->save = 1;
   e_p_del_df((FLWND *) f->b, f->ins);
   return(0);
}

int e_make_library(library, ofile, f)
     char *library;
     char *ofile;
     FENSTER *f;
{
#ifdef NOANSI
   char *ar_arg[5];
#else
   char *ar_arg[5] = {  NULL, NULL, NULL, NULL, NULL  };
#endif
   int ret = 0, file = -1;
   PIC *pic = NULL;
#ifdef NOANSI
   {  int i;
      for(i = 0; i < 5; i++) ar_arg[i] = NULL;
   }
#endif
   ar_arg[0] = "ar";
   if(access(library, 0)) ar_arg[1] = "-cr";
   else ar_arg[1] = "-r";
   ar_arg[2] = library;
   ar_arg[3] = ofile;
   if((ret = e_p_mess_win("Insert into Archive", 4, ar_arg, &pic, f)) != 0) goto m_l_ende;
   e_sys_ini();
   file = e_exec_inf(f, ar_arg, 4);
   e_sys_end();
   if(!file) goto m_l_ende;
   if((ret = e_p_exec(file, f, pic)) != 0) goto m_l_ende;
   pic = NULL;
/*
#ifdef RANLIB
    ar_arg[0] = "ranlib";
    ar_arg[1] = library;
    ar_arg[2] = NULL;
    if(ret = e_p_mess_win("Convert Archive", 2, ar_arg, &pic, f)) goto m_l_ende;
    e_sys_ini();
    file = e_exec_inf(f, ar_arg, 2);
    e_sys_end();
    if(file) ret = e_p_exec(file, f, pic);
#endif
*/
   m_l_ende:
    return((!ret && file) ? 0 : -1);
}

#ifdef TESTOUT
int print_test(char *f, char *s)
{
   extern struct EXT h_error;
   char tmp[256];
   sprintf(tmp, f, s);
   e_error(tmp, 0, h_error.cn->fb);
   return(0);
}
#endif

int e_system(estr, cn)
     char *estr;
     ECNT *cn;
{
#ifdef MOUSE
   int g[4];
#endif
   int ret;
   PIC *outp;
   FENSTER *f;
#if  MOUSE
   g[0] = 2;
   fk_mouse(g);
#endif
   f = cn->f[cn->mxedt-1];
   outp = e_open_view(0,0,MAXSCOL-1,MAXSLNS-1,cn->fb->ws,1);
   fk_locate(0,0);
   fk_cursor(1);
   e_s_sys_ini();
   ret = system(estr);
#ifndef DJGPP
   if(!(e_we_sw & 1))
#endif
   {  printf(e_msg[13]);
      fflush(stdout);
      fk_getch();
   }
#ifdef UNIX
   e_s_sys_end();
#endif
   e_close_view(outp, 1);
   fk_cursor(0);
#if  MOUSE
   g[0] = 1;
   fk_mouse(g);
#endif
   return(ret);
}

int e_d_p_message(str, f, sw)
     char *str;
     FENSTER *f;
     int sw;
{
   ECNT *cn = f->ed;
   BUFFER *b;
   SCHIRM *s;
   int i, j, k;
   if(str[0] == '\0' || str[0] == '\n') return(0);
   for(i = cn->mxedt; i > 0 && strcmp(cn->f[i]->datnam, "Messages"); i--);
   if(i == 0)
   {  if(e_edit(cn, "Messages")) return(-1);
      else i = cn->mxedt;
   }
   f = cn->f[i];
   b = cn->f[i]->b;
   s = cn->f[i]->s;
   k = 0;
   do
   {  e_new_line(b->mxlines, b);
      i = b->mxlines-1;
      for(j = 0; j < f->e.x - f->a.x - 2 && str[k] != '\n'
	    && str[k] != '\0'; j++, k++) *(b->bf[i].s+j) = str[k];
      if(str[k] != '\0')
      {  *(b->bf[i].s+j) = '\n';
	 *(b->bf[i].s+j+1) = '\0';
      }
      b->bf[i].len = j;
      b->bf[i].nrc = j + 1;
   }
   while(str[k] != '\n' && str[k] != '\0');
   b->b.y = b->mxlines-1;
   if(sw) e_rep_win_tree(cn);
   else if(e_we_sw & 1)
   {  e_schirm(f, 0);
      e_cursor(f, 0);
      e_refresh();
   }
   return(0);
}
/*
int e_install(FENSTER *f)
{
    char *tp, text[120], text1[512], string[120];
    FILE *fp;
    PIC *outp;
    int num = 0, len, i, j, k, l, n;
    struct proj_var {  char *var, *string;  }  *p_v;
    if(e_p_make(f)) return(-1);
    if(!e__project) return(0);
    if((fp = fopen(e_prog.project, "r")) == NULL)
    {   sprintf(text, e_msg[6], e_prog.project);
        e_error(text, 0, f->fb); 
        return(ESC); 
    }
    p_v = MALLOC(sizeof(struct proj_var));
    while((tp = e_fgets(text, 120, fp)))
    {	if(text[0] != '#' && text[(len = strlen(text))-1] == ':')
	{  p_v[num].var = MALLOC(len);
	   text[len-1] = '\0';
	   strcpy(p_v[num].var, text);
	   p_v[num].string = MALLOC(1);
	   p_v[num].string[0] = '\0';
           while(e_fgets(text, 120, fp) && text[0] != '\n' 
				&& text[0] != '\0')
	   {  len = strlen(p_v[num].string) + strlen(text) + 2;
	      p_v[num].string = REALLOC(p_v[num].string, len);
	      strcat(p_v[num].string, " ");
	      strcat(p_v[num].string, text);
	   }
	   ++num;
	   p_v = REALLOC(p_v, (num + 1) * sizeof(struct proj_var));
        }
    }
    rewind(fp);
    while((tp = e_fgets(text, 120, fp)) && strncmp(text, "install:", 8));
    while(tp && e_gets(text, 120, fp) && text[0] != '\n' && text[0] != '\0')
    {   if(text[0] == '#') continue;
	for(i = k = 0; k < 512 && (text1[k] = text[i]); i++, k++)
	{  if(text[i] == '$' && text[i+1] == '{')
	   {  	for(j = 0; text[i+j+2] && text[i+j+2] != '}'; j++)
				string[j] = text[i+j+2];
		string[j] = '\0';
		for(l = 0; l < num; l++)
		{  if(!strcmp(p_v[l].var, string))
		   {  for(n = 0; text1[k] = p_v[l].string[n]; n++, k++);
		      k--;
		      i += (j+2);
		      break;
		   }
		}
	   }
	}
        e_d_p_message(text1, f);
	system(text1);        
    }
    for(i = 0; i < num; i++) {  FREE(p_v[i].var);  FREE(p_v[i].string);  }
    FREE(p_v);
    fclose(fp);
    return(0);
}
*/
#if MOUSE
int e_d_car_mouse(f)
     FENSTER *f;
{
   extern struct mouse e_mouse;
   BUFFER *b = f->ed->f[f->ed->mxedt]->b;
   SCHIRM *s = f->ed->f[f->ed->mxedt]->s;
   if(e_mouse.y-f->a.y+s->c.y-1 == b->b.y) return(CR);
   else
   {  b->b.y = e_mouse.y-f->a.y+s->c.y-1;
      b->b.x = e_mouse.x-f->a.x+s->c.x-1;
   }
   return(0);
}
#endif

int e_exec_make(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   char **arg = NULL;
   int i, file, argc;
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   efildes[0] = efildes[1] = -1;
   wfildes[0] = wfildes[1] = -1;
   for(i = cn->mxedt; i > 0; i--)
   if(!strcmp(cn->f[i]->datnam, "Makefile") ||
			!strcmp(cn->f[i]->datnam, "makefile"))
   {  e_switch_window(cn->edt[i], cn->f[cn->mxedt]);
      e_save(cn->f[cn->mxedt]);
   }
   if(e_new_message(f)) return(ESC);
   f = cn->f[cn->mxedt];
   e_sys_ini();
   if(e_s_prog.compiler) FREE(e_s_prog.compiler);
   e_s_prog.compiler = MALLOC(5*sizeof(char));
   strcpy(e_s_prog.compiler, "make");
   argc = e_make_arg(&arg, e_prog.arguments);
   if(argc == 0)
   {  arg[1] = NULL;
      argc = 2;
   }
   else
   {  for(i = 1; i < argc; i++) arg[i] = arg[i+1];
   }
   if((file = e_exec_inf(f, arg, argc)) == 0)
   {  e_sys_end();
#ifdef XWINDOW
      if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
      return(ESC);
   }
   e_sys_end();
   for(i = 0; i < argc - 1; i++) FREE(arg[i]);
   FREE(arg);
   i = e_p_exec(file, f, NULL);
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(LASTCUR);
#endif
   return(i);
}

int e_run_sh(f)
     FENSTER *f;
{
   int ret, len = strlen(f->datnam);
   char estr[128];
   if(strcmp(f->datnam+len-3, ".sh")) return(1);
   
#ifdef XWINDOW
   if(e_we_sw & 1) fk_pointer(WTCUR);
#endif
   f->filemode |= 0100;
   if(f->save) e_save(f);
   strcpy(estr, f->datnam);
   strcat(estr, " ");
   if(e_prog.arguments) strcat(estr, e_prog.arguments);
#ifdef XWINDOW
   if(e_we_sw & 1)  {  ret = e_x_system(estr);  fk_pointer(LASTCUR);  }
   else
#endif
   ret = e_system(estr, f->ed);
   return(0);
}

/*  Neu Project   */

struct proj_var {  char *var, *string;  }  **p_v = NULL;
int p_v_n = 0;


char *e_interpr_var(string)
     char *string;
{
   int i, j;
   for(i = 0; string[i]; i++)
   {  if(string[i] == '\\') 
		for(j = i; (string[j] = string[j+1]) != '\0'; j++);
      else if(string[i] == '\'' || string[i] == '\"')
      {  for(j = i; (string[j] = string[j+1]) != '\0'; j++);
	 i--;
      }
   }
   return(string);
}

char *e_expand_var(string, f)
     char *string;
     FENSTER *f;
{
   int i, j = 0, k, len, kl = 0;
   char *var = NULL, *v_string, *tmp;
   for(i = 0; string[i]; i++)
   {  if(string[i] == '\'')
      {  kl = kl ? 0 : 1;
	 for(j = i; (string[j] = string[j+1]) != '\0'; j++);
	 i--;  continue;
      }
      if(string[i] == '\\' && (string[i+1] == 'n' || string[i+1] == 'r'))
      {  string[i] = string[i+1] == 'n' ? '\n' : '\r';
	 for(j = i+1; (string[j] = string[j+1]) != '\0'; j++);
	 continue;
      }
      if(string[i] == '$' && !kl && (!i || string[i-1] != '\\'))
      {  if(string[i+1] == '(')
	 {  for(j = i+2; string[j] && string[j] != ')'; j++);
	    if(!string[j]) continue;
	 }
	 else if(string[i+1] == '{')
	 {  for(j = i+2; string[j] && string[j] != '}'; j++);
	    if(!string[j]) continue;
	 }
	 if(string[i+1] == '(' || string[i+1] == '{')
	 {  if(!(var = MALLOC((j-i-1) * sizeof(char))))
	    {  e_error(e_msg[0], 0, f->fb);  return(string);  }
	    for(k = i+2; k < j; k++) var[k-i-2] = string[k];
	    var[k-i-2] = '\0';
	 }
	 else
	 {  if(!(var = MALLOC(2 * sizeof(char))))
	    {  e_error(e_msg[0], 0, f->fb);  return(string);  }
	    var[0] = string[i+1];  var[1] = '\0';
	 }
	 if(!(v_string = getenv(var)))
	 {  for(k = 0; k < p_v_n - 1; k++)
	    {  if(!strcmp(p_v[k]->var, var))
	       {  v_string = p_v[k]->string;  break;  }
	    }
	 }
	 if(string[i+1] == '(' || string[i+1] == '{') len = (j-i+1);
	 else len = 2;
	 if(!v_string)
	 {  for(k = i; (string[k] = string[k+len]) != '\0'; k++);
	    if(!(string = REALLOC(tmp = string, (strlen(string) + 1)
							 * sizeof(char))))
	    {  FREE(var);  e_error(e_msg[0], 0, f->fb);  return(tmp);  }
	 }
	 else
	 {  len = strlen(v_string) - len;
	    if(len >= 0)
	    {  if(!(string = REALLOC(tmp = string,
		      (k = strlen(string) + len + 1) * sizeof(char))))
	       {  FREE(var);  e_error(e_msg[0], 0, f->fb);  return(tmp);  }
	       for(k--; k > j + len; k--)string[k] = string[k-len];
	       for(k = i; v_string[k-i]; k++) string[k] = v_string[k-i];
	    }
	    else
	    {  for(k = i; (string[k] = string[k-len]) != '\0'; k++);
	       for(k = i; v_string[k-i]; k++) string[k] = v_string[k-i];
	       if(!(string = REALLOC(tmp = string,
		      (strlen(string) + 1) * sizeof(char))))
	       {  FREE(var);  e_error(e_msg[0], 0, f->fb);  return(tmp);  }
	    }
	 }
	 FREE(var);
      }
   }
   return(string);
}

int e_read_var(f)
     FENSTER *f;
{
   struct proj_var **tmp;
   FILE *fp;
   char str[256], *sp1, *sp2, *stmp;
   int i, j;
   if((fp = fopen(e_prog.project, "r")) == NULL) return(-1);
/*
   {  sprintf(str, e_msg[6], e_prog.project);
      e_error(str, 0, f->fb);
      return(1);
   }
*/
   if(p_v)
   {  for(i = 0; i < p_v_n; i++)
      {  if(p_v[i])
	 {  if(p_v[i]->var) FREE(p_v[i]->var);
	    if(p_v[i]->string) FREE(p_v[i]->string);
	    FREE(p_v[i]);
	 }
      }
      FREE(p_v);
   }
   p_v_n = 0;
   if(!(p_v = MALLOC(sizeof(struct proj_var *))))
   {  fclose(fp);  e_error(e_msg[0], 0, f->fb);  return(-1);  }
   while(fgets(str, 256, fp))
   {  for(i = 0; isspace(str[i]); i++);
      if(!str[i]) continue;
      else if(str[i] == '#')
      {  while (str[strlen(str)-1] != '\n')
	 {  fgets(str, 256, fp);  }
	 continue;
      }
      sp1 = str+i;
      for(i = 0; isalnum(sp1[i]); i++);
      for(j = i; isspace(sp1[j]); j++);
      if(str[j] != '=') continue;
      for(j++; isspace(sp1[j]) && sp1[j] != '\n'; j++);
      sp2 = sp1 + j;
      sp1[i] = '\0';
      p_v_n++;
      if(!(p_v = REALLOC(tmp = p_v, sizeof(struct proj_var *) * p_v_n)))
      {  p_v = tmp;  fclose(fp);  e_error(e_msg[0], 0, f->fb);  return(-1);  }
      if(!(p_v[p_v_n-1] = MALLOC(sizeof(struct proj_var))))
      {  fclose(fp);  e_error(e_msg[0], 0, f->fb);  return(-1);  }
      if(!(p_v[p_v_n-1]->var = MALLOC((i+1) * sizeof(char))))
      {  fclose(fp);  e_error(e_msg[0], 0, f->fb);  return(-1);  }
      strcpy(p_v[p_v_n-1]->var, sp1);
      if(!(p_v[p_v_n-1]->string = MALLOC((strlen(sp2)+1) * sizeof(char))))
      {  fclose(fp);  e_error(e_msg[0], 0, f->fb);  return(-1);  }
      strcpy(p_v[p_v_n-1]->string, sp2);
      while(p_v[p_v_n-1]->string[i = strlen(p_v[p_v_n-1]->string) - 1]
			!= '\n' || p_v[p_v_n-1]->string[i-1] == '\\')
      {  if(p_v[p_v_n-1]->string[i-1] == '\\') p_v[p_v_n-1]->string[i-1] = '\0';
	 if(!fgets(str, 256, fp)) break;
	 if(!(p_v[p_v_n-1]->string = REALLOC(stmp = p_v[p_v_n-1]->string,
		(strlen(p_v[p_v_n-1]->string)+strlen(str)+1) * sizeof(char))))
	 {  p_v[p_v_n-1]->string = stmp;
	    fclose(fp);  e_error(e_msg[0], 0, f->fb);  return(-1);  }
	 strcat(p_v[p_v_n-1]->string, str);
      }
      p_v[p_v_n-1]->string[strlen(p_v[p_v_n-1]->string) - 1] = '\0';
      for(i = 0; p_v[p_v_n-1]->string[i]; i++)
      if(p_v[p_v_n-1]->string[i] == '\t') p_v[p_v_n-1]->string[i] = ' ';
      p_v[p_v_n-1]->string = e_expand_var(p_v[p_v_n-1]->string, f);
   }
   fclose(fp);
   return(0);
}

int e_install(f)
     FENSTER *f;
{
   char *tp, *sp, *string, *tmp, text[256];
   FILE *fp;
   int i, j;
   if(e_p_make(f)) return(-1);
   if(!e__project) return(0);
   if((fp = fopen(e_prog.project, "r")) == NULL)
   {  sprintf(text, e_msg[6], e_prog.project);
      e_error(text, 0, f->fb);
      return(ESC);
   }
   while((tp = fgets(text, 256, fp)))
   {  if(text[0] == '\t') continue;
      for(i = 0; isspace(text[i]); i++);
      if(!strncmp(text+i, "install:", 8))
      {  while(tp && (text[j = strlen(text)-1] != '\n'
			|| text[j-1] == '\\'))
	 tp = fgets(text, 256, fp);
	 break;
      }
   }
   if(!tp) {  fclose(fp);  return(1);  }
   while(tp && (tp = fgets(text, 256, fp)))
   {  for(i = 0; isspace(text[i]); i++);
      sp = text+i;
      if(sp[0] == '#')
      {  while(tp && (text[j = strlen(text)-1] != '\n'
			|| text[j-1] == '\\'))
	 tp = fgets(text, 256, fp);
	 continue;
      }
      if(text[0] != '\t') break;
      if(!(string = MALLOC(strlen(sp) + 1)))
      {  fclose(fp);  e_error(e_msg[0], 0, f->fb);  return(-1);  }
      strcpy(string, sp);
      while(tp && (text[j = strlen(text)-1] != '\n'
						|| text[j-1] == '\\'))
      {  tp = fgets(text, 256, fp);
	 if(tp)
	 {  if(!(string = REALLOC(tmp = string,
				strlen(string) + strlen(text) + 1)))
	    {  fclose(fp);  FREE(tmp);  e_error(e_msg[0], 0, f->fb);
	       return(-1);
	    }
	    strcat(string, text);
	 }
      }
      if(p_v_n) p_v_n++;
      string = e_expand_var(string, f);
      if(p_v_n) p_v_n--;
      e_d_p_message(string, f, 1);
      system(string);
      FREE(string);
   }
   fclose(fp);
   return(0);
}
/*
int e_install(FENSTER *f)
{
    int i;
    e_read_var(f);
    for(i = 0; i < p_v_n; i++)
    {	e_d_p_message(p_v[i]->var, f);
        e_d_p_message("=", f);
        e_d_p_message(p_v[i]->string, f);
    }
    return(0);
}
*/
struct dirfile *e_p_get_args(string)
     char *string;
{
   struct dirfile *df = MALLOC(sizeof(struct dirfile));
   char **tmp;
   int i, j, k;
   if(!df) return(NULL);
   if(!(df->name = MALLOC(sizeof(char *))))
   {  FREE(df); return(NULL);  }
   df->anz = 0;
   for(i = 0; string[i]; )
   {  for(; isspace(string[i]); i++);
      for(j = i; string[j] && !isspace(string[j]); j++);
      if(j == i) break;
      df->anz++;
      if(!(df->name = REALLOC(tmp = df->name, df->anz * sizeof(char *))))
      {  df->anz--;  df->name = tmp;  return(df);  }
      if(!(df->name[df->anz-1] = MALLOC((j-i+1)*sizeof(char))))
      {  df->anz--;  return(df);  }
      for(k = i; k < j; k++) *(df->name[df->anz-1] + k - i) = string[k];
      *(df->name[df->anz-1] + k - i) = '\0';
      e_interpr_var(df->name[df->anz-1]);
      i = j;
   }
   return(df);
}

struct dirfile *e_p_get_var(string)
     char *string;
{
   int i;
   for(i = 0; i < p_v_n; i++)
   {  if(!strcmp(p_v[i]->var, string))
      return(e_p_get_args(p_v[i]->string));
   }
   return(NULL);
}

int e_c_project(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   struct dirfile *df = NULL;
#ifdef NOANSI
   char **arg;
#else
   char **arg;
#endif
   int i, j, k, file= -1, len, elen, argc, libsw = 0, exlib = 0, sccs = 0;
   char ofile[128];
#ifdef CHECKHEADER
   struct stat lbuf[1], obuf[1];
#else
   struct stat lbuf[1], cbuf[1], obuf[1];
#endif
   PIC *pic = NULL;
   last_time = (M_TIME) 0;
   e_p_l_comp = 0;
   if(e_new_message(f)) return(ESC);
   f = cn->f[cn->mxedt];
   FREE(e_s_prog.comp_str);
   e_s_prog.comp_str = NULL;
   e_s_prog.comp_sw &= ~1;
   e_argc = 6;
   argc = 5;
   for(i = f->ed->mxedt; i > 0
	&& (f->ed->f[i]->dtmd != 'D' || f->ed->f[i]->ins != 4
				     || !f->ed->f[i]->save); i--);
   if(i > 0) e_p_update_prj_fl(f);
   if(e_read_var(f))
   {  sprintf(ofile, e_msg[6], e_prog.project);
      e_error(ofile, 0, f->fb);
      return(-1);
   }
   e_arg = (char **) MALLOC(e_argc*sizeof(char *));
   arg = (char **) MALLOC(argc*sizeof(char *));
   for(i = 0; i < 7; i++)
   {  switch(i)
      {  case(0):
	    df = e_p_get_var("CMP");
	    if(!df)
	    {  e_error(e_p_msg[8], 0, f->fb);
	       e_free_arg(arg, argc);  e_free_arg(e_arg, e_argc);
	       return(-1);
	    }
	 case(1):
	    if(i == 1) df = e_p_get_var("CMPFLAGS");
	    if(df)
	    {  if(i == 0) {  e_argc = 1; argc = 1;  }
	       for(k = 0; k < df->anz; k++, e_argc++, argc++)
	       {  j = e_argc == 1 ? 1 : 0;
		  e_arg = REALLOC(e_arg, (e_argc+2)*sizeof(char *));
		  e_arg[e_argc-j] = MALLOC(strlen(df->name[k]) + 1);
		  strcpy(e_arg[e_argc-j], df->name[k]);
		  arg = REALLOC(arg, (argc+2)*sizeof(char *));
		  arg[argc-j] = MALLOC(strlen(df->name[k]) + 1);
		  strcpy(arg[argc-j], df->name[k]);
		  if(e_argc > 1) e_s_prog.comp_str =
			e_cat_string(e_s_prog.comp_str, e_arg[e_argc-j]);
	       }
	       freedf(df);
	    }
	    else if(i == 0)
	    {  argc = e_make_arg(&arg, e_s_prog.comp_str);
	       e_argc = e_make_arg(&e_arg, e_s_prog.comp_str);
	    }
	    if(i == 0)
	    {  arg[1] = MALLOC(3);
	       strcpy(arg[1], "-c");
	       e_arg[1] = MALLOC(3);
	       strcpy(e_arg[1], "-o");
	    }
	    break;
	 case(2):
	    df = e_p_get_var("EXENAME");
	    elen = strlen(e_prog.exedir)-1;
#ifdef DJGPP
	    if(e_prog.exedir[elen] == DIRC || e_prog.exedir[elen] == '/')
#else
	    if(e_prog.exedir[elen] == '/')
#endif
	    sprintf(ofile, "%s%s", e_prog.exedir,
			(df && df->name[0][0]) ? df->name[0] : "a.out");
	    else
	    sprintf(ofile, "%s/%s", e_prog.exedir,
			(df && df->name[0][0]) ? df->name[0] : "a.out");
	    if(df) freedf(df);
	    e_s_prog.exe_name = e_make_string(e_s_prog.exe_name, ofile);
	    e_argc = e_add_arg(&e_arg, e_s_prog.exe_name, 2, e_argc);
	    break;
	 case(3):
	    df = e_p_get_var("LIBNAME");
	    if(df)
	    {  strcpy(library, df->name[0]);
	       if(access(library, 0)) exlib = 1;
	       else stat(library, lbuf);
	       freedf(df);
	    }
	    else
	    library[0] = '\0';
	    break;
	 case(4):
	    df = e_p_get_var("CMPSWTCH");
	    if(df)
	    {  if(!strcmp(df->name[0], "other"))
	       e_s_prog.comp_sw = 1;
	       freedf(df);
	    }
	    df = e_p_get_var("CMPMESSAGE");
	    if(df)
	    {  char *tmpstr = MALLOC(1);
	       tmpstr[0] = '\0';
	       for(k = 0; k < df->anz; k++)
	       {  tmpstr = REALLOC(tmpstr,
			(strlen(tmpstr)+strlen(df->name[k])+2)*sizeof(char));
		  if(k) strcat(tmpstr, " ");
		  strcat(tmpstr, df->name[k]);
	       }
	       e_s_prog.intstr = e_make_string(e_s_prog.intstr, tmpstr);
	       FREE(tmpstr);
	       freedf(df);
	    }
	    else e_s_prog.intstr = e_make_string(e_s_prog.intstr, cc_intstr);
	    break;
	 case(5):
	    df = e_p_get_var("FILES");
	    if(!df)
	    {  e_error(e_p_msg[8], 0, f->fb);
	       e_free_arg(arg, argc);  e_free_arg(e_arg, e_argc);
	       return(-1);
	    }
	    arg[argc] = NULL;
	    elen = strlen(e_prog.exedir)-1;
	    for(k = 0; k < df->anz; k++)
	    {  for(j = cn->mxedt; j > 0; j--)
	       if(!strcmp(cn->f[j]->datnam, df->name[k]) && cn->f[j]->save)
	       e_save(cn->f[j]);
#ifdef DJGPP
	       for(j = strlen(df->name[k])-1; j >= 0 && df->name[k][j] != DIRC
                  && df->name[k][j] != '/' && df->name[k][j] != ':'; j--);
	       if(e_prog.exedir[elen] == DIRC || e_prog.exedir[elen] == '/')
#else
	       for(j = strlen(df->name[k])-1;
			j >= 0 && df->name[k][j] != DIRC; j--);
	       if(e_prog.exedir[elen] == '/')
#endif
                  sprintf(ofile, "%s%s ", e_prog.exedir, df->name[k]+j+1);
	       else sprintf(ofile, "%s/%s ", e_prog.exedir, df->name[k]+j+1);
	       for(j = strlen(ofile); j > 0 && ofile[j] != '.'; j--);
	       ofile[j+1] = 'o';
	       ofile[j+2] = '\0';
	       if(!stat(ofile, obuf))
	       {  if(obuf->st_mtime > last_time) last_time = obuf->st_mtime;
#ifdef CHECKHEADER
		  if(!e_check_header(df->name[k], obuf->st_mtime, cn, 0)) goto gt_library;
#else
		  stat(df->name[k], cbuf);
		  if(obuf->st_mtime >= cbuf->st_mtime) goto gt_library;
#endif
	       }
#ifdef CHECKHEADER
	       else e_check_header(df->name[k], (M_TIME) 0, cn, 0);
#endif
	       argc = e_add_arg(&arg, df->name[k], argc, argc);
#ifndef NOOPTFILEDIR
	       argc = e_add_arg(&arg, "-o", argc, argc);
	       argc = e_add_arg(&arg, ofile, argc, argc);
#endif
	       arg[argc] = NULL;
	       remove(ofile);
               sccs = 1;
	       j = e_p_mess_win("Compiling", argc, arg, &pic, f);
	       e_sys_ini();
	       if(j != 0 || (file = e_exec_inf(f, arg, argc)) == 0)
	       {  e_sys_end();  e_free_arg(arg, argc);  freedf(df);
		  e_free_arg(e_arg, e_argc);  if(pic) e_close_view(pic, 1);
		  return(ESC);
	       }
	       e_sys_end();
	       e_p_l_comp = 1;
	       if(e_p_exec(file, f, pic))
	       {  e_free_arg(arg, argc); e_free_arg(e_arg, e_argc);
		  freedf(df);  return(-1);  }
	       pic = NULL;
	       for(j = strlen(ofile); j >= 0 && ofile[j] != '/'; j--);
	       if(!exlib && library[0] != '\0' && strcmp(ofile+j+1, "main.o") &&
	   	(strncmp(e_s_prog.exe_name, ofile+j+1,
		   (len = strlen(e_s_prog.exe_name))) || ofile[len] == '.'))
               {   if(e_make_library(library, ofile, f))
                   {  e_free_arg(arg, argc);
                      e_free_arg(e_arg, e_argc); freedf(df); return(-1);  
                   }
                   else libsw = 1;
               }
	       for(j = 0; j < 3; j++) FREE(arg[argc-j-1]);
	       argc -= 3;
	       gt_library:
	       for(j = strlen(ofile); j >= 0 && ofile[j] != '/'; j--);
	       if(library[0] == '\0' || !strcmp(ofile+j+1, "main.o") ||
	   	(!strncmp(e_s_prog.exe_name, ofile+j+1,
		   (len = strlen(e_s_prog.exe_name))) && ofile[len] == '.'))
                   e_argc = e_add_arg(&e_arg, ofile, e_argc, e_argc);
	       else if(exlib || obuf->st_mtime >= lbuf->st_mtime)
	       {  if(e_make_library(library, ofile, f))
		  {  e_free_arg(arg, argc);
		     e_free_arg(e_arg, e_argc); freedf(df); return(-1);
		  }
		  else libsw = 1;
	       }
	    }
#ifdef RANLIB
	    if(libsw && library[0] != '\0')
	    {  char *ar_arg[3];
	       ar_arg[0] = "ranlib";
	       ar_arg[1] = library;
	       ar_arg[2] = NULL;
	       if(!(j = e_p_mess_win("Convert Archive", 2, ar_arg, &pic, f)))
	       {  e_sys_ini();
		  file = e_exec_inf(f, ar_arg, 2);
		  e_sys_end();
		  if(file) j = e_p_exec(file, f, pic);
	       }
	       if(j || !file)
	       {  e_free_arg(arg, argc);
		  e_free_arg(e_arg, e_argc); freedf(df); return(-1);
	       }
	    }
#endif
	    if(library[0] != '\0')
	    e_argc = e_add_arg(&e_arg, library, e_argc, e_argc);
	    freedf(df);
	    break;
	 case(6):
	    df = e_p_get_var("LDFLAGS");
	    if(!df) break;
	    FREE(e_s_prog.libraries);
	    e_s_prog.libraries = NULL;
	    for(k = 0; k < df->anz; k++, e_argc++)
	    {  e_arg = REALLOC(e_arg, (e_argc+2)*sizeof(char *));
	       e_arg[e_argc] = MALLOC(strlen(df->name[k]) + 1);
	       strcpy(e_arg[e_argc], df->name[k]);
	       e_s_prog.libraries = e_cat_string(e_s_prog.libraries, e_arg[e_argc]);
	    }
	    freedf(df);
	    break;
      }
   }
   e_arg[e_argc] = NULL;
   e_free_arg(arg, argc);
   if(!sccs) e_p_exec(file, f, pic);
   return(0);
}

int e_free_arg(arg, argc)
     char **arg;
     int argc;
{
   int i;
   for(i = 0; i < argc; i++) if(arg[i]) FREE(arg[i]);
   FREE(arg);
   return(i);
}

char *e_find_var(var)
     char *var;
{
   int i;
   for(i = 0; i < p_v_n && strcmp(p_v[i]->var, var); i++);
   if(i >= p_v_n) return(NULL);
   else return(p_v[i]->string);
}

struct dirfile **e_make_prj_opt(f)
     FENSTER *f;
{
   int i, j, ret;
   char **tmp, *sp, *tp, text[256];
   FILE *fp;
   struct dirfile *save_df = NULL;
   for(i = f->ed->mxedt; i > 0
	&& (f->ed->f[i]->dtmd != 'D' || f->ed->f[i]->ins != 4
				     || !f->ed->f[i]->save); i--);
   if(i > 0) {  save_df = e_p_df[0];  e_p_df[0] = NULL;  }
   if(e_p_df) freedfN(e_p_df, 3);
   e_p_df = MALLOC(3 * sizeof(struct dirfile *));
   if(!e_p_df) return(e_p_df);
   for(i = 0; i < 3; i++) e_p_df[i] = NULL;
   e_s_prog.comp_sw = 0;
   ret = e_read_var(f);
   if(ret)
   {  e_s_prog.compiler = e_make_string(e_s_prog.compiler, "gcc");
      e_s_prog.comp_str = e_make_string(e_s_prog.comp_str, "-g");
      e_s_prog.libraries = e_make_string(e_s_prog.libraries, "");
      e_s_prog.exe_name = e_make_string(e_s_prog.exe_name, "a.out");
      e_s_prog.intstr = e_make_string(e_s_prog.intstr, cc_intstr);
      strcpy(library, "");
      for(i = !save_df ? 0 : 1; i < 3; i++)
      {  e_p_df[i] = MALLOC(sizeof(struct dirfile));
	 e_p_df[i]->name = MALLOC(sizeof(char *));
	 e_p_df[i]->name[0] = MALLOC(2 * sizeof(char));
	 *e_p_df[i]->name[0] = ' '; *(e_p_df[i]->name[0] + 1) = '\0';
	 e_p_df[i]->anz = 1;
      }
      if(save_df) e_p_df[0] = save_df;
      return(e_p_df);
   }
   if(!(e_p_df[1] = MALLOC(sizeof(struct dirfile)))) return(e_p_df);
   if(!(e_p_df[1]->name = MALLOC(sizeof(char *)))) return(e_p_df);
   e_p_df[1]->anz = 0;
   if(!(e_p_df[2] = MALLOC(sizeof(struct dirfile)))) return(e_p_df);
   if(!(e_p_df[2]->name = MALLOC(sizeof(char *)))) return(e_p_df);
   e_p_df[2]->anz = 0;
   for(i = 0; i < p_v_n; i++)
   {  if(!strcmp(p_v[i]->var, "CMP"))
      e_s_prog.compiler = e_make_string(e_s_prog.compiler, p_v[i]->string);
      else if(!strcmp(p_v[i]->var, "CMPFLAGS"))
      e_s_prog.comp_str = e_make_string(e_s_prog.comp_str, p_v[i]->string);
      else if(!strcmp(p_v[i]->var, "LDFLAGS"))
      e_s_prog.libraries = e_make_string(e_s_prog.libraries, p_v[i]->string);
      else if(!strcmp(p_v[i]->var, "EXENAME"))
      e_s_prog.exe_name = e_make_string(e_s_prog.exe_name, p_v[i]->string);
      else if(!strcmp(p_v[i]->var, "CMPMESSAGE"))
      e_s_prog.intstr = e_make_string(e_s_prog.intstr,
					e_interpr_var(p_v[i]->string));
      else if(!strcmp(p_v[i]->var, "LIBNAME"))
      strcpy(library, p_v[i]->string);
      else if(!strcmp(p_v[i]->var, "CMPSWTCH"))
      {  if(!strcmp(p_v[i]->string, "other")) e_s_prog.comp_sw = 1;
      }
      else if(!strcmp(p_v[i]->var, "FILES"))
      e_p_df[0] = e_p_get_args(p_v[i]->string);
      else
      {  e_p_df[1]->anz++;
	 if(!(e_p_df[1]->name = REALLOC(tmp =
				e_p_df[1]->name, e_p_df[1]->anz * sizeof(char *))))
	 {  e_p_df[1]->anz--;  e_p_df[1]->name = tmp;  return(e_p_df);  }
	 if(!(e_p_df[1]->name[e_p_df[1]->anz-1] = MALLOC((strlen(p_v[i]->var)
			+ strlen(p_v[i]->string) + 2)*sizeof(char))))
	 {  e_p_df[1]->anz--;  return(e_p_df);  }
	 sprintf(e_p_df[1]->name[e_p_df[1]->anz-1], "%s=%s",
					p_v[i]->var, p_v[i]->string);
      }
   }
   if(!e_s_prog.compiler)
   e_s_prog.compiler = e_make_string(e_s_prog.compiler, "gcc");
   if(!e_s_prog.comp_str)
   e_s_prog.comp_str = e_make_string(e_s_prog.comp_str, "-g");
   if(!e_s_prog.libraries)
   e_s_prog.libraries = e_make_string(e_s_prog.libraries, "");
   if(!e_s_prog.exe_name)
   e_s_prog.exe_name = e_make_string(e_s_prog.exe_name, "a.out");
   if(!e_s_prog.intstr)
   e_s_prog.intstr = e_make_string(e_s_prog.intstr, cc_intstr);
   if(!e_p_df[0])
   {  e_p_df[0] = MALLOC(sizeof(struct dirfile));
      e_p_df[0]->anz = 0;
   }
   if((fp = fopen(e_prog.project, "r")) == NULL)
   {  sprintf(text, e_msg[6], e_prog.project);
      e_error(text, 0, f->fb);
      return(e_p_df);
   }
   while((tp = fgets(text, 256, fp)))
   {  if(text[0] == '\t') continue;
      for(i = 0; isspace(text[i]); i++);
      if(!strncmp(text+i, "install:", 8))
      {  while(tp && (text[j = strlen(text)-1] != '\n'
			|| text[j-1] == '\\'))
	 tp = fgets(text, 256, fp);
	 break;
      }
   }
   if(!tp) {  fclose(fp);  return(e_p_df);  }
   while(tp && (tp = fgets(text, 256, fp)))
   {  for(i = 0; isspace(text[i]); i++);
      sp = text+i;
      if(sp[0] == '#')
      {  while(tp && (text[j = strlen(text)-1] != '\n'
			|| text[j-1] == '\\'))
	 tp = fgets(text, 256, fp);
	 continue;
      }
      if(text[0] != '\t') break;
      if(sp[0] == '\0') continue;
      e_p_df[2]->anz++;
      if(!(e_p_df[2]->name = REALLOC(tmp =
				e_p_df[2]->name, e_p_df[2]->anz * sizeof(char *))))
      {  e_p_df[2]->anz--;  e_p_df[2]->name = tmp;  fclose(fp);  return(e_p_df);  }
      if(!(e_p_df[2]->name[e_p_df[2]->anz-1] = MALLOC((strlen(sp) + 1))))
      {  e_p_df[2]->anz--;  fclose(fp);  return(e_p_df);  }
      
      strcpy(e_p_df[2]->name[e_p_df[2]->anz-1], sp);
      while(tp && (text[j = strlen(text)-1] != '\n'
						|| text[j-1] == '\\'))
      {  tp = fgets(text, 256, fp);
	 if(tp)
	 {  j = strlen(e_p_df[2]->name[e_p_df[2]->anz-1]);
	    *(e_p_df[2]->name[e_p_df[2]->anz-1]+j-2) = '\0';
	    if(!(e_p_df[2]->name[e_p_df[2]->anz-1] =
		    REALLOC(sp = e_p_df[2]->name[e_p_df[2]->anz-1],
			strlen(e_p_df[2]->name[e_p_df[2]->anz-1])
			+ strlen(text) + 1)))
	    {  fclose(fp);  FREE(sp);  e_error(e_msg[0], 0, f->fb);
	       return(e_p_df);
	    }
	    strcat(e_p_df[2]->name[e_p_df[2]->anz-1], text);
	 }
      }
      j = strlen(e_p_df[2]->name[e_p_df[2]->anz-1]);
      if(*(e_p_df[2]->name[e_p_df[2]->anz-1]+j-1) == '\n')
		      *(e_p_df[2]->name[e_p_df[2]->anz-1]+j-1) = '\0';
   }
   fclose(fp);
   for(i = 0; i < 3; i++)
   {  if(!e_p_df[i])
      {  e_p_df[i] = MALLOC(sizeof(struct dirfile));
	 e_p_df[i]->name = MALLOC(sizeof(char *));
	 e_p_df[i]->anz = 0;
      }
      e_p_df[i]->name = REALLOC(e_p_df[i]->name,
				(e_p_df[i]->anz + 1) * sizeof(char *));
      e_p_df[i]->name[e_p_df[i]->anz] = MALLOC(2*sizeof(char));
      *e_p_df[i]->name[e_p_df[i]->anz] = ' ';
      *(e_p_df[i]->name[e_p_df[i]->anz] + 1) = '\0';
      e_p_df[i]->anz++;
   }
   if(save_df) {  freedf(e_p_df[0]);  e_p_df[0] = save_df;  }
   return(e_p_df);
}

int freedfN(df, n)
     struct dirfile **df;
     int n;
{
   int i;
   for(i = 0; i < n; i++) if(df[i]) freedf(df[i]);
   FREE(df);
   df = NULL;
   return(0);
}

int e_wrt_prj_fl(f)
     FENSTER *f;
{
   int i, len;
   FILE *fp;
   char text[256];
   for(i = f->ed->mxedt; i > 0 && 
      (f->ed->f[i]->dtmd != 'D' || f->ed->f[i]->ins != 4); i--);
#ifdef DJGPP
   if(i == 0 || e_prog.project[0] == DIRC || e_prog.project[0] == '/'
      || e_prog.project[1] == ':')
#else
   if(i == 0 || e_prog.project[0] == DIRC)
#endif
      strcpy(text, e_prog.project);
   else
      sprintf(text, "%s/%s", f->ed->f[i]->dirct, e_prog.project);
   if((fp = fopen(text, "w")) == NULL)
   {  sprintf(text, e_msg[6], e_prog.project);
      e_error(text, 0, f->fb);
      return(-1);
   }
   fprintf(fp, "#\n# xwpe - project-file: %s\n", e_prog.project);
   fprintf(fp, "# createted by xwpe version %s\n#\n", VERSION);
   for(i = 0; i < e_p_df[1]->anz; i++)
   fprintf(fp, "%s\n", e_p_df[1]->name[i]);
   fprintf(fp, "\nCMP=\t%s\n", e_s_prog.compiler);
   fprintf(fp, "CMPFLAGS=\t%s\n", e_s_prog.comp_str);
   fprintf(fp, "LDFLAGS=\t%s\n", e_s_prog.libraries);
   fprintf(fp, "EXENAME=\t%s\n", e_s_prog.exe_name);
   if(library[0]) fprintf(fp, "LIBNAME=\t%s\n", library);
   fprintf(fp, "CMPSWTCH=\t%s\n", e_s_prog.comp_sw ? "other" : "gnu");
   fprintf(fp, "CMPMESSAGE=\t\'");
   for(i = 0; e_s_prog.intstr[i]; i++)
   {  if(e_s_prog.intstr[i] == '\n') fprintf(fp, "\\n");
      else if(e_s_prog.intstr[i] == '\r') fprintf(fp, "\\r");
      else if(e_s_prog.intstr[i] == '\\' || e_s_prog.intstr[i] == '\''
		|| e_s_prog.intstr[i] == '\"' )
      {  fputc('\\', fp);  fputc(e_s_prog.intstr[i], fp);  }
      else fputc(e_s_prog.intstr[i], fp);
   }
   fprintf(fp, "\'\n");
   fprintf(fp, "\nFILES=\t");
   for(i = 0, len = 8; i < e_p_df[0]->anz; i++)
   {  len += strlen(e_p_df[0]->name[i]);
      if(len > 80)  {  fprintf(fp, " \\\n\t");  len = 1;  }
      fprintf(fp, "%s ", e_p_df[0]->name[i]);
   }
   fprintf(fp, "\n");
   if(e_p_df[2]->anz > 0) fprintf(fp, "\ninstall:\n");
   for(i = 0; i < e_p_df[2]->anz; i++)
   fprintf(fp, "\t%s\n", e_p_df[2]->name[i]);
   fclose(fp);
   return(0);
}

int e_p_update_prj_fl(f)
     FENSTER *f;
{
   if(!e_make_prj_opt(f)) return(-1);
   if(e_wrt_prj_fl(f)) return(-1);
   return(0);
}

int e_p_add_df(fw, sw)
     FLWND *fw;
     int sw;
{
   char *title = NULL, str[256];
   int i;
   if(sw == 4) title = "Add File";
   else if(sw == 5) title = "Add Variable";
   else if(sw == 6) title = "Add Command";
   if(e_add_arguments(str, title, fw->f, 0, AltA, NULL))
   {  fw->df->anz++;
      fw->df->name = REALLOC(fw->df->name, fw->df->anz * sizeof(char *));
      for(i = fw->df->anz - 1; i > fw->nf; i--)
      fw->df->name[i] = fw->df->name[i-1];
      fw->df->name[i] = MALLOC(strlen(str)+1);
      strcpy(fw->df->name[i], str);
   }
   return(0);
}

int e_p_edit_df(fw, sw)
     FLWND *fw;
     int sw;
{
   char *title = NULL, str[256];
   int new = 0;
   if(sw == 4) title = "Change Filename";
   else if(sw == 5) title = "Change Variable";
   else if(sw == 6) title = "Change Command";
   if(fw->nf < fw->df->anz-1 && fw->df->name[fw->nf])
   strcpy(str, fw->df->name[fw->nf]);
   else new = 1;
   if(e_add_arguments(str, title, fw->f, 0, AltA, NULL))
   {  if(fw->nf > fw->df->anz-2)
      {  fw->nf = fw->df->anz-1;
	 fw->df->anz++;
	 fw->df->name = REALLOC(fw->df->name, fw->df->anz * sizeof(char *));
	 fw->df->name[fw->df->anz-1] = fw->df->name[fw->df->anz-2];
      }
      if(!new) FREE(fw->df->name[fw->nf]);
      fw->df->name[fw->nf] = MALLOC(strlen(str)+1);
      if(fw->df->name[fw->nf])strcpy(fw->df->name[fw->nf], str);
   }
   return(0);
}

int e_p_del_df(fw, sw)
     FLWND *fw;
     int sw;
{
   int i;
   if(fw->nf > fw->df->anz-2) return(0);
   fw->df->anz--;
   for(i = fw->nf; i < fw->df->anz; i++)
   fw->df->name[i] = fw->df->name[i+1];
   return(0);
}

int e_p_mess_win(header, argc, argv, pic, f)
     char *header;
     int argc;
     char **argv;
     PIC **pic;
     FENSTER *f;
{
   char *tmp = MALLOC(sizeof(char));
   int i, ret;
   fk_cursor(0);
   tmp[0] = '\0';
   for(i = 0; i < argc && argv[i] != NULL; i++)
   {  if(!(tmp = REALLOC(tmp, (strlen(tmp)+strlen(argv[i])+2)*sizeof(char))))
      return(-2);
      strcat(tmp, argv[i]);
      strcat(tmp, " ");
   }
   ret = e_mess_win(header, tmp, pic, f);
   FREE(tmp);
   fk_cursor(1);
   return(ret);
}

int e_p_red_buffer(b)
     BUFFER *b;
{
   int i;
   for(i = 1; i < b->mxlines; i++)
   if(b->bf[i].s != NULL) FREE( b->bf[i].s );
   b->bf[0].s[0] = WR;
   b->bf[0].s[1] = '\0';
   b->bf[0].len = 0;
   b->bf[0].nrc = 1;
   b->mxlines = 1;
   return(0);
}

int e_new_message(f)
     FENSTER *f;
{
   int i;
   if(e_p_m_buffer) e_p_red_buffer(e_p_m_buffer);
   for(i = f->ed->mxedt; i > 0; i--)
   if(!strcmp(f->ed->f[i]->datnam, "Messages"))
   {  e_switch_window(f->ed->edt[i], f->ed->f[f->ed->mxedt]);
      e_close_window(f->ed->f[f->ed->mxedt]);
   }
   if(access("Messages", 0) == 0) remove("Messages");
   if(e_edit(f->ed, "Messages")) return(ESC);
   return(0);
}

int e_p_show_messages(f)
     FENSTER *f;
{
   int i;
   for(i = f->ed->mxedt; i > 0; i--)
   if(!strcmp(f->ed->f[i]->datnam, "Messages"))
   {  e_switch_window(f->ed->edt[i], f->ed->f[f->ed->mxedt]);
      break;
   }
   if(i <= 0 && e_edit(f->ed, "Messages")) {  return(-1);  }
   f = f->ed->f[f->ed->mxedt];
   if(f->b->mxlines == 0)
   {  e_new_line(0, f->b);
      e_ins_nchar(f->b, f->s, "No Messages", 0, 0, 11);
      e_schirm(f, 1);
   }
   return(0);
}

#ifdef NEWCOMP

int e_p_konv_mess(var, str, txt, file, cmp, y, x)
     char *var;
     char *str;
     char *txt;
     char *file;
     char *cmp;
     int *y;
     int *x;
{
   int i;
   char *cp;
   if(!strncmp(var, "FILE", 4) && !isalnum(var[4]))
   {  for(i = strlen(str) - 1; i >= 0 && !isspace(str[i]); i--);
      strcpy(file, str+i+1);
   }
   else if(!strncmp(var, "CMPTEXT", 7) && !isalnum(var[7])) strcpy(cmp, str);
   else if(!strncmp(var, "LINE", 4) && !isalnum(var[4]))
   {  if(!isdigit(str[0])) return(1);
      *y = atoi(str);
      if(var[4] == '+') *y += atoi(var+5);
      else if(var[4] == '-') *y -= atoi(var+5);
   }
   else if(!strncmp(var, "COLUMN", 6) && !isalnum(var[6]))
   {  if(!strncmp(var+6, "=BEFORE", 7))
      {  txt[0] = 'B';
	 strcpy(txt+1, str);
	 *x = 0;
	 var += 13;
      }
      else if(!strncmp(var+6, "=AFTER", 6))
      {  txt[0] = 'A';
	 strcpy(txt+1, str);
	 *x = strlen(str);
	 var += 12;
      }
      else if(!strncmp(var+6, "=PREVIOUS?", 10))
      {  if(!str[0]) return(1);
	 for(i = 0; (txt[i] = var[16+i])
			&& txt[i] != '+' && txt[i] != '-'; i++);
	 txt[i] = '\0';
	 var += (16+i);
	 cp = strstr(str, txt);
	 for(i = 0; str+i < cp; i++);
	 *x = i;
	 txt[0] = 'P'; txt[1] = '\0';
      }
      else if(!isdigit(str[0])) return(1);
      else
      {  *x = atoi(str);
	 txt[0] = '\0';
	 var += 6;
      }
      if(var[0] == '+') *x += atoi(var+1);
      else if(var[0] == '-') *x -= atoi(var+1);
   }
   return(0);
}

int e_p_comp_mess(a, b, c, txt, file, cmp, y, x)
     char *a;
     char *b;
     char *c;
     char *txt;
     char *file;
     char *cmp;
     int *y;
     int *x;
{
   int i, n, k = 0, bsl = 0;
   char *ctmp, *cp, *var = NULL, *str = NULL;
   if(c > b) return(0);
   if(a[0] == '*' && !a[1]) return(2);
   if(!a[0] && !b[0]) return(2);
   if(!a[0] || !b[0]) return(0);
   if(a[0] == '*' && (a[1] == '*' || a[1] == '$'))
	return(e_p_comp_mess(++a, b, c, txt, file, cmp, y, x));
   if(a[0] == '$' && a[1] == '{')
   {  for(k = 2; a[k] && a[k] != '}'; k++);
      var = MALLOC((k-1) * sizeof(char));
      for(i = 2; i < k; i++) var[i-2] = a[i];
      var[k-2] = '\0';
      if(a[k]) k++;
      if(!a[k]) return(!e_p_konv_mess(var, b, txt, file, cmp, y, x));
      n = a[k] == '\\' ? k : k+1;
   }
   else if(a[0] == '*'&& a[1] != '\\') {  k = 1; n = 2;  }
   else n = 1;
   for(; bsl || (a[n] && a[n] != '*' && a[n] != '?' && a[n] != '['
			&& (a[n] != '$' || a[n+1] != '{' )); n++)
   bsl = a[n] == '\\' ? !bsl : 0;
   if(a[0] == '*' || a[0] == '$')
   {  if(a[k] == '?')
      {  cp = MALLOC((strlen(a)+1)*sizeof(char));
	 for(i = 0; i < k && (cp[i] = a[i]); i++);
	 for(i++; (cp[i-1] = a[i]) != '\0'; i++);
	 FREE(var);
	 n = e_p_comp_mess(cp, ++b, ++c, txt, file, cmp, y, x);
	 FREE(cp);
	 return(n);
      }
      if(a[k] == '[')
      {  for(i = 0; b[i] &&
		!(n = e_p_comp_mess(a+k, b+i, c+i, txt, file, cmp, y, x)); i++);
	 if(!b[i]) return(0);
	 if(a[0] == '$')
	 {  str = MALLOC((i+1)*sizeof(char));
	    for(k = 0; k < i; k++) str[k] = b[k];
	    str[i] = '\0';
	    e_p_konv_mess(var, str, txt, file, cmp, y, x);
	    FREE(var);
	    FREE(str);
	 }
	 return(n);
      }
      n -= k;
      ctmp = MALLOC(n+1);
      for(i = 0; i < n; i++) ctmp[i] = a[i+k];
      ctmp[n] = '\0';
      cp = strstr(b, ctmp);
      FREE(ctmp);
      if(cp == NULL) return(0);
      if(a[0] == '$')
      {  for(i = 0; c + i < cp; i++);
	 str = MALLOC((i+1)*sizeof(char));
	 for(i = 0; c + i < cp; i++) str[i] = c[i];
	 str[i] = '\0';
	 i = e_p_konv_mess(var, str, txt, file, cmp, y, x);
	 FREE(var);  
	 FREE(str);
	 if(i) return(0);
      }
      if(!a[k+n] && !cp[n]) return(2);
      if(!a[k+n]) return(e_p_comp_mess(a, cp+1, cp+1, txt, file, cmp, y, x));
      if((i = e_p_comp_mess(a+k+n, cp+n, cp+n, txt, file, cmp, y, x))) return(i);
      if(file[0] && *y > -1) return(0);
      return(e_p_comp_mess(a, cp+1, a[0] == '$' ? c : cp+1, 
					txt, file, cmp, y, x));
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
	 if(a[k] == ']' || a[k] == '\0') return(0);
	 for(; a[k] && (a[k] != ']'); k++);
	 n-=(k+1);  a+=(k+1);  b++;
      }
   }
   if(n <= 0) return(e_p_comp_mess(a, b, c, txt, file, cmp, y, x));
   if((k = strncmp(a, b, n)) != 0) return(0);
   return(e_p_comp_mess(a+n, b+n, c+n, txt, file, cmp, y, x));
}

int e_p_cmp_mess(srch, b, ii, kk, ret)
     char *srch;
     BUFFER *b;
     int *ii;
     int *kk;
     int ret;
{
   char *cp, cmp[128], file[128], search[80], tmp[4][128], **wtxt = NULL;
   int j, l, m, n, iy, i = *ii, k = *kk, x = 0, y = -1, wnum = 0, *wn = NULL;
   cmp[0] = search[0] = file[0] = '\0';
   wtxt = MALLOC(1);
   wn = MALLOC(1);
   for(j = 0, n = 0; n < 4 && srch[j]; n++)
   {  for(l = 0; (tmp[n][l] = srch[j]); j++, l++)
      {  if(j > 1 && srch[j] == '?' && srch[j-1] == '{' && srch[j-2] == '$')
	 {  wnum++;
	    wn = REALLOC(wn, wnum * sizeof(int));
	    wtxt = REALLOC(wtxt, wnum * sizeof(char *));
	    if(srch[j+1] == '*') wn[wnum-1] = -1;
	    else wn[wnum-1] = atoi(srch+j+1);
	    for(j++; srch[j] && srch[j] != ':'; j++);
	    if(!srch[j]) {  wnum--;  break;  }
	    for(m = 0; srch[j+m] && srch[j+m] != '}'; m++);
	    wtxt[wnum-1] = MALLOC((m+1) * sizeof(char));
	    for(m = 0, j++;
		(wtxt[wnum-1][m] = srch[j]) && srch[j] != '}'; j++, m++);
	    wtxt[wnum-1][m] = '\0';
	    l -= 3;
	 }
	 else if(srch[j] == '\r' || srch[j] == '\n')
	 {  if(srch[j+1] == '\r' || srch[j+1] == '\n')
	    {  tmp[n][l] = '\n';  tmp[n][l+1] = '\0';  j++;  }
	    else tmp[n][l] = '\0';
	    j++;  break;
	 }
      }
   }
   e_p_comp_mess(tmp[0], b->bf[i].s, b->bf[i].s, search, file, cmp, &y, &x);
   iy = i;
   do
   {  if(n > 1 && file[0] && i < b->mxlines-1)
      {  y = -1;
	 while(b->bf[i].s[b->bf[i].len-1] == '\\') i++;
	 i++;
	 e_p_comp_mess(tmp[1], b->bf[i].s, b->bf[i].s, search, file, cmp, &y, &x);
	 iy = i;
      }
      do
      {  if(n > 2 && file[0] && y >= 0 && i < b->mxlines-1)
	 {  while(b->bf[i].s[b->bf[i].len-1] == '\\') i++;
	    i++;
	    l = e_p_comp_mess(tmp[2], b->bf[i].s, b->bf[i].s, search, file, cmp, &y, &x);
	    if(!l && n > 3)
	    l = e_p_comp_mess(tmp[3], b->bf[i].s, b->bf[i].s, search, file, cmp, &y, &x);
	 }
	 else l = 1;
	 if(file[0] && y >= 0 && l != 0)
	 {  err_li[k].file = MALLOC((strlen(file)+1)*sizeof(char));
	    strcpy(err_li[k].file, file);
	    err_li[k].line = y;
	    if(search[0] == 'P')
	    {  cp = strstr(b->bf[iy].s, cmp);
	       if(!cp) x = 0;
	       else
	       {  for(m = 0; b->bf[iy].s + m < (unsigned char *)cp; m++);
		  x -= m;
	       }
	       err_li[k].srch = MALLOC((strlen(cmp)+2)*sizeof(char));
	       err_li[k].srch[0] = 'P';
	       strcpy(err_li[k].srch+1, cmp);
	    }
	    else if(search[0])
	    {  err_li[k].srch = MALLOC((strlen(search)+1)*sizeof(char));
	       strcpy(err_li[k].srch, search);
	    }
	    else err_li[k].srch = NULL;
	    err_li[k].x = x;
	    err_li[k].y = i;
	    err_li[k].text = MALLOC(strlen((char *)b->bf[i].s) + 1);
	    strcpy(err_li[k].text, (char *)b->bf[i].s);
	    err_li[k].text[b->bf[i].len] = '\0';
	    k++;
	    err_num++;
	    if(!ret)
	    {  for(ret = -1, m = 0; ret && m < wnum; m++)
	       {  if(wn[m] == -1 && !(b->cn->edopt & 32)
			    && strstr(b->bf[i].s, wtxt[m])) ret = 0;
		  else if(wn[m] > -1 && !(b->cn->edopt & 32)
			&& !strncmp(b->bf[i].s+wn[m], wtxt[m],
					strlen(wtxt[m]))) ret = 0;
	       }
	    }
	    if(!ret && wnum <= 0) ret = -1;
            while(b->bf[i].s[b->bf[i].len-1] == '\\') i++;
	 }
      } while(n > 2 && file[0] && y >= 0 && l != 0 && i < b->mxlines-1);
      if(n > 2 && file[0] && y >= 0 && l == 0) i--;
   } while(n > 1 && file[0] && y >= 0 && i < b->mxlines-1);
   if(n > 1 && file[0] && y < 0) i--;
   *ii = i;
   *kk = k;
   for(m = 0; m < wnum; m++) FREE(wtxt[m]);
   FREE(wn);
   FREE(wtxt);
   return(ret);
}

#endif

#endif

