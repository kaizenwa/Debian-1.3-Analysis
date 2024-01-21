/* edit.h						  */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */
/*
                     Header Datei fuer  FK - Editor
*/

#define VERSION "Version 1.4.2"

#ifndef MODELL
#include "model.h"
#endif
#include "keys.h"
#include <stdio.h>
#include <stdlib.h>
#include<string.h>
#include <ctype.h>

#ifdef DOS
#include <dir.h>
#include "dosmakro.h"
#include "doskeys.h"
#define MAXSLNS 25
#define MAXSCOL 80
#define MENOPT 8
#define MAXEDT 9
#endif

#ifdef UNIX
/* #include <curses.h>  */
#include <unistd.h>
#include "unixmakr.h"
#include "unixkeys.h"
extern int MAXSLNS, MAXSCOL, MENOPT;
#define MAXEDT 35
#endif

#define MAXLINES 10
#define MAXCOLUM 120
#define SCHBEG 2

#if  MOUSE
struct mouse { int x; int y; int k; };
#endif
struct EXT { char sw; struct CNT *cn; };
struct dirfile { int anz; char **name; };
typedef struct PKT { int x; int y; } PUNKT;
#ifndef DJGPP
typedef struct CLR { int f; int b; int fb; } COLOR;
#else
typedef struct CLR { unsigned char f; unsigned char b;
					unsigned char fb; } COLOR;
#endif
typedef struct PICSTR { char *p; PUNKT a; PUNKT e; } PIC;
typedef struct FND {  char search[80], replace[80], file[80], dirct[128];
		       int sn; int rn; unsigned int sw; } FIND;

typedef struct frb { COLOR er, es, et, ez, ek, em, hh, hb, hm;
	COLOR db, dy;
	COLOR mr, ms, mt, mz, df, nr, ne, nt, nsnt, fr, fa, ft, fz;
	COLOR frft, fs, nsft, fsm, nz, ns, nm, of;
	COLOR ct, cr, ck, cp, cc;
	char dc, ws; 
			} FARBE;

/*
typedef struct SWPC{ FILE *fp; unsigned int zmax, max, anz;
			  char *bl; fpos_t **ptr; } SWC;
typedef struct SWPP{ int n, bg, nmax; struct BFF *b;} SWP;
typedef struct SWPF{ int anz, cur; SWP **sp; } SWF;
*/
#ifdef WEUNDO
typedef struct undo {  	int type; PUNKT b, a, e;
			union {  char c; void far *pt;  } u;
			struct undo *next; }  Undo;
#endif

#ifndef SWAP
typedef struct STR{ unsigned char far *s; int len, nrc; } STRING;
typedef struct BFF { STRING *bf; PUNKT b; PUNKT mx; 
	      int mxlines, cl, clsv;
#ifdef WEUNDO
	      Undo *ud, *rd;
#endif
	      struct CNT *cn; struct FNST *f; FARBE *fb; } BUFFER;
#else
#include "swap.h"
#endif
#ifdef DEBUGGER
typedef struct SCHRM { PUNKT ka, ke, ks, pt[9];
	PUNKT da, de, fa, fe, a, e, c; FARBE *fb; int *brp;  } SCHIRM;
#else
typedef struct SCHRM { PUNKT ka; PUNKT ke; PUNKT ks; PUNKT pt[9];
	PUNKT fa; PUNKT fe; PUNKT a; PUNKT e; PUNKT c; FARBE *fb;  } SCHIRM;
#endif
typedef struct OPTION { char *t; int x; int s; int as; } OPT;
typedef struct WOPTION { char *t; int x, s, n, as; } WOPT;
typedef struct OPTKAST { char *t; int x; char o; int (*fkt)(); } OPTK;
typedef struct        MAINOPT { int a; int l; int n; OPTK *o; } MOPT;

typedef struct FNST { PUNKT a; PUNKT e; PUNKT sa; PUNKT se;
   char zoom; FARBE *fb; PIC *pic; char *dirct;
   char *datnam; int winnum; char ins; char dtmd; int save;
   char *hlp_str; WOPT *blst; 
   int nblst;
#ifdef UNIX
   int filemode, flg;
   int *c_sw;
   struct col_sw *c_st;
#endif
   struct CNT *ed; struct BFF *b; struct SCHRM *s;  } FENSTER;

typedef struct CNT { 
		char dtmd, prnt, autosv; 
		int maxcol, tabn;
#ifdef WEUNDO
		int maxchg, numundo;
#endif
#ifdef UNIX
		int flopt, edopt;
#else
		int uhr;
#endif
		int mxedt, curedt, edt[MAXEDT]; 
		char *dirct, *edtdrct, *libdrct, *optfile, *tabs; 
#ifdef SWAP
		char *swpfile, *swp2file; SFM *sp;
#endif
		struct dirfile *sdf, *rdf, *fdf, *ddf, *wdf, *hdf, *shdf;
		FIND fd; FARBE *fb; FENSTER *f[MAXEDT+1];
	      } ECNT;

typedef struct fl_wnd { int xa, ya, xe, ye, ia, ja, nf, nxfo, nyfo;
			int mxa, mya, mxe, mye, srcha;
			struct dirfile *df; FENSTER* f; } FLWND;
typedef struct FLBFF {  struct dirfile *cd, *dd, *df;
			struct fl_wnd *fw, *dw;
			char *rdfile, sw;
			int xfa, xfd, xda, xdd;		} FLBFFR;
typedef struct {  int x, y; char *txt;  } W_O_TXTSTR;
typedef struct {  int xt, yt, xw, yw, nw, wmx, nc, sw; 
		  char *header; char *txt;
		  struct dirfile **df;	} W_O_WRSTR;
typedef struct {  int xt, yt, xw, yw, nw, wmx, nc, num, sw; 
		  char *header;  } W_O_NUMSTR;
typedef struct {  int x, y, nc, sw, num; char *header;  } W_O_SSWSTR;
typedef struct {  int x, y, nc, sw; char *header;  } W_O_SPSWSTR;
typedef struct {  int num, np; W_O_SPSWSTR **ps; } W_O_PSWSTR;
typedef struct {  int x, y, nc, sw; char *header; 
			int (*fkt)(); } W_O_BTTSTR;

typedef struct {  int xa, ya, xe, ye, bgsw, crsw;
		  int frt, frs, ftt, fts, fst, fss, fsa, fbt;
		  int fbs, fbz, fwt, fws;
		  int tn, sn, pn, bn, wn, nn;
		  char *name;
		  PIC *pic;
		  W_O_TXTSTR **tstr;
		  W_O_SSWSTR **sstr;
		  W_O_PSWSTR **pstr;
		  W_O_BTTSTR **bstr;
		  W_O_WRSTR  **wstr;
		  W_O_NUMSTR **nstr;
		  FENSTER *f;
		}  W_OPTSTR;




/*   we_main.c   */
void e_ini_desk();
void e_ini_farbe();
void e_int_hnd();
int e_switch_blst();

/*   we_block.c   */
int e_blck_del();
int e_show_clipboard();
int e_edt_del();
int e_edt_copy();
int e_edt_einf();
int e_blck_versch();
void e_vsch_block();
int e_blck_copy();
void e_copy_block();
int e_blck_begin();
int e_blck_end();
int e_blck_hide();
int e_blck_read();
int e_blck_write();
int e_repl_find();
int e_rep_search();
int e_find();
int e_replace();
int e_fnd_fl();
int e_grp_fl();
int e_goto_line();
int e_blck_to_right();
int e_blck_to_left();

/*   we_dos.c   */
#ifdef DOS
#if  MOUSE
int fk_mouse(int g[]);
#endif
int fk_cursor(int sw);
int fk_locate(int spalte, int zeile);
int fk_lfw_test( int disk);
int fk_setdisk(int n);
void e_ini_schirm();
/*  void e_pr_char(int x, int y, int c, int frb);  */
int e_getch(void);
int e_ctrl_break();
int e_tst_sim(int b);
#endif
void ini_repaint();
void end_repaint();


/*   we_edit.c   */
int e_edit();
int e_eingabe();
int e_tst_cur();
int e_tst_fkt();
int e_ctrl_k();
int e_ctrl_q();
int e_tst_dfkt();
int e_blk();
int e_car_ret();
void e_cursor();
int e_del_line();
int e_del_nchar();
int e_ins_nchar();
int e_new_line();
int e_put_char();
int e_su_lblk();
int e_su_rblk();
void e_zlsplt();
char *e_mkeddir();
int e_ins_nbchar();
int e_del_nbchar();
int e_Lst_zeichen();
void e_mouse_Leiste();
int e_chr_sp();
#ifdef WEUNDO
Undo *e_remove_undo();
int e_add_undo();
int e_make_undo();
int e_make_redo();
int e_make_rudo();
int e_autosave();
char *e_make_postf();
#endif

/*   we_e_aus.c   */
void e_cls();
int e_puts();
void e_pr_str();
int e_pr_zstring();
int e_schr_nchar();
void e_pr_nstr();
int e_schreib_zif();
int e_schreib_leiste();
int e_schr_nzif();
int e_pr_str_wsd();
int e_pr_str_scan();

#ifdef DOS
/*   we_fl_dos.c   */
int e_manager(FENSTER *f);
int e_manager_first(FENSTER *f);
int e_ed_manager(int sw, FENSTER *f);
struct dirfile *e_find_files(char *sufile);
struct dirfile *e_find_dir(char *sufile);
int e_dsk_test(int disk);
char *e_file_info(char *filen, char *str);
int e_h_error(char *msg);
int e_p_handler(int errval,int ax,int bp,int si);
int e_d_handler(int errval,int ax,int bp,int si);
int e_printer_port(FENSTER *f);
int e_drucke_datei(FENSTER *f);
#endif
#if defined(DJGPP) || defined(DOS)
struct dirfile *e_mk_drives(void);
#endif
#ifdef UNIX
/*   we_fl_unix.c   */
int e_file_first();
int e_file_schirm();
int e_manager();
int e_manager_first();
int e_saveas();
int e_execute();
int e_shell();
int e_ed_manager();
int e_file_eingabe();
struct dirfile *e_dfnull();
struct dirfile *e_search_files();
struct dirfile *e_ext_dirfile();
struct dirfile *e_ext_dirdir();
int e_drucke_datei();
int e_remove();
int e_rename();
int e_fl_mng_options();
int e_sh_wastebasket();
int e_del_wastebasket();
char *e_ret_wastefile();
int e_copy();
int e_link();
int e_duplicate();
int e_mk_newdir();
int e_attributes();
int e_renocpy_dir();
int e_renocpy();
int e_dir_del_options();
int e_quit_wastebasket();
#ifndef NOSYMLINKS
int fk_link();
int fk_ren_link();
#endif
int e_ed_man();
#endif

/*   we_fl_fkt.c   */
char *e_getedpath();
char *e_mkfilename();
PUNKT e_readin();
PUNKT e_readbin();
int e_new();
int e_crypt_save();
int e_m_save();
int e_save();
int e_saveall();
int e_quit();
int e_write();
char *e_new_qual();
int freedf();
char *e_mk_path();
struct dirfile *e_mk_cur_dir();
int e_file_window();
int e_pr_file_window();
struct dirfile *e_ext_dirfile();
struct dirfile *e_ext_dirdir();
int e_help_last();
int e_help_comp();
int e_help_loc();
int e_help_free();

/*   we_hfkt.c   */
int e_toupper();
int e_strstr();
int e_ustrstr();
int e_urstrstr();
int e_rstrstr();
int e_str_len();
int e_str_nrc();
int e_num_kst();
int e_anz_zif();
char *e_numtozif();
int e_ziftonum();
COLOR e_s_x_clr();
COLOR e_n_x_clr();
#ifdef UNIX
COLOR e_s_t_clr();
COLOR e_n_t_clr();
#endif
char *e_blktostr();
PUNKT e_s_pkt();
int e_pr_uul();
int e_strcncmp();

/*   we_menue.c   */
int e_men_lst();
int e_men_kst();
OPTK e_optkin();

/*   we_mouse.c   */
#if  MOUSE
int e_mshit();
int e_m1_mouse();
int e_m2_mouse();
int e_m3_mouse();
int e_er_mouse();
int e_msg_mouse();
#ifdef DOS
int e_mng_mouse(int xa, int ya, int xe, int ye, int na, int nmax, int *n, int cold);
#else
int e_mng_mouse();
#endif
int fl_wnd_mouse();
int e_lst_mouse();
void e_eck_mouse();
int e_edt_mouse();
int e_ccp_mouse();
void e_cur_mouse();
int e_opt_ck_mouse();
int e_opt_cw_mouse();
int e_opt_bs_mouse();
void e_opt_eck_mouse();
int e_opt_mouse();

int e_fr_mouse();
int e_mv_mouse();
int e_data_ein_mouse();
int e_opt_bs_mouse_1();
int e_opt_bs_mouse_2();
int e_opt_bs_mouse_3();
int e_rahmen_mouse();
#endif

/*   we_opt.c   */
int e_about_WE();
int e_clear_desk();
int e_repaint_desk();
int e_sys_info();
int e_ad_colors();
int e_dif_colors();
void e_pr_dif_colors();
int e_x_frb_menue();
void e_pr_x_col_kasten();
void e_pr_ed_beispiel();
int e_opt_switches();
int e_std_colors();
int e_opt_numbers();
void e_set_time();
void e_uhr();
int e_opt_save();
int e_save_opt();
int e_opt_read();
int e_help();
int e_co_help();
int e_add_arguments();
W_O_TXTSTR **e_add_txtstr();
W_O_WRSTR **e_add_wrstr();
W_O_NUMSTR **e_add_numstr();
W_O_SSWSTR **e_add_sswstr();
W_O_SPSWSTR **e_add_spswstr();
W_O_PSWSTR **e_add_pswstr();
W_O_BTTSTR **e_add_bttstr();
int freeostr();
W_OPTSTR *e_init_opt_kst();
int e_opt_move();
int e_get_sw_cmp();
int e_get_opt_sw();
int e_opt_kst();
int e_edt_options();
int e_read_colors();
int e_ad_colors_md();


/*   we_wind.c   */
int e_error();
int e_message();
void e_firstl();
int e_pr_filetype();
PIC *e_open_view();
int e_close_view();
void e_pr_line();
void e_std_rahmen();
void e_ed_rahmen();
int e_schirm();
int e_size_move();
PIC *e_std_kst();
PIC *e_ed_kst();
int e_close_window();
void e_switch_window();
int e_ed_zoom();
int e_ed_cascade();
int e_ed_tile();
int e_ed_next();
int e_mess_win();
PIC *e_change_pic();
struct dirfile *e_add_df();
int e_schr_nchar_wsv();
int e_schr_lst_wsv();
int e_rep_win_tree();
int e_opt_sec_box();
int e_close_buffer();
int e_list_all_win();


#ifdef UNIX

/*   we_unix.c   */
#ifndef DJGPP
int e_abs_refr();
#endif
int e_tast_sim();
void e_err_end();
void e_err_save();
void e_exit();
char *e_mkfilepath();
int e_compstr();
struct dirfile *e_find_files();
struct dirfile *e_find_dir();
char *e_file_info();
void ini_repaint();
void end_repaint();
int e_frb_t_menue();
void e_pr_t_col_kasten();
int e_ini_unix();
int e_recover();

int e_frb_x_menue();
void e_pr_x_col_kasten();

#ifndef NOANSI
extern int (*fk_u_locate)(int x, int y);
extern int (*fk_u_cursor)(int x);
extern int (*e_u_initscr)(int argc, char *argv[]);
extern int (*fk_u_putchar)(char c);
extern int (*u_bioskey)(int sw);
extern int (*e_frb_u_menue)(int sw, int xa, int ya, FENSTER *f, int md);
extern COLOR (*e_s_u_clr)(int f, int b);
extern COLOR (*e_n_u_clr)(int fb);
extern void (*e_pr_u_col_kasten)(int xa, int ya, int x, 
					int y, FENSTER *f, int sw);
extern int (*fk_mouse)(int g[]);
#else
extern int (*fk_u_locate)();
extern int (*fk_u_cursor)();
extern int (*e_u_initscr)();
extern int (*fk_u_putchar)();
extern int (*u_bioskey)();
extern int (*e_frb_u_menue)();
extern COLOR (*e_s_u_clr)();
extern COLOR (*e_n_u_clr)();
extern void (*e_pr_u_col_kasten)();
extern int (*fk_mouse)();
#endif
extern int (*e_u_refresh)();
extern int (*e_u_getch)();
extern int (*e_u_sys_ini)();
extern int (*e_u_sys_end)();


/*    we_term.c    */

char *init_key();
char *init_kkey();
int init_cursor();
int e_begscr();
int e_endwin();
int fk_t_cursor();
int fk_t_putchar();
int fk_attrset();
int e_t_refresh();
int e_t_sys_ini();
int e_t_sys_end();
int e_t_getch();
int e_find_key();
int e_exitm();
int fk_t_locate();
int fk_t_mouse();
int e_t_initscr();
int e_t_kbhit();

#ifdef XWINDOW

/*    we_xterm.c     */

int fk_show_cursor();
int e_ini_size();
int get_GC();
int load_font();
int e_init_colplane();
int e_x_initscr();
int e_get_arg();
int e_get_geometry();
int e_x_getch();
int fk_x_mouse();
int e_x_refresh();
int fk_x_locate();
int fk_x_cursor();
int e_x_sys_ini();
int e_x_sys_end();
int fk_x_putchar();
int x_bioskey();
int fk_x_pointer();
int e_x_system();
int e_make_xrect();
int e_make_xrect_abs();
int e_cp_X_to_buffer();
int e_copy_X_buffer();
int e_paste_X_buffer();
int e_x_change();
int e_x_repaint_desk();
int e_put_pic_xrect();
int e_get_pic_xrect();
int e_setlastpic();
int e_make_xr_rahmen();
int e_x_kbhit();
#endif
int e_ini_schirm();
#ifdef DJGPP
int x_bioskey(int sw);
char *e_getcwd(char *buf, int n);
#endif

#endif

#ifdef PROG
#include "progr.h"
#endif

#ifdef DEBUGGER
int e_deb_inp();
int e_deb_out();
int e_e_line_read();
int e_d_dum_read();
int e_d_switch_out();
int e_pr_out_str();
int e_d_p_exec();
int e_d_getchar();
int e_d_quit();
int e_d_add_watch();
int e_remove_all_watches();
int e_make_watches();
int e_edit_watches();
int e_delete_watches();
int e_d_p_watches();
int e_deb_stack();
int e_d_p_stack();
int e_make_stack();
int e_breakpoint();
int e_remove_breakpoints();
int e_make_breakpoint();
int e_exec_deb();
int e_start_debug();
int e_run_debug();
int e_deb_run();
int e_deb_step();
int e_deb_next();
int e_deb_options();
int e_d_step_next();
int e_read_output();
int e_d_pr_sig();
int e_make_line_num();
int e_d_goto_break();
int e_d_is_watch();
int e_help_ret();
int e_debug_switch();
int e_d_putchar();
int e_g_sys_ini();
int e_g_sys_end();
int e_g_sys_first();
int e_d_flush_bf();
int e_make_line_num2();
int e_test_command();
#if MOUSE
int e_deb_op_mouse();
#endif
#endif

#ifdef FREETEST
int sc_txt_2(FENSTER *f);
#endif

extern char *e_msg[];
extern char e_we_sw;

