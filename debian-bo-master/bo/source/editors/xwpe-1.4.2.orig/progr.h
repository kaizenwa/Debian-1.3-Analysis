/* progr.h						  */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include <time.h>

struct e_s_prog {  char *language, *compiler, *comp_str, *libraries,
			*exe_name, *filepostfix, *intstr, key; 
		   int	comp_sw, x;  };

struct e_prog {  	int num, n;  
			char *arguments, *project, *exedir, *sys_include;
			struct e_s_prog **comp;  };

extern int e__project;
extern struct e_s_prog e_s_prog;
extern struct e_prog e_prog;

typedef struct col_sw {
   unsigned char **res_wrd, **opr;
   unsigned char *no_alnum;
   unsigned char *cm_bg, *cm_nd, *cm_te, *cm_ch;
   unsigned char c_str, c_c, c_pr, c_nc, c_nl, c_ew, c_na, to_u;
   int  i_cm, i_nl, i_af;
} COLSW;

typedef struct {  FILE *fp;  BUFFER *b;  PUNKT p;  }  E_AFILE;

#ifdef DJGPP
typedef long M_TIME;
#else
typedef time_t M_TIME;
#endif
/*
int e_compile(struct FNST *f);
int e_make(struct FNST *f);
int e_run(struct FNST *f);
int e_debug(struct FNST *f);
int e_arguments(struct FNST *f);
int e_run_options(struct FNST *f);
int e_project_options(struct FNST *f);
*/

/*   we_prog.c   */

int e_prog_switch();
int e_compile();
int e_make();
int e_p_make();
int e_run();
int e_c_project();
int e_free_arg();
int e_comp();
int e_exec_inf();
int e_print_arg();
int e_show_error();
int e_make_error_list();
int e_previous_error();
int e_next_error();
int e_line_read();
int e_arguments();
int e_check_c_file();
int e_check_header();
char *e_fgets();
char *e_make_string();
char *e_cat_string();
int e_make_arg();
int e_ini_prog();
int e_copy_prog();
int e_run_options();
int e_run_c_options();
int e_project_options();
int e_system();
int e_d_p_message();
int e_install();
int e_exec_make();
int e_funct();
int e_funct_in();
int e_p_help();
int e_data_first();
int e_data_schirm();
int e_data_eingabe();
int e_run_sh();
int e_project();
int e_p_mess_win();
int e_p_add_df();
int e_p_del_df();
int e_p_edit_df();
int e_d_car_ret();
int e_d_car_mouse();
int e_add_arg();
int e_new_message();
int e_p_cmp_mess();
int e_project_name();
int e_wrt_prj_fl();
int e_p_update_prj_fl();
int freedfN();
int e_p_red_buffer();
int e_read_var();

/*   we_progn.c  */

int e_read_pr_opt();
int e_scfbol();
int e_sc_all();
int e_program_opt();
int e_sc_nw_txt();
int *e_sc_txt();
void e_pr_c_line();
char *e_dup_word();
int e_add_synt_tl();
E_AFILE *e_aopen();
int e_aclose();
char *e_agets();
char *e_sh_spl1();
char *e_sh_spl2();
char *e_sh_spl3();
char *e_sh_spl4();
char *e_sh_spl5();
struct dirfile *e_c_add_df();
int e_find_def();
int e_show_nm_f();
int e_sh_def();
int e_sh_nxt_def();
int e_nxt_brk();
int e_mk_beauty();
int e_p_beautify();

#ifdef PROGTOOLS
int e_make_opt_kst(FENSTER *f);
int e_read_opt_kst(FENSTER *f);
int e_write_opt_kst(FENSTER *f);
int e_dev_opt_mouse(int *x, int *y, W_OPTSTR *o);
int e_dev_opt_move(W_OPTSTR *o);
#endif

