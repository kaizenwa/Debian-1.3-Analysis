/* we_menue.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"

int e_ad_colors_1();
int e_ad_colors_2();
int e_list_all_win();
int e_p_show_messages();
int e_p_show_watches();
int e_program_opt();
int e_copy_X_buffer();
int e_paste_X_buffer();
int e_sh_def();
int e_sh_nxt_def();
int e_nxt_brk();
int e_blck_gt_beg();
int e_blck_gt_end();
int e_blck_mrk_line();
int e_blck_mrk_all();
int e_blck_to_left();
int e_blck_to_right();
int e_p_beautify();
int e_cl_project();
int e_p_add_item();
int e_p_del_item();
int e_show_project();
int e_info();
int e_help_options();
int e_hp_ret();
int e_hp_back();
int e_hp_prev();
int e_hp_next();
int e_deb_out();

/*   Hauptmenue-Leiste   */

int e_men_lst(n, f)
     int n;
     FENSTER *f;
{
   extern int e_mn_men;
   int i, c = 255, nold = n <= 0 ? 1 : n-1;
   extern OPT opt[];
   extern char *e_hlp, *e_hlp_str[];
   ECNT *cn = f->ed;
   MOPT *o = MALLOC(MENOPT*sizeof(MOPT));
   for(i = 0; i < MENOPT; i++) o[i].l = 0;
   fk_cursor(0);
   if(n < 0) n = 0;
   else c = CR;
#ifdef UNIX
#ifdef NEWSTYLE
   if(e_we_sw & 1) o[0].a = -3;
   else 
#endif
	o[0].a = -2;
   o[0].n = 6;
   o[0].l = 23;
#else
   o[0].a = -3;
   o[0].n = 4;
   o[0].l = 20;
#endif
   if( (o[0].o = MALLOC(o[0].n * sizeof(OPTK))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   o[0].o[0] = e_optkin("About WE", 0, 'A', e_about_WE);
   o[0].o[1] = e_optkin("Clear Desktop", 0, 'C', e_clear_desk);
   o[0].o[2] = e_optkin("Repaint Desktop", 0, 'R', e_repaint_desk);
   o[0].o[3] = e_optkin("System Info", 0, 'S', e_sys_info);
#ifdef UNIX
   o[0].o[4] = e_optkin("Show Wastebasket", 5, 'W', e_sh_wastebasket);
   o[0].o[5] = e_optkin("Delete Wastebasket", 0, 'D', e_del_wastebasket);
#endif
   o[1].a = -3;
#ifdef DOS
   o[1].l = 21;
   o[1].n = 6;
   if( (o[1].o = MALLOC(o[1].n * sizeof(OPTK))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   o[1].o[0] = e_optkin("File-Manager  F3", 0, 'M', e_manager);
   o[1].o[1] = e_optkin("New"             , 0, 'N', e_new);
   o[1].o[2] = e_optkin("Save          F2", 0, 'S', e_m_save);
   o[1].o[3] = e_optkin("Save All"        , 6, 'L', e_saveall);
   o[1].o[4] = e_optkin("Save Crypt      ", 5, 'C', e_crypt_save);
   o[1].o[5] = e_optkin("Quit       Alt X", 0, 'Q', e_quit);
#endif
#ifdef UNIX
   o[1].l = 24;
   o[1].n = 12;
   if( (o[1].o = MALLOC(o[1].n * sizeof(OPTK))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   if(f->ed->edopt & 1)
   {  o[1].o[0] = e_optkin("File-Manager     F2", 0, 'M', e_manager);
      o[1].o[1] = e_optkin("ADd F.-Mng Shift F2", 1, 'D', e_manager_first);
      o[1].o[3] = e_optkin("Save         Alt F2", 0, 'S', e_m_save);
      o[1].o[11] = e_optkin("Quit         Alt F4", 0, 'Q', e_quit);
   }
   else
   {  o[1].o[0] = e_optkin("File-Manager     F3", 0, 'M', e_manager);
      o[1].o[1] = e_optkin("ADd F.-Mng  Ctrl F3", 1, 'D', e_manager_first);
      o[1].o[3] = e_optkin("Save             F2", 0, 'S', e_m_save);
      o[1].o[11] = e_optkin("Quit          Alt X", 0, 'Q', e_quit);
   }
   o[1].o[2] = e_optkin("New"                , 0, 'N', e_new);
   o[1].o[4] = e_optkin("Save As"            , 5, 'A', e_saveas);
   o[1].o[5] = e_optkin("Save aLl"           , 6, 'L', e_saveall);
   o[1].o[6] = e_optkin("Execute"            , 0, 'E', e_execute);
   o[1].o[7] = e_optkin("SHell"              , 1, 'H', e_shell);
   o[1].o[8] = e_optkin("Find"               , 0, 'F', e_fnd_fl);
   o[1].o[9] = e_optkin("Grep"               , 0, 'G', e_grp_fl);
   o[1].o[10] = e_optkin("Print File"         , 0, 'P', e_drucke_datei);
#endif
   o[2].a = -3;
   o[2].l = 27;
#ifdef WEUNDO
#if defined(XWINDOW) && !defined(DJGPP)
   if(e_we_sw & 1) o[2].n = 9;
   else
#endif
   o[2].n = 7;
#else
   o[2].n = 4;
#endif
   if( (o[2].o = MALLOC(o[2].n * sizeof(OPTK))) == NULL)
   e_error(e_msg[0], 1, f->fb);
#if defined(DOS) || defined(XWINDOW) || defined(DJGPP)
   o[2].o[0] = e_optkin("Cut     Shift Del / ^X", 2, 'T', e_edt_del);
   o[2].o[1] = e_optkin("Copy         ^Ins / ^C", 0, 'C', e_edt_copy);
   o[2].o[2] = e_optkin("Paste   Shift Ins / ^V", 0, 'P', e_edt_einf);
   o[2].o[3] = e_optkin("Show Buffer         ^W", 0, 'S', e_show_clipboard);
#ifdef WEUNDO
   o[2].o[4] = e_optkin("Delete            ^Del", 0, 'D', e_blck_del);
   o[2].o[5] = e_optkin("Undo                ^U", 0, 'U', e_make_undo);
   o[2].o[6] = e_optkin("Redo                ^R", 0, 'R', e_make_redo);
#ifndef DJGPP
   if(e_we_sw & 1)
   {  o[2].o[7] = e_optkin("PAste(XBuffer) Alt Ins", 0, 'A', e_copy_X_buffer);
      o[2].o[8] = e_optkin("COpy(XBuffer)  Alt Del", 0, 'O', e_paste_X_buffer);
   }
#endif
#endif
#else
   o[2].o[0] = e_optkin("Cut                 ^X", 2, 'T', e_edt_del);
   o[2].o[1] = e_optkin("Copy                ^C", 0, 'C', e_edt_copy);
   o[2].o[2] = e_optkin("Paste               ^V", 0, 'P', e_edt_einf);
   o[2].o[3] = e_optkin("Show Buffer         ^W", 0, 'S', e_show_clipboard);
#ifdef WEUNDO
   o[2].o[4] = e_optkin("Delete                ", 0, 'D', e_blck_del);
   o[2].o[5] = e_optkin("Undo                ^U", 0, 'U', e_make_undo);
   o[2].o[6] = e_optkin("Redo                ^R", 0, 'R', e_make_redo);
#endif
#endif
   o[3].a = -3;
   o[3].l = 28;
   o[3].n = 4;
   if( (o[3].o = MALLOC(o[3].n * sizeof(OPTK))) == NULL)
   e_error(e_msg[0], 1, f->fb);
#ifdef DOS
   o[3].o[0] = e_optkin("Find              ^F7", 0, 'F', e_find);
   o[3].o[1] = e_optkin("Replace        Alt F7", 0, 'R', e_replace);
   o[3].o[2] = e_optkin("Search again       F7", 0, 'S', e_rep_search);
   o[3].o[3] = e_optkin("Go to Line      Alt G", 0, 'G', e_goto_line);
#else
   if(f->ed->edopt & 1)
   {  o[3].o[0] = e_optkin("Find      Alt F3 / ^O F", 0, 'F', e_find);
      o[3].o[1] = e_optkin("Replace Shift F3 / ^O A", 0, 'R', e_replace);
      o[3].o[2] = e_optkin("Search again         F3", 0, 'S', e_rep_search);
   }
   else
   {  o[3].o[0] = e_optkin("Find          F4 / ^O F", 0, 'F', e_find);
      o[3].o[1] = e_optkin("Replace   Alt F4 / ^O A", 0, 'R', e_replace);
      o[3].o[2] = e_optkin("Search again         ^L", 0, 'S', e_rep_search);
   }
   o[3].o[3] = e_optkin("Go to Line        Alt G", 0, 'G', e_goto_line);
#endif
   o[4].a = -3;
   o[4].l = 25;
   o[4].n = 14;
   if( (o[4].o = MALLOC(o[4].n * sizeof(OPTK))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   o[4].o[0] = e_optkin("Begin Mark      ^K B", 0, 'B', e_blck_begin);
   o[4].o[1] = e_optkin("End Mark        ^K K", 0, 'E', e_blck_end);
   o[4].o[2] = e_optkin("Mark WhOle      ^K X", 5, 'O', e_blck_mrk_all);
   o[4].o[3] = e_optkin("Mark Line       ^K L", 5, 'L', e_blck_mrk_line);
   o[4].o[4] = e_optkin("Goto Begin      ^K A", 0, 'G', e_blck_gt_beg);
   o[4].o[5] = e_optkin("Goto ENd        ^K Z", 6, 'N', e_blck_gt_end);
   o[4].o[6] = e_optkin("Copy            ^K C", 0, 'C', e_blck_copy);
   o[4].o[7] = e_optkin("Move            ^K V", 0, 'M', e_blck_versch);
   o[4].o[8] = e_optkin("Delete          ^K Y", 0, 'D', e_blck_del);
   o[4].o[9] = e_optkin("Hide            ^K H", 0, 'H', e_blck_hide);
   o[4].o[10] = e_optkin("Read            ^K R", 0, 'R', e_blck_read);
   o[4].o[11] = e_optkin("Write           ^K W", 0, 'W', e_blck_write);
   o[4].o[12] = e_optkin("Move to RIght   ^K I", 9, 'I', e_blck_to_right);
   o[4].o[13] = e_optkin("Move to LefT    ^K U", 11, 'T', e_blck_to_left);
#ifdef PROG
   if(e_we_sw & 2)
   {  o[5].a = -3;
      o[5].l = 35;
      o[5].n = 12;
      if( (o[5].o = MALLOC(o[5].n * sizeof(OPTK))) == NULL)
      e_error(e_msg[0], 1, f->fb);
      o[5].o[0] = e_optkin("Compile         Alt F9 / Alt C", 0, 'C', e_compile);
      o[5].o[1] = e_optkin("Make                F9 / Alt M", 0, 'M', e_make);
      o[5].o[2] = e_optkin("Run                ^F9 / Alt U", 0, 'R', e_run);
      o[5].o[3] = e_optkin("Install                  Alt L", 0, 'I', e_install);
      o[5].o[4] = e_optkin("Execute Make             Alt A", 0, 'E', e_exec_make);
      o[5].o[5] = e_optkin("Next Error      Alt F8 / Alt T", 0, 'N', e_next_error);
      o[5].o[6] = e_optkin("Previous Error  Alt F7 / Alt V", 0, 'P', e_previous_error);
      o[5].o[7] = e_optkin("Show Definition           ^O S", 0, 'S', e_sh_def);
      o[5].o[8] = e_optkin("Show NeXt Definition      ^O N", 7, 'X', e_sh_nxt_def);
      o[5].o[9] = e_optkin("Matching BracKet          ^O K", 13, 'K', e_nxt_brk);
      o[5].o[10] = e_optkin("Beautify                  ^O B", 0, 'B', e_p_beautify);
      o[5].o[11] = e_optkin("Arguments                     ", 0, 'A', e_arguments);
      
      o[MENOPT-4].a = -3;
      o[MENOPT-4].l = 23;
      o[MENOPT-4].n = 5;
      if( (o[MENOPT-4].o = MALLOC(o[MENOPT-4].n * sizeof(OPTK))) == NULL)
      e_error(e_msg[0], 1, f->fb);
      o[MENOPT-4].o[0] = e_optkin("Open Project", 5, 'P', e_project);
      o[MENOPT-4].o[1] = e_optkin("Close Project", 0, 'C', e_cl_project);
      o[MENOPT-4].o[2] = e_optkin("Add Item", 0, 'A', e_p_add_item);
      o[MENOPT-4].o[3] = e_optkin("Delete Item", 0, 'D', e_p_del_item);
      o[MENOPT-4].o[4] = e_optkin("Options", 0, 'O', e_project_options);
   }
#endif
#ifdef DEBUGGER
   if(e_we_sw & 2)
   {  o[MENOPT-5].a = -3;
      o[MENOPT-5].l = 33;
      o[MENOPT-5].n = 11;
      if( (o[MENOPT-5].o = MALLOC(o[MENOPT-5].n * sizeof(OPTK))) == NULL)
      e_error(e_msg[0], 1, f->fb);
      
      if(f->ed->edopt & 1)
      {  o[MENOPT-5].o[0] = e_optkin("Toggel Breakpoint  F5 / ^G B", 7, 'B', e_breakpoint);
	 o[MENOPT-5].o[2] = e_optkin("Make Watch        ^F5 / ^G W", 5, 'W', e_make_watches);
	 o[MENOPT-5].o[6] = e_optkin("Show stacK        ^F3 / ^G K", 9, 'K', e_deb_stack);
      }
      else
      {  o[MENOPT-5].o[0] = e_optkin("Toggel Breakpoint ^F8 / ^G B", 7, 'B', e_breakpoint);
	 o[MENOPT-5].o[2] = e_optkin("Make Watch        ^F7 / ^G W", 5, 'W', e_make_watches);
	 o[MENOPT-5].o[6] = e_optkin("Show stacK        ^F6 / ^G K", 9, 'K', e_deb_stack);
      }
      o[MENOPT-5].o[1] = e_optkin("ReMove all Breakp.      ^G M", 2, 'M', e_remove_breakpoints);
      o[MENOPT-5].o[3] = e_optkin("Edit watch              ^G E", 0, 'E', e_edit_watches);
      o[MENOPT-5].o[4] = e_optkin("Delete watch            ^G D", 0, 'D', e_delete_watches);
      o[MENOPT-5].o[5] = e_optkin("Remove All watches      ^G A", 7, 'A', e_remove_all_watches);
      o[MENOPT-5].o[7] = e_optkin("Step               F7 / ^G S", 0, 'S', e_deb_step);
      o[MENOPT-5].o[8] = e_optkin("Next               F8 / ^G N", 0, 'N', e_deb_next);
      o[MENOPT-5].o[9] = e_optkin("Run/Continue     ^F10 / ^G R", 0, 'R', e_deb_run);
      o[MENOPT-5].o[10] = e_optkin("Quit              ^F2 / ^G Q", 0, 'Q', e_d_quit);
   }
#endif
   o[MENOPT-3].a = -4;
   o[MENOPT-3].l = 22;
#ifdef DOS
   o[MENOPT-3].n = 7;
#else
#ifdef PROG
   if(e_we_sw & 2)
#ifdef DEBUGGER
   o[MENOPT-3].n = 9;
#else
   o[MENOPT-3].n = 8;
#endif
   else
#endif
   o[MENOPT-3].n = 6;
#endif
   if( (o[MENOPT-3].o = MALLOC(o[MENOPT-3].n * sizeof(OPTK))) == NULL)
   e_error(e_msg[0], 1, f->fb);
#ifdef DOS
   o[MENOPT-3].o[0] = e_optkin("Editor Colors", 0, 'E', e_ad_colors);
   o[MENOPT-3].o[1] = e_optkin("Desk Colors", 0, 'D', e_ad_colors_1);
   o[MENOPT-3].o[2] = e_optkin("Option Colors", 0, 'O', e_ad_colors_2);
   o[MENOPT-3].o[3] = e_optkin("Standard Colors", 9, 'C', e_std_colors);
   o[MENOPT-3].o[4] = e_optkin("SWitches", 1, 'W', e_opt_switches);
   o[MENOPT-3].o[5] = e_optkin("Numbers", 0, 'N', e_opt_numbers);
   o[MENOPT-3].o[6] = e_optkin("Save Options", 0, 'S', e_opt_save);
#else
   o[MENOPT-3].o[0] = e_optkin("Adjust Colors", 0, 'A', e_ad_colors);
   o[MENOPT-3].o[1] = e_optkin("Read Colors", 0, 'R', e_read_colors);
   o[MENOPT-3].o[2] = e_optkin("Save Options", 0, 'S', e_opt_save);
   o[MENOPT-3].o[3] = e_optkin("Editor", 0, 'E', e_edt_options);
   o[MENOPT-3].o[4] = e_optkin("File-Manager", 0, 'F', e_fl_mng_options);
   o[MENOPT-3].o[5] = e_optkin("Help", 0, 'H', e_help_options);
#ifdef PROG
   if(e_we_sw & 2)
   {  o[MENOPT-3].o[6] = e_optkin("ProGramming", 3, 'G', e_program_opt);
      o[MENOPT-3].o[7] = e_optkin("Compiler", 0, 'C', e_run_options);
#ifdef DEBUGGER
      o[MENOPT-3].o[8] = e_optkin("Debugger", 0, 'D', e_deb_options);
#endif
   }
#endif
#endif
#ifdef NEWSTYLE
   if(e_we_sw & 1) o[MENOPT-2].a = -13;
   else 
#endif
	o[MENOPT-2].a = -14;
   o[MENOPT-2].l = 28;
#ifdef DJGPP
#ifdef PROG
   o[MENOPT-2].n = 10;
#else
   o[MENOPT-2].n = 8;
#endif
#else
#ifdef UNIX
#ifdef PROG
   if(e_we_sw & 2)
#ifdef DEBUGGER
   o[MENOPT-2].n = !(e_we_sw & 1) ? 11: 10;
#else
   o[MENOPT-2].n = !(e_we_sw & 1) ? 10: 9;
#endif
   else
#endif
   o[MENOPT-2].n = !(e_we_sw & 1) ? 8: 7;
#else
   o[MENOPT-2].n = !(e_we_sw & 1) ? 7: 6;
#endif
#endif
   if( (o[MENOPT-2].o = MALLOC(o[MENOPT-2].n * sizeof(OPTK))) == NULL)
   e_error(e_msg[0], 1, f->fb);
#ifdef DOS
   o[MENOPT-2].o[0] = e_optkin("Size/Move      ^F5", 0, 'S', e_size_move);
   o[MENOPT-2].o[1] = e_optkin("Zoom            F5", 0, 'Z', e_ed_zoom);
   o[MENOPT-2].o[2] = e_optkin("Tile"              , 0, 'T', e_ed_tile);
   o[MENOPT-2].o[3] = e_optkin("Cascade"           , 1, 'A', e_ed_cascade);
   o[MENOPT-2].o[4] = e_optkin("Next            F6", 2, 'X', e_ed_next);
   o[MENOPT-2].o[5] = e_optkin("Close       Alt F3", 0, 'C', e_close_window);
#else
   if(f->ed->edopt & 1)
   {  o[MENOPT-2].o[0] = e_optkin("Size/Move            ^L", 0, 'S', e_size_move);
      o[MENOPT-2].o[1] = e_optkin("Zoom   Shift F6 / Alt Z", 0, 'Z', e_ed_zoom);
      o[MENOPT-2].o[4] = e_optkin("Next        ^F6 / Alt N", 2, 'X', e_ed_next);
      o[MENOPT-2].o[5] = e_optkin("Close       ^F4 / Alt X", 0, 'C', e_close_window);
   }
   else
   {  o[MENOPT-2].o[0] = e_optkin("Size/Move        Alt F2", 0, 'S', e_size_move);
      o[MENOPT-2].o[1] = e_optkin("Zoom         F5 / Alt Z", 0, 'Z', e_ed_zoom);
      o[MENOPT-2].o[4] = e_optkin("Next         F6 / Alt N", 2, 'X', e_ed_next);
      o[MENOPT-2].o[5] = e_optkin("Close            Alt F3", 0, 'C', e_close_window);
   }
   o[MENOPT-2].o[2] = e_optkin("Tile           Shift F4", 0, 'T', e_ed_tile);
   o[MENOPT-2].o[3] = e_optkin("Cascade        Shift F5", 1, 'A', e_ed_cascade);
   o[MENOPT-2].o[6] = e_optkin("List All          Alt 0", 0, 'L', e_list_all_win);
#ifndef DJGPP
   if(!(e_we_sw & 1))
#endif
      o[MENOPT-2].o[7] = e_optkin("Output    Alt F5 / ^G P", 0, 'O', e_deb_out);
#ifdef PROG
   if(e_we_sw & 2)
   {  o[MENOPT-2].o[o[MENOPT-2].n-2] = e_optkin("Messages", 0, 'M', e_p_show_messages);
      o[MENOPT-2].o[o[MENOPT-2].n-1] = e_optkin("Project", 0, 'P', e_show_project);
#ifdef DEBUGGER
      o[MENOPT-2].o[o[MENOPT-2].n-3] = e_optkin("Watches", 0, 'W', e_p_show_watches);
#endif
   }
#endif
#endif
#ifdef NEWSTYLE
   if(e_we_sw & 1) o[MENOPT-1].a = -19;
   else 
#endif
	o[MENOPT-1].a = -20;
   o[MENOPT-1].l = 25;
#if defined(PROG) && !defined(DJGPP)
   o[MENOPT-1].n = 8;
#else
   o[MENOPT-1].n = 6;
#endif
   if( (o[MENOPT-1].o = MALLOC(o[MENOPT-1].n * sizeof(OPTK))) == NULL)
   e_error(e_msg[0], 1, f->fb);
   o[MENOPT-1].o[0] = e_optkin("Editor            F1", 0, 'E', e_help);
#if defined(PROG) && !defined(DJGPP)
   o[MENOPT-1].o[1] = e_optkin("FUnction Index   ^F1", 1, 'U', e_funct_in);
   o[MENOPT-1].o[2] = e_optkin("Functions     Alt F1", 0, 'F', e_funct);
#endif
   o[MENOPT-1].o[o[MENOPT-1].n-5] = 
        	e_optkin("Info                ", 0, 'I', e_info);
   o[MENOPT-1].o[o[MENOPT-1].n-4] = 
        	e_optkin("Goto            <CR>", 0, 'G', e_hp_ret);
   o[MENOPT-1].o[o[MENOPT-1].n-3] = 
        	e_optkin("Back           <BSP>", 0, 'B', e_hp_back);
   o[MENOPT-1].o[o[MENOPT-1].n-2] = 
        	e_optkin("Next  Alt F8 / Alt T", 0, 'G', e_hp_next);
   o[MENOPT-1].o[o[MENOPT-1].n-1] = 
        	e_optkin("Prev. Alt F7 / Alt V", 0, 'P', e_hp_prev);
   if(n < 0 || n > MENOPT-1) n = 0;
   while (c != ESC)
   {  f = cn->f[cn->mxedt];
      if(e_tst_dfkt(f, c) == 0)  {  c = 0; break;  }
      
      for(i = 0; i < MENOPT; i++)
      if(c == opt[i].s || c == opt[i].as) {  n = i; c = CR;  }
      
      if (nold != n)
      {  e_pr_str_wsd(opt[nold].x, 0, opt[nold].t, f->fb->mt.fb, 0, 1,
		f->fb->ms.fb, ( nold == 0 ? 0 : opt[nold].x-e_mn_men),
		(nold == MENOPT-1) ? MAXSCOL-1 : opt[nold+1].x-e_mn_men-1);
	 e_pr_str_wsd(opt[n].x, 0, opt[n].t, f->fb->mz.fb, 0, 1,
		f->fb->mz.fb, ( n == 0 ? 0 : opt[n].x-e_mn_men),
		(n == MENOPT-1) ? MAXSCOL-1 : opt[n+1].x-e_mn_men-1);
	 nold = n;
      }
      if ( c == CR)
#ifdef PROG
#ifdef DEBUGGER
      {  if(!(e_we_sw & 2) && n > 4) e_hlp = e_hlp_str[9+n];
	 else e_hlp = e_hlp_str[6+n];
#else
      {  if(!(e_we_sw & 2) && n > 4) e_hlp = e_hlp_str[8+n];
	 else e_hlp = e_hlp_str[6+n];
#endif
#else
      {  e_hlp = e_hlp_str[6+n];
#endif
	 if(o[n].l != 0) c = e_men_kst(opt[n].x + o[n].a, 1,
                       opt[n].x + o[n].l + o[n].a, o[n].n+2, n, o[n].o, f);
	 if (c < MENOPT) {  n = c; c = CR;  }
	 else if(c != ESC) c = 0;
      }
      else
      {  e_hlp = e_hlp_str[14+MENOPT];
#if  MOUSE
	 if( (c = e_getch()) == -1) c = e_m1_mouse();
#else
	 c = e_getch();
#endif
	 c = e_toupper(c);
      }
      if ( c == CDO || c == CtrlN ) c = CR;
      else if ( c == CLE || c == CtrlB ) n--;
      else if ( c == CRI || c == CtrlF ) n++;
      else if ( c == POS1 || c == CtrlA ) n = 0;
      else if ( c == ENDE || c == CtrlE ) n = MENOPT-1;
      else if ( c == AltX ) c = e_quit(f);
      
      if(n < 0) n = MENOPT-1;
      else if(n >= MENOPT) n = 0;
      
   }
   f = cn->f[cn->mxedt];
   e_pr_str_wsd(opt[nold].x, 0, opt[nold].t, f->fb->mt.fb, 0, 1,
	f->fb->ms.fb, ( nold == 0 ? 0 : opt[nold].x-e_mn_men),
	(nold == MENOPT-1) ? MAXSCOL-1 : opt[nold+1].x-e_mn_men-1);
   for(i = 0; i < MENOPT; i++)
   if(o[i].l != 0) FREE(o[i].o);
   FREE(o);
   fk_cursor(1);
   return(c);
}

/*   Untermenue - Kasten    */

int e_men_kst(xa, ya, xe, ye, nm, fopt, f)
     int xa;
     int ya;
     int xe;
     int ye;
     int nm;
     OPTK *fopt;
     FENSTER *f;
{
#if MOUSE
   extern struct mouse e_mouse;
   extern int e_mn_men;
#endif
   PIC *pic;
   int i, n = 0, nold = 1, c = 0;
   extern OPT opt[];
#ifdef NEWSTYLE
   if(e_we_sw & 1)
   pic = e_open_view(xa+1, ya, xe-1, ye, f->fb->mt.fb, 1);
   else
#endif
   pic = e_open_view(xa, ya, xe, ye, f->fb->mt.fb, 1);
   
   if(pic == NULL)  {  e_error(e_msg[0], 0, f->fb); return(ESC);  }
   e_std_rahmen(xa+1, ya, xe-1, ye, NULL, 0, f->fb->mr.fb, 0);
   for (i = ya + 1; i < ye; i++)
   e_pr_str_scan(xa+3, i, fopt[i-ya-1].t, f->fb->mt.fb, fopt[i-ya-1].x, 1,
	   f->fb->ms.fb, xa+2, xe-2);
   
#if  MOUSE
   e_refresh();
   while (e_mshit() != 0)
   {  c = -1;
      if(e_mouse.y > ya && e_mouse.y < ye)
      {  n = e_mouse.y - ya -1;
	 if (nold != n)
         {  if(nold < ye-ya-1 && nold >= 0)
	    e_pr_str_scan(xa+3, nold+ya+1, fopt[nold].t, f->fb->mt.fb,
				fopt[nold].x, 1, f->fb->ms.fb, xa+2, xe-2);
	    e_pr_str_scan(xa+3, n+ya+1, fopt[n].t, f->fb->mz.fb,
				fopt[n].x, 1, f->fb->mz.fb, xa+2, xe-2);
	    nold = n;
	    e_refresh();
         }
      }
      else if(e_mouse.y == 0)
      {	for(i = 1; i < MENOPT; i++)
	      if (e_mouse.x < opt[i].x-e_mn_men) break;
	if(i != nm+1)
	{  e_close_view(pic, 1);
	   return(i-1);
	}
      }
   }
   if(c < 0 && e_mouse.y > ya && e_mouse.y < ye 
			&& e_mouse.x > xa && e_mouse.x < xe) c = MBKEY;
#endif
   while (c != ESC)
   {  if (nold != n)
      {  if(nold < ye-ya-1 && nold >= 0)
	 e_pr_str_scan(xa+3, nold+ya+1, fopt[nold].t, f->fb->mt.fb,
				fopt[nold].x, 1, f->fb->ms.fb, xa+2, xe-2);
	 e_pr_str_scan(xa+3, n+ya+1, fopt[n].t, f->fb->mz.fb,
				fopt[n].x, 1, f->fb->mz.fb, xa+2, xe-2);
	 nold = n;
      }
#if  MOUSE
      if(c != MBKEY)
      {  if((c = e_toupper(e_getch())) == -1)
	      c = e_m2_mouse(xa, ya, xe, ye, fopt);
      }
      else c = CR;
#else
      c = e_toupper(e_getch());
#endif
      for (i = 0; i < MENOPT; i++)
      if( c == opt[i].as)
      {  e_close_view(pic, 1); return (i);  }
      for (i = 0; i < ye - ya - 1; i++)
      if( c == fopt[i].o)
      {  e_close_view(pic, 1);
	 fopt[i].fkt(f);
	 return(ESC);
      }
      if(c == Alt0) c = ESC;
      if (i > ye - ya) c = ESC;
      else if( c == CR)
      {  e_close_view(pic, 1);  fopt[n].fkt(f); return(ESC);  }
      else if ( c == CUP || c == CtrlP ) n = n > 0 ? n-1 : ye - ya - 2 ;
      else if ( c == CDO || c == CtrlN ) n = n < ye-ya-2 ? n+1 : 0 ;
      else if ( c == CLE || c == CtrlB ) {  c = nm > 0 ? nm-1 : MENOPT-1; break;  }
      else if ( c == CRI || c == CtrlF ) {  c = nm < MENOPT-1 ? nm+1 : 0;  break;  }
      else if ( c == POS1 || c == CtrlA ) n = 0;
      else if ( c == ENDE || c == CtrlE ) n = ye-ya-2;
      else
      {  e_close_view(pic, 1);
	 if(c != ESC &&  e_tst_dfkt(f, c) == 0) return(ESC);
#ifdef NEWSTYLE
	 pic = e_open_view(xa+1, ya, xe-1, ye, f->fb->mt.fb, 1);
#else
	 pic = e_open_view(xa, ya, xe, ye, f->fb->mt.fb, 1);
#endif
	 e_std_rahmen(xa+1, ya, xe-1, ye, NULL, 0, f->fb->mr.fb, 0);
	 for (i = ya + 1; i < ye; i++)
	 e_pr_str_scan(xa+3, i, fopt[i-ya-1].t, f->fb->mt.fb,
				fopt[i-ya-1].x, 1, f->fb->ms.fb, xa+2, xe-2);
	 e_pr_str_scan(xa+3, n+ya+1, fopt[n].t, f->fb->mz.fb,
				fopt[n].x, 1, f->fb->mz.fb, xa+2, xe-2);
      }
   }
   e_close_view(pic, 1);
   return(c == ESC ? 255 : c);
}

/*  Optionen Struktur beschreiben   */

OPTK e_optkin(t, x, o, fkt)
     char *t;
     int x;
     char o;
     int (*fkt)();
{
   OPTK opt;
   opt.t = t;
   opt.x = x;
   opt.o = o;
   opt.fkt = fkt;
   return(opt);
}


