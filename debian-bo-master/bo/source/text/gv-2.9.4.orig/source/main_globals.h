/*
**
** main_globals.h
**
** Copyright (C) 1995-1997 Johannes Plass
** 
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
** 
** Author:   Johannes Plass (plass@thep.physik.uni-mainz.de)
**           Department of Physic
**           Johannes Gutenberg-University
**           Mainz, Germany
**
*/

#ifndef _GV_MAIN_GLOBALS_H_
#define _GV_MAIN_GLOBALS_H_

#ifdef _GV_MAIN_C_
#   define EXTERN
#else
#   define EXTERN extern
#endif

EXTERN Bool			show_date;
EXTERN Bool			show_locator;
EXTERN Bool			show_title;
EXTERN Bool			show_date;
EXTERN Bool			show_locator;
EXTERN Bool			show_title;
EXTERN Bool			show_toggleCurrentPage;
EXTERN Bool			show_toggleAllPages;
EXTERN Bool			show_toggleEvenPages;
EXTERN Bool			show_toggleOddPages;
EXTERN Bool			show_unmarkAllPages;
EXTERN Bool			show_saveMarkedPages;
EXTERN Bool			show_saveAllPages;
EXTERN Bool			show_printMarkedPages;
EXTERN Bool			show_printAllPages;
EXTERN Bool			show_openFile;
EXTERN Bool			show_autoResize;
EXTERN Bool			show_showThisPage;
EXTERN Bool			show_updateFile;
EXTERN Bool			show_checkFile;
EXTERN Bool			show_panner;


EXTERN float                   default_xdpi;           /* default xdpi from ghostview widget */
EXTERN float                   default_ydpi;           /* default ydpi from ghostview widget */
EXTERN int                     num_ghosts;             /* number of ghostview widgets active */
EXTERN FILE                    *psfile;                /* file to display */
EXTERN String                  gv_filename;            /* its filename */
EXTERN String                  gv_oldfilename;         /* previous filename */
EXTERN String                  gv_scanfilename;        /* previous filename */
EXTERN int                     current_page;           /* current page being displayed */
EXTERN int                     current_magstep;        /* current magnification */
EXTERN int                     current_llx;            /* current bounding box */
EXTERN int                     current_lly;
EXTERN int                     current_urx;
EXTERN int                     current_ury;
EXTERN String                  toc_text;               /* page labels (Table of Contents) */
EXTERN int                     toc_length;             /* length of page label text */
EXTERN int                     toc_entry_length;       /* length of one entry */
EXTERN int                     gv_print_mode;          /* printing mode */
EXTERN int                     gv_save_mode;           /* saving mode */
EXTERN time_t                  mtime;                  /* last modified time of input file */
EXTERN struct document         *doc;                   /* document structure */
EXTERN struct document         *olddoc;                /* document structure */
EXTERN Atom                    wm_delete_window;       /* Atom sent to destroy a window */
EXTERN XErrorHandler           old_Xerror;             /* standard error handler */
EXTERN Boolean                 dying;                  /* whether an X error caused our exit */
EXTERN XErrorEvent             bomb;                   /* what the error was */

EXTERN int			gv_scroll_mode;

EXTERN int			gv_pagemedia;
EXTERN int			gv_pagemedia_old;
EXTERN int			gv_pagemedia_auto;
EXTERN int			gv_pagemedia_auto_old;
EXTERN int			gv_default_pagemedia;
EXTERN int			gv_fallback_pagemedia;
EXTERN int			gv_num_std_pagemedia;

EXTERN int			gv_orientation;
EXTERN int			gv_orientation_old;
EXTERN int			gv_orientation_auto;
EXTERN int			gv_orientation_auto_old;
EXTERN int			gv_fallback_orientation;
#ifdef USE_SWAP_LANDSCAPE
EXTERN int			gv_swap_landscape;
EXTERN int			gv_swap_landscape_old;
#endif

EXTERN Display			*gv_display;
EXTERN String			gv_class;
EXTERN String			gv_name;
EXTERN String			gv_intern_name;
EXTERN String			gv_application_name;
EXTERN String			gv_user_defaults_file;
EXTERN String			gv_style_file;
EXTERN String			gv_ad_file;
EXTERN int			gv_print_mode;
EXTERN int			gv_save_mode;
EXTERN String			gv_print_command;	/* command used to print doc, usually "lpr" */
EXTERN int			gv_print_kills_file;	/* whether the print symbiont removes the file after printing */
EXTERN int			gv_exiting;		/* flag set when exiting gv */

EXTERN String			gv_gs_interpreter;
EXTERN String			gv_gs_x11_device;
EXTERN String			gv_gs_x11_alpha_device;
EXTERN String			gv_gs_cmd_scan_pdf;	/* command used to extract DSC outlines from a pdf file */
EXTERN String			gv_gs_cmd_conv_pdf;	/* command used to convert a pdf file to ps  */
EXTERN int			gv_gs_safer;
EXTERN int                      gv_gs_quiet;
EXTERN String                   gv_gs_arguments;

/* if a page is requested but gv is busy the following variable
holds the number of the requested page.
As soon as gv is notified that rendering a page is complete
this variable is checked and the corresponding page
will be displayed if necessary
*/
EXTERN int			gv_pending_page_request;

EXTERN XtAppContext 		app_con;
EXTERN AppResources 		app_res;

/* Widgets */
EXTERN Widget   toplevel;
EXTERN Widget      control;
EXTERN Widget         titlebutton;
EXTERN Widget            titlemenu;
EXTERN Widget         datebutton;
EXTERN Widget            datemenu;
EXTERN Widget         locator;
EXTERN Widget         fileButton;
EXTERN Widget            fileMenu;
EXTERN Widget               openEntry;
EXTERN Widget               reopenEntry;
EXTERN Widget               printAllEntry;
EXTERN Widget               printMarkedEntry;
EXTERN Widget               saveAllEntry;
EXTERN Widget               saveMarkedEntry;
EXTERN Widget               stopEntry;
EXTERN Widget               dscEntry;
EXTERN Widget               optionEntry;
EXTERN Widget               copyrightEntry;
EXTERN Widget               quitEntry;
EXTERN Widget         pageButton;
EXTERN Widget            pageMenu;
EXTERN Widget               nextEntry;
EXTERN Widget               showEntry;
EXTERN Widget               prevEntry;
EXTERN Widget               centerEntry;
EXTERN Widget         magstepButton;
EXTERN Widget            magstepMenu;
EXTERN Widget               *magstepEntry;
EXTERN Widget         orientationButton;
EXTERN Widget            orientationMenu;
EXTERN Widget               autoOrientEntry;
EXTERN Widget               portraitEntry;
EXTERN Widget               landscapeEntry;
EXTERN Widget               upsidedownEntry;
EXTERN Widget               seascapeEntry;
EXTERN Widget         processButton;
EXTERN Widget            processMenu;
#ifdef USE_SWAP_LANDSCAPE
EXTERN Widget               swapEntry;
#endif
EXTERN Widget         pagemediaButton;
EXTERN Widget		 pagemediaMenu;
EXTERN Widget               autoMediaEntry;
EXTERN Widget		    *pagemediaEntry;
EXTERN Widget         tocFrame;
EXTERN Widget            toc;
EXTERN Widget         w_toggleCurrentPage;
EXTERN Widget         w_toggleAllPages;
EXTERN Widget         w_toggleEvenPages;
EXTERN Widget         w_toggleOddPages;
EXTERN Widget         w_unmarkAllPages;
EXTERN Widget         w_saveMarkedPages;
EXTERN Widget         w_saveAllPages;
EXTERN Widget         w_printMarkedPages;
EXTERN Widget         w_printAllPages;
EXTERN Widget         w_openFile;
EXTERN Widget         w_autoResize;
EXTERN Widget         w_showThisPage;
EXTERN Widget         w_checkFile;
EXTERN Widget         w_updateFile;

EXTERN Widget         pannerFrame;
EXTERN Widget            panner;
EXTERN Widget            slider;

EXTERN Widget         viewFrame;
EXTERN Widget            viewClip;
EXTERN Widget               viewControl;
EXTERN Widget                  page;

/* Popup widgets */
EXTERN Widget	infopopup;
EXTERN Widget	optionpopup;
EXTERN Widget	dialogpopup;
EXTERN Widget	notepopup;
EXTERN Widget	versionpopup;

EXTERN Widget	FileSel_popup;
EXTERN Widget		FileSel;

#undef EXTERN
#endif /* _GV_MAIN_GLOBALS_H_ */
