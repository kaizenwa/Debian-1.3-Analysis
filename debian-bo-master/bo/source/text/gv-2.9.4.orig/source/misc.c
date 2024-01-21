/*
**
** misc.c
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

/*
 * This code is derived from:
*/

/*
 * misc.c -- Everything that isn't a callback or action.
 * Copyright (C) 1992  Timothy O. Theisen
 *   Author: Tim Theisen           Systems Programmer
 * Internet: tim@cs.wisc.edu       Department of Computer Sciences
 *     UUCP: uwvax!tim             University of Wisconsin-Madison
 *    Phone: (608)262-0438         1210 West Dayton Street
 *      FAX: (608)262-9777         Madison, WI   53706
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
*/

/*
#define MESSAGES
*/
#include "message.h"

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#ifndef SEEK_SET
#   define SEEK_SET 0
#endif

#include <signal.h>
#ifdef SIGNALRETURNSINT
#   define SIGVAL int
#else
#   define SIGVAL void
#endif

#define GV_MAXLENGTH 512

#ifdef VMS
#   include<descrip.h>
#   include<lnmdef.h>
#   include<lib$routines.h>
#   include<starlet.h>
#   include<rmsdef.h>
#else  
#   include <sys/types.h>
#   include <sys/stat.h>
#   include <unistd.h>
#endif

#include <math.h>

#include "paths.h"
#include INC_X11(Xos.h)
#include INC_X11(Xatom.h)
#include INC_X11(Intrinsic.h)
#include INC_X11(StringDefs.h)
#include INC_X11(Shell.h)
#include INC_XAW(Cardinals.h)
#include INC_XAW(SimpleMenu.h)
#include INC_XAW(SmeBSB.h)
#include INC_XAW(SmeLine.h)
#include INC_XAW(AsciiText.h)
#include INC_X11(IntrinsicP.h)
#include INC_XAW(TextP.h)
#include INC_XMU(StdCmap.h)
#include "Aaa.h"
#include "Frame.h"
#include "Ghostview.h"

#ifdef VMS
#   include <unixio.h>
#   define unlink remove
#endif

#include "actions.h"
#include "callbacks.h"
#include "d_memdebug.h"
#include "file.h"
#include "gv.h"
#include "ps.h"
#include "doc_misc.h"
#include "info.h"
#include "main_resources.h"
#include "main_globals.h"
#include "misc.h"
#include "note.h"
#include "error.h"
#include "save.h"

#ifndef max
#   define max(a, b)	((a) > (b) ? (a) : (b))
#endif
#ifndef min
#   define min(a, b)	((a) < (b) ? (a) : (b))
#endif

#define UNMAP_CONTROL	(1<<0)
#define UNMAP_PAGEVIEW	(1<<1)
#define UNMAP_PAGE	(1<<2)
#define MAP_CONTROL	(1<<3)
#define MAP_PAGEVIEW	(1<<4)
#define MAP_PAGE	(1<<5)

#if NeedFunctionPrototypes
#   define PT(aaa) aaa
#else 
#   define PT(aaa) ()
#endif
static Boolean set_new_magstep      PT(());
static Boolean set_new_orientation  PT((int));
static Boolean set_new_pagemedia    PT((int));
static void    build_pagemedia_menu PT(());
static Widget  build_label_menu     PT((Widget,String,String,Pixmap));
static void    layout_ghostview     PT(());
#undef PT

/*############################################################*/
/* misc_savePagePosition */
/*############################################################*/

static int pagepos_x,pagepos_y,pagepos_saved=0;

void
misc_savePagePosition()
{
  GhostviewReturnStruct grs;
  Position x1,x2,y1,y2;
  BEGINMESSAGE(misc_savePagePosition)
  x1 = x2 = ((Position)viewClip->core.width)/2  - viewControl->core.x - page->core.x;
  y1 = y2 = ((Position)viewClip->core.height)/2 - viewControl->core.y - page->core.y;
  GhostviewGetBBofArea (page,x1,y1,x2,y2,&grs);
  pagepos_x = (int) grs.psx;
  pagepos_y = (int) grs.psy;
  pagepos_saved = 1;
  ENDMESSAGE(misc_savePagePosition)
}

/*############################################################*/
/* misc_restorePagePosition */
/*############################################################*/

int
misc_restorePagePosition(xP,yP)
  int *xP;
  int *yP;
{
  BEGINMESSAGE(misc_restorePagePosition)
  if (pagepos_saved) {
    *xP = pagepos_x;
    *yP = pagepos_y;
    ENDMESSAGE(misc_restorePagePosition)
    return(1);
  }
  ENDMESSAGE(misc_restorePagePosition)
  return(0);
}

/*############################################################*/
/* misc_restorePagePosition */
/*############################################################*/

void
misc_resetPagePosition()
{
  BEGINMESSAGE(misc_resetPagePosition)
  pagepos_saved = 0;
  ENDMESSAGE(misc_resetPagePosition)
}

/*------------------------------------------------------------*/
/* misc_openFile */
/*------------------------------------------------------------*/

String
misc_openFile(name, fpP)
   String name;
   FILE **fpP;
{
   char *str,*error=NULL;
   FILE *fp=NULL;

   BEGINMESSAGE(misc_openFile)

   if (!name) {
      INFMESSAGE(no filename)
      str = "No file-name provided.";
      error=GV_XtNewString(str);
   }
   else if (strcmp(name, "-")) {
      INFSMESSAGE(trying to open,name)
      if (file_fileIsNotUseful(name)) {
         INFMESSAGE(file is not useful)
         str="Invalid file specification\nor useless file.";
         error=GV_XtNewString(str);         
      }
      else if ((fp = fopen(name, "r")) == NULL) {
         INFMESSAGE(failed to open)
         INFIMESSAGE(error number,errno)
         error = open_fail_error(errno,GV_ERROR_OPEN_FAIL,name,0);
      }
   }
   if (fpP) *fpP=fp;
   else if (fp) fclose(fp);

   ENDMESSAGE(misc_openFile)
   return(error);
}

/*############################################################*/
/* misc_testFile */
/*############################################################*/

String
misc_testFile(name)
   String name;
{
   char *error;
   BEGINMESSAGE(misc_testFile)
   error = misc_openFile(name,NULL);
   ENDMESSAGE(misc_testFile)
   return(error);
}

/*------------------------------------------------------------*/
/* misc_changeFile */
/*------------------------------------------------------------*/

static String
misc_changeFile(name)
   String name;
{
   FILE *fp=NULL;
   struct stat sbuf;
   String error=NULL;

   BEGINMESSAGE(misc_changeFile)
   INFSMESSAGE(trying to open,name)
   error = misc_openFile(name,&fp);
   if (!error) {
      if (gv_oldfilename) GV_XtFree(gv_oldfilename);
      gv_oldfilename = gv_filename;
      gv_filename = GV_XtNewString(name);
      if (gv_scanfilename) {
         unlink(gv_scanfilename);
         PS_XtFree(gv_scanfilename);
         gv_scanfilename=NULL;
      }
      if (psfile) fclose(psfile);
      psfile = fp;
      stat(gv_filename, &sbuf);
      mtime = sbuf.st_mtime;
      INFSMESSAGE(new,gv_filename)
   } else if (name && (!strcmp(name,"-"))) {
      INFMESSAGE(using stdin as input)
      error = NULL;
      if (gv_oldfilename) GV_XtFree(gv_oldfilename);
      gv_oldfilename = gv_filename;
      gv_filename = GV_XtNewString(name); 
      if (gv_scanfilename) {
         unlink(gv_scanfilename);
         PS_XtFree(gv_scanfilename);
         gv_scanfilename=NULL;
      }
      if (psfile) fclose(psfile);
      psfile = NULL;
   }
   ENDMESSAGE(misc_changeFile)
   return(error);
}

/*############################################################*/
/* close_file */
/*############################################################*/

String close_file(file,name)
   FILE *file;
   String name;
{
   char *error=NULL;
   char *error_close_fail     = "Cannot close file %s\n";

   BEGINMESSAGE(close_file)
   if (file && fclose(file)!=0) {
      char tmp[512];
      sprintf(tmp,error_close_fail,name);
      error=GV_XtNewString(tmp);
   }
   ENDMESSAGE(close_file)
   return(error);
}

/*############################################################*/
/* check_file */
/* check if there is a new version of the file */
/* returns -1 if no filename or error in checking file */
/*          0 if no new version exists */
/*          1 if new version exists */
/*############################################################*/

int
check_file(mode)
int mode;
{
   int status=0;
   struct stat sbuf;
   char *tmpname;
   int  r = -1;
#  ifdef VMS
      char *pos;
#  endif

   BEGINMESSAGE(check_file)

   if (!gv_filename) {
      INFMESSAGE(no filename) ENDMESSAGE(check_file)
      return(r);
   }
   if (!strcmp(gv_filename,"-")) {
      INFMESSAGE(reading from stdin; nothing to update) ENDMESSAGE(check_file) return(0);
   }

#ifdef VMS
   if (mode&CHECK_FILE_DATE) {
#else
   if (1) {
#endif
      INFMESSAGE(checking file date)
      status = stat(gv_filename, &sbuf);
      if (!status && mtime != sbuf.st_mtime) {
         INFMESSAGE(file has changed)
         ENDMESSAGE(check_file)
         return(1);
      }
   }

   tmpname=gv_filename;
   r=status;

#ifdef VMS

   if ( (mode&CHECK_FILE_VERSION) ||
        (status&&(mode&CHECK_FILE_DATE)&&strrchr(gv_filename,';'))
      ) {
      tmpname = GV_XtNewString(gv_filename);
      pos = strrchr(tmpname,';'); /* strip the version */
      if (pos) *pos='\0';
      status = stat(tmpname, &sbuf);
      if (!status) {
         if (mtime != sbuf.st_mtime) {
            if (pos) { /* get the full specification of the new file*/
               unsigned long s;
               char newfile[256];
               struct dsc$descriptor_s sd;
               struct dsc$descriptor_s fd;
               unsigned long context = 0;       

               sd.dsc$w_length  = (unsigned short)strlen(tmpname);
               sd.dsc$b_dtype   = DSC$K_DTYPE_T;
               sd.dsc$b_class   = DSC$K_CLASS_S;
               sd.dsc$a_pointer = tmpname;
               fd.dsc$w_length  = sizeof(newfile)-1;
               fd.dsc$b_dtype   = DSC$K_DTYPE_T;
               fd.dsc$b_class   = DSC$K_CLASS_S;
               fd.dsc$a_pointer = newfile;
               s=lib$find_file(&sd,&fd,&context,0,0,0,0);
               if (s != RMS$_SUC) {
                  INFMESSAGE(not found) r = -1;
               } else {
                  newfile[fd.dsc$w_length]='\0';
                  pos = strchr(newfile,' ');
                  if (pos) *pos = '\0';
                  INFSMESSAGE(found new file:,newfile)
                  if (gv_oldfilename) GV_XtFree(gv_oldfilename);
                  gv_oldfilename = gv_filename;
                  gv_filename=GV_XtNewString(newfile);
                  INFSMESSAGE(new file:,gv_filename)
                  r = 1;
               }
               lib$find_file_end(&context);
             } else { /* file changed, but filename shows no version number */
                INFSMESSAGE(new file:,gv_filename)
                r = 1;
             }
         } else {
           INFMESSAGE(no new version)
           r = 0;
         }
      } else r=status;
   }
#endif /* VMS */

   if (r<0) {
     char message[GV_MAXLENGTH]; 
     if (r != -2) {
        INFSMESSAGE(cannot access file:,tmpname)
        sprintf(message,"Unable to access file '%s'\n",tmpname);
     } else { /* privilege violation */
        INFSMESSAGE(user not authorized to access file:,tmpname)
        sprintf(message,"User is not authorized to access file '%s'\n",tmpname);
     }
     NotePopupShowMessage(message);
   }
   if (gv_filename!=tmpname) GV_XtFree(tmpname);
   ENDMESSAGE(check_file)
   return(r);
}

/*------------------------------------------------------------*/
/* show_currentPageMarker */
/*------------------------------------------------------------*/

static void
show_currentPageMarker(show)
   Boolean show;
{
   INFMESSAGE(executing set_page_mark)
   if (toc_text && (current_page >= 0)) {
      int marker = current_page*toc_entry_length + toc_entry_length-2;
      if (show) toc_text[marker] = '<';
      else      toc_text[marker] = ' ';
      XawTextInvalidate(toc, marker, marker+1);
   }
}

/*------------------------------------------------------------*/
/* render_page */
/* Start rendering a new page */
/*------------------------------------------------------------*/

static void
render_page(gvw,number)
    Widget gvw;
    int number;
{
    int i;

    BEGINMESSAGE(render_page)

    if (!gv_filename) { INFMESSAGE(no file) ENDMESSAGE(render_page) return; }

    INFIMESSAGE(displaying page,number)

    if (toc_text) {
       Boolean processflag;
       Boolean idleflag;
       Boolean noinputflag;
       INFMESSAGE(toc available)
       GhostviewState(gvw,&processflag,&idleflag,&noinputflag);
#      ifdef MESSAGES
          if (processflag) {INFMESSAGE(interpreter running)}
          else             {INFMESSAGE(no interpreter running)}
          if (idleflag)    {INFMESSAGE(widget is idle)}
          else             {INFMESSAGE(widget is busy)}
          if (noinputflag) {INFMESSAGE(interpreter has no input)}
          else             {INFMESSAGE(interpreter has input)}
#      endif
       /* Check first what the state of the ghostview widget is.
          Some documents show additional lines between
          the 'showpage' and the next '%%Page' comment.
          In this case the 'noinputflag' is 'False' but the additional
          lines are not really of significance (at least in no document I have
          encountered).
          So we ignore this flag and start from scratch only if the widget is
          busy or if no interpreter is running.
          Only if 'GV_RESTART_IF_CBUSY' is defined the noinputflag will be
          considered.
       */
#ifdef GV_RESTART_IF_BUSY /* ###jp### added 1.2.95 */
       if (processflag && idleflag && noinputflag) {
#else
       if (processflag && idleflag) {
#endif
          INFMESSAGE(displaying next page)
 	  GhostviewNextPage(gvw);
       } else {
          INFMESSAGE(starting new interpreter)
 	  GhostviewEnableInterpreter(gvw);
	  GhostviewSendPS(gvw, psfile, doc->beginprolog,
			  doc->lenprolog, False);
	  GhostviewSendPS(gvw, psfile, doc->beginsetup,
			  doc->lensetup, False);
       }
       if (doc->pageorder == DESCEND) i = (doc->numpages - 1) - current_page;
       else                           i = current_page;
       GhostviewSendPS(gvw, psfile, doc->pages[i].begin,doc->pages[i].len, False);
    } else {
       INFMESSAGE(no toc available)
       if (!GhostviewIsInterpreterRunning(gvw)) {
          INFMESSAGE(enabling interpreter for unstructured document)
          GhostviewEnableInterpreter(gvw);
       }
       else if (GhostviewIsInterpreterReady(gvw)) {
          INFMESSAGE(displaying page of unstructured document)
          GhostviewNextPage(gvw);
       }
       else {
          INFMESSAGE(interpreter running but not ready)
          XBell(XtDisplay(gvw), 0);
       }
    }

    if (gvw == page) {
       INFIMESSAGE(current page:,current_page)
       if (toc_text) {
          XtSetSensitive(prevEntry, current_page != 0);
          XtSetSensitive(nextEntry, current_page != (int)doc->numpages-1);
       }
       if (toc_text) {
          INFMESSAGE(marking current_page as current)
          XawTextUnsetSelection(toc);
          show_currentPageMarker(True);
          XawTextSetInsertionPoint(toc, current_page * toc_entry_length);
       }
    }

    ENDMESSAGE(render_page)
}

/*############################################################*/
/* show_page */
/* This routine is probably the heart of GV */
/* It receives requests from the various callbacks and actions, */
/* maps them onto three flags (need_layout, need_setup, need_render) */
/* and calls the necessary subroutines */
/*############################################################*/

void
show_page(number,data1)
   int number;
   XtPointer data1;
{
   Bool need_layout = False;
   Bool need_setup  = False;
   Bool need_render = False;
   int request=number;

   BEGINMESSAGE(show_page)
   INFIMESSAGE(received,request)

/*
    if (!gv_filename && request!=REQUEST_SETUP && request!=REQUEST_TOGGLE_RESIZE) {
       INFMESSAGE(no filename) ENDMESSAGE(show_page) return;
    }
*/
/*
    if (!gv_filename && page) 
       { if (page->core.mapped_when_managed) show_widgets(UNMAP_PAGE);  }
    else 
       { if (!(page->core.mapped_when_managed)) show_widgets(MAP_PAGE); }
*/

   if ( /* check if file has changed */
        gv_filename &&
        (request != REQUEST_NEW_FILE) &&
        (request != REQUEST_REOPEN) &&
        (request != REQUEST_TOGGLE_RESIZE) &&
        (request != REQUEST_SETUP) 
      ) {
      int changed = check_file(CHECK_FILE_DATE);
      if (changed==1) {
         INFMESSAGE(file has changed; requesting new file)
         request = REQUEST_NEW_FILE;
      } else if (changed == -1) {
         INFMESSAGE(file is not accessible)
         ENDMESSAGE(show_page)
         return;
      }
   }

   if (!toc_text && (request==REQUEST_REDISPLAY)) {
      INFMESSAGE(request to redisplay non DSC file; changing to request for new file)
      request=REQUEST_NEW_FILE;
   }

   if (request >= NO_CURRENT_PAGE) {
      INFMESSAGE(request for new page)
      if (GhostviewIsBusy(page)) {
         INFMESSAGE(busy state)
         if (toc_text) {
            number = doc_putPageInRange(doc,number);
            gv_pending_page_request=number;
            INFIMESSAGE(will remember,gv_pending_page_request)
         }
         ENDMESSAGE(show_page)
         return;
      }
      show_currentPageMarker(False);
      need_layout = need_setup = 
          set_new_orientation(number)|set_new_pagemedia(number);
      need_render = True;
   } else if (request<NO_CURRENT_PAGE) {
      INFIMESSAGE(analyzing,request)
      switch (request) {
      case REQUEST_TOGGLE_RESIZE:
		INFMESSAGE(### request for change of resize behaviour)
		number=current_page;
		need_layout = True;
		need_setup  = False;
		need_render = False;
		break;
      case REQUEST_REDISPLAY:
		INFMESSAGE(### request for redisplay)
		number=current_page;
		need_layout = False;
		need_setup  = False;
		need_render = True;
		break;
      case REQUEST_SETUP:
		INFMESSAGE(### request for setup)
		number=current_page;
		need_layout =	set_new_magstep()
				|set_new_orientation(number)
				|set_new_pagemedia(number);
                need_setup  = True;
		need_render = True;
		break;
      case REQUEST_NEW_MAGSTEP:
		INFMESSAGE(### request for new magstep)
		number=current_page;
		need_layout = need_setup = need_render = 
				set_new_magstep();
                if (!need_layout) {ENDMESSAGE(show_page) return;}
                break;
      case REQUEST_NEW_PAGEMEDIA:
		INFMESSAGE(### request for new pagemedia)
		number=current_page;
		need_layout = need_setup = need_render =
				set_new_pagemedia(number);
                if (!need_layout) {ENDMESSAGE(show_page) return;}
                break;
      case REQUEST_NEW_ORIENTATION:
		INFMESSAGE(### request for new orientation)
		number=current_page;
		need_layout = need_setup = need_render =
				set_new_orientation(number);
                if (!need_layout) {ENDMESSAGE(show_page) return;}
		break;
      case REQUEST_REOPEN:
      case REQUEST_NEW_FILE:
	{
                String filename;
                String error = NULL;
                if (data1) filename = (String) data1;
                else       filename = gv_filename;
		INFMESSAGE(### request to open or reopen file)
                error = misc_changeFile(filename);
                if (error) {
		   NotePopupShowMessage(error);
		   GV_XtFree(error);
 		   ENDMESSAGE(show_page)
                   return;
                }
		if (request==REQUEST_REOPEN) {
		   INFMESSAGE(request to reopen file)
		   number=current_page;
		} else {
		   INFMESSAGE(request to open new file)
		   if (request==number) number=NO_CURRENT_PAGE;
		}
		need_layout = setup_ghostview();
		number = doc_putPageInRange(doc,number);
		need_layout = need_layout
				|set_new_orientation(number)
				|set_new_pagemedia(number);
                need_setup  = True;
                need_render = True;
		break;
	}
      default:
		INFMESSAGE(### unknown request)
		fprintf(stderr,"  %s: Unknown request in show_page\n",gv_application_name);
		ENDMESSAGE(show_page)
		return;
      }
   }

   if (!psfile && need_render) {
      INFMESSAGE(no psfile; forcing setup and layout)
      need_setup=True;
      need_layout=True;
   }

#  ifdef MESSAGES
      if (need_layout) {INFMESSAGE(### need layout)} else {INFMESSAGE(### do not layout)}
      if (need_setup)  {INFMESSAGE(### need setup)}  else {INFMESSAGE(### do not setup)}
      if (need_render) {INFMESSAGE(### need render)} else {INFMESSAGE(### do not render)}
#  endif

/*
   if (need_setup && filename) {
      if (we have unmapped windows) GhostviewClearBackground(page);
   }
*/
   if (need_layout) layout_ghostview();

   if (need_setup)  GhostviewSetup(page);
   if (!gv_filename) {
      need_render=False;
      INFMESSAGE(no filename; forcing no render)
   }
   if (toc_text) {
      number = doc_putPageInRange(doc,number);
      current_page = number;
   }
   if (need_render)  render_page(page,number);

#if 0
   /* note that this may cause rendering before the windows are mapped.
      Up to now I had no problems due to the retardation in the display
      caused by starting gs, so let's try ....
      If this fails somehow we have to move the next line
      a little up and activate the disabled GhostviewClearBackground 
      lines above.
   */
   if (need_layout) show_widgets(MAP_WIDGETS);
#endif

   gv_pending_page_request=NO_CURRENT_PAGE; /* eliminate any pending requests now */

   ENDMESSAGE(show_page)
}

/*############################################################*/
/* setup_ghostview */
/* This includes:
 *  scanning the PostScript file,
 *  setting the title and date labels,
 *  building the pagemedia menu,
 *  building the toc (table of contents)
 *  sensitizing the appropriate menu buttons,
 *  popping down and erasing the infotext popup.
 */
/*############################################################*/

Boolean
setup_ghostview()
{
    Arg args[10];
    Cardinal n;
    int oldtoc_entry_length;
    char *tocp;
    static String nothing = "";
    Pixmap bitmap;
    String label,buttonlabel;

    BEGINMESSAGE(setup_ghostview)
    /* Reset to a known state. */
    psfree(olddoc);
    olddoc = doc;
    doc = NULL;
    current_page = NO_CURRENT_PAGE;
    if (toc_text) GV_XtFree(toc_text);
    oldtoc_entry_length = toc_entry_length;
    toc_text = NULL;

    INFMESSAGE(scanning file for structure information)
    doc_scanFile(&psfile,&doc,gv_filename,&gv_scanfilename,gv_gs_cmd_scan_pdf);

    {
      int m;
      m = gv_pagemedia;
      if (olddoc && olddoc->nummedia && m >= 0) {
	if (m >= olddoc->nummedia) m = m - olddoc->nummedia;
	else                       m = MEDIA_ID_INVALID;
      }
      if (doc && doc->nummedia && m >= 0) {
         m = m + doc->nummedia;
      }
      if (m != gv_pagemedia) gv_pagemedia = gv_pagemedia_old = m;
    }
    if (gv_pagemedia == MEDIA_ID_INVALID || gv_pagemedia == MEDIA_ID_BB) {
       int m;
       gv_pagemedia_old         = MEDIA_ID_INVALID;
       m = doc_convStringToPageMedia(doc,app_res.default_pagemedia);
       if (m== MEDIA_ID_AUTO) {
          gv_pagemedia_auto     = 1;
          gv_pagemedia_auto_old = 0;
          gv_pagemedia          = MEDIA_ID_INVALID;
       } else {
          gv_pagemedia_auto     = 0;
          gv_pagemedia_auto_old = 1;
          gv_pagemedia          = m;
       }
    }

    if (show_title) {
       if (doc && doc->title) {
          buttonlabel = doc->title;
          label = doc->title;
          bitmap = app_res.document_bitmap;
       } 
       else {
          if (gv_filename) {
#            ifdef VMS
                buttonlabel = strrchr(gv_filename,']');
                if (!buttonlabel) buttonlabel = strrchr(gv_filename,':');
                if (buttonlabel) buttonlabel++;
                else buttonlabel = gv_filename;
                label = gv_filename;
#            else
                buttonlabel = gv_filename;
                label = gv_filename;
#            endif
          }
          else { buttonlabel = ""; label = ""; }
          bitmap = None;
       }
                                                  n=0;
       XtSetArg(args[n], XtNlabel, buttonlabel);  n++;
       XtSetValues(titlebutton, args, n);  
       if (titlemenu) XtDestroyWidget(titlemenu); 
       titlemenu = build_label_menu(titlebutton, "title", label, bitmap);
    }

    if (show_date) {
       if (doc && doc->date) {
          label = doc->date;
          bitmap = app_res.document_bitmap;
       } 
       else {
          if (psfile) { label = ctime(&mtime); } 
          else { label = ""; }
          bitmap = None;
       }

                                                  n=0;
       XtSetArg(args[n], XtNlabel, label);        n++;
       XtSetValues(datebutton, args, n);
       if (datemenu) XtDestroyWidget(datemenu);
       datemenu = build_label_menu(datebutton, "date", label, bitmap);   
    }

    build_pagemedia_menu();

    /* Reset ghostscript and output messages popup */
#   ifdef VMS
    {
       Bool disable=False;
       if ( !doc || !olddoc ||
	    olddoc->beginprolog != doc->beginprolog ||
	    olddoc->endprolog != doc->endprolog ||
	    olddoc->beginsetup != doc->beginsetup ||
	    olddoc->endsetup != doc->endsetup
       ) disable=True;
       if (!disable) {
          char *fn = GV_XtNewString(gv_filename);
          char *ofn= GV_XtNewString(gv_oldfilename);
          char *pos;
          pos = strrchr(fn,';');  if (pos) *pos='\0';
          pos = strrchr(ofn,';'); if (pos) *pos='\0';
          if (strcmp(fn, ofn)) disable=True;
          GV_XtFree(fn);
          GV_XtFree(ofn);
       }
       if (disable==True) {   
	  GhostviewDisableInterpreter(page);
          cb_popdownInfoPopup((Widget)NULL,(XtPointer)NULL,(XtPointer)NULL);
          cb_resetInfoPopup((Widget)NULL,(XtPointer)NULL,(XtPointer)NULL);
       }
    }
#   else
    if (!doc || !olddoc ||
	strcmp(gv_oldfilename, gv_filename) ||
	olddoc->beginprolog != doc->beginprolog ||
	olddoc->endprolog != doc->endprolog ||
	olddoc->beginsetup != doc->beginsetup ||
	olddoc->endsetup != doc->endsetup) {
        INFMESSAGE(disabling interpreter)
	GhostviewDisableInterpreter(page);
        cb_popdownInfoPopup((Widget)NULL,(XtPointer)NULL,(XtPointer)NULL);
        cb_resetInfoPopup((Widget)NULL,(XtPointer)NULL,(XtPointer)NULL);
    }
#   endif

    /* Build table of contents */
    if (doc && doc->structured) {
	int maxlen = 0;
	int i, j;

        INFMESSAGE(toc available)
	if (doc->labels_useful) {
	    for (i = 0; i < doc->numpages; i++) 
		maxlen = max(maxlen, strlen(doc->pages[i].label));
	} else {
	    double x;
	    x = doc->numpages;
	    maxlen = log10(x) + 1;
	}
	toc_entry_length = maxlen + 3;
	toc_length = doc->numpages * toc_entry_length - 1;
	toc_text = GV_XtMalloc(toc_length + 2); /* include final NULL */

	for (i = 0, tocp = toc_text; i < doc->numpages;
	     i++, tocp += toc_entry_length) {
	    if (doc->labels_useful) {
		if (doc->pageorder == DESCEND) {
		    j = (doc->numpages - 1) - i;
		} else {
		    j = i;
		}
		sprintf(tocp, " %*s \n", maxlen, doc->pages[j].label);
	    } else {
		sprintf(tocp, " %*d \n", maxlen, i+1);
	    }
	}
	toc_text[toc_length] = '\0';
							n=0;
	XtSetArg(args[n], XtNfilename, NULL);      	n++;
	XtSetValues(page, args, n);
    } else {
        INFMESSAGE(toc not available)
	toc_length = 0;
	toc_entry_length = 3;
							 n=0;
	XtSetArg(args[n], XtNfilename, gv_filename);     n++;
	XtSetValues(page, args, n);
    }
							n=0;
    XtSetArg(args[n], XtNlength, toc_length);		n++;
    if (toc_text) {
	XtSetArg(args[n], XtNstring, toc_text);		n++;
    } else {
	/* Text widget sometimes blows up when given a NULL pointer */
	XtSetArg(args[n], XtNstring, nothing);		n++;
    }
    XtSetValues(toc, args, n);

    if (show_saveMarkedPages)	XtSetSensitive(w_saveMarkedPages,  (toc_text    != NULL));
    if (show_toggleCurrentPage)	XtSetSensitive(w_toggleCurrentPage,(toc_text    != NULL));
    if (show_toggleAllPages)	XtSetSensitive(w_toggleAllPages,   (toc_text    != NULL));
    if (show_toggleEvenPages)	XtSetSensitive(w_toggleEvenPages,  (toc_text    != NULL));
    if (show_toggleOddPages)	XtSetSensitive(w_toggleOddPages,   (toc_text    != NULL));
    if (show_unmarkAllPages)	XtSetSensitive(w_unmarkAllPages,   (toc_text    != NULL));
    if (show_saveMarkedPages)	XtSetSensitive(w_saveMarkedPages,  (toc_text   != NULL));
    if (show_saveAllPages)	XtSetSensitive(w_saveAllPages,     (psfile      != NULL));
    if (show_printMarkedPages)	XtSetSensitive(w_printMarkedPages, (toc_text    != NULL));
    if (show_printAllPages)	XtSetSensitive(w_printAllPages,    (psfile      != NULL));
    if (show_checkFile)		XtSetSensitive(w_checkFile,        (gv_filename != NULL));
    if (show_updateFile)	XtSetSensitive(w_updateFile,       (gv_filename != NULL));
    if (show_showThisPage)	XtSetSensitive(w_showThisPage,     (gv_filename != NULL));

    XtSetSensitive(reopenEntry,      (psfile      != NULL));
    XtSetSensitive(printAllEntry,    (psfile      != NULL));
    XtSetSensitive(printMarkedEntry, (toc_text    != NULL));
    XtSetSensitive(saveAllEntry,     (psfile      != NULL));
    XtSetSensitive(saveMarkedEntry,  (toc_text    != NULL));
    XtSetSensitive(nextEntry,        (gv_filename != NULL));
    XtSetSensitive(showEntry,        (gv_filename != NULL));
    XtSetSensitive(prevEntry,        (toc_text    != NULL));
    XtSetSensitive(centerEntry,      (gv_filename != NULL));

    ENDMESSAGE(setup_ghostview)
    return oldtoc_entry_length != toc_entry_length;
}

Dimension view_width, view_height, view_border;
Dimension toc_width, toc_height, toc_border;
Dimension toc_leftMargin, toc_rightMargin;
Dimension control_width, control_height;
Dimension page_width, page_height;
XFontStruct *toc_font;
Dimension tocFrame_width,tocFrame_desiredWidth,tocFrame_height,tocFrame_hSpace,tocFrame_shadowWidth;


/*------------------------------------------------------------*/
/* layout_ghostview */
/*------------------------------------------------------------*/

static void
layout_ghostview()
{
   Arg       args[10];
   Cardinal  n;
   Dimension tocFrame_width;
   Dimension page_prefWidth, page_prefHeight;
   Dimension page_width, page_height;
   static Boolean firsttime=True;
   Boolean auto_resize;

   BEGINMESSAGE(layout_ghostview)

   if (!firsttime) {
      XtSetArg(args[0], XtNallowShellResize,&auto_resize);
      XtGetValues(toplevel, args,ONE);
      if (auto_resize != app_res.auto_resize) {
         INFMESSAGE(######## changing resize behaviour)
#        ifdef MESSAGES
            if (app_res.auto_resize) {INFMESSAGE(shell is allowed to resize)}
            else                     {INFMESSAGE(shell must not resize)}
#        endif
         XtSetArg(args[0], XtNallowShellResize,app_res.auto_resize);
         XtSetValues(toplevel, args,ONE);
         if (show_autoResize) {
            if (app_res.auto_resize) XtSetArg(args[0],XtNlabel,GV_AUTO_RESIZE_YES);
            else                     XtSetArg(args[0],XtNlabel,GV_AUTO_RESIZE_NO);
            XtSetValues(w_autoResize, args,ONE);
            if (app_res.auto_resize==False) {
               ENDMESSAGE(layout_ghostview) return;
            }
            INFIMESSAGE(setting tocFrame height:,TOC3D_INITIAL_HEIGHT)
            XtSetArg(args[0], XtNheight,TOC3D_INITIAL_HEIGHT);
            XtSetValues(tocFrame, args, ONE);
         }
      }
   }

   INFMESSAGE(#### retrieving dimensions)
   XtSetArg(args[0], XtNpreferredWidth, &page_prefWidth);
   XtSetArg(args[1], XtNpreferredHeight, &page_prefHeight);
   XtSetArg(args[2], XtNwidth, &page_width);
   XtSetArg(args[3], XtNheight, &page_height);
   XtGetValues(page, args, FOUR);
   INFIIMESSAGE(## preferred,page_prefWidth,page_prefHeight)
   INFIIMESSAGE(## actual,page_width,page_height)

   toc_width = (toc_font->max_bounds.width+toc_font->min_bounds.width)/2*(toc_entry_length-1)
               + toc_leftMargin+toc_rightMargin;
   XtSetArg(args[0], XtNwidth, &tocFrame_width);
   XtGetValues(tocFrame, args, ONE); INFIMESSAGE(tocFrame:,tocFrame_width)
   tocFrame_desiredWidth = toc_width+2*toc_border+2*tocFrame_hSpace+2*tocFrame_shadowWidth;
   IIMESSAGE(tocFrame_width,tocFrame_desiredWidth)

   if (tocFrame_width!=tocFrame_desiredWidth) {
      INFMESSAGE(#### setting tocFrame to its desired width)
      XtSetArg(args[0], XtNwidth, tocFrame_desiredWidth);
      XtSetValues(tocFrame, args, ONE);
   }

   if (page_prefWidth != page_width || page_prefHeight != page_height) {
      INFMESSAGE(#### setting ghostview widget size to its preferred size)
/*
      if (!firsttime) GhostviewClearBackground(page);
*/
      XtSetArg(args[0], XtNwidth,           page_prefWidth);
      XtSetArg(args[1], XtNheight,          page_prefHeight);
      XtSetValues(page, args, TWO);
      cb_positionPage(page,(XtPointer)NULL,(XtPointer)NULL);
   }

   if (firsttime) {
     if (show_autoResize) {
       if (app_res.auto_resize) XtSetArg(args[0], XtNlabel,GV_AUTO_RESIZE_YES);
       else                     XtSetArg(args[0], XtNlabel,GV_AUTO_RESIZE_NO);
                                XtSetValues(w_autoResize, args,ONE);
     }
						            n=0;
     XtSetArg(args[n], XtNminWidth, (Dimension)app_res.minimum_width);  n++;
     XtSetArg(args[n], XtNminHeight,(Dimension)app_res.minimum_height);n++;
     if (app_res.auto_resize==False) {
        INFMESSAGE(switching to No-Resize mode)
        XtSetArg(args[n], XtNallowShellResize,app_res.auto_resize); n++;
     }
     XtSetValues(toplevel, args,n);
     firsttime=False;
   }
 
  ENDMESSAGE(layout_ghostview)
}

/*############################################################*/
/* setup_layout_ghostview */
/*############################################################*/

void
setup_layout_ghostview()
{
    Arg args[5];
    Cardinal n;

    BEGINMESSAGE(setup_layout_ghostview )

    INFMESSAGE(#### getting tocFrame dimensions)
								n=0;
    XtSetArg(args[n], XtNhSpace,          &tocFrame_hSpace);	n++;
    XtSetArg(args[n], XtNshadowWidth,     &tocFrame_shadowWidth);n++;
    XtGetValues(tocFrame, args, n);

    INFMESSAGE(#### getting toc info)
								n=0;
    XtSetArg(args[n], XtNfont, &toc_font);			n++;
    XtSetArg(args[n], XtNleftMargin, &toc_leftMargin);		n++;
    XtSetArg(args[n], XtNrightMargin, &toc_rightMargin);	n++;
    XtSetArg(args[n], XtNborderWidth, &toc_border);		n++;
    XtGetValues(toc, args, n);

    ENDMESSAGE(setup_layout_ghostview)
}

/*------------------------------------------------------------*/
/* set_new_magstep */
/*------------------------------------------------------------*/

static Boolean
set_new_magstep()
{
    int new_magstep;
    Boolean changed = False;
    Arg args[5];
    Cardinal n;
    float xdpi, ydpi;

    BEGINMESSAGE(set_new_magstep)
    new_magstep = app_res.magstep;
    /* If magstep changed, stop interpreter and setup for new dpi. */
    if (new_magstep != current_magstep) {
        char label[15];
        float scalefactor = 1.0;
        int scale = new_magstep;

	GhostviewDisableInterpreter(page);
	changed = True;
        if (scale<0) { while (scale++) scalefactor /= 1.18921; }
        else         { while (scale--) scalefactor *= 1.18921; }
	xdpi = default_xdpi * scalefactor;
	ydpi = default_ydpi * scalefactor;

        sprintf(label," %5.3f ",(scalefactor*scalefactor));
        XtSetArg(args[0], XtNlabel, label);
        XtSetValues(magstepButton, args, ONE);

							n=0;
	XtSetArg(args[n], XtNlxdpi, (1000*xdpi));	n++;
	XtSetArg(args[n], XtNlydpi, (1000*ydpi));	n++;
	XtSetValues(page, args, n);

	XtSetArg(args[0], XtNleftBitmap, None);
	XtSetValues(magstepEntry[current_magstep - app_res.minimum_magstep],
		    args, ONE);
	current_magstep = new_magstep;
    }
    XtSetArg(args[0], XtNleftBitmap, app_res.selected_bitmap);
    XtSetValues(magstepEntry[current_magstep - app_res.minimum_magstep],
		args, ONE);

    ENDMESSAGE(set_new_magstep)
    return changed;
}

/*------------------------------------------------------------*/
/* set_orientationButton_label */
/*------------------------------------------------------------*/

static void
set_orientationButton_label(orientation)
   int orientation;
{
   Arg args[1];
   Widget w = portraitEntry;
   String label;
    
   BEGINMESSAGE(set_orientationButton_label)
   if (orientation == O_LANDSCAPE)       w = landscapeEntry;
   else if (orientation == O_UPSIDEDOWN) w = upsidedownEntry;
   else if (orientation == O_SEASCAPE)   w = seascapeEntry;
   XtSetArg(args[0], XtNlabel,&label);
   XtGetValues(w, args, ONE);
   XtSetArg(args[0], XtNlabel,label);
   XtSetValues(orientationButton, args, ONE);
   ENDMESSAGE(set_orientationButton_label)
}

/*------------------------------------------------------------*/
/* set_selectedBitmap */
/*------------------------------------------------------------*/

static void 
set_selectedBitmap(w,selected)
   Widget w;
   int selected;
{
   Arg args[1];
   Pixmap bitmap;

   BEGINMESSAGE(set_selectedBitmap)
   if (selected) bitmap = app_res.selected_bitmap;
   else          bitmap = None;
   XtSetArg(args[0], XtNleftBitmap, bitmap);
   XtSetValues(w,args, ONE);   
   ENDMESSAGE(set_selectedBitmap)
}

/*------------------------------------------------------------*/
/* set_newBitmapIfChanged */
/*------------------------------------------------------------*/

static void 
set_newBitmapIfChanged(w,new_bitmap)
   Widget w;
   Pixmap new_bitmap;
{
   Arg args[1];
   Pixmap old_bitmap;

   BEGINMESSAGE(set_newBitmapIfChanged)
   XtSetArg(args[0], XtNleftBitmap, &old_bitmap);
   XtGetValues(w, args, ONE);
   if (new_bitmap != old_bitmap) {
      XtSetArg(args[0], XtNleftBitmap, new_bitmap);
      XtSetValues(w, args, ONE);
   }
   ENDMESSAGE(set_newBitmapIfChanged)
}

/*------------------------------------------------------------*/
/* set_new_orientation */
/*------------------------------------------------------------*/

static Boolean
set_new_orientation(pagenumber)
   int pagenumber;
{
   Boolean changed  = False;
   int from_doc = 0;
   int no;
   Widget w;
   Pixmap bitmap;
   XtPageOrientation xto,xto_old;

   BEGINMESSAGE(set_new_orientation)

   no = O_NONE;
   if (no == O_NONE && gv_orientation != gv_orientation_old) {
      INFIMESSAGE(forcing new orientation to be,no)
      no = gv_orientation;
      INFMESSAGE(disabling automatic orientation)
      gv_orientation_auto = 0;
   }
   if (no == O_NONE && gv_orientation_auto) {
      int po;
      po = doc_preferredOrientationOfPage(doc,pagenumber);
      INFIMESSAGE(using orientation from doc, po)
      if (po != O_NONE) {
         INFIMESSAGE(using orientation from doc, po)
         no = po;
         from_doc = 1;
      }
   }
   if (no==O_NONE) no = gv_orientation_old;
   if (no!=O_PORTRAIT && no!=O_LANDSCAPE && no!=O_SEASCAPE && no!=O_UPSIDEDOWN)
      no = gv_fallback_orientation;
   gv_orientation = no;

   xto     = doc_convDocOrientToXtOrient(gv_orientation,    gv_swap_landscape    );
   xto_old = doc_convDocOrientToXtOrient(gv_orientation_old,gv_swap_landscape_old);
   IIMESSAGE(xto,xto_old)

   if (xto != xto_old) {
      Arg args[1];
      if      (gv_orientation_old == O_PORTRAIT)   w = portraitEntry;
      else if (gv_orientation_old == O_LANDSCAPE)  w = landscapeEntry;
      else if (gv_orientation_old == O_UPSIDEDOWN) w = upsidedownEntry;
      else                                         w = seascapeEntry;
      set_selectedBitmap(w,0);

      INFIMESSAGE(changing orientation for page to be,xto)
      GhostviewDisableInterpreter(page);
      XtSetArg(args[0], XtNorientation, xto);
      XtSetValues(page, args, ONE);
      changed = True;
      set_orientationButton_label(gv_orientation);
   }

   if (from_doc) bitmap = app_res.document_bitmap;
   else          bitmap = app_res.selected_bitmap;
   if      ( no == O_PORTRAIT)   w = portraitEntry;
   else if ( no == O_LANDSCAPE)  w = landscapeEntry;
   else if ( no == O_UPSIDEDOWN) w = upsidedownEntry;
   else                          w = seascapeEntry;
   set_newBitmapIfChanged(w,bitmap);

   if (gv_swap_landscape != gv_swap_landscape_old)
      set_selectedBitmap(swapEntry,gv_swap_landscape);
   if (gv_orientation_auto != gv_orientation_auto_old)
      set_selectedBitmap(autoOrientEntry,gv_orientation_auto);

   gv_orientation_old       = gv_orientation;
   gv_orientation_auto_old  = gv_orientation_auto;
   gv_swap_landscape_old    = gv_swap_landscape;

   ENDMESSAGE(set_new_orientation)
   return(changed);
   
}

/*------------------------------------------------------------*/
/* set_pagemediaButton */
/*------------------------------------------------------------*/

static void
set_pagemediaButton_label(media_id)
   int media_id;
{ 
   String s = NULL;
   Arg args[1];

   BEGINMESSAGE(set_pagemediaButton_label)
   if (media_id>=0) {
      Widget w;
      if (pagemediaEntry[media_id]) w = pagemediaEntry[media_id];
      else                          w = pagemediaEntry[media_id-1];
      XtSetArg(args[0], XtNlabel, &s);
      XtGetValues(w, args, ONE);
   } 
   else if (media_id==MEDIA_ID_BB) s = "Bounding Box";
   else s = "?";
   XtSetArg(args[0], XtNlabel, s);
   XtSetValues(pagemediaButton, args, ONE);          
   ENDMESSAGE(set_pagemediaButton_label)
}

/*------------------------------------------------------------*/
/* set_new_pagemedia */
/*------------------------------------------------------------*/

static Boolean
set_new_pagemedia(pagenumber)
   int pagenumber;
{
   int new_llx,new_lly,new_urx,new_ury;
   Boolean changed = False;
   int from_doc = 0;
   Arg args[4];
   Widget w = NULL; 
   Pixmap bitmap;
   int num_doc_media;
   int nm;

   BEGINMESSAGE(set_new_pagemedia)

   nm = MEDIA_ID_INVALID;
   if (nm == MEDIA_ID_INVALID && gv_pagemedia != gv_pagemedia_old) {
      INFIMESSAGE(forcing new pagemedia to be,nm)
      nm = gv_pagemedia;
      INFMESSAGE(disabling automatic pagemedia)
      gv_pagemedia_auto = 0;
   }
   if (nm == MEDIA_ID_INVALID && gv_pagemedia_auto) {
      int pm;
      pm = doc_preferredMediaOfPage(doc,pagenumber,&new_llx,&new_lly,&new_urx,&new_ury);
      if (pm != MEDIA_ID_INVALID) {
         INFIMESSAGE(using pagemedia preferred from doc, pm)
         nm = pm;
         from_doc = 1;
      }
   }
   if (nm==MEDIA_ID_INVALID) nm = gv_pagemedia_old;
   if (nm==MEDIA_ID_INVALID) nm = gv_fallback_pagemedia;
   gv_pagemedia = nm;

   /* If pagemedia changed, remove the old marker. */
   IIMESSAGE(gv_pagemedia,gv_pagemedia_old)
   if (gv_pagemedia != gv_pagemedia_old) {
      if (gv_pagemedia_old>=0) {
         if (pagemediaEntry[gv_pagemedia_old]) w = pagemediaEntry[gv_pagemedia_old];
         else                                  w = pagemediaEntry[gv_pagemedia_old-1];
         set_selectedBitmap(w,0);
      }
      set_pagemediaButton_label(gv_pagemedia);
   }

   if (gv_pagemedia >= 0) {
      if (from_doc) bitmap = app_res.document_bitmap;
      else          bitmap = app_res.selected_bitmap;
      if (pagemediaEntry[gv_pagemedia]) w = pagemediaEntry[gv_pagemedia];
      else                              w = pagemediaEntry[gv_pagemedia-1];
      set_newBitmapIfChanged(w,bitmap);
   }

   if (gv_pagemedia_auto != gv_pagemedia_auto_old) set_selectedBitmap(autoMediaEntry,gv_pagemedia_auto);

   num_doc_media=0;
   if (doc) num_doc_media = doc->nummedia;
 
   if (gv_pagemedia == MEDIA_ID_BB) {
     /*  Don't have to anything here since doc_preferredMediaOfPage
      *  set new_llx, etc. in case the document provides a bounding
      *  box specification.
      */
   } else {
      new_llx = new_lly = 0;
      if (gv_pagemedia < num_doc_media) {
         new_urx = doc->media[gv_pagemedia].width-1;
	 new_ury = doc->media[gv_pagemedia].height-1;
      } else {
         new_urx = papersizes[gv_pagemedia-num_doc_media].width-1;
         new_ury = papersizes[gv_pagemedia-num_doc_media].height-1;
      }
   }

   /* If bounding box changed, setup for new size. */
   if ((new_llx != current_llx) || (new_lly != current_lly) ||
      (new_urx != current_urx) || (new_ury != current_ury)) {
      INFMESSAGE(bounding box changed)
      INFIIMESSAGE(lower left:,new_llx,new_lly)
      INFIIMESSAGE(upper right:,new_urx,new_ury)
      GhostviewDisableInterpreter(page);
      changed = True;
      current_llx = new_llx;
      current_lly = new_lly;
      current_urx = new_urx;
      current_ury = new_ury;
      XtSetArg(args[0], XtNllx, current_llx);
      XtSetArg(args[1], XtNlly, current_lly);
      XtSetArg(args[2], XtNurx, current_urx);
      XtSetArg(args[3], XtNury, current_ury);
      XtSetValues(page, args, FOUR);
   }

   gv_pagemedia_old = gv_pagemedia;
   gv_pagemedia_auto_old = gv_pagemedia_auto;

   ENDMESSAGE(set_new_pagemedia)
   return changed;
}

/*------------------------------------------------------------*/
/* same_document_media */
/*------------------------------------------------------------*/

static Boolean
same_document_media()
{
   int i;
   Boolean same = True;

   BEGINMESSAGE(same_document_media)
   if (olddoc == NULL && doc == NULL)          same=True;
   else if (olddoc == NULL || doc == NULL)     same=False;
   else if (olddoc->nummedia != doc->nummedia) same=False;
   else for (i = 0; i < doc->nummedia; i++) {
       if (strcmp(olddoc->media[i].name, doc->media[i].name)) {
          same=False;
          break;
       }
   }
   ENDMESSAGE(same_document_media)
   return(same);
}

/*------------------------------------------------------------*/
/* build_pagemedia_menu */
/*------------------------------------------------------------*/

static void
build_pagemedia_menu()
{
    Arg args[5];
    Cardinal n;
    Widget w;
    int i,num_doc_media;

    BEGINMESSAGE(build_pagemedia_menu)
    if (pagemediaMenu && same_document_media()) {
       ENDMESSAGE(build_pagemedia_menu)
       return;
    }
    if (pagemediaMenu) XtDestroyWidget(pagemediaMenu);

    pagemediaMenu = XtCreatePopupShell("menu", simpleMenuWidgetClass,
				       pagemediaButton, NULL, ZERO);

							n=0;
           XtSetArg(args[n], XtNleftMargin, 20);	n++;
    autoMediaEntry = XtCreateManagedWidget("automatic",smeBSBObjectClass,pagemediaMenu,args,n);
           XtAddCallback(autoMediaEntry,XtNcallback,cb_setPagemedia,(XtPointer)MEDIA_ID_AUTO);
    set_selectedBitmap(autoMediaEntry,gv_pagemedia_auto);
    XtCreateManagedWidget("line",smeLineObjectClass,pagemediaMenu,args,ZERO);

    /* Build the Page Media menu */
    /* the Page media menu has three parts.
     *  - the automatic media detection entry.
     *  - the document defined page medias
     *  - the standard page media defined from Adobe's PPD
     */
    num_doc_media = 0;
    if (doc) num_doc_media = doc->nummedia;

    i = gv_num_std_pagemedia + num_doc_media;
    if (pagemediaEntry) GV_XtFree(pagemediaEntry);
    pagemediaEntry = (Widget *) GV_XtMalloc(i * sizeof(Widget));

	       					n=0;
    XtSetArg(args[n], XtNleftMargin, 20);	n++;
    if (doc && doc->nummedia) {
	for (i = 0; i < doc->nummedia; i++) {
	    pagemediaEntry[i] = XtCreateManagedWidget(doc->media[i].name,
				smeBSBObjectClass, pagemediaMenu, args, n);
	    XtAddCallback(pagemediaEntry[i], XtNcallback,
			  cb_setPagemedia, (XtPointer)i);
	}
                                                n=0;
	w = XtCreateManagedWidget("line", smeLineObjectClass, pagemediaMenu,args, n);
    }

						n=0;
    XtSetArg(args[n], XtNleftMargin, 20);	n++;
    for (i = 0; papersizes[i].name; i++) {
	pagemediaEntry[i+num_doc_media] = NULL;
	if (i > 0) {
	    /* Skip over same paper size with small imageable area */
	    if ((papersizes[i].width == papersizes[i-1].width) &&
		(papersizes[i].height == papersizes[i-1].height)) {
		continue;
	    }
	}
	pagemediaEntry[i+num_doc_media] = XtCreateManagedWidget(
					    papersizes[i].name,
					    smeBSBObjectClass, pagemediaMenu,
					    args, n);
	XtAddCallback(pagemediaEntry[i+num_doc_media], XtNcallback,
		      cb_setPagemedia, (XtPointer)(i+num_doc_media));
    }
    ENDMESSAGE(build_pagemedia_menu)
}

/*------------------------------------------------------------*/
/* build_label_menu */
/*------------------------------------------------------------*/

static Widget
build_label_menu(parent, name, label, bitmap)
    Widget parent;
    String name, label;
    Pixmap bitmap;
{
    Arg args[5];
    Cardinal n;
    Widget menu, entry;

    BEGINMESSAGE(build_label_menu)
								n=0;
    menu = XtCreatePopupShell(GV_LABEL_MENU_NAME, simpleMenuWidgetClass,parent, args, n);
								n=0;
    XtSetArg(args[n], XtNlabel, label);			        n++;
    if (bitmap) {
       XtSetArg(args[n], XtNleftMargin, 20);		        n++;
       XtSetArg(args[n], XtNleftBitmap, bitmap);		n++;
    }
    XtSetArg(args[n], XtNjustify, XtJustifyCenter);	        n++;
    entry = XtCreateManagedWidget(name, smeBSBObjectClass,menu, args, n);
    ENDMESSAGE(build_label_menu)
    return menu;
}

/*############################################################*/
/* catch_Xerror */
/* Catch X errors die gracefully if one occurs */
/*############################################################*/

int
catch_Xerror(dpy, err)
    Display *dpy;
    XErrorEvent *err;
{
    BEGINMESSAGE(catch_Xerror)
    if (err->error_code == BadImplementation) {
	old_Xerror(dpy, err);
	return 0;
    }
    if (dying) return 0;
    dying = True;
    bomb = *err;
    XtDestroyWidget(toplevel);
    ENDMESSAGE(catch_Xerror)
    return 0;
}
