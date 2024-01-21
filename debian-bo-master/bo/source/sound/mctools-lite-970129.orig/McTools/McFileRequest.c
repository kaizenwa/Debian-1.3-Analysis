/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

  McFilerequest.c

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; see the file COPYING.LIB.  If
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
*/
#include "../config.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <fcntl.h>
#include <locale.h>
#include <signal.h>
#include <unistd.h>
#include <dirent.h>
#include <string.h>
#include "McGlob.h"
#include <sys/stat.h>
#include <ctype.h>
#include <pwd.h>
#include <grp.h>

#include "McApp.h"
#include "McGadget.h"
#include "McFocus.h"
#include "McSlider.h"
#include "McText.h"
#include "McBitmap.h"
#include "McString.h"
#include "McSelector.h"
#include "McResource.h"
#include "McUtils.h"

#include "McFileRequest.h"

static int GetDirectory(McFileRequest *req, unsigned char *pat);
static void mode_string (mode_t mode, unsigned char *str);
static void RemakeInfo(McFileRequest *req);
static void FreeDirList(McFileRequest *req);
static void typein_proc(McGadget *gadget);
static void open_proc(McGadget *gadget);
static void exit_proc(McGadget *gadget);
static void rescan_proc(McGadget *gadget);
static void parent_proc(McGadget *gadget);
static void select_up(McGadget *gadget);
static void select_down(McGadget *gadget);
static void configure_proc(McWindow *mcw);
static void do_rescan(McFileRequest *req);
static int event_proc(McWindow *mcw, XEvent *event);
static void focus_proc(McGadget *gadget);
static int FormatLen(const unsigned char *str, int *width);

typedef struct DirEntry {
  unsigned char *filename;
  unsigned char *info;
  int size;
  mode_t mode;
  nlink_t nlink;
  uid_t uid;
  gid_t gid;
  time_t atime;
  time_t ctime;
  time_t mtime;
  struct DirEntry *next;
} DirEntry;

static DirEntry *FindCurrentEntry(McFileRequest *req);

enum {
  BTN_EXIT,
  BTN_RESCAN,
  BTN_PARENT,
  BTN_OPEN,

  TXT_PATTERN,
  TXT_PATH,
  TXT_FILE,

  STR_MASK,
  STR_PATH,
  STR_FILE,

  SEL_FILES,
  PRP_FILES,

  NUM_GADGETS, /* Change McFileRequest.h!! */
};

#define WIDTH  43
#define HEIGHT 316

McFileRequest *McCreateFileRequest(McApp *app, int flags,
				   unsigned char *title,
				   unsigned char *pattern,
				   unsigned char *path,
				   unsigned char *file,
				   unsigned char *openmsg,
				   unsigned char *format,
				   int (*callback)(unsigned char *path,
						   unsigned char *file,
						   unsigned char *mask)) {
  McFileRequest *req;
  int i,w;

  if (!(req = calloc(sizeof(McFileRequest), 1))) return NULL;

  if (!format) format=McGetResource(app, "dirFormat");
  if (!format) format="%p %n %s";
  if (!strcmp(format, "ll")) format="%p %h %o %g %s %tm %n";

  req->app=app;
  req->dirinfo = NULL;
  req->dirlist = NULL;
  req->listfont = NULL;
  req->oldfile = NULL;
  req->newpath=0;
  req->newfile=0;
  req->flags=flags;
  req->callback=callback;
  req->listfont=app->fixedFont;
  req->format=format;


  {
    int direction, ascent, descent;
    XCharStruct overall;
    w=40;
    req->formatlen=FormatLen(format, &w);
    req->namelen=w;
    XTextExtents(req->listfont, "M", 1,
		 &direction, &ascent, &descent, &overall);
    w=req->formatlen*overall.width;
    if (w<269) w=269;
  }

  /*
   * Create the window
   */
  if (flags&MCF_POPUP)
    req->mcw=McCreateSimpleWindow(app, title,
				  w+WIDTH, HEIGHT, (w+WIDTH)>>1, HEIGHT>>1,
				  (w+WIDTH)>>1, HEIGHT>>1,
				  configure_proc, event_proc);
  else
    req->mcw=McCreateSimpleWindow(app, title,
				  w+WIDTH, HEIGHT, (w+WIDTH)>>1, HEIGHT>>1,
				  0, 0, configure_proc, event_proc);

  req->mcw->focusCallback=focus_proc;

/*240,164,88,12*/
  req->Gad[0] = MakeButton(req->mcw, -12, 292, 60, 16, BTN_EXIT,
		      _M("Cancel"), exit_proc);
  req->Gad[0]->topLeftGravity = SouthEastGravity;
  req->Gad[1] = MakeButton(req->mcw, -88, 292, 60, 16, BTN_RESCAN,
		      _M("Rescan"),rescan_proc);
  req->Gad[1]->topLeftGravity = SouthEastGravity;
  req->Gad[2] = MakeButton(req->mcw, -164, 292, 60, 16, BTN_PARENT,
		      _M("Parent"), parent_proc);
  req->Gad[2]->topLeftGravity = SouthEastGravity;
  if (!openmsg) openmsg=_M("Open");
  req->Gad[3] = MakeButton(req->mcw, -240, 292, 60, 16, BTN_OPEN,
			   openmsg,   open_proc);
  req->Gad[3]->topLeftGravity = SouthEastGravity;
  req->mcw->mainButton=req->Gad[3];

  req->Gad[4] =  MakeRText(req->mcw, 64, 210, TXT_PATTERN, _M("Pattern:"));
  req->Gad[4]->topLeftGravity = SouthWestGravity;
  req->Gad[5] =  MakeRText(req->mcw, 64, 237, TXT_PATH,       _M("Path:"));
  req->Gad[5]->topLeftGravity = SouthWestGravity;
  req->Gad[6] =  MakeRText(req->mcw, 64, 264, TXT_FILE,       _M("File:"));
  req->Gad[6]->topLeftGravity = SouthWestGravity;

  if (!pattern) pattern="*";
  if (!path) path=".";
  if (!file) file="";

  if (strlen(path) && (!strncmp(path, file, strlen(path)))) file+=strlen(path);
  if (*file=='/') file++;

  chdir(path);

  req->Gad[7] = MakeString(req->mcw, 70, 210, w-39, 17, STR_MASK,pattern,
			   NULL, NULL);
  req->Gad[7]->topLeftGravity = SouthWestGravity;
  req->Gad[7]->bottomRightGravity = NorthEastGravity;
  req->Gad[8] = MakeString(req->mcw, 70, 237, w-39, 17, STR_PATH, path,
			   NULL, NULL);
  req->Gad[8]->topLeftGravity = SouthWestGravity;
  req->Gad[8]->bottomRightGravity = NorthEastGravity;
  req->Gad[9] = MakeString(req->mcw, 70, 264, w-39, 17, STR_FILE, file,
		      NULL, NULL);
  ((McString *)req->Gad[9]->specialInfo)->callbackChange=typein_proc;
  req->Gad[9]->topLeftGravity = SouthWestGravity;
  req->Gad[9]->bottomRightGravity = NorthEastGravity;

  req->Gad[10] = MakeSelector(req->mcw, 12, 10, w-2, 191, SEL_FILES,
			      req->listfont, select_up, select_down);
  req->Gad[10]->bottomRightGravity = SouthEastGravity;

  McAddFocusGadget(req->Gad[10], 1);
  McAddFocusGadget(req->Gad[7], 1);
  McAddFocusGadget(req->Gad[8], 1);
  McAddFocusGadget(req->Gad[9], 1);
  McAddFocusGadget(req->Gad[3], 1);
  McAddFocusGadget(req->Gad[2], 1);
  McAddFocusGadget(req->Gad[1], 1);
  McAddFocusGadget(req->Gad[0], 1);

  req->Gad[11] = MakeProp(req->mcw, w+19, 10, 12, 191, PRP_FILES, NULL);
  req->Gad[11]->topLeftGravity = NorthEastGravity;
  req->Gad[11]->bottomRightGravity = SouthWestGravity;

  for (i=0; i<NUM_GADGETS; i++) req->Gad[i]->customData=req;
  req->mcw->customData=req;

  McSelectorBindSlider(req->Gad[10],req->Gad[11]);

  /*
   * Display the window
   */
  McInitGadgets(req->mcw);
  McMapWindow(req->mcw);

  getcwd(((McString *)req->Gad[STR_PATH]->specialInfo)->buf,
	 ((McString *)req->Gad[STR_PATH]->specialInfo)->bufsize);
  McStringSetText(req->Gad[STR_PATH], NULL);
  rescan_proc(req->Gad[STR_PATH]);

  McSetFocus(req->Gad[SEL_FILES]);
  
  return req;
}

void McRemoveFileRequest(McFileRequest **req) {
  if (*req) {
    McFreeWindow((*req)->mcw);
    FreeDirList(*req);
    free(*req);
    *req=NULL;
  }
}

/**************************************************************************/

static void focus_proc(McGadget *gadget) {
  McFileRequest *req=(McFileRequest *)gadget->customData;

  if ((gadget==req->Gad[STR_PATH]) || (gadget==req->Gad[STR_MASK]))
    McSetMainButton(req->Gad[BTN_RESCAN]);
  else
    McSetMainButton(req->Gad[BTN_OPEN]);
}

/**************************************************************************/

static int do_open(McFileRequest *req) {
  unsigned char *path, *file, *mask, *result;
  DirEntry *next;

  next=FindCurrentEntry(req);
  if (next && (S_ISDIR(next->mode))) {
    rescan_proc(req->Gad[BTN_OPEN]);
    return 0;
  }

  file=((McString *)req->Gad[STR_FILE]->specialInfo)->buf;

  if (!strlen(file)) goto beep;

  if ((req->flags&MCF_MUST_EXIST) || (req->callback)) {
    path=((McString *)req->Gad[STR_PATH]->specialInfo)->buf;
    mask=((McString *)req->Gad[STR_MASK]->specialInfo)->buf;

    if (path[strlen(path)-1]=='/')
      path[strlen(path)-1]=0;
    result = (unsigned char *)alloca(strlen(path) + 2 + strlen(file));
    strcpy(result, path);
    strcat(result, "/");
    strcat(result, file);

    if (*file=='/') {
      result=file;
      path=NULL;
    }

    if (req->flags&MCF_MUST_EXIST) {
      struct stat st;
      if (stat(result, &st)) {
beep: 
	XBell(req->app->display, 0);
	return 0;
      }
    }

    if ((req->callback) && ((*(req->callback))(path, result, mask))) return 1;
  }

  return 0;
}

static DirEntry *FindCurrentEntry(McFileRequest *req) {
  McSelector *selector=(McSelector *)req->Gad[SEL_FILES]->specialInfo;
  register DirEntry *next;
  register int i;
  unsigned char *file;

  i=0;
  file=((McString *)req->Gad[STR_FILE]->specialInfo)->buf;
  next = req->dirlist;
  while (next) {
    if (!strcmp(next->filename, file)) break;
    next=next->next;
    i++;
  }

  if (next) {
    if (selector->selection!=i)
      McSelectorSelect(req->Gad[SEL_FILES], i);
  } else {
    if (selector->selection!=-1)
      McSelectorSelect(req->Gad[SEL_FILES], -1);
  }

  return next;
}

static void typein_proc(McGadget *gadget) {
  McFileRequest *req=(McFileRequest *)gadget->customData;
  FindCurrentEntry(req);
}

static void open_proc(McGadget *gadget) {
  McFileRequest *req=(McFileRequest *)gadget->customData;
  if (do_open(req)) McRemoveFileRequest(&req);
}

static void select_down(McGadget *gadget) {
  McFileRequest *req=(McFileRequest *)gadget->customData;
  McSelector *selector = (McSelector *)gadget->specialInfo;
  register int i = selector->selection;
  register DirEntry *next;
  unsigned char buf[1024];
  DIR *dir;

  if (!req->oldfile)
    req->oldfile = strdup(((McString *)req->Gad[STR_FILE]->specialInfo)->buf);

  next = req->dirlist;
  while(next && (--i>=0)) next=next->next;

  if (next) {
    if (S_ISDIR(next->mode)) {
      if (((i=strlen(next->filename))<255) && (getcwd(buf, 1022-i))) {
	if(buf[(i=strlen(buf))-1]!='/') buf[i++]='/';
	strcpy(buf+i,next->filename);
	req->newfile = 0;
	McStringSetText(req->Gad[STR_FILE], req->oldfile);
	if ((dir=opendir(buf))) {
	  McStringSetText(req->Gad[STR_PATH], buf);
	  req->newpath = (strcmp(next->filename,"..")?1:2);
	  closedir(dir);
	} else {
	  getcwd(((McString *)req->Gad[STR_PATH]->specialInfo)->buf,
		 ((McString *)req->Gad[STR_PATH]->specialInfo)->bufsize);
	  McStringSetText(req->Gad[STR_PATH], NULL);
	  req->newpath = 0;
	}
      } else {
	req->newfile = 0;
	req->newpath = 0;
      }
    } else {
      getcwd(((McString *)req->Gad[STR_PATH]->specialInfo)->buf,
	     ((McString *)req->Gad[STR_PATH]->specialInfo)->bufsize);
      McStringSetText(req->Gad[STR_PATH], NULL);
      McStringSetText(req->Gad[STR_FILE], next->filename);
      req->newfile = 1;
      req->newpath = 0;
    }
  } else {
    XBell(req->app->display,0);
  }
  if (selector->flags & SEL_DOUBLE_CLICK) {
    if (req->newpath) {
      if (req->mcw->keyboardFocus!=req->Gad[SEL_FILES])
	McSetFocus(req->Gad[STR_PATH]);
      McGadgetUpdate(req->Gad[STR_PATH]);
      rescan_proc(req->Gad[STR_PATH]);
    }
    select_up(gadget);
    if (req->newfile) {
      if (req->mcw->keyboardFocus!=req->Gad[SEL_FILES])
	McSetFocus(req->Gad[STR_FILE]);
      McGadgetUpdate(req->Gad[STR_FILE]);
      req->Gad[BTN_OPEN]->flags|=GAD_SELECTED;
      McGadgetBusy(req->Gad[BTN_OPEN]);
      XFlush(req->app->display);
      if (do_open(req)) {
	McRemoveFileRequest(&req);
	return;
      }
      req->Gad[BTN_OPEN]->flags&=~GAD_SELECTED;
      McGadgetRedraw(req->Gad[BTN_OPEN]);
    }
    if (!req->newpath && !req->newfile) {
      XBell(req->app->display, 0);
    }
  }
}

static void select_up(McGadget *gadget) {
  McFileRequest *req=(McFileRequest *)gadget->customData;
  free(req->oldfile); req->oldfile=NULL;
}

/**************************************************************************/

static void parent_proc(McGadget *gadget) {
  McFileRequest *req=(McFileRequest *)gadget->customData;

  McString *string = (McString *)req->Gad[STR_PATH]->specialInfo;
  unsigned char *newdir = (unsigned char *)calloc(string->bufsize+4, 1);
  int i;
  DIR *dir = NULL;

  strcpy(newdir,string->buf);
  if (newdir[(i=strlen(newdir))-1]!='/')
    newdir[i++]='/';
  strcpy(newdir+i,"..");
  dir=NULL;
  if ((dir=opendir(newdir)) && (!chdir(newdir))) { 
    getcwd(((McString *)req->Gad[STR_PATH]->specialInfo)->buf,
	   ((McString *)req->Gad[STR_PATH]->specialInfo)->bufsize);
    McStringSetText(req->Gad[STR_PATH], NULL);
    GetDirectory(req, ((McString *)req->Gad[STR_MASK]->specialInfo)->buf);
  } else {
    XBell(req->app->display, 0);
  }
  if (dir) closedir(dir);
  free(newdir);
}

static void rescan_proc(McGadget *gadget) {
  McFileRequest *req=(McFileRequest *)gadget->customData;

  if (gadget==req->Gad[BTN_RESCAN])
    gadget=NULL;
  else
    gadget=req->Gad[BTN_RESCAN];

  if (gadget) {
    gadget->flags|=GAD_SELECTED;
    McGadgetBusy(gadget);
    XFlush(req->app->display);
  }

  do_rescan(req);

  if (gadget) {
    gadget->flags&=~GAD_SELECTED;
    McGadgetRedraw(gadget);
  }
}

static void do_rescan(McFileRequest *req) {
  if (!chdir(((McString *)req->Gad[STR_PATH]->specialInfo)->buf)) { 
    getcwd(((McString *)req->Gad[STR_PATH]->specialInfo)->buf,
	   ((McString *)req->Gad[STR_PATH]->specialInfo)->bufsize);
    McStringSetText(req->Gad[STR_PATH], NULL);
    GetDirectory(req, ((McString *)req->Gad[STR_MASK]->specialInfo)->buf);
  } else {
    XBell(req->app->display, 0);
    McSelectorSetList(req->Gad[SEL_FILES], NULL, 0, 0);
    McGadgetUpdate(req->Gad[PRP_FILES]);
  }
}

static void configure_proc(McWindow *mcw) {
  RemakeInfo((McFileRequest *)mcw->customData);
}

/**************************************************************************/

static void FreeDirList(McFileRequest *req) {
  DirEntry *now=req->dirlist, *last;
  while(now) {
    free(now->filename);
    free(now->info);
    last=now->next;
    free(now);
    now=last;
  }
  req->dirlist=NULL;
}


static void MakeInfo(McFileRequest *req, DirEntry *now) {
  int i, fieldlen, namelen;
  unsigned char *str, *pd;
  int width = McSelectorWidth(req->Gad[SEL_FILES])-1;
  char buf[10];
  struct group *grp;
  struct passwd *pwd;
  time_t rightnow, when;

  time(&rightnow);

  if (now->info) free(now->info);

  pd = now->info = (unsigned char *)calloc(req->formatlen+40, 1);
  str=req->format;

  namelen=width-(req->formatlen-req->namelen);
  if (namelen<12) namelen=req->namelen;
  if (namelen<1) namelen=12; /* Still enuff for 8.3 (: */

  while(*str) {
    if (*str=='%' && str[1]) {
      str++;
      fieldlen=0;
      while (isdigit(*str)) {
	fieldlen=fieldlen*10+*str-'0';
	str++;
      }
      switch(*str) {
      case 'p':
	if (!fieldlen) fieldlen=10;
	if (now->size>=0)
	  mode_string(now->mode, pd);
	else
	  strcpy(pd, "          ");
	pd+=fieldlen;
	break;
      case 'n':
	if (!fieldlen) fieldlen=namelen;
	strncpy(pd, now->filename, fieldlen);
	i=strlen(now->filename);
	if (i<fieldlen) memset(pd+i, ' ', fieldlen-i);
	pd+=fieldlen;
	namelen=20;
	break;
      case 's':
	if (!fieldlen) fieldlen=8;
	if (now->size>=0) {
	  if (S_ISDIR(now->mode)) {
	    if (strcmp(now->filename,"..")) {
	      memset(pd, ' ', fieldlen);
	      memcpy(pd, "   <DIR>", 8);
	    } else {
	      memcpy(pd, "<UP-DIR>", 8);
	    }
	  } else {
	    sprintf(buf, "%%%dd", fieldlen);
	    sprintf(pd, buf, now->size);
	  }
	} else {
	  memset(pd, ' ', fieldlen);
	  memcpy(pd, "<\?\?\?>", 5);
	}
	pd+=fieldlen;
	break;
      case 'o':
	if (!fieldlen) fieldlen=8;
	pwd=getpwuid(now->uid);
	if (pwd) {
	  sprintf(buf, "%%-%ds", fieldlen);
	  sprintf(pd, buf, pwd->pw_name);
	} else {
	  sprintf(buf, "%%-%dd", fieldlen);
	  sprintf(pd, buf, now->uid);
	}
	pd+=fieldlen;
	break;
      case 'g':
	if (!fieldlen) fieldlen=8;
	grp=getgrgid(now->gid);
	if (grp) {
	  sprintf(buf, "%%-%ds", fieldlen);
	  sprintf(pd, buf, grp->gr_name);
	} else {
	  sprintf(buf, "%%-%dd", fieldlen);
	  sprintf(pd, buf, now->gid);
	}
	pd+=fieldlen;
	break;
      case 'h':
	if (!fieldlen) fieldlen=3;
	sprintf(buf, "%%%dd", fieldlen);
	sprintf(pd, buf, now->nlink);
	pd+=fieldlen;
	break;
      case 't':
	str++;
	switch(*str) {
	case 'a':
	  when=now->atime;
	  break;
	case 'c':
	  when=now->ctime;
	  break;
	case 'm':
	  when=now->mtime;
	  break;
	}

	if (!fieldlen) fieldlen=12;
	strcpy (pd, ctime (&when));
	if (rightnow > when + 6L * 30L * 24L * 60L * 60L
	    || rightnow < when - 60L * 60L) {
	  strcpy (pd + 11, pd + 19);
	}
	memmove(pd, pd+4, strlen(pd-4));
	pd+=fieldlen;
	break;
      }
    } else {
      *pd++=*str;
    }
    if (*str) str++;
  }
  *pd=0;
}

/****************************************************************************/

  /* Format options are shown with their default length
   * %10p	Permissions
   * %<X>n	Filename, <X> means as long as there's room left in the window
   * %8s	Size, <DIR> for dirs, <UP-DIR> for Parent
   * %9o	Owner
   * %9g	Group
   * %2h	Number of hardlinks
   * %12tm	mtime
   * %12tc	ctime
   * %12ta	atime
   */
static int FormatLen(const unsigned char *str, int *width) {
  int formatlen=0, addwidth=0, namelen=0;

  while(*str) {
    if (*str=='%' && str[1]) {
      int fieldlen=0;
      str++;
      while (isdigit(*str)) {
	fieldlen=fieldlen*10+*str-'0';
	str++;
      }
      switch(*str) {
      case 'p':
	if (!fieldlen) fieldlen=10;
	break;
      case 'n':
	if (!fieldlen) {
	  if (!addwidth) {
	    addwidth=1;
	    namelen=20;
	  } else {
	    fieldlen=20;
	  }
	} else {
	  namelen=fieldlen;
	}
	break;
      case 's':
	if (!fieldlen) fieldlen=8;
	break;
      case 'o':
	if (!fieldlen) fieldlen=8;
	break;
      case 'g':
	if (!fieldlen) fieldlen=8;
	break;
      case 'h':
	if (!fieldlen) fieldlen=3;
	break;
      case 't':
	str++;
	switch(*str) {
	case 'a':
	case 'c':
	case 'm':
	  if (!fieldlen) fieldlen=12;
	  break;
	default:
	  fprintf(stderr,
		  _M("%s: warning: unknown directory format specifier.\n"),
		  myname);
	  break;
	}
	break;
      default:
	fprintf(stderr,
		_M("%s: warning: unknown directory format specifier.\n"),
		myname);
	fieldlen=0;
	break;
      }
      formatlen+=fieldlen;
    } else {
      formatlen++;
    }
    if (*str) str++;
  }

  if (addwidth) {
    if (formatlen<*width) {
      namelen=*width-formatlen;
      formatlen=*width;
    } else {
      formatlen+=namelen;
    }
  }

  *width=namelen;
  return formatlen;
}

/****************************************************************************/

static void UpdateSelectorBox(McFileRequest *req, int num) {
  McSelector *selector = (McSelector *)(req->Gad[SEL_FILES]->specialInfo);
  selector->first = 0;

  McSelectorSetList(req->Gad[SEL_FILES], req->dirinfo, num, 0);

  num-= McSelectorHeight(req->Gad[SEL_FILES])+1;
  if (num<1) num=1;
  McGadgetUpdate(req->Gad[PRP_FILES]);
}

static void RemakeInfo(McFileRequest *req) {
  DirEntry *now;
  int num=0;
  now=req->dirlist;
  while(now) {
    MakeInfo(req, now);
    req->dirinfo[num]=now->info;
    now=now->next;
    num++;
  }
  UpdateSelectorBox(req, num);
}

static int sort_func(const void *a, const void *b);

static int GetDirectory(McFileRequest *req, unsigned char *pat) {
  unsigned char *file,buf[2];
  DirEntry *first = NULL, *last=NULL, *now=NULL;
  int num;
  struct stat st;
  int res;
  int i,dirs,str,files;

  glob_t glb = {0};
  /*
   * Glob two times, once with '*' as pattern to get all directories,
   * then with the actual pattern to find the files.
   * (Strange thing, if I use GLOB_MARK, I get a / behind EVERY path
   * rather than only behind every dir...
   */
  res=glob("*", 0, NULL, &glb);
#ifdef GLOB_NOMATCH
  if (res==0 || res==GLOB_NOMATCH) {
#else
  if (res==0) {
#endif
    res=0;
    for(dirs=0;glb.gl_pathv[dirs];dirs++);

    /*
     * Multi pattern globbing patch from Sander van Malssen(svm@kozmix.ow.nl)
     *
     * Letting glob() do the sorting here is useless, since when using
     * multiple patterns, only the files matching the same pattern are
     * sorted against each other. This is as bad as no sorting, so it
     * is done manually afterwards.                                   Olav
     */
    {
      char *p, *tmp = strdup(pat);
      p = strtok (tmp, " ");
      glob(p, GLOB_APPEND|GLOB_NOSORT, NULL, &glb);
      while ((p = strtok (NULL, " ")))
	glob(p, GLOB_APPEND|GLOB_NOSORT, NULL, &glb);
      free(tmp);
    }

    for (files=dirs;glb.gl_pathv[files];files++);
    files-=dirs;
    qsort(&glb.gl_pathv[dirs], files, sizeof(char *), sort_func);

    num=0;
    file="..";
    if (getcwd(buf,2) && (!strcmp(buf,"/"))) i=0; else i=-1;

    for (;(i<0) || ((file=glb.gl_pathv[i])); i++) {
      if (((i<0) || (strcmp(file,".") && strcmp(file,"..")))) {
	str=stat(file, &st);
	if (((i<dirs) && ((!str) && ((st.st_mode & S_IFMT) == S_IFDIR))) ||
	    ((i>=dirs) && ((str) || ((st.st_mode & S_IFMT) != S_IFDIR)))) {
	  now = (DirEntry *)calloc(sizeof(DirEntry), 1);
	  if (last) last->next=now;
	  if (!first) first=now;
	  last=now;
	  now->filename = strdup(file);
	  now->next=NULL;
	  if (str) {
	    now->size=-1;
	    now->mode=-1;
	    now->uid=-1;
	    now->gid=-1;
	    now->mtime=-1;
	  } else {
	    now->size=st.st_size;
	    now->mode=st.st_mode;
	    now->uid=st.st_uid;
	    now->gid=st.st_gid;
	    now->atime=st.st_atime;
	    now->ctime=st.st_ctime;
	    now->mtime=st.st_mtime;
	    now->nlink=st.st_nlink;
	  }
	  now->info = NULL;
	  MakeInfo(req, now);
	  num++;
	}
      }
    }
    FreeDirList(req);
    req->dirlist=first;
    if (req->dirinfo) free(req->dirinfo);
    req->dirinfo=(const unsigned char **)calloc(num, sizeof(unsigned char *));
    now=req->dirlist;
    num=0;
    while(now) {
      req->dirinfo[num]=now->info;
      now=now->next;
      num++;
    }
    UpdateSelectorBox(req, num);
    globfree(&glb);
  }
  return res;
}

static int sort_func(const void *a, const void *b) {
  return strcmp(((unsigned char **) a)[0], ((unsigned char **) b)[0]);
}

/**************************************************************************
 * static void exit_proc(McGadget *gadget)
 *
 */
static void exit_proc(McGadget *gadget) {
  McFileRequest *req=(McFileRequest *)gadget->customData;
  if ((!(req->callback)) || ((*(req->callback))(NULL, NULL, NULL)))
    McRemoveFileRequest(&req);
}

/****************************************************************************/
/* This is 'stolen' from the GNU fileutils. Since this software is also GNU */
/* its ok. Yeah, that's what free software is for...                        */
/****************************************************************************/

static unsigned char ftypelet (mode_t bits) {
  if ((bits & S_IFMT) == S_IFBLK)
    return 'b';
  if ((bits & S_IFMT) == S_IFCHR)
    return 'c';
  if ((bits & S_IFMT) == S_IFDIR)
    return 'd';
  if ((bits & S_IFMT) == S_IFREG)
    return '-';
  if ((bits & S_IFMT) == S_IFIFO)
    return 'f';
  if ((bits & S_IFMT) == S_IFLNK)
    return 'l';
  if ((bits & S_IFMT) == S_IFSOCK)
    return 's';
  if ((bits & S_IFMT) == S_IFLNK)
    return 'l';
  return '?';
}

static void rwx (mode_t bits, unsigned char *chars) {
  chars[0] = (bits & S_IREAD) ? 'r' : '-';
  chars[1] = (bits & S_IWRITE) ? 'w' : '-';
  chars[2] = (bits & S_IEXEC) ? 'x' : '-';
}

static void setst (mode_t bits, unsigned char *chars) {
#ifdef S_ISUID
  if (bits & S_ISUID)
    {
      if (chars[3] != 'x')
        /* Set-uid, but not executable by owner.  */
        chars[3] = 'S';
      else
        chars[3] = 's';
    }
#endif
#ifdef S_ISGID
  if (bits & S_ISGID)
    {
      if (chars[6] != 'x')
        /* Set-gid, but not executable by group.  */
        chars[6] = 'S';
      else
        chars[6] = 's';
    }
#endif
#ifdef S_ISVTX
  if (bits & S_ISVTX)
    {
      if (chars[9] != 'x')
        /* Sticky, but not executable by others.  */
        chars[9] = 'T';
      else
        chars[9] = 't';
    }
#endif
}

static void mode_string (mode_t mode, unsigned char *str) {
  str[0] = ftypelet (mode);
  rwx ((mode & 0700) << 0, &str[1]);
  rwx ((mode & 0070) << 3, &str[4]);
  rwx ((mode & 0007) << 6, &str[7]);
  setst (mode, str);
}

/**************************************************************************
 * static int event_proc(McWindow *mcw, XEvent *event)
 *
 */
static int event_proc(McWindow *mcw, XEvent *event) {
  McFileRequest *req=(McFileRequest *)mcw->customData;

  if(event->type==ClientMessage) {
    if ((event->xclient.format == 32) &&
	(event->xclient.data.l[0] == req->app->wmDelWin)) {
      if (req->callback) (*(req->callback))(NULL, NULL, NULL);
      McRemoveFileRequest(&req);
      return 1;
    }
  }
  return 0;
}
