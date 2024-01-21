#undef DBG_STATUS
#undef DBG_LIST

/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

 This file contains code taken from the
 WorkBone CD-Player, Copyright (C) 1993 Thomas McWilliams

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Abandon hope all ye who enter here */

#include "../config.h"

#include "X11/Xlib.h"
#include <stdio.h>
#include <stdlib.h>
#include "struct.h"
#include <McTools/McApp.h>
#include <McTools/McResource.h>
#include <McTools/McBitmap.h>
#include <McTools/McGadget.h>
#include <McTools/McOrder.h>
#include <McTools/McDigits.h>
#include <McTools/McTip.h>
#include "icons.h"
#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include <strings.h>
#include "hardware.h"
#include "xplaycd.h"
#include "cd.h"
#include "discid.h"

int cue = 0, speed = 0;
struct cdinfo thiscd, *cd = &thiscd;
int cur_track = 1;		/* Current track number, starting at 1 */
int save_track = 1;
int cd_stopped_at_end = 0;
int repeat = 0;
int clk_mode = CLK_TRACK;

unsigned char *currentFile = NULL;

int error = 0, enable_eject=1;
int cur_track, cur_pos_abs, cur_pos_rel, cur_tracklen, cur_cdlen, cur_cdmode,
  cur_ntracks, cur_firsttrack, cur_lasttrack, cur_stoptrack, cur_length,
  cur_index, cur_frame;

static int last_track_displayed = -1;

static void no_cd(void);
static void shuffle_cd(void);
static int find_end(void);
static void play_list(int from);
static int find_first(void);
static int find_next(void);
static int find_prev(void);
static void replay_list(void);
static McOrderItem *ValidateCurrent(void);
#ifdef SHOW_CD_MODE
static void show_cd_mode(void);
#endif

static McOrderItem *currentItem = NULL;

static INLINE void SetCurrent(McOrderItem *it) {
  currentItem = it;
  cur_track = it->id;
}

static void alarm_handler(int dummy) {
  error=0;
}

void cd_init(void) {
  struct sigaction alarm_sigaction= { alarm_handler, 0, 0, 0 };

  sigaction(SIGALRM,&alarm_sigaction,0);
  srand(time(NULL));
  thiscd.trk = NULL;
}

static int set_error(int e) {
  if (!e) return 0;
  error = e;
  cd_stopped_at_end=0;
  alarm(2);
  return e;
}

/* NeedToPoll becomes TRUE if we have to
 * poll even when the window is iconified
 */
static INLINE void UpdateNeedToPoll(void) {
  NeedToPoll = ((cur_cdmode == CDPLAY) &&
		(repeat || (cur_lasttrack!=cur_stoptrack)));
}

/*	0	No CD in drive.
 *	1	CD in drive.
 *	2	CD has just been inserted (TOC has been read)
 *	3	CD has just been removed
 */

int cd_action(int request, int down) {
  int cds, tmppos, track;

  /********    AUTOMATICS    ************/

  cds=cd_status();
#ifdef DBG_STATUS
  fprintf(stderr, "Status: %d\n",cds);
#endif

  if ((cds==4) || (cds==7)) return cds;
  if (cds==0) no_cd();
  if (cds==2) new_cd(1);

  if (cur_cdmode==CDPLAY) {
    save_track = cur_track;
    cd_stopped_at_end = 1;
  }

#if 0
  printf("cur_track=%d  cur_stoptrack=%d  cur_lasttrack=%d\n",
	 cur_track, cur_stoptrack, cur_lasttrack);
#endif

  if (cue>=0) {
    if (cur_cdmode==CDNULL ||
      ((cur_cdmode==CDPLAY)&&(cur_track==cur_lasttrack+1)&&(cur_track--,1)) ||
      ((cur_cdmode==CDSTOP)&&(cd_stopped_at_end))) {

#ifdef SHOW_CD_MODE
      show_cd_mode();
#endif
      cd_stopped_at_end=0;
      if (cur_lasttrack!=cur_stoptrack) {
	cur_track=cur_lasttrack;
	if ((track=find_next())) {
	  cur_lasttrack=cur_stoptrack;
	  cur_track=track;
	  play_list(cur_pos_rel);
	  save_track=cur_track;
	}
      } else {
	if (repeat) {
	  find_first();
	  play_list(0);
	}
      }
    }
  }
  
  UpdateNeedToPoll();

  /* If we are not visible, no need to process further */
  if (!mainWindow->window_visible) return cd_status();

  if (request==BTN_EJECT) {
    if (checkmount()) return cd_status();
    if (enable_eject) {
      set_error(eject_cd());
    } else {
      if (!set_error(load_cd())) {
	if ((cds=cd_status())==2) new_cd(1);
      }
    }
    poll_out=5;
    return cd_status();
  }

  if ((cds==2) || (cds==1)) {

    if (request==REQ_POLL) {
#ifdef DBG_STATUS
      fprintf(stderr, "poll... cue:%d  mode:%d\n",cue, cur_cdmode);
#endif

      /********    FAST SEARCH    ************/

      if ((((cue>0) && (Gad[BTN_FF]->flags&GAD_SELECTED)) ||
	   ((cue<0) && (Gad[BTN_REWIND]->flags&GAD_SELECTED))) &&
	  (cur_cdmode==CDPLAY)) {
#ifdef DBG_STATUS
	fprintf(stderr, "pos: %d\n",cur_pos_rel+cue);
#endif

	if ((cue<0) && (cur_track==cur_lasttrack+1) && (cur_pos_rel<=0)) {
	  /* Skip a pause */
	  cur_track--;
	  play_list(thiscd.trk[cur_track-1].length-2+cur_pos_rel);
#if 0
	  printf("skip: cur_track=%d  s=%d\n",
		 cur_track, cur_pos_rel-1);
#endif
	} else {
	  switch(speed++) {
	  case 10:
	    cue=cue<<1;
	    break;
	  case 25:
	    cue=cue<<1;
	    break;
	  case 40:
	    cue=cue<<1;
	    break;
	  }

	  tmppos = cur_pos_rel + cue;
	  if (tmppos >= thiscd.trk[cur_track - 1].length) {
	    if ((track=find_next())) {
	      play_list(0);
	    } else {
	      cue=0; speed=0;
	    }
	  } else {
	    if (tmppos<0) {
	      if ((track=find_prev())) {
		int q;
		if (cur_pos_rel<0)
		  q=cur_pos_rel+tmppos;
		else
		  q=tmppos;
		play_list(thiscd.trk[track-1].length+q);
#if 0
		printf("track=%d  cur_track=%d  s=%d  q=%d\n",
		       track, cur_track, thiscd.trk[track-1].length+q, q);
#endif
	      } else {
		cue=0; speed=0;
	      }
	    } else {
	      if (cur_lasttrack) {
#ifdef DBG_LIST
		printf("play_cd(%d,%d,%d)       n=%d  last=%d  stop=%d\n",
		       cur_track,tmppos, cur_lasttrack,
		       cur_ntracks, cur_lasttrack, cur_stoptrack);
#endif
		play_cd(cur_track,tmppos,cur_lasttrack);
	      } else {
		play_list(tmppos);
	      }
	    }
	  }
	}
      } else {
	cue=0;
      }
    } else {

#ifdef KERNEL_HANG
      /* FIXME: The kernel hangs completely if commands
       *        are sent to the cdrom very fast. This is at least
       *        true with 1.1.9 and 1.1.50 used with a PAS16 &
       *        CDR-H93MV cdrom drive.
       *        I work around by accepting commands only every
       *        0.4s
       */
      if (udiff(&old, &old)<300000) {
	XBell(app->display, 0);
	return cds;
      }
#endif

      /********    BUTTON PRESSES    ************/

      if (request<NUM_GADGETS) {

	if (request!=BTN_REWIND && request!=BTN_FF) {
	  cue=0; speed=0;
	}

	switch(request) {
	  
	case BTN_SHUFFLE:
	  shuffle_cd();
	  down=-1;
	  /* FALLTHROUGH */
	  
	case ORD_SONGS:
	  if ((down>0) && (down<=cur_ntracks)) {
	    cur_track=down;
	    /* FIXME: Der schnappt sich immer das erste icon... */
	    play_list(0);
	    break;
	  }
	  if (cur_cdmode == CDPLAY)
	    replay_list();
	  else
	    find_first();
	    find_end();
	  break;
	  
	case BTN_REWIND:
	  cue=0; speed=0;
	  
	  if (down && (cur_cdmode == CDPAUSE)) {
	    pause_cd();
	    cur_cdmode = CDPLAY;
	  }
	  
	  if (cur_cdmode == CDPLAY) {
	    if (down) {
	      cue = -2; speed=0;
	    } else {
	      cue = 0;
	    }
	  }
	  break;
	  
	case BTN_PLAYSTOP:
	  if (cur_cdmode == CDSTOP || cur_cdmode == CDNULL) {
	    start_cd();
	    break;
	  }
	  
	  if (cur_cdmode == CDPLAY || cur_cdmode == CDPAUSE) {
	    save_track = cur_track;
	    if (save_track<1) save_track=1;
	    stop_cd ();
	    find_end();
	    cd_stopped_at_end = 0;
	    cue=0; speed=0;
	    break;
	  }
	  
	case BTN_PAUSE:
	  if (cur_cdmode == CDPLAY || cur_cdmode == CDPAUSE) {
	    pause_cd();
	  }
	  break;
	  
	case BTN_FF:
	  cue=0; speed=0;
	  
	  if (down && (cur_cdmode == CDPAUSE)) {
	    pause_cd();
	    cur_cdmode = CDPLAY;
	  }
	  
	  if (cur_cdmode == CDPLAY) {
	    if (down) {
	      cue = 2; speed=0;
	    } else {
	      cue = 0;
	    }
	  }
	  break;
	  
	case BTN_REPEAT:
	  repeat = down;
	  UpdateNeedToPoll();
	  break;
	  
	  
	case BTN_PREV:
	  if (!down) break;
	  if (cur_cdmode == CDSTOP) {
	    cur_track=save_track;
	    track=find_prev();
	    if (track) save_track=track;
	    cd_stopped_at_end = 0;
	    break;
	  }
	  
	  if (cur_cdmode == CDPLAY || cur_cdmode == CDPAUSE) {
	    track=0;
	    if (cur_pos_rel<=2)
	      track=find_prev();
	    if (!track)
	      track=cur_track;
	    cur_track = track;
	    play_list(0);
	    cd_stopped_at_end = 0;
	    break;
	  }
	  
	  break;
	  
	case BTN_NEXT:
	  if (!down) break;
	  if (cur_cdmode == CDSTOP) {
	    cur_track=save_track;
	    if ((track=find_next())) save_track=track;
	    cd_stopped_at_end = 0;
	    break;
	  }
	  if (cur_cdmode == CDPLAY || cur_cdmode == CDPAUSE) {
	    if ((track=find_next())) {
	      cur_track=track;
	      play_list(0);
	      cd_stopped_at_end = 0;
	    }
	    break;
	  }
	  break;
	  
	case MSG_TIME:
	  clk_mode++;
	  if (clk_mode>=NUM_CLKMODES)
	    clk_mode=0;
	  break;
	  
	}
      } else {
	XBell(app->display, 0); /* too often... */
      }
    }
  }
  if (cur_cdmode==CDEJECT) enable_eject=0; else enable_eject=1;

  return cd_status();
}

/********    DISPLAY UPDATE    ************/

void cd_update(int cds) {
  int min, sec;
  char buf[256];
  int indx;

  if (cds>=0) {
    /* If we are not visible, no need to process further */
    if (!mainWindow->window_visible) return;
  } else {
    /* First call, update everything */
    cds=cd_status();
  }

  if (cds==0) no_cd();
  if (cds==2) new_cd(1);

#ifdef DBG_STATUS
  fprintf(stderr, "Status: %d\n",cds);
#endif

  /* Statusline */
  if (error)
    switch(error) {
    case -4:
      strcpy(buf, _("Disabled"));
      break;
    case -3:
    case -2:
      strcpy(buf, _("Data"));
      break;
    default:
      sprintf(buf,"E%d", error);
    }
  else 
    switch (cur_cdmode) {
    case CDNULL:
      strcpy(buf, _("Stopped"));
      break;
    case CDPLAY:
      strcpy(buf, _("Playing"));
      break;
    case CDPAUSE:
      strcpy(buf, _("Pause"));
      break;
    case CDSTOP:
      strcpy(buf, _("Stopped"));
      break;
    case CDEJECT:
      last_track_displayed=-1;
      strcpy(buf, _("No disk"));
      break;
    case CDMOUNT:
      strcpy(buf, _("Mounted"));
      break;
    case CDERROR:
      strcpy(buf, _("Error"));
      break;
    default:
      sprintf(buf, _("Mode %d"), cur_cdmode);
    }
  if (strcmp(Gad[MSG_STATUS]->normalLabel->text,buf)) {
    McChangeText(mainWindow, Gad[MSG_STATUS]->normalLabel, buf, 0);
    Gad[MSG_STATUS]->normalLabel->x =
      ((Gad[MSG_STATUS]->width - Gad[MSG_STATUS]->normalLabel->width)>>1) - 1;
    McGadgetRedraw(Gad[MSG_STATUS]);
  }

  /* Pause Button */
  if (cur_cdmode==CDPAUSE) {
    if (!(Gad[BTN_PAUSE]->flags & GAD_SELECTED)) {
      Gad[BTN_PAUSE]->flags |= GAD_SELECTED;
      McGadgetRedraw(Gad[BTN_PAUSE]);
    }
  } else {
    if (Gad[BTN_PAUSE]->flags & GAD_SELECTED) {
      Gad[BTN_PAUSE]->flags &= ~GAD_SELECTED;
      McGadgetRedraw(Gad[BTN_PAUSE]);
    }
  }

  /* Songname */
  if (((cur_cdmode==CDPLAY) && (last_track_displayed != cur_track)) ||
      ((cur_cdmode==CDSTOP) && (last_track_displayed != save_track)) ||
      (cur_cdmode==CDEJECT)) {
    if (cur_cdmode!=CDEJECT) {
      last_track_displayed=(cur_cdmode==CDPLAY)?cur_track:save_track;
      if ((last_track_displayed>=1) && (last_track_displayed<=cur_ntracks)) {
	strcpy(buf,thiscd.trk[last_track_displayed-1].songname);
      } else {
	*buf=0;
      }
    } else {
      *buf=0;
    }
    McChangeText(mainWindow, Gad[MSG_TITLE]->normalLabel, buf, 0);
    Gad[MSG_TITLE]->normalLabel->x =
      ((Gad[MSG_TITLE]->width - Gad[MSG_TITLE]->normalLabel->width)>>1) + 2;
    if (Gad[MSG_TITLE]->tip) free((string_t)(Gad[MSG_TITLE]->tip));
    if (Gad[MSG_TITLE]->normalLabel->x<0) {
      Gad[MSG_TITLE]->normalLabel->x=0;
      Gad[MSG_TITLE]->tip=strdup(buf);
    } else {
      Gad[MSG_TITLE]->tip=NULL;
    }
    McGadgetRedraw(Gad[MSG_TITLE]);
  }


  /* update clocktip */
  {
    static char clocktip[64] = "?";
    Gad[MSG_TIME]->tip=clocktip;
    if (McQueryTipGadget(app)==Gad[MSG_TIME]) {
      char *ptr, newtip[64];
      int pos;
      ptr=newtip;
  
      pos = (cur_pos_rel>0)?cur_pos_rel:-cur_pos_rel;
      sec = pos % 60;
      min = pos / 60;
      sprintf(ptr, _("Track: %s%02d:%02d"),
	      (cur_pos_rel<0)?"-":" ", min, sec);
      ptr+=strlen(ptr);

      pos = (cur_pos_rel>0)?
	(thiscd.trk[cur_track-1].length-cur_pos_rel):-cur_pos_rel;
      sec = pos % 60;
      min = pos / 60;
      sprintf(ptr, "/%s%02d:%02d%s",
	      (cur_pos_rel<0)?"-":"", min, sec, (cur_pos_rel<0)?"":" ");
      ptr+=strlen(ptr);

      pos = ((cur_pos_rel>0)?cur_pos_rel:0) + thiscd.trk[cur_track-1].elapsed;
      sec = pos % 60;
      min = pos / 60;
      sprintf(ptr, _("  Disc: %02d:%02d"), min, sec);
      ptr+=strlen(ptr);

      if (cur_length) {
	sec = cur_length - 
	  (((cur_pos_rel>0)?cur_pos_rel:0) + thiscd.trk[cur_track-1].elapsed);
	if (sec>=0) {
	  min = sec / 60;
	  sec = sec % 60;
	  sprintf(ptr, "/%02d:%02d", min, sec);
	}
      }
      if (strcmp(newtip, clocktip)) {
	strcpy(clocktip, newtip);
	McTipRedraw(app);
      }
    }
  }

  /* update clocks */
  switch (clk_mode) {
    int pos;
  case CLK_TRACK:
    indx = PIX_T;
    pos = (cur_pos_rel>0)?cur_pos_rel:-cur_pos_rel;
    sec = pos % 60;
    min = pos / 60;
    sprintf(buf, "%s%02d:%02d%s",
	    (cur_pos_rel<0)?"-":" ", min, sec, (cur_pos_rel<0)?"":" ");
    break;
  case CLK_TRACKREMAIN:
    indx = PIX_TR;
    pos = (cur_pos_rel>0)?
      (thiscd.trk[cur_track-1].length-cur_pos_rel):-cur_pos_rel;
    sec = pos % 60;
    min = pos / 60;
    sprintf(buf, "%s%02d:%02d%s",
	    (cur_pos_rel<0)?"-":" ", min, sec, (cur_pos_rel<0)?"":" ");
    break;
  case CLK_ELAPSED:
    indx = PIX_E;
    pos = ((cur_pos_rel>0)?cur_pos_rel:0) + thiscd.trk[cur_track-1].elapsed;
    sec = pos % 60;
    min = pos / 60;
    sprintf(buf, "%02d:%02d", min, sec);
    break;
  default:
    indx = PIX_R;
    if (cur_length) {
      sec = cur_length - 
	(((cur_pos_rel>0)?cur_pos_rel:0) + thiscd.trk[cur_track-1].elapsed);
      if (sec>=0) {
	min = sec / 60;
	sec = sec % 60;
	sprintf(buf, "%02d:%02d", min, sec);
	break;
      }
    }
    strcpy(buf, "--:--");
  }

  {
    int update=0;
    if (Gad[MSG_TIME]->normalBitmap != &pmap[indx]) {
      Gad[MSG_TIME]->normalBitmap = &pmap[indx];
      update=1;
    }

    if (strcmp(Gad[MSG_TIME]->normalLabel->text,buf)) {
      McChangeText(mainWindow, Gad[MSG_TIME]->normalLabel, buf, 0);
      Gad[MSG_TIME]->normalLabel->x =
	((Gad[MSG_TIME]->width - Gad[MSG_TIME]->normalLabel->width)>>1) + 2;
      update=1;
    }

    if (update)
      McGadgetUpdate(Gad[MSG_TIME]);
  }

  switch (cur_cdmode) {
  case CDEJECT:
    strcpy(buf, "--");
    break;
  case CDSTOP:
    sprintf(buf, "%02d", save_track);
    break;
  default:
    sprintf(buf, "%02d", cur_track);
    break;
  }
  if (strcmp(Gad[MSG_TRACK]->normalLabel->text,buf)) {
    McChangeText(mainWindow, Gad[MSG_TRACK]->normalLabel, buf, 0);
    Gad[MSG_TRACK]->normalLabel->x =
      ((Gad[MSG_TRACK]->width - Gad[MSG_TRACK]->normalLabel->width)>>1);
    McGadgetUpdate(Gad[MSG_TRACK]);
  }

  if (cur_cdmode == CDPLAY || cur_cdmode == CDPAUSE)
    indx = PIX_STOP;
  else
    indx = PIX_PLAY;
  if (Gad[BTN_PLAYSTOP]->normalBitmap != &pmap[indx]) {
    Gad[BTN_PLAYSTOP]->normalBitmap = &pmap[indx];
    McGadgetUpdate(Gad[BTN_PLAYSTOP]);
  }

  if (!enable_eject) {
#ifdef CDROMCLOSETRAY
      indx=PIX_NEWCD;
#else
      indx=PIX_NOLOAD;
#endif
  } else {
    indx=PIX_EJECT;
  }
  if (Gad[BTN_EJECT]->normalBitmap != &pmap[indx]) {
    Gad[BTN_EJECT]->normalBitmap = &pmap[indx];
    McGadgetRedraw(Gad[BTN_EJECT]);
  }
}

int free_songs(void) {
  McOrderItem *it, *next;
  if ((it=ORDER(Gad[ORD_SONGS])->first)) {
    while(it) {
      next=it->next;
      free(it);
      it=next;
    }
    ORDER(Gad[ORD_SONGS])->first=NULL;
    return 1;
  }
  return 0;
}
  
/**************************************************************************/

static void no_cd(void) {
  if (free_songs()) {
    McGadgetRedraw(Gad[ORD_SONGS]);
  }
  McSetTitle(mainWindow, app->window_title);
  cd_stopped_at_end = 0;
}

void update_db(int reread) {
  new_cd(reread);
  if (cur_cdmode == CDPLAY) replay_list(); else { find_first(); find_end(); }
}

static void AddOrderTips(void) {
  McOrderItem *it;
  int i;

  it=ORDER(Gad[ORD_SONGS])->first;
  if (!it) return;
  for (i=0; it; i++, it=it->next) {
    int n=it->id;
    if ((n>0) && (n<=thiscd.ntracks)) {
      if (thiscd.trk[n-1].songname[0])
	it->tip=thiscd.trk[n-1].songname;
      else
	it->tip="     ...     ";
    } else if (n==ID_START) {
      it->tip=_("Start mark");
    } else if (n==ID_STOP) {
      it->tip=_("End mark");
    } else it->tip="Bug mark (-:";
  }
}

void new_cd(int reread) {
  McOrderItem *it;
  int i;
  unsigned char buf[512];

  save_track = 1;
  free_songs();

  if (reread) {
    if (currentFile) free(currentFile);
    *thiscd.cdname=0;
    currentFile=read_db();
  }

  if (!ORDER(Gad[ORD_SONGS])->first) {
    int flag=0;
    it=NULL;
    for (i=0;i<cur_ntracks;i++) {
      if (thiscd.trk[i].avoid) {
	it=McCreateOrderItem(it);
	if (!flag) { ORDER(Gad[ORD_SONGS])->first=it; flag=1; }
	it->id=i+1;
      }
    }

    it=McCreateOrderItem(it);
    it->id=ID_START;
    if (!flag) { ORDER(Gad[ORD_SONGS])->first=it; flag=1; }

    for (i=0;i<cur_ntracks;i++) {
      if (!thiscd.trk[i].avoid) {
	it=McCreateOrderItem(it);
	it->id=i+1;
      }
    }

    it=McCreateOrderItem(it);
    it->id=ID_STOP;
  }

  AddOrderTips();

  McGadgetRedraw(Gad[ORD_SONGS]);
  cur_stoptrack=cur_ntracks;

  save_track=cur_track=find_first();
  find_end();

  if (*thiscd.cdname) {
    strcpy(buf, app->class);
    strcat(buf,": ");
    strcat(buf,thiscd.cdname);
  } else {
    strcpy(buf, APP_VERSION);
  }
  McSetTitle(mainWindow, buf);
  last_track_displayed = -1;
  cd_update(cd_status());
}

static void shuffle_cd(void) {
  McOrderItem *beg, *end, *it;
  int i, cnt;

  save_track = 1;

  /* Shuffle affects only songs between start & end mark, so find them and
   * count the number of songs inbetween
   */
  beg=ORDER(Gad[ORD_SONGS])->first;
  while(beg && (beg->id !=ID_START)) beg=beg->next;
  if (beg) beg=beg->next;
  if (!beg) {
    fprintf(stderr,_("%s: Confused, no start mark.\n"), myname);
    return;
  }

  cnt=0;
  end=beg->next;
  while(end && (end->id !=ID_STOP)) { cnt++; end=end->next; }
  if (!end) {
    fprintf(stderr,_("%s: Confused, no stop mark.\n"), myname);
    return;
  }

  /* Now find out if the list is in order, that is rising monotonically */
  it = beg->next; i=beg->id;
  while(it && (it!=end) && (it->id>i)) { i=it->id; it=it->next; }


  if (it==end) { /* it is rising monotonically, so shuffle it */
    /* From Morgan Antonsson <d0antmo@dtek.chalmers.se> */
    /* Here is my algorithm. It is more random.         */
    int times, j;
    i=0;
    while(beg && (beg!=end)) {
      times = random() % (cnt - i + 1);
      if(times > 0) {
	it = beg;
	for(j = 0; j < times; j++)
	  it = it->next;
	j = it->id;
	it->id = beg->id;
	beg->id = j;
      }
      i++;
      beg=beg->next;
    }
    /* End of changes. */
    
    beg=beg->next;
  } else { /* It seems to be shuffled, so order it */
    while(beg && (beg!=end)) {
      it=beg->next;
      while(it && (it!=end)) {
	if (it->id < beg->id) { i=it->id; it->id=beg->id; beg->id=i; }
	it=it->next;
      }
      beg=beg->next;
    }
  }
  
  McGadgetRedraw(Gad[ORD_SONGS]);
}

/*************************************************************************/

void start_cd(void) {
  if (cd_stopped_at_end)
    save_track = find_first();  /* If the CD has stopped at its
				   end then start over */
  cur_track=save_track;
  play_list(0);
}

static void play_list(int from) {
  int e;

  ValidateCurrent();
  e = find_end();

#ifdef DBG_LIST
  printf("play_cd(%d,%d,%d)       n=%d  last=%d  stop=%d\n",
	 cur_track,from,e,cur_ntracks, cur_lasttrack, cur_stoptrack);
#endif
  set_error(play_cd(cur_track,from,e));

  ValidateCurrent();
}

static void replay_list(void) {
  int e = find_end();

#ifdef DBG_LIST
  printf("replay_cd(%d)     n=%d  last=%d  stop=%d\n",
	 e,cur_ntracks, cur_lasttrack, cur_stoptrack);
#endif
  if (e!=cur_lasttrack) {
    set_error(replay_cd(e));
  }
}

/***************************************************************************/

static int find_next(void) {
  McOrderItem *it = ValidateCurrent();

#ifdef DBG_LIST
  printf("find next of %d\n", cur_track);
#endif
  if (it) it=it->next;
  if (it && it->id>cur_ntracks) it=it->next;
  if (it && it->id>cur_ntracks) it=NULL;
  if (it) { SetCurrent(it); return it->id; } else { return 0; }
}

static int find_prev(void) {
  McOrderItem *it = ValidateCurrent();

#ifdef DBG_LIST
  printf("find prev of %d\n",cur_track);
#endif
  if (it) it=it->prev;
  if (it && it->id>cur_ntracks) it=it->prev;
  if (it && it->id>cur_ntracks) it=NULL;
  if (it) { SetCurrent(it); return it->id; } else { return 0; }
}

static int find_first(void) {
  McOrderItem *it = ORDER(Gad[ORD_SONGS])->first;

#ifdef DBG_LIST
  printf("find first\n");
#endif
  while(it && (it->id != ID_START)) it=it->next;
  if (it) it=it->next;
  if (it && it->id>cur_ntracks) it=0;
  if (it) SetCurrent(it);
  return it?it->id:1;
}

static int find_end(void) {
  /* Find out how many tracks can be played in a row
   * e.g. if the current list is 5 2 3 4 7 6 and track is 2, then
   * 2 3 4 can be played in a row. This prevents nasty pausing (like done
   * by PocketCD for Windoze)
   * Do some additional computing: first_track, stop_track and track_lengths
   */
  McOrderItem *tr, *it = ORDER(Gad[ORD_SONGS])->first,
                   *cu = ValidateCurrent();
  int to_track=0, first_track=0;
  int in_list = 0;

  cur_length=0;
  cur_stoptrack=0;
  while(it) {
    tr=it->next;
    if (tr && (it->id==ID_START)) first_track=tr->id;
    if (first_track && (it->id<=cur_ntracks) && (!cur_stoptrack)) {
      if (cu==it) in_list=1;
      thiscd.trk[it->id-1].elapsed=cur_length;
      cur_length+=thiscd.trk[it->id-1].length;
    }
    if (tr && (tr->id==ID_STOP)) cur_stoptrack=it->id;
    if (!to_track) {
      if (it==cu) to_track=cu->id;
    } else {
      if (cu) {
	if (it->id==to_track+1) {
	  to_track++;
	} else {
	  cu=NULL;
	}
      }
    }
    it=it->next;
  }
  if (!cur_stoptrack) { cur_stoptrack=cur_ntracks; cur_length=0; }

  UpdateNeedToPoll();
#if 0
  printf("end=%d\n", to_track?to_track:cur_ntracks);
#endif
  return to_track?to_track:cur_ntracks;
}

/***************************************************************************/

long udiff(struct timeval *old, struct timeval *new) {
  struct timeval new2;
  long oldu, newu, diff;

  gettimeofday(&new2, NULL);
  if (old->tv_sec + 2146 > new2.tv_sec) {
    oldu = old->tv_sec * 1000000 + old->tv_usec;
    newu = new2.tv_sec * 1000000 + new2.tv_usec;
    diff = newu - oldu;
  } else {
    diff = 2147483647;
  }

  if (new) {
    new->tv_usec = new2.tv_usec;
    new->tv_sec  = new2.tv_sec;
  }
  return diff;

}

/***************************************************************************/

static McOrderItem *ValidateCurrent() {
  if (!currentItem) currentItem = ORDER(Gad[ORD_SONGS])->first;

  if (currentItem->id!=cur_track) {
    while (currentItem && (currentItem->id!=cur_track))
      currentItem=currentItem->next;
    if (!currentItem) {
      currentItem = ORDER(Gad[ORD_SONGS])->first;
      while (currentItem && (currentItem->id!=cur_track))
	currentItem=currentItem->next;
    }
    if (!currentItem) currentItem = ORDER(Gad[ORD_SONGS])->first; /* Uh Oh */
  }
  return currentItem;
}

#ifdef SHOW_CD_MODE
static void show_cd_mode(void) {
  switch(cur_cdmode) {
  case CDNULL:
    printf("CDNULL\n");
    return;
  case CDSTOP:
    printf("CDSTOP\n");
    return;
  case CDPLAY:
    printf("CDPLAY\n");
    return;
  case CDEJECT:
    printf("CDEJECT\n");
    return;
  case CDPAUSE:
    printf("CDPAUSE\n");
    return;
  default:
    printf("CDMODE=%d\n",cur_cdmode);
    return;
  }
}
#endif
