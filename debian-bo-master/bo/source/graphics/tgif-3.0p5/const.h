/*
 * Author:      William Chia-Wei Cheng (william@cs.ucla.edu)
 *
 * Copyright (C) 1990-1996, William Chia-Wei Cheng.
 *
 * Permission limited to the use, copy, display, distribute without
 * charging for a fee, and produce derivative works of "tgif" and
 * its documentation for not-for-profit purpose is hereby granted by
 * the Author, provided that the above copyright notice appears in
 * all copies made of "tgif" and that both the copyright notice
 * and this permission notice appear in supporting documentation,
 * and that the name of the Author not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  The Author makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.  All other rights (including, but not limited to, the
 * right to sell "tgif", the right to sell derivative works of
 * "tgif", and the right to distribute "tgif" for a fee) are
 * reserved by the Author.
 *
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
 * OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/const.h,v 3.0 1996/05/06 16:04:17 william Exp $
 */

#ifndef _TGIF_CONST_H_
#define _TGIF_CONST_H_

#if __STDC__ || defined(__cplusplus) || defined(c_plusplus)
/* ANSI || C++ */
#ifdef _NO_PROTO
   /* just in case ANSI or C++ doesn't handle function prototypes well */
#define ARGS_DECL(args) ()
#else
#define ARGS_DECL(args) args
#endif
#else
#define ARGS_DECL(args) ()
#endif

#include "tgif_dbg.h"

#define TOOL_NAME "Tgif"

#ifndef NULL
#define NULL 0
#endif

#ifndef TRUE
#define FALSE 0
#define TRUE 1
#endif

#define INVALID (-1)
#undef BAD
#define BAD (-2)

#define SINGLECOLOR (FALSE)
#define MULTICOLOR (TRUE)

#define BUTTONSMASK ((Button1Mask)|(Button2Mask)|(Button3Mask))

#ifndef max
#define max(A,B) (((A)>(B)) ? (A) : (B))
#define min(A,B) (((A)>(B)) ? (B) : (A))
#endif

#ifndef round
#define round(X) (((X) >= 0) ? (int)((X)+0.5) : (int)((X)-0.5))
#endif

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#define MAX_ZOOMED_IN 3
#define HALF_W(w) (((w)&0x1) ? ((w)>>1)+1 : ((w)>>1))
#define ZOOMED_HALF_W(w) ((zoomedIn) ? ((w)<<(zoomScale-1)) : \
      ((((w)>>(zoomScale))&0x1) ? (((w)>>(zoomScale+1))+1) : \
      ((w)>>(zoomScale+1))))

#define OFFSET_X(AbsX) ((zoomedIn) ? (((AbsX)-drawOrigX)<<zoomScale) : \
                                     (((AbsX)-drawOrigX)>>zoomScale))
#define OFFSET_Y(AbsY) ((zoomedIn) ? (((AbsY)-drawOrigY)<<zoomScale) : \
                                     (((AbsY)-drawOrigY)>>zoomScale))

#define ABS_X(OffsetX) ((zoomedIn) ? (((OffsetX)>>zoomScale)+drawOrigX) : \
                                     (((OffsetX)<<zoomScale)+drawOrigX))
#define ABS_Y(OffsetY) ((zoomedIn) ? (((OffsetY)>>zoomScale)+drawOrigY) : \
                                     (((OffsetY)<<zoomScale)+drawOrigY))

#define ZOOMED_SIZE(AbsSize) ((zoomedIn) ? ((AbsSize)<<zoomScale) : \
                                           ((AbsSize)>>zoomScale))

#define GRID_ZOOMED_SIZE(AbsSize) ((zoomedIn) ? (AbsSize) : \
                                                ((AbsSize)>>zoomScale))

#define ABS_SIZE(ZoomedSize) ((zoomedIn) ? ((ZoomedSize)>>zoomScale) : \
                                           ((ZoomedSize)<<zoomScale))
#define GRID_ABS_SIZE(ZoomedSize) ((zoomedIn) ? (ZoomedSize) : \
                                                ((ZoomedSize)<<zoomScale))

#define SetRecVals(R,X,Y,W,H) ((R).x=(X),(R).y=(Y),(R).width=(W),(R).height=(H))

#define MARK(W,GC,X,Y) XFillRectangle(mainDisplay,W,GC, \
      (X)-handleSize,(Y)-handleSize,(handleSize<<1)+1,(handleSize<<1)+1)
#define MARKV(W,GC,X,Y) XDrawLine(mainDisplay,W,GC, \
      (X)-(handleSize+1),(Y),(X)+(handleSize+1),(Y)); \
      XDrawLine(mainDisplay,W,GC,(X),(Y)-(handleSize+1),(X),(Y)+(handleSize+1))
#define MARKO(W,GC,X,Y) XFillArc(mainDisplay,W,GC, \
      (X)-handleSize,(Y)-handleSize,(handleSize<<1)+1,(handleSize<<1)+1, \
      0,(360<<6))
#define PtInMark(PtX,PtY,MarkX,MarkY) ((PtX) >= (MarkX)-handleSize && \
      (PtY) >= (MarkY)-handleSize && (PtX) <= (MarkX)+handleSize && \
      (PtY) <= (MarkY)+handleSize)
#define MyDashedLine(W,GC,V,N) XDrawLines(mainDisplay,W,GC,V,N,CoordModeOrigin)

#define MAXSTRING 256
#define MAXPATHLENGTH 256

/* object types */

#define OBJ_POLY 0
#define OBJ_BOX 1
#define OBJ_OVAL 2
#define OBJ_TEXT 3
#define OBJ_POLYGON 4
#define OBJ_GROUP 5
#define OBJ_SYM 6
#define OBJ_ICON 7
#define OBJ_ARC 8
#define OBJ_RCBOX 9
#define OBJ_XBM 10
#define OBJ_XPM 11

/* drawing modes */

#define NOTHING 0
#define DRAWTEXT 1
#define DRAWBOX 2
#define DRAWCIRCLE 3
#define DRAWPOLY 4
#define DRAWPOLYGON 5
#define DRAWARC 6
#define DRAWRCBOX 7
#define FREEHAND 8
#define VERTEXMODE 9
#define ROTATEMODE 10

#define MAXCHOICES 11

/* stipple patterns */

#define NONEPAT 0
#define SOLIDPAT 1
#define BACKPAT 2
#define SCROLLPAT 7
#define MAXPATTERNS 32

/* line stuff */

#define LINE_THIN 0
#define LINE_MEDIUM 1
#define LINE_THICK 2
#define LINE_CURVED 3 /* compatibility hack for fileVersion <= 3 */

#define MAXLINEWIDTHS 7

#define LT_STRAIGHT 0
#define LT_SPLINE 1
#define LT_INTSPLINE 2

#define MAXLINETYPES 3

#define LS_PLAIN 0
#define LS_RIGHT 1
#define LS_LEFT 2
#define LS_DOUBLE 3

#define MAXLINESTYLES 4

#define MAXDASHES 9

#define NOCONT (FALSE)
#define CONT (TRUE)

#define NORB (FALSE)
#define RB (TRUE)

/* font stuff */

#define FONT_TIM 0
#define FONT_COU 1
#define FONT_HEL 2
#define FONT_CEN 3
#define FONT_SYM 4

#define MAXFONTS 5

#define STYLE_NR 0
#define STYLE_BR 1
#define STYLE_NI 2
#define STYLE_BI 3

#define MAXFONTSTYLES 4

#define FONT_DPI_75 0
#define FONT_DPI_100 1

#define MAXFONTDPIS 2

#define MAXFONTSIZES 6

#define JUST_L 0
#define JUST_C 1
#define JUST_R 2

#define MAXJUSTS 3

#define PUSH_FONT 0
#define PUSH_SIZE 1
#define PUSH_STYLE 2
#define PUSH_JUST 3
#define PUSH_ROTATE 4
#define PUSH_PEN 5
#define PUSH_FILL 6
#define PUSH_VSPACE 7
#define PUSH_COLORINDEX 8
#define PUSH_UNDERLINEON 9
#define PUSH_UNDERLINE 10

/* alignment */

#define ALIGN_N 0

#define ALIGN_L 1
#define ALIGN_C 2
#define ALIGN_R 3

#define ALIGN_T 1
#define ALIGN_M 2
#define ALIGN_B 3

#define ALIGN_S 4

#define MAXALIGNS 5

#define ALIGN_SHIFT (MAXALIGNS)

#define ALIGN_NN ((ALIGN_N<<ALIGN_SHIFT)|ALIGN_N)
#define ALIGN_NT ((ALIGN_N<<ALIGN_SHIFT)|ALIGN_T)
#define ALIGN_NM ((ALIGN_N<<ALIGN_SHIFT)|ALIGN_M)
#define ALIGN_NB ((ALIGN_N<<ALIGN_SHIFT)|ALIGN_B)
#define ALIGN_NS ((ALIGN_N<<ALIGN_SHIFT)|ALIGN_S)
#define ALIGN_LN ((ALIGN_L<<ALIGN_SHIFT)|ALIGN_N)
#define ALIGN_LT ((ALIGN_L<<ALIGN_SHIFT)|ALIGN_T)
#define ALIGN_LM ((ALIGN_L<<ALIGN_SHIFT)|ALIGN_M)
#define ALIGN_LB ((ALIGN_L<<ALIGN_SHIFT)|ALIGN_B)
#define ALIGN_LS ((ALIGN_L<<ALIGN_SHIFT)|ALIGN_S)
#define ALIGN_CN ((ALIGN_C<<ALIGN_SHIFT)|ALIGN_N)
#define ALIGN_CT ((ALIGN_C<<ALIGN_SHIFT)|ALIGN_T)
#define ALIGN_CM ((ALIGN_C<<ALIGN_SHIFT)|ALIGN_M)
#define ALIGN_CB ((ALIGN_C<<ALIGN_SHIFT)|ALIGN_B)
#define ALIGN_CS ((ALIGN_C<<ALIGN_SHIFT)|ALIGN_S)
#define ALIGN_RN ((ALIGN_R<<ALIGN_SHIFT)|ALIGN_N)
#define ALIGN_RT ((ALIGN_R<<ALIGN_SHIFT)|ALIGN_T)
#define ALIGN_RM ((ALIGN_R<<ALIGN_SHIFT)|ALIGN_M)
#define ALIGN_RB ((ALIGN_R<<ALIGN_SHIFT)|ALIGN_B)
#define ALIGN_RS ((ALIGN_R<<ALIGN_SHIFT)|ALIGN_S)
#define ALIGN_SN ((ALIGN_S<<ALIGN_SHIFT)|ALIGN_N)
#define ALIGN_ST ((ALIGN_S<<ALIGN_SHIFT)|ALIGN_T)
#define ALIGN_SM ((ALIGN_S<<ALIGN_SHIFT)|ALIGN_M)
#define ALIGN_SB ((ALIGN_S<<ALIGN_SHIFT)|ALIGN_B)
#define ALIGN_SS ((ALIGN_S<<ALIGN_SHIFT)|ALIGN_S)

/* color */

#define MAXCOLORS 11

/* button stuff */

#define CONFIRM_YES 0
#define CONFIRM_NO 1
#define CONFIRM_CANCEL 2

#define MAX_CONFIRMS 3

#define BUTTON_INVERT 0
#define BUTTON_NORMAL 1

/* page style */

#define PORTRAIT 0
#define LANDSCAPE 1
#define HIGHPORT 2 /* obsolete */
#define HIGHLAND 3 /* obsolete */
#define SLIDEPORT 4 /* obsolete */
#define SLIDELAND 5 /* obsolete */

#define MAXPAGESTYLES 2

/* where to print */

#define PRINTER 0
#define LATEX_FIG 1
#define PS_FILE 2
#define XBM_FILE 3
#define TEXT_FILE 4
#define EPSI_FILE 5
#define GIF_FILE 6
#define HTML_FILE 7

#define MAXWHERETOPRINT 8

/* measurement */

#define ENGLISH_GRID 0
#define METRIC_GRID 1

#define PIX_PER_INCH 128
#define ONE_INCH (PIX_PER_INCH)
#define HALF_INCH (PIX_PER_INCH/2)
#define QUARTER_INCH (PIX_PER_INCH/4)
#define EIGHTH_INCH (PIX_PER_INCH/8)

#define DEFAULT_ENGLISH_GRID (EIGHTH_INCH)

#define PIX_PER_MM 5
#define ONE_MM (PIX_PER_MM)
#define TWO_MM (PIX_PER_MM*2)
#define FIVE_MM (PIX_PER_MM*5)
#define ONE_CM (PIX_PER_MM*10)

#define DEFAULT_METRIC_GRID (TWO_MM)

/* rotation -- clockwise */

#define ROTATE0 0
#define ROTATE90 1
#define ROTATE180 2
#define ROTATE270 3

#define CLOCKWISE90 (90<<6)
#define COUNTER90 ((-90)<<6)
#define CLOCKWISE180 (180<<6)
#define COUNTER180 ((-180)<<6)
#define CLOCKWISE270 (270<<6)
#define COUNTER270 ((-270)<<6)

/* flipping */

#define NO_FLIP 0
#define HORI_ODD (1<<0)
#define HORI_EVEN (1<<1)
#define VERT_ODD (1<<2)
#define VERT_EVEN (1<<3)

/* arc */

#define ARC_CCW 0 /* counter-clock-wise */
#define ARC_CW 1 /* clock-wise */

/* main menu */

#define MENU_MODE 0
#define MENU_FILE 1
#define MENU_EDIT 2
#define MENU_LAYOUT 3
#define MENU_MOVEMODE 4
#define MENU_ARRANGE 5
#define MENU_PAGE 6
#define MENU_PAGELAYOUT 7
#define MENU_HORIALIGN 8
#define MENU_VERTALIGN 9
#define MENU_FONT 10
#define MENU_STYLE 11
#define MENU_SIZE 12
#define MENU_SHAPE 13
#define MENU_STRETCHTEXT 14
#define MENU_LINEDASH 15
#define MENU_LINESTYLE 16
#define MENU_LINETYPE 17
#define MENU_LINEWIDTH 18
#define MENU_FILL 19
#define MENU_PEN 20
#define MENU_COLOR 21
#define MENU_IMAGEPROC 22
#define MENU_NAVIGATE 23
#define MENU_SPECIAL 24
#define MENU_HELP 25

#define MAXMENUS 26

/* file menu */

#define FILE_NEW 0
#define FILE_OPEN 1
#define FILE_SAVE 2
#define FILE_SAVENEW 3
#define FILE_IMPORT 4
#define FILE_IMPORTXBM 5
#define FILE_IMPORTXPM 6
#define FILE_IMPORTEPS 7
#define FILE_IMPORTGIF 8
#define FILE_IMPORTOTHERS 9
#define FILE_EMBEDEPS 10
#define FILE_BROWSEXBM 11
#define FILE_BROWSEXPM 12
#define FILE_BROWSEOTHERS 13
#define FILE_DOMAIN 14
#define FILE_DUMP 15
#define FILE_USR_DUMP 16
#define FILE_DUMPSELECTED 17
#define FILE_PRINTONE 18
#define FILE_SETEXPORTTRIM 19
#define FILE_INPUT_POLY 20
#define FILE_INPUT_POLYGON 21
#define FILE_SET_TEMPLATE 22
#define FILE_SOLVE 23
#define FILE_SIMULATE 24
#define FILE_PROBE 25
#define FILE_ANIMATE 26
#define FILE_ESCAPE 27
#define FILE_SAVESELAS 28
#define FILE_SAVESYMINLIB 29
#define FILE_QUIT 30

#define FILEMENUENTRIES 31

/* edit menu */

#define EDIT_REDRAW 0
#define EDIT_DUP 1
#define EDIT_DELETE 2
#define EDIT_SELALL 3
#define EDIT_UNDO 4
#define EDIT_REDO 5
#define EDIT_DEL_POINT 6
#define EDIT_ADD_POINT 7
#define EDIT_COPY 8
#define EDIT_CUT 9
#define EDIT_PASTE 10
#define EDIT_PASTE_FROM_FILE 11
#define EDIT_UPDATE 12
#define EDIT_SCALE 13
#define EDIT_FLUSH_UNDO 14
#define EDIT_PRINT_MSG_BUF 15
#define EDIT_INV_XBM 16
#define EDIT_PUSH_CUR_CHOICE 17
#define EDIT_MAKE_PREC_ARC 18
#define EDIT_CUT_MAPS 19
#define EDIT_RESTORE_MAPS 20
#define EDIT_UPDATE_EPS 21
#define EDIT_CV_INTSPLINE 22
#define EDIT_SMOOTH_HINGE 23
#define EDIT_MAKE_REGULAR 24
#define EDIT_BREAK_TEXT 25
#define EDIT_SET_SEL_LINEWIDTH 26
#define EDIT_ADD_COLOR 27
#define EDIT_BREAK_MAPS 28
#define EDIT_LAYOUT_ON_ARC 29
#define EDIT_PRECISE_ROTATE 30
#define EDIT_JOIN_POLY 31
#define EDIT_CUT_POLY 32
#define EDIT_GET_BBOX 33

#define MAXEDITMENUS 34

/* layout menu */

#define LAYOUT_INCGRID 0
#define LAYOUT_DECGRID 1
#define LAYOUT_GRID 2
#define LAYOUT_SNAP 3
#define LAYOUT_GOHOME 4
#define LAYOUT_SETHOME 5
#define LAYOUT_ZOOMIN 6
#define LAYOUT_ZOOMOUT 7
#define LAYOUT_DEFZOOM 8
#define LAYOUT_ZOOMWAYOUT 9
#define LAYOUT_LAND 10
#define LAYOUT_PORT 11
#define LAYOUT_PRINTMAG 12
#define LAYOUT_TOGGLE_WHERE_TO_PRINT 13
#define LAYOUT_TOGGLE_COLOR_PS 14
#define LAYOUT_TOGGLE_MOVE_MODE 15
#define LAYOUT_TOGGLE_GRID_SYSTEM 16
#define LAYOUT_TOGGLE_MAP_SHOWN 17
#define LAYOUT_TOGGLE_USE_GRAY 18
#define LAYOUT_TOGGLE_SHOW_MEASURE 19
#define LAYOUT_TOGGLE_SET_MEASURE_UNIT 20
#define LAYOUT_TOGGLE_SHOW_MENUBAR 21
#define LAYOUT_TOGGLE_SHOW_STATUS 22
#define LAYOUT_TOGGLE_ONEMOTION_SELMOVE 23
#define LAYOUT_TOGGLE_COLOR_LAYERS 24

#define MAXLAYOUTMENUS 25

/* move mode stuff */

#define CONST_MOVE 0
#define UNCONST_MOVE 1

#define MAXMOVEMODES 2

/* stretchable text stuff */

#define NO_STRETCHABLE_TEXT 0
#define STRETCHABLE_TEXT 1

#define MAXSTRETCHABLEMODES 2

/* arrange menu */

#define ARRANGE_FRONT 0
#define ARRANGE_BACK 1
#define ARRANGE_GROUP 2
#define ARRANGE_UNGROUP 3
#define ARRANGE_LOCK 4
#define ARRANGE_UNLOCK 5
#define ARRANGE_ALIGNOBJ 6
#define ARRANGE_ALIGNGRID 7
#define ARRANGE_ALIGNPAGE 8
#define ARRANGE_DISTROBJ 9
#define FLIP_HORIZONTAL 10
#define FLIP_VERTICAL 11
#define ROTATE_CLOCKWISE 12
#define ROTATE_COUNTER 13
#define SET_TEXT_ROTATE 14
#define SET_ROTATE_INC 15
#define ALIGN_OBJ_TOP 16
#define ALIGN_OBJ_MIDDLE 17
#define ALIGN_OBJ_BOTTOM 18
#define ALIGN_OBJ_LEFT 19
#define ALIGN_OBJ_CENTER 20
#define ALIGN_OBJ_RIGHT 21
#define ABUT_HORIZONTAL 22
#define ABUT_VERTICAL 23
#define ALIGN_CENTERANENDPOINT 24

#define MAXARRANGEMENUS 25

/* nagivate menu */

#define NAVIGATE_BACK 0
#define NAVIGATE_FORWARD 1
#define NAVIGATE_REFRESH 2
#define NAVIGATE_HOTLIST 3
#define NAVIGATE_ADD 4
#define NAVIGATE_HISTORY 5
#define NAVIGATE_HYPERSPACE 6

#define MAXNAVIGATEMENUS 7

/* special menu */

#define SPECIAL_SYM 0
#define SPECIAL_UNSYM 1
#define SPECIAL_INST 2
#define SPECIAL_ICON 3
#define SPECIAL_UNICON 4
#define SPECIAL_PUSH 5
#define SPECIAL_POP 6
#define SPECIAL_ADDATTR 7
#define SPECIAL_DETACHATTR 8
#define SPECIAL_SHOWATTR 9
#define SPECIAL_SHOWATTRNAME 10
#define SPECIAL_HIDEATTR 11
#define SPECIAL_HIDEATTRNAME 12
#define SPECIAL_MOVEATTR 13
#define SPECIAL_EDITATTR 14
#define SPECIAL_ANIMATESEND 15
#define SPECIAL_ANIMATEFLASH 16
#define SPECIAL_ADDFILEATTR 17
#define SPECIAL_DETACHFILEATTR 18
#define SPECIAL_EDITFILEATTR 19
#define SPECIAL_UPDATESYMS 20
#define SPECIAL_IMPORT_ATTRS 21
#define SPECIAL_EXPORT_ATTRS 22
#define SPECIAL_MERGE_WITH_TABLE 23
#define SPECIAL_EXPORT_TO_TABLE 24
#ifdef _TGIF_WB
#define SPECIAL_WHITE_BOARD 25
#endif /* _TGIF_WB */

#ifdef _TGIF_WB
#define MAXSPECIALMENUS 26
#else /* ~_TGIF_WB */
#define MAXSPECIALMENUS 25
#endif /* _TGIF_WB */

/* page menu */

#define PAGE_NEXT 0
#define PAGE_PREV 1
#define PAGE_NAME 2
#define PAGE_GOTO 3
#define PAGE_ADDBEFORE 4
#define PAGE_ADDAFTER 5
#define PAGE_DEL 6
#define PAGE_PRINT_ONE_IN_STACK 7
#define PAGE_PAPER_SIZE_IN_STACK 8
#define PAGE_DEL_MANY 9
#define PAGE_PRINT_ONE_FILE_PER_PAGE 10

#define MAXPAGESTACKMENUS 11

#define PAGE_TOGGLELINE 0
#define PAGE_SIZING 1
#define PAGE_PRINT_ONE 2
#define PAGE_PAPER_SIZE 3

#define MAXPAGETILEMENUS 4

#define PAGE_STACK 0
#define PAGE_TILE 1

#define MAXPAGELAYOUTMODES 2

/* scroll stuff */

#define SCROLL_LEFTEND 0
#define SCROLL_LEFT 1
#define SCROLL_RIGHT 2
#define SCROLL_RIGHTEND 3
#define SCROLL_UPEND 4
#define SCROLL_UP 5
#define SCROLL_CHECKALL 6
#define SCROLL_UNCHECKALL 7
#define SCROLL_DOWN 8
#define SCROLL_DOWNEND 9

#define MAXSCROLLBUTTONS 10

/* pixel stuff */

#ifdef sun
#define PUT_A_POINT(dpy,win,gc,x,y) XDrawPoint(dpy,win,gc,x,y)
#else
#ifdef ultrix
#define PUT_A_POINT(dpy,win,gc,x,y) XDrawPoint(dpy,win,gc,x,y)
#else
#ifdef _USE_XDRAWPOINT_TO_PUT_A_POINT
#define PUT_A_POINT(dpy,win,gc,x,y) XDrawPoint(dpy,win,gc,x,y)
#else
#define PUT_A_POINT(dpy,win,gc,x,y) XDrawLine(dpy,win,gc,x,y,x,y)
#endif
#endif
#endif

/* file stuff */

#define OBJ_FILE_SAVED 0
#define SYM_FILE_SAVED 1

/* xbm real_type */

#define XBM_XBM 0
#define XBM_EPS 1
#define XBM_EPSI 2

/* cmd stuff */

#define CMD_COMPOSITE	0
#define CMD_NEW		1
#define CMD_DELETE	2
#define CMD_MOVE	3
#define CMD_STRETCH	4
#define CMD_ONE_TO_MANY	5
#define CMD_MANY_TO_ONE	6
#define CMD_REPLACE	7
#define CMD_GOTO_PAGE	8

/* rcb radius stuff */

#define DEF_RCB_RADIUS (EIGHTH_INCH)
#define MIN_RCB_RADIUS 4

/* text editing stuff */

#define KEY_NONE 0
#define KEY_UP 1
#define KEY_DOWN 2
#define KEY_LEFT 3
#define KEY_RIGHT 4

/* select name dialog box stuff */

#define ITEM_DSPED 10
#define ITEM_LEN 30
#define ROW_HEIGHT ((msgFontPtr==NULL)?(defaultFontHeight+1):(msgFontHeight+1))
#define BUTTON_OK 0
#define BUTTON_SETDIR 1
#define BUTTON_CANCEL 2
#define MAXBUTTONS 3

/* status window stuff */

#define MAX_STATUS_BTNS 3

/* copypaste stuff */

#define TGIF_HEADER 0x80

/* interrupt stuff */

#define MAXINTRS 2

/* remote status */

#define TG_REMOTE_STATUS_OK 0
#define TG_REMOTE_STATUS_INTR 1 /* user interrupt */
#define TG_REMOTE_STATUS_MEM 2 /* memory allocation failed */
#define TG_REMOTE_STATUS_WRITE 3 /* write failed */
#define TG_REMOTE_STATUS_READ 4 /* read failed */
#define TG_REMOTE_STATUS_NET 5 /* unexpect network error */
#define TG_REMOTE_STATUS_HOST 6 /* gethostbyname failed */
#define TG_REMOTE_STATUS_FORMAT 7 /* format error */
#define TG_REMOTE_STATUS_TERM 8 /* terminated by the server */
#define TG_REMOTE_STATUS_FILE 9 /* fail to open file/socket */

#define FTP_LOGGED_IN 0x10000

/* corner stuff */

#define CORNER_NONE 0
#define CORNER_LT 1
#define CORNER_TOP 2
#define CORNER_RT 3
#define CORNER_RIGHT 4
#define CORNER_RB 5
#define CORNER_BOTTOM 6
#define CORNER_LB 7
#define CORNER_LEFT 8

/* free rotation stuff */

#define CTM_SX 0
#define CTM_SIN 1
#define CTM_MSIN 2
#define CTM_SY 3
#define CTM_TX 4
#define CTM_TY 5

/* msgbox stuff */

#define MB_ID_FAILED 0
#define MB_ID_OK 1
#define MB_ID_CANCEL 2
#define MB_ID_YES 3
#define MB_ID_NO 4
#define MB_ID_EXTRA 5

#define MB_BTN_NONE		0
#define MB_BTN_OK		1
#define MB_BTN_YESNOCANCEL	2
#define MB_BTN_OKCANCEL		3
#define MB_BTN_YESNO		4
#define MB_BTN_EXTRA		0x0008

#define MB_ICON_STOP		0x0010
#define MB_ICON_QUESTION	0x0020
#define MB_ICON_INFORMATION	0x0040
#define MB_ICON_DIALOG		0x0080

#define MB_BTNMASK		0x0007
#define MB_ICONMASK		0x00f0

#define INFO_MB (MB_BTN_OK | MB_ICON_INFORMATION)
#define YNC_MB  (MB_BTN_YESNOCANCEL | MB_ICON_QUESTION)
#define YN_MB   (MB_BTN_YESNO | MB_ICON_QUESTION)
#define STOP_MB (MB_BTN_OK | MB_ICON_STOP)

#define MB_PIXMAP_STOP 0
#define MB_PIXMAP_QUESTION 1
#define MB_PIXMAP_INFORMATION 2
#define MB_PIXMAP_DIALOG 3

#define MAX_MB_ICONS 4

/* expr stuff */

#define NULL_VAL 0
#define INT_VAL 1
#define DBL_VAL 2
#define STR_VAL 3

/* file version stuff */

#define START_HAVING_ATTRS 8

/* choice window width */

#define MAXCHOICEWINDOWCOLS 13

#endif /*_TGIF_CONST_H_*/
