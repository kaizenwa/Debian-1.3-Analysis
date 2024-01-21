#ifndef lint
static char SccsBtnhId[] = "%W%  %G%";
#endif

/*
 * Module:	buttons.h
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Define structures for SAO button toolkit
 * Typedefs:	ButtonLabel, ButtonLook, ButtonFeel, ButtonSpec
 * Typedefs:	BoxGeometry, BoxParent, BorderPatterns
 * Typedefs:	ButtonRecord, ButtonBox
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		21 March 1989
 *		{n} <who> -- <does what> -- <when>
 */

#define SUBMENU_LIMIT 5
#define OPTION_LIMIT 6
#define DATA_LIMIT 4	/* option and data limit should be both even or odd */

#define OFF_OUT 0
#define OFF_IN 1
#define ON_OUT 2
#define ON_IN 3

/* BUTTON FUNCTION CODES */
#define BTNNoOp    0x00  /* makes no report, does not highlight */
#define BTNFlash   0x01  /* reports selection event, flashed when pressed */
#define BTNOneShot 0x02  /* reports selection event, lighted while pressed */
#define BTNToggle  0x03  /* alternates on and off with selection events */
#define BTNWhile   0x04  /* reports selection and release events */
#define BTNCoWhile 0x05  /* same as While, unlights others while pressed */
#define BTNMode    0x06  /* ganged radio button, can have submenus */
#define BTNCoMode  0x07  /* same as Mode, gangs with others in same menu */


/* Detail of mouse button pressed event to distinguish among possible modes */
/* detail = (xbutton.button << 16) | (xbutton.state & 0xffff); */
#define BTNDetail(a,b) (((a) << 16) | ((b) & 0xffff))


typedef struct _ButtonLabel {
  unsigned char *label;	/* byte bitmap of label pattern */ 
  unsigned char *mask;	/* byte bitmap mask to stencil label */
  int width, height;	/* dimensions in pixels of label and mask */
  float x_ratio;	/* placement as fraction of button width */
  float y_ratio;	/* placement as fraction of button height */
  int x_offset;		/* offset from label center for positioning */
  int y_offset;		/* offset from label center for positioning */
  int bytes_per_line;	/* number of chars in bitmap line (allows padding) */
  int x_clip;		/* 0=even, <0 clip from left, >0 clip from right */
  int y_clip;		/* 0=even, <0 clip from top, >0 clip from bottom */
  int *breaks;		/* array of clip offsets for one axis */
} ButtonLabel;

/* BUTTON INFORMATION PASSED TO BUTTON CREATION ROUTINE */
typedef struct _ButtonLook {
  ButtonLabel *off_out_1;	/* label and params for off & unoccupied */
  ButtonLabel *off_out_2;	/* 2nd label allows 2 parts placed as sized */
  ButtonLabel *on_out_1;	/* label and params for on & unoccupied */
  ButtonLabel *on_out_2;	/* 2nd label allows 2 parts placed as sized */
  ButtonLabel *off_in_1;	/* label and params for off & occupied */
  ButtonLabel *off_in_2;	/* 2nd label allows 2 parts placed as sized */
  ButtonLabel *on_in_1;		/* label and params for on & occupied */
  ButtonLabel *on_in_2;		/* 2nd label allows 2 parts placed as sized */
} ButtonLook;

typedef struct _ButtonFeel {
  char *title;			/* name of button for reference */
  int nfunctions;		/* number of response options given */
  /* functional type (for each mouse button response) */
  int function[OPTION_LIMIT];	/* function identifiers */
  /* test to identify and/or verify response */
  /* (((xbutton.button<<16)|(xbutton.state&0xffff))&mask)==reference */
  int mask[OPTION_LIMIT];	/* function masks */
  int reference[OPTION_LIMIT];	/* function references */
  /* user supplied data to be returned when selected */
  int data[DATA_LIMIT];		/* data returned when selected */
} ButtonFeel;

typedef struct _ButtonSpec {
  ButtonLook *look;
  ButtonFeel *feel;
} ButtonSpec;

typedef struct _BoxGeometry {
  int panel_index;		/* index of parent panel window */
  int btn_cnt;			/* number of defined button */
  int box_cols, box_rows;	/* geometric arrangement of box */
  double parent_cols, parent_rows;	/* geometric arrangement of panel */
  double box_col_x, box_row_y;	/* panel index of first box button */
  int off_inverse, on_inverse;	/* flag for state to use inverse video */
} BoxGeometry;

typedef struct _BoxParent {
  Window wndwID;
  Display *display;
  int x, y;
  int width, height;
  int xwdth, yhght;
} BoxParent;

typedef struct _BorderPatterns {
  unsigned char *off_out; /* bitmaps of 32x32 button borders */
  unsigned char *off_in;  /* To create border patterns of buttons */
  unsigned char *on_out;
  unsigned char *on_in;
} BorderPatterns;

typedef struct _ButtonRecord {
  Display *display;		/* X server connection */
  Window wndwID;		/* window handle */
  GC gc;			/* graphics context for XPutImage() */
  unsigned int width;		/* dimensions of button */
  unsigned int height;
  XImage *image[4];		/* bitmaps to draw for each phase */
  ButtonLook *look;		/* appearance specifications */
  ButtonFeel *feel;		/* interaction specifications */
  int selected;			/* status booleans */
  int highlight;
  int occupied;
  int submenu_count[OPTION_LIMIT];	/* number of button's submenu boxes */
  int *submenu[OPTION_LIMIT][SUBMENU_LIMIT];	/* list of submenu boxes */
#ifdef ALLIANT
    int id[4];
#endif
} ButtonRecord;

typedef struct _ButtonBox {
  Display *display;		/* X server connection */
  Window wndwID;		/* window ID of box enclosing window */
  Window parentID;		/* ID of window of which wndwID is subwindow */
  int parent_width, parent_height;	/* references for resize check */
  Visual *visual;		/* X11 Visual to create bitmap XImages */
  unsigned long background;	/* window background pixel value */
  BoxGeometry *geometry;	/* button arrangement and count info */  
  int btn_cnt;			/* number of actual buttons */
  ButtonRecord *buttons;	/* array of button records */
  BorderPatterns *borders;	/* corner patterns to make button border */
  int mode_btn;			/* currently active "mode" button */
  int mode_btn_func;		/* response function of active mode */
  int co_mode_btn;		/* currently active "comode" button */
  int co_mode_func;		/* response function of active comode */
  int down_btn;			/* button currently being selected */
  int down_btn_func;		/* response function of selected button */
  int down_mouse_btn;		/* mouse button used to make selection */
  int window_count;		/* number of windows which might get events */
  Window *window_list;		/* list of their ID's */
  struct _ButtonBox *parentmenu;	/* parent if this is a submenu */
  int submenu_count;		/* number of active mode submenus */
  int cosubmenu_count;		/* number of active comode submenus */
  int co_menu_count;		/* number of co-equal menus */
  struct _ButtonBox *submenu[SUBMENU_LIMIT];	/* list of active submenus */
  struct _ButtonBox *cosubmenu[SUBMENU_LIMIT];	/* list of active cosubmenus */
  struct _ButtonBox *co_menu[SUBMENU_LIMIT];	/* list of co-equal menus */
} *ButtonBox;


/* Define application callable subroutines */
ButtonBox MakeButtonBox();
void AttachSubmenu();
void JoinMenu();
void MountButtonMenu();
void ResizeButtons();
void ResizeBox();
void SetTAEButtonLook();
int ButtonEvent();
int ButtonControl();
int PushButton();
int TouchButton();
int ReleaseButton();
int SetToggleButton();
int ButtonNumber();
int ButtonStatus();
int EnableButton();
int DisableButton();
