/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */


#define STR_NEXT   "Next"
#define STR_PREV   "Prior"
#define STR_UP     "Up"
#define STR_DOWN   "Down"
#define STR_RIGHT  "Right"
#define STR_LEFT   "Left"
#define STR_HOME   "Home"
#define STR_END    "End"
#define STR_ESC    "Escape"
#define STR_RET    "Return"
#define STR_TAB    "Tab"
#define STR_BS     "BackSpace"



void vgaopen(void);
void vgagraph(void);
void vgaclose(void);
unsigned char vgagetchar(int);
void (*vgasetclip)(int, int, int, int);
void (*vgaupdate)(int, int, int, int);
void (*vgadrawrect)(int, int, int, int, int);
void (*vgadrawrector)(int, int, int, int, int);
void (*vgadrawrectbg)(int, int, int, int, int);
void (*vgacopybitmapgs)(int, int, int, int, void*);
void (*vgacopybitmapbw)(int, int, int, int, void*);
void vgascreen(int);
void vgadrawstatus(char, int, int);
void vgaupdatestatus(void);
int  vgasetstatuslines(int);





