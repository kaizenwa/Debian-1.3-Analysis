/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */

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



#define ID_WINDOW   256
#define ID_POINTER  280


#define ID_BEGIN  257
#define ID_END 258
#define ID_PAGEDOWN 259
#define ID_PAGEUP  260
#define ID_DOWN 261
#define ID_LEFT 262
#define ID_RIGHT 263
#define ID_UP 264
#define ID_DELETE 265
#define ID_ESCAPE 266
#define ID_CTRLC 268




