// lcx11.h   Header file for lcx11.cxx - part of LinCity.
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.

#define TRUE 1
#define FALSE 0
#define MOUSE_LEFTBUTTON Button1
#define MOUSE_RIGHTBUTTON Button3

#define WINWIDTH 640    /* default window width */
#define WINHEIGHT 480   /* default window height */
#define FATAL   -1
#define WARNING -2


/* Type Definitions */
typedef struct _disp
{
        Window  win;
        Display *dpy;
        int     screen;
        Window  root;
        char    *dname;
               
        long    bg;  /* colors */
        XColor  bg_xcolor;
        GC      pixcolour_gc[256];
        Atom    kill_atom, protocol_atom;
        Colormap cmap;
} disp;                                                                                

extern int             winX, winY,mouse_button,x_key_value,cs_mouse_shifted;
extern unsigned int    winW, winH;
extern disp            display;

extern char    *bg_color;
extern char    pixmap[];
extern char    myfont[256*8];
extern int     verbose;
extern int    pix_double;
//extern int     stay_in_front;  /* Try to stay in clear area of the screen. */
extern int     text_bg;
extern int     text_fg;
extern int     x_key_value;
extern int     borderx,bordery;
extern long unsigned int  colour_table[256];
extern int     xclip_x1,xclip_y1,xclip_x2,xclip_y2,clipping_flag;
extern unsigned char *open_font;
extern int open_font_height;
extern int suppress_next_expose;
extern int no_init_help;
extern char colour_pal_file[],fontfile[];
extern int cs_mouse_x,cs_mouse_y;
extern float gamma_correct_red,gamma_correct_green,gamma_correct_blue;

void parse_xargs(int,char **,char **);
void do_setcustompalette(XColor *);
void Create_Window(char *);
void HandleError(char *,int);
void HandleEvent(XEvent *);
void refresh_screen(int,int,int,int);
void my_x_putchar(int,int,int);
void open_x_putchar(int,int,int);
void do_call_event(int);
void call_event(void);
void call_wait_event(void);

#ifdef USE_PIXMAPS
extern Pixmap icon_pixmap[];
extern struct TYPE main_types[];
#endif
