/*                                   */
/* xemeraldia   ---- init-graphics.c */
/*                                   */

#include "graphics.h"
#include "bitmaps.h"
#include <X11/Xlib.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>
#include <X11/Shell.h>
#include <X11/Xaw/AsciiText.h>

static void  createBWPixmaps (), createColoredPixmaps ();
static void  createGCs (), createCrushAnimePixmaps ();

Widget  board_w, nextItem_w, quit, start, score_disp, level_disp;
Widget  score_frame, score_text, high_sc_w;
GC      draw_gc, delete_gc;
Pixmap  block[BLOCK_VARIETY * 2 + 1], crush[CRUSH_ANIME_FRAMES];
Pixmap  board_pix, star;
int     black, white;
int     colored;


void  initXlib (w)
     Widget w;
{
  int     depth;
  Font    font;
  Visual *vi;

  black = BlackPixel (XtDisplay (w), DefaultScreen (XtDisplay (w)));
  white = WhitePixel (XtDisplay (w), DefaultScreen (XtDisplay (w)));

  depth = DefaultDepthOfScreen (XtScreen (w));
  vi = DefaultVisual (XtDisplay (w), DefaultScreen (XtDisplay (w)));
  colored = ((depth != 1) && (vi->class != GrayScale));

  board_pix = XCreatePixmap (XtDisplay (w), RootWindowOfScreen (XtScreen (w)),
			     WIN_WIDTH, WIN_HEIGHT, depth);
  createGCs (w);
  createCrushAnimePixmaps (w, depth);
  if (colored)
    createColoredPixmaps (w, depth);
  else
    createBWPixmaps (w, depth);
  clearNextItem ();
  clearScreen (w);

  font = XLoadFont (XtDisplay (w),
		    "-*-new century schoolbook-*-*-*-*-20-*-*-*-*-*-*-*");
  XSetFont (XtDisplay (w), draw_gc, font);
}


void  initXt (w)
      Widget  w;
{
  Widget  bigBox, buttonBox, dispBox, nextBox, Score, Level, Next;

  String  trans =
    "!Shift<Btn1Down>: CCRotation() \n\
     !<Btn1Down>:      MoveLeft() \n\
     !Shift<Btn3Down>: Rotation() \n\
     !<Btn3Down>:      MoveRight() \n\
     !Shift<Btn2Down>: MoveDown() \n\
     <KeyPress>h:      MoveLeft() \n\
     <KeyPress>l:      MoveRight() \n\
     <KeyPress>k:      Rotation() \n\
     <KeyPress>j:      CCRotation() \n\
     <KeyPress>space:  MoveDown() \n\
     <KeyPress>u:      MoveLeft() \n\
     <KeyPress>o:      MoveRight() \n\
     <KeyPress>i:      Rotation() \n\
     <KeyPress>q:      Quit() \n\
     <KeyPress>p:      StartGame() \n\
     <KeyPress>s:      StartGame() \n\
     <KeyPress>Left:   MoveLeft() \n\
     <KeyPress>Down:   Rotation() \n\
     <KeyPress>Begin:  Rotation() \n\
     <KeyPress>Up:     CCRotation() \n\
     <KeyPress>Right:  MoveRight()";
  String  board_w_trans =
    "<Expose>:    RedrawBoard()";
  String  nextItem_w_trans =
    "<Expose>:    RedrawNextItem()";

  bigBox = XtVaCreateManagedWidget ("bigBox", formWidgetClass, w,
				    XtNtranslations,
				    XtParseTranslationTable (trans), NULL);
  board_w = XtVaCreateManagedWidget ("board", widgetClass, bigBox,
				    XtNtranslations,
				    XtParseTranslationTable (board_w_trans),
				    XtNwidth, WIN_WIDTH,
				    XtNheight, WIN_HEIGHT, NULL);
  nextBox = XtVaCreateManagedWidget ("nextBox", formWidgetClass, bigBox,
				     XtNfromHoriz, board_w, NULL);
  Next = XtVaCreateManagedWidget ("NEXT", labelWidgetClass, nextBox,
				   XtNwidth, BLOCK_WIDTH * 3, NULL);
  nextItem_w = XtVaCreateManagedWidget ("nextItem", widgetClass, nextBox,
				   XtNtranslations,
				   XtParseTranslationTable (nextItem_w_trans), 
				   XtNwidth, BLOCK_WIDTH * 3,
				   XtNheight, BLOCK_HEIGHT * 3,
				   XtNfromVert, Next, NULL);
  dispBox = XtVaCreateManagedWidget ("dispBox", formWidgetClass, bigBox,
				     XtNfromHoriz, board_w,
				     XtNfromVert, nextBox, NULL);
  buttonBox = XtVaCreateManagedWidget ("buttonBox", formWidgetClass, bigBox,
				       XtNfromHoriz, board_w,
				       XtNfromVert, dispBox, NULL);
  start = XtVaCreateManagedWidget ("Start", commandWidgetClass, buttonBox,
				   XtNwidth, BLOCK_WIDTH * 3, NULL);
  quit = XtVaCreateManagedWidget ("Quit", commandWidgetClass, buttonBox,
				  XtNwidth, BLOCK_WIDTH * 3,
				  XtNfromVert, start, NULL);
  Score = XtVaCreateManagedWidget ("SCORE", labelWidgetClass, dispBox,
				   XtNwidth, BLOCK_WIDTH * 3, NULL);
  score_disp = XtVaCreateManagedWidget ("Score", labelWidgetClass, dispBox,
					XtNwidth, BLOCK_WIDTH * 3,
					XtNfromVert, Score,
					XtNlabel, "      0", NULL);
  Level = XtVaCreateManagedWidget ("LEVEL", labelWidgetClass, dispBox,
				   XtNwidth, BLOCK_WIDTH * 3,
				   XtNfromVert, score_disp, NULL);
  level_disp = XtVaCreateManagedWidget ("Level", labelWidgetClass, dispBox,
					XtNwidth, BLOCK_WIDTH * 3,
					XtNfromVert, Level,
					XtNlabel, "      0", NULL);
  if (app_data.usescorefile)
    {
      high_sc_w = XtVaCreateManagedWidget ("Scores", commandWidgetClass,
					    buttonBox,
					    XtNwidth, BLOCK_WIDTH * 3,
					    XtNfromVert, quit, NULL);
      score_frame = XtVaCreatePopupShell ("ScoreFrame", 
                                          transientShellWidgetClass, w,
                                          XtNtransientFor, (XtArgVal) w,
                                          XtNtitle, "xemeraldia's high-scores",
                                          NULL);
      score_text  = XtVaCreateManagedWidget ("ScoreText",asciiTextWidgetClass, 
					     score_frame, NULL);
    }
}


static void  createGCs (w)
     Widget  w;
{
  XGCValues     values;

  values.foreground  = white;
  values.background  = black;
  draw_gc = XCreateGC (XtDisplay (w), RootWindowOfScreen (XtScreen (w)),
		       GCForeground | GCBackground, &values);
  values.foreground  = black;
  values.background  = black;
  delete_gc = XCreateGC (XtDisplay (w), board_pix, 
			 GCForeground | GCBackground, &values);
}


static void  createBWPixmaps (w, depth)
     Widget  w;
     int     depth;
{
  int   i;
  char *block_bits[BLOCK_VARIETY * 2 + 1];

  star = XCreatePixmapFromBitmapData (XtDisplay (w),
				      RootWindowOfScreen (XtScreen (w)),
				      star_bits,
				      BLOCK_WIDTH, BLOCK_HEIGHT,
				      white, black, depth); 
  block_bits[1] = block1_bits;
  block_bits[2] = block2_bits;
  block_bits[3] = block3_bits;
  block_bits[4] = block4_bits;
  block_bits[5] = block5_bits;
  block_bits[6] = block6_bits;
  block_bits[7] = block1cr_bits;
  block_bits[8] = block2cr_bits;
  block_bits[9] = block3cr_bits;
  block_bits[10] = block4cr_bits;
  block_bits[11] = block5cr_bits;
  block_bits[12] = block6cr_bits;
  for (i = 1; i <= BLOCK_VARIETY * 2; i++)
    {
      block[i] = XCreatePixmapFromBitmapData (XtDisplay (w),
					  RootWindowOfScreen (XtScreen (w)),
					  block_bits[i],
					  BLOCK_WIDTH, BLOCK_HEIGHT,
					  white, black, depth); 
    }
}


static void  createColoredPixmaps (w, depth)
     Widget  w;
     int     depth;
{
  int   i;
  Pixel block_pixel[BLOCK_VARIETY + 1];

  star = XCreatePixmapFromBitmapData (XtDisplay (w),
				      RootWindowOfScreen (XtScreen (w)),
				      star_bits,
				      BLOCK_WIDTH, BLOCK_HEIGHT,
				      app_data.starpixel, black, depth); 
  block_pixel[1] = app_data.block1pixel;
  block_pixel[2] = app_data.block2pixel;
  block_pixel[3] = app_data.block3pixel;
  block_pixel[4] = app_data.block4pixel;
  block_pixel[5] = app_data.block5pixel;
  block_pixel[6] = app_data.block6pixel;
  for (i = 1; i <= BLOCK_VARIETY; i++)
    block[i] = XCreatePixmapFromBitmapData (XtDisplay (w),
					  RootWindowOfScreen (XtScreen (w)),
					  colorblock_bits,
					  BLOCK_WIDTH, BLOCK_HEIGHT,
					  block_pixel[i], black, depth);
  for (i = BLOCK_VARIETY + 1; i <= BLOCK_VARIETY * 2; i++)
    block[i] = XCreatePixmapFromBitmapData (XtDisplay (w),
					  RootWindowOfScreen (XtScreen (w)),
					  colorblockcr_bits,
					  BLOCK_WIDTH, BLOCK_HEIGHT,
					  block_pixel[i - BLOCK_VARIETY],
					  black, depth); 
}


static void  createCrushAnimePixmaps (w, depth)
     Widget  w;
     int     depth;
{
  int   i;
  unsigned char *crush_bits[CRUSH_ANIME_FRAMES];

  crush_bits[0] = crush0_bits;
  crush_bits[1] = crush1_bits;
  crush_bits[2] = crush2_bits;
  crush_bits[3] = crush3_bits;
  crush_bits[4] = crush4_bits;
  for (i = 0; i < CRUSH_ANIME_FRAMES; i++)
    crush[i] = XCreatePixmapFromBitmapData (XtDisplay (w),
					    RootWindowOfScreen (XtScreen (w)),
					    crush_bits[i],
					    BLOCK_WIDTH, BLOCK_HEIGHT,
					    white, black, depth);
}
