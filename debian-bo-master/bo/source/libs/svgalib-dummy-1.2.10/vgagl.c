#include <stdio.h>
#include "vgagl.h"

int gl_setcontextvga( int m) { return -1; }
int gl_setcontextvgavirtual( int m) { return -1; }
void gl_setcontextvirtual( int w, int h, int bpp, int bitspp, void *v) {}
GraphicsContext * gl_allocatecontext( ) { return NULL; }
void gl_setcontext( GraphicsContext * gc) {}
void gl_getcontext( GraphicsContext * gc) {}
void gl_freecontext( GraphicsContext * gc) {}
void gl_setcontextwidth( int w) {}
void gl_setcontextheight( int h) {}
void gl_setclippingwindow( int x1, int y1, int x2, int y2) {}
void gl_enableclipping( ) {}
void gl_disableclipping( ) {}
void gl_setpixel( int x, int y, int c) {}
int gl_getpixel( int x, int y) { return -1; }
void gl_hline( int x1, int y, int x2, int c) {}
void gl_fillbox( int x, int y, int w, int h, int c) {}
void gl_putboxpart( int x, int y, int w, int h, int ow, int oh, void *b,
					int ox, int oy) {}
void gl_putbox( int x, int y, int w, int h, void *b) {}
void gl_putboxmask( int x, int y, int w, int h, void *b) {}
void gl_getbox( int x, int y, int w, int h, void *b) {}
void gl_copybox( int x1, int y1, int w, int h, int x2, int y2) {}
void gl_clearscreen( int c) {}
int gl_rgbcolor( int r, int g, int b) { return -1; }
void gl_setpixelrgb( int x, int y, int r, int g, int b) {}
void gl_getpixelrgb( int x, int y, int *r, int *g, int *b) {}
void gl_setdisplaystart( int x, int y) {}
void gl_setscreenoffset( int o) {}
int gl_enablepageflipping( GraphicsContext * gc) { return 0; }
void gl_copyscreen( GraphicsContext * gc) {}
void gl_copyboxtocontext( int x1, int y1, int w, int h, GraphicsContext *
						  gc, int x2, int y2) {}
void gl_copyboxfromcontext( GraphicsContext * gc, int x1, int y1, int w,
							int h, int x2, int y2) {}
void gl_line( int x1, int y1, int x2, int y2, int c) {}
void gl_circle( int sx, int sy, int r, int c) {}
void gl_compileboxmask( int w, int h, void *_dp1, void *_dp2) {}
int gl_compiledboxmasksize( int w, int h, void *_dp1) { return 0; }
void gl_putboxmaskcompiled( int x, int y, int w, int h, void *_dp) {}
void gl_scalebox( int w1, int h1, void *_dp1, int w2, int h2, void *_dp2) {}
void gl_colorfont( int fw, int fh, int fg, void *_dp) {}
void gl_setfont( int fw, int fh, void *font) {}
void gl_setwritemode( int m) {}
void gl_write( int x, int y, char *s) {}
void gl_writen( int x, int y, int n, char *s) {}
void gl_expandfont( int fw, int fh, int fg, void *_f1, void *_f2) {}
void gl_setfontcolors( int bg, int fg) {}
void gl_getpalettecolor( int c, int *r, int *g, int *b) {}
void gl_setpalettecolor( int c, int r, int g, int b) {}
void gl_setpalettecolors( int s, int n, void *_dp) {}
void gl_getpalettecolors( int s, int n, void *_dp) {}
void gl_getpalette( void *p) {}
void gl_setpalette( void *p) {}
void gl_setrgbpalette( ) {}
