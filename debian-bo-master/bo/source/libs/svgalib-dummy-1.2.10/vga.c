#include <unistd.h>
#include "vga.h"

void vga_setchipset(int c) {}
void vga_setchipsetandfeatures(int c, int par1, int par2) {}
void takevtcontrol(void) {}
void vga_setpage(int p) {}
void vga_setreadpage(int p) {}
void vga_setwritepage(int p) {}
void vga_safety_fork(void (*shutdown_routine) (void)) {}
int vga_setmode(int mode) { return -1; }
void vga_gettextfont(void *font) {}
void vga_puttextfont(void *font) {}
void vga_gettextmoderegs(void *regs) {}
void vga_settextmoderegs(void *regs) {}
int vga_getcurrentmode(void) {}
int vga_getcurrentchipset(void) {}
void vga_disabledriverreport(void) {}
vga_modeinfo *vga_getmodeinfo(int mode) { return NULL; }
int vga_hasmode(int mode) { return 0; }
int vga_lastmodenumber(void) { return 0; }
int vga_setcolor(int color) { return 0; }
int vga_screenoff(void) { return 0; }
int vga_screenon(void) { return 0; }
int vga_getxdim(void) { return 0; }
int vga_getydim(void) { return 0; }
int vga_getcolors(void) { return 0; }
int vga_white(void) { return 0; }
int vga_claimvideomemory(int m) { return 0; }
int vga_setmodeX(void) { return 0; }
int vga_getch(void) { return -1; }
int vga_flip(void) { return 0; }
int vga_setflipchar(int c) { return 0; }
void vga_setlogicalwidth(int w) {}
void vga_setdisplaystart(int a) {}
void vga_bitblt(int srcaddr, int destaddr, int w, int h, int pitch) {}
void vga_imageblt(void *srcaddr, int destaddr, int w, int h, int pitch) {}
void vga_fillblt(int destaddr, int w, int h, int pitch, int c) {}
void vga_hlinelistblt(int ymin, int n, int *xmin, int *xmax, int pitch, int c)
{}
void vga_blitwait(void) {}
int vga_ext_set(unsigned what,...) { return 0; }
int vga_getmousetype(void) { return 0; }
int vga_getmonitortype(void) { return 0; }
void vga_setmousesupport(int s) {}
void vga_lockvc(void) {}
void vga_unlockvc(void) {}
void vga_runinbackground(int stat) {}
int vga_oktowrite(void) { return 0; }
int vga_clear(void) { return 0; }
int vga_setpalette(int index, int red, int green, int blue) { return 0; }
int vga_getpalette(int index, int *red, int *green, int *blue) { return 0; }
int vga_setpalvec(int start, int num, int *pal) { return 0; }
int vga_getpalvec(int start, int num, int *pal) { return 0; }
int vga_drawpixel(int x, int y) { return 0; }
int vga_getpixel(int x, int y) { return 0; }
int vga_drawline(int x1, int y1, int x2, int y2) { return 0; }
int vga_drawscanline(int line, unsigned char *colors) { return 0; }
int vga_drawscansegment(unsigned char *colors, int x, int y, int length)
{ return 0; }
int vga_getscansegment(unsigned char *colors, int x, int y, int length)
{ return 0; }
int vga_dumpregs(void) { return 0; }
int vga_getmodenumber(char *m) { return -1; }
int vga_getdefaultmode(void) { return -1; }
char *vga_getmodename(int m) { return ""; }
void vga_waitretrace(void) {}
unsigned char *vga_getgraphmem(void) { return NULL; }
int vga_setlinearaddressing(void) { return -1; }
int vga_getkey(void) { return 0; }
int vga_waitevent(int which, fd_set * in, fd_set * out, fd_set * except,
				  struct timeval *timeout) { return 0; }
void vga_reserved19(void) {}
void vga_reserved20(void) {}
void vga_reserved21(void) {}
void vga_reserved22(void) {}
void vga_copytoplanar256(unsigned char *virtual, int pitch, int voffset,
						 int vpitch, int w, int h) {}
void vga_copytoplanar16(unsigned char *virtual, int pitch, int voffset,
						int vpitch, int w, int h) {}
void vga_copytoplane(unsigned char *virtual, int pitch, int voffset,
					 int vpitch, int w, int h, int plane) {}
int vga_setrgbcolor(int r, int g, int b) { return 0; }
int vga_setegacolor(int c) { return 0; }
int vga_accel(unsigned operation,...) { return -1; }

unsigned char *graph_mem;

int vga_init(void)
{
	/* reset uid and return with error */
	if (geteuid() != getuid()) seteuid(getuid());
	if (getegid() != getgid()) setegid(getgid());
	return -1;
}

