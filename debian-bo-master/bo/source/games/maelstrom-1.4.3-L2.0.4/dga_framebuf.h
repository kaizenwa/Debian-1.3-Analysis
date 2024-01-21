
/* X11 DGA Graphics routines for Maelstrom!  (By Sam Lantinga) */

#ifndef _dga_framebuf_h
#define _dga_framebuf_h
#if defined(linux) && defined(USE_DGA)

#include "framebuf.h"

#include <X11/Xmd.h>
#include <X11/Xutil.h>
#include <X11/extensions/xf86dga.h>
#define private priv			/* Hack, hack :) */
#include <X11/extensions/xf86vmode.h>
#undef private

#include "stack.tmpl"


/* This class allocates a DGA frame-buffer and provides high-level
   routines to blit images into it.
*/
class DGA_FrameBuf : public FrameBuf {

public:
	DGA_FrameBuf(unsigned short width, unsigned short height, int bpp);
	~DGA_FrameBuf();

	int            Alloc_Cmap(Color Cmap[]);
	int            Alloc_Private_Cmap(Color Cmap[]);
	void           RefreshArea(int x0, int y0, int width, int height);
	void           Refresh(void);
	void           Hide_Cursor(void);
	void           Show_Cursor(void);
	void           Fade(int steps);
	void           FlushEvents(void);
	int            NumEvents(void);
	void           GetEvent(XEvent *event);
	int            KeyToAscii(XEvent *event, char *buf, int buflen,
								KeySym *key);
	void           Flush(int sync);

	char *DisplayType(void) {
		return("XFree86 3.1.2E - 8-bit DGA Display");
	}

protected:
	/* The core blit routines */
	void DGA_Blit(int x0, int y0, int width, int height);
	void DGA_BlitBuf(int x0, int y0, int width, int height, char *buf);

	Display *display;		// The X11 display connection
	Window TopWin;			// The top-level Maelstrom window
	Colormap colormap;		// The current colormap
	unsigned long White;		// The white pixel

	int zoom;			// Have we zoomed the X display?
	char *vbase;			// The pointer to the video memory
	int vwidth;			// Width of video scanline
	int vpage;			// Size of video page
	int vlines;			// Scanlines per video page

	/* Resize-event queue structures */
	typedef struct {
		int x, y;
		int width, height;
	} Area;
	Stack<Area> *Reefer;
	friend int sort_areas(Area *item1, Area *item2);

	/* Chunk size in which the stack is allocated */
	const int	 ChunkSize = 32;

	/* Mouse stuff */
	double mouseX, mouseY;
	double mouse_accel;
	int showmouse;
	CIcon mouseSprite;
	unsigned char *behind_mouse;

	void DrawMouse(void) {
		unsigned char *sdata = mouseSprite.pixels;
		unsigned char *mdata = mouseSprite.mask;
		int row, col, offset;
		int m_x=(int)mouseX, m_y=(int)mouseY;
		int m_width = mouseSprite.width;
		int m_height = mouseSprite.height;
		unsigned char new_area[m_width*m_height];

		/* Save what's behind the mouse */
		for ( row=0; row<m_height; ++row ) {
			memcpy(&behind_mouse[row*m_width], 
				&shared_mem[(m_y+row)*WIDTH+m_x], m_width);
		}
		memcpy(new_area, behind_mouse, m_width*m_height);

		/* Draw the mouse in the new area */
		for ( row=0; row<m_height; ++row ) {
			for ( col=0; col<m_width; ++col, ++sdata ) {
				offset = ((row*m_width)+col);
				if ((mdata[offset/8]>>(7-(offset%8))) & 0x01) {
					new_area[offset] = Pixel_colors[*sdata];
				}
			}
		}
		DGA_BlitBuf(m_x, m_y, m_width, m_height, (char *)new_area);
	}
	void EraseMouse(void) {
		DGA_BlitBuf((int)mouseX, (int)mouseY, mouseSprite.width,
				mouseSprite.height, (char *)behind_mouse);
	}
	void MoveMouse(int x, int y, XEvent *event) {
		if ( showmouse )
			EraseMouse();
		mouseX += (mouse_accel*x);
		if ( mouseX < 0 )
			mouseX = 0;
		if ( mouseX > (WIDTH-mouseSprite.width) )
			mouseX = (WIDTH-mouseSprite.width);
		mouseY += (mouse_accel*y);
		if ( mouseY < 0 )
			mouseY = 0;
		if ( mouseY > (HEIGHT-mouseSprite.height) )
			mouseY = (HEIGHT-mouseSprite.height);
		if ( showmouse )
			DrawMouse();
	}

};
#endif /* Linux && use DGA */
#endif /* _dga_framebuf_h */
