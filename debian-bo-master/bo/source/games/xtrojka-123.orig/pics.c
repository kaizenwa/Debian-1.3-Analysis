/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	27.xi.1995
 *	modified:
 *
 *	This is low-level picture manager
 */


#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>

#ifdef XPM
#include <X11/xpm.h>
#endif

#include "xtrojka.h"
#include "tr_core.h"
#include "pics.h"
#include "window.h"
#include "debug.h"
#include "pictures.h"

extern Widget trojkamenu_but, optionsmenu_but, speedmenu_but;
extern Widget stat_shell;

extern flag is_color;

extern Widget main_screen;
extern Widget stat_screen;
extern Widget slist_screen;
extern Widget leftpillar;
extern Widget rightpillar;	
extern Widget screen;

extern Colormap the_colormap;
extern int	the_depth;

Pixmap app_iconPic;
Pixmap stat_iconPic;
Pixmap slist_iconPic;
Pixmap blockPic[tc_blocks + 1];
Pixmap patPic[tc_blocks];
Pixmap newPic;
Pixmap titlePic;
Pixmap leftpillarPic;
Pixmap rightpillarPic;

#ifdef XPM
Pic blockPicXpm[tc_blocks];
Pic leftpillarPicXpm;
Pic rightpillarPicXpm;
Pic stat_iconPicXpm;
Pic app_iconPicXpm;
Pic slist_iconPicXpm;
#endif


void init_bitmaps(void)
{
	Pixel black_pix, white_pix;

	DEBUG("pics.c", "make_bitmaps")

	black_pix = BlackPixel(XtDisplay(main_screen), 0);
	white_pix = WhitePixel(XtDisplay(main_screen), 0);

	
	app_iconPic = MkBitmap(app_icon_bits, app_icon_width, app_icon_height);
	stat_iconPic = MkBitmap(stat_icon_bits, stat_icon_width, stat_icon_height);
	slist_iconPic = MkBitmap(slist_icon_bits, slist_icon_width, slist_icon_height);

	if(is_color) {
		/* read the pillars */

		/* left pillar */
		leftpillarPic = MkBitPixmap(leftpillar_bits, leftpillar_width,
			leftpillar_height, black_pix, white_pix);

		rightpillarPic = MkBitPixmap(rightpillar_bits, rightpillar_width,
			rightpillar_height, black_pix, white_pix);

		/* right pillar */

		newPic = MkBitPixmap(new_bits, new_width,
			new_height, black_pix, white_pix);
	
		titlePic = MkBitPixmap(title_bits, title_width,
			title_height, black_pix, white_pix);
#ifdef XPM
		MkXpmPixmap(block1_xpm, blockPicXpm[0]);
		MkXpmPixmap(block2_xpm, blockPicXpm[1]);
		MkXpmPixmap(block3_xpm, blockPicXpm[2]);
		MkXpmPixmap(block4_xpm, blockPicXpm[3]);
		MkXpmPixmap(block5_xpm, blockPicXpm[4]);
		MkXpmPixmap(leftpillar_xpm, leftpillarPicXpm);
		MkXpmPixmap(rightpillar_xpm, rightpillarPicXpm);

		MkXpmPixmap(app_icon_xpm, app_iconPicXpm);
		MkXpmPixmap(slist_icon_xpm,slist_iconPicXpm);
		MkXpmPixmap(stat_icon_xpm, stat_iconPicXpm);
#endif

	} else {

	/* read the blocks */
		/* block 0 */
		blockPic[0] = MkBitmap(block0_bits, 
					block0_width, block0_height);

		/* block 1 */
		blockPic[1] = MkBitmap(block1_bits, 
					block1_width, block1_height);

		/* block 2 */
		blockPic[2] = MkBitmap(block2_bits, 
					block2_width, block2_height);

		/* block 3 */
		blockPic[3] = MkBitmap(block3_bits, 
					block3_width, block3_height);

		/* block 4 */
		blockPic[4] = MkBitmap(block4_bits, 
					block4_width, block4_height);

		/* block 5 */
		blockPic[5] = MkBitmap(block5_bits, 
					block5_width, block5_height);
	/* read the patterns */
		/* pattern 1 */
		patPic[0] = MkBitmap(pat1_bits, 
					pat1_width, pat1_height);

		/* pattern 2 */
		patPic[1] = MkBitmap(pat2_bits, 
					pat2_width, pat2_height);

		/* pattern 3 */
		patPic[2] = MkBitmap(pat3_bits, 
					pat3_width, pat3_height);

		/* pattern 4 */
		patPic[3] = MkBitmap(pat4_bits, 
					pat4_width, pat4_height);

		/* pattern 5 */
		patPic[4] = MkBitmap(pat5_bits, 
					pat5_width, pat5_height);

	/* read the pillars */

		/* left pillar */
		leftpillarPic = MkBitmap(leftpillar_bits, 
					leftpillar_width, leftpillar_height);

		/* right pillar */
		rightpillarPic = MkBitmap(rightpillar_bits, 
					rightpillar_width, rightpillar_height);

		/* "new" picture */
		newPic = MkBitmap(new_bits, new_width, new_height);

		/* the title picture */
		titlePic = MkBitmap(title_bits, title_width, title_height);
	}
}


void set_icons()
{
	DEBUG("pics.c", "set_icons")

#ifdef XPM
	XtVaSetValues(main_screen, XtNiconPixmap, app_iconPicXpm.pic, NULL);
	XtVaSetValues(stat_screen, XtNiconPixmap, stat_iconPicXpm.pic, NULL);
	XtVaSetValues(slist_screen, XtNiconPixmap, slist_iconPicXpm.pic, NULL);
#else
	XtVaSetValues(main_screen, XtNiconPixmap, app_iconPic, NULL);
	XtVaSetValues(stat_screen, XtNiconPixmap, stat_iconPic, NULL);
	XtVaSetValues(slist_screen, XtNiconPixmap, slist_iconPic, NULL);

#endif
}

