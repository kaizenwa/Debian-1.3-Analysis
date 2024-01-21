#ifndef W_SETUP_H
#define W_SETUP_H
/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985 by Supoj Sutanthavibul
 * Parts Copyright (c) 1994 by Brian V. Smith
 * Parts Copyright (c) 1991 by Paul King
 *
 * The X Consortium, and any party obtaining a copy of these files from
 * the X Consortium, directly or indirectly, is granted, free of charge, a
 * full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 * nonexclusive right and license to deal in this software and
 * documentation files (the "Software"), including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software subject to the restriction stated
 * below, and to permit persons who receive copies from any such party to
 * do so, with the only requirement being that this copyright notice remain
 * intact.
 * This license includes without limitation a license to do the foregoing
 * actions under any patents of the party supplying this software to the 
 * X Consortium.
 *
 * Restriction: The GIF encoding routine "GIFencode" in f_wrgif.c may NOT
 * be included if xfig is to be sold, due to the patent held by Unisys Corp.
 * on the LZW compression algorithm.
 */

#define		PIX_PER_INCH		1200
#define		PIX_PER_CM		450	/* ((int)(PIX_PER_INCH / 2.54
						 * + (5.0/2))) */
#define DISPLAY_PIX_PER_INCH 80

/* Portrait dimensions */
#define		DEF_CANVAS_HT_PORT	9*DISPLAY_PIX_PER_INCH
#define		DEF_CANVAS_WD_PORT	8.5*DISPLAY_PIX_PER_INCH

/* Landscape dimensions */
#define		DEF_CANVAS_HT_LAND	8.5*DISPLAY_PIX_PER_INCH
#define		DEF_CANVAS_WD_LAND	11*DISPLAY_PIX_PER_INCH

#define		DEF_RULER_WD		24

#ifndef MAX_TOPRULER_WD
#define		MAX_TOPRULER_WD		1152
#endif
#ifndef MAX_SIDERULER_HT
#define		MAX_SIDERULER_HT	900
#endif
#define		MIN_MOUSEFUN_WD		240

#define		MAXDEPTH		999

#define		MODE_SW_HT	32	/* height of a mode switch icon */
#define		MODE_SW_WD	36	/* width of a mode switch icon */

#define		DEF_SW_PER_ROW		2 /* def num of btns per row in mode panel */

#define		DEF_INTERNAL_BW		1
#define		POPUP_BW		2

extern int	TOOL_WD, TOOL_HT;
extern int	CMDPANEL_WD, CMDPANEL_HT;
extern int	MODEPANEL_WD, MODEPANEL_HT;
extern int	MODEPANEL_SPACE;
extern int	MSGFORM_WD, MSGFORM_HT;
extern int	MSGPANEL_WD;
extern int	MOUSEFUN_WD, MOUSEFUN_HT;
extern int	INDPANEL_WD;
extern int	CANVAS_WD, CANVAS_HT;
extern int	CANVAS_WD_LAND, CANVAS_HT_LAND;
extern int	CANVAS_WD_PORT, CANVAS_HT_PORT;
extern int	INTERNAL_BW;
extern int	TOPRULER_WD, TOPRULER_HT;
extern int	SIDERULER_WD, SIDERULER_HT;
extern int	SW_PER_ROW;
extern int	NUM_MODE_SW;
#endif /* W_SETUP_H */
