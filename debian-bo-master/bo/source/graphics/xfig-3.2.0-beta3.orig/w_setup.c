/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985 by Supoj Sutanthavibul
 * Parts Copyright (c) 1991 by Paul King
 * Parts Copyright (c) 1994 by Brian V. Smith
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

#include "fig.h"
#include "resources.h"
#include "w_setup.h"
#include "w_util.h"

#define	NUM_DRAW_SW 16 /* kludge - shouldn't have to edit this by hand */

int		TOOL_WD, TOOL_HT;
int		CMDPANEL_WD, CMDPANEL_HT = 22;
int		MODEPANEL_WD, MODEPANEL_HT;
int		MODEPANEL_SPACE;
int		MSGFORM_WD, MSGFORM_HT = 18;
int		MSGPANEL_WD = 600;
int		MOUSEFUN_WD, MOUSEFUN_HT;
int		INDPANEL_WD;
int		CANVAS_WD, CANVAS_HT;
int		CANVAS_WD_LAND, CANVAS_HT_LAND;
int		CANVAS_WD_PORT, CANVAS_HT_PORT;
int		INTERNAL_BW;
int		TOPRULER_WD, TOPRULER_HT;
int		SIDERULER_WD, SIDERULER_HT;
int		SW_PER_ROW, SW_PER_COL;

setup_sizes(new_canv_wd, new_canv_ht)
    int		    new_canv_wd, new_canv_ht;
{
    int		    NUM_CMD_SW;

    /*
     * make the width of the mousefun panel about 1/3 of the size of the
     * canvas width and the cmdpanel the remaining width. Be sure to set it
     * up so that cmdpanel buttons can be allocated a size which divides
     * evenly into the remaining space.
     */
    CANVAS_WD = new_canv_wd;
    if (CANVAS_WD < 10)
	CANVAS_WD = 10;
    CANVAS_HT = new_canv_ht;
    if (CANVAS_HT < 10)
	CANVAS_HT = 10;

    SIDERULER_WD = RULER_WD + 8;	/* allow for 100's numbers */
    TOPRULER_HT = RULER_WD;
    TOPRULER_WD = CANVAS_WD;
    SIDERULER_HT = CANVAS_HT;
    if (TOPRULER_WD > MAX_TOPRULER_WD)
	TOPRULER_WD = MAX_TOPRULER_WD;
    if (SIDERULER_HT > MAX_SIDERULER_HT)
	SIDERULER_HT = MAX_SIDERULER_HT;

    MODEPANEL_WD = (MODE_SW_WD + INTERNAL_BW) * SW_PER_ROW + INTERNAL_BW;
    NUM_CMD_SW = num_cmd_sw();	/* kludge - NUM_CMD_SW local to w_cmdpanel.c */
    CMDPANEL_WD = (((2 * CANVAS_WD) / 3 + MODEPANEL_WD +
		    SIDERULER_WD) / NUM_CMD_SW) * NUM_CMD_SW;
    MOUSEFUN_WD = (MODEPANEL_WD + CANVAS_WD + SIDERULER_WD - CMDPANEL_WD);
    while (MOUSEFUN_WD < MIN_MOUSEFUN_WD) {
	MOUSEFUN_WD += NUM_CMD_SW;
	CMDPANEL_WD -= NUM_CMD_SW;
    }
    if (CMDPANEL_WD < 5 * NUM_CMD_SW)
	CMDPANEL_WD = 5 * NUM_CMD_SW;
    MSGFORM_WD = CMDPANEL_WD;
    INDPANEL_WD = MODEPANEL_WD + CANVAS_WD + SIDERULER_WD + INTERNAL_BW*2;
    MODEPANEL_SPACE = CANVAS_HT + RULER_WD - (MODE_SW_HT + INTERNAL_BW) *
	(ceil((double)NUM_DRAW_SW/SW_PER_ROW) +
	ceil((double)(NUM_MODE_SW-NUM_DRAW_SW)/SW_PER_ROW));
    if (MODEPANEL_SPACE < 2)
	MODEPANEL_SPACE = 2;
}
