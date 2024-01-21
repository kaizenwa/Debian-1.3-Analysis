/* Minimal text LDEF, no fancy features, stripped down from a version by Adam Winer (?).
   Copyright (C) 1993, 1994 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* Spacing parameters. */

#define kLeftOffset	2
#define kTopOffset	0

pascal void
main(short listmsg, Boolean selected, Rect *listrectptr, Cell listcell,
	 short listdataoffset, short listdatalength, ListHandle listhandle)
{
	FontInfo fontInfo;
	ListPtr listptr;
	SignedByte hStateList, hStateCells;
	Ptr celldata;
	short leftdraw, topdraw, wayleft;
	
	/* Lock and dereference the list and cell data handles. */	
	hStateList = HGetState((Handle) listhandle);
	HLock((Handle) listhandle);
	listptr = *listhandle;
	hStateCells = HGetState(listptr->cells);
	HLock(listptr->cells);
	celldata = *(listptr->cells);
	/* Decipher the desired action on the list. */
	switch (listmsg) {
	  case lInitMsg:
	  	/* We don't need any initialization. */
	  	break;
	  case lDrawMsg:
		EraseRect(listrectptr);
	  	if (listdatalength > 0) {
	  		/* Determine starting point for drawing. */
	  		wayleft = listcell.h * listptr->cellSize.h + listptr->rView.left;
	  		leftdraw = wayleft + listptr->indent.h + kLeftOffset;
	  		topdraw = listrectptr->top + listptr->indent.v + kTopOffset;
			GetFontInfo(&fontInfo);
			MoveTo(leftdraw, topdraw + fontInfo.ascent);
			TextFace(0);
			DrawText(celldata, listdataoffset, listdatalength);
	  	}
		if (!selected) break;
		/* Fall through to draw selected cell in hilite color. */
	  case lHiliteMsg:
	  	/* Do hilite color. */
	  	BitClr(&HiliteMode, pHiliteBit);
	  	InvertRect(listrectptr);
	  	break;
	  case lCloseMsg:
	  	break;
	}
	HSetState(listptr->cells, hStateCells);
	HSetState((Handle) listhandle, hStateList);
}
