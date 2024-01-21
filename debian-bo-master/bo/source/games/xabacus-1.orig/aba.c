#include "main.h"
#include <stdio.h>

#define DrawArc(x,y,w,h)	XFillArc(display, base, gc[BEADS],x,y,w,h,0,360*64)

/*----------------------------------------------------------------------
 * initAbacus, initializes the bits in the column array to represent
 *	the resting positions of the beads: 2 bead on top deck pos 1 & 2,
 *	pos 2 empty; 5 beads lower deck pos 4-8, pos 3 empty
 *
 *	=============	Row Position (index 'r' )
 *		O			0
 *		O			1			Upper Deck
 *		|			2
 *	=============
 *		|			3
 *		O			4
 *		O			5			Lower Deck
 *		O			6
 *		O			7
 *		O			8
 *	=============
 *		passed, returns: nothing
 */
initAbacus()
{
int i;
#ifdef DEBUG
	(void)fprintf(stderr," DEBUG initAbacus():\n");
#endif
	/* 502d=111110011 (2 bead on top deck pos 1 & 2; 5 beads lower deck
	 * pos 4-8, pos 2 & 3 are empty initially) */
	for(i=0; i<MAXCOLS; i++) column[i]=499;

}/* init abacus */

/*----------------------------------------------------------------------
 * drawAbacus, calls the necessary routines to draw the abacus;
 *	for handling exposures.
 *
 *      passed, returns: nothing
 */

drawAbacus()
{
unsigned int i;
#ifdef DEBUG
	(void)fprintf(stderr," DEBUG drawAbacus():\n");
#endif
	/* since the beads are XOR'd it causes problems if the window isn't
	 * cleared*/
	XClearWindow( display, base);

	drawRails();
	drawFrame();
	drawMidFrame();

	for(i=0; i<ncols; i++) drawColumn(i);

}/* draw*/

animateBead(r,c)
unsigned int r,c;
{
int i;

	if(r<3){	/* selection in upper deck*/
    	/* find empty row in upperdeck; pos 0,1,2*/
    	for(i=0; i<3; i++) if(!RowOccupied(i,column[c])) break;

		if(i<r)		/* if empty row is above cur bead...*/
			moveBeadUp(r,c);
		else
			moveBeadDn(r,c);
	}
	else{		/* selection in upper deck*/

        /* find empty row in lowerdeck; pos 3-8 */
        for(i=3; i<9; i++) if(!RowOccupied(i,column[c])) break;

        if(i<r)     /* if empty row is above cur bead...*/
            moveBeadUp(r,c);
        else
            moveBeadDn(r,c);

	}
}/* animate bead*/


moveBeadUp(r,c)
unsigned int r,c;
{

	/* keep looking for an empty position directly above*/
	if(RowOccupied(r-1,column[c])) moveBeadUp(r-1,c);

    undrawBead(r,c);
    column[c]=column[c]-(1<<r); /* row 'r' is now empty */
    drawBead(r-1,c);
    column[c]=column[c]+((unsigned)1<<((r-1))); /* row 'r-1' now is occupied*/

	return;
}/* move bead up*/

moveBeadDn(r,c)
unsigned int r,c;
{

	/* keep looking for an empty position directly below*/
    if(RowOccupied(r+1,column[c])) moveBeadDn(r+1,c);

    undrawBead(r,c);
    column[c]=column[c]-(1<<r); /* row 'r' is now empty */
    drawBead(r+1,c);
    column[c]=column[c]+((unsigned)1<<((r+1))); /* row 'r+1' now is occupied*/

}/* move bead down*/

/*----------------------------------------------------------------------
 * drawColumn, calls the routines necessary to re-draw 1 complete column
 *
 *		passed: an index (0 - ncols) representing colimn to be drawn
 *		returns: nothing
 */
drawColumn(col)
unsigned int col;
{
unsigned int beadnum;

	for(beadnum=0; beadnum<9; beadnum++) 
		if(RowOccupied(beadnum,column[col])) drawBead(beadnum, col);

}/* draw column*/


/*----------------------------------------------------------------------
 * drawBead, draws 1 bead
 *
 *		passed: an index representing the column number the bead is in
 *				an index representing the row of the bead is in
 *				rows numbered 0 to 9 (7 beads + 2 blanks)
 */
drawBead(beadnum,col)
unsigned int col,beadnum;
{

	if(beadnum<3)	/* drawing beads on top deck*/
	DrawArc( framewidth+colgap+(col*(beadwidth+colgap)), 
		framewidth+(beadnum*(beadheight+rowgap)), beadwidth, beadheight);
	else			/* drawing beads on bot deck, account for middle frame*/
		DrawArc( framewidth+colgap+(col*(beadwidth+colgap)), 
		(2*framewidth)+(beadnum*(beadheight+rowgap)), 
		beadwidth, beadheight);

	XFlush(display);

}

/*----------------------------------------------------------------------
 * undrawBead, undraws a bead at given location (The beads are drawn using
 *	the XOR function so this is possible.
 */
undrawBead(beadnum,col)
unsigned int col,beadnum;
{

	drawBead(beadnum,col);
}

/*----------------------------------------------------------------------
 * drawFrame, drawMidFrame, drawRails, draw the rest of the abacus
 *	it is advisable to call drawRails before the ~Frame routines
 */
drawMidFrame()
{

	XDrawLine(display, base, gc[FRAME],0,midframey+(ROWGAP<<1),
			width,midframey+(ROWGAP<<1));
	XFlush(display);

}/* draw mid frame*/

drawFrame()
{

	XDrawRectangle(display, base, gc[FRAME], framewidth/2,framewidth/2, 
		width-(framewidth),height-(framewidth));
	XFlush(display);

}/* draw frame*/

drawRails()
{
int i;

	for(i=0; i<ncols; i++)
		XDrawLine(display, base, gc[RAILS],
			framewidth+colgap+(beadwidth/2)+(i*(beadwidth+colgap)),0,
			framewidth+colgap+(beadwidth/2)+(i*(beadwidth+colgap)),height);

	XFlush(display);

}/* draw Rails*/

