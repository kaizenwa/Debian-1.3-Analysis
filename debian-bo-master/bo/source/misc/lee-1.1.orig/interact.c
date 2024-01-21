#include "defs.h"


void actInWorldInteractive(organism, numExpelled, startX, startY)
	struct indiv	*organism;
	int				numExpelled, startX, startY;
{
	if (gInteractive) {
#ifdef THINK_C
		if ((organism->worldx == gCurrentZoomCell.h) && (organism->worldy == gCurrentZoomCell.v) && (gCurrentZoomItem == 0) && !((organism->worldx==startX) && (organism->worldy==startY))) {
			gCurrentZoomItem+=numExpelled;
		}
		drawCell(organism->worldx, organism->worldy);
#endif
	}
}





void	ins_org_Interactive(ip)
	struct indiv	*ip;
{
	if (gInteractive) {
#ifdef THINK_C
		if ((ip->worldx == gCurrentZoomCell.h) && (ip->worldy == gCurrentZoomCell.v))
			gCurrentZoomItem++;
		drawCell(ip->worldx, ip->worldy);
#endif
	}
}




void	EventsInteractive()
{	
#ifdef THINK_C
	do {
		HandleEvent();
	} while  (gPaused && (!gDone));
#endif
}





void	del_org_Interactive(x, y, itemNum)
	int		x,y,itemNum;
{
	if (gInteractive) {
#ifdef THINK_C
		if ((x == gCurrentZoomCell.h) && (y == gCurrentZoomCell.v))
			if (gCurrentZoomItem>itemNum)
				gCurrentZoomItem--;
			else if (gCurrentZoomItem==itemNum)
				gCurrentZoomItem==0;
		drawCell(x, y);
#endif
	}
}




void update_world_Interactive(xpos, ypos)
	int		xpos, ypos;
{
	if (gInteractive) {
#ifdef THINK_C
		if ((xpos == gCurrentZoomCell.h) && (ypos == gCurrentZoomCell.v))
			gCurrentZoomItem++;
		drawCell(xpos, ypos);
#endif
	}
}




void	updateGenInteractive()
{
	if (gInteractive) {
#ifdef THINK_C
		updateGeneration();
#endif 
	}
}



void	InteractiveInit(argc, argv)
	int     *argc;
	char  ***argv;
{
#ifdef THINK_C
		ToolBoxInit();
		MoreMasters();
		MaxApplZone();
		InitMenuBar();
		*argc = ccommand(argv);
		InitWindow();
		gInitDialog = GetNewDialog(INIT_DIALOG, NIL_POINTER, (WindowPtr) -1);
		SetPort(gInitDialog);
		DrawDialog(gInitDialog);
		SetPort(gTheWindow);
                if ((XMAX > 25)||(YMAX > 25))
                {
                        verbose = 1;
                        gInteractive = FALSE;
                        printf("\nMacintosh Warning:");
                        printf("\n\tWorld size too large for interactive mode:");
                        printf("\n\tRunning in non-interactive mode...\n");
                }
                else
                {
                        verbose = 0;
                        gInteractive = TRUE;
                }
#endif
}




void	InteractiveFinalSetUp()
{
#ifdef THINK_C
		DisposDialog(gInitDialog);
#endif
}




void	moveInteractiveLasso(orgItemNum, isSelected, organism)
	int 			orgItemNum;
	boolean			*isSelected;
	struct indiv	*organism;
{

#ifdef THINK_C
	Rect		r;
	PenState	thePen;
#endif


	if (gInteractive) {
#ifdef THINK_C
		if ((FrontWindow()==gZoomCellWindow) && (gCurrentZoomCell.h == organism->worldx) && (gCurrentZoomCell.v == organism->worldy) && gLassoOn && (gCurrentZoomItem==orgItemNum)) {
			*isSelected = TRUE;
			
			GetPenState(&thePen);
			PenPat(white);
			SetRect(&r, START_X+gCurrentZoomCell.h*SQUARE_SIZE, START_Y+gCurrentZoomCell.v*SQUARE_SIZE, START_X+(gCurrentZoomCell.h+1)*SQUARE_SIZE - GAP_SIZE, START_Y+(gCurrentZoomCell.v+1)*SQUARE_SIZE-GAP_SIZE);
			SetRect(&r, r.left-2, r.top-2,r.right+2, r.bottom+2);
			FrameRect(&r);
			SetPenState(&thePen);
	
			gCurrentZoomCell.h = -1;
			gCurrentZoomCell.v = -1;
		}
#endif
	}
}


void	moveInteractDrawSelectFrame(isSelected, new_pos)
	boolean		isSelected;
	position	new_pos;
{

#ifdef THINK_C
	Rect		r;
	PenState	thePen;
#endif


	if (gInteractive) {
#ifdef THINK_C
                if (isSelected) {
                        gCurrentZoomCell.h = new_pos.x;
                        gCurrentZoomCell.v = new_pos.y;
                        gCurrentZoomItem = 0;

			GetPenState(&thePen);
			SetRect(&r, START_X+gCurrentZoomCell.h*SQUARE_SIZE, START_Y+gCurrentZoomCell.v*SQUARE_SIZE, START_X+(gCurrentZoomCell.h+1)*SQUARE_SIZE - GAP_SIZE, START_Y+(gCurrentZoomCell.v+1)*SQUARE_SIZE-GAP_SIZE);
			SetRect(&r, r.left-2, r.top-2,r.right+2, r.bottom+2);
			FrameRect(&r);
			SetPenState(&thePen);
		}
#endif
	}
}
