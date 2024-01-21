/***********************************************************
*  Mirror Magic II -- McDuffins Revenge                    *
*----------------------------------------------------------*
*  ©1994 Artsoft Development                               *
*        Holger Schemel                                    *
*        33659 Bielefeld-Senne                             *
*        Telefon: (0521) 493245                            *
*        eMail: aeglos@valinor.ms.sub.org                  *
*               aeglos@uni-paderborn.de                    *
*               q99492@pbhrzx.uni-paderborn.de             *
*----------------------------------------------------------*
*  editor.c                                                *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#include "editor.h"
#include "screens.h"
#include "game.h"
#include "tools.h"

int gels2[] =
{
  80,0,16,21,24,28,37,32,33,38,48,47,
  100,68,96,105,43,40,41,36,44,104,106,108
};
int ct[] =
{
  0,17, 16,21, 20,25, 97,114, 114,119,
  118,135, 134,139, 138,143, 150,155
};

void DrawLevelEd()
{
  DrawLevel();
  XFillRectangle(display,pix[DB_DOOR],gc,0,0,DXSIZE,DYSIZE);

  for(i=0;i<24;i++)
  {
    j=gels2[i];
    XCopyArea(display,pix[BACK],pix[DB_DOOR],gc,
	      SX+(j%16)*32,SY+(j/16)*32, 32,32, 2+(i%3)*32,2+(i/3)*32);
  }
  XCopyArea(display,pix[DOOR],pix[DB_DOOR],gc,202,50, 96,18, 2,260);
  OpenDoor(0);
}

void LevelEd(int mx, int my, int button)
{
  static int el=1;
  static int count=-1, cntx=-1, cnty=-1;
  static BOOL saving=FALSE;
  static buttondownpos=0;

  if (!button)
    buttondownpos=0;
  else if (!buttondownpos)
  {
    if (mx<DX)
      buttondownpos=1;
    else if (my<DY+262)
      buttondownpos=2;
    else if (my<DY+276)
      buttondownpos=3;
  }
  else
  {
    if (mx<DX)
      { if (buttondownpos!=1) return; }
    else if (my<DY+262)
      { if (buttondownpos!=2) return; }
    else if (my<DY+276)
      { if (buttondownpos!=3) return; }
  }
    
  if (!saving)	/* normal operation: editing the level */
  {
    x=(mx-SX)/32; 
    y=(my-SY)/32; 
    DO=1;
    if (button && mx>DX && my>DY+260 && my<DY+276)
    {
      if (mx>DX+2 && mx<DX+46)
      {
	if (AreYouSure("Are you sure to clear this level ?",3))
	{
	  for(x=0;x<16;x++) 
	    for(y=0;y<12;y++) 
	      Ray[x][y]=0;
	  ClearWindow(); 
	  BackToFront();
	}
      }
      if (mx>DX+52 && mx<DX+96)
      {
	for(k=0,y=0;y<12;y++) 
	  for(x=0;x<16;x++)
	    if (Ray[x][y]>20 && Ray[x][y]<25) 
	      k=1;
	if (!k)
	{
	  AreYouSure("No Level without Gregor Mc Duffin please !",0);
	  CloseDoor(0);
	  OpenDoor(1);
	  Delay(3000000);
	  CloseDoor(1);
	  OpenDoor(0);
	}
	else
	{
	  if (!AreYouSure("Save this level and kill the old ?",-3))
	  {
	    game_status=MAINMENU;
	    DrawMainMenu();
	    BackToFront();
	    return;
	  }
	  else
	  {
	    AreYouSure("Please choose time for the new level !",0);
	    XCopyArea(display,pix[DOOR],pix[DB_DOOR],gc,
		      202,30, 96,20, 102,258);
	    DrawTextExt(pix[DB_DOOR],gc,
			130,262,int2str(leveltime,3),FS_SMALL,FC_YELLOW);
	    OpenDoor(1);
	    saving=TRUE;
	  }
	}
      }
    }
    else if (button && mx>DX && mx<DX+98)
    {
      if (my>DY && my<DY+258)
      {
	x=(mx-DX-1)/32; 
	y=(my-DY-1)/32;
	el=Els[3*y+x];
	if (x==2 && y==2) 
	  el=47+16*(mx>DX+80)+32*(my>DY+80);
      }
    }
    else if ((OK=button) && mx>=SX && mx<SX+512 && my>=SY && my<SY+384)
    {
      BT=((mx-SX)/16-x*2+1)<<((my-SY)/16-y*2)*2;
      EL=Ray[x][y];
      if (OK==2)
      {
	if (EL>32 && EL<96)
	{
	  Ray[x][y]&=(0xFFFF^BT);
	  if ((Ray[x][y]&15)==0) 
	    Ray[x][y]=0;
	  DrawElement(x,y,Ray[x][y]);
	}
	else
	{
	  Ray[x][y]=0;
	  DrawGraphic(x,y,-1);
	}
	BackToFront();
	Delay(10000);
      }
      else
      {
	if (x==cntx && y==cnty && count>Counter())
	{
	  Delay(10000);
	  return;
	}

	l=1;
	if (el>32 && el<96)
	{
	  if (EL<32 || EL>95) 
	    EL=0;
	  EL=(el & 0xF0) | (EL & 0x0F) | BT; 
	  l=0;
	}
	else
	{
	  for(j=0;j<18;j+=2)
	  {
	    if (el>ct[j] && el<ct[j+1] && EL>ct[j] && EL<ct[j+1])
	    { 
	      if (OK==1)
	      {
		if (++EL>ct[j+1]-1) 
		  EL=ct[j]+1; 
	      }
	      else if (OK==3)
	      {
		if (--EL<ct[j]+1) 
		  EL=ct[j+1]-1; 
	      }
	      break; 
	    }
	  }
	  if (j>17) 
	    EL=el;
	}
	DrawElement(x,y,Ray[x][y]=EL);
	BackToFront();

	count=Counter()+10;
	cntx=x;
	cnty=y;
      }
    }
  }
  else	/* saving==TRUE: setting the time for the new level */
  {
    if (!button || mx<DX+3 || mx>DX+95 || my<DY+259 || my>DY+275)
      return;

    if ((mx<DX+20 && leveltime>1) || (mx>DX+78 && leveltime<100))
    {
      leveltime=leveltime-1+2*(mx>DX+78);
      DrawText(DX+30,DY+262,int2str(leveltime,3),FS_SMALL,FC_YELLOW);
      BackToFront();
      Delay(50000);
    }

    if (mx>DX+24 && mx<DX+74) 
    {
      DrawTextExt(pix[DB_DOOR],gc,
		  130,262,int2str(leveltime,3),FS_SMALL,FC_YELLOW);
      CloseDoor(1);
      for(x=0;x<16;x++)
	for(y=0;y<12;y++) 
	  Ur[x][y]=Ray[x][y];
      SaveLevel(level_nr);
      saving=FALSE;

      game_status=MAINMENU;
      DrawMainMenu();
      BackToFront();
    }
  }
}
