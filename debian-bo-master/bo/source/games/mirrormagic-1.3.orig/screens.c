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
*  screens.c                                               *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#include "screens.h"
#include "events.h"
#include "sound.h"
#include "game.h"
#include "tools.h"
#include "editor.h"

void DrawMainMenu()
{
  int i;

  FadeSounds();
  GetPlayerConfig();
  LoadLevel(level_nr);

  ClearWindow();
  DrawText(SX+16, SY+8,  "MIRROR MAGIC II",FS_BIG,FC_YELLOW);
  DrawText(SX+32, SY+46, "Copyright 1995 by Holger Schemel",
	   FS_SMALL,FC_RED);
  DrawText(SX+32, SY+64, "Name:",FS_BIG,FC_GREEN);
  DrawText(SX+192,SY+64, player.alias_name,FS_BIG,FC_RED);
  DrawText(SX+32, SY+96, "Level:",FS_BIG,FC_GREEN);
  DrawText(SX+352,SY+96, int2str(level_nr,2),FS_BIG,(level_nr>49)*FC_YELLOW);
  DrawText(SX+32, SY+128,"Hall Of Fame",FS_BIG,FC_GREEN);
  DrawText(SX+32, SY+160,"Level Creator",FS_BIG,FC_GREEN);
  DrawText(SY+32, SY+192,"Info Screen",FS_BIG,FC_GREEN);
  DrawText(SX+32, SY+224,"Start Game",FS_BIG,FC_GREEN);
  DrawText(SX+32, SY+256,"Setup",FS_BIG,FC_GREEN);
  DrawText(SX+32, SY+288,"Quit",FS_BIG,FC_GREEN);

  DrawMicroLevel(SX+12*32,SY+6*32);

  for(i=0;i<8;i++)
    DrawGraphic(0,i+2,45);
  DrawGraphic(10,3,45);
  DrawGraphic(13,3,45);

  DrawText(SX+54,SY+326,"A Game by Artsoft Development",FS_SMALL,FC_BLUE);
  DrawText(SX+40,SY+344,"Graphics: Deluxe Paint IV Amiga",
	   FS_SMALL,FC_BLUE);
  DrawText(SX+60,SY+362,"Sounds: AudioMaster IV Amiga",
	   FS_SMALL,FC_BLUE);

  InitAnimation();
  HandleMainMenu(0,0,0,0,MB_MENU_MARK);
}

void HandleMainMenu(int mx, int my, int dx, int dy, int button)
{
  static int choice=3;
  static int redraw=TRUE;
  int x=(mx+32-SX)/32, y=(my+32-SY)/32;

  if (redraw)
  {
    DrawGraphic(0,choice-1,44);
    redraw=FALSE;
  }

  if (dx || dy)
  {
    if (dx && choice==4)
    {
      x=(dx<0 ? 11 : 14);
      y=4;
    }
    else if (dy)
    {
      x=1;
      y=choice+dy;
    }
    else
      x=y=0;

    if (y<3)
      y=3;
    else if (y>10)
      y=10;
  }

  if (!mx && !my && !dx && !dy)
  {
    x=1;
    y=choice;
  }

  if (button && y==4 && ((x==11 && level_nr>0) || (x==14 && level_nr<99)))
  {
    static long level_delay=0;

    if (level_nr==50 && x==11 && player.handicap<50)
      goto out;
    if (level_nr==player.handicap && level_nr<49 && x==14)
      goto out;

    if (!DelayReached(&level_delay,20))
      goto out;

    level_nr+=1-2*(x==11);
    DrawTextExt(drawto,gc,
		SX+352,SY+96,int2str(level_nr,2),
		FS_BIG,(level_nr>49)*FC_YELLOW);
    DrawTextExt(window,gc,
		SX+352,SY+96,int2str(level_nr,2),
		FS_BIG,(level_nr>49)*FC_YELLOW);

    LoadLevel(level_nr);
    DrawMicroLevel(SX+12*32,SY+6*32);
  }
  else if (x==1 && y>=3 && y<=10)
  {
    if (button)
    {
      if (y!=choice)
      {
	DrawGraphic(0,y-1,44);
	DrawGraphic(0,choice-1,45);
      }
      choice=y;
    }
    else
    {
      if (y==3)
      {
	game_status = TYPENAME;
	HandleTypeName(strlen(player.alias_name),0);
      }
      else if (y==4)
      {
	level_nr=(level_nr<50)*50;
	DrawText(SX+352,SY+96,int2str(level_nr,2),
		 FS_BIG,(level_nr>49)*FC_YELLOW);
	LoadLevel(level_nr);
	DrawMicroLevel(SX+12*32,SY+6*32);
      }
      else if (y==5)
      {
	game_status=HALLOFFAME;
	DrawHallOfFame(-1);
	redraw=TRUE;
      }
      else if (y==6)
      {
	game_status=LEVELED;
	DrawLevelEd();
	redraw=TRUE;
      }
      else if (y==7)
      {
	game_status=HELPSCREEN;
	DrawHelpScreen();
	redraw=TRUE;
      }
      else if (y==8)
      {
	game_status=PLAYING;
	InitGame();
	redraw=TRUE;
      }
      else if (y==9)
      {
	game_status = SETUP;
	DrawSetupScreen();
	redraw = TRUE;
      }
      else if (y==10)
      {
        if (AreYouSure("Do you really want to quit ?",1))
	  game_status=EXITGAME;
      }
    }
  }
  BackToFront();

  out:

  if (game_status==MAINMENU)
    DoAnimation();
}

/* global, so DrawHelpScreen()/ShowHelpScreen() can both see them... */
static int gels1[] =
{
  20,0,16,28,104,37,40,80,41,33,48,64,
  24,100,108,44,36,47,105,96,42,32,38,106
};
static int gcyc[] =
{
  4,16,4,0,0,0,0,16,0,0,16,4, 4,4,4,3,0,0,0,4,2,0,2,0
};

void DrawHelpScreen()
{
  int i;
  long xt,yt;
  static char *gtxt[] =
  {
    "Gregor McDuffin", "The Magician",
    "Gnome with",      "Rotating Mirror",
    "Fixed Metallic",  "Polarisator",
    "Kettle with",     "Spell Ingredient",
    "Light Sensitive", "Metallic Block",
    "Light Sensitive", "Cobalt Bomb",
    "Metallic Lock",   "",
    "Rotating",        "Polarisator",
    "Golden Key to",   "open the Locks",
    "4 different",     "Walls",
    "Spell Beamer",    "",
    "Moving Cruncher", "eats Kettles",
    "Spell Receiver",  "",
    "Stone Gnome with","Fixed Mirror",
    "Fixed Wooden",    "Polarisator",
    "Magic Balloon",   "gives Bonusscore",
    "Light Sensitive", "Wooden Block",
    "Surprise Balloon","",
    "Wooden Lock",     "",
    "Rotating 2 ways", "Polarisator",
    "Incandescent",    "Lamp must shine",
    "Random",          "Reflecting Prism",
    "Light Sensitive", "Safety Device",
    "Spell Amplifier", "gives extra fuel"
  };

  ClearWindow();
  for(i=0;i<24;i++)
  {
    DrawGraphic((i/12)*8,i%12,gels1[i]);
    xt=SX+34+(i/12)*256;
    yt=SY+(i%12)*32;
    DrawText(xt,yt+2+8*(*gtxt[i*2+1]==0),
	     gtxt[i*2],FS_SMALL,((i%12)%2)*FC_YELLOW);
    if (*gtxt[i*2+1])
      DrawText(xt,yt+16,gtxt[i*2+1],0,3*((i%12)%2));
  }

  XCopyArea(display,drawto,drawto,gc,154,9, 12,5, 154,12);
  BackToFront();
  InitAnimation();
  PlaySoundLoop(SND_RHYTHMLOOP);
}

void HandleHelpScreen(int button)
{
  static int helpscreen_state=0;
  static long helpscreen_delay=0;
  int button_released = !button;
  int i;

  if (button_released)
  {
    FadeSound(SND_RHYTHMLOOP);
    DrawMainMenu();
    BackToFront();
    game_status=MAINMENU;
  }
  else
  {
    if (DelayReached(&helpscreen_delay,10))
    {
      helpscreen_state = (helpscreen_state<15 ? helpscreen_state+1 : 0);
      for(i=0;i<24;i++) 
      {
	DrawGraphicExt(drawto,gc,(i/12)*8,i%12,
		       gels1[i]+(gcyc[i]*helpscreen_state)/16);
	DrawGraphicExt(window,gc,(i/12)*8,i%12,
		       gels1[i]+(gcyc[i]*helpscreen_state)/16);
      }
    }
    DoAnimation();
    XFlush(display);
  }
}

void HandleTypeName(int newxpos, KeySym key)
{
  static int xpos = 0, ypos = 2;
  unsigned char ascii;

  if (newxpos)
  {
    xpos = newxpos;
    DrawText(SX+6*32,SY+ypos*32,player.alias_name,FS_BIG,FC_YELLOW);
    DrawGraphic(xpos+6,ypos,44);
    return;
  }

  if ((key>=XK_A && key <=XK_Z) || (key>=XK_a && key<=XK_z && 
      xpos<MAX_NAMELEN-1))
  {
    if (key>=XK_A && key<=XK_Z)
      ascii = 'A'+(char)(key-XK_A);
    if (key>=XK_a && key<=XK_z)
      ascii = 'a'+(char)(key-XK_a);
    player.alias_name[xpos] = ascii;
    player.alias_name[xpos+1] = 0;
    xpos++;
    DrawTextExt(drawto,gc,SX+6*32,SY+ypos*32,
		player.alias_name,FS_BIG,FC_YELLOW);
    DrawTextExt(window,gc,SX+6*32,SY+ypos*32,
		player.alias_name,FS_BIG,FC_YELLOW);
    DrawGraphic(xpos+6,ypos,44);
  }
  else if (key==XK_Delete && xpos>0)
  {
    player.alias_name[xpos] = 0;
    xpos--;
    DrawGraphic(xpos+6,ypos,44);
    DrawGraphic(xpos+7,ypos,-1);
  }
  else if (key==XK_Return && xpos>0)
  {
    DrawText(SX+6*32,SY+ypos*32,player.alias_name,FS_BIG,FC_RED);
    DrawGraphic(xpos+6,ypos,-1);
    if (!strcmp(player.alias_name,"Artsoft"))
      player.handicap = 50;
    SavePlayerInfo();
    game_status=MAINMENU;
  }
  BackToFront();
}

void DrawHallOfFame(int pos)
{
  int y;
  char txt[40];

  if (pos<0) 
    LoadScore(level_nr);
  ClearWindow();
  DrawText(SX+64,SY+10,"Hall Of Fame",FS_BIG,FC_YELLOW);
  sprintf(txt,"HighScores of Level %d",level_nr);
  DrawText(SX+256-strlen(txt)*7,SY+48,txt,FS_SMALL,FC_RED);
  for(y=0;y<10;y++)
  {
    DrawText(SX,SY+64+y*32,"................",FS_BIG,(y!=pos)*FC_GREEN);
    DrawText(SX,SY+64+y*32,highscore[y].Name,FS_BIG,(y!=pos)*FC_GREEN);
    DrawText(SX+384,SY+64+y*32,
	     int2str(highscore[y].Score,4),FS_BIG,(y!=pos)*FC_GREEN);
  }
  BackToFront();

  InitAnimation();
  PlaySound(SND_HALLOFFAME);
}

void HandleHallOfFame(int button)
{
  int button_released = !button;

  if (button_released)
  {
    FadeSound(SND_HALLOFFAME);
    game_status=MAINMENU;
    DrawMainMenu();
    BackToFront();
  }
  else
    DoAnimation();
}

void DrawSetupScreen()
{
  int i;

  ClearWindow();
  DrawText(SX+16, SY+16,  "SETUP",FS_BIG,FC_YELLOW);
  DrawText(SX+32, SY+2*32,"Sound:",FS_BIG,FC_GREEN);
  DrawText(SX+32, SY+3*32,"Sound loops:",FS_BIG,FC_GREEN);
  DrawText(SX+32, SY+4*32,"Game music:",FS_BIG,FC_GREEN);
  DrawText(SX+32, SY+5*32,"Toons:",FS_BIG,FC_GREEN);
  DrawText(SX+32, SY+7*32,"Exit",FS_BIG,FC_GREEN);
  DrawText(SX+32, SY+8*32,"Save and exit",FS_BIG,FC_GREEN);

  if (SETUP_SOUND_ON(player.setup))
    DrawText(SX+13*32, SY+2*32,"on",FS_BIG,FC_YELLOW);
  else
    DrawText(SX+13*32, SY+2*32,"off",FS_BIG,FC_BLUE);

  if (SETUP_SOUND_LOOPS_ON(player.setup))
    DrawText(SX+13*32, SY+3*32,"on",FS_BIG,FC_YELLOW);
  else
    DrawText(SX+13*32, SY+3*32,"off",FS_BIG,FC_BLUE);

  if (SETUP_SOUND_MUSIC_ON(player.setup))
    DrawText(SX+13*32, SY+4*32,"on",FS_BIG,FC_YELLOW);
  else
    DrawText(SX+13*32, SY+4*32,"off",FS_BIG,FC_BLUE);

  if (SETUP_TOONS_ON(player.setup))
    DrawText(SX+13*32, SY+5*32,"on",FS_BIG,FC_YELLOW);
  else
    DrawText(SX+13*32, SY+5*32,"off",FS_BIG,FC_BLUE);

  for(i=2;i<9;i++)
    if (i!=6)
      DrawGraphic(0,i,45);

  InitAnimation();
  HandleSetupScreen(0,0,0,0,MB_MENU_MARK);
}

void HandleSetupScreen(int mx, int my, int dx, int dy, int button)
{
  static int choice=3;
  static int redraw=TRUE;

  int x=(mx+32-SX)/32, y=(my+32-SY)/32;

  if (redraw)
  {
    DrawGraphic(0,choice-1,44);
    redraw=FALSE;
  }

  if (dx || dy)
  {
    if (dy)
    {
      x=1;
      y=choice+dy;
    }
    else
      x=y=0;

    if (y==7)
      y=(dy>0 ? 8 : 6);

    if (y<3)
      y=3;
    else if (y>9)
      y=9;
  }

  if (!mx && !my && !dx && !dy)
  {
    x=1;
    y=choice;
  }

  if (x==1 && y>=3 && y<=9 && y!=7)
  {
    if (button)
    {
      if (y!=choice)
      {
	DrawGraphic(0,y-1,44);
	DrawGraphic(0,choice-1,45);
      }
      choice=y;
    }
    else
    {
      if (y==3 && sound_status==SOUND_AVAILABLE)
      {
	if (SETUP_SOUND_ON(player.setup))
	  DrawText(SX+13*32, SY+2*32,"off",FS_BIG,FC_BLUE);
	else
	  DrawText(SX+13*32, SY+2*32,"on ",FS_BIG,FC_YELLOW);
	player.setup ^= SETUP_SOUND;
      }
      else if (y==4 && sound_loops_allowed)
      {
	if (SETUP_SOUND_LOOPS_ON(player.setup))
	  DrawText(SX+13*32, SY+3*32,"off",FS_BIG,FC_BLUE);
	else
	  DrawText(SX+13*32, SY+3*32,"on ",FS_BIG,FC_YELLOW);
	player.setup ^= SETUP_SOUND_LOOPS;
      }
      else if (y==5 && sound_loops_allowed)
      {
	if (SETUP_SOUND_MUSIC_ON(player.setup))
	  DrawText(SX+13*32, SY+4*32,"off",FS_BIG,FC_BLUE);
	else
	  DrawText(SX+13*32, SY+4*32,"on ",FS_BIG,FC_YELLOW);
	player.setup ^= SETUP_SOUND_MUSIC;
      }
      else if (y==6)
      {
	if (SETUP_TOONS_ON(player.setup))
	  DrawText(SX+13*32, SY+5*32,"off",FS_BIG,FC_BLUE);
	else
	  DrawText(SX+13*32, SY+5*32,"on ",FS_BIG,FC_YELLOW);
	player.setup ^= SETUP_TOONS;
      }
      else if (y==8 || y==9)
      {
        if (y==9)
	  SavePlayerInfo();

	GetPlayerConfig();
	game_status=MAINMENU;
	DrawMainMenu();
	redraw=TRUE;
      }
    }
  }
  BackToFront();

  if (game_status==SETUP)
    DoAnimation();
}
