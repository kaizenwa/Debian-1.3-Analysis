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
*  game.c                                                  *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#include "game.h"
#include "misc.h"
#include "tools.h"
#include "screens.h"
#include "sound.h"

BOOL CreateNewLevelFile()
{
  int i,j;
  FILE *file;

  if (!(file=fopen(LEVEL_FILE,"w")))
    return(FALSE);

  for(i=0;i<100;i++)
  {
    fputc(21,file);
    for(j=1;j<192;j++) 
      fputc(0,file);
    fputc(100,file);
    for(j=0;j<7;j++)
      fputc(0,file);
  }
  fclose(file);
  chmod(LEVEL_FILE, LEVEL_PERMS);
  return(TRUE);
}

BOOL CreateNewScoreFile()
{
  int i,j;
  FILE *file;

  if (!(file=fopen(SCORE_FILE,"w")))
    return(FALSE);

  for(i=0;i<100;i++)
  {
    for(j=0;j<MAX_PLAYERS;j++)
    {
      fprintf(file,"%10s ",EMPTY_ALIAS);
      fprintf(file,"%04d ",0);
    }
  }
  fclose(file);
  chmod(SCORE_FILE, SCORE_PERMS);
  return(TRUE);
}

BOOL CreateNewNamesFile()
{
  FILE *file;

  if (!(file=fopen(NAMES_FILE,"w")))
    return(FALSE);

  fputs(NAMES_COOKIE,file);		/* Formatkennung */
  fclose(file);

  chmod(NAMES_FILE, NAMES_PERMS);
  return(TRUE);
}

void LoadLevel(int level_nr)
{
  int x,y;
  FILE *file;

  if (!(file=fopen(LEVEL_FILE,"r")))
  {
    if (!CreateNewLevelFile())
    {
      fprintf(stderr,"%s: cannot create level file '%s'!\n",
	      progname,LEVEL_FILE);
    }
    else if (!(file=fopen(LEVEL_FILE,"r"))) 
    {
      fprintf(stderr,"%s: cannot load level %d!\n",
	      progname,level_nr);
    }
  }

  if (file)
  {
    fseek(file,level_nr*200,SEEK_SET);
    for(y=0;y<12;y++) 
      for(x=0;x<16;x++) 
	Ur[x][y] = fgetc(file);
    leveltime = fgetc(file);
    fclose(file);
  }
  else
  {
    for(y=0;y<12;y++) 
      for(x=0;x<16;x++)
	Ur[x][y] = 0;
    Ur[0][0] = 21;
    Ur[15][11] = 25;
    leveltime = 100;
  }
}

void LoadScore(int level_nr)
{
  int i;
  FILE *file;

  if (!(file=fopen(SCORE_FILE,"r")))
  {
    if (!CreateNewScoreFile())
    {
      fprintf(stderr,"%s: cannot create score file '%s'!\n",
	      progname,SCORE_FILE);
    }
    else if (!(file=fopen(SCORE_FILE,"r"))) 
    {
      fprintf(stderr,"%s: cannot load score for level %d!\n",
	      progname,level_nr);
    }
  }

  if (file)
  {
    fseek(file,level_nr*(MAX_PLAYERS*(MAX_NAMELEN+5)),SEEK_SET);
    for(i=0;i<MAX_PLAYERS;i++)
    {
      fscanf(file,"%s",highscore[i].Name);
      fscanf(file,"%d",&highscore[i].Score);
    }
    fclose(file);
  }
  else
  {
    for(i=0;i<MAX_PLAYERS;i++)
    {
      strcpy(highscore[i].Name,EMPTY_ALIAS);
      highscore[i].Score = 0;
    }
  }
}

BOOL ConvertNames()
{
  int i,j;
  char cookie[256];
  FILE *file;
  char *login_name = GetLoginName();
  struct PlayerInfo playerlist[MAX_PLAYERS];

  if (!(file=fopen(NAMES_FILE,"r")))	/* altes Format lesen ... */
  {
    fprintf(stderr,"%s: cannot convert old names file!\n",progname);
    return(FALSE);
  }

  fscanf(file,"%s",cookie);
  if (!strcmp(cookie,NAMES_COOKIE12))	/* Namensdateiformat 1.2 */
  {
    for(i=0;i<MAX_PLAYERS;i++)
    {
      fscanf(file,"%s",playerlist[i].login_name);
      fscanf(file,"%s",playerlist[i].alias_name);
      fscanf(file,"%d",&playerlist[i].last_used);
      fscanf(file,"%d",&playerlist[i].handicap);
      fscanf(file,"%x",&playerlist[i].setup);
    }
    fclose(file);
  }
  else					/* Namensdateiformat 1.0 ? */
  {
    rewind(file);
    for(i=0;i<MAX_PLAYERS;i++)
    {
      fscanf(file,"%s",playerlist[i].alias_name);
      if (!strcmp(playerlist[i].alias_name,EMPTY_ALIAS))
	strcpy(playerlist[i].login_name,EMPTY_LOGIN);
      else
	strcpy(playerlist[i].login_name,login_name);
      fseek(file,i*12+11,0);
      playerlist[i].handicap = fgetc(file);
      playerlist[i].last_used = FALSE;
      playerlist[i].setup = 0;
    }
    fclose(file);
  }

  CreateNewNamesFile();			/* ... und neu wieder schreiben */

  for(i=0;i<MAX_PLAYERS;i++)
  {
    for(j=0;j<MAX_NAMELEN;j++)
      player.login_name[j] = player.alias_name[j] = 0;
    strncpy(player.login_name,playerlist[i].login_name,MAX_NAMELEN-1);
    strncpy(player.alias_name,playerlist[i].alias_name,MAX_NAMELEN-1);
    player.handicap = playerlist[i].handicap;
    player.setup = playerlist[i].setup;

    SavePlayerInfo();
  }

  return(TRUE);
}

void LoadPlayerInfo()
{
  int i;
  char cookie[256];
  FILE *file;
  char *login_name = GetLoginName();
  struct PlayerInfo default_player;

  for(i=0;i<MAX_NAMELEN;i++)
    default_player.login_name[i] = default_player.alias_name[i] = 0;
  strncpy(default_player.login_name,login_name,MAX_NAMELEN-1);
  strncpy(default_player.alias_name,login_name,MAX_NAMELEN-1);
  default_player.handicap = 0;
  default_player.setup = DEFAULT_SETUP;

  if (!(file=fopen(NAMES_FILE,"r")))
  {
    if (!CreateNewNamesFile())
    {
      fprintf(stderr,"%s: cannot create names file '%s'!\n",
	      progname,NAMES_FILE);
    }
    else if (!(file=fopen(NAMES_FILE,"r"))) 
    {
      fprintf(stderr,"%s: cannot load player information!\n",
	      progname);
    }
  }

  if (file)
  {
    fgets(cookie,NAMES_COOKIE_LEN,file);
    if (strcmp(cookie,NAMES_COOKIE))	/* älteres Format? */
    {
      fclose(file);
      if (!ConvertNames())
	file = NULL;
      else if (!(file=fopen(NAMES_FILE,"r"))) 
      {
	fprintf(stderr,"%s: cannot load player information!\n",
		progname);
      }
      else
	fgets(cookie,NAMES_COOKIE_LEN,file);
    }
  }

  if (!file)
  {
    player = default_player;
    level_nr = player.handicap;
    return;
  }

  while(1)
  {
    for(i=0;i<MAX_NAMELEN;i++)
      player.login_name[i] = fgetc(file);
    for(i=0;i<MAX_NAMELEN;i++)
      player.alias_name[i] = fgetc(file);
    player.handicap = fgetc(file);
    player.setup = (fgetc(file)<<8) | fgetc(file);

    if (feof(file))		/* Spieler noch nicht in Liste enthalten */
    {
      player = default_player;

      fclose(file);
      if (!(file=fopen(NAMES_FILE,"a")))
      {
	fprintf(stderr,"%s: cannot append new player to names file!\n",
		progname);
      }
      else
      {
	for(i=0;i<MAX_NAMELEN;i++)
	  fputc(player.login_name[i],file);
	for(i=0;i<MAX_NAMELEN;i++)
	  fputc(player.alias_name[i],file);
	fputc(player.handicap,file);
	fputc(player.setup / 256,file);
	fputc(player.setup % 256,file);
      }
      break;
    }
    else			/* prüfen, ob Spieler in Liste enthalten */
      if (!strncmp(player.login_name,login_name,MAX_NAMELEN-1))
	break;
  }

  level_nr = player.handicap;
  fclose(file);
}

void SaveLevel(int level_nr)
{
  int x,y;
  FILE *file;

  if (!(file=fopen(LEVEL_FILE,"r+")))
  {
    fprintf(stderr,"%s: cannot save level %d!\n",
	    progname,level_nr);
    return;
  }

  fseek(file,level_nr*200,SEEK_SET);
  for(y=0;y<12;y++) 
    for(x=0;x<16;x++) 
      fputc(Ur[x][y],file);
  fputc(leveltime,file);
  fclose(file);
}

void SaveScore(int level_nr)
{
  int i;
  FILE *file;

  if (!(file=fopen(SCORE_FILE,"r+")))
  {
    fprintf(stderr,"%s: cannot save score for level %d!\n",
	    progname,level_nr);
    return;
  }

  fseek(file,level_nr*(MAX_PLAYERS*(MAX_NAMELEN+5)),SEEK_SET);
  for(i=0;i<MAX_PLAYERS;i++)
  {
    fprintf(file,"%10s ",highscore[i].Name);
    fprintf(file,"%04d ",highscore[i].Score);
  }
  fclose(file);
}

void SavePlayerInfo()
{
  int i;
  char cookie[256];
  FILE *file;
  char *login_name = GetLoginName();
  struct PlayerInfo default_player;

  if (!(file=fopen(NAMES_FILE,"r+")))
  {
    fprintf(stderr,"%s: cannot save player information!\n",
	    progname);
    return;
  }

  fgets(cookie,NAMES_COOKIE_LEN,file);
  if (strcmp(cookie,NAMES_COOKIE))	/* ungültiges Format? */
  {
    fprintf(stderr,"%s: wrong format of names file!\n",progname);
    fclose(file);
    return;
  }

  while(1)
  {
    for(i=0;i<MAX_NAMELEN;i++)
      default_player.login_name[i] = fgetc(file);
    for(i=0;i<MAX_NAMELEN;i++)
      default_player.alias_name[i] = fgetc(file);
    default_player.handicap = fgetc(file);
    default_player.setup = (fgetc(file)<<8) | fgetc(file);

    if (feof(file))		/* Spieler noch nicht in Liste enthalten */
      break;
    else			/* prüfen, ob Spieler in Liste enthalten */
      if (!strncmp(default_player.login_name,player.login_name,MAX_NAMELEN-1))
      {
	fseek(file,-(2*MAX_NAMELEN+1+2),SEEK_CUR);
	break;
      }
  }

  for(i=0;i<MAX_NAMELEN;i++)
    fputc(player.login_name[i],file);
  for(i=0;i<MAX_NAMELEN;i++)
    fputc(player.alias_name[i],file);
  fputc(player.handicap,file);
  fputc(player.setup / 256,file);
  fputc(player.setup % 256,file);

  fclose(file);
}

void GetPlayerConfig()
{
  if (sound_status==SOUND_OFF)
    player.setup &= ~SETUP_SOUND;
  if (!sound_loops_allowed)
  {
    player.setup &= ~SETUP_SOUND_LOOPS;
    player.setup &= ~SETUP_SOUND_MUSIC;
  }

  sound_on = SETUP_SOUND_ON(player.setup);
  sound_loops_on = SETUP_SOUND_LOOPS_ON(player.setup);
  sound_music_on = SETUP_SOUND_MUSIC_ON(player.setup);
  toons_on = SETUP_TOONS_ON(player.setup);
}

void DrawLevel()
{
  int x,y;

  G[0].Nr=G[1].Nr=BL=LP=PM=OK=CT=DO=0;

  ClearWindow();
  for(x=0;x<16;x++) for(y=0;y<12;y++)
  {
    OK=Ray[x][y]=Ur[x][y];
    Hit[x][y]=Box[x][y]=0;
    if (OK==29) 
      BL++;
    if (OK==145) 
      LP++;
    if ((OK>0 && OK<17) || (OK>97 && OK<114) || (OK>118 && OK<139))
    {
      Cyc[CT].XP=x; 
      Cyc[CT].YP=y;
      Cyc[CT].Dr=RND(16)*(1-2*RND(2));
      CT++;
    }
    if (OK>114 && OK<119)
    {
      Pac[PM].XP=x; 
      Pac[PM].YP=y;
      Pac[PM].Dr=OK-115+((OK-114)%2)*2;
      PM++;
    }
    if (OK>20 && OK<25) 
    { 
      First.XP=x; 
      First.YP=y; 
      First.Wk=4*(OK-21); 
    }
    if (OK>97 && OK<114) 
    { 
      k=G[0].Nr; 
      G[k].XP=x; 
      G[k].YP=y; 
      G[k].Nr=1; 
    }
    DrawElement(x,y,OK);
  }
  BackToFront();
}


void InitGame()
{
  DrawLevel();

  if (CT) for(i=0;i<16;i++)
  {
    for(j=0;j<CT;j++)
    {
      if (!Cyc[j].Dr) 
	continue;
      x=Cyc[j].XP; 
      y=Cyc[j].YP;
      k=Cyc[j].Dr; 
      k/=ABS(k);
      l=Ray[x][y]+=k;
      if (l==0 || l==17 || l==97 || l==114 || l==118 || (l==135 && k==1))
      { 
	l-=k*16; 
	Ray[x][y]=l; 
      }
      else if (l==139 || (l==134 && k==-1)) 
      { 
	l-=k*4; 
	Ray[x][y]=l; 
      }
      DrawElement(x,y,l);
      Cyc[j].Dr-=k;
    }
    BackToFront();
    ColorCycling();
    Delay(100000);
  }

  XCopyArea(display,pix[DOOR],pix[DB_DOOR],gc, 300,0, 100,280, 300,0);

  LX=First.XP*32;
  if (First.Wk%8) 
    LX+=14; 
  else 
    LX=LX+28*(First.Wk==0);
  LY=First.YP*32;
  if (First.Wk%8) 
    LY=LY+28*(First.Wk==12); 
  else 
    LY+=14;
  RP=DP=GP=WN=SC=OV=DR=SR=SL=OB=S1=0; 
  SRx=SRy=-1;
  EN=leveltime;
  EC=Ec=OC=Oc=PC=Pc=CT=Ct=0; DO=1;

  NextRay(LX,LY);

  LW=First.Wk;
  XS=2*Step[LW].x; 
  YS=2*Step[LW].y;
  DrawTextExt(pix[DB_DOOR],gc,300+glx,gly,int2str(level_nr,2),FS_SMALL,FC_YELLOW);
  DrawTextExt(pix[DB_DOOR],gc,300+gbx,gby,int2str(BL,3),FS_SMALL,FC_YELLOW);
  DrawTextExt(pix[DB_DOOR],gc,300+gsx,gsy,int2str(SC,4),FS_SMALL,FC_YELLOW);
  OpenDoor(3);

  Delay(100000);

  if (sound_loops_on)
    PlaySoundExt(SND_FUEL,PSND_MAX_VOLUME,PSND_MAX_RIGHT,PSND_LOOP);

  XCopyArea(display,pix[DOOR],drawto,gc, 
	    200+gex,geey-EN+2, 32,EN, GEX,GEEY-EN+2);
  for(k=0;k<EN;k+=2)
  {
    if (!sound_loops_on)
      PlaySoundStereo(SND_FUEL,PSND_MAX_RIGHT);

    XCopyArea(display,pix[DOOR],window,gc, 
	      200+gex,geey-k, 32,2+k, GEX,GEEY-k);
    XFlush(display);
    ColorCycling();
    Delay(20000);
  }

  if (sound_loops_on)
    StopSound(SND_FUEL);

  if (sound_music_on)
    PlaySoundLoop(SND_TYGER);

  ScanLaser();
}

void NextRay(int lx, int ly)
{
  RayList[RP][0]=SX+2 +lx; 
  RayList[RP][1]=SY+2 +ly; 
  RP++;
}

void NextDam(int ex, int ey)
{
  DamList[DP].Mr=0;
  DamList[DP].Wk=LW; 
  DamList[DP].Nr=RP;
  DamList[DP].XP=ex; 
  DamList[DP].YP=ey; 
  DP++;
}

BOOL StepBehind()
{
  int x=LX-XS,y=LY-YS;
  int lastx=x,lasty=y;

  if (RP)
  {
    lastx=RayList[RP-1][0]-SX-2;
    lasty=RayList[RP-1][1]-SY-2;
    return((x-lastx)*XS<0 || (y-lasty)*YS<0);
  }
  else
    return(FALSE);
}


int ScanPixel()
{
  for(;;)
  {
    i=0;
    OK=0;

    while(i<4)
    {
/*
      extern char *DoubleRayScreenMaske_bits;
*/
      int bx,by,bbx,bby,byte,bit,bitmask,pixel;

      bx=SX+LX+(i%2)*2;
      by=SY+LY+(i/2)*2;
      bbx=(bx-SX+32)/32-1;  /* ...+32...-1 to get correct */
      bby=(by-SY+32)/32-1;  /* negative values!           */
      if (bbx>=0 && bbx<=15 && bby>=0 && bby<=11)
      {
	int obj=Ray[bbx][bby];

	if (!obj)
	  pixel=0;
	else if (obj>31 && obj<96)
	{
	  int pos=((by-SY-bby*32)/16)*2+(bx-SX-bbx*32)/16;

	  pixel=(obj & (1<<pos));
	}
	else
	{
	  int g,e=obj;

	  if (e<30) g=e-1;
	  else if (e==30) g=37;
	  else if (e==31) g=32;
	  else if (e==96) g=36;
	  else if (e==97) g=47;
	  else if (e>97 && e<114) g=e-50;
	  else if (e==114) g=38;
	  else if (e>114 && e<119) g=e-47;
	  else if (e>118 && e<143) g=e-39;
	  else if (e>142 && e<147) g=e-103;
	  else if (e==147) g=44+RND(3);
	  else if (e>147 && e<151) g=e-44;
	  else if (e>150 && e<155) g=e-43;
	  else if (e==155) g=107;

/*
	  bx=bx-bbx*32+((obj-1)%16)*32;
	  by=by-bby*32+((obj-1)/16)*32;
*/
	  bx=bx-bbx*32+(g%16)*32;
	  by=by-bby*32+(g/16)*32;

	  byte=by*80+bx/8;
	  bit=bx%8;
	  bitmask=1<<bit;

	  pixel=XGetPixel(imagemask,bx,by);

/*
	  pixel=DoubleRayScreenMaske_bits[byte] & bitmask;
*/
	}
      }
      else
      {
	byte=by*80+bx/8;
	bit=bx%8;
	bitmask=1<<bit;

	pixel=XGetPixel(imagemask,bx,by);

/*
	pixel=DoubleRayScreenMaske_bits[byte] & bitmask;
*/
      }

      if ((Sign[LW] & (1<<i)) && pixel)
	OK|=(1<<i);


/*
      if ((Sign[LW] & (1<<i)) && (ReadPixel(rp,4+LX+i%2,4+LY+i/2) > 1))
	OK|=(1<<i);
*/

      i++;
    }

    if (OK)
    {
      EX=(LX+XS)/32;
      EY=(LY+YS)/32;
      return(OK);
    }
    LX+=XS;
    LY+=YS;
  }
}

void ScanLaser()
{
  int end=0,rf=RP;
  unsigned short color_scale = 0xFFFF/15;

  OL=OB=BK=0;

  if (OV<92) 
    SetRGB(pen_ray,(OV/6)*color_scale,0x0000,(15-(OV/6))*color_scale);

  if (DO) 
  { 
    DBM=1; 
    DrawLaser(0,1); 
    DBM=0; 
  }

/*
  StopSound(1);
*/

  for(;;)
  {
    if (RP>250 || DP>250) 
    { 
      end=OL=1; 
      break; 
    }

    BT=ScanPixel();

    if (LX<0 || LX>511 || LY<0 || LY>383) 
      { EL=0; break; }
    if (EX<0 || EX>15 || EY<0 || EY>11) 
      { EL=0; break; }
    if (BT==6) 
      { EX=(LX-1)/32; EY=(LY+1)/32; }
    if (BT==9) 
      { EX=(LX-1)/32; EY=(LY-1)/32; }
    EL=Ray[EX][EY];
    if (!EL) 
      { if (!Edge()) break; }
    else if (EL==114) 
      { if (Polarizer()) break; }
    else if ((EL>16 && EL<21) || (EL>150 && EL<155)) 
      { if (Polarizer()) break; }
    else if ((EL==96 || EL==143) || (EL>147 && EL<150)) 
      { if (Block()) { rf=1; break; } }
    else if (EL<17 || (EL>28 && EL<32) || EL>95) 
      { if (Objekt()) break; }
    else if (EL<25) 
      { if (LaserGun()) break; }
    else if (EL<29) 
      { if (Receiver()) break; }
    else if (EL<48) 
      { if (Walls()) break; }
    else if (EL<96) 
      { if (Walls2()) break; }
    else break;
    if (DO && rf) 
      DrawLaser(rf-1,1);
    rf=RP;
  }

  if (!end && !BK && !StepBehind())
    NextRay(LX-=XS,LY-=YS);
  if (DO && rf) 
    DrawLaser(rf-1,1);

  Ct=CT=Counter();
}

void DrawLaser(int start, int mode)
{
  int L,L1=start,L2=-1;

  if (GP && start<GP && !mode) 
  { 
    DrawLaser(GP,0); 
    BackToFront();
    RP=GP; 
    GP=0; 
  }
  if (GP && start<GP && mode)
  {
    int r=RP,g=GP;
    RP=GP; 
    GP=0; 
    if (!DO) 
      DBM=1;
    DrawLaser(0,1);
    RP=r; 
    L1=start=GP=g; 
    if (!DO) 
      DBM=0;
  }

  while(L2!=RP)
  {
    if (RP-L1<120) 
      L2=RP; 
    else 
      L2=L1+120;

    XDrawLines(display,drawto,line_gc[mode],
	       (XPoint *)(&RayList[L1][0]),L2-L1,CoordModeOrigin);
    if (mode)
      XDrawLines(display,window,line_gc[mode],
		 (XPoint *)(&RayList[L1][0]),L2-L1,CoordModeOrigin);
    XFlush(display);

    L1=L2-1;
  }

  if (mode) 
    return;

  if (DP)
  {
    int dx,dy,de;
    OK=0;
    if (start) 
      for(k=0;k<DP;k++)
	if (DamList[k].Nr==start+1) 
	{ 
	  OK=k; 
	  break; 
	}
    for(k=OK;k<DP;k++)
    {
      dx=DamList[k].XP;
      dy=DamList[k].YP;
      de=Ray[dx][dy];
      if (Hit[dx][dy]==DamList[k].Nr)
	if (!(de>97 && de<114 && k==OK)) 
	  Hit[dx][dy]=0;
      if (Box[dx][dy]==DamList[k].Nr) 
	Box[dx][dy]=0;
      DrawElement(dx,dy,de);
    }
    x=DamList[OK].XP;
    y=DamList[OK].YP;
    L=Ray[x][y];
    if (GP && L>97 && L<114)
    { 
      LW=L-98; 
      DP=OK+1; 
    }
    else 
      LW=DamList[DP=OK].Wk;
  }	
  redraw_mask|=REDRAW_FIELD;

  L=Ray[x][y];
  RP=start+1;
  if (!start) 
    LW=First.Wk;
  LX=RayList[start][0] -(SX+2); 
  LY=RayList[start][1] -(SY+2);
  XS=2*Step[LW].x; 
  YS=2*Step[LW].y;
  if (start) if (L>97 && L<139)
  {
    if (LW==(LW>>1)<<1) 
      OK=8; 
    else 
      OK=3+1*(L<114);
    if (L>118 || !GP) 
    { 
      RP--; 
      OK*=-1; 
    }
    LX+=OK*XS; 
    LY+=OK*YS;
  }
  else if (L) 
  { 
    LX-=3*XS; 
    LY-=3*YS; 
    RP--; 
  }
}

BOOL Objekt()
{
  if (Edge())
    return(FALSE);
  NextDam(EX,EY);
  if (EX!=(LX+5*XS)/32 || EY!=(LY+5*YS)/32)
  {
    LX+=2*XS;
    LY+=2*YS;
    return(FALSE);
  }
  if (LX+5*XS<0 || LY+5*YS<0)
  {
    LX+=2*XS;
    LY+=2*YS;
    return(FALSE);
  }
  if (EL>118 && EL<135 && ((EL-119)%2 || (EL-119)/2!=LW%8))
  { 
    PlaySoundStereo(SND_KINK,ST(EX)); 
    DP--;
    return(TRUE); 
  }
  if (EL>134 && EL<139 && (EL-135)!=LW%4)
  { 
    PlaySoundStereo(SND_KINK,ST(EX));
    DP--;
    return(TRUE); 
  }
  if (EL<98 || (EL>113 && EL<151)) 
    NextRay(LX=EX*32+14,LY=EY*32+14);
  if (EL<17 || EL==31 || (EL>118 && EL<143))
  {
    AW=LW;
    DP--; 
    NextDam(EX,EY);
    DamList[DP-1].Mr=1;
    if (!Hit[EX][EY]) 
      Hit[EX][EY]=DamList[DP-1].Nr;
    if (EL<17 || EL>138)
    {
      if (EL<17) 
	LW=16-LW+(EL-1);
      else 
	LW=16-LW+4*(EL-139);
      if (LW<0 || LW>=16) 
	LW = LW<0 ? LW+16 : LW-16;
    }
    if (EL==31) 
      LW=RND(16);
    XS=2*Step[LW].x; 
    YS=2*Step[LW].y;
    if (LW==(LW>>1)<<1) 
      OK=8; 
    else 
      OK=4;
    LX+=OK*XS; 
    LY+=OK*YS;
    if ((EL<17 || EL>138) && !S1 && AW!=LW)
    {
/*	
      MoveSprite(vp,&Pfeil[2],4L+16*EX,5L+16*EY+1); 
*/
      SPK=6; 
    }
    if ((EL<119 || EL>138) && AW!=LW) 
      PlaySoundStereo(SND_LASER,ST(EX)); 
    else 
      S1=0;
    return(OL=ABS(LW-DamList[DP-1].Wk)==8);
  }
  if (EL==150) 
    return(BK=1);
  if (EL==30) 
    PlaySoundStereo(SND_KINK,ST(EX));
  if (EL==29 || EL==144 || EL==147 || (EL>114 && EL<119))
  {
    if (EL<115 || EL>118) 
      Bang(EX,EY,28+44*(EL>29));
    if (EL==29)
    {
      if (BL) 
	DrawText(GBX,GBY,int2str(--BL,3),FS_SMALL,FC_YELLOW);
      DrawText(GSX,GSY,int2str(SC+=10,4),FS_SMALL,FC_YELLOW);
      if (!BL)
      {
	int x,y,z;

        PlaySoundStereo(SND_KLING,ST(EX));
	for(z=0;z<3;z++)
	{
	  for(y=0;y<12;y++) 
	    for(x=0;x<16;x++)
	      if (Ray[x][y]==25+z) 
		DrawElement(x,y,Ray[x][y]=26+z);
	  BackToFront();
	  Delay(200000);
	}
	DrawLaser(0,1);
      }
    }
    else if (EL==144) 
      SL++;
    else if (EL==147) 
      DrawText(GSX,GSY,int2str(SC+=10,4),FS_SMALL,FC_YELLOW);
    else if (EL>114 && EL<119)
    {
      DeletePacMan(EX,EY);
      DrawText(GSX,GSY,int2str(SC+=50,4),FS_SMALL,FC_YELLOW);
    }
    BackToFront();
    return(FALSE);
  }
  if (EL==145 || EL==146)
  {
    PlaySoundStereo(SND_KINK,ST(EX));

    DrawLaser(0,1);
    if (Ray[EX][EY]==145) 
    { 
      Ray[EX][EY]=146; 
      LP--; 
    }
    else
    { 
      Ray[EX][EY]=145; 
      LP++; 
    }
    DrawElement(EX,EY,Ray[EX][EY]);
    DrawLaser(0,1);

    BackToFront();
    return(BK=1);
  }
  if (EL>97 && EL<114 && !GP && G[1].Nr)
  {
    DP--;
    OK=EL-90; 
    if (OK>15) 
      OK-=16;
    if (LW==OK)
    {
      NextRay(LX=EX*32+14,LY=EY*32+14);
      NextDam(EX,EY);
      DamList[DP-1].Mr=1;
      if (!Hit[EX][EY]) 
	Hit[EX][EY]=DamList[DP-1].Nr;
      l=(EX==G[0].XP && EY==G[0].YP);
      EX=G[l].XP; 
      EY=G[l].YP;
      LX=EX*32+14; 
      LY=EY*32+14;
      LW=Ray[EX][EY]-98;
      XS=2*Step[LW].x; 
      YS=2*Step[LW].y;
      GP=RP;
      NextRay(LX,LY);
      NextDam(EX,EY); 
      DamList[DP-1].Mr=1;
      if (!Hit[EX][EY]) 
	Hit[EX][EY]=DamList[DP-1].Nr;
      if (LW==(LW>>1)<<1) 
	OK=8; 
      else 
	OK=4;
      LX+=OK*XS; 
      LY+=OK*YS;
      return(FALSE);
    }
  }
  return(TRUE);
}

BOOL Edge()
{
  if (!(BT==1 || BT==2 || BT==4 || BT==8) || !(LW%4)) 
    return(FALSE);
  else
  {
    int xe=2,ye=2;
    if (BT&5) 
      xe=-2; 
    if (BT&3) 
      ye=-2;
    NextDam((LX+xe)/32,(LY+ye)/32);
    LX+=XS; 
    LY+=YS;
    return(TRUE);
  }
}

BOOL Polarizer()
{
  if (Edge()) 
    return(FALSE);
  if (EL<21) 
    return(Walls()); 
  else 
    return(Walls2());
}

BOOL Block()
{
  OK=0;
  if ((EL==143 || EL==149) && !SL) 
    OK=1;
  if (EL==96 || EL==148)
  {
    int i,x,y,ex=EX*32+14,ey=EY*32+14;

    OK=1;
    for(i=1;i<32;i++)
    {
      x=LX+i*XS;
      y=LY+i*YS;
      if ((x==ex || x==ex+1) && (y==ey || y==ey+1))
	OK=0;
    }
/* was:
    if ((XS && (x=LX+10*XS*2/ABS(XS))<0) || (YS && (y=LY+10*YS*2/ABS(YS))<0))
      OK=1;
    if ((XS && EX!=x/32) || (YS && EY!=y/32)) 
      OK=1;
*/
  }
  if (OK && (EL==96 || EL==149)) 
    return(Walls2());
  if (OK)
  {
    NextRay(LX-XS,LY-YS);
    NextDam(EX,EY);
    if (!Box[EX][EY]) 
      Box[EX][EY]=RP;
    return(Walls());
  }

  if (EL==143 || EL==149)
  {
    int xs=XS/2, ys=YS/2;

    if ((BT&6)==6 || (BT&9)==9) 
    {
      OL=(EL==143);
      return(TRUE);
    }
    if (ABS(xs*ys)==1 && (BT==3 || BT==5 || BT==10 || BT==12))
      NextDam(EX-xs*(BT%5>0),EY-ys*(BT%3>0));
    NextRay(LX,LY);

    Bang(EX,EY,76);

    SL--;
    if (EL==143 && Box[EX][EY])
    {
      DrawLaser(Box[EX][EY]-1,0);
      BackToFront();
      ScanLaser();
      return(TRUE);
    }
    return(FALSE);
  }
  if (EL==96 || EL==148)
  {
    int xs=XS/2, ys=YS/2;

    if ((BT&6)==6 || (BT&9)==9)
    {	
      OL=(EL==148);
      return(TRUE);
    }
    if (ABS(xs*ys)==1 && (BT==3 || BT==5 || BT==10 || BT==12))
      NextDam(EX-xs*(BT%5>0),EY-ys*(BT%3>0));
    NextDam(EX,EY);
    NextRay(LX=EX*32+14,LY=EY*32+14);
    OB=BK=1;
    return(TRUE);
  }
  return(TRUE);
}

BOOL LaserGun()
{
   if (Edge()) 
     return(FALSE);
   PlaySoundStereo(SND_AUTSCH,ST(EX));
   OL=1;
   return(TRUE);
}

BOOL Receiver()
{
  int t=0;
  int middle=1;

  if (Edge()) 
    return(FALSE);

  if (EL<28) 
    t=1;

  if (LW%2 && (EX!=(LX+6*XS)/32 || EY!=(LY+6*YS)/32)) 
    middle=0;
  if (LW%2 && (LX+6*XS<0 || LY+6*YS<0)) 
    middle=0;

/*
  if (LW%2 && (EX!=(LX+6*XS)/32 || EY!=(LY+6*YS)/32)) 
    t=1;
  if (LW%2 && (LX+6*XS<0 || LY+6*YS<0)) 
    t=1;
*/

  if (t) 
  {
    PlaySoundStereo(SND_HOLZ,ST(EX));
    return(TRUE);
  }

  if (middle)
    NextRay(LX=EX*32+14,LY=EY*32+14);
  else
    NextRay(LX-=XS,LY-=YS);

  NextDam(EX,EY);
  BK=1;
  if (!LP) 
    WN=1;
  return(TRUE);
}

BOOL Walls()
{
  if (LW%4 && (BT==3 || BT==5 || BT==10 || BT==12))
  {
    PlaySoundStereo(SND_HUI,ST(EX));
    NextRay(LX-=XS,LY-=YS);
    if (!(LW%2))
    {
      if (BT==3 || BT==12) 
      { 
	LX+=2*XS; 
	LW=16-LW; 
      }
      else	
      { 
	LY+=2*YS; 
	LW=16-LW+8; 
      }
      if (LW<0 || LW>=16) 
	LW = LW<0 ? LW+16 : LW-16;
      NextRay(LX,LY);
      XS=2*Step[LW].x; 
      YS=2*Step[LW].y;
      return(FALSE);
    }
    else if (BT==3 || BT==12)
    {
      LW=16-LW;
      if (ABS(XS)==4) 
	NextRay(LX+=2*XS,LY);
      else 
      { 
	NextRay(LX+=XS,LY+(YS/4)*2); 
	NextRay(LX+=XS,LY); 
      }
      YS=2*Step[LW].y;
      return(FALSE);
    }
    else
    {
      LW=16-LW+8;
      if (LW<0 || LW>=16) 
	LW = LW<0 ? LW+16 : LW-16;
      if (ABS(YS)==4) 
	NextRay(LX,LY+=2*YS);
      else 
      { 
	NextRay(LX+(XS/4)*2,LY+=YS); 
	NextRay(LX,LY+=YS); 
      }
      XS=2*Step[LW].x;
      return(FALSE);
    }
  }	
  if (Edge()) 
    return(FALSE);
  else 
  {
    OL=1;
    return(TRUE);
  }
}

BOOL Walls2()
{
  if (Edge()) 
    return(FALSE);
  if (ABS(XS)==4 && (BT==5 || BT==10))
  { 
    NextRay(LX-=XS,LY-=YS); 
    LX+=3*((XS/4)*2); 
    LY+=2*YS; 
  }
  if (ABS(YS)==4 && (BT==3 || BT==12))
  { 
    NextRay(LX-=XS,LY-=YS); 
    LX+=2*XS; 
    LY+=3*((YS/4)*2); 
  }
  if (EL<64 || EL==96 || EL==114 || EL==149 || (EL>150 && EL<155))
  { 
    PlaySoundStereo(SND_HOLZ,ST(EX)); 
    return(TRUE); 
  }
  if (EL<80)           /* Eis */
  {
    MT=(LX+XS)/16-EX*2+1;          /* Quadrant (horizontal) */
    MT<<=(((LY+YS)/16-EY*2)>0)*2;  /*    ||    (vertikal)   */
    if (!(LW%4)) 
      MT+=MT*(2+(LW%8==0)*2);
    if (MT==1 || MT==2 || MT==4 || MT==8) 
      for(k=0;k<4;k++)
      {
	if (MT==(1<<k) && (XS>0)==(k%2) && (YS>0)==(k/2)) 
	  MT=15-(8>>k);
	else if (ABS(XS)==4 && MT==(1<<k) && (XS>0)==(k%2) && (YS<0)==(k/2))
	  MT=3+(k/2)*9;
	else if (ABS(YS)==4 && MT==(1<<k) && (XS<0)==(k%2) && (YS>0)==(k/2))
	  MT=5+(k%2)*5;
      }
  }
  else                 /* Amöbe */
  {
    EL=Ray[(LX-2*XS)/32][(LY-2*YS)/32];
    if (!(EL==0 || (EL>80 && EL<96)))
    {
      EL=0;
      return(TRUE);
    }
    MT=(LX-2*XS)/16-((EX=(LX-2*XS)/32))*2+1;
    MT<<=((LY-2*YS)/16-((EY=(LY-2*YS)/32))*2)*2;
    if (!(LW%4)) 
      MT+=MT*(2+(LW%8==0)*2);
    EL|=80;
  }
  return(TRUE);
}

void Bang(int xb, int yb, int b)
{
  int e=Ray[xb][yb],tb=5;

  if (b==72 && e!=144) 
    tb=9;
  DrawLaser(0,1);

  if ((e>114 && e<119) || e==144) 
    PlaySoundStereo((e==144 ? SND_KLING : SND_QUIEK),ST(xb));
  else if (e==30 || (e>20 && e<25)) 
    PlaySoundStereo(SND_ROAAAR,ST(xb));
  else if (e==144) 
    PlaySoundStereo(SND_KLING,ST(xb));
  else 
    PlaySoundStereo((b==76 ? SND_WHOOSH : SND_KABUMM),ST(xb));

  for(k=0;k<tb;k++)
  {
    if (k==tb-1)
      b=-tb;

    Delay(50000);

    DrawGraphic(xb,yb,b+k);
    BackToFront();
  }
  Ray[xb][yb]=0;
  DrawLaser(0,1);
}


void ClickElement(int cx, int cy, int cb)
{
  int ce;
  static int cb_new=1;
  static int cb_delay=0;
  static int ct=0;

  if (TWO) 
    PLY^=1;
  RT=1;

  if (!cb)
  {
    cb_new=1;
    cb_delay=0;
    return;
  }

  CT=Counter();
  if (CT>=ct && CT<ct+20+cb_delay*20)
    return;
  ct=CT;

  if (cb==2)
  {
    cb=3;	/* middle button has no function */
    return;
  }
  else if (cb==3)
    cb=2;

/*
  if (TWO) 
    PLY^=1;
  RT=1;

  if (!cb)
  {
    cb_new=1;
    return;
  }
*/

  RT=0; 

  if (cx<SX || cx>SX+511 || cy<SY || cy>SY+383) 
    return;
/*
  if (ReadPixel(drawto,cx,cy)==pen_bg) 
    return;
*/

  if (!Ray[(cx-SX)/32][(cy-SY)/32])
    return;

  cx=(cx-SX)/32; 
  cy=(cy-SY)/32;
  ce=Ray[cx][cy];
  x=cx; 
  y=cy;

  if ((ce>0 && ce<17) || (ce>97 && ce<114) || (ce>118 && ce<139))
  {
    if (ce<17 && ObjHit(x,y,1)) 
      S1=1;
    if (ce>118 && ((EX==x && EY==y) || ObjHit(x,y,1))) 
      S1=1;
    RotateMirror(cb);
  }
  else if (ce>20 && ce<25)
  {
    if (!SR) 
    {
      DrawLaser(0,0);
      BackToFront();
    }
    x=cx; 
    y=cy;
    if (cb==1) 
      if (++ce>24) 
	ce=21;
    if (cb==2) 
      if (--ce<21) 
	ce=24;
    First.XP=x; 
    First.YP=y;
    First.Wk=4*(ce-21);
    LX=First.XP*32;
    if (First.Wk%8) 
      LX+=14; 
    else 
      LX=LX+28*(First.Wk==0);
    LY=First.YP*32;
    if (First.Wk%8) 
      LY=LY+28*(First.Wk==12); 
    else 
      LY+=14;
    RP=DP=0; 
    DO=1;
    NextRay(LX,LY);
    LW=First.Wk;
    XS=2*Step[LW].x; 
    YS=2*Step[LW].y;
    DrawElement(x,y,Ray[x][y]=ce);
    BackToFront();

    if (!SR) 
      ScanLaser();

/*
    WaitCounter(Counter()+8);
*/

  }
  else if (ce==114 && SR)
  {
    if (cx!=SRx || cy!=SRy)
      return;

/*
    if (ReadPixel(rp,SX+16*cx+6,SY+16*cy+8)<15) 
      return;
*/

    SR=0;
    SRx=SRy=-1;
    DrawGraphic(cx,cy,38);
    ScanLaser();
  }
  else if (ce==114 && !SR && cb_new)
  {
    SR=1; 
    SRx=cx;
    SRy=cy;
    OL=0;
    DrawLaser(0,0);
    BackToFront();
    DrawGraphic(cx,cy,39);
    BackToFront();
  }
  else if (ce==147)
  {
    Bang(cx,cy,72);
    DrawText(GSX,GSY,int2str(SC+=10,4),FS_SMALL,FC_YELLOW);
    BackToFront();

    DrawLaser(0,1);
  }
  cb_delay=cb_new;
  cb_new=0;
}

void RotateMirror(int rt)
{
  CT=Counter();

/*
  static int ct=0;
  CT=Counter();

  if (CT>=ct && CT<ct+10)
    return;

  ct=CT;
*/

  if (Ray[x][y]<17)
  {
    if (rt==1) 
      if (++Ray[x][y]>16) 
	Ray[x][y]=1;
    if (rt==2) 
      if (--Ray[x][y]<1) 
	Ray[x][y]=16;
  }
  else if (Ray[x][y]>134)
  {
    if (rt==1) 
      if (++Ray[x][y]>138) 
	Ray[x][y]=135;
    if (rt==2) 
      if (--Ray[x][y]<135) 
	Ray[x][y]=138;
  }
  else if (Ray[x][y]>118)
  {
    if (rt==1) 
      if (++Ray[x][y]>134) 
	Ray[x][y]=119;
    if (rt==2) 
      if (--Ray[x][y]<119) 
	Ray[x][y]=134;
  }
  else
  {
    if (rt==1) 
      if (++Ray[x][y]>113) 
	Ray[x][y]=98;
    if (rt==2) 
      if (--Ray[x][y]<98) 
	Ray[x][y]=113;
  }
  if (ObjHit(x,y,1)) 
    OK=Hit[x][y]; 
  else 
    OK=-1;
  if (OK<0)
  {
    int TST=1;

    if (ObjHit(x,y,6)) 
      TST=2;
    DrawElement(x,y,Ray[x][y]);
    BackToFront();
    if (Ray[x][y]>97 && x==EX && y==EY) 
    { 
      TST=0;

      if (Ray[x][y]<114)	/* aargh! (98-113: Beamer) */
	RP--;

      ScanLaser(); 
    }

    if (TST==2) 
    { 
      DO=0; 
      DrawLaser(0,1); 
      DO=1; 
    }

/*
    WaitCounter(CT+8);
*/

    return;
  }
  DrawLaser(OK-1,0);
  BackToFront();
  ScanLaser();

/*
  WaitCounter(CT+8);
*/

}

BOOL ObjHit(int obx, int oby, int bits)
{
  obx*=32; 
  oby*=32;
  if (bits&1) 
    if (ReadPixel(drawto,SX+obx+15,SY+oby+15)==pen_ray) 
      return(TRUE);
  if (bits&2) 
    for(ll=0;ll<4;ll++)
      if (ReadPixel(drawto,SX+obx+31*(ll%2),SY+oby+31*(ll/2))==pen_ray)
	return(TRUE);
  if (bits&4) 
    for(ll=0;ll<4;ll++)
      if (ReadPixel(drawto,SX+4+obx+22*(ll%2),SY+4+oby+22*(ll/2))==pen_ray) 
	return(TRUE);
  return(FALSE);
}

void DeletePacMan(int px, int py)	
{
  Bang(px,py,72);
  if (PM==1) 
  {
    PM=0;
    return;
  }
  for(i=0;i<PM;i++) 
    if (Pac[i].XP==px && Pac[i].YP==py) 
      break;
  for(PM--,j=i;j<PM;j++)
  {
    Pac[j].XP=Pac[j+1].XP;
    Pac[j].YP=Pac[j+1].YP;
    Pac[j].Dr=Pac[j+1].Dr;
  }
}

void GameWon()
{
  int bumplevel = FALSE;

  StopSound(SND_WARNTON);
  FadeSound(SND_TYGER);

  if (sound_loops_on)
    PlaySoundExt(SND_SIRR,PSND_MAX_VOLUME,PSND_MAX_RIGHT,PSND_LOOP);

  SetRGB(pen_ray,0xFFFF,0xFFFF,0xFFFF);

  if (EN>0) 
  {
    for(;EN>=0;EN--)
    {
      if (!sound_loops_on)
	PlaySoundStereo(SND_SIRR,PSND_MAX_RIGHT);

      DrawText(GSX,GSY,int2str(SC+=5,4),FS_SMALL,FC_YELLOW);
      XCopyArea(display,pix[DOOR],drawto,gc, 
		300+gex,gey, 32,100-EN, GEX,GEY);
      redraw_mask|=REDRAW_DOOR;
      BackToFront();
      Delay(20000);
    }
  }

  if (sound_loops_on)
    StopSound(SND_SIRR);

  XCopyArea(display,drawto,pix[DB_DOOR],gc, 
	    DX,DY, DXSIZE,DYSIZE, 300,0);
  CloseDoor(3);

  if (level_nr==player.handicap && level_nr<50) 
  { 
    player.handicap++; 
    bumplevel = TRUE;
    SavePlayerInfo(); 
  }

  if ((OK=NewHiScore())>=0) 
  {
    game_status=HALLOFFAME;
    DrawHallOfFame(OK);
    if (bumplevel)
      level_nr++;
  }
  else
  {
    game_status=MAINMENU;
    if (bumplevel)
      level_nr++;
    DrawMainMenu();
  }
  BackToFront();
}

BOOL NewHiScore()
{
  int OK=-1;

  LoadScore(level_nr);

  if (!strcmp(player.alias_name,EMPTY_ALIAS) || SC<highscore[9].Score) 
    return(-1);

  for(k=0;k<10;k++) 
  {
    if (SC>highscore[k].Score)	/* Spieler kommt in High-Score-Liste */
    {
      if (k<9)
      {
	int m = 9;

#ifdef ONE_PER_NAME
	for(l=k;l<10;l++)
	  if (!strcmp(player.alias_name,highscore[l].Name))
	    m = l;
	if (m==k)	/* Spieler überschreibt seine alte Position */
	  goto put_into_list;
#endif

	for(l=m;l>k;l--)
	{
	  strcpy(highscore[l].Name,highscore[l-1].Name);
	  highscore[l].Score=highscore[l-1].Score;
	}
      }

      put_into_list:
      sprintf(highscore[k].Name,player.alias_name);
      highscore[k].Score=SC; 
      OK=k;
      break;
    }

#ifdef ONE_PER_NAME
    else if (!strcmp(player.alias_name,highscore[k].Name))
      break;	/* Spieler schon mit besserer Punktzahl in der Liste */
#endif

  }

  if (OK>=0) 
    SaveScore(level_nr);

  return(OK);
}

void ColorCycling(void)
{
  static CC,Cc=0;

  static int color,old=0xF00,new=0x010,mult=1;
  static unsigned short red,green,blue;

  if (color_status==STATIC_COLORS)
    return;

  CC=Counter();

  if (CC<Cc || CC>Cc+5)
  {
    Cc=CC;

    color=old+new*mult;
    if (mult>0)
      mult++;
    else
      mult--;

    if (ABS(mult)==16)
    {
      mult=-mult/16;
      old=color;
      new=new<<4;
      if (new>0x100)
	new=0x001;
    }
    
    red   = 0x0e00*((color&0xF00)>>8);
    green = 0x0e00*((color&0x0F0)>>4);
    blue  = 0x0e00*((color&0x00F));
    SetRGB(pen_magicolor[0],red,green,blue);

    red   = 0x1111*((color&0xF00)>>8);
    green = 0x1111*((color&0x0F0)>>4);
    blue  = 0x1111*((color&0x00F));
    SetRGB(pen_magicolor[1],red,green,blue);
  }
}

int GameActions(int mx, int my, int button)	/* was: WaitCounter() */
{
  unsigned short color_scale = 0xFFFF/15;

  EC=OC=PC=CT=Counter();

  if (PM && (PC>Pc+25 || PC<Pc))
  {
    Pc=PC;
    MovePacMen();
    if (DP>200 && !SR) 
    { 
      DrawLaser(0,0); 
      BackToFront();
      ScanLaser(); 
    }
  }

  if (EC>Ec+400 || EC<Ec)
  {
    Ec=EC; 
    EN--;
    if (EN>=0)
    {
      XCopyArea(display,pix[DOOR],drawto,gc, 
		300+gex,gey, 32,100-EN, GEX,GEY);
      XCopyArea(display,pix[DOOR],window,gc, 
		300+gex,gey, 32,100-EN, GEX,GEY);
    }
    else if (color_status==DYNAMIC_COLORS)
    {
      for(i=15;i>=0;i--) 
      { 
	SetRGB(pen_ray,0x0000,0x0000,i*color_scale);
	Delay(50000);
      }
      DrawLaser(0,0); 
      BackToFront();

      StopSound(SND_WARNTON);
      FadeSound(SND_TYGER);

      CR=1;
      return(AreYouSure("Out of magic energy ! Play it again ?",-3)+1);
    }
  }	

  if (mx>DX+34 && mx<DX+64 && my>DY+122 && my<DY+152 && button)
  {
    if (!AreYouSure("Do you really want to quit the game ?",-3))
    { 
      OpenDoor(3); 
      return(0); 
    }
    else return(1);
  }

  if (mx>DX && my>DY+126 && my<DY+148 && button)
  {
    if (mx>DX+6 && mx<DX+28)
    { 
      sound_music_on=FALSE;
      FadeSound(SND_TYGER);
    }
    else if (mx>DX+70 && mx<DX+92)
    { 
      sound_music_on=TRUE;
      PlaySoundLoop(SND_TYGER); 
    }  
  }

  OK=(!(OL+OV) && EL!=30 && EL!=114 && EL!=97 && (EL&0xF0)!=64);
  OK=(OK && EL!=96 && EL!=148 && EL!=150 && (EL&0xF0)!=80);
  if (OK) 
    return(0);

  if (((OL && OV<100) || (!OL && OV>0)) && ((OC-Oc)>(6+!OL*12) || OC<Oc))
  {
    Oc=OC;
    if (OL) 
      OV++; 
    else 
      OV--;
    if (OV<92) 
      SetRGB(pen_ray,(OV/6)*color_scale,0x0000,(15-(OV/6))*color_scale);

/*
    if (OL && !(CC[1] && !(CB&2))) 
*/

    if (OL)
    {
      if (sound_loops_on)
	PlaySoundExt(SND_WARNTON,PSND_MAX_VOLUME,PSND_MAX_RIGHT,PSND_LOOP);
      else
	PlaySoundStereo(SND_WARNTON,PSND_MAX_RIGHT);
    }

/*
    if (!OL && (CC[1] && !(CB&2))) 
*/

    if (!OL)
      StopSound(SND_WARNTON);
    if (OL) 
    {
      XCopyArea(display,pix[DOOR],drawto,gc, 
		200+gox,goey-OV, 32,OV, GOX,GOEY-OV);
      XCopyArea(display,pix[DOOR],window,gc, 
		200+gox,goey-OV, 32,OV, GOX,GOEY-OV);
    }
    else 
    {
      XCopyArea(display,pix[DOOR],drawto,gc, 
		300+gox,goy, 32,100-OV, GOX,GOY);
      XCopyArea(display,pix[DOOR],window,gc, 
		300+gox,goy, 32,100-OV, GOX,GOY);
    }

    if (OV==100)
    {
      for(i=15;i>=0;i--) 
      { 
	SetRGB(pen_ray,i*color_scale,0x0000,0x0000); 
	Delay(50000);
      }
      DrawLaser(0,0); 
      BackToFront();

/*
      StopSound(1); 
*/

      CR=1;
      return(AreYouSure("Magic spell hit Mc Duffin ! Play it again ?",-3)+1);
    }
  }
  if (SR) 
    return(0);

  CT-=Ct;

  if (EL==30 && CT>150)
  {

    Bang(EX,EY,72);
    DP--; 	
    DrawLaser(0,0);
    BackToFront();
    RP=0;
    Bang(First.XP,First.YP,72);

    CR=1;
    return(AreYouSure("Bomb killed Mc Duffin ! Play it again ?",-3)+1);
  }
  if (EL==114 && CT>50)
  {
    SR=1;
    SRx=EX;
    SRy=EY;
    DrawLaser(0,0);
    BackToFront();
    DrawGraphic(EX,EY,39);
    BackToFront();

  }
  if (EL==97 && CT>150)
  {
    int g;

    OK=RND(5);
    switch(OK)
    {
      case  0: 
        EL=RND(16)+1; 
	break;
      case  1: 
	EL=RND(3)+29;
	break;
      default: 
	EL=RND(29)+114;
	break;
    }
    g=el2gfx(EL);

    for(l=0;l<50;l++)
    {
      x=RND(26); 
      y=RND(26);

      XCopyArea(display,pix[BACK],window,gc,
		SX+(g%16)*32+x,SY+(g/16)*32+y, 6,6, SX+EX*32+x,SY+EY*32+y);
      XFlush(display);

/*
      DrawLaser(0,1);
*/

      Delay(50000);
    }

    DrawElement(EX,EY,Ray[EX][EY]=EL);
    BackToFront();

/*
    LX=EX*32+14; 
    LY=EY*32+14;
    if (LW==(LW>>1)<<1) 
      OK=8; 
    else 
      OK=4;
    LX-=OK*XS; 
    LY-=OK*YS; 

    RP-=2; 
    DP--;
*/

    for(k=(DP-1)*(DP>0);k>=0;k--) 
      if (DamList[k].Mr) 
	break;
    if (k>0)
      DrawLaser(DamList[k].Nr-1,0);
    else
      DrawLaser(0,0);

    ScanLaser();
    return(0);
  }
  if ((EL&0xF0)==64 && CT>100)		/* ice */
  {
    PlaySoundStereo(SND_SLURP,ST(EX));
    for(k=0;k<5;k++)
    {
      OK=k+1; 
      if (k==4) 
      { 
	Ray[EX][EY]&=(MT^0xFF); 
	OK=0; 
      }
      Delay(100000);
      DrawWalls2(EX,EY,Ray[EX][EY],OK,MT);
      BackToFront();
    }

    if (Ray[EX][EY]==64) 
      Ray[EX][EY]=0;
/*
    RP--; 
    LX=RayList[RP][0]- (SX+2); 
    LY=RayList[RP][1]- (SY+2);
*/

    for(k=(DP-1)*(DP>0);k>=0;k--) 
      if (DamList[k].Mr) 
	break;
    if (k>0)
      DrawLaser(DamList[k].Nr-1,0);
    else
      DrawLaser(0,0);

    BackToFront();

    ScanLaser();
    return(0);
  }
  if ((EL&0xF0)==80 && CT>120)		/* amoebe */
  {
    int kk,dx,dy,de,dm;

    k=Ray[EX][EY];
    if ((k!=0 && k<80) || k>=96) 
      goto NoAMB;
    for(k=DP-1;k>=0;k--) 
      if (DamList[k].Mr) 
	break;
    r=RP; 
    d=DP; 
    kk=k;
    if (kk>0)
    {
      DrawLaser(DamList[kk].Nr-1,0);
      BackToFront();
      DBM=1; 
      RP++; 
      DrawLaser(0,1); 
      RP--; 
      DBM=0;
      x=DamList[kk].XP; 
      y=DamList[kk].YP;
      DrawElement(x,y,Ray[x][y]);
      BackToFront();
    }

    for(k=0;k<4;k++) 
      if (MT&(1<<k))
      {
	if (ReadPixel(drawto,SX+EX*32+14+(k%2)*2,SY+EY*32+31*(k/2))==pen_ray) 
	  break;
	if (ReadPixel(drawto,SX+EX*32+31*(k%2),SY+EY*32+14+(k/2)*2)==pen_ray) 
	  break;
      }
    for(l=0;l<4;l++) 
      if (MT&(1<<l))
      {
	if (ReadPixel(drawto,SX+EX*32+31*(l%2),SY+EY*32+31*(l/2))==pen_ray) 
	  break;
      }
    if (GP || kk<1 || k<4 || l<4 || 
	ReadPixel(drawto,SX+EX*32+14,SY+EY*32+14)==pen_ray)
    { 
      RP=r; 
      DP=d; 
      DrawLaser(0,0); 
      BackToFront();
    }
    Ray[EX][EY]=EL|MT;
    DrawWalls2(dx=EX,dy=EY,de=Ray[EX][EY],4,dm=MT);
    BackToFront();
    DO=0; 
    S1=300; 
    ScanLaser();
    DrawLaser(0,1); 
    S1=0;
    PlaySoundStereo(SND_AMOEBE,ST(dx));
    for(k=4;k>=0;k--)
    {
      Delay(20000);
      DrawWalls2(dx,dy,de,k,dm);
      BackToFront();
    }
    DO=1;
    DrawLaser(0,1);

    NoAMB:
    return(0);
  }
  if ((EL==96 || EL==148) && OB && CT>150)
  {
    if (ABS(XS)>ABS(YS))
      k=0; 
    else 
      k=1;
    if (XS<YS) 
      k+=2;
    for(l=0;l<4;l++)
    {
      if (l) 
	k++; 
      if (k>3) 
	k=0;
      x=EX+Step[k*4].x; 
      if (x<0 || x>15) 
	continue;
      y=EY+Step[k*4].y; 
      if (y<0 || y>11) 
	continue;
      if (Ray[x][y]) 
	continue;
      if (ObjHit(x,y,7)) 
	continue;
      break;
    }
    if (l>3)
    {
      OL=(EL==148);
      return(0);
    }

    PlaySoundStereo(SND_BONG,ST(EX));

    Ray[EX][EY]=0;
    Ray[x][y]=EL;

    DrawGraphic(EX,EY,-1);
    DrawElement(x,y,EL);
    BackToFront();

    if (EL==148 && Box[EX][EY])
    {
      DrawLaser(Box[EX][EY]-1,0);
      BackToFront();
      DBM=1;
      DrawLaser(RP-1,1);
      DBM=0;
    }
    ScanLaser();
    return(0);
  }
  if (EL==150 && CT>20)
  {
    XCopyArea(display,pix[DOOR],drawto,gc, 
	      200+gex,geey-EN+2, 32,EN, GEX,GEEY-EN+2);
    for(k=EN;k<100;k+=2)
    {
      XCopyArea(display,pix[DOOR],window,gc, 
		200+gex,geey-k, 32,2+k, GEX,GEEY-k);
      XFlush(display);
      Delay(20000);
    }

    EN=100;
    EL=Ray[EX][EY]=155;
    DrawElement(EX,EY,EL);

    DrawLaser(0,1);
    BackToFront();
    Ec=Counter();

    return(0);
  }
  return(0);
}

void MovePacMen()
{
  static int p=-1;
  int mx,my,ox,oy,nx,ny;
  int g,t;

  if (++p>=PM) 
    p=0;
  Pac[p].Dr--;
  for(l=1;l<5;l++)
  {
    Pac[p].Dr++; 
    if (Pac[p].Dr>4) 
      Pac[p].Dr=1;
    if (Pac[p].Dr%2) 
    { 
      mx=0; 
      my=Pac[p].Dr-2; 
    }
    else 
    { 
      my=0; 
      mx=3-Pac[p].Dr; 
    }
    nx=(ox=Pac[p].XP)+mx;
    ny=(oy=Pac[p].YP)+my;
    t=Ray[nx][ny];
    if (nx<0 || nx>15 || ny<0 || ny>11) 
      continue;
    if (t!=0 && t!=29 && t!=30 && (t&0xF0)!=80) 
      continue;
    if (ObjHit(nx,ny,1)) 
      continue;
    Ray[ox][oy]=0;
    Ray[nx][ny]=114+(Pac[p].Dr-1+(Pac[p].Dr%2)*2);
    Pac[p].XP=nx;
    Pac[p].YP=ny;
    g=Ray[nx][ny]-115L;
    DrawGraphic(ox,oy,-1);
    if (t) 
    {
      CT=Counter();
      ox=SX+ox*32;
      oy=SY+oy*32;
      for(i=1;i<33;i+=2)
      {
	XCopyArea(display,pix[BACK],window,gc, 
		  SX+g*32,SY+4*32, 32,32, ox+i*mx,oy+i*my);
	XFlush(display);
	Delay(1000);
      }
      Ct=Ct+Counter()-CT;
    }
    DrawElement(nx,ny,Ray[nx][ny]);
    BackToFront();
    if (!SR) 
    {
      DrawLaser(0,1);

      if (ObjHit(nx,ny,4)) 
      {
	NextDam(nx,ny);
	DamList[DP-1].Nr=0;
      }
    }
    if (t==30) 
    {
      DeletePacMan(nx,ny);
    }
    if ((t&0xF0)==80 && (LX+2*XS)/16==nx && (LY+2*YS)/16==ny)
    {
      RP--; 
      ScanLaser();
    }
    break;
  }
}
