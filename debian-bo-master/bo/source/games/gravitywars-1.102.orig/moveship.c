/* GravityWars 1.1,  (C) Sami Niemi -95 */

#include <asm/io.h>
#include "memory.h"
#include "macros.h"


/*----------------------------------------------------------------- moveShip */
/* Once again... This routine contains a lot of bad "undocumented" non logical
/* code due to all the changes to the project that was never supposed to
/* become a real game... */

int moveShip(short nr, unsigned char flag) {

  static short n,num,x,y,xx,yy,x2,y2,wflag,level_adr,m;
  static short e_num,lx,ly,tmp1,tmp2,sl;
  static long pal_m;
  static long pal_n;
  static uchar reg;

  static char blocktype;

  static uchar *shipptr;
  static uchar *backptr;

  backptr=shipbackM;

  x=sx[nr] >> STEP;
  y=sy[nr] >> STEP;
  getbox(x, y, backptr);
  


  if (!(flag & 1))
    num=nr;
  else
    num=nr+2;
 
/*===================*/
  switch ((sf[nr] & 63) >> 3) {
    
  case 0: 
    wflag=1;
    shipptr=ship[num][sa[nr] >> 5];
    break;
  case 1:
    wflag=0;
    ship_flag=ship_flag | 1;             /* Keep the explosion still */
    sf[nr]++;
    shipptr=block[45];
    break;
  case 2:
    wflag=0;
    sf[nr]++;
    shipptr=block[46];
    break;
  case 3:
    wflag=0;
    sf[nr]++;
    shipptr=block[47];
    break;
  case 4:
    wflag=0;
    sf[nr]++;
    shipptr=block[48];
    break;
  case 5:
    wflag=0;
    sf[nr]++;
    shipptr=block[49];
    break;
  case 6:
    wflag=2;
    sf[nr]=sf[nr] & 65472;

/*
    sx[nr]=Lsx[nr];
    sy[nr]=Lsy[nr];
*/

    sa[nr]=0;
    sVx[nr]=sVy[nr]=sVg[nr]=1;  /* Shouldn't ever be Zero */
    sA[nr]=0;

    /* AIR */
    gravity=15;
    lift_thrust=10;
    medium=1000;
    friction=1014;
    ship_flag=ship_flag & 0xfd; /* Water=0 */
    MaxNorm=3072;
    MaxFall=4096;

    shipptr=ship[num][sa[nr] >> 5];

    putbox(x,y,backptr);

    ShipLife--;
    if (ShipLife<=0) 
      the_end();
    else {
      ShipFuel=BaseFuel;

      getbox(Lsx[nr] >> STEP, Lsy[nr] >> STEP, backptr);

      if (ShipTime<=0) {
	putstamp(272,win_y+184,67,76,85);
	for(n=0; n<=100; n++) {
	  AnimateAll();
	  putstamp(272,win_y+184,67,76,85);
	}
	killstamp(272,win_y+184);
      }
      ShipTime=BaseTime;
      ShipFuel=BaseFuel;
      
      for(n=0; n<=1023; n++)  /* Plot the new ship before scrolling */ 
	shipmix[n]=shipptr[n] ? shipptr[n] : backptr[n];
      
      /* ANIMATE THE SHIP INTO THE NEW POSITION */
      
      scrollY(nr,y-240,(Lsy[nr] >> STEP)-232);
      updatescore();
      
      for(n=0; n<=N_ACTION; n++) {
	if (!action[n].state) {
	  action[n].state=TRUE;
	  action[n].x=(Lsx[nr] >> STEP)+16;
	  action[n].y=(Lsy[nr] >> STEP)+16;
	  action[n].start=157;
	  action[n].stop=162;
	  action[n].frame=157;
	  action[n].speed=7;
	  action[n].delay=7;
	  m=n;
	  goto nomoreloop2;
	}
      }
    nomoreloop2:
      
      sx[nr]=Lsx[nr];
      sy[nr]=Lsy[nr];
      
      for(n=0; n<=40; n++) {
	AnimateAll();
      }
      action[m].state=FALSE;
      putbox(Lsx[nr] >> STEP, Lsy[nr] >> STEP, shipmix);
    }
    break;
  }
    
  if (wflag<2) {

    if ( ((ship_flag & 2)==0) || (wflag==0) )
      for(n=0; n<=1023; n++)
	shipmix[n]=shipptr[n] ? shipptr[n] : backptr[n];
    

    
  

    /*===================*/
    shipptr=ship[num & 254][sa[nr] >> 5]; /* & 254 to mask out thrust */


    x2=x+31;
    y2=y+31;
    
    num_of_collisions=0;

    /* Check for Collision */
    if ((sf[nr] & 63)==0) {   /* ZERO => Not Exploding! */
      
      if ((ship_flag&1)==0) { /* If !ZERO => LAYING STILL */
	
	n=0;
	for(yy=y; yy<=y2; yy++) {
	  for(xx=x; xx<=x2; xx++) {
	    n++;
	    
	    if (
		(shipptr[n]!=0) &&
		(
		 (
		  (backptr[n]>127)
		  &&
		  (backptr[n]<175)
		  ) 
		 ||
		 (backptr[n]==DOOR1COLOR)
		 )
		) {
	     
	      num_of_collisions++;

	      /* The exceptions go here! */
	      
	      /* beep(5000);*/

	      
	      
	      wflag=0; /* Marks if in exception */
	      ship_flag=ship_flag & 0xfe;
	      
	      lx=(x+16) >> 5; 
	      ly=(y+16) >> 5;
	      for(e_num=0; e_num<=n_cexcept; e_num++) {
		
		if ((ly==c_except[e_num].y) &&
		    (lx==c_except[e_num].x)) {
		  
		  switch(c_except[e_num].type) {

		  case E_FUEL:
		    
		    fueling=TRUE;
		    /* Gimme more fuel goes here */
 
		  case E_START:
		    
		    if (
			((sa[nr]<=56) ||
			 (sa[nr]>=968)) &&
			((sVy[nr]+sVg[nr])<900)) {
		      
		      /* Landed succesfully here */
		      
		      num_of_collisions--;

		      ship_flag=ship_flag | 1;
		      sa[nr]=0;
		      sVy[nr]=0;
		      
		      wflag=1;
		      beep(10000);
		    }
		    break;

		  default:
		    
		    fueling=FALSE;

		  }
		  
		}
	      }
	      
			            
#ifndef TRAINER      

	      if ((wflag==0) && (num_of_collisions>5)) {
		sf[nr]=(sf[nr] & 65472) | 8;
		num_of_collisions=0;
	      }
	      
	      if ((num_of_collisions>5) || (wflag==1)) goto Explode;
      
#endif 
	    }
	    
	  }
	}

    
	/* Check the level collisions if no other collisions occured */
	lx=(x+16) >> 5;
	ly=(y+16) >> 5;

	level_adr=(ly << 4)+(ly<<2)+lx;  /* ly*20+lx */
	blocktype=objects[level_adr];

	switch(blocktype) {

	case E_WIND_Q:
	  sVx[nr]-=50;
	  sVy[nr]-=50;
	  break;

        case E_WIND_W:
          sVy[nr]-=50;
          break;

        case E_WIND_E:
          sVx[nr]+=50;
          sVy[nr]-=50;
          break;

        case E_WIND_D:
          sVx[nr]+=50;
          break;

        case E_WIND_C:
          sVx[nr]+=50;
          sVy[nr]+=50;
          break;

        case E_WIND_X:
          sVy[nr]+=50;
          break;

        case E_WIND_Z:
          sVx[nr]-=50;
          sVy[nr]+=50;
          break;

        case E_WIND_A:
          sVx[nr]-=50;
          break;

	case E_AIR:
	  
	  if ((ship_flag & 2)==2) { /* Lifting! (Pintajännite) */
	    sVy[nr]=sVy[nr] >> 5;
	    sVg[nr]=sVg[nr] << 5;
	  }
	  
	  /* Default Air Values */
	  gravity=15;
	  lift_thrust=10;
	  medium=1000;
	  friction=1024;
	  ship_flag=ship_flag & 0xfd; /* Water=0 */ 
	  MaxNorm=3072;
	  MaxFall=4096;
 	  break;
	  
	case E_WKEY:
          NumKeys--;
	  blocktype=E_WBONUS;
	  goto h2o;

	case E_WFUEL:
	  ShipFuel+=80; /* 5 */
          if (ShipFuel>1599) ShipFuel=1599;
          blocktype=E_WBONUS;
          updatescore();
	  goto h2o;

        case E_WBONUS1:
          ShipScore+=2;
	  blocktype=E_WBONUS;
          updatescore();
	  goto h2o;

	case E_WBONUS2:
          ShipScore+=3;
	  blocktype=E_WBONUS;
          updatescore();
	  goto h2o;

        case E_WBONUS3:
          ShipScore+=4;
	  blocktype=E_WBONUS;
          updatescore();
	  goto h2o;
	
	case E_WBONUS4:
          ShipScore+=6;
	  blocktype=E_WBONUS;
          updatescore();
	  goto h2o;
	

	case E_TOP_WATER: 
	case E_WATER:
	h2o:
	  watermask(x,y,blocktype,backptr);

	  /* Water */
	  if ((ship_flag & 2)==0) { /* Splash! */
	    sVx[nr]=sVx[nr] >> 2;
	    sVy[nr]=sVy[nr] >> 2;
	    sVg[nr]=sVg[nr] >> 2;
	  }
	  watercount++;

	  sVy[nr]+=((-SIN[watercount & 31]*sVx[nr]) >> 15);
	  sVx[nr]+=((SIN[watercount & 31]*(sVy[nr]+sVg[nr])) >> 15);
	  
	  gravity=4;
	  lift_thrust=15;
	  medium=970;
	  friction=750;
	  ship_flag=ship_flag | 2; /* Water=1 */
	  MaxNorm=2048;
	  MaxFall=2048;
 	  break;
	  
	case E_BONUS8:
	  ShipScore+=8;
	  blocktype=E_BONUS;
	  updatescore();
	  break;

        case E_BONUS7:
          ShipScore+=7;
          blocktype=E_BONUS;
	  updatescore();
          break;

 	case E_BONUS6:
          ShipScore+=6;
	  blocktype=E_BONUS;
	  updatescore();
          break;

        case E_BONUS5:
          ShipScore+=5;
          blocktype=E_BONUS;
	  updatescore();
          break;

 	case E_BONUS4:
          ShipScore+=4;
	  blocktype=E_BONUS;
	  updatescore();
          break;

        case E_BONUS3:
          ShipScore+=3;
	  blocktype=E_BONUS;
	  updatescore();
          break;

        case E_BONUS2:
          ShipScore+=2;
	  blocktype=E_BONUS;
	  updatescore();
          break;

        case E_BONUS1:
          ShipScore+=1;
	  blocktype=E_BONUS;
	  updatescore();
          break;

	case E_KEY:
	  NumKeys--;
	  blocktype=E_BONUS;
  	  break;

        case E_XLIFE:
          ShipLife++;
	  if (ShipLife>9) ShipLife=9;
          blocktype=E_BONUS;
	  updatescore();
          break;

        case E_XFUEL:
          ShipFuel+=80; /* 5 */
	  if (ShipFuel>1599) ShipFuel=1599;
          blocktype=E_BONUS;
	  updatescore();
          break;

        case E_XTIME:
          ShipTime+=640; /* 5 */
	  if (ShipTime>12671) ShipTime=12671;
	  blocktype=E_BONUS;
	  updatescore();
          break;
  	    
	case E_STOP:
	  beep(10000);

	  levelnum++;
	  loaddata();  /* Branches to outro() if it cant load! */ 


	  for (pal_m=0; pal_m<=767; pal_m++) {
	    p3[pal_m]=63;
	    p0[pal_m]=0;
	    p2[pal_m]=63-palB[pal_m];	   
	    p2B[pal_m]=63-pal[pal_m];
	    pal[pal_m]=realpal[pal_m];
	    palB[pal_m]=realpal2[pal_m];
	  }

  /* Roll The Split Screen Down   */
	  for(sl=464; sl<=480; sl++) {
	    outb(0x18,0x3d4);       /* LineCompare */
	    outb(sl&255,0x3d5);        
	    outb(7,0x3d4);          /* Overflow    */
	    reg=inb(0x3d5);         
	    outb(reg|((sl&256)>>4),0x3d5);
	    outb(9,0x3d4);          /* Maximum Scanline */
	    reg=inb(0x3d5);          
	    outb(reg|((sl&512)>>3),0x3d5); 
	    vga_waitretrace();
	  }

	  if (levelnum&1) {

	    for(pal_n=992; pal_n>=32; pal_n-=64) {
	      for (pal_m=0; pal_m<=767; pal_m++)
		p1[pal_m]=63-(( ((long)p2[pal_m])*pal_n)>>10);
	      vga_waitretrace();
	      gl_setpalette(p1);
	    }

	  }
	  else {

	    for(pal_n=992; pal_n>=32; pal_n-=64) {
              for (pal_m=0; pal_m<=767; pal_m++)
                p1[pal_m]=63-(((long)p2B[pal_m])*pal_n)>>10;
              vga_waitretrace();

              gl_setpalette(p1);
            }
 

	  }
	  
	  gl_setpalette(p3);

	  /* DRAW NEW LEVEL AND INITIALIZE IT HERE */
	  drawScreen();

	  /* Put New Score Sign */
	  for(n=0; n<=4095; n++) {
	    tmpscore[n]=score[n];
	    score[n]=levelcode[n];
	  }

	  tmp1=(sy[nr] >> STEP)-240;
	  if (tmp1<16)
	    tmp1=16;
	  else
	    if (tmp1>960)
	      tmp1=960;
	  win_y=tmp1+8;

	  putletter(1426,codes[levelnum-1][0]-'a');
	  putletter(1435,codes[levelnum-1][1]-'a');
	  putletter(1444,codes[levelnum-1][2]-'a');
	  putletter(1453,codes[levelnum-1][3]-'a');
	  putletter(1462,codes[levelnum-1][4]-'a');
	  putletter(1471,codes[levelnum-1][5]-'a');  

	  sx[nr]=Lsx[nr];  /* unnecessary??*/
	  sy[nr]=Lsy[nr];
	  
	  x=sx[nr] >> STEP;
	  y=sy[nr] >> STEP;
	  
	  sa[nr]=0;
	  sVx[nr]=sVy[nr]=sVg[nr]=1;  /* Shouldn't ever be Zero(?)*/
	  sA[nr]=0;
	  
	  getbox(Lsx[nr] >> STEP, Lsy[nr] >> STEP, backptr);
	  
	  shipptr=ship[num][0];
	  for(n=0; n<=1023; n++)
	    shipmix[n]=shipptr[n] ? shipptr[n] : backptr[n];
	  
	  
	  wflag=1; /* Do not plot */

	  
	  putscore(nr,win_y);
	  tmp1=(sy[nr] >> STEP)-240;
	  if (tmp1<16)
	    tmp1=16;
	  else
	    if (tmp1>960)
	      tmp1=960;
	  gl_setdisplaystart(0,tmp1);

	  for (pal_m=0; pal_m<=767; pal_m++) {
	    p3[pal_m]=63;
	    p0[pal_m]=0;
	    p2[pal_m]=63-palB[pal_m];	   
	    p2B[pal_m]=63-pal[pal_m];
	  }

	  if (levelnum&1) {

	    for(pal_n=16; pal_n<=1008; pal_n+=32) {
	      for (pal_m=0; pal_m<=767; pal_m++)
		p1[pal_m]=63-(((long)p2B[pal_m])*pal_n)>>10;
	      vga_waitretrace();
	      gl_setpalette(p1);
	    }
	    gl_setpalette(pal);
	    
	  } 
	  else {
	    for(pal_n=16; pal_n<=1008; pal_n+=32) {
              for (pal_m=0; pal_m<=767; pal_m++)
                p1[pal_m]=63-(((long)p2[pal_m])*pal_n)>>10;
              vga_waitretrace();
              gl_setpalette(p1);
            }
            gl_setpalette(palB);
	  }

	  /* ANIMATE THE SHIP INTO THE NEW LEVEL */

	  for(n=0; n<=N_ACTION; n++) {
	    if (!action[n].state) {
	      action[n].state=TRUE;
	      action[n].x=(Lsx[nr] >> STEP)+16;
	      action[n].y=(Lsy[nr] >> STEP)+16;
	      action[n].start=157;
	      action[n].stop=162;
	      action[n].frame=157;
	      action[n].speed=6;
	      action[n].delay=6;
	      m=n;
	      goto nomoreloop;
	    }
	  }
	nomoreloop:

	  /* Roll the SplitScreen Up   */
	  for(sl=480; sl>=464; sl--) {
	    outb(0x18,0x3d4);       /* LineCompare */
	    outb(sl&255,0x3d5);        
	    outb(7,0x3d4);          /* Overflow    */
	    reg=inb(0x3d5);         
	    outb(reg|((sl&256)>>4),0x3d5);
	    outb(9,0x3d4);          /* Maximum Scanline */
	    reg=inb(0x3d5);          
	    outb(reg|((sl&512)>>3),0x3d5); 
	    vga_waitretrace();
	  }


	  for(n=0; n<=35; n++) {
	    AnimateAll();
	  }
	  action[m].state=FALSE;
 	  putbox(Lsx[nr] >> STEP, Lsy[nr] >> STEP, shipmix);

	  getbox(old_x, old_y,shipback[nr]);
	 
	  stampnum(levelnum);
	  
	  while (keyboard_update()) {
            AnimateAll();
            putstamp(272,win_y+184,68,69,70);
	  }
	  while (!keyboard_update()) {
	    AnimateAll();
	    putstamp(272,win_y+184,68,69,70);
	  }
	  killstamp(272,win_y+184);

	  for(n=0; n<=4095; n++) {
	    score[n]=tmpscore[n];
	  } 

	  AnimateAll(); /* Activate the new Score again */

	  while (keyboard_update()==0);


      	  break;

	}

	wipe.active=255; /* Not Active */
	
	if (blocktype==E_BONUS)  {
	  beep(15000);

	  backptr=shipback[nr];

	  tmp1=37+(rand()&1);
	  tmp2=lx+(ly<<4)+(ly<<2);

	  level[tmp2]=tmp1;
	  objects[tmp2]=E_NULL;
	  getbox(x,y, backptr);

	  wipe.x=lx << 5;
	  wipe.y=ly << 5;
	  wipe.active=tmp1;
	}
	else
	  if (blocktype==E_WBONUS)  {
	    beep(15000);

	    backptr=shipback[nr];
 
	    tmp2=lx+(ly<<4)+(ly<<2);
	    
	    level[tmp2]=204;
	    objects[tmp2]=E_WATER;
	    getbox(x,y, backptr);
            watermask(x,y,E_WATER,backptr);

	    wipe.x=lx << 5;
	    wipe.y=ly << 5;
	    wipe.active=204; 	    
	  }
      

	if (NumKeys==0) {          /* == should be enough... */
	  NumKeys--;
	  level[stop_x+(stop_y<<4)+(stop_y<<2)]=41;
	  objects[stop_x+(stop_y<<4)+(stop_y<<2)]='x';
          putbox(stop_x<<5, stop_y<<5, block[41]);
	}

      }
      else {
	if (thrust_len>lift_thrust) {
	  sVy[nr]-=128;
	  ship_flag=ship_flag & 0xfe;
	  fueling=FALSE;
	}
      } 
    }

    if ((fueling) && ((ShipTime&7)==0)) {
      ShipFuel+=16;
      if (ShipFuel>1599) ShipFuel=1599;
      updatescore();
    } 
        
    
  Explode:
  }  

  if (wflag<2) {
    vga_waitretrace(); 

    if (wipe.active!=255) 
      putbox(wipe.x,wipe.y,block[wipe.active]); 	    

    putbox(old_x, old_y, shipback[nr]); 
    putbox(x, y, shipmix);

    for(n=0; n<=1023; n++) shipback[nr][n]=backptr[n];
  }
}


