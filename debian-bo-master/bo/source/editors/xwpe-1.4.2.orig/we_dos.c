/* we_dos.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"
#ifdef DOS
#include <dos.h>
#include <dir.h>
#include <conio.h>


/*   Mouse-Grundfunktion    */

#if  MOUSE
int fk_mouse(int g[])
{
   extern int fk__mouse_stat;
   union REGS cpu;
   if (fk__mouse_stat==0) return(0);
   if (fk__mouse_stat==1)
   {  cpu.x.ax=0;
      int86(0x33,&cpu,&cpu);
      if (cpu.x.ax==0)
      {  fk__mouse_stat=0;
	 return(-1);  }
      fk__mouse_stat=2;
   }
   cpu.x.ax=g[0];
   cpu.x.bx=g[1];
   cpu.x.cx=g[2];
   cpu.x.dx=g[3];
   int86(0x33,&cpu,&cpu);
   g[0]=cpu.x.ax;
   g[1]=cpu.x.bx;
   g[2]=cpu.x.cx;
   g[3]=cpu.x.dx;
   return(1);
}
#endif

/*  Cursor Ein- und Ausschalten   */

int fk_cursor(int sw)
{
   if(sw == 0) _setcursortype(_NOCURSOR);
   else _setcursortype(_NORMALCURSOR);
   return(0);
}

/*  Cursor lokalisieren  */

int fk_locate(int spalte, int zeile)
{
   extern int fk__cursor;
   union REGS cpu;
   fk__cursor = 160*zeile + 2*spalte;
   cpu.h.bh=0;
   cpu.h.ah=0x02;
   cpu.h.dh=zeile;
   cpu.h.dl=spalte;
   int86(0x10,&cpu,&cpu);
   return(0);
}

/*   Laufwerke testen   */

int fk_lfw_test( int disk)
{
   union REGS cpu;
   char s[80];
   cpu.h.ah = 0x44;
   cpu.h.al = 0x0e;
   cpu.h.bl = 1+disk;
   intdos(&cpu, &cpu);
   if(!cpu.x.cflag || cpu.x.ax == 1) return(cpu.h.al);
   disk = getcurdir(disk+1, s);
   return(disk);
}

/*   String auf Schirm schreiben   */
/*
fk_puts(char *s, int xa, int ya, int fgc, int bgc)
{
     extern char far *schirm;
     int i, frb = 16 * bgc + fgc;
     int anf = 160 * ya + 2 * xa;
     if(xa >= MAXSCOL || ya >= MAXSLNS) return(-1);
     for(i = 0; s[i] != '\0' && i < 2000; i++)
     {   schirm[anf + 2*i] = s[i];
	 schirm[anf + 2*i + 1] = frb;
     }
     return(0);
}
*/
/*   Auf Laufwerk aufschalten (getestet)   */

int fk_setdisk(int n)
{
   char s[80];
   union REGS cpu;
   cpu.h.ah = 0x44;
   cpu.h.al = 0x0e;
   cpu.h.bl = n+1;
   intdos(&cpu, &cpu);
   if(cpu.x.cflag != 0 && cpu.x.ax != 1 && getcurdir(n+1, s) != 0)
   return(cpu.x.ax < 1 ? 1 : cpu.x.ax);
   if(cpu.x.cflag == 0 && cpu.h.al != 0)
   {  cpu.h.ah = 0x44;
      cpu.h.al = 0x0f;
      cpu.h.bl = n+1;
      intdos(&cpu, &cpu);
      if(cpu.x.cflag != 0 && cpu.x.ax != 1) return(cpu.x.ax);
   }
   setdisk(n);
   return(0);
}
/*
	      Schirm initialisieren      */

void e_ini_schirm()
{
   extern char far *schirm;
   union REGS cpu;
   cpu.h.ah = 15;
   int86(0x10, &cpu, &cpu);
   if(cpu.h.al == 2 || cpu.h.al == 7) schirm = (char far *)0xb0000000;
   else if(cpu.h.al == 3) schirm = (char far *)0xb8000000;
   else
   {  cpu.h.ah = 0;
      cpu.h.al = 3;
      int86(0x10, &cpu, &cpu);
      cpu.h.ah = 15;
      int86(0x10, &cpu, &cpu);
      if(cpu.h.al == 3) schirm = (char far *)0xb8000000;
      else
      {  printf("Terminal NOT in the right Videostatus !\n");
	 exit(1);
      }
   }
   cpu.x.ax = 0x5801;  /*   Setzen der Speicherverwaltung  */
   cpu.x.bx = 0x01;    /*   auf Best_Fit                   */
   intdos(&cpu, &cpu);
}

/*  Ein Zeichen auf Schirm schreiben   */
/*    Wird als Makro definiert         */
/*
void e_pr_char(int x, int y, int c, int frb)
{
    extern char far *schirm;
    *(schirm + 160*y + 2*x) = c;
    *(schirm + 160*y + 2*x + 1) = frb;
}
*/
/*
	  Zeichen von Tastatureinlesen     */

int e_get_ctrlc = 0;

int e_getch(void)
{
#if  MOUSE
   extern struct mouse e_mouse;
   extern struct EXT uhr;
   int g[4] = {  1, 0, 0, 0,  };
#endif
   int c = 0, b = 0;
#if  MOUSE
   fk_mouse(g);
   while (c == 0)
   {  if (e_get_ctrlc != 0) {  e_get_ctrlc = 0; return(3);  }
      else if (kbhit() == 0 /* && ungetch(1) != EOF */ )
      {  /*  getch();  */
	 g[0] = 3;
	 fk_mouse(g);
	 if(g[1] != 0)
	 {  c = - g[1];
	    e_mouse.x = g[2]/8;
	    e_mouse.y = g[3]/8;
	    e_mouse.k = 1;
	 }
	 if(uhr.sw != 0) e_uhr();
      }
      else
      {
#endif
	 b = bioskey(1);
	 if( (c = getch()) == 0 )
	 {  c = 255 + getch();
	    if((bioskey(2) & 3) != 0) c = 512 + c ;
	 }
	 else if( c == 255 ) c = e_tst_sim(b);
#if  MOUSE
	 e_mouse.k = 0;
      }
   }
   g[0] = 2;
   fk_mouse(g);
#endif
   return(c);
}
/*
	Tastatur-Simulation (Sondertasten)    */

int e_tst_sim(int b)
{
   switch (b)
   {  case 2815:  return(838);
      case 1791:  return(839);
      case 3327:  return(840);
      case 2047:  return(842);
      case 2303:  return(844);
      case 3071:  return(846);
      case 2559:  return(847);
      case 3583:  return(848);
      case  767:  return(849);
      case 1279:  return(850);
      case  511:  return(851);
      case 1023:  return(852);
   }
   return(0);
}
/*
       Ctrl-Break-tricksen     */

int e_ctrl_break()
{
   e_get_ctrlc = 3;
/*    ungetch(3);   */
   return(3);
}

char far *tmpschirm, far *svschirm;

void ini_repaint(ECNT *cn)
{
   svschirm = schirm;
   if((tmpschirm = (char far *)MALLOC(4000*sizeof(char))) == NULL)
   e_error(e_msg[0], 1, cn->fb);
   else
   {
      schirm = tmpschirm;
      e_cls(cn->fb->df.fb, cn->fb->dc);
      e_ini_desk(cn);
   }
}

void end_repaint()
{
   int n;
   if(tmpschirm != NULL)
   {  schirm = svschirm;
      for(n = 0; n < 160*MAXSLNS; n++)
	    *(schirm + n) = *(tmpschirm + n);
      FREE((char *)tmpschirm);
   }
}


#endif
