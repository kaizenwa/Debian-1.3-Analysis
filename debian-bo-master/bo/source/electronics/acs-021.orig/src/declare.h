/*$Id: declare.h,v 11.22 96/02/18 11:46:01 al Exp $ -*- C++ -*-
 * global functions
 * A remnant of the old C code
 */
#include "md.h"
#ifndef DECLARE_H
#define DECLARE_H
/*--------------------------------------------------------------------------*/
class CS;
class CARD;
/*--------------------------------------------------------------------------*/
		void	new_event(double);
/* compiler */	void	  initialize_io();
		void	  setup_traps();
		void	  shell();
/* ac_fix   */	void	  acfix(CARD*);
/* crtset   */	int	  testcrt();
		struct graph *initcrt();
		void      expandsubckt(CARD*,const char*);
/* fft      */	void	  fft(COMPLEX*,int,int);
/* c_file   */	char	  *getcmd(const char*,char*,int);
/* findbr   */	CARD  *findbranch(CS&,CARD*,CARD*);
	 	CARD  *findbranch_samescope(const char*,CARD*);
	  const CARD  *findbranch_sametype(const char*,const CARD*);
/* generat  */	double	  gen();
/* line     */	void	  initgraph(struct graph*);
 		void	  stext(int,int,const char*,int);
		void	  setpixel(int,int,int);
		void	  box(int,int,int,int,int);
		void	  line(int,int,int,int,int);
/* main     */	int	  main(int,const char*[]);
/* nodes    */	int	  newnode_subckt();
		int	  newnode_model();
/* plot     */	void	  plottr(double);
		int	  plopen(int,double,double,bool);
		void	  plclose();
		void	  plclear();
		void	  pllocate();
/* tr_fix   */	void	  trfix(CARD*);
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
