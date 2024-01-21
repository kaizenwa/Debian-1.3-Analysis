/*$Id: io.h,v 11.22 96/02/18 11:46:23 al Exp $ -*- C++ -*-
 * shared data for all io functions
 * other io related stuff, like files and formatting
 */
#include "md.h"
#ifndef IO_H
#define IO_H
/*--------------------------------------------------------------------------*/
class CS;
/*--------------------------------------------------------------------------*/
class IO {
public:
 static int mstdin;
 static int mstdout;
 static int mstderr;
 static int mprint;
 static int where;		/* where to send data, multi-format */
 static int formaat;		/* how to format io.  Basic option. */
 static FILE *whence;		/* get file from.  std C file. */
 static bool suppresserrors;
 static bool echoflag;		/* not used */
 static bool printflag;		/* not used */
 static bool incipher;		/* flag: decrypt input file */
 static bool outcipher;		/* flag: encrypt output file */
 static bool pack;		/* flag: convert whitespace to tabs on out */
 static bool ploton;
 static bool plotset;
 static FILE *stream[MAXHANDLE+1];	/* reverse of fileno() */
};
/*--------------------------------------------------------------------------*/
/* contrl */	void	initio(int,FILE*);
		void	decipher(char*);
		void	outreset();
		bool	outset(CS&,const char*,const char*);
/* findf */	char*	findfile(const char*,const char*,int);
/* getln */	char* 	getlines(char*,int,FILE*);
/* out */	void	mtab(int,int);
		void	mprintf(int,const char*,...);
		void	mputs(const char*,int);
		void	mputc(int,int);
/* xopen */	void	xclose(FILE**);
		FILE*	xopen(CS&,const char*,const char*);
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
