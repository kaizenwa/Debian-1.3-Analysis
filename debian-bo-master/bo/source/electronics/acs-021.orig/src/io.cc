/*$Id: io.cc,v 11.28 96/03/03 23:07:49 al Exp $ -*- C++ -*-
 * shared data for all io functions, initialization, default values
 */
#include "io.h"

int   IO::mstdin = 1;
int   IO::mstdout = 2;
int   IO::mstderr = 2;
int   IO::mprint = 16;
int   IO::where = 0;
int   IO::formaat = 0;
FILE* IO::whence = 0;
bool   IO::suppresserrors = false;
bool   IO::echoflag = false;
bool   IO::printflag = false;
bool   IO::incipher = false;
bool   IO::outcipher = false;
bool   IO::pack = false;
bool   IO::ploton = false;
bool   IO::plotset = false;
FILE *IO::stream[MAXHANDLE+1];
