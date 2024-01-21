// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef CURSESSCR_H
#define CURSESSCR_H

#ifdef USE_NCURSES
#include <curses.h>
#else
#include <termios.h>
#endif

class Sample;

class CursesScr
{
public:
  CursesScr(unsigned char);
  ~CursesScr();

  void setFile(char *);
  void setInfo(char *, char *, char *, unsigned char, unsigned int, short);
  void setSamples(Sample *[], unsigned int, unsigned char);
  void setMem(int, int);
  void setPos(unsigned int, unsigned int);
  int getChar();

#ifdef USE_NCURSES
  void scrollSamples(int);
#endif

private:
  unsigned char background_;
#ifdef USE_NCURSES
  int sampleLines_;
  unsigned int nrSamples_;
  unsigned char showEmpty_;
  int firstVisible_;

  WINDOW *infoWin;
  WINDOW *sampleWin;
  WINDOW *memWin;
  WINDOW *posWin;
#else
  long old_flags_;
  struct termios old_termio_;
#endif
};

inline int
CursesScr::getChar()
{
  if (!background_)
    {
#ifdef USE_NCURSES
      return wgetch(posWin);
#else
      return getchar();
#endif
    }
  else
    return EOF;
}

#endif
