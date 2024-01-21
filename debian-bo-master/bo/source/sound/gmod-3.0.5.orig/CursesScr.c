// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <fcntl.h>

#include "CursesScr.h"
#include "Sample.h"

CursesScr::CursesScr(unsigned char background)
{
  background_ = background;

  if (!background)
    {
#ifdef USE_NCURSES
      sampleWin = 0;
      initscr();
      cbreak();
      noecho();
      infoWin = newwin(6, 0, 0, 0);
      memWin = newwin(1, 0, LINES - 2, 0);
      posWin = newwin(1, 0, LINES - 1, 0);
      keypad(posWin, TRUE);
      wtimeout(posWin, 0);
#else
      struct termios new_termio;

      old_flags_ = fcntl(0, F_GETFL);
      fcntl(0, F_SETFL, old_flags_ | O_NONBLOCK);
      tcgetattr(0, &old_termio_);
      new_termio = old_termio_;
      new_termio.c_lflag = 0;
      new_termio.c_cc[VTIME] = 0;
      new_termio.c_cc[VMIN] = 0;
      tcsetattr(0, TCSANOW, &new_termio);             
#endif
    }
}

CursesScr::~CursesScr()
{
  if (!background_)
    {
#ifdef USE_NCURSES
      endwin();
#else
      fcntl(0, F_SETFL, old_flags_);
      tcsetattr(0, TCSANOW, &old_termio_);                               
#endif
    }
}

#ifdef USE_NCURSES
void
CursesScr::scrollSamples(int direction)
{
  if (direction > 0)
    {
      if (firstVisible_ < (nrSamples_ - sampleLines_))
	firstVisible_++;
    }
  else
    {
      if (firstVisible_ > 0)
	  firstVisible_--;
    }

  prefresh(sampleWin, firstVisible_, 0, 6, 0, 6 + sampleLines_ - 1, COLS - 1);
}
#endif

void
CursesScr::setFile(char *fileName)
{
  if (!background_)
    {
#ifdef USE_NCURSES
      werase(infoWin);
      waddstr(infoWin, "File: ");
      waddstr(infoWin, fileName);
      waddstr(infoWin, "\n");
#else
      fputs("\nFile: ", stdout);
      fputs(fileName, stdout);
      fputs("\n", stdout);
#endif
    }
}

void
CursesScr::setInfo(char *name, char *desc, char *comment,
		   unsigned char nrChannels, unsigned int nrPatterns,
		   short songlength)
{
  if (!background_)
    {
#ifdef USE_NCURSES
      waddstr(infoWin, "Name: ");
      waddstr(infoWin, name);
      waddstr(infoWin, "\nType: ");
      waddstr(infoWin, desc);
      waddstr(infoWin, "\nComment: ");
      waddstr(infoWin, comment);
      wprintw(infoWin, "\n\n%u channels, %u patterns, %u positions\n",
	      nrChannels, nrPatterns, songlength);
      wrefresh(infoWin);
#else
      fputs("Name: ", stdout);
      fputs(name, stdout);
      fputs("\nType: ", stdout);
      fputs(desc, stdout);
      fputs("\nComment: ", stdout);
      fputs(comment, stdout);
      printf("\n\n%u channels, %u patterns, %u positions\n",
	     nrChannels, nrPatterns, songlength);
#endif
    }
}

void
CursesScr::setSamples(Sample *samples[], unsigned int nrSamples,
		      unsigned char showEmpty)
{
  if (!background_)
    {
      int i;

#ifdef USE_NCURSES
      if (sampleWin)
	delwin(sampleWin);

      sampleWin = newpad(nrSamples, COLS);
      sampleLines_ = LINES - 8;
      
      if (sampleLines_ > nrSamples)
	sampleLines_ = nrSamples;

      for (i = 0; i < nrSamples; i++)
	samples[i]->printInfo(sampleWin, i);

      nrSamples_ = nrSamples;
      showEmpty_ = showEmpty;

      firstVisible_ = 0;

      prefresh(sampleWin, 0, 0, 6, 0, 6 + sampleLines_ - 1, COLS - 1);
#else
      for (i = 0; i < nrSamples; i++)
	samples[i]->printInfo(i, showEmpty);
#endif  
    }
}

void
CursesScr::setMem(int avail, int total)
{
  if (!background_)
    {
#ifdef USE_NCURSES
      werase(memWin);
      wprintw(memWin, "=== %d bytes of %d total onboard used ===",
	      total - avail, total);
      wrefresh(memWin);
#else
      printf("=== %d bytes of %d total onboard used ===\n", total - avail,
	     total);
#endif
    }
}

void
CursesScr::setPos(unsigned int pos, unsigned int pat)
{
  if (!background_)
    {
#ifdef USE_NCURSES
      mvwprintw(posWin, 0, 0, "Position %03d, Pattern %03d", pos, pat);
      wrefresh(posWin);
#else
      printf("Position %03d, Pattern %03d\r", pos, pat);
      fflush(stdout);
#endif  
    }
}


