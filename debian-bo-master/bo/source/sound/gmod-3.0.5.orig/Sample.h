// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef __Sample_h
#define __Sample_h

#include <stdio.h>

#ifdef USE_NCURSES
#include <curses.h>
#endif

#include "defines.h"

class Sequencer;

class Sample
{
public:
  Sample();
  virtual ~Sample() {};
  virtual int load(Sequencer &, FILE *, int, int, void * = 0, void * = 0) = 0;
  virtual int slide_rate(int, unsigned char) const;
  virtual int hasVolumeEnvelope() const { return 0; };
  virtual int hasPanEnvelope() const { return 0; }
  virtual int volumeEnvelopeY(int &, unsigned short &, char) const
  { return -1; };
  virtual int panEnvelopeY(int &, unsigned short &, char) const
  { return -1; }
  virtual void decode(char *) const {};
  virtual int pan() const { return -1; };
  short finetune() const { return _finetune; };
  void finetune(short ft) { _finetune = ft; };
  unsigned char volume() const { return _volume; };
  const char *name() const { return _name; };
  void get_patch_info(struct patch_info &) const;
  int ok() const { return _ok; };
#ifndef USE_X
#ifdef USE_NCURSES
  void printInfo(WINDOW *, int);
#else
  void printInfo(int, const unsigned char);
#endif
#endif

protected:
  int _length;
  int loop_start;
  int loop_end;
  int _ok;
  short _finetune;
  unsigned char _volume;
  char _name[SAMPNAME_LEN];

  unsigned int _mode;
  unsigned int _base_freq;
  unsigned int _base_note;
};

#ifndef USE_X
#ifdef USE_NCURSES
inline void
Sample::printInfo(WINDOW *win, int i)
{
  char num[4];
  
  if (_ok)
    sprintf(num, "%03d", i);
  else
    strcpy(num, "xxx");

  wprintw(win, "%s: L%06u B%06u E%06u V%03u %s\n",
	  num, _length, loop_start, loop_end, _volume, _name);
}
#else
inline void
Sample::printInfo(int i, unsigned char show_empty)
{
  char num[4];

  if ((_length > 0) || show_empty)
    {
      if (_ok)
	sprinf(num, "%03d", i);
      else
	strcpy(num, "xxx");

      PRINTF("%s: L%06u B%06u E%06u V%03u %s\n",
	     num, _length, loop_start, loop_end, _volume, _name);
    }
}
#endif
#endif

inline int
Sample::slide_rate(int rate, unsigned char slide_dir) const
{
  if (slide_dir == SLIDE_POS)
    return (rate * 256);
  else
    return (-rate * 256);
}

inline
Sample::Sample() : _length(0), loop_start(0), loop_end(0), _ok(0),
  _finetune(0), _volume(0), _mode(0), _base_freq(0), _base_note(0)
{
  _name[0] = '\0';
}

#endif
