// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef __TrackShell_h
#define __TrackShell_h

#include <qapp.h>
#include <qlistbox.h>
#include <qpushbt.h>

#include "defines.h"  //
#include "structs.h"  // for "pattern" type

class TrackShell : public QWidget
{
  Q_OBJECT
public:
  TrackShell(QWidget *w = 0);
  void insertLine(const char *);
  void setCurrent(int);
  void updateTracker(int, int, short [MAX_POSITION][MAX_TRACK],
		     struct note_info *[MAX_PATTERN * MAX_TRACK]);

 public slots:
 void showTrackShell();
  void setChannels(int);

protected:
  void resizeEvent(QResizeEvent *);

private:
  QListBox *patternList;
  QPushButton *closeButton;
  int nrChannels;

 private slots:
 void closeTrackShell();
};

inline void
TrackShell::setCurrent(int current)
{
  patternList->setCurrentItem(current);
  patternList->centerCurrentItem();
}

#endif
