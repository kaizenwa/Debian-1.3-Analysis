// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <string>
#include <stdio.h>

#include "TrackShell.h"

TrackShell::TrackShell(QWidget *w) : QWidget(w, "trackShell")
{
  setMinimumSize(70, 40);
  setCaption("Xgmod Tracker");

  patternList = new QListBox(this, "patternList");
  patternList->setGeometry(5, 5, 140, 160);
  patternList->setScrollBar(FALSE);
  patternList->setAutoScrollBar(FALSE);
  patternList->setBottomScrollBar(FALSE);
  patternList->setAutoBottomScrollBar(FALSE);

  closeButton = new QPushButton(this, "closeButton");
  closeButton->setText("Close");
  closeButton->setGeometry(40, 200, 60, 25);
  connect(closeButton, SIGNAL(clicked()), this, SLOT(closeTrackShell()));

  resize(150, 170);
}

void
TrackShell::showTrackShell()
{
  show();
}

void
TrackShell::closeTrackShell()
{
  close();
}

void
TrackShell::resizeEvent(QResizeEvent *)
{
  closeButton->move((width() - 60) / 2, height() - 30);
  patternList->resize(width() - 10, height() - 40);
}

void
TrackShell::setChannels(int channels)
{
  resize((patternList->fontMetrics()).maxWidth() * (channels * 4 + 6) + 20, height());
  nrChannels = channels;
}

void 
TrackShell::updateTracker(int pos, int pattern_len,
			  short voice_table[MAX_POSITION][MAX_TRACK],
			  struct note_info *pattern_table[MAX_PATTERN * MAX_TRACK])
{
  static char *notes[] = {"C-", "C#", "D-", "D#", "E-", "F-", "F#", "G-",
			  "G#", "A-", "A#", "B-"};
  int pat, channel;
  int note, voice, octave;
  char note_str[8];
  string trackString;

  patternList->setAutoUpdate(FALSE);
  patternList->clear();

  for (pat = 0; pat < pattern_len; pat++)
    {
      sprintf (note_str, "[%03d] ", pat);
      trackString = note_str;

      for (channel = 0; channel < nrChannels; channel++)
	{
	  voice = voice_table[pos][channel];
	  note = (pattern_table[voice])[pat].note;
	  
	  if (note == NOTE_STOP)
	    trackString += "STO ";
	  else if ((note == 0) || (note > 127))
	    trackString += "... ";
	  else
	    {
	      octave = note / 12;
	      note = note % 12;
	      sprintf(note_str, "%s%01d ", notes[note], octave);
	      trackString += note_str;
	    }
	}
      
      trackString += '\0';
      patternList->insertItem(trackString.data());
    }
  
  patternList->update();
  patternList->setAutoUpdate(TRUE);
}

#include "TrackShell.moc"
  
