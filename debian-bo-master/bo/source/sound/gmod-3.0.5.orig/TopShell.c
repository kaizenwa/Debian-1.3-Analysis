// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <stdlib.h>

#include <qpixmap.h>
#include <qpopmenu.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

#include "Sequencer.h"

#include "CommentShell.h"
#include "OptShell.h"
#include "TopShell.h"
#include "TrackShell.h"
#include "QueueShell.h"
#include "SampleShell.h"

const char *TopShell::back_xbm =
"#define back_width 16
#define back_height 15
static unsigned char back_bits[] = {
   0x80, 0x80, 0xc0, 0xc0, 0xe0, 0xe0, 0xf0, 0xf0, 0xf8, 0xf8, 0xfc, 0xfc,
   0xfe, 0xfe, 0xff, 0xff, 0xfe, 0xfe, 0xfc, 0xfc, 0xf8, 0xf8, 0xf0, 0xf0,
   0xe0, 0xe0, 0xc0, 0xc0, 0x80, 0x80};";

const char *TopShell::forward_xbm =
"#define forward_width 16
#define forward_height 15
static unsigned char forward_bits[] = {
   0x01, 0x01, 0x03, 0x03, 0x07, 0x07, 0x0f, 0x0f, 0x1f, 0x1f, 0x3f, 0x3f,
   0x7f, 0x7f, 0xff, 0xff, 0x7f, 0x7f, 0x3f, 0x3f, 0x1f, 0x1f, 0x0f, 0x0f,
   0x07, 0x07, 0x03, 0x03, 0x01, 0x01};";

const char *TopShell::previous_xbm =
"#define previous_width 18
#define previous_height 15
static unsigned char previous_bits[] = {
   0x03, 0x02, 0x02, 0x03, 0x03, 0x03, 0x83, 0x83, 0x03, 0xc3, 0xc3, 0x03,
   0xe3, 0xe3, 0x03, 0xf3, 0xf3, 0x03, 0xfb, 0xfb, 0x03, 0xff, 0xff, 0x03,
   0xfb, 0xfb, 0x03, 0xf3, 0xf3, 0x03, 0xe3, 0xe3, 0x03, 0xc3, 0xc3, 0x03,
   0x83, 0x83, 0x03, 0x03, 0x03, 0x03, 0x03, 0x02, 0x02};";

const char *TopShell::next_xbm =
"#define next_width 18
#define next_height 15
static unsigned char next_bits[] = {
   0x01, 0x01, 0x03, 0x03, 0x03, 0x03, 0x07, 0x07, 0x03, 0x0f, 0x0f, 0x03,
   0x1f, 0x1f, 0x03, 0x3f, 0x3f, 0x03, 0x7f, 0x7f, 0x03, 0xff, 0xff, 0x03,
   0x7f, 0x7f, 0x03, 0x3f, 0x3f, 0x03, 0x1f, 0x1f, 0x03, 0x0f, 0x0f, 0x03,
   0x07, 0x07, 0x03, 0x03, 0x03, 0x03, 0x01, 0x01, 0x03};";

const char *TopShell::play_xbm =
"#define play_width 15
#define play_height 15
static unsigned char play_bits[] = {
  0x08, 0x00, 0x18, 0x00, 0x38, 0x00, 0x78, 0x00, 0xf8, 0x00, 0xf8, 0x01,
  0xf8, 0x03, 0xf8, 0x07, 0xf8, 0x03, 0xf8, 0x01, 0xf8, 0x00, 0x78, 0x00,
  0x38, 0x00, 0x18, 0x00, 0x08, 0x00, };";

const char *TopShell::stop_xbm =
"#define stop_width 15
#define stop_height 15
static unsigned char stop_bits[] = {
   0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f,
   0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f,
   0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f};";

struct info_array
{
  char *name;
  const char *pixmap;
  int size;
  int x, y, width, height;
};

TopShell::TopShell(CommentShell *commentShell, OptShell *optShell,
		   QueueShell *queueShell, SampleShell *sampleShell,
		   TrackShell *trackShell)
 : QWidget(0, "topShell"), do_random(1)
{
  struct info_array button_info[12] = {
    {"backButton", back_xbm, 272, 12, 80, 30, 30},
    {"forwButton", forward_xbm, 281, 47, 80, 30, 30},
    {"prevButton", previous_xbm, 377, 83, 80, 30, 30},
    {"nextButton", next_xbm, 365, 117, 80, 30, 30},
    {"playButton", play_xbm, 271, 152, 80, 30, 30},
    {"stopButton", stop_xbm, 272, 187, 80, 30, 30},
    {"queueButton", "Queue", 0, 20, 115, 60, 25},
    {"optionButton", "Options", 0, 85, 115, 60, 25},
    {"trackButton", "Track", 0, 150, 115, 60, 25},
    {"sampleButton", "Samples", 0, 20, 145, 60, 25},
    {"commentButton", "Comment", 0, 85, 145, 60, 25},
    {"exitButton", "Exit", 0, 150, 145, 60, 25}
  };

  int i, j = 0;

  setCaption(HEADING);
  setMinimumSize(230, 175);
  setMaximumSize(230, 175);
  resize(230, 175);

  fileLabel = new QLabel(this, "fileLabel");
  fileLabel->setText(HEADING);
  fileLabel->setAlignment(AlignCenter);
  fileLabel->setFrameStyle(QFrame::Panel | QFrame::Sunken);
  fileLabel->setGeometry(5, 5, 220, 20);

  nameLabel = new QLabel(this, "nameLabel");
  nameLabel->setText("Copyright 1997 by Andrew J Robinson");
  nameLabel->setAlignment(AlignCenter);
  nameLabel->setFrameStyle(QFrame::Panel | QFrame::Sunken);
  nameLabel->setGeometry(5, 25, 220, 20);

  positionLcd = new QLCDNumber(7, this);
  positionLcd->display("EEE:000");
  positionLcd->setGeometry(12, 50, 112, 25);
  positionLcd->setFrameStyle(QFrame::Panel | QFrame::Sunken);

  channelLcd = new QLCDNumber(5, this);
  channelLcd->display("00 CH");
  channelLcd->setGeometry(136, 50, 80, 25);
  channelLcd->setFrameStyle(QFrame::Panel | QFrame::Sunken);

  for (i = 0; i < 12; i++)
    {
      button[i] = new QPushButton(this, button_info[i].name);
      button[i]->setGeometry(button_info[i].x, button_info[i].y,
			     button_info[i].width, button_info[i].height);
      
      if (button_info[i].size == 0)
	button[i]->setText(button_info[i].pixmap);
      else
	{
	  pixmaps[j] = new QPixmap;
	  pixmaps[j]->loadFromData((const uchar *)button_info[i].pixmap, button_info[i].size);
	  button[i]->setPixmap(*pixmaps[j]);
	  j++;
	}
    }

  connect(button[0], SIGNAL(clicked()), this, SLOT(backClicked()));
  connect(button[1], SIGNAL(clicked()), this, SLOT(forwardClicked()));
  connect(button[2], SIGNAL(clicked()), this, SLOT(previousClicked()));
  connect(button[3], SIGNAL(clicked()), this, SLOT(nextClicked()));
  connect(button[4], SIGNAL(clicked()), this, SLOT(playClicked()));
  connect(button[5], SIGNAL(clicked()), this, SLOT(stopClicked()));
  connect(button[6], SIGNAL(clicked()), queueShell, SLOT(showQueueShell()));
  connect(button[7], SIGNAL(clicked()), optShell, SLOT(showOptShell()));
  connect(button[8], SIGNAL(clicked()), trackShell, SLOT(showTrackShell()));
  connect(button[9], SIGNAL(clicked()), sampleShell, SLOT(showSampleShell()));
  connect(button[10], SIGNAL(clicked()), commentShell, SLOT(showCommentShell()));
  connect(button[11], SIGNAL(clicked()), qApp, SLOT(quit()));
  connect(this, SIGNAL(channelSignal(int)), trackShell, SLOT(setChannels(int)));

  connect(queueShell, SIGNAL(currentDeleted()), this, SLOT(setRandom()));
}

void
TopShell::setRandom()
{
  do_random = 1;
}

void
TopShell::setPosition(int position)
{
  char positionDisplay[8];

  if (position < 0)
    sprintf(positionDisplay, "EEE:%03d", max_position);
  else
    sprintf(positionDisplay, "%03d:%03d", position, max_position);

  positionLcd->display(positionDisplay);
}

void
TopShell::setChannels(int channels)
{
  char channelDisplay[6];

  sprintf(channelDisplay, "%02d CH", channels);
  channelLcd->display(channelDisplay);

  emit(channelSignal(channels));
}

void
TopShell::doNext(int skip_error)
{
  extern Sequencer *seq;
  extern OptShell *optShell;
  extern QueueShell *queueShell;
  int start_ok = 0;

  if (current_mod >= 0)
    {
      seq->stop_playback ();
      end_module (0);

      if (optShell->removeChecked() == TRUE)
	queueShell->removeModule(current_mod);
      else
	current_mod++;
    }
  else
    current_mod = -current_mod;

  do {
    if (current_mod >= x_info.nrFileStrings)
      current_mod = -1;

    pick_random ();

    if (current_mod >= 0)
      {
	if (!(start_ok = start_playback (0)))
	  {
	    if (skip_error)
	      queueShell->removeModule(current_mod);
	    else
	      current_mod = -current_mod - 1;
	  }
      }
  } while (skip_error && !start_ok && (current_mod >= 0));
}

void
TopShell::nextClicked()
{
  doNext(0);
}

void
TopShell::previousClicked()
{
  extern Sequencer *seq;
  extern OptShell *optShell;
  extern QueueShell *queueShell;

  if (current_mod >= 0)
    {
      seq->stop_playback();
      end_module (0);
    }

  if (current_mod < 0)
    current_mod = -current_mod - 2;
  else
    {
      if (optShell->removeChecked() == TRUE)
	queueShell->removeModule(current_mod);

      if (current_mod >= 0)
	current_mod--;
    }

  pick_random ();
  
  if (current_mod >= 0)
    if (!start_playback (0))
      current_mod = -current_mod - 1;
}

void
TopShell::forwardClicked()
{
  extern int start_pos;
  extern Sequencer *seq;

  if (current_mod >= 0)
    {
      seq->stop_playback();
      end_module(STOP_FORWBACK);
      start_pos = actual_pos + 1;
      start_playback(STOP_FORWBACK);
    }
}

void
TopShell::backClicked()
{
  extern int start_pos;
  extern Sequencer *seq;

  if (current_mod >= 0)
    {
      seq->stop_playback();
      end_module(STOP_FORWBACK);

      if (actual_pos > 0)
	start_pos = actual_pos - 1;
      else
	start_pos = 0;

      start_playback(STOP_FORWBACK);
    }
}

void
TopShell::stopClicked()
{
  extern Sequencer *seq;

  if (current_mod >= 0)
    {
      seq->stop_playback();
      end_module(0);
      current_mod = -current_mod - 1;
      do_random = 0;
    }
}

void
TopShell::playClicked()
{
  extern Sequencer *seq;
  
  if (current_mod < 0)
    {
      current_mod = -current_mod - 1;

      if (do_random)
	{
	  pick_random();
	  do_random = 0;
	}
      
      if (current_mod >= x_info.nrFileStrings)
	current_mod = -1;
    }
  else
    {
      seq->stop_playback();
      end_module(0);
    }

  if (current_mod >= 0)
    if (!start_playback (0))
      current_mod = -current_mod - 1;
}

void
TopShell::pick_random(void)
{
  extern OptShell *optShell;

  if (x_info.nrFileStrings > 0)
    {
      if (optShell->randomChecked() == TRUE)
	current_mod = rand () % x_info.nrFileStrings;
    }
}

#include "TopShell.moc"
