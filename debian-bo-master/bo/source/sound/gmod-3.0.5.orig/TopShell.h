// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef __TopShell_h
#define __TopShell_h

#include <qapp.h>
#include <qlabel.h>
#include <qlcdnum.h>
#include <qpixmap.h>
#include <qpushbt.h>
#include <qwidget.h>

class CommentShell;
class OptShell;
class QueueShell;
class TrackShell;
class SampleShell;

class TopShell : public QWidget
{
  Q_OBJECT

  signals:
  void channelSignal(int);

public:
  TopShell(CommentShell *, OptShell *, QueueShell *, SampleShell *, TrackShell *);
  void moduleFile(const char *);
  void moduleTitle(const char *);
  void setPosition(int);
  void setChannels(int);
  void setMaxPosition(int);
  void doNext(int);

private:
  void pick_random();

  static const char *back_xbm;
  static const char *forward_xbm;
  static const char *previous_xbm;
  static const char *next_xbm;
  static const char *play_xbm;
  static const char *stop_xbm;

  QLabel *fileLabel;
  QLabel *nameLabel;
  
  QLCDNumber *positionLcd;
  QLCDNumber *channelLcd;

  QPushButton *button[12];
  QPixmap *pixmaps[6];

  int max_position;
  int do_random;

 private slots:
 void backClicked();
  void forwardClicked();
  void playClicked();
  void nextClicked();
  void previousClicked();
  void stopClicked();
  void setRandom();
};

inline void
TopShell::setMaxPosition(int position)
{
  max_position = position;
}

inline void
TopShell::moduleFile(const char *mod_file)
{
  fileLabel->setText(mod_file);
}

inline void
TopShell::moduleTitle(const char *mod_title)
{
  nameLabel->setText(mod_title);
}

#endif
