// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef __OptShell_h
#define __OptShell_h

#include <qapp.h>
#include <qbttngrp.h>
#include <qchkbox.h>
#include <qpushbt.h>
#include <qradiobt.h>
#include <qwidget.h>

class OptShell : public QWidget
{
  Q_OBJECT
public:
  OptShell(QWidget *w = 0);
  
  bool highlightChecked() const;
  bool fiftyhzChecked() const;
  bool ntscChecked() const;
  bool bpmChecked() const;
  bool speed0Checked() const;
  bool loopBreakChecked() const;
  bool randomChecked() const;
  bool removeChecked() const;
  bool absoluteChecked() const;
  int octaveSelected() const;

 public slots:
 void showOptShell();

private:
  int loadOptions();

  QButtonGroup *playGroup;
  QButtonGroup *loadGroup;
  QButtonGroup *octaveGroup;
  QButtonGroup *queueGroup;

  QCheckBox *playCheckBox[2];
  QCheckBox *loadCheckBox[3];
  QCheckBox *queueCheckBox[3];
  QRadioButton *octaveButton[3];

  QPushButton *saveButton;
  QPushButton *closeButton;

 private slots:
 void closex();
  void saveClicked();
};

inline bool OptShell::highlightChecked() const
{
  return queueCheckBox[2]->isChecked();
}

inline bool OptShell::fiftyhzChecked() const
{
  return loadCheckBox[0]->isChecked();
}

inline bool OptShell::ntscChecked() const
{
  return loadCheckBox[1]->isChecked();
}

inline bool OptShell::bpmChecked() const
{
  return loadCheckBox[2]->isChecked();
}

inline bool OptShell::speed0Checked() const
{
  return playCheckBox[1]->isChecked();
}

inline bool OptShell::loopBreakChecked() const
{
  return playCheckBox[0]->isChecked();
}

inline bool OptShell::randomChecked() const
{
  return queueCheckBox[1]->isChecked();
}

inline bool OptShell::removeChecked() const
{
  return queueCheckBox[0]->isChecked();
}

inline bool OptShell::absoluteChecked() const
{
  return queueCheckBox[3]->isChecked();
}

inline void OptShell::closex()
{
  close();
}

#endif
