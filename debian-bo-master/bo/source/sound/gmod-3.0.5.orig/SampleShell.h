// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef __SampleShell_h
#define __SampleShell_h

#include "defines.h"
#include "structs.h"
#include "Sample.h"

#include <qapp.h>
#include <qlistbox.h>
#include <qpushbt.h>

class SampleShell : public QWidget
{
  Q_OBJECT
public:
  SampleShell(QWidget *w = 0);
  void setSamples(Sample **, int);
  
 public slots:
 void showSampleShell();

protected:
  void resizeEvent(QResizeEvent *);

private:
  QListBox *sampleList;
  QPushButton *closeButton;

 private slots:
 void closeSampleShell();
};

#endif
