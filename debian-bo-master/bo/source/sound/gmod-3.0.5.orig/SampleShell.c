// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <stdio.h>

#include "SampleShell.h"

SampleShell::SampleShell(QWidget *w) : QWidget(w, "sampleShell")
{
  setMinimumSize(70, 40);
  setCaption("Xgmod Samples");

  sampleList = new QListBox(this, "sampleList");
  sampleList->setGeometry(5, 5, 200, 160);
  
  closeButton = new QPushButton(this, "closeButton");
  closeButton->setText("Close");
  closeButton->setGeometry(75, 200, 60, 25);
  connect(closeButton, SIGNAL(clicked()), this, SLOT(closeSampleShell()));

  resize(210, 200);
}

void
SampleShell::showSampleShell()
{
  show();
}

void
SampleShell::closeSampleShell()
{
  close();
}

void
SampleShell::resizeEvent(QResizeEvent *)
{
  closeButton->move((width() - 60) / 2, height() - 30);
  sampleList->resize(width() - 10, height() - 40);
}

void
SampleShell::setSamples(Sample **samples, int nr_samples)
{
  int i;
  char sampleName[SAMPNAME_LEN + 6];

  sampleList->setAutoUpdate(FALSE);
  sampleList->clear();

  for (i = 0; i < nr_samples; i++)
    {
      if (samples[i]->ok())
	sprintf(sampleName, "[%03d] ", i);
      else
	strcpy(sampleName, "[xxx] ");

      strcat(sampleName, samples[i]->name());
      sampleList->insertItem(sampleName);
    }

  sampleList->update();
  sampleList->setAutoUpdate(TRUE);
}

#include "SampleShell.moc"
