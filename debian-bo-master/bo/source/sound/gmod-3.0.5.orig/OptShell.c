// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <String.h>
#include <fstream.h>
#include <qmsgbox.h>
#include <stdlib.h>

#include "OptShell.h"

OptShell::OptShell(QWidget *w) : QWidget(w, "optShell")
{
  setMinimumSize(380, 240);
  setMaximumSize(380, 240);
  resize(380, 240);
  setCaption("Xgmod Options");

  playGroup = new QButtonGroup(this, "playGroup");
  playGroup->setTitle("Play Options");

  playCheckBox[0] = new QCheckBox(playGroup);
  playCheckBox[0]->setText("Break Infinite Loops");
  playCheckBox[0]->setGeometry(10,15,160,25);
  playCheckBox[1] = new QCheckBox(playGroup);
  playCheckBox[1]->setText("Ignore Speed0 Commands");
  playCheckBox[1]->setGeometry(10,45,160,25);

  playGroup->setGeometry(195,125,180,75);

  loadGroup = new QButtonGroup(this, "loadGroup");
  loadGroup->setTitle("Load Options");

  loadCheckBox[0] = new QCheckBox(loadGroup);
  loadCheckBox[0]->setText("50 Hz Clock Frequency");
  loadCheckBox[0]->setGeometry(10,15,160,25);
  loadCheckBox[1] = new QCheckBox(loadGroup);
  loadCheckBox[1]->setText("NTSC Sample Timing");
  loadCheckBox[1]->setGeometry(10,45,160,25);
  loadCheckBox[2] = new QCheckBox(loadGroup);
  loadCheckBox[2]->setText("Disable BPM Tempos");
  loadCheckBox[2]->setGeometry(10,75,160,25);

  loadGroup->setGeometry(5,5,180,110);

  queueGroup = new QButtonGroup(this, "loadGroup");
  queueGroup->setTitle("Queue Options");

  queueCheckBox[0] = new QCheckBox(queueGroup);
  queueCheckBox[0]->setText("Unqueue After Play");
  queueCheckBox[0]->setGeometry(10,15,160,25);
  queueCheckBox[1] = new QCheckBox(queueGroup);
  queueCheckBox[1]->setText("Random Order");
  queueCheckBox[1]->setGeometry(10,45,160,25);
  queueCheckBox[2] = new QCheckBox(queueGroup);
  queueCheckBox[2]->setText("Highlight Current");
  queueCheckBox[2]->setGeometry(10,75,160,25);

  queueGroup->setGeometry(5,125,180,110);

  octaveGroup = new QButtonGroup(this, "octaveGroup");
  octaveGroup->setTitle("Octave Selection");
  
  octaveButton[0] = new QRadioButton(octaveGroup);
  octaveButton[0]->setText("Limited");
  octaveButton[0]->setGeometry(10, 15, 160, 25);
  octaveButton[1] = new QRadioButton(octaveGroup);
  octaveButton[1]->setText("Extended");
  octaveButton[1]->setGeometry(10, 45, 160, 25);
  octaveButton[2] = new QRadioButton(octaveGroup);
  octaveButton[2]->setText("Automatic");
  octaveButton[2]->setGeometry(10, 75, 160, 25);

  octaveGroup->setGeometry(195, 5, 180, 110);

  saveButton = new QPushButton(this, "saveButton");
  saveButton->setText("Save");
  saveButton->setGeometry(220, 210, 60, 25);
  connect(saveButton, SIGNAL(clicked()), this, SLOT(saveClicked()));

  closeButton = new QPushButton(this, "closeButton");
  closeButton->setText("Close");
  closeButton->setGeometry(285,210,60,25);
    
  connect(closeButton, SIGNAL(clicked()), SLOT(closex()));

  if (!loadOptions())
    octaveButton[0]->setChecked(TRUE);
}

int
OptShell::octaveSelected() const
{
  int i;

  for (i = 0; i < 3; i++)
    if (octaveButton[i]->isChecked() == TRUE)
      return i;

  return 0;
}

void
OptShell::showOptShell()
{
  show();
}

void
OptShell::saveClicked()
{
  String filename;
  int i;

  filename = getenv("HOME");
  filename += "/.xgmodrc";

  ofstream outStream(filename);

  if (!outStream)
    {
      filename.prepend("Cannot open\n");
      QMessageBox::message("Xgmod Error", filename, 0, this);
      return;
    }

  for (i = 0; i < 2; i++)
    {
      if (playCheckBox[i]->isChecked() == TRUE)
	outStream << '1';
      else
	outStream << '0';
    }

  for (i = 0; i < 3; i++)
    {
      if (loadCheckBox[i]->isChecked() == TRUE)
	outStream << '1';
      else
	outStream << '0';
    }

  for (i = 0; i < 3; i++)
    {
      if (queueCheckBox[i]->isChecked() == TRUE)
	outStream << '1';
      else
	outStream << '0';
    }
  
  outStream << (char)(octaveSelected() + '0');

  if (!outStream)
    {
      filename.prepend("Error writing\n");
      QMessageBox::message("Xgmod Error", filename, 0, this);
      return;
    }

  outStream.close();
}

int
OptShell::loadOptions()
{
  String filename;
  int i;
  char input;

  filename = getenv("HOME");
  filename += "/.xgmodrc";

  ifstream inStream(filename);

  if (!inStream)
    {
      filename.prepend("Cannot open\n");
      QMessageBox::message("Xgmod Error", filename);
      return 0;
    }

  for (i = 0; i < 2; i++)
    {
      inStream >> input;

      if (input == '1')
	playCheckBox[i]->setChecked(TRUE);
    }

  for (i = 0; i < 3; i++)
    {
      inStream >> input;

      if (input == '1')
	loadCheckBox[i]->setChecked(TRUE);
    }

  for (i = 0; i < 3; i++)
    {
      inStream >> input;

      if (input == '1')
	queueCheckBox[i]->setChecked(TRUE);
    }
  
  inStream >> input;

  input -= '0';
  octaveButton[input]->setChecked(TRUE);

  if (!inStream)
    {
      filename.prepend("Error reading\n");
      QMessageBox::message("Xgmod Error", filename);
      return 0;
    }

  inStream.close();

  return 1;
}

#include "OptShell.moc"
