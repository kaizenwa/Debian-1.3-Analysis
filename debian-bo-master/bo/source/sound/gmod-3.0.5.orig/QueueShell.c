// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include <stdlib.h>
#include <qmsgbox.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

#include "QueueShell.h"
#include "OptShell.h"
#include "Sequencer.h"

struct q_info_array QueueShell::button_info[8] = 
{
  {"addButton", "Add", 0, 60, 60, 25},
  {"saveButton", "Save", 65, 60, 60, 25},
  {"currentButton", "Current", 130, 60, 60, 25},
  {"playButton", "Play", 195, 60, 60, 25},
  {"removeButton", "Remove", 0, 30, 60, 25},
  {"clearButton", "Clear", 65, 30, 60, 25},
  {"shuffleButton", "Shuffle", 130, 30, 60, 25},
  {"closeButton", "Close", 195, 30, 60, 25}
};

QueueShell::QueueShell(QWidget *w) : QWidget(w, "queueShell")
{
  int i;

  setMinimumSize(265, 75);
  setCaption("Xgmod Queue");

  for (i = 0; i < 8; i++)
    {
      buttons[i] = new QPushButton(this, button_info[i].name);
      buttons[i]->setText(button_info[i].label);
      buttons[i]->setGeometry(button_info[i].x + 5, 270 - button_info[i].y,
			      button_info[i].width, button_info[i].height);
    }
		    
  connect (buttons[0], SIGNAL(clicked()), this, SLOT(addClicked()));
  connect (buttons[1], SIGNAL(clicked()), this, SLOT(saveClicked()));
  connect (buttons[2], SIGNAL(clicked()), this, SLOT(currentClicked()));
  connect (buttons[3], SIGNAL(clicked()), this, SLOT(playClicked()));
  connect (buttons[4], SIGNAL(clicked()), this, SLOT(removeClicked()));
  connect (buttons[5], SIGNAL(clicked()), this, SLOT(clearClicked()));
  connect (buttons[6], SIGNAL(clicked()), this, SLOT(shuffleClicked()));
  connect (buttons[7], SIGNAL(clicked()), this, SLOT(closeClicked()));

  queueList = new QListBox(this, "queueList");
  queueList->setGeometry(5, 5, 255, 200);
  connect(queueList, SIGNAL(selected(int)), this, SLOT(playClicked()));

  loadDialog = new XQFileDialog;
  loadDialog->setCaption("Xgmod - Add");
  connect (loadDialog, SIGNAL(fileSelected(const char *)), this,
	   SLOT(loadFileSelected(const char *)));

  saveDialog = new QFileDialog;
  saveDialog->setCaption("Xgmod - Save");
  connect (saveDialog, SIGNAL(fileSelected(const char *)), this,
	   SLOT(saveFileSelected(const char *)));

  resize (265, 270);
}

void
QueueShell::showQueueShell()
{
  show();
}

void
QueueShell::closeClicked()
{
  close();
}

void
QueueShell::resizeEvent(QResizeEvent *)
{
  int i;

  for (i = 0; i < 8; i++)
    buttons[i]->move((width() - 255) / 2 + button_info[i].x, height() - button_info[i].y);

  queueList->resize(width() - 10, height() - 70);
}

void
QueueShell::addClicked()
{
  loadDialog->show();
}

void
QueueShell::saveClicked()
{
  saveDialog->show();
}

void
QueueShell::add_module (int add_place, const char *filename)
{
  if (current_mod >= add_place)
    current_mod++;

  x_info.nrFileStrings++;

  x_info.fileStrings =
    (char * *)realloc (x_info.fileStrings, sizeof (char *) * x_info.nrFileStrings);

  memmove (x_info.fileStrings + add_place + 1,
	   x_info.fileStrings + add_place,
	   (x_info.nrFileStrings - add_place - 1) * sizeof (char *));

  x_info.fileStrings[add_place] = strdup (filename);
  queueList->insertItem(filename, add_place);
}

void
QueueShell::loadFileSelected (const char *filename)
{
  int add_place;
  FILE *fp;
  char buffer[BUFSIZ];

  add_place = queueList->currentItem();

  if (add_place == -1)
    add_place = queueList->count() - 1;

  if ((fp = fopen (filename, "r")) == NULL)
    {
      QMessageBox::message("Xgmod Error", "Cannot open file", 0, loadDialog);
      return;
    }

  fgets (buffer, BUFSIZ, fp);

  queueList->setAutoUpdate(FALSE);

  if (!memcmp (buffer, "@(#)GMOD-PLAYLIST", 17))
    while (fgets (buffer, BUFSIZ, fp) != NULL)
      {
	buffer[strlen (buffer) - 1] = '\0';
	add_place++;
	add_module (add_place, buffer);
      }
  else
    {
      add_place++;
      add_module (add_place, filename);
    }

  fclose (fp);

  queueList->setCurrentItem(add_place);
  queueList->update();
  queueList->setAutoUpdate(TRUE);
}

void
QueueShell::currentClicked()
{
  int highlight_mod;

  if (current_mod < 0)
    highlight_mod = -current_mod - 1;
  else
    highlight_mod = current_mod;

  if (highlight_mod < x_info.nrFileStrings)
    {
      queueList->setCurrentItem(highlight_mod);
      queueList->centerCurrentItem();
    }
  else
    queueList->setCurrentItem(-1);
}

void
QueueShell::playClicked()
{
  int o_current_mod;
  extern Sequencer *seq;
 
  if (queueList->currentItem() != -1)
    {
      o_current_mod = current_mod;
      current_mod = queueList->currentItem();

      // stop playback and deallocate current module
      if (o_current_mod >= 0)
	{
	  seq->stop_playback ();
	  end_module (0);
	}

      // start playback of the new module
      if (!start_playback (0))
	current_mod = -current_mod - 1;
    }
  else
    QMessageBox::message("Xgmod Error", "No module is selected", 0, this);
}

void
QueueShell::saveFileSelected(const char *filename)
{
  FILE *fp;
  int i;

  if ((fp = fopen (filename, "w")) == NULL)
    QMessageBox::message("Xgmod Error", "Cannot open file", 0, saveDialog);
  else
    {
      fprintf (fp, "@(#)GMOD-PLAYLIST\n");

      for (i = 0; i < x_info.nrFileStrings; i++)
	fprintf (fp, "%s\n", (x_info.fileStrings)[i]);

      fclose (fp);
    }
}

void
QueueShell::removeModule(int current)
{
  int highlight = queueList->currentItem();

  queueList->removeItem(current);

  if (highlight != -1)
    {
      if (highlight >= queueList->count())
	highlight--;
      
      queueList->setCurrentItem(highlight);
    }
  
  free (x_info.fileStrings[current]);
  x_info.nrFileStrings--;
  
  if (x_info.nrFileStrings != current)
    memmove (x_info.fileStrings + current,
	     x_info.fileStrings + current + 1,
	     (x_info.nrFileStrings - current) * sizeof (char *));
  
  x_info.fileStrings =
    (char * *)realloc (x_info.fileStrings, sizeof (char *) *
		       x_info.nrFileStrings);
}
  
void
QueueShell::removeClicked()
{
  int current;
  unsigned char lstop_flag = 0;
  //extern TopShell *topShell;
  extern OptShell *optShell;
  extern Sequencer *seq;

  if ((current = queueList->currentItem()) != -1)
    {
      removeModule(current);

      if (current == current_mod)
	{
	  lstop_flag = STOP_GOTO;

	  // stop playback and deallocate current module
	  if (current_mod >= 0)
	    {
	      seq->stop_playback ();
	      end_module (0);
	    }

	  if (current_mod >= x_info.nrFileStrings)
	    current_mod = -1;

	  if (x_info.nrFileStrings > 0)
	    {
	      if (optShell->randomChecked() == TRUE)
		current_mod = rand () % x_info.nrFileStrings;
	    }
	}
      else if (current < current_mod)
	current_mod--;
      else if (current == (-current_mod - 1))
	emit currentDeleted();

      if (x_info.nrFileStrings == 0)
	{
	  current_mod = -1;
	  emit currentDeleted();
	}

      //topShell->check_buttons ();

      // start playback of new module if current module was deleted
      if ((lstop_flag == STOP_GOTO) && (current_mod >= 0))
	if (!start_playback (0))
	  current_mod = -current_mod - 1;
    }
  else
    QMessageBox::message("Xgmod Error", "No module is selected", 0, this);
}

void
QueueShell::clearClicked()
{
  int i, o_current_mod;
  extern Sequencer *seq;

  queueList->clear();

  for (i = 0; i < x_info.nrFileStrings; i++)
    free (x_info.fileStrings[i]);
  free (x_info.fileStrings);
  x_info.nrFileStrings = 0;
  x_info.fileStrings = NULL;
  o_current_mod = current_mod;
  current_mod = -1;
  
  // stop playback and deallocate current module
  if (o_current_mod >= 0)
    {
      seq->stop_playback ();
      end_module (0);
    }

  //topShell->check_buttons ();
  emit currentDeleted();
}

void
QueueShell::shuffleClicked()
{
  int i, swap_num;
  char *tmp_string;
  int current = queueList->currentItem();

  for (i = 0; i < x_info.nrFileStrings; i++)
    {
      swap_num = rand () % x_info.nrFileStrings;

      tmp_string = x_info.fileStrings[i];
      x_info.fileStrings[i] = x_info.fileStrings[swap_num];
      x_info.fileStrings[swap_num] = tmp_string;

      if (current_mod == i)
	current_mod = swap_num;
      else if (swap_num == current_mod)
	current_mod = i;
    }

  queueList->setAutoUpdate(FALSE);
  queueList->clear();

  for (i = 0; i < x_info.nrFileStrings; i++)
    queueList->insertItem(x_info.fileStrings[i]);

  if (current != -1)
    queueList->setCurrentItem(current);
  
  queueList->update();
  queueList->setAutoUpdate(TRUE);
}

#include "QueueShell.moc"  
