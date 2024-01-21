// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef __QueueShell_h
#define __QueueShell_h

#include <qapp.h>
#undef Unsorted
#include <qfiledlg.h>
#include <qlistbox.h>
#include <qpushbt.h>
#include <qwidget.h>

class TopShell;

struct q_info_array
{
  char *name, *label;
  int x, y, width, height;
};

// XQFileDialog is a special QFileDialog which doesn't close when a file
// is selected
class XQFileDialog : public QFileDialog
{
  Q_OBJECT
 protected slots:
  void done(int);
};

inline void XQFileDialog::done(int r)
{
  if (r == Rejected)
    QFileDialog::done(Rejected);
};

class QueueShell : public QWidget
{
  Q_OBJECT

  signals:
  void currentDeleted();

public:
  QueueShell(QWidget *w = 0);
  void addFile(const char *);
  void removeModule(int);

 public slots:
 void currentClicked();

protected:
  void resizeEvent(QResizeEvent *);

private:
  void add_module(int, const char *);
  QListBox *queueList;
  QPushButton *buttons[8];
  QFileDialog *saveDialog;
  XQFileDialog *loadDialog;

  static struct q_info_array button_info[8];

 private slots:
 void showQueueShell();
  void closeClicked();
  void addClicked();
  void saveClicked();
  void playClicked();
  void removeClicked();
  void clearClicked();
  void shuffleClicked();
  void loadFileSelected(const char *);
  void saveFileSelected(const char *);
};

inline void
QueueShell::addFile(const char *filename)
{
  queueList->insertItem(filename);
}

#endif
