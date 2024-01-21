// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#ifndef __CommentShell_h
#define __CommentShell_h

#include <qapp.h>
#include <qlabel.h>
#include <qpushbt.h>

class CommentShell : public QWidget
{
  Q_OBJECT
public:
  CommentShell(QWidget *w = 0);
  void setComment(const char *text);

 public slots:
 void showCommentShell();

protected:
  void resizeEvent(QResizeEvent *);

private:
  QLabel *commentLabel;
  QPushButton *closeButton;

 private slots:
 void closeCommentShell();
};

inline void
CommentShell::setComment(const char *text)
{
  commentLabel->setText(text);
}

#endif
