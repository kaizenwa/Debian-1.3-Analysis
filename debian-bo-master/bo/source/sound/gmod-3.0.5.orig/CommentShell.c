// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

#include "CommentShell.h"

CommentShell::CommentShell(QWidget *w) : QWidget(w, "commentShell")
{
  setMinimumSize(60, 40);
  setCaption("Xgmod Comment");
 
  commentLabel = new QLabel(this, "commentLabel");
  commentLabel->setGeometry(5, 5, 200, 160);
  commentLabel->setAutoResize(FALSE);
  commentLabel->setAlignment(AlignLeft | AlignTop | WordBreak);
  commentLabel->setFrameStyle(QFrame::Panel | QFrame::Sunken);

  closeButton = new QPushButton(this, "closeButton");
  closeButton->setText("Close");
  closeButton->setGeometry(75, 200, 60, 25);
  closeButton->connect(closeButton,  SIGNAL(clicked()), this, SLOT(closeCommentShell()));
  
  resize(210, 200);
}

void
CommentShell::showCommentShell()
{
  show();
}

void
CommentShell::closeCommentShell()
{
  close();
}

void
CommentShell::resizeEvent(QResizeEvent *)
{
  closeButton->move((width() - 60) / 2, height() - 30);
  commentLabel->resize(width() - 10, height() - 40);
}

#include "CommentShell.moc"
