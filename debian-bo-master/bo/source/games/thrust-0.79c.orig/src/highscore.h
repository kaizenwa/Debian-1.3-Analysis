
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifndef HIGHSCORE_H
#define HIGHSCORE_H

#include "thrust_types.h"

#define HIGHSCORES (5)

typedef struct {
  char name[40];
  int score;
} highscoreentry;

extern highscoreentry highscorelist[HIGHSCORES];

#ifdef __STDC__
void writehighscores(void);
int readhighscores(void);
char *standardname(void);
int inithighscorelist(void);
int ahighscore(int score);
void inserthighscore(char *name, int score);
#endif

#endif /* HIGHSCORE_H */
