/* Definitions for players in Xconq.
   Copyright (C) 1992, 1993, 1994 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

typedef struct a_player {
    short id;                 /* unique id for the player */
    char *name;               /* proper name of the player */
    char *configname;         /* name of a particular configuration */
    char *displayname;        /* name of the desired display */
    char *aitypename;         /* name of an AI type */
    short advantage;          /* player's desired initial advantage */
    char *password;           /* encrypted password of the player */
    struct a_side *side;      /* the side being played */
    struct a_player *next;    /* pointer to the next player */
} Player;

/* This is the mapping between players and sides. */

typedef struct {
    struct a_side *side;      /* the side */
    struct a_player *player;  /* the player assigned to the side */
    int locked;               /* true if the assignment can't be changed */
} Assign;

/* Iteration over all players. */

#define for_all_players(v) for (v = playerlist; v != NULL; v = v->next)

extern Player *playerlist;
extern Player *last_player;

extern Assign *assignments;

extern Player *add_player PARAMS ((void));
extern Player *find_player PARAMS ((int n));
extern void canonicalize_player PARAMS ((Player *player));
extern char *player_desig PARAMS ((Player *player));
