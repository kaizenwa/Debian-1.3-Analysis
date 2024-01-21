
/*
    xskat - a card game for 1 to 3 players.
    Copyright (C) 1996  Gunter Gerhardt

    This program is free software; you can redistribute it freely.
    Use it at your own risk; there is NO WARRANTY.
*/

#define TEXT_C

#include <stdio.h>
#include "text.h"

#define VERSION   " X S K A T   1.6 "
#define COPYRIGHT "Copyright 1996 © Gunter Gerhardt"
#define EMAIL     "(gerhardt@draeger.com)"

static char *ger_text[]={
  "Null","Karo","Herz","Pik","Kreuz","Grand",
  "As","10","König","Dame","Bube","9","8","7",
  " A"," 10"," K"," D"," B"," 9"," 8"," 7",
  "Hand gespielt","Schneider angesagt","Schwarz angesagt",
  "Ouvert gespielt","Ouvert Hand gespielt",
  "Passe","Ja","Nein","Spieler%d","Computer","links","rechts",
  "Drücken","spielt ","ouvert Hand","ouvert","Hand","Ouvert",
  "überlegt","Gereizt bis : %d","Löschen"," spielte ",
  "Gewonnen","Verloren","%d Augen",
  "Überreizt !","Gegner nicht Schneider !","Gegner nicht schwarz !",
  "Der Alleinspieler gewinnt","Der Alleinspieler verliert",
  "das Nullspiel.","das Spiel schwarz !","mit %d zu %d Augen.",
  " Hand ? "," Spiel beenden ? "," Spielende ","beendet das Spiel","Aha",
  " Löschen ? "," Spiel ","Schneider","Schwarz","Spielen"," Hinweis ",
  "Nur bei Handspielen kann Schneider",
  "oder schwarz angesagt werden.     ",
  "Ouvert schließt schwarz angesagt  ",
  "ein (außer bei Null natürlich).   ",
  "Du hast höher gereizt als der    ",
  "Wert des angesagten Spiels !     ",
  "Null:23       Null ouvert:46     ",
  "Null Hand:35  Null ouvert Hand:59",
  " Angesagt ist "," Resultat ","Der Spielwert ist","Ende",
  "Protokoll","Weiter"," Protokoll ",
  "Im Skat ist :",
  "Im Skat war :",
  "Gereizt bis :",
  "Gewonnen mit:",
  "Verloren mit:",
  "Spielliste"," Spielliste ",VERSION,
  COPYRIGHT,EMAIL,
  "Sortieren","Aufwärts","Abwärts","Alternierend","Sequentiell",
  "Normal",
  "Gereizt bis :","Gespielt wird :","Letzter Stich :","Gedrückt :",
  "Du bist dran!","Vordefiniertes Spiel","Speichern",
  "Dieses Programm ist freie Software;",
  "es kann frei verbreitet werden.",
  "Verwendung auf eigenes Risiko;",
  "es gibt KEINE GARANTIE."
};

static char *eng_text[]={
  "Null","Diamond","Heart","Spade","Club","Grand",
  "Ace","10","King","Queen","Jack","9","8","7",
  " A"," 10"," K"," Q"," J"," 9"," 8"," 7",
  "Played Hand","Said Schneider","Said schwarz",
  "Played ouvert","Played ouvert Hand",
  "Pass","Yes","No","Player%d","Computer","left","right",
  "Done","plays ","ouvert Hand","ouvert","Hand","Ouvert",
  "thinks","You said : %d","Clear"," played ",
  "Won","Lost","%d points",
  "Value too low !","Opponents not Schneider !","Opponents not schwarz !",
  "The player wins","The player loses",
  "the Null game.","the game schwarz !","with %d vs %d points.",
  " Hand ? "," Quit game ? "," Game over ","quits the game","OK",
  " Clear ? "," Game ","Schneider","Schwarz","Play"," Reminder ",
  "Only when playing Hand you may    ",
  "say Schneider or schwarz.         ",
  "Ouvert includes schwarz           ",
  "(except when playing Null).       ",
  "The value of your game           ",
  "is not high enough !             ",
  "Null:23       Null ouvert:46     ",
  "Null Hand:35  Null ouvert Hand:59",
  " Playing "," Result ","The value of the game is","Quit",
  "Log","Continue"," Log ",
  "Skat is     :",
  "Skat was    :",
  "Player said :",
  "Won with    :",
  "Lost with   :",
  "Game list"," Game list ",VERSION,
  COPYRIGHT,EMAIL,
  "Sort","Up","Down","Alternating","Sequential",
  "Normal",
  "Player said :","Playing :","Last cards :","Skat :",
  "It's your turn!","predefined game","Save",
  "This program is free software;",
  "you can redistribute it freely.",
  "Use it at your own risk;",
  "there is NO WARRANTY."
};

static struct {
  char **arr;
  char *name;
} textdesc[] = {
  {ger_text,"german"},
  {eng_text,"english"}
};

init_text()
{
  textarr=textdesc[lang].arr;
}

int langidx(s,def)
char *s;
int def;
{
  char h[80];
  int i;

  for (i=0;i<79 && s && *s;i++,s++) {
    h[i]=tolower(*s);
  }
  h[i]=0;
  for (i=0;i<sizeof(textdesc)/sizeof(textdesc[0]);i++) {
    if (!strcmp(textdesc[i].name,h)) return i;
  }
  if (s) {
    fprintf(stderr,"Unknown language '%s'\n",h);
  }
  if (!def) {
    return -1;
  }
  for (i=0;i<sizeof(textdesc)/sizeof(textdesc[0]);i++) {
    if (!strcmp(textdesc[i].name,DEFAULT_LANGUAGE)) return i;
  }
  return 0;
}
