/*                             */
/* xemeraldia    ----- main.c  */
/*                             */

#include <stdlib.h>
#include "graphics.h"
#include "games.h"
#include <pwd.h>

void  initXlib (), initXt (); /* In init-graphics.c */
static void  Quit ();

AppData  app_data;
char    *name, *programname;

static XtResource  resources[] = {
  {"useScoreFile", "Boolean", XtRBoolean, sizeof (Boolean),
     XtOffsetOf (AppData, usescorefile), XtRImmediate, (XtPointer) True},
  {"scorefile", "ScoreFile", XtRString, sizeof (String),
     XtOffsetOf (AppData, scorefile), XtRString, HIGH_SCORE_TABLE},
  {"block1color", "Block1Color", XtRPixel, sizeof (Pixel),
     XtOffsetOf (AppData, block1pixel), XtRString, "red"},
  {"block2color", "Block2Color", XtRPixel, sizeof (Pixel),
     XtOffsetOf (AppData, block2pixel), XtRString, "blue"},
  {"block3color", "Block3Color", XtRPixel, sizeof (Pixel),
     XtOffsetOf (AppData, block3pixel), XtRString, "green"},
  {"block4color", "Block4Color", XtRPixel, sizeof (Pixel),
     XtOffsetOf (AppData, block4pixel), XtRString, "yellow"},
  {"block5color", "Block5Color", XtRPixel, sizeof (Pixel),
     XtOffsetOf (AppData, block5pixel), XtRString, "violet"},
  {"block6color", "Block6Color", XtRPixel, sizeof (Pixel),
     XtOffsetOf (AppData, block6pixel), XtRString, "sky blue"},
  {"starcolor", "StarColor", XtRPixel, sizeof (Pixel),
     XtOffsetOf (AppData, starpixel), XtRString, "orange"},
};

static XrmOptionDescRec Options[] = {
  {"-score",    "useScoreFile",  XrmoptionNoArg,  "TRUE"},
  {"-noscore",  "useScoreFile",  XrmoptionNoArg,  "FALSE"},
};

static  XtActionsRec  actions[] = {
    {"RedrawBoard",      RedrawBoard},
    {"MoveLeft",         MoveLeft},
    {"MoveRight",        MoveRight},
    {"Rotation",         Rotation},
    {"CCRotation",       CCRotation},
    {"MoveDown",         MoveDown},
    {"PrintHighScores",  PrintHighScores},
    {"Done",             Done},
    {"RedrawNextItem",   RedrawNextItem},
    {"Quit",             Quit},
    {"StartGame",        StartGame}
};

static char *fallback_resources[] = {
  "*Scores.baseTranslations:#override <Btn1Down>,<Btn1Up>:PrintHighScores()",
  "?.ScoreText*Font:-adobe-*-*-*-*-*-18-*-*-*-*-*-*-*",
  "XEmeraldia*ScoreFrame.width: 500",
  "XEmeraldia*ScoreFrame.height: 500",
  "XEmeraldia*ScoreText*length: 80",
  "XEmeraldia*ScoreText*edittype: read",
  "XEmeraldia*ScoreText*displayCaret: False",
  "XEmeraldia*ScoreText*scrollVertical: never",
  "XEmeraldia*ScoreText*scrollHorizontial: never",
  "?.ScoreFrame*baseTranslations:#override \
          <ButtonPress>:Done()\n\
          <KeyPress>:Done()",
  NULL
};


int  main (argc, argv)
     int   argc;
     char *argv[];
{
  XtAppContext   app_context;
  Widget         topLevel;
  struct passwd *who;

  programname = argv[0];
  topLevel = XtVaAppInitialize (&app_context, "XEmeraldia",
				Options, XtNumber (Options),
				(Cardinal *)&argc, argv, fallback_resources, NULL);
  XtGetApplicationResources (topLevel, (XtPointer)&app_data,
			     resources, XtNumber (resources), NULL, 0);
  initXt (topLevel);

  XtAppAddActions (app_context, actions, XtNumber (actions));
  XtAddCallback (quit, XtNcallback, Quit, NULL);
  XtAddCallback (start, XtNcallback, StartGame, NULL);
  XtRealizeWidget (topLevel);

  initXlib (topLevel);
  who  = getpwuid (getuid ());
  name = who -> pw_name;
  read_high_scores ();

  XtAppMainLoop (app_context);
}


static void  Quit (w, client_data, call_data)
     Widget     w;
     XtPointer  client_data, call_data;
{
  exit (0);
}
