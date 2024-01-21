/*
 * Display bar graphs aranged by unique id and by non-unique group id.
 * Continuously reads a new data set and updates the display.
 * 
 * Very slow on a 486Dx2/66 with 16MB and a LB Tseng ET4000 (not a W32).
 *
 * Based on a cut up of the GLUT example/scube.c which rotates a cube 
 * above a checker board with shadows, lighting etc.
 *
 * I reused the scube code for lighting and colors.
 *
 * (c) Copyright 1995, Michael Hamilton
 *
 * You have the same rights as stated in the original SGI copyright.
 * (see below).
 *
 * Input data format:  A continous series of data sets each of which has
 * the following format (newlines are significant).
 *
 *   title
 *   nitems nvalues valuename1 valuename2 valuename3 ...
 *   itemID
 *   groupID
 *   value1 value2 value3 ...
 *   repeat of the above ...
 *
 * E.g.
 *
 *   Selected processes on sputnik3
 *   4 3 CPU RSS MEM
 *   100
 *   michael
 *   0.000000 0.043189 0.000000 
 *   196
 *   root
 *   0.166667 0.559287 0.178711 
 *   250
 *   michael
 *   2.083333 1.768806 0.609375 
 *   275
 *   michael
 *   0.000000 0.054172 0.021484 
 *
 *   Note titles, nitems, nvalues can change for each data set.
 *   Data within a data set can be in any order, the program will
 *   sort it by ID and group ID (in the previous example ID is process ID,
 *   and group ID is process username).
 *
 *   Accepts data from stdin (e.g. rsh output).
 */

/* ---- ORIGINAL scube.c stuff: */

/* Copyright (c) Mark J. Kilgard, 1994. */

/**
 * (c) Copyright 1993, 1994, Silicon Graphics, Inc.
 * ALL RIGHTS RESERVED 
 * Permission to use, copy, modify, and distribute this software for 
 * any purpose and without fee is hereby granted, provided that the above
 * copyright notice appear in all copies and that both the copyright notice
 * and this permission notice appear in supporting documentation, and that 
 * the name of Silicon Graphics, Inc. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission. 
 *
 * THE MATERIAL EMBODIED ON THIS SOFTWARE IS PROVIDED TO YOU "AS-IS"
 * AND WITHOUT WARRANTY OF ANY KIND, EXPRESS, IMPLIED OR OTHERWISE,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY OR
 * FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL SILICON
 * GRAPHICS, INC.  BE LIABLE TO YOU OR ANYONE ELSE FOR ANY DIRECT,
 * SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY
 * KIND, OR ANY DAMAGES WHATSOEVER, INCLUDING WITHOUT LIMITATION,
 * LOSS OF PROFIT, LOSS OF USE, SAVINGS OR REVENUE, OR THE CLAIMS OF
 * THIRD PARTIES, WHETHER OR NOT SILICON GRAPHICS, INC.  HAS BEEN
 * ADVISED OF THE POSSIBILITY OF SUCH LOSS, HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE
 * POSSESSION, USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * US Government Users Restricted Rights 
 * Use, duplication, or disclosure by the Government is subject to
 * restrictions set forth in FAR 52.227.19(c)(2) or subparagraph
 * (c)(1)(ii) of the Rights in Technical Data and Computer Software
 * clause at DFARS 252.227-7013 and/or in similar or successor
 * clauses in the FAR or the DOD or NASA FAR Supplement.
 * Unpublished-- rights reserved under the copyright laws of the
 * United States.  Contractor/manufacturer is Silicon Graphics,
 * Inc., 2011 N.  Shoreline Blvd., Mountain View, CA 94039-7311.
 *
 * OpenGL(TM) is a trademark of Silicon Graphics, Inc.
 */

/*
 * 1992 David G Yu -- Silicon Graphics Computer Systems
 */

/* ---- End of original scube.c stuff. */

#define VERSION_STRING "Gr_Monitor Version 0.53"

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <sys/ioctl.h>
#include <pwd.h>
#ifdef linux
#include <termcap.h>
#endif
#include <termios.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <ctype.h>
#include <setjmp.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <assert.h>
#include <stdarg.h>

#include <GL/gl.h>
#include <GL/glut.h>

#define DEBUG_MIN 1
#define DEBUG_MAX 3

#define ROTATION      6
#define PER_ROW      18
#define START_ANGLE  55
#define START_Y      -3
#define START_DOLLY  -9
#define FONT_SCALE   0.002
#define ROW_GAP      3.0
#define CRAM_FACTOR  0.25

#define GR_GATHER_COMMAND "GR_GATHER_COMMAND"

#ifndef GATHER_COMMAND
#define GATHER_COMMAND "/usr/local/bin/gr_gather"
#endif

#define MAX_LINE        10000
#define MAX_VALUES        200
#define MAX_TEXT           79
#define MAX_CONTROLLABLE   10

#define WIN_WIDTH	  600	/* Default */
#define WIN_HEIGHT	  400	/* Default */

				/* glFrustum settings for perspective */
#define CLIP_LEFT        -1.0	/* Maps to window edges */
#define CLIP_RIGHT        1.0   /* Ditto */
#define CLIP_BOTTOM      -0.6   /* Ditto */
#define CLIP_TOP          1.26  /* Ditto */
#define CLIP_NEAR         1.0   /* Front clipping plane */
#define CLIP_FAR        500.0   /* Back clipping plane */

static char key_help[][50] = { 
  "exit:               ESC",
  "echo help to tty:   ?",
  "graphs per row +/-: p/P",
  "adjust zoom +/-:    a/A",
  "rotate +/-:         x/X y/Y z/Z",
  "translate +/-:      i/I j/J k/K",
  "toggle rotation:    r",
  "select bar type:    0..9",
  "scale selected +/-: s/S",
  "scale selected 1.0: n",
  "toggle selected:    o",
  "toggle lighting:    l or L",
  "toggle fog:         f",
  "toggle halt:        h",
  "step thru halt:     SPACE"
  ""
};

static int key_menu;

static int debug = 0;

static int num_items;

static int useRGB = 1;
static int useLighting = 1;
static int useFog = 0;
static int useDB = 1;

static int isvisible = 1;
static int halted = 0;

static int skip = 1;

static int cram = 0;
static int per_row = PER_ROW;
static int gap = ROW_GAP;
static float cram_factor = CRAM_FACTOR;

static int rotate = 0;
static int rotate_y = 0;
static int rotate_x = 1;
static int rotate_z = 0;
static int rotate_angle = START_ANGLE;

static int trans_y = START_Y;
static int trans_x = 0;
static int trans_z = START_DOLLY;

static float zoom_horz = 1.0;
static float zoom_vert = 1.0;

static char value_name[MAX_VALUES][MAX_TEXT + 1];

static int hide_value[MAX_CONTROLLABLE] = {
  0,0,0,0,0, 0,0,0,0,0 
};

static float scale_value[MAX_CONTROLLABLE] = {
  1.0, 1.0, 1.0, 1.0, 1.0, 
  1.0, 1.0, 1.0, 1.0, 1.0 
};

static int value_index = 0;

static int use_stdin = 0;

static int sleep_seconds = 5;

static FILE *inputfp = NULL;

void *font = GLUT_STROKE_ROMAN;
void *fonts[] = {GLUT_STROKE_ROMAN, GLUT_STROKE_MONO_ROMAN};

#define GREY	0
#define RED	1
#define GREEN	2
#define BLUE	3
#define CYAN	4
#define MAGENTA	5
#define YELLOW	6
#define BLACK	7

static float materialColor[8][4] =
{
  {0.8, 0.8, 0.8, 1.0},
  {0.8, 0.0, 0.0, 1.0},
  {0.0, 0.8, 0.0, 1.0},
  {0.0, 0.0, 0.8, 1.0},
  {0.0, 0.8, 0.8, 1.0},
  {0.8, 0.0, 0.8, 1.0},
  {0.8, 0.8, 0.0, 1.0},
  {0.0, 0.0, 0.0, 0.4},
};

static float lightPos[4] =
{2.0, 4.0, 2.0, 1.0};
static float lightDir[4] =
{-2.0, -4.0, -2.0, 1.0};
static float lightAmb[4] =
{0.2, 0.2, 0.2, 1.0};
static float lightDiff[4] =
{0.8, 0.8, 0.8, 1.0};
static float lightSpec[4] =
{0.4, 0.4, 0.4, 1.0};

static float groundPlane[4] =
{0.0, 1.0, 0.0, 1.499};
static float backPlane[4] =
{0.0, 0.0, 1.0, 0.899};

static float fogColor[4] =
{0.0, 0.0, 0.0, 0.0};
static float fogIndex[1] =
{0.0};

static char *windowNameRGBDB = "Display RGB Double buffered";
static char *windowNameRGB = "Display RGB Single buffered";
static char *windowNameIndexDB = "Display Color DB Double buffered";
static char *windowNameIndex = "Display Color DB Single buffered";

static float cube_vertexes[6][4][4] =
{
  {
    {-1.0, -1.0, -1.0, 1.0},
    {-1.0, -1.0, 1.0, 1.0},
    {-1.0, 1.0, 1.0, 1.0},
    {-1.0, 1.0, -1.0, 1.0}},

  {
    {1.0, 1.0, 1.0, 1.0},
    {1.0, -1.0, 1.0, 1.0},
    {1.0, -1.0, -1.0, 1.0},
    {1.0, 1.0, -1.0, 1.0}},

  {
    {-1.0, -1.0, -1.0, 1.0},
    {1.0, -1.0, -1.0, 1.0},
    {1.0, -1.0, 1.0, 1.0},
    {-1.0, -1.0, 1.0, 1.0}},

  {
    {1.0, 1.0, 1.0, 1.0},
    {1.0, 1.0, -1.0, 1.0},
    {-1.0, 1.0, -1.0, 1.0},
    {-1.0, 1.0, 1.0, 1.0}},

  {
    {-1.0, -1.0, -1.0, 1.0},
    {-1.0, 1.0, -1.0, 1.0},
    {1.0, 1.0, -1.0, 1.0},
    {1.0, -1.0, -1.0, 1.0}},

  {
    {1.0, 1.0, 1.0, 1.0},
    {-1.0, 1.0, 1.0, 1.0},
    {-1.0, -1.0, 1.0, 1.0},
    {1.0, -1.0, 1.0, 1.0}}
};

static float cube_normals[6][4] =
{
  {-1.0, 0.0, 0.0, 0.0},
  {1.0, 0.0, 0.0, 0.0},
  {0.0, -1.0, 0.0, 0.0},
  {0.0, 1.0, 0.0, 0.0},
  {0.0, 0.0, -1.0, 0.0},
  {0.0, 0.0, 1.0, 0.0}
};


typedef struct {
  char id[MAX_TEXT + 1];
  char group[MAX_TEXT + 1];
  int group_size;
  float *value;
} item_t;


void projection(float, float);
void display(item_t *, int, int);

item_t *allocate_data (int num_items,
		       int num_values)
{
  static item_t *allocated_store = NULL;
  static int allocated_items = 0;
  static int allocated_values = 0;

  int i;

  if (num_items != allocated_items || num_values != allocated_values) {
    if (allocated_store) {
      for (i=0; i < allocated_items; i++) {
	free(allocated_store[i].value);
      }
      free((void *)allocated_store);
    }
    allocated_store = malloc (num_items * sizeof (item_t));
    assert(allocated_store != NULL);
    for (i=0; i < num_items; i++) {
      allocated_store[i].value = malloc (sizeof(float) * num_values);
    }
    allocated_items = num_items;
    allocated_values = num_values;
  }
  
  return allocated_store;
}


int compareData(const void *arg1, const void *arg2)
{
  register const item_t *item1 = (item_t *) arg1;
  register const item_t *item2 = (item_t *) arg2;
  register int result;
  if ((result = strcmp(item1->group, item2->group)) != 0) {
    return result;
  }
  return strcmp(item1->id, item2->id);
}

int DataReady(FILE *inputfp) 
{
  /* For select(2). */
  struct timeval tv;
  fd_set in;

  /* Non-blocking read */

  tv.tv_sec = 0;
  tv.tv_usec = 0;
  FD_ZERO(&in);
  FD_SET(fileno(inputfp), &in);
  return select(16, &in, 0, 0, &tv) > 0;
}

item_t *getData(FILE *inputfp,
		char *title,
		int *num_items, 
		int *num_values, 
		int prev_values) {

  static item_t *data_set = NULL;

  static int prev_num_items, prev_num_values;

  int i, j;
  int id, group_size;

  char group[MAX_TEXT];
 
  static char line[MAX_LINE], *upto;

  if ((prev_values || !DataReady(inputfp)) && data_set) {
    if (debug >= DEBUG_MAX) fputs("Returning old data.\n", stderr);
    *num_items  = prev_num_items;
    *num_values = prev_num_values;
    return data_set;
  }

  /* Data is ready or we have no existing data-set */

  if (debug >= DEBUG_MAX) fputs("Getting new data.\n", stderr);

  fgets(title, MAX_TEXT, inputfp);

  fgets(line, MAX_LINE, inputfp);
  
  *num_items = strtol(line, &upto, 0);
  *num_values = strtol(upto, &upto, 0);

  assert (*num_values <= MAX_VALUES);

  for (i = 0; i < *num_values; i++) {
    strncpy(value_name[i],strtok(upto," "),MAX_TEXT);
    value_name[i][MAX_TEXT] = '\0';
    upto = NULL;
  }

  data_set = allocate_data (*num_items, *num_values);
  
  for (i = 0; i < *num_items; i++) { 

    fgets(data_set[i].id, MAX_TEXT, inputfp);
    fgets(data_set[i].group, MAX_TEXT, inputfp);

    fgets(line,MAX_LINE,inputfp);
        
    for (j = 0, upto=line; j < *num_values; j++) {
      data_set[i].value[j] = strtod(upto, &upto); 
    }
  }

  qsort(data_set, *num_items, sizeof(item_t), &compareData) ;

  strcpy(group, data_set[(*num_items) - 1].group) ;
  group_size = 1;
  for (i = (*num_items) - 1; i >= 0; i--) { 
    if (strcmp(data_set[i].group, group) != 0) {
      strcpy(group,data_set[i].group);
      group_size = 1;
    }
    /* printf("gs=%d\n",group_size); */
    data_set[i].group_size = group_size;
    group_size++;
  }

  if (cram) {
    
  }

  if (debug >= DEBUG_MAX) {
    fprintf(stderr, "title=%s num_items=%d num_values=%d\n", title, *num_items, *num_values);
  }

  prev_num_items  = *num_items;
  prev_num_values = *num_values;
 
  return data_set;
}

void do_update(void)
{
  static int count = 0;
  static char title[MAX_TEXT];

  int num_items;
  int num_values;

  item_t *data_set = getData(inputfp, title, &num_items, &num_values, halted);

  ++count;

  if (skip) {
    while (DataReady(inputfp)) { /* Skip over data until we are up to date */
      ++count;
      if (debug >= DEBUG_MIN) {
	fprintf(stderr,
		"Can't render fast enough - skipping over data set %d.\n",
		count);
      }
      data_set = getData(inputfp, title, &num_items, &num_values, halted);
    } 
  }

  glutSetWindowTitle(title);
  glutSetIconTitle(title);

  if (isvisible) {
    if (debug >= DEBUG_MAX) fputs("Displaying.\n", stderr) ;
    display(data_set, num_items, num_values);    
    /*    glutPostRedisplay(); */
    if (debug >= DEBUG_MAX) fprintf(stderr, "Done (%d).\n", count) ;
  }
}

void do_idle(void)
{
				/* Only update if there's new data. */
  if (rotate || (!halted && DataReady(inputfp))) {	
    if (debug >= DEBUG_MAX) fputs("Update display during idle time.\n", stderr);
    do_update();
  }
  else {
    sleep(1);
  }
}

void key_help_to_stdout(void)
{
  int i;
  puts("\nKey Summary.\n");
  for (i = 0; *(key_help[i]); i++) {
    puts(key_help[i]);
  }
}

void
keyboard(unsigned char ch, int x, int y)
{
  int update = 1; /* Most commands need a display update */

  switch (ch) {

  case 27:             /* escape */
    exit(0);
    break;

  case 'L':
  case 'l':
    useLighting = !useLighting;
    useLighting ? glEnable(GL_LIGHTING) : glDisable(GL_LIGHTING);
    glutPostRedisplay();
    update = 0;
    break;
  case 'F':
  case 'f':
    useFog = !useFog;
    useFog ? glEnable(GL_FOG) : glDisable(GL_FOG);
    glutPostRedisplay();
    update = 0;
    break;

  case 'R':
  case 'r':
    rotate = !rotate;
    break;

  case 's':
    scale_value[value_index] += 0.2;
    break;
  case 'S':
    scale_value[value_index] -= 0.2;
    if (scale_value[value_index] < 0) scale_value[value_index] = 0.0;
    break;

  case 'n':
  case 'N':
    scale_value[value_index] = 1.0;
    break;

  case 'o':
  case 'O':
    hide_value[value_index] = !hide_value[value_index];
    break;

  case 'i':
    trans_x = 1 ;
    break;
  case 'I':
    trans_x = -1 ;
    break;
  case 'j':
    trans_y = 1;
    break;
  case 'J':
    trans_y = -1;
    break;
  case 'k':
    trans_z = 1;
    break;
  case 'K':
    trans_z = -1;
    break;

  case 'x':
    rotate_x = 1 ;
    break;
  case 'X':
    rotate_x = -1 ;
    break;
  case 'y':
    rotate_y = 1;
    break;
  case 'Y':
    rotate_y = -1;
    break;
  case 'z':
    rotate_z = 1;
    break;
  case 'Z':
    rotate_z = -1;
    break;


  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    value_index = ch - '0';
    update = 0;
    break;

  case 'p':
    per_row++;
    break;
  case 'P':
    if (per_row > 1) per_row--;
    break;

  case 'a':
    zoom_vert -= 0.1;
    zoom_horz -= 0.1;
    break;
  case 'A':
    zoom_vert += 0.1;
    zoom_horz += 0.1;
    break;

  case 'H':
  case 'h':
    halted = !halted;
    if (halted) update = 0;
    break;

  case ' ':
    update = 1;
    break;

  case '?':
    key_help_to_stdout();
    break;

  default:
    break;
  }

  if (update) {
    if (debug >= DEBUG_MAX) fputs("Update display after keypress.\n", stderr);
    do_update();
  }

}

void strokePrintf(const char *format, ...)
{
  va_list ap; 

  static char buf[1000];

  int i;

  va_start (ap, format);
  
  vsprintf (buf, format, ap);

  glPushMatrix();

  for (i = 0; i < strlen(buf); i++) {
    glutStrokeCharacter(font, buf[i]);
  }

  glPopMatrix();
}


void
buildColormap(void)
{
  if (useRGB) {
    return;
  } else {
    int mapSize = 1 << glutGet(GLUT_WINDOW_BUFFER_SIZE);
    int rampSize = mapSize / 8;
    int entry;
    int i;

    for (entry = 0; entry < mapSize; ++entry) {
      int hue = entry / rampSize;
      GLfloat val = (entry % rampSize) * (1.0 / (rampSize - 1));
      GLfloat red, green, blue;

      red = (hue == 0 || hue == 1 || hue == 5 || hue == 6) ? val : 0;
      green = (hue == 0 || hue == 2 || hue == 4 || hue == 6) ? val : 0;
      blue = (hue == 0 || hue == 3 || hue == 4 || hue == 5) ? val : 0;

      glutSetColor(entry, red, green, blue);
    }

    for (i = 0; i < 8; ++i) {
      materialColor[i][0] = i * rampSize + 0.2 * (rampSize - 1);
      materialColor[i][1] = i * rampSize + 0.8 * (rampSize - 1);
      materialColor[i][2] = i * rampSize + 1.0 * (rampSize - 1);
      materialColor[i][3] = 0.0;
    }

    fogIndex[0] = -0.2 * (rampSize - 1);
  }
}

static void
setColor(int c)
{
  if (useLighting) {
    if (useRGB) {
      glMaterialfv(GL_FRONT_AND_BACK,
        GL_AMBIENT_AND_DIFFUSE, &materialColor[c][0]);
    } else {
      glMaterialfv(GL_FRONT_AND_BACK,
        GL_COLOR_INDEXES, &materialColor[c][0]);
    }
  } else {
    if (useRGB) {
      glColor4fv(&materialColor[c][0]);
    } else {
      glIndexf(materialColor[c][1]);
    }
  }
}

static void
drawBar(int color, float height)
{
  int i, j;

  setColor(color);

  for (i = 0; i < 6; ++i) {
    glNormal3fv(&cube_normals[i][0]);
    glBegin(GL_POLYGON);
    for (j = 0; j < 4; j++) {
      if (cube_vertexes[i][j][1] >= 0) {
	glVertex4f(cube_vertexes[i][j][0],
		   height == 0 ? 0.00001 : height,
		   cube_vertexes[i][j][2],
		   cube_vertexes[i][j][3]
		   );
      }
      else {
	glVertex4fv(&cube_vertexes[i][j][0]);
      }      
    }
    glEnd();
  }
}

drawIDname( char *id )
{
  while (*(id) == ' ') ++id;	/* Skip spaces */
  setColor(BLACK);
  glPushMatrix();
  glTranslatef(-0.3, -0.65, 0.7);
  glScalef(FONT_SCALE,1.0,FONT_SCALE);
  glRotatef(-90, 1.0, 0.0, 0.0);
  strokePrintf("%s", id);
  glPopMatrix();
}

void 
drawOneItem(int num_bars, item_t *item)
{

  int i;

  drawIDname(item->id);

  glPushMatrix();

  glScalef(0.3, 1.0, 0.3);

  for (i = 0; i < num_bars; i++) {
    if (i >= MAX_CONTROLLABLE || !hide_value[i]) {
				/* draw bar */	
      if (i < MAX_CONTROLLABLE && scale_value[i] != 1.0) {
	drawBar(i + 1, item->value[i] * scale_value[i]);
      }
      else {
	drawBar(i + 1, item->value[i]);        /* draw bar */
      }
    }
    glTranslatef(0.0, 0.0, -3.33);
  }
  glPopMatrix();

}

drawValueName( char *bname, int num )
{
  setColor(BLACK);
  glPushMatrix();
  glTranslatef(-1.4, -0.65, -num);
  glScalef(FONT_SCALE*1.5,1.0,FONT_SCALE*1.5);
  glRotatef(-90, 1.0, 0.0, 0.0);
  strokePrintf("%s", bname);
  glPopMatrix();
}

drawGname( char *group, int length )
{
  setColor(RED);
  glPushMatrix();
  glTranslatef((length + 0.5)/2.0 - 2.0, -0.65, 1.3);
  glScalef(FONT_SCALE*2,1.0,FONT_SCALE*2);
  glRotatef(-90, 1.0, 0.0, 0.0);
  strokePrintf("%s", group);
  glPopMatrix();
}

void 
drawBase(char *gname, int length, int num_values)
{

  int i, j;

  drawGname(gname, length);
  for (i = 0; i < num_values ; i++) {
    drawValueName(value_name[i], i);
  }

  glPushMatrix();

  /* printf("length=%d\n",length); */


  glTranslatef(-0.5, -1.0, -(num_values - 1));
  glScalef(1.0, 0.3, 1.0);

  setColor(GREY);

  for (i = 0; i < 6; ++i) {
    glNormal3fv(&cube_normals[i][0]);
    glBegin(GL_POLYGON);
    for (j = 0; j < 4; j++) {
      glVertex4f((cube_vertexes[i][j][0] >= 0) ? (length + 0.5) : cube_vertexes[i][j][0],
		 cube_vertexes[i][j][1],
		 (cube_vertexes[i][j][2] >= 0) ? (num_values + 0.5) : cube_vertexes[i][j][2],
		 cube_vertexes[i][j][3]
		 );
    }
    glEnd();
  }

  glPopMatrix();

}



void showData (item_t *data_set, int num_items, int num_values)
{
  int num_drawn;
  int num_in_group;
  int step;
  char last_group[MAX_TEXT];
  float location;

  glPushMatrix();

  glTranslatef(-(per_row / 2 + 1), 0.0, 0.0);

  num_in_group = data_set[0].group_size;

  num_drawn = 0;
  step = 0;

  while (num_drawn < num_items) {
    
    int i;
    
    num_in_group = data_set[num_drawn].group_size ;
    drawBase(data_set[num_drawn].group,num_in_group,num_values);
    
    for (i = 0; i < num_in_group; i++) {
      drawOneItem(num_values, &(data_set[num_drawn]));
      num_drawn++;
      glTranslatef(1, 0.0, 0.0);
      step++;
    }
    
    if (num_drawn < num_items) {

      if (step > per_row || step + data_set[num_drawn].group_size > per_row
	  && (!cram 
	      || (step > (1 - cram_factor) * per_row)
	      || (step + data_set[num_drawn].group_size > per_row * (1 + cram_factor)))
	  ) {
	glTranslatef(-step, 0.0, -(num_values + gap));
	step = 0;
      } else {
	glTranslatef(3, 0.0, 0.0);
	step += 3;
      }
    }
    
  }
  
  glPopMatrix();
}

void
display(item_t *data_set, int num_items, int num_values)
{

  static int first_time_thru = 1;

  projection(zoom_horz, zoom_vert);

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  if (trans_x || trans_y || trans_z) {
    glTranslatef(trans_x, trans_y, trans_z); 
    trans_x = trans_y = trans_z = 0;
  }

  if (rotate || rotate_x || rotate_y || rotate_z) {
    glRotatef(rotate_angle, rotate_x, rotate_y, rotate_z);
    rotate_angle = ROTATION;
    rotate_x = rotate_y = rotate_z = 0;
    if (rotate) rotate_y = 1;
  }

  /* Draw bars */
  showData(data_set, num_items, num_values);

  glDepthMask(GL_TRUE);
  if (useRGB) {
    glDisable(GL_BLEND);
  } else {
    glDisable(GL_POLYGON_STIPPLE);
  }
  if (useFog) {
    glEnable(GL_FOG);
  }
  glutSwapBuffers();
}

void
visible(int state)
{
  isvisible = state == GLUT_VISIBLE ;
}

void
fog_select(int fog)
{
  glFogf(GL_FOG_MODE, fog);
  glutPostRedisplay();
}

void
menu_select(int mode)
{
  switch (mode) {
  case 1:
    halted = 0;
    break;
  case 2:
    halted = 1;
    break;
  case 3:
    useFog = !useFog;
    useFog ? glEnable(GL_FOG) : glDisable(GL_FOG);
    glutPostRedisplay();
    break;
  case 4:
    useLighting = !useLighting;
    useLighting ? glEnable(GL_LIGHTING) : glDisable(GL_LIGHTING);
    glutPostRedisplay();
    break;
  case 5:
    exit(0);
    break;
  }
}

void do_nothing(int choice)
{
				/* Dummy for key_help menu */
}

void projection(float zoom_horz, float zoom_vert)
{
  static float last_h = 0;
  static float last_v = 0;
  if (zoom_horz != last_h || zoom_vert != last_v) {
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glFrustum(CLIP_LEFT * zoom_horz, CLIP_RIGHT * zoom_horz, CLIP_BOTTOM * zoom_vert, CLIP_TOP * zoom_vert, CLIP_NEAR, CLIP_FAR);
    glMatrixMode(GL_MODELVIEW);
    last_h = zoom_horz;
    last_v = zoom_vert;
  }
}

void gl_setup(int argc, char **argv, int width, int height) {

  int fog_menu, i;
  char *name;

  glutInitWindowSize(width, height);
  glutInit(&argc, argv);
  /* choose visual */
  if (useRGB) {
    if (useDB) {
      glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
      name = windowNameRGBDB;
    } else {
      glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB | GLUT_DEPTH);
      name = windowNameRGB;
    }
  } else {
    if (useDB) {
      glutInitDisplayMode(GLUT_DOUBLE | GLUT_INDEX | GLUT_DEPTH);
      name = windowNameIndexDB;
    } else {
      glutInitDisplayMode(GLUT_SINGLE | GLUT_INDEX | GLUT_DEPTH);
      name = windowNameIndex;
    }
  }

  glutCreateWindow(name);

  buildColormap();

  glutKeyboardFunc(keyboard);
  glutDisplayFunc(do_update);
  glutVisibilityFunc(visible);

  key_menu = glutCreateMenu(do_nothing);

  for (i = 0; *(key_help[i]); i++) {
    glutAddMenuEntry(key_help[i], 0);
  }

  glutAttachMenu(GLUT_MIDDLE_BUTTON);
  

  fog_menu = glutCreateMenu(fog_select);
  glutAddMenuEntry("Linear fog", GL_LINEAR);
  glutAddMenuEntry("Exp fog", GL_EXP);
  glutAddMenuEntry("Exp^2 fog", GL_EXP2);

  glutCreateMenu(menu_select);
  glutAddSubMenu("Key summary", key_menu);
  glutAddMenuEntry("Unfreeze", 1);
  glutAddMenuEntry("Freeze", 2);
  glutAddMenuEntry("Toggle fog", 3);
  glutAddMenuEntry("Toggle lighting", 4);
  glutAddSubMenu("Fog type", fog_menu);
  glutAddMenuEntry("Quit", 5);
  glutAttachMenu(GLUT_RIGHT_BUTTON);

  /* setup context */
  projection(zoom_horz, zoom_vert);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatef(0.0, 0.0, -3.0);

  glEnable(GL_DEPTH_TEST);

  if (useLighting) {
    glEnable(GL_LIGHTING);
  }
  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_POSITION, lightPos);
  glLightfv(GL_LIGHT0, GL_AMBIENT, lightAmb);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, lightDiff);
  glLightfv(GL_LIGHT0, GL_SPECULAR, lightSpec);
  /* 
   * glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, lightDir);
   * glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 80);
   * glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, 25);
   */

  glEnable(GL_NORMALIZE);

  if (useFog) {
    glEnable(GL_FOG);
  }
  glFogfv(GL_FOG_COLOR, fogColor);
  glFogfv(GL_FOG_INDEX, fogIndex);
  glFogf(GL_FOG_MODE, GL_EXP);
  glFogf(GL_FOG_DENSITY, 0.5);
  glFogf(GL_FOG_START, CLIP_NEAR);
  glFogf(GL_FOG_END, CLIP_FAR);

  glEnable(GL_CULL_FACE);
  glCullFace(GL_BACK);

  glShadeModel(GL_SMOOTH);

  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);

#if 0
  if (useLogo) {
    glPolygonStipple((const GLubyte *) sgiPattern);
  } else {
    glPolygonStipple((const GLubyte *) shadowPattern);
  }
#endif

  glClearColor(0.0, 0.0, 0.0, 1);
  glClearIndex(0);
  glClearDepth(1);

}

void usage(void) 
{
  puts("");
  puts("gather | display [options]");
  puts(" ");
  puts("Options:");
  puts("    -witdh n   pixals.");
  puts("    -height n  pixals.");
  puts("    -perrow n  data sets per row.");
  puts("    -cram x    (x * perrow) <= in row or in row <= ((1 + x) * perrow)");
  puts("    -gap x     gap between rows in floating point.");
  puts("    -angle n   starting tilt toward or away from you.");
  puts("    -dolly n   starting viewing distance from origin.");
  puts("    -zoom x    zoom factor horizontal and vertical.");
  puts("    -zhorz x   horizontal zoom factor.");
  puts("    -zvert x   vertical zoom factor.");
  puts("    -geometry  window size and location");
  puts("    -color     take over color map");
  puts("    -nolight   disable lighting");
  puts("    -fog       enable fog effects");
  puts("    -onebuf    disable double buffering");
  puts("    -rotate    enable rotation");
  puts("    -stdin     take input from stdin");         
  puts("    -host h    rsh gather on host");         
  puts("    -sleep n   sleep seconds between updates.");         
  puts("    -noskip    don't skip data even if it comes in too fast.");        
  puts("    -verbose   all feedback/debugging output to stderr enabled."); 
  puts("    -v         report skipped data due to it arriving too fast.");     
  puts("");

}

int
main(int argc, char **argv)
{

  
  int width = WIN_WIDTH, height = WIN_HEIGHT;
  int i;

  char hostname[MAX_TEXT] = "";

  /* process commmand line args */
  for (i = 1; i < argc; ++i) {

    if (!strcmp("-c", argv[i])) {
      useRGB = !useRGB;

    } else if (!strcmp("-color", argv[i])) {
      useRGB = !useRGB;

    } else if (!strcmp("-nolight", argv[i])) {
      useLighting = !useLighting;

    } else if (!strcmp("-fog", argv[i])) {
      useFog = !useFog;

    } else if (!strcmp("-onebuf", argv[i])) {
      useDB = !useDB;

    } else if (!strcmp("-rotate", argv[i])) {
      rotate = 1;

    } else if (!strcmp("-width", argv[i])) {
      width = strtol(argv[++i],NULL,0);

    } else if (!strcmp("-height", argv[i])) {
      height = strtol(argv[++i],NULL,0);

    } else if (!strcmp("-perrow", argv[i])) {
      per_row = strtol(argv[++i],NULL,0);

    } else if (!strcmp("-cram", argv[i])) {
      cram = 1;
      cram_factor = strtod(argv[++i],NULL);

    } else if (!strcmp("-angle", argv[i])) {
      rotate_angle = strtol(argv[++i],NULL,0);

    } else if (!strcmp("-dolly", argv[i])) {
      trans_z = strtol(argv[++i],NULL,0);

    } else if (!strcmp("-gap", argv[i])) {
      gap = strtod(argv[++i],NULL);
      
    } else if (!strcmp("-stdin", argv[i])) {
      use_stdin = 1;

    } else if (!strcmp("-host", argv[i])) {
      strcpy(hostname, argv[++i]);

    } else if (!strcmp("-sleep", argv[i])) {
      sleep_seconds = strtod(argv[++i],NULL);
      if (sleep_seconds == 0) {
	halted = 1;
      }
    } else if (!strcmp("-noskip", argv[i])) {
      skip = 0;

    } else if (!strcmp("-zoom", argv[i])) {
      zoom_horz = zoom_vert = strtod(argv[++i],NULL);
    } else if (!strcmp("-zvert", argv[i])) {
      zoom_vert = zoom_vert = strtod(argv[++i],NULL);
    } else if (!strcmp("-zhorz", argv[i])) {
      zoom_horz = strtod(argv[++i],NULL);

    } else if (!strcmp("-verbose", argv[i])
	       || !strcmp("-debug", argv[i])) {
      debug = DEBUG_MAX;

    } else if (!strcmp("-v", argv[i])) {
      debug = DEBUG_MIN;

    } else {
      usage();
    }
  }
  
  if (use_stdin) {
    inputfp = stdin;
  }
  else {
    char *cmd = getenv(GR_GATHER_COMMAND);
    char command[MAX_LINE];

    strcpy(command, cmd ? cmd : GATHER_COMMAND);

    if (sleep_seconds) {
      sprintf(command, "%s -sleep %d", command, sleep_seconds);
    }

    if (*hostname) {
      char rsh[MAX_LINE];
      sprintf(rsh, "rsh -n %s '%s'", hostname, command);
      inputfp = popen(rsh, "r");
    } else {
      inputfp = popen(command, "r");
    }
  }
	       
  puts(VERSION_STRING);
  puts("Press the middle mouse button for key summary.");
  puts("Starting up...");
  
  sleep(2);			/* Give the data gatherer a chance 
				 * to warm up.
				 */

  /* loop, collecting process info and sleeping */
  
  gl_setup(argc, argv, width, height);

  glutIdleFunc(do_idle);

  glutMainLoop();
}



/*
 * Normal end of execution.
 */
void
end(void)
{
    exit(0);
}


/*
 * SIGTSTP catcher.
 */
void
stop(void)
{
    exit(0);
}


