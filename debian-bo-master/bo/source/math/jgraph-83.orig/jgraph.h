/* 
 * $Source: /tmp_mnt/n/fs/grad1/jsp/src/jgraph/RCS/jgraph.h,v $
 * $Revision: 8.3 $
 * $Date: 92/11/30 11:42:24 $
 * $Author: jsp $
 */

#include "list.h"
#include "prio_list.h"
#ifdef LCC
#include <stdlib.h>
#endif

#define PPI 120
#define FPPI 120.0
#define CPI 72.0
#define FCPI 72.0
#define CNULL ((char *)0)
#define GMNULL ((Point)0)
#define FSIG -10010.0
#define ISIG -11111111
#define HASH_SIZE 5.0
#define MHASH_SIZE 2.0

typedef struct point {
  struct point *flink;
  struct point *blink;
  float x;
  float y;
  char e;          /* 'x' for x_ebars, 'y' for y_ebars, 'p' for neither */
} *Point;

typedef struct flist {
  struct flist *flink;
  struct flist *blink;
  float f;
} *Flist;

typedef struct label {
  char *label;
  float x;
  float y;
  float rotate;
  char *font;
  float fontsize;
  char hj;
  char vj;
  float gray[3];
  char graytype;
  float linesep;
  float xmin;
  float xmax;
  float ymin;
  float ymax;
  int nlines;
} *Label;

typedef struct curve {
  struct curve *flink;
  struct curve *blink;
  int num;
  Label l;
  Label lmark;
  Point pts;
  Point yepts;
  Point xepts;
  int npts;
  Point general_marks;
  float marksize[2];
  float fill[3];
  float gray[3];
  float afill[3];
  float pfill[3];
  float linethick;
  float mrotate;
  Flist gen_linetype;
  char graytype ;
  char filltype ;
  char afilltype;
  char pfilltype;
  char pattern;
  float parg;
  char apattern;
  float aparg;
  char ppattern;
  float pparg;
  char marktype;
  char linetype;
  char *postscript;
  char *eps;
  int postfile;
  int rarrow;
  int larrow;
  int rarrows;
  int larrows;
  int bezier;
  int poly;
  float asize[2];
  int clip;
} *Curve;

typedef struct string {
  struct string *flink;
  struct string *blink;
  int num;
  Label s;
} *String;

typedef struct hash {
  struct hash *flink;
  struct hash *blink;
  float loc;
  float size;
  int major;
} *Hash;

typedef struct deflt {
  float rotate;
  float fontsize;
  Point general_marks;
  float fill;
  float linethick;
  float marksize[2];
  char *font;
  char hj;
  char vj;
  char marktype;
} *Default;
  
typedef struct axis {
  Label label;
  Label hl;
  float max;
  float min;
  float pmax;
  float pmin;
  float logmin;
  float logfactor;
  float size;
  float psize;
  float factor;
  float hash_interval;
  float hash_start;
  float hash_scale;
  float log_base;
  float draw_hash_marks_at;
  float draw_hash_labels_at;
  float draw_at;
  float gray[3];
  char graytype;
  float gr_gray[3];
  char gr_graytype;
  float mgr_gray[3];
  char mgr_graytype;
  char hash_format;
  int grid_lines;
  int mgrid_lines;
  int draw_hash_labels;
  int draw_axis_line;
  int draw_hash_marks;
  int draw_axis_label;
  int auto_hash_labels;
  int auto_hash_marks;
  int minor_hashes;
  int precision;
  int start_given;
  String hash_labels;
  Hash hash_lines;
  int is_x;
  int is_lg;
} *Axis;

typedef struct legend {
  float linelength;
  float linebreak;
  int anylines;
  float midspace;
  char type; /* 'n' = off, 'u' = userdefined (use Label), 'c' = custom */
  Label l;
} *Legend;

typedef struct graph {
  struct graph *flink;
  struct graph *blink;
  int num;
  float xminval;
  float yminval;
  float xmaxval;
  float ymaxval;
  float x_translate;
  float y_translate;
  Axis x_axis;
  Axis y_axis;
  Curve curves;
  Legend legend;
  String strings;
  Label title;
  int clip;
  int border;
  Default def;
} *Graph;

typedef struct graphs {
  struct graphs *flink;
  struct graphs *blink;
  Graph g;
  float height;
  float width;
  int bb[4]; /* Bounding box */
  char *preamble; 
  char *epilogue; 
  int prefile;
  int epifile;
  int page;
} *Graphs;

extern float ctop();
extern float disttop();
extern float intop();
extern float ptoc();
extern float ptodist();

extern char *getlabel();
extern char *getmultiline();

/* Stuff defined in jgraph.c */

extern Curve new_line();
extern Curve new_curve();
extern Curve get_curve();
extern Graph new_graph();
extern Graph get_graph();
extern String new_string();
extern String get_string();
extern Label new_label();
extern char *MARKTYPESTRS[];
extern char MARKTYPES[];
extern int NMARKTYPES;
extern int NORMALMARKTYPES;
extern char *PATTERNS[];
extern char PTYPES[];
extern int NPATTERNS;
