/*
 * Copyright (1992)
 *
 * Jeff Elman      University of California, San Diego
 * Rik Belew       University of California, San Diego
 * Stefano Nolfi   Institute of Psychology, C.N.R., Rome
 * Filippo Menczer University of California, San Diego
 * Greg Linden     University of California, San Diego
 *
 * This software may be redistributed without charge; this notice
 * should be preserved.
 */

#include <stdio.h>
#include <math.h>
#include <string.h>


#ifdef THINK_C
#include <stdlib.h>
#include <console.h>
#define _H_defs
#endif


/************ ADJUST AS NEEDED AND RECOMPILE ******************/
/***************** SIMULATION PARAMETERS **********************/

#define NINPUTS      5        /* max number of inputs units        */
#define NHIDDENS     6        /* max number of hidden units        */
#define NOUTPUTS     7	      /* max number of output units        */

#define	XMAX         25      /* world x dimention (max=25 for Mac!) */
#define YMAX         25      /* world y dimention (max=25 for Mac!) */

#define TYPES        2        /* number of different food types */

#define ALPHA	     100      /* reproduction threshold */

#define	INIT_ABUNDANCE		0.4 /* For initial food amount */

#define WEIGHT_LIMIT		1.0 /* For weigth initialization */ 

/********************* DEFAULT OPTIONS *************************/

#define WMUPE	0.15	      /* percentage of mutated weights */
#define BMUPE	0.15	      /* percentage of mutated biases */
#define INIT_MUTATION_RANGE	2.0 /* Default mutation range */
#define INIT_LEARN_RATE		0.2 /* Default learning rate */ 

#define INIT_GENERATIONS	20	/* Default generations */
#define INIT_LIFE_CYCLES	100	/* Default sweeps */
#define INIT_POP		50	/* Default population size */

#define FILE_ROOT	"test"	/* Default filename root for file I/O */
				
/********************* SENSOR TYPES *********************/

#define CONTACT		0	/* only sense the cell in front */
#define GUT_SENSOR	1	/* senses the atoms in its own gut */
#define AMBIENT		2	/* sense cells around, by dist. */



/********************* SENSOR PARAMETERS   ***************/

#define MU_SENSOR_PROB 0.0    /* prob. of mutating sensory system */

/* by default, COMPLEX_SIZE=1 and NSENSORS multiple of TYPES */

#define COMPLEX_SIZE 1	      /* number of atoms in complex */

#define NSENSORS     (2*TYPES)	/* total number of sensors */

#define MAXRANGE     10       /* max ambient sensor range */
#define MAXRANGESIZE ((2*MAXRANGE*MAXRANGE)-(4*MAXRANGE)+4)


/********************* MOTOR TYPES **********************/

#define BINARY		0	/* 00 nop, 01 ->, 10 <-, 11 move */


/********************* MOTOR PARAMETERS  **********************/

#define MU_MOTOR_PROB  0.0    /* prob. of mutating motor system */

#define NMOTORS      1	      /* total number of motors */

#define BINARY_ENERGY_USE  0.1 /* Energy cost for movement */
#define BIN_POWER	1	/* binary motor moves by one cell */


/************** CONSTANTS SHOULD NOT BE CHANGED **************/
/************** SENSORY-MOTOR SYSTEM CONSTANTS ***************/


#define UP 		0	/* Used for direction */
#define LEFT 		-1
#define RIGHT 		1
#define DOWN 		2

#define FORWARD 	0	/* Used for location. */
#define LEFT_SIDE 	-1	/* Relative to direction */
#define RIGHT_SIDE 	1	
#define BACKWARD 	2

/******************* MACINTOSH *******************/

#define	SQUARE_SIZE	10
#define GAP_SIZE	2
#define ORG_MOUTH	22
#define START_X		20
#define START_Y		20
#define BORDER_OFFSET	3

#define		BASE_RES_ID  	400

#define		MENU_BAR_ID		400
#define		APPLE_MENU_ID  	400
#define		FILE_MENU_ID  	401
#define		EDIT_MENU_ID  	402
#define		OPTIONS_MENU_ID  	403
#define		GRAPH_MENU_ID	404

#define		WINDOW_ID  		400
#define		ZOOM_CELL_WIND	405
#define		INIT_DIALOG		400
#define		ABOUT_ALERT		400
#define		ERROR_ALERT		450

#define		PREV_ICON				405
#define		NEXT_ICON				406
#define		ARROW_OFFSET_MIDDLE		10
#define		ARROW_OFFSET_BOTTOM		35
#define		ARROW_SIZE				32

#define		ABOUT_CHOICE 	1
#define		QUIT_CHOICE 	3
#define		DISPLAY_CHOICE	1
#define		LASSO_CHOICE	2
#define		PAUSE_CHOICE	3



/******************* SYSTEM CONSTANTS **********************/

#define TRUE		1
#define FALSE		0
#define	NIL_POINTER	0L

#ifndef NULL
#define NULL		0
#endif

#ifndef EOF
#define EOF		(-1)
#endif

/*
 *      @(#)limits.h 1.12 89/08/31 SMI; from S5R2 1.1
 *      LONG_MAX is the largest signed long int
 *	this is 2^31-1=2147483647 (2^15-1=32767 for DOS)
 */
#define LONG_MAX                 0x7FFFFFFF

/*
 * size of state array for random generator: do not change!
 */
#define RAND_DEG 31     

#define LOG_RES		100	/* resolution for sigmoid argument */
#define NSIGMA   	(10*LOG_RES) /* elements of sigmoid matrix */


/********************* TYPEDEF'S ************************/

typedef int boolean;

typedef struct reaction
	{
	boolean possible; 	/* true is entry is in table */
	float energy;		/* >0 esothermic, <0 endothermic */
	int by_prod[TYPES];	/* vector of by-product atoms */
	} reaction;

typedef struct position {
        int x,y;
} position;

typedef struct offset {
        int x,y;
	float d;
} offset;

typedef struct listofPositions{
        position                current;
	float			distance;
        struct listofPositions  *next;
} listofPositions;

typedef struct motor 
	{
	int	system;			/* eg, 1-cell or polar... */
	int	mask[NOUTPUTS];		/* which output units
					 * correspond to the motor */
	int	power; 			/* how far it can move */
	/* int location, orientation; */
	} motor;

typedef struct sensor
	{
	int	system;			/* eg, contact or ambient */
	int	mask[NINPUTS];		/* which input units
					 * correspond to the sensor */
	int	complex[TYPES];		/* eg, {1,0,1} = AC complex */ 
	int	orientation;		/* eg, FORWARD or LEFT_SIDE */
	int	range;			/* max dist. (#binary moves) */
	/* int	location ; */
	} sensor;


struct indiv {
        /*
	 * non-net morph features
	 */

        int 	gutsize;		/* max no. atoms */
	int	gut[TYPES];		/* gut content vector */
        sensor 	sensor_specs[NSENSORS];	/* my sensors */ 
	motor	motor_specs[NMOTORS];	/* my motors */
        float  energy;			/* my energy                       */
	int    worldx;			/* my x position                   */
	int    worldy;			/* my y position                   */
	int    direction;		/* my direction                    */
	int    generation;		/* my generation                   */
	int    lifecycle;		/* my age                          */
	int    id;			/* my number                       */
	int    pid;			/* my parent's number              */
	float  error;			/* my learning error 	           */
	int  fit;			/* number offsprings */
	struct indiv *last;		/* pointer to previous individual  */
	struct indiv *next;		/* pointer to next individual      */

	/*
	 * network features
	 */
	float   **layerp;		 /* the layers themselves           */
	float   **deltap;		 /* deltas                          */
	float   **weightp;		 /* weights between layers          */
	float   **biasp;		 /* biases for each node, each layer*/
	float   **iiweightp;	 /* backup weights between layers   */
	float   **iibiasp;		 /* backup biases for each node, each layer*/

	};


typedef struct cell
	{
	enum {org, food} type;
	union{
		struct	indiv	*ap;	/* if type is org  */
		int		atom;	/* if type is food */
	     } datum;
	struct cell *next; /* points to next org/food in cell */
	} cell;



/*********************** EXTERNS ***********************/

extern char		fileroot[];
extern position			moveDiff();
extern position			find_cell ();
extern listofPositions*	range();
extern listofPositions*	check_world ();
extern position 		binaryMotorMove ();
extern position			distance ();
extern float			clogistic();
extern float			logistic();
extern float			scale();
extern double 			exp();
extern struct indiv *	add_indiv();
extern struct indiv *	delete_indiv();
extern struct indiv *	getnext_indiv();
extern struct indiv *	get_indiv();
extern float 			rans();
extern double atof();
extern char *strcpy();
extern long time();
extern long Rstate[RAND_DEG];
extern char *optarg;
extern long seed;
extern int ecount;
extern FILE *errfp;
extern FILE *tablefp;
extern char nameworlds[];
extern reaction react_table[TYPES][TYPES];
extern int distrib[TYPES][5];
extern float rate;
extern int sweeps;
extern boolean errsig;
extern boolean printout;
extern boolean learn;
extern int generations;
extern int start_num_amoebas;
extern int startgeneration;
extern float mutations_range;
extern int mutations;
extern int bmutations;
extern boolean mbiasestoo;
extern boolean unary_reactions;
extern int verbose;
extern int save_best;
extern int gutsize;
extern int save_everyn;
extern int *layer_descp;
extern int nlayers;
extern	float weight_limit;
extern  int nlayers;
extern  int *layer_descp;
extern	int s_type[];
extern	int m_type[];
extern int   s_range[];
extern int   s_orient[];
extern  int gutsize;	
extern	FILE	*cfp;
extern  int   pop_size;
extern  float   energy_reserve;
extern  FILE  *resfp;
extern  int   lifecycle;
extern  int   gen;
extern  struct indiv *a_head;
extern  float global_error;
extern  boolean gDone;
extern  float amoebat[];
extern   struct indiv *end_head;
extern  struct indiv *t_head;
extern  cell  *world[XMAX][YMAX];
extern  offset *off_vec[4][MAXRANGESIZE];
extern float	norm_dist[];
extern float  sigmoide[];
extern float    abundance;
extern boolean	gInteractive;
extern void     del_org_Interactive(); 
extern void     InteractiveInit(); 
extern void     InteractiveFinalSetUp(); 
extern void     moveInteractDrawSelectFrame(); 
extern void     update_world_Interactive(); 
extern void     EventsInteractive(); 
extern void     actInWorldInteractive(); 
extern void     moveInteractiveLasso(); 
extern void     updateGenInteractive(); 
extern void     ins_org_Interactive(); 





#ifdef THINK_C
extern WindowPtr	gTheWindow, gZoomCellWindow;
extern DialogPtr	gInitDialog;
extern MenuHandle	gAppleMenu, gFileMenu, gEditMenu, gOptionsMenu, gGraphsMenu;
extern boolean		gLassoOn, gPaused;
extern Point		gCurrentZoomCell;
extern int			gCurrentZoomItem;
#endif

