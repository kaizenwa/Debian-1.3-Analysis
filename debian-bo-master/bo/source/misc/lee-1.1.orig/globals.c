/* globals.c
 *                         Copyright (1992)
 *
 *         Jeff Elman.  University of California, San Diego
 *          Rik Belew.  University of California, San Diego
 *      Stefano Nolfi.  Institute of Psychology, Rome.
 *    Filippo Menczer.	University of California, San Diego
 *        Greg Linden.	University of California, San Diego
 *
 *        This software may be redistributed without charge;
 *                 this notice should be preserved.
 */



#include "defs.h"

/*
 * parameters
 */


#ifdef THINK_C
WindowPtr		gTheWindow, gZoomCellWindow;
DialogPtr		gInitDialog;
MenuHandle		gAppleMenu, gFileMenu, gEditMenu, gOptionsMenu, gGraphsMenu;
boolean			gLassoOn = TRUE, gPaused = FALSE;
Point			gCurrentZoomCell;
int				gCurrentZoomItem=0;
#endif


boolean			gDone = FALSE;
boolean			gInteractive = FALSE;

long	seed;           /* random seed  */                      
boolean    	errsig = FALSE; 	/* print error signal  */
int	ecount = 0;            /* save global error every N sweeps         */
boolean     learn = TRUE;     	/* Turn on/off learning.  */
boolean     printout = FALSE;	/*  print lifecycle information              */
int     sweeps = INIT_LIFE_CYCLES;
				/*  Moves per organism per generation */
float   rate = INIT_LEARN_RATE;
				/* Default learning rate */
int     generations=INIT_GENERATIONS;
			    /* number of generations                    */
int     startgeneration = 0;  /* Generation to start with */

float   mutations_range = INIT_MUTATION_RANGE;
				/* range of mutations                       */
int     mutations; 		/* number weight mutations */
int     bmutations; 		/* number bias mutations       */
boolean     mbiasestoo=TRUE;   /* if 1 we mutate biases too                */
boolean     unary_reactions=FALSE;   /* if 1 we read unary reactions from .tu file        */

int     verbose=1;             /* verbose level  (0-1-2-3-4-5) */
int     save_best=0;           /* save the best N individual each gen.     */
int     save_everyn=0;         /* save individual each X generations       */


/*
 * files
 */

FILE   *errfp;	               /* globals error file                       */
FILE   *resfp;                 /* results file                             */
char    fileroot[64] = FILE_ROOT;       /* for config file name */


/*
 * organisms
 */

int     start_num_amoebas = INIT_POP;
				/* initial number of organisms        */
int     gutsize;		/* initialized from .cf file */
int	nlayers;		/* number of layers in network            */
int	*layer_descp;		/* no. of nodes in each layer             */
float	weight_limit=WEIGHT_LIMIT; /* for weight initialization */

struct  indiv    *a_head;	/* linked list of amoeba strux            */
struct  indiv    *t_head;	/* linked list of amoeba strux            */
struct  indiv    *end_head;	/* end of the chain                       */


/*
 * others
 */

char	*optarg;		/* for options */
float	global_error;		/* store global error                     */
int     gen;                    /* current generation                     */
int     lifecycle;              /* current lifecycle                      */
float   amoebat[NINPUTS];            /*  amoeba teach array               */
float   sigmoide[NSIGMA+1];     /*  logistic matrix                       */
int	pop_size;		/* current number of organisms            */
float energy_reserve=0.0;	/* energy at death accumulated
				 * since start of run: <= 0.0 
				 * (not saved if loading, -g)
				 */
int	s_type[NSENSORS];	/* sensor types */
int	s_orient[NSENSORS];	/* sensor types */
int	s_range[NSENSORS];	/* sensor types */
int	m_type[NMOTORS];	/* motor types */
offset  *off_vec[4][MAXRANGESIZE];	
				/* ambient sensor offsets 
				 * with increasing range...
				 * 1st index is direction:
				 * 0 = LEFT, 1 = UP,
				 * 2 = RIGHT, 3 = DOWN
				 */
float	norm_dist[MAXRANGE];	/* 
				 * used for normalization of ambient 
				 * sensor signal
				 */
reaction react_table[TYPES][TYPES];
				/* each entry of this
				 * symmetric table has
				 * the corresponding
				 * reaction structure 
				 */
float	abundance = INIT_ABUNDANCE;
				/* Initial fraction of cells 
				 * that may have each type of food;
				 * for now, same for all types
				 */
cell *world[XMAX][YMAX]; 	/* each cell points to a linked 
				 * list of elements, one for each
				 * food piece or (pointer to)
				 * eventual individual present
				 * in the cell
				 */
int	distrib[TYPES][5];	/* the elements of each food
				 * type have pseudo-normal
				 * distribution, with these
				 * 2-D centers and variances;
				 * the latter is described in
				 * terms of the number of
				 * uniform distributions 
				 * (i.e., calls to random())
				 * we average over;
				 * the last field gives the
				 * regeneration rate for 
				 * elements of that type
				 */
