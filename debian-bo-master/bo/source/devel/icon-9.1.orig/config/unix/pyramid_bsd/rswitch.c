
/*
 * Necessary system headers
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/context.h>
#include <sys/errno.h>
#include <sys/psw.h>

/* #define DEBUG */
/* #define DEBUGVERBOSE */

#ifdef DEBUGVERBOSE
#ifndef DEBUG
#define DEBUG
#endif
#endif

/*
 * Some useful macros
 */
#define PAGE_SIZE	getpagesize()
#define Pages(x)	(PAGE_SIZE*(x))
#define CONTROL_STACK	0.1
#define CSP_ALIGN(x)	(char *)(((int)(x))&~0x7f)
#define INITIAL_PSW	(PSW_FO|PSW_FZ&PSW_Z)

extern int errno;
extern int new_context();
static int doreturn=0;

/*
 * Structure to hold stack frames
 * Members of (struct context) frame point into (char *)space
 */
typedef struct {
	struct context *frame;
	char *space;
} Context;

coswitch(old_cs, new_cs, first)
int *old_cs, *new_cs;
int first;
{
	Context *old = (Context *)old_cs;
	Context *new = (Context *)new_cs;
	int ret;

#ifdef DEBUG
	FILE *devtty;
	devtty = (FILE *)fopen( "/dev/tty", "w" );
	setbuf( devtty, NULL );
	fprintf( devtty, "---> Entered coswitch old:0x%x new:0x%x first:0x%x\n",
	old,new,first);
#endif

	 if(  first == 0 ) {
		/*
		 * First invokation - allocate a new context,
		 * save old context (done by chgstack)
		 * Should end up executing with new stack in new_context().
		 */
		(void)mallocframe( old ); /* to save present context */
		(void)mallocframe( new ); /* to create new context   */
#ifdef DEBUGVERBOSE
		printframe( devtty, "Changing stack to new frame", new->frame );
#endif
		/*
		 * Not ok to return from this chgstack, except as noted below.
		 */
		doreturn=0;
		ret = chgstack( new->frame, old->frame );
#ifdef DEBUG
		fprintf( devtty, "chgstack: returned status = %d\n", ret );
#endif
		if( ret < 0 ) {
			perror("chgstack failed");
			syserr("no point in continuing");
		}
		if( ! doreturn )
		syserr("new_context() should not have returned in coswitch");

	} else {

		/*
		 * New context has already been allocated. We save the present
		 * frame and then restore new_cs context - this is effectively a
		 * longjmp to the next statement after the chgstack above.
		 * Hence we set doreturn to indicate that it was ok to
		 * return from the chgstack.
		 */
#ifdef DEBUGVERBOSE
		printframe( devtty, "Changing stack to new frame", new->frame );
#endif
		doreturn=1;
		ret = chgstack( new->frame, old->frame );
		/* NOTREACHED */
#ifdef DEBUG
		fprintf( devtty, "chgstack: returned status = %d\n", ret );
#endif
		if( ret < 0 ) {
			perror( "chgstack failed" );
			syserr("no point in continuing");
		}
	}
}

void mallocframe( new )
Context *new;
{
	if( (new->space = (char *)malloc( Pages( 109 ) )) == (char *)0 )
nomem:
		/*
		 * Fatal termination - this may occur given
		 * the amount of dynamic memory we need per stack frame
		 */
		syserr("coswitch: out of dynamic memory");
	if( (new->frame = (struct context *)
		malloc( sizeof( struct context ) )) == (struct context *)0 )
		goto nomem;
	/*
	 * Ok, we have the memory - interconnect it in a suitable division
	 * between data and control stacks.
	 */
	new->frame->stktop = CSP_ALIGN(
		new->space + Pages(6+(int)(100*(1.0-CONTROL_STACK))));
	new->frame->stkcfp =
		new->frame->stkbottom = (new->frame->stktop - Pages(3));
	new->frame->codeaddr = new_context;
	new->frame->psw = INITIAL_PSW;
}

#ifdef DEBUGVERBOSE
void printframe( file, message, frame )
FILE *file;
char *message;
struct context *frame;
{
fprintf(file,"Stack frame \"%s\" at 0x%x\n", message, frame);
fprintf(file,
"\tstktop:0x%x stkcfp:0x%x stkbot:0x%x codeaddr:0x%x psw:0x%x\n",
frame->stktop,frame->stkcfp,frame->stkbottom,frame->codeaddr,frame->psw);
}
#endif
