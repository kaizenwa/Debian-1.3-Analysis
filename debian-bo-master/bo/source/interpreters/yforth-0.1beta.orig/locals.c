/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     locals.c
 * Abstract:        locals word set
 */

/* Implementation notes
 * Local variables make use of the register "bp" of the Virtual Machine,
 * which stores the location, wihtin the return stack, of the first
 * local variable. All references to local variables are made relative
 * to this register. This implies that "bp" must be saved between calls of
 * words that make use of local variables, and every "exiting word" that
 * make a word terminate must reset it.
 * This is achieved by an auxiliary variable, called "local_defined", set
 * to 1 inside a colon definition when local variables are used.
 * Local names are stored dinamically by allocating a structure "word_def"
 * for any name. The function which searches the vocabulary for a particular
 * word has been modified accordingly so that the first try is always made
 * in this dynamic vocabulary, pointed by "first_local".
 */

#include <string.h>
#include <stdlib.h>
#include "yforth.h"
#include "core.h"
#include "locals.h"

/**************************************************************************/
/* VARIABLES **************************************************************/
/**************************************************************************/

static struct word_def *first_local;
static unsigned int local_defined;

/**************************************************************************/
/* WORDS ******************************************************************/
/**************************************************************************/

void _paren_local_paren() {
	register UCell u = (UCell) *sp++;
	register Char *s = (Char *) *sp++;
	declare_local(s, u);
}

/* restore "bp" register from return stack */
void _paren_bp_restore_paren() {
	rp += (Cell) *ip++;
	bp = (Cell *) *rp++;
}

/* save "bp" register on return stack */
void _paren_bp_save_paren() {
	*--rp = (Cell) bp;
	bp = rp - 1;
}

/* push on the data stack the value of i-th local variable, where i is the
 * Cell value pointed to by "ip" when "_paren_read_local_paren" is called.
 */
void _paren_read_local_paren() {
	register UCell offset = (UCell) *ip++;
	*--sp = *(bp - offset);
}

/* update the i-th local variable with the Cell value on the data stack.
 * See "_paren_read_local_paren" for a comment about the value "i"
 */
void _paren_write_local_paren() {
	register UCell offset = (UCell) *ip++;
	*(bp - offset) = *sp++;
}

/**************************************************************************/
/* AUXILIARY FUNCTIONS ****************************************************/
/**************************************************************************/

/* clear_locals: called inside the compilation of a colon definition to
 * compile the code that restore "bp" and free the dynamic vocabulary of
 * local names
 */
void clear_locals() {
	if (local_defined) {
		compile_cell((Cell) _paren_bp_restore_paren);
		compile_cell((Cell) local_defined);	/* # di variabili locali */
	}
	free_locals();
	local_defined = 0;
}

/* free_locals: release the dynamic vocabulary. Called by "clear_locals". */
void free_locals() {
	register struct word_def *p = first_local, *p1;
	while (p) {
		free(p->name);
		p1 = p->link;
		free(p);
		p = p1;
	}
	first_local = NULL;
}

void init_locals() {
}

/* declare_local: declare a new local variable. If it's the first local
 * variable for the current colon definition, compile the code to save
 * the register "bp"
 */
void declare_local(Char *s, UCell u) {
	struct word_def *p = (struct word_def *) malloc(sizeof(struct word_def));
	if (p) {
		p->name = (Char *) malloc(u + 1);
		if (p->name) {
			p->name[0] = (Char) u;
			memcpy(p->name + 1, s, u);
			p->link = first_local;
			p->class = A_LOCAL;
			p->func[0] = (pfp) (local_defined++);
			if (!first_local) compile_cell((Cell) _paren_bp_save_paren);
			first_local = p;
		} else free(p);
	}
}

/* get_first_local: interface function that returns a pointer to the first
 * local name defined (actually is the last name, since names are stored
 * in reverse order for efficiency, but this doesn't matter)
 */
struct word_def *get_first_local() {
	return (first_local);
}

/* locals_defined: interface function that returns true if current word
 * has some local name defined
 */
int locals_defined() {
	return (local_defined);
}

