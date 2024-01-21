#ifndef _DELTAQ_H_
#define _DELTAQ_H_
/*
 * deltaq.h
 *
 * Manages the delta queue.  This queue is used for multiple time delays.  
 * It is based upon a similiar structure in "Operating System Design Vol 1"
 * by D.E. Comer.  I changed enhanced their idea by using linked lists so we 
 * have an 'inifinite' list.
 *
 * Changes:
 *	14/03/95	Created
 *	20/03/95 cs  	Added NULL to last 'next' ptr in queue (always handy)
 *			Disable alarm while in routine and take into account elapsed time
 * 	03/04/95 cs	Added some sigblocks for alarm signal
 *	04/04/95 cs 	Sanity checks for alarm() parameters. Alarm returns 0 if
 			time remaining < 0.5? secs, but alarm(0) turns it off
 			
 */

struct dq_node {
	unsigned int	delay;		/* Max delay is MAX_UINT */
	int	id;		/* ID number for queue entry */
	void	(*funct)(int id);	/* Function to use when timer expires */
	struct dq_node *next;	
};

int dq_add(unsigned int delay, void (*funct)(int));
int dq_del(int id);
void dq_dump(char *c, int len);

#endif /* DELTAQ_H_ */