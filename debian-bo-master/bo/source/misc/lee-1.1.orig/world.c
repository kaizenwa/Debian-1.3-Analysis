/* world.c
 *                         Copyright (1993)
 *
 *         Jeff Elman.  University of California, San Diego
 *          Rik Belew.  University of California, San Diego
 *      Stefano Nolfi.  Institute of Psychology, Rome.
 *    Filippo Menczer.  University of California, San Diego
 *        Greg Linden.  University of California, San Diego
 *
 *        This software may be redistributed without charge;
 *                 this notice should be preserved.
 */
 
#include "defs.h"





/*
 * called in populati.c;
 * the world is updated to insert the organism in its cell
 */
ins_org(ip)

	struct indiv *ip;
{
	cell *temp;

	temp = (cell *)malloc(sizeof(cell));
	if (temp == (cell *) NIL_POINTER)
	{
		fprintf(stderr, "cell malloc error in ins_org()\n");
		exit(-1);
	}
	temp->type = org;
	temp->datum.ap = ip;
	temp->next = world[ip->worldx][ip->worldy];
	world[ip->worldx][ip->worldy] = temp;

	ins_org_Interactive(ip);
}



/*
 * the world is updated to insert a food element in a cell
 */
boolean ins_food(element, x, y, check)

	int element;
	int x, y;
	boolean check;
{
	cell	*temp, *temp1;

	temp = temp1 = world[x][y];
	while (temp != (cell *)NIL_POINTER) {
		if (check)
		  if (temp->type==food)
		    if (react_table[temp->datum.atom][element].possible)
			return(FALSE);
		temp1 = temp;
		temp = temp->next;
	}

	temp = (cell *)malloc(sizeof(cell));
	if (temp == (cell *) NIL_POINTER)
	{
		fprintf(stderr, "cell malloc error in ins_food()\n");
		exit(-1);
	}
	temp->type = food;
	temp->datum.atom = element;
	temp->next = (cell *)NIL_POINTER;
	if (temp1 == (cell *)NIL_POINTER) world[x][y] = temp;
	else temp1->next = temp;

	return(TRUE);
}

/*
 * called in populati.c;
 * the world is updated to delete the organism from its cell
 */
del_org(ip)

	struct indiv *ip;
{
	cell *temp, *temp2;
	boolean done = FALSE;
	int	itemNum=0;
	int	x,y;
	
	x = ip->worldx;
	y = ip->worldy;

	temp = world[ip->worldx][ip->worldy];
	if (temp == (cell *)NIL_POINTER)
	{
		printf("ERROR: can't find indiv to be deleted");
		exit(1);
	}
	else if ((temp->type == org) && (temp->datum.ap == ip))
	{
		world[ip->worldx][ip->worldy] = temp->next;
		free((char *) temp);
		done = TRUE;
	}
	else while ((temp->next != (cell *)NIL_POINTER) && 
		    !((temp->next->type == org) && 
		      (temp->next->datum.ap == ip))) {
			temp = temp->next;
			itemNum++;
	}
	if (!done)
	{
		if (temp->next == (cell *)NIL_POINTER)
		{
			printf("ERROR: can't find indiv to be deleted");
			exit(1);
		}
		else
		{
			temp2 = temp->next;
			temp->next = temp2->next;
			free((char *) temp2);
		}
	}


	del_org_Interactive(x,y, itemNum);
}




/*
 * called in populati.c;
 * the world is filled with food elements of
 * the various types according to their distribution
 */
init_world()

{
	register int 	x,y,i;
	int		t;

	/*
	 * initialize world cells
	 */
	for (x=0; x<XMAX; x++)
	   for (y=0; y<YMAX; y++)
	      world[x][y] = (cell *)NIL_POINTER;

	/*
	 * fill in cells with food elements
	 */
	t = abundance * XMAX * YMAX;
	for (i=0; i<t; i++) update_world();
}



/* 
 * called in populati.c;
 * the world is replenished with new food elements
 * to make up for the ones that have been consumed
 */
update_world()

{
	int 		t,i;
	int 		xpos, ypos;
	int		counter;

 
	for (t=0; t<TYPES; t++)
	   	if (0.5 + rans(0.5) < (float)distrib[t][4]/100.0)  
	   	{

			/* Don't allow two reactable atoms in the
			   same cell.  */
	
			counter=0;
			
			do 
			{
				if (counter>100) 
				{
					printf("Unable to place food!\n");
					exit(1);
				}
	
				xpos = 0;
				ypos = 0;
				for (i=0; i<distrib[t][2]; i++)
				   xpos += (mrand(XMAX)-(int)(XMAX/2));
				for (i=0; i<distrib[t][3]; i++)
				   ypos += (mrand(YMAX)-(int)(YMAX/2));
				xpos /= distrib[t][2];
				ypos /= distrib[t][3];
				xpos += (XMAX+distrib[t][0]);
				ypos += (YMAX+distrib[t][1]);
				xpos %= XMAX;
				ypos %= YMAX;
				counter++;
			} while (!(ins_food(t, xpos, ypos, TRUE)));
			
			update_world_Interactive(xpos, ypos);
		}
}




/*      After digestion, the organism can only
        retain gutSize number of atoms inside it.
        Extra must be removed. */

int expel (organism)
        struct indiv    *organism;
{
        int atoms_in_gut=0, counter, numExpelled = 0;

        for (counter=0; counter<TYPES;counter++)
                atoms_in_gut+=organism->gut[counter];

        while (atoms_in_gut>organism->gutsize) {
                counter = mrand(TYPES);
                if (organism->gut[counter]>0) {
                        organism->gut[counter]--;
                        ins_food(counter, organism->worldx, organism->worldy, FALSE);
                        atoms_in_gut--;
                        numExpelled++;
                }
        }

        return(numExpelled);
}


/*
 * u_digest() takes care of easy case when reactions
 * are unary (-U option) rather than binary:
 * all atomes reactive, no by-products...
 */
void u_digest (organism)
        struct indiv    *organism;
{
	int x;
	float calories = 0.0;

	for (x=0;x<TYPES;x++)
	{
		calories += organism->gut[x] * react_table[x][0].energy;
		organism->gut[x] = 0;
		organism->energy += calories;
	}
}


/*      digest() attemps to match the atoms in the
        gut to get a reaction.  Any reaction can
        produce new atoms which are still in
        the gut and can produce secondary
        reactions.  These reactions can cause
        energy to be both added and subtracted from
        the organism. */

void digest (organism)
        struct indiv    *organism;
{

        int     order[TYPES], counter, x,y, swap;
        boolean react;

        do {
                react = FALSE;

                /* Randomly determine the order with which to check
                the reactions */
                for (counter=0;counter<TYPES; counter++)
                        order[counter] = counter;
                if (TYPES>1) {
                        for (counter = 0; counter<TYPES*2; counter++)
{
                                if (mrand(2) == 1) {
                                        x = mrand(TYPES);
                                        do {
                                                y = mrand(TYPES);
                                        } while (y==x);
                                        swap = order [x];
                                        order[x] = order [y];
                                        order[y] = swap;
                                }
                        }
                }

                for (x=0;x<TYPES;x++)
                        if (organism->gut[order[x]]>0)
                                for (y=0;y<=x;y++)
                                        if (organism->gut[order[y]]>0)
                                               if ((x!=y)||(organism->gut[order[x]]>1))
                                                        react = reactAtoms(organism, order[x], order[y]);
        } while (react);
}


/*      Used by digest().
        If the atoms x and y can react, react
        them, modify the energy of of the organism appropriately,
        remove the atoms from the gut vector, add the byproducts,
        and set react = TRUE */

boolean
reactAtoms (organism, x, y)
        struct indiv            *organism;
        int                     x,y;
{
        int                     counter;


        if (react_table[x][y].possible==FALSE)
                return(FALSE);
        else {
                organism->gut[x]--;
                organism->gut[y]--;

                for (counter=0; counter<TYPES; counter++)
                        organism->gut[counter] += react_table[x][y].by_prod[counter];

                organism->energy+=react_table[x][y].energy;

                return(TRUE);
        }
}





/*
 * called by act_in_world() in gworld.c;
 * all food-content of new cell position is put
 * in the gut (may exceed gut_size);
 * the world is updated (food removed)
 * 
 * NOTE: for now, organisms cannot eat each other
 */
ingest(ip)

	struct indiv *ip;
{
	cell		*t1, *t2;
	int			numIngested=0;

	while (((t1 = world[ip->worldx][ip->worldy]) != 
		(cell *)NIL_POINTER) && (t1->type == food))
	{
		++(ip->gut[t1->datum.atom]);
		numIngested++;
		world[ip->worldx][ip->worldy] = t1->next;
		free((char *) t1);
	}
	if ((t2 = t1) != (cell *)NIL_POINTER) 
	  if (t1->type == org)
	    	while (t1->next != (cell *)NIL_POINTER)
	    {
		t1 = t1->next;
		if (t1->type == food)
		{
			++(ip->gut[t1->datum.atom]);
			numIngested++;
			t2->next = t1->next;
			free((char *) t1);
			t1 = t2;
		}

		else if (t1->type == org) 
			t2 = t1;
		else
		{
			printf("ERROR: unrecognized cell content\n");
			exit(1);
		}
	}
	    
	return(numIngested);	    
}



/*
 * act_in_world() causes the organism to move
 * and ingest food; if reactions are unary, all atoms
 * are processes and there is no need to expel
 */

void act_in_world (organism)
        struct indiv            *organism;
{
        int     startX, startY, numExpelled=0;

        startX = organism->worldx;
        startY = organism->worldy;

        move(organism);
        sense_world(organism);
        ingest(organism);
        if (unary_reactions) u_digest(organism);
	else
	{ 
		digest(organism);
        	numExpelled = expel(organism);
	}

        actInWorldInteractive(organism, numExpelled, startX, startY);
}



