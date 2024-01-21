/* populati.c
 *                         Copyright (1992)
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
 * main loop, let individuals evolve generation by generation
 */

generati()

{
	struct  indiv *ap;
	char 		buf1[40];
	char 		buf2[40];
	int 		flag;
	int 		k;
	int 		population;
	register int 	i;
	register float	*input;
	long 		tempo, tempo0;
	char   resfile[64];            /* the results file name      */




	if (verbose>0) {
		/*
		 * open the results file: the name is the seed
		 * and it contains:
		 * -- generation
		 * -- pop_size
		 * -- whatever else is printed by save_dat(),
		 *    depending on the experiment...
		 */
		sprintf(resfile, "%ld.dat", seed);
		resfp = fopen(resfile, "a");
	}


	/*
	 * go thru all generations requested
	 */
	time(&tempo0);
	for (gen=startgeneration; gen < generations; gen++)
	{


		updateGenInteractive();


		/*
		 * we open .err file
		 *
		 * ATTENTION: this file contains errors in a
		 * format different fron the original, due to
		 * having swapped the loops over indiv's and
		 * lifecycles!!!
		 */
		if ((ecount > 0) && (errsig) && (verbose>0))
		{
			sprintf(buf1,"%d",gen);
			genfilen(buf2,buf1,"err");
			errfp = fopen(buf2, "w+");
			if (errfp == NULL)
			{
				printf("ERROR: Can't open .err file");
				exit(-1);
			}
		}

		if (verbose > 0) 
		{
			time(&tempo);
			printf("\nRunning generation: %d,\t",gen);
			printf("Pop: %d,\t",pop_size); 
			printf("Time: %ld\n",tempo-tempo0);

			/*
			 * save data
			 */
			fprintf(resfp, "%d\t%d", gen, pop_size);
			save_dat(resfp);
			fprintf(resfp, "\n");
			fflush(resfp);

		}

	        for (lifecycle=0; lifecycle < sweeps; lifecycle++)
		  {

                   if (verbose >= 2)
                        printf("\ngen %d, time %d ",gen,lifecycle);


	           /*
	            * for each time, we let the individuals live;
		    * this is done stochastically, as 
		    * each individual is picked at random
		    * out of the current population
	            */
		   population = pop_size;
		   for (k=0; k<population; k++)
		   {
		      ap = get_indiv(mrand(pop_size));
		      ap->lifecycle++;


		      EventsInteractive();


			/*
			 * sense the world,
			 * activate the network
			 * and make an action
			 */
			sense_world(ap);
			activate_net(ap);
			act_in_world(ap);


                        /*
                         *  we print cycle by cycle informations
                         */
                        if ((printout) && (verbose>0))
                           print_out(ap,lifecycle);


			/*
			 * LEARNING:
			 * 1. we copy activations from input
			 * units to teaching inputs, in order
			 * for learning to compute the correct
			 * prediction error with the new
			 * sensory input (after the move)
			 * 2. we change weights
			 */
			input = *ap->layerp;
			for (i=0; i<*layer_descp; i++)
				amoebat[i] = *input++;
			if ((learn) || (errsig))
		        {
				update_net(ap, amoebat, learn);
                                ap->error += global_error;


                                if ((ecount > 0) && (((lifecycle + 1) % ecount) == 0) && (lifecycle > 0) && (verbose > 0))
                                   {
                                     fprintf(errfp,"%.2f ",ap->error);
                                     ap->error = 0.0;
                                   }
			}



			/*
			 * if energy is high, reproduce:
			 * this is for now deterministic and 
			 * determined by the energy threshold
			 * ALPHA, which at the beginning is
			 * twice the energy expected average
			 */
			if (ap->energy >= ALPHA) reproduce(ap);

			/*
			 * if energy is low, die
			 */
			if (ap->energy <= 0.0) die(ap);
		     
		     
			if (gDone)
				break;
	     
		   }

                   if ((ecount > 0) && (errsig) && (((lifecycle + 1) % ecount) == 0) && (verbose > 0))
                     fprintf(errfp,"\n");

	       	   if (gDone)
			break;

		   /*
		    * replenish world food  
		    */
		   update_world();

	          }

               if (verbose >= 2)
                  printf("\n\nNext generation\n\n");

               if (((ecount > 0) || gDone) && (verbose>0))
                   fclose(errfp);

		next_generation();

		if (gDone)
			break;

	       /*
	        * save this generation 
	        */
		if ( gen == (generations - 1) || 
		   ((save_everyn > 0) && ((gen % save_everyn) == 0)))
			
			if (verbose>0)
			{
				save_world(gen);
				save_all_indiv(gen);
			}

           }

	if (verbose > 0)
		fclose(resfp);

}





/*
 * init first generation
 */

init_pop(num_ind)

	int	num_ind;
{
	struct	 indiv *ap;	
	register int i;

	/*
	 * initialize the linked list of indiv structures;
	 * add_indiv() needs struct ptr, generation, id, parent id
	 * in the indiv struct (see defs.h) 
	 */
	
	a_head = (struct indiv *) malloc(sizeof(struct indiv));
	if (a_head == NULL) {
		fprintf(stderr, "a_head malloc err\n");
		exit(-1);
	}
	ap = a_head;
	ap->next = (struct indiv *)-1;
	ap->last = (struct indiv *)0;
	for (i = 0; i < num_ind; i++)
	{
		ap = add_indiv(ap, 0, i, 0);
	}
	end_head = ap;
	end_head->id = -999;
}



/*
 * add an individual to the linked list of individual structures; take
 * a valid (but empty) amoeba structure pointer, fill it out, and
 * return the address of the next amoeba structure.  Other
 * stuff (like weights) must get filled in by calling routine.
 */
struct indiv *
add_indiv(ip, g, id, pid)

	struct  indiv *ip;
	int	g;		/* generation number */
	int	id;		/* id of this amoeba */
	int	pid;		/* parent id */
{
	int i,j;
	int size,count;

	/*
	 * we'd better not tromp on existing structures
	 */
	if (ip->next != (struct indiv *)-1)
	       {
		fprintf(stderr, "individual already exists\n");
		exit(-1);
	       }
        /*
         * we allocate memory for the net
         */
        build_geno_net(ip);
        build_pheno_net(ip);

       /*
        * load actual indiv only for the initial generation
        */
       if (startgeneration == 0)
       {
	ip->generation = g;
	ip->id         = id;
	ip->pid        = pid;
	ip->energy     = (float)mrand(ALPHA);
	ip->gutsize    = gutsize;
	ip->error      = 0.0;
	ip->fit        = 0;
	ip->lifecycle  = 0;
	ip->worldx     = mrand(XMAX);
	ip->worldy     = mrand(YMAX);
	
	switch (mrand(4)){
		case 0:
			ip->direction  = UP;
			break;
		case 1:
			ip->direction  = LEFT;
			break;
		case 2:
			ip->direction  = DOWN;
			break;
		case 3:
			ip->direction  = RIGHT;
			break;
	}
				
	for (i=0;i<TYPES;i++) ip->gut[i] = 0;
	count = 0;
	for (i=0;i<NSENSORS;i++)
	{

		ip->sensor_specs[i].system = s_type[i];

		size = sensor_size(ip->sensor_specs[i].system);
		count += size;
		for (j=0;j<*layer_descp;j++)
			ip->sensor_specs[i].mask[j] = get_mask(count, size, j);	
		for (j=0;j<TYPES;j++)
			ip->sensor_specs[i].complex[j] = 0;

		/*
		 * DEFAULT INITIALIZATION OF SENSOR COMPLEXES
		 * assuming	NSENSORS=const*TYPES
		 * 		MU_SENSOR_PROB=0.0 
		 * result is (e.g., COMPLEX_SIZE=1):
		 *	A(0),B(1),...,X(TYPES-1),A,B,...,X,...
		 */
		for (j=0;j<COMPLEX_SIZE;j++)
                	++ip->sensor_specs[i].complex[i%TYPES];

		/*
		 * alternatively we can use the following: 
		 * RANDOM INITIALIZATION OF SENSOR COMPLEXES
		 *	
		for (j=0;j<COMPLEX_SIZE;j++)
			++ip->sensor_specs[i].complex[mrand(TYPES)];
		 *
		 */

		ip->sensor_specs[i].orientation = s_orient[i];
		ip->sensor_specs[i].range = s_range[i];

		/* location = ...; */
	}
	count = 0;
	for (i=0;i<NMOTORS;i++)
	{

		ip->motor_specs[i].system = m_type[i];

		size = motor_size(ip->motor_specs[i].system);
		count += size;
		for (j=0;j<*(layer_descp+nlayers-1);j++)
			ip->motor_specs[i].mask[j] = get_mask(count, size, j);
		ip->motor_specs[i].power = motor_power();
		/* location, orientation = ...; */
	}
	/*
	 * update the world to insert the org. in its cell
	 */
	ins_org(ip);
        /*
         * initialize net weights and biases
         */
	init_net(ip);
       }

	/*
	 * get a new structure and stick it in 'next' element
	 */
	ip->next = (struct indiv *)malloc(sizeof(struct indiv));
	if (ip->next == NIL_POINTER)
	      {
		fprintf(stderr, "malloc error on individual\n");
		exit(-1);
	      }
	/*
	 * put old guy's address the new one's structure so he can
	 * work backward
	 */
	(ip->next)->last = ip;	
	/*
	 * tells us the next guy not yet real
	 */	
	(ip->next)->next = (struct indiv *)-1;	
	return(ip->next);
}



/*
 * generate a children ip2 from a father ip1:
 * genotype of father is used to generate offspring;
 * for now, development is limited to a copy of the
 * backup weights (genotype) onto the normal weights
 * (phenotype) 
 */

reproduce(ip1)

	struct  indiv *ip1;

{
	struct  indiv *ip2;
	float  **iiwp1;
	float  **iibp1;
	float  *iiw1;
	float  *iib1;	
	float  **wp2;
	float  **bp2;
        float  **iiwp2;
        float  **iibp2;
	float  *w2;
	float  *b2;
        float  *iiw2;
        float  *iib2;
	int    *l;
	int    i,j,ii;

	/*
	 * get a new structure for the child 
	 */
	ip2 = (struct indiv *)malloc(sizeof(struct indiv));
	if (ip2 == NIL_POINTER)
	      {
		fprintf(stderr, "malloc error on child\n");
		exit(-1);
	      }
	/*
 	 * append child to end of population chain
 	 */
	(end_head->last)->next = ip2;
	ip2->last = end_head->last;
	ip2->next = end_head;
	end_head->last = ip2;

	ip2->gutsize    = ip1->gutsize;
	ip2->generation = ip1->generation + 1;
	ip2->lifecycle  = 0;
	ip2->id		= pop_size;
	ip2->pid        = ip1->id;
	ip2->error      = 0.0;
	ip1->fit++;
	ip2->fit        = 0;
	ip1->energy	/= 2.0;
	ip2->energy	= ip1->energy;
	ip2->worldx     = mrand(XMAX);
	ip2->worldy     = mrand(YMAX);
	switch (mrand(4)){
		case 0:
			ip2->direction  = UP;
			break;
		case 1:
			ip2->direction  = LEFT;
			break;
		case 2:
			ip2->direction  = DOWN;
			break;
		case 3:
			ip2->direction  = RIGHT;
			break;
	}
        for (i=0;i<TYPES;i++) ip2->gut[i] = 0;
        for (i=0;i<NSENSORS;i++)
        {
                ip2->sensor_specs[i].system = ip1->sensor_specs[i].system;
                for (j=0;j<*layer_descp;j++)
                        ip2->sensor_specs[i].mask[j] = ip1->sensor_specs[i].mask[j];
                for (j=0;j<TYPES;j++)
                        ip2->sensor_specs[i].complex[j] = ip1->sensor_specs[i].complex[j];
		ip2->sensor_specs[i].orientation = ip1->sensor_specs[i].orientation;
		ip2->sensor_specs[i].range = ip1->sensor_specs[i].range;
                /* location = ...; */
        }
        for (i=0;i<NMOTORS;i++)
        {
                ip2->motor_specs[i].system = ip1->motor_specs[i].system;
                for (j=0;j<*(layer_descp+nlayers-1);j++)
                        ip2->motor_specs[i].mask[j] = ip1->motor_specs[i].mask[j];
                ip2->motor_specs[i].power = ip1->motor_specs[i].power;
                /* location, orientation = ...; */
        }
	/*
	 * update the world to insert the org. in its cell
	 */
	ins_org(ip2);
	/*
	 * update population size 
	 */ 
	pop_size++;

	if (verbose > 4)
	  printf("\n%d_%d generated %d_%d\n",ip1->generation,ip1->id,
                                          ip2->generation,ip2->id);

	/*
	 * copy genotype from father to child  
	 * and mutate the child's genotype
	 */
	build_geno_net(ip2);

	for (i=0,iibp1=ip1->iibiasp,iibp2=ip2->iibiasp,l=layer_descp; 
		i<nlayers; i++,iibp1++,iibp2++,l++)

	    for (ii=0,iib1=*iibp1,iib2=*iibp2; 
	    	ii< *l; ii++,iib1++,iib2++)

			*iib2 = *iib1;

	for(i=0,iiwp1=ip1->iiweightp,iiwp2=ip2->iiweightp,l=layer_descp;
		i<(nlayers-1); i++,iiwp1++,iiwp2++,l++)

	    for (ii=0,iiw1=*iiwp1,iiw2=*iiwp2; 
	    	ii< *l * *(l + 1); ii++,iiw1++,iiw2++)

			*iiw2 = *iiw1;

	mutate(ip2);

	/*
	 * develop phenotype from genotype:
	 * for now, the latter is made of the backup weights
	 * and development is simply a copy of these onto the 
	 * actual learnable weights;
	 * note that subsequent references to an individual 
	 * are to its phenotype! 
	 */
	build_pheno_net(ip2);

	for (i=0,bp2=ip2->biasp,iibp2=ip2->iibiasp,l=layer_descp; 
		i<nlayers; i++,bp2++,iibp2++,l++)

	    for (ii=0,b2=*bp2,iib2=*iibp2; 
	    	ii< *l; ii++,b2++,iib2++)

			*b2 = *iib2;

	for (i=0,wp2=ip2->weightp,iiwp2=ip2->iiweightp,l=layer_descp; 
		i<(nlayers-1); i++,wp2++,iiwp2++,l++)

	    for (ii=0,w2=*wp2,iiw2=*iiwp2; 
	    	ii< *l * *(l + 1); ii++,w2++,iiw2++)

			*w2 = *iiw2;
}



/*
 * delete all of an indiv's data structures;
 * delete him from linked list of amoeba structures;
 * return pointer to the next guy
 */
struct indiv *
delete_indiv(ip)

	struct	indiv *ip;
{
	struct indiv *next;

	if (ip->next == (struct indiv *) -1)
	{
		printf("error: deleting a non-individual\n");
		exit(-1);
	}
	/*
	 * update world to delete org. from its cell
	 */
	del_org(ip);
	next = ip->next;
	if (ip != a_head)
	{
		(ip->last)->next = ip->next;
		(ip->next)->last = ip->last;
	}
	else
	{
		a_head = ip->next;
		(ip->next)->last = (struct indiv *) 0;
	}
	destroy_network(ip);
	free((char *) ip);
	return(next);
}



/*
 * return the next indiv; if called with
 * -1 initialize
 */

struct indiv *
getnext_indiv(flag)

	int	flag;
{
	static	struct indiv *ap;
	if (flag == -1)
             {
	       ap = a_head;
	       return(a_head);
	     }
           else
             {
	       if (ap->next == (struct indiv *)-1)
                    {
		      return((struct indiv *) -1);
		    }
                  else
                    {
		      ap = ap->next;
		      return(ap);
		    }
	     }
}


/*
 * return the amoeba whose id number is passed
 */

struct indiv *
get_indiv(id)

	int	id;
{
	struct	indiv *ap;

	ap = a_head;
	while (ap->id != id)
           {
	    if (ap->next == (struct indiv *) -1)
                {
		  printf("Unable to find indiv: %d\n", id);
		  exit(-1);
		}
            else
		  ap = ap->next; 
	   }
	return(ap);
}	


/*
 * mutate the genotype of an individual
 * including sensor and motor systems
 */
mutate(ip)

    struct indiv *ip;

{

    float      **wp;
    float      **bp;
    float      *w;
    float      *b;
    int        *bldp;
    int        m;
    int        n1,n2,l;


    for(m=0;m<mutations;m++)
	{
		l=mrand(nlayers-1);
		n1 = mrand(*(layer_descp+l));
		n2 = mrand(*(layer_descp+l+1));
		wp = ip->iiweightp + l;
		bldp = layer_descp + l;
		w = *wp;

		if (verbose > 2)
			printf("\nmutated layer %d from unit %d to %d",l,n1,n2);

		*(w + n1 + (n2 * *bldp)) += rans(mutations_range);
        }

    if (mbiasestoo) 
      for(m=0;m<bmutations;m++)
	{
		l=mrand(nlayers-1);
		n1 = mrand(*(layer_descp+l+1));
		bp = ip->iibiasp + l + 1;
		b = *bp;

		if (verbose > 2)
			printf("\nmutated bias of layer %d unit %d",l+1,n1);

		*(b + n1) += rans(mutations_range);
	}

    mu_sensorymotor(ip);
}


/*
 * ATTENTION: has been changed; no reproduction here,
 *            moved into inner loop instead;
 *	here we save the results for the current generation
 */
next_generation()
{

	struct	indiv *ap;
	struct	indiv *tap;
	struct	indiv *best_ap;
	int	flag;
	float   v;
	int     f;
	int     maxbest;
	char    genfile[64];
	FILE	*fp;

	/*
	 * save energy info for each amoeba and store in .gen file;
         */
	if (verbose > 1)
	{
	 sprintf(genfile, "%ld.gen", seed);
	 if ((fp=fopen(genfile, "a")) == NULL)
	 {
		printf("error: opening file %s",genfile);
		exit(-1);
	 }
	 flag = -1;
	 while ((ap = getnext_indiv(flag))->next != (struct indiv *) -1)
	 {
		fprintf(fp,"%.2f ",ap->energy);
		flag = 0;
	 }
	 fprintf(fp,"\n");
	 fclose(fp);
	}

	/*
	 *  we save the best individuals
	 */
	if (save_best > 0)
	{
	 /*
	  * based on energy, sort to get the best amoebas 
	  * and save their indiv info in temporary amoeba struct.
	  */

	if (verbose >= 2)
		printf("Best :");

	maxbest = (save_best < pop_size)? save_best : pop_size;
	for(f=0;f<maxbest;f++)
	   {
		flag = -1;
		v = -99999.9;
		while ((ap = getnext_indiv(flag))->next != (struct indiv *) -1)
		{
			if (ap->energy > v)
			{
				v = ap->energy;
				best_ap = ap;
			}
			flag = 0;
		}

		if (verbose >= 2)
		  printf("%d (%.2f) ",best_ap->id,best_ap->energy);

                /*
                 * we delete the best individual from
		 * the a_head chain
		 */
                if (best_ap == a_head)
                {
                        a_head = best_ap->next;
			a_head->last = (struct indiv *) 0;
                }
                else
                {
                        (best_ap->last)->next = best_ap->next;
                        (best_ap->next)->last = best_ap->last;
                }

		/*
		 * we add best_ap to t_head chain
		 */
		if (f == 0)
		{
			t_head = best_ap;
			tap = t_head;
			tap->next = end_head;
		}
		else
		{
			tap->next = best_ap;
			best_ap->last = tap;
			tap = best_ap;
			tap->next = end_head;
		}
	   }   
	
	if (verbose >= 2) 
		printf("\n");


	if (verbose > 0)
		save_best_indiv(maxbest);




	/*
 	 * re-append t_head to a_head 
	 */
	if (end_head->last == (struct indiv *) 0)
	  {
		a_head = t_head;
		a_head->last = (struct indiv *) 0;
	  }
	else
	  {
		t_head->last = end_head->last;
		(t_head->last)->next = t_head;
	  }
        end_head->last = tap;
	}
}




/*
 * delete individual, both genotype and phenotype 
 */

die(ap)

    struct indiv *ap;

{
	int	i,j;

	/*
	 * update energy_reserve for checking energy conservation
	 * and drop gut content in current cell
	 */
	if (ap->energy < 0.0) energy_reserve += ap->energy;
	for (i=0;i<TYPES;i++)
		for (j=0;j<ap->gut[i];j++)
			(void)ins_food(i,ap->worldx,ap->worldy,FALSE);

	/*
	 * update population size
	 */
	pop_size--;
	if (pop_size < 1)
	{
		printf("\nEXTINCTION OCCURRED!\n");
		exit(1);
	}

	/*
	 * remove the individual from memory;
	 * keep id's consecutive by changing the
	 * largest id to the id of the dead
	 */
	(get_indiv(pop_size))->id = ap->id;
	delete_indiv(ap);
}


