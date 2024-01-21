/* io.c
 *                         Copyright (1992)
 *
 *         Jeff Elman.  University of California, San Diego
 *          Rik Belew.  University of California, San Diego
 *      Stefano Nolfi.  Institute of Psychology, Rome.
 *    Filippo Menczer.  University of California, San Diego
 *
 *        This software may be redistributed without charge;
 *                 this notice should be preserved.
 */


#include "defs.h"


/*
 * save world info in a gen.wld file
 */
save_world(g)

	int  g;
{
	FILE	*fp;
        char    sbuffer[64];
        char    sroot[64];
        register int    x,y;
	cell	*temp;
	int	marker = -1; /* any negative number OK */

	sprintf(sroot,"%d",g);
	genfilen(sbuffer,sroot,"wld");
	if ((fp=fopen(sbuffer, "w")) == NULL)
	       {
		printf("ERROR : I can't open %s file",sbuffer);
		exit(1);
	       }

        /*
         * save on file food element cells 
         */
        for (x=0; x<XMAX; x++)
           for (y=0; y<YMAX; y++)
	   {
		temp = world[x][y];
		while(temp != (cell *)NIL_POINTER)
		{
			if(temp->type == food)
				fprintf(fp, "%d\n", temp->datum.atom);	
			temp = temp->next;
		}
		fprintf(fp, "%d\n", marker);
	   }

	fclose(fp);
}

/*
 * load world info from a gen.wld file
 */
load_world(g)

	int  g;
{
	FILE	*fp;
        char    sbuffer[64];
        char    sroot[64];
        register int    x,y;
        int     flag;
	struct  indiv *ap;
	int	temp;

	sprintf(sroot,"%d",g-1);
	genfilen(sbuffer,sroot,"wld");
	if ((fp=fopen(sbuffer, "r")) == NULL)
	       {
		printf("ERROR : I can't open %s file",sbuffer);
		exit(1);
	       }

        /*
         * initialize world cells
         */
        for (x=0; x<XMAX; x++)
           for (y=0; y<YMAX; y++)
              world[x][y] = (cell *)NIL_POINTER;

	/*
	 * read in from file and insert food elements 
	 */
        for (x=0; x<XMAX; x++)
           for (y=0; y<YMAX; y++)
		do
		{
			fscanf(fp, "%d\n", &temp);
			if (temp >= 0) ins_food (temp,x,y,FALSE);
		}
		while(temp >= 0);

	/*
	 * insert organisms
	 */
	flag = -1;
	while ((ap = getnext_indiv(flag))->next != (struct indiv *) -1)
                {
		  ins_org(ap);
                  flag = 0;	
                }	

	fclose(fp);
}

/*
 * save all individuals in a gen.ind file
 */
save_all_indiv(g)

	int  g;
{
	FILE	*fopen();
	FILE	*fp;
        char    sbuffer[64];
        char    sroot[64];
        int     flag;
	struct  indiv *ap;
	int	i;

	sprintf(sroot,"%d",g);
	genfilen(sbuffer,sroot,"ind");
	if ((fp=fopen(sbuffer, "w")) == NULL)
	       {
		printf("ERROR : I can't open %s file",sbuffer);
		exit(1);
	       }

	fprintf(fp,"**POP    : %d\n",pop_size);
	fprintf(fp,"**SEED   : %ld\n",seed);
	fprintf(fp,"**STATE:");
        for (i = 0; i < RAND_DEG; i++)
        {
                if (!(i%4)) fprintf(fp, "\n");
                fprintf(fp, "0x%08lx\t", Rstate[i]);
        }
        fprintf(fp, "%d\n", Getptr());

	flag = -1;
	while ((ap = getnext_indiv(flag))->next != (struct indiv *) -1)
                {
		  save_indiv(ap,fp,g);
                  flag = 0;	
                }	
	fflush(fp);	
	fclose(fp);
}





/*
 * save the best x individuals in a superX.ind file
 */
save_best_indiv(x)

	int  x;
{
	FILE	*fp;
        char    sbuffer[64];
        char    sroot[64];
        int     xx;
	struct  indiv *tap;


        for(xx=0,tap=t_head;xx<x;xx++,tap=tap->next)
         {
	    sprintf(sroot,"super%d",xx+1);
	    genfilen(sbuffer,sroot,"ind");
	    if ((fp=fopen(sbuffer, "a")) == NULL)
	           {
		    printf("ERROR : I can't open %s file",sbuffer);
		    exit(1);
	           }
	    save_indiv(tap,fp,gen);
	    fflush(fp);		
	    fclose(fp);
	 }	
}






/*
 * save an individual structure in a file
 * we save backup weights
 */
save_indiv(ap,fp,g)

	struct	indiv *ap;
	FILE	*fp;
	int     g;
{
	float  **wp;
	float  **iiwp;
	float  **bp;
	float  **iibp;
	float  *w;
	float  *iiw;
	float  *b;
	float  *iib;
	int    *ldp;
	int    *bldp;
	int    *tldp;
	int    i,j,k;

	fprintf(fp,"**INDIV    : %d_%d.ind\n",g,ap->id);

	fprintf(fp,"gutsize    : %d\n",ap->gutsize);

	for (i=0;i<TYPES;i++)
		fprintf(fp,"gut[%d]: %d\n",i,ap->gut[i]);

	for (i=0;i<NSENSORS;i++)
	{
		fprintf(fp,"sensor[%d].system: %d\n",i,ap->sensor_specs[i].system);
		for (j=0;j<*layer_descp;j++)
			fprintf(fp,"sensor[%d].mask[%d]: %d\n",i,j,ap->sensor_specs[i].mask[j]);
		for (j=0;j<TYPES;j++)
			fprintf(fp,"sensor[%d].complex[%d]: %d\n",i,j,ap->sensor_specs[i].complex[j]);
		fprintf(fp,"sensor[%d].orientation: %d\n",i,ap->sensor_specs[i].orientation);
		fprintf(fp,"sensor[%d].range: %d\n",i,ap->sensor_specs[i].range);
		/* fprintf(...location...); */
	}

	for (i=0;i<NMOTORS;i++)
	{
		fprintf(fp,"motor[%d].system: %d\n",i,ap->motor_specs[i].system);
		for (j=0;j<*(layer_descp+nlayers-1);j++)
			fprintf(fp,"motor[%d].mask[%d]: %d\n",i,j,ap->motor_specs[i].mask[j]);
		fprintf(fp,"motor[%d].power: %d\n",i,ap->motor_specs[i].power);
		/* fprintf(...location,orientation...); */
	}

	fprintf(fp,"energy     : %f\n",ap->energy);

	fprintf(fp,"worldx     : %d\n",ap->worldx);
	fprintf(fp,"worldy     : %d\n",ap->worldy);

	fprintf(fp,"direction  : %d\n",ap->direction);

	fprintf(fp,"generation : %d\n",ap->generation);

	fprintf(fp,"lifecycle  : %d\n",ap->lifecycle);

	fprintf(fp,"id         : %d\n",ap->id);
	fprintf(fp,"pid        : %d\n",ap->pid);

	fprintf(fp,"error      : %f\n",ap->error);

	fprintf(fp,"fit        : %d\n",ap->fit);

	fprintf(fp, "NETWORK CONFIGURED BY RULE\n");
	fprintf(fp, "# weights after %d (err = %f) runs\n", ap->lifecycle,ap->error);
	fprintf(fp, "# WEIGHTS\n");
	for (i=1, bldp=(layer_descp),  
			  tldp=(layer_descp+1),
			  wp=ap->weightp, iiwp=ap->iiweightp; 
		i < nlayers; 
		i++, bldp++, tldp++, wp++, iiwp++)
	{
		fprintf(fp, "# from layer %d\n", i-1);
		for (j=0, w = *wp, iiw = *iiwp; j < *bldp; j++)
		{
			for (k=0; k < *tldp; k++)
			{
			     fprintf(fp, "%e (%e)\n", 
				*(w + j + (k * *bldp)),
				*(iiw + j + (k * *bldp)));
			}
		}
	}
	fprintf(fp, "# BIASES\n");
	for (i=0, ldp = layer_descp, 
			bp=ap->biasp, iibp=ap->iibiasp; 
		i<nlayers; 
		i++, ldp++, bp++, iibp++)
	      {
		fprintf(fp, "# from layer %d\n", i);
		for (j=0, b = *bp, iib = *iibp; j < *ldp; j++)
		        {
			fprintf(fp, "%e (%e)\n", *b, *iib);
			b++;
			iib++;
		        }
	      }

}






/*
 * load all individuals from a gen.ind file
 * we replace generation number of the original individual with 'gen'
 */
load_all_indiv(g)

	int  g;
{
	FILE	*fp;
        char    sbuffer[64];
        char    sroot[64];
        int     flag;
	struct  indiv *ap;
	int	i;

	sprintf(sroot,"%d",g-1);
	genfilen(sbuffer,sroot,"ind");
	if ((fp=fopen(sbuffer, "r")) == NULL)
	       {
		printf("ERROR : I can't open %s file",sbuffer);
		exit(1);
	       }

	fscanf(fp,"**POP    : %d\n",&pop_size);
	init_pop(pop_size);
	fscanf(fp,"**SEED   : %ld\n",&seed);
	fscanf(fp,"**STATE:");
        for (i = 0; i < RAND_DEG; i++)
                if (fscanf(fp, " 0x%lx ", &Rstate[i]) < 1)
                {
                        printf("Problem: garbled State\n");
                        exit(1);
                }
        fscanf(fp, " %d ", &i);
        Setptr(i);

	flag = -1;
	while ((ap = getnext_indiv(flag))->next != (struct indiv *) -1)
                {
		  load_indiv(ap,fp);
                  flag = 0;
                }
	fclose(fp);
}






/*
 * load an individual structure from a file
 * we load the starting weights and we inizialize the other weights 
 */
load_indiv(ap,fp)

	struct	indiv *ap;
	FILE	*fp;
{

	float  **wp;
	float  **iiwp;
	float  **bp;
	float  **iibp;
	float  *w;
	float  *iiw;
	float  *b;
	float  *iib;
	int    *ldp;
	int    *bldp;
	int    *tldp;
	int    i,j,k;
        int    buf1,buf2;

	fscanf(fp,"**INDIV    : %d_%d.ind\n",&buf1,&buf2);

	fscanf(fp,"gutsize    : %d\n",&ap->gutsize);

        for (i=0;i<TYPES;i++)
                fscanf(fp,"gut[%d]: %d\n",&buf1,&ap->gut[i]);

        for (i=0;i<NSENSORS;i++)
        {
                fscanf(fp,"sensor[%d].system: %d\n",&buf1,&ap->sensor_specs[i].system);
                for (j=0;j<*layer_descp;j++)
                        fscanf(fp,"sensor[%d].mask[%d]: %d\n",&buf1,&buf2,&ap->sensor_specs[i].mask[j]);
                for (j=0;j<TYPES;j++)
                        fscanf(fp,"sensor[%d].complex[%d]: %d\n",&buf1,&buf2,&ap->sensor_specs[i].complex[j]);
		fscanf(fp,"sensor[%d].orientation: %d\n",&i,&ap->sensor_specs[i].orientation);
		fscanf(fp,"sensor[%d].range: %d\n",&i,&ap->sensor_specs[i].range);
                /* fscanf(...location...); */
        }

        for (i=0;i<NMOTORS;i++)
        {
                fscanf(fp,"motor[%d].system: %d\n",&buf1,&ap->motor_specs[i].system);
		for (j=0;j<*(layer_descp+nlayers-1);j++)
                        fscanf(fp,"motor[%d].mask[%d]: %d\n",&buf1,&buf2,&ap->motor_specs[i].mask[j]);
                fscanf(fp,"motor[%d].power: %d\n",&buf1,&ap->motor_specs[i].power);
                /* fscanf(...location,orientation...); */
        }

	fscanf(fp,"energy     : %f\n",&ap->energy);			

	fscanf(fp,"worldx     : %d\n",&ap->worldx);
	fscanf(fp,"worldy     : %d\n",&ap->worldy);

	fscanf(fp,"direction  : %d\n",&ap->direction);

	fscanf(fp,"generation : %d\n",&ap->generation);

	fscanf(fp,"lifecycle  : %d\n",&ap->lifecycle);

	fscanf(fp,"id         : %d\n",&ap->id);
	fscanf(fp,"pid        : %d\n",&ap->pid);			

	fscanf(fp,"error      : %f\n",&ap->error);			

	fscanf(fp,"fit        : %d\n",&ap->fit);

	fscanf(fp, "NETWORK CONFIGURED BY RULE\n");
	fscanf(fp, "# weights after %*d (err = %*f) runs\n");
	fscanf(fp, "# WEIGHTS\n");
	for (i=1, bldp=(layer_descp), 
			  tldp=(layer_descp+1), 
			  wp=ap->weightp, iiwp=ap->iiweightp; 
		i < nlayers; 
		i++, bldp++, tldp++, wp++, iiwp++)
	{
		fscanf(fp, "# from layer %d\n",&buf1);
		for (j=0, w = *wp, iiw = *iiwp; j < *bldp; j++)
		{
			for (k=0; k < *tldp; k++)
			{
				fscanf(fp, "%e (%e)\n", 
					(w + j + (k * *bldp)),
					(iiw + j + (k * *bldp)));
			}
		}
	}
	fscanf(fp, "# BIASES\n");
	for(i=0, ldp = layer_descp, 
			 bp= ap->biasp,
			 iibp = ap->iibiasp;
		i<nlayers; 
		i++, ldp++,  bp++, iibp++)
	{
		fscanf(fp, "# from layer %d\n",&buf1);
		for (j=0, b = *bp, iib = *iibp; j < *ldp; j++)
		{
			fscanf(fp, "%e (%e)\n", b, iib);
			b++;
			iib++;
		}
	}
	
}






/*
 *  debugging function
 *  print current chain
 */


print_chain()
{
	int  flag;
	struct indiv *ap;
	
	flag = -1;
	while ((ap = getnext_indiv(flag)) != (struct indiv *) -1)
	{
		flag = 0;
		printf("\n%d (%d %d)",ap->id,(ap->last)->id, (ap->next)->id);
	}
}

