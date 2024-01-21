/* lee.c
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
 * generate a file name using a root and a suffix
 */
genfilen(filename,root,suffix)

   char  *filename;
   char  *root;
   char  *suffix;

{
  sprintf(filename,"%s.%s",root,suffix);
}




main(argc, argv)
	int     argc;
	char  **argv;
{
	int	n_weights=0;
	int	n_biases=0;
	boolean	w_mu_flag=0;
	boolean	b_mu_flag=0;
	float	p_w_mu;
	float	p_b_mu;
	int     i;
	int     c;
	char	buf[64];


	InteractiveInit(&argc, &argv);

	time(&seed);

	carica_sigmoide();

	for (i=0;i<4;i++)
		for (c=0;c<MAXRANGESIZE;c++)
			off_vec[i][c] = (offset *)malloc(sizeof(offset));	
	carica_offsets(MAXRANGE);

	/*
	 * we read the command-line options
	 */
	while ((c = getopt(argc, argv, "R:E:Nf:s:p:r:G:P:g:h:n:o:bUv:d:e:u")) != EOF) {

		switch (c) {
		case 'R':
			seed = atol(optarg);
			break;
		case 'E':
			ecount = atoi(optarg);
			errsig=TRUE;
			break;
		case 'N':
			learn=FALSE;
			break;
		case 'f':
			strcpy(fileroot, optarg);
			break;
		case 's':
			sweeps = atoi(optarg);
			break;
		case 'p':
			if ( *optarg== 'o')
			  printout=TRUE;
			break;
		case 'r':
			rate = (float) atof(optarg);
			break;
		case 'G':
			generations = atoi(optarg);
			break;
		case 'P':
			start_num_amoebas = atoi(optarg);
			break;						
		case 'g':
			startgeneration = atoi(optarg);
			break;					
		case 'h':
			mutations_range = atof(optarg);		
			break;
		case 'n':
			w_mu_flag++;
			p_w_mu = (float)atoi(optarg)/100.0;
			break;
		case 'o':
			b_mu_flag++;
			p_b_mu = (float)atoi(optarg)/100.0;
			break;
		case 'b':
			mbiasestoo=FALSE;
			break;      	
		case 'U':
			unary_reactions=TRUE;
			break;      	
		case 'v':
			verbose = atoi(optarg);		
			break;
		case 'd':
			save_best =  atoi(optarg);
			break;
		case 'e':
			save_everyn = atoi(optarg);
			break;
		case 'u':
		case '?':
		default:
			usage();
			exit(2);
			break;
		}
	}


	/*
	 * we configure the network and figure out mutations;
	 */
	config();

	for (i=1;i<nlayers;i++)
	{
		n_weights += (*(layer_descp+i-1) * *(layer_descp+i));	
		n_biases += *(layer_descp + i);
	}

	if (w_mu_flag) mutations = (int)(p_w_mu * n_weights);
	else mutations = (int)(WMUPE * n_weights);

	if (b_mu_flag) bmutations = (int)(p_b_mu * n_biases);
	else bmutations = (int)(BMUPE * n_biases);


	if  (verbose > 0) {
		/*
		 * print command information on the screen
		 */
		printf("\n\n");		
                printf("Config and table files          : %s\n", fileroot);
		printf("Stop at generation              : %d\n",generations);
		printf("Life sweeps per generation      : %d\n", sweeps);
		printf("Net's (first) 3 layers          : %d -> %d -> %d \n",
				*layer_descp, *(layer_descp + 1), *(layer_descp + 2));
		printf("Gutsize                         : %d\n",gutsize);

		if (learn)
			printf("Learning rate                   : %.2f\n",rate);

		if (startgeneration > 0)
                        printf("We start at generation          : %d\n",startgeneration);
                else
		{
                        printf("Random seed                     : %ld\n", seed);
			printf("Initial population              : %d\n",start_num_amoebas);
		}

		printf("Number of weight mutations      : %d\n",mutations);

		if (mbiasestoo)
			printf("Number of bias mutations        : %d\n",bmutations);
		printf("Range of mutations              : %.2f\n",mutations_range);
		if (save_best > 0)
			printf("Saving only the %d best each generation\n",save_best);
		if (save_everyn > 0)
			printf("Saving individuals each %d generations\n",save_everyn);
		if (errsig)
			printf("Saving global error each %d sweeps\n", ecount);	
                printf("\n\n");
	}




	/*
	 * we read in the food distributions
         * and the reaction table from the .tb/tu file
	 */
	table();

        /*
         * load or initialize state, world, 
	 * and population (order is important!)
         */
	if (startgeneration > 0)
	{
		load_all_indiv(startgeneration);
		load_world(startgeneration);
	}
	else
	{
		Srand(seed);
        	init_world();
		pop_size = start_num_amoebas;
        	init_pop(pop_size);
	}



	InteractiveFinalSetUp();

	/*
	 * main generational loop
	 */
        generati();
}





table()
{
	register int	i,x,y,z;
	char		tablefile[64];
	FILE		*tablefp;

	if (unary_reactions) genfilen(tablefile,fileroot,"tu");
        else genfilen(tablefile,fileroot,"tb");
        tablefp = fopen(tablefile, "r");
        if (tablefp == NULL)
                {
                printf("\nERROR: Can't open table (.tb/tu) file\n");
                exit(1);
                }

	/*
	 * first get the food distributions
	 */
        for (i=0; i<TYPES; i++)
        {
                fscanf(tablefp,"%d", &distrib[i][0]);  
                fscanf(tablefp,"%d", &distrib[i][1]);    
                fscanf(tablefp,"%d", &distrib[i][2]);    
                fscanf(tablefp,"%d", &distrib[i][3]);    
                fscanf(tablefp,"%d", &distrib[i][4]);    
        }        

	/*
	 * then get the actual table entries
	 */
	if (unary_reactions) 
		for (x=0; x<TYPES; x++)
                        fscanf(tablefp, "%f\n", &react_table[x][0].energy);
	else for (x=0; x<TYPES; x++)
                for (y=0; y<TYPES; y++)
                {
                        fscanf(tablefp, "%d\n", &react_table[x][y].possible);
                        fscanf(tablefp, "%f\n", &react_table[x][y].energy);
                        for (z=0; z<TYPES; z++)
                                fscanf(tablefp, "%d\n", &react_table[x][y].by_prod[z]);
                }

	fclose(tablefp);
}





config()
{
	register int i;
	int	*ldp;
	char	configfile[64];
	FILE	*cfp;
	

	genfilen(configfile,fileroot,"cf");
	cfp = fopen(configfile, "r");
	if (cfp == NULL)
           {
	    printf("ERROR : file %s not found",configfile);
	    exit(1);
	   }
	/*
	 * first line is # of layers, need this to proceed
	 */
	fscanf(cfp, "%d\n", &nlayers);
	/*
	 * next come descriptions of # of elements per layer;
	 */

	layer_descp = (int *)malloc(nlayers * sizeof(int));

	for (i=0, ldp=layer_descp; i<nlayers; i++, ldp++)
	{
		fscanf(cfp, "%d\n", ldp);
	}
	
	if ((*layer_descp>NINPUTS)||(*(layer_descp+nlayers-1)>NOUTPUTS))
	{
		printf("\nERROR: number of input or output units too large\n");
		exit(1);
	}


	/*
	 * next the the gut size
	 */
        fscanf(cfp, "%d\n", &gutsize);	
	/*
	 * next the types of sensors and motors
	 */
	for (i=0; i<NSENSORS; i++)
	{
		fscanf(cfp, "%d\n", &s_type[i]);
		fscanf(cfp, "%d\n", &s_orient[i]);
		fscanf(cfp, "%d\n", &s_range[i]);
		if (s_range[i]>MAXRANGE)
		{
			printf("ERROR: range too large!\n");
			exit(1);
		}
	}
	for (i=0; i<NMOTORS; i++)
	{
		fscanf(cfp, "%d\n", &m_type[i]);
	}
	fclose(cfp);
}






usage()
{
	printf("\nCOMMAND-LINE OPTIONS:\n\n");
	printf("-R #\tuse # to seed random generator\n");
	printf("-E #\trecord error in .err file every # sweeps\n");
	printf("-N  \tno learning\n");
	printf("-f #\tspecify <fileroot> for .cf and .tb files\n");
	printf("-s #\trun for # sweeps\n");
	printf("-po \tprint informations each cycle\n");
	printf("-r #\tuse learning rate #\n");
	printf("-G #\tnumber of generations\n");
	printf("-P #\tsize of initial population\n");
	printf("-g #\tstart generation\n");
	printf("-h #\tmutations range\n");
	printf("-n #\tpercent of weights mutated (0-100)\n");
	printf("-o #\tpercent of biases mutated  (0-100)\n");
	printf("-b  \tdo not mutate biases\n");
	printf("-U  \tunary reactions (use .tu file)\n");
	printf("-v #\tverbose mode (0-5)\n");
	printf("-d #\tsave only best # amoebas of generation\n");
	printf("-e #\tsave individuals each # generations\n");
	printf("-u  \tusage\n");
}

