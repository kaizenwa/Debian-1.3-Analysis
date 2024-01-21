/* table.c
 *                         Copyright (1992)
 *
 *          Rik Belew.  University of California, San Diego
 *    Filippo Menczer.  University of California, San Diego
 *
 *        This software may be redistributed without charge;
 *                 this notice should be preserved.
 */


#include <stdio.h>
#include "defs.h"

main()
{
	char		filename[64];
	char		root[64];
	FILE		*fp;
	int		i, j, k, choice;
	reaction	table[TYPES][TYPES];
	int		distrib[TYPES][5];

        printf("\n\nThis program allows you to specify a description");
	printf("\nof the L.E.E. environment and of its ``chemistry''.");
        printf("\n\nRoot name of table file? ");
        scanf("%s", root);

        /*
         * get distributions for food sources
         */
	printf("\nThere are %d atom types, numbered from %d to %d.", TYPES, 0, (TYPES-1));
	printf("\nFor each atom type, you will be asked:");
	printf("\n\n--mu(x) = x-center of distribution (0-%d)",XMAX-1);
	printf("\n--mu(y) = y-center of distribution (0-%d)",YMAX-1);
	printf("\n--s(x) = x-inverse-sigma (1=uniform, 6=sharp gaussian)");
	printf("\n--s(y) = y-inverse-sigma (1=uniform, 6=sharp gaussian)");
	printf("\n--p = prob-per-cycle that the world is replenished with this type (0-100)\n"); 
        for (i=0; i<TYPES; i++)
        {
		printf("\nType %d: mu(x)? ",i);
		scanf("%d", &distrib[i][0]);
		printf("\nType %d: mu(y)? ",i);
		scanf("%d", &distrib[i][1]);
		printf("\nType %d: s(x)? ",i);
		scanf("%d", &distrib[i][2]);
		printf("\nType %d: s(y)? ",i);
		scanf("%d", &distrib[i][3]);
		printf("\nType %d: p? ",i);
		scanf("%d", &distrib[i][4]);
        }

	printf("\n\nThere are two possibilities for the reactions:");
	printf("\n- binary reactions: couple of atoms --> energy + byproducts");
	printf("\n- unary  reactions: single atom     --> energy");
	printf("\n\nHow many atoms should react together [1 or 2] ? ");
	scanf("%d",&choice);

switch(choice){

case 2:	/* binary reactions */
        sprintf(filename, "%s.tb", root);
        fp = fopen(filename, "w");
	if (fp == NULL)
	{
		printf("ERROR: Can't open .tb file");
		exit(1);
	}
	printf("\n\nWRITING FOOD DISTRIBUTION AND REACTION TABLE TO %s\n", filename);
        /*
         * save distributions for food sources
         */
        for (i=0; i<TYPES; i++)
		for (j=0; j<5; j++)
			fprintf(fp,"%d\n",distrib[i][j]);

	printf("\nThere are %d atom types, numbered from %d to %d", TYPES, 0, (TYPES-1));
	printf("\nFor each atom couple, you will be asked:");
	printf("\n\n-- if the reaction is possible (0=NO, 1=YES)");
	printf("\n-- the energy (>0 if released, <0 if absorbed)");
	printf("\n   (note: must enforce energy < %f)", (float)ALPHA);
	printf("\n-- the number of by-product atoms of each type");
 
	/*
	 * reactions are temporarily saved into the half-table
	 */
	for (i=0; i<TYPES; i++)
                for (j=0; j<=i; j++)
                {
			printf("\n\nReaction %d<->%d possible? ", i, j);
                        scanf("%d", &table[i][j].possible);
			if (!table[i][j].possible)
			{
				table[i][j].energy = 0.0;
				for (k=0; k<TYPES; k++)
					table[i][j].by_prod[k] = 0;
			}
			else
			{
				printf("\nEnergy? ");
                        	scanf("%f", &table[i][j].energy);
                        	for (k=0; k<TYPES; k++)
				{
					printf("\nNumber of %d by-products? ", k);
                                	scanf("%d", &table[i][j].by_prod[k]);
				}
			}
                }

	/*
	 * reactions are saved into the file 
	 * root.tb for use in main()
	 */
	printf("\n\nWriting file...");
	for (i=0; i<TYPES; i++)
                for (j=0; j<TYPES; j++)
                {
			fprintf(fp,"%d\n",((j<=i)? table[i][j].possible: table[j][i].possible));
			fprintf(fp,"%f\n",((j<=i)? table[i][j].energy: table[j][i].energy));
                       	for (k=0; k<TYPES; k++)
				fprintf(fp,"%d\n",((j<=i)? table[i][j].by_prod[k]: table[j][i].by_prod[k]));
                }
	break;

case 1:	/* unary reactions */
        sprintf(filename, "%s.tu", root);
        fp = fopen(filename, "w");
	if (fp == NULL)
	{
		printf("ERROR: Can't open .tu file");
		exit(1);
	}
	printf("\n\nWRITING FOOD DISTRIBUTION AND REACTION TABLE TO %s\n", filename);
	printf("\nREMINDER: use lee -U option for unary reactions!\n");
        /*
         * save distributions for food sources
         */
        for (i=0; i<TYPES; i++)
		for (j=0; j<5; j++)
			fprintf(fp,"%d\n",distrib[i][j]);

	printf("\nThere are %d atom types, numbered from %d to %d", TYPES, 0, (TYPES-1));
	printf("\nFor each atom type, you will be asked its energy");
	printf("\nNotes: energy > 0 if released, < 0 if absorbed");
	printf("\n       must enforce energy < %f", (float)ALPHA);
	printf("\n       all elements reactive, no by-products");
 
	/*
	 * reactions are saved into the file 
	 * root.tu for use in main()
	 */
	for (i=0; i<TYPES; i++)
        {
		printf("\nEnergy for element %d ? ", i);
		scanf("%f", &table[i][0].energy);
		fprintf(fp,"%f\n", table[i][0].energy);
        }
	break;

default:
	printf("\n\nSorry, only unary or binary reactions currently available\n");
	exit(1);
}
	fclose(fp);
	printf("\nDone.\n");
}

