/* config.c
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
	char		root[64], filename[64]; 
	FILE		*fp;
	int		nlayers, i, j;
	int		ns = NSENSORS;
	int		nm = NMOTORS;
	float 		f;

	printf("\n\nRoot name of .cf file? ");
	scanf("%s", root);
	sprintf(filename, "%s.cf", root);
	fp = fopen(filename, "w");
	if (fp == NULL)
	{
		printf("ERROR: Can't open .cf file");
		exit(1);
	}
	printf("\n\nWRITING DATA TO %s\n", filename);
        /*
         * get and save network configuration
         */
	printf("\nNETWORK CONFIGURATION\n");
	printf("\nNumber of layers? ");
	scanf("%d", &nlayers);
	fprintf(fp, "%d\n" , nlayers);
	printf("\nNote: #inputs = [depends on sensors];");
	printf("\n      #outputs = [depends on motors] + [#inputs (for prediction)].\n");
	for(i=0;i<nlayers;i++)
	{
		printf("\nNumber of units in layer %d? ", i);
		scanf("%d", &j);
		fprintf(fp, "%d\n", j);
	}
	/*
	 * get and save gut size
	 */
	printf("\nGut initial size (int > 0)? ");
	scanf("%d", &j);
	fprintf(fp, "%d\n", j);
	/*
	 * get and save number of sensors of each type
	 */
	printf("\n\nSENSORS: there are %d of them.", ns);
	printf("\nFor each, please input the type (see file defs.h for available types).");
	printf("\n\nNote: write a number even if it doesn't make sense\n");
	for(i=0;i<ns;i++)
	{
		printf("\nType of sensor %d? ", i);
		scanf("%d", &j);
		fprintf(fp, "%d\n", j);

		printf("\nOrientation of sensor %d (-1..2)? ", i);
		scanf("%d", &j);
		fprintf(fp, "%d\n", j);

		printf("\nRange of sensor %d (int>0)? ", i);
		scanf("%d", &j);
		fprintf(fp, "%d\n", j);
	}
	/*
	 * get and save number of motors of each type
	 */
	printf("\n\nMOTORS: there are %d of them.", nm);
	printf("\nFor each, please input the type (see file defs.h for available types)\n");
	for(i=0;i<nm;i++)
	{
		printf("\nType of motor %d? ", i);
		scanf("%d", &j);
		fprintf(fp, "%d\n", j);
	}
	fclose(fp);
	printf("\n\nDone.\n");
}

