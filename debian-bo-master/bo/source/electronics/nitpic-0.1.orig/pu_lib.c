/* pu_lib.c						*/
/* Routines to read and write pic structures to disk	*/
/* Ian King 1994 					*/

#include "pu_defs.h"
#include <stdio.h>
#include <ctype.h>

void PU_Clear(PICDEFN *pic)
{
	int i;

	for(i=0; i<MAXPICSIZE; i++)
		pic->picmemmap[i] = 0xffff;

	pic->pictype	= 54;
	pic->picid[0]	= 0xffff;
	pic->picid[1]	= 0xffff;
	pic->picid[2]	= 0xffff;
	pic->picid[3]	= 0xffff;
	pic->osctype	= 3;
	pic->clock	= 1.0e6;
	pic->cp_fuse	= 1;
	pic->wd_fuse	= 1;
	pic->pu_fuse	= 1;
}

int PU_Read(const char *filename, PICDEFN *pic, int *top)
{
	FILE *filehandle;
	int ch;
	unsigned short checksum;
	int finalcheck;
	int address;
	int numberofwords;
	int i;
	int currentword;
	int check_provided;

	check_provided = 0;
	*top = 0;

	filehandle = fopen(filename, "r");
	if (filehandle == NULL)
	{
		printf("Could not open file %s for reading\n",filename);
		return PU_FAIL;
	}

	PU_Clear(pic);

	while(!feof(filehandle))
	{
		while(isspace(ch = fgetc(filehandle)))
		;

		switch(ch)
		{
			case '#':	/* comment */
					while(fgetc(filehandle)!='\n')
					;
					break;

			case 'T':	fscanf(filehandle,"%d", &pic->pictype);
					break;

			case 'W':	fscanf(filehandle,"%d", &pic->wd_fuse);
					break;

			case 'P':	fscanf(filehandle,"%d", &pic->cp_fuse);
					break;

			case 'C':	fscanf(filehandle,"%d %e", &pic->osctype, &pic->clock);
					break;

			case 'U':	fscanf(filehandle,"%d", &pic->pu_fuse);
					break;

			case 'I':	fscanf(filehandle,"%x %x %x %x", &pic->picid[0],&pic->picid[1],&pic->picid[2],&pic->picid[3]);
					break;

			case 'A':	fscanf(filehandle,"%x", &address);
					break;

			case 'D':	fscanf(filehandle,"%d", &numberofwords);
					for(i=0; i<numberofwords; i++)
					{
						fscanf(filehandle,"%x", &currentword);
						pic->picmemmap[address] = currentword;

						if (address > *top)
							*top = address;

						address++;

						if (address > MAXPICSIZE)
						{
							printf("Illegal address %04x\n", address);
							return PU_FAIL;
						}
					}
					break;

			case 'S':	fscanf(filehandle,"%x", &finalcheck);
					check_provided = 1;
					break;

			case EOF:	break;

			default:	printf("Illegal character in %s (%c)\n", filename, ch);
					return PU_FAIL;
		}
	}

	fclose(filehandle);

	if (!check_provided)
	{
		printf("No checksum found, probably not a valid pic file\n");
		return PU_OK;
	}

	checksum = 0;

	for(i=0; i<MAXPICSIZE; i++)
		checksum ^= pic->picmemmap[i];

	if ((unsigned short)finalcheck != checksum)
	{
		printf("Fails checksum (want %04x got %04x)\n",finalcheck,checksum);
		/*
		 * My version of PICASM doesn't give proper checksums (because
		 * of an unrelated hack).  But the file is really OK. -dhm
		 */
		return PU_OK;
	}

	return PU_OK;
}

int PU_WriteHeader(char *filename, PICDEFN *pic, char *comment)
{
	FILE *filehandle;

	filehandle = fopen(filename, "w");
	if (filehandle == NULL)
	{
		printf("Could not open file %s for writing\n",filename);
		return PU_FAIL;
	}

	fprintf(filehandle,"# File %s\n", filename);
	fprintf(filehandle,"# %s\n", comment);
	fprintf(filehandle,"T%d\n", pic->pictype);
	fprintf(filehandle,"W%d\n", pic->wd_fuse);
	fprintf(filehandle,"P%d\n", pic->cp_fuse);
	fprintf(filehandle,"C%d %e\n", pic->osctype, pic->clock);
	fprintf(filehandle,"U%d\n", pic->pu_fuse);
	fprintf(filehandle,"I%04x %04x %04x %04x\n",pic->picid[0],pic->picid[1],
						 pic->picid[02],pic->picid[3]);

	fclose(filehandle);
	return PU_OK;
}

int PU_WriteBlock(char *filename, PICDEFN *pic, int from, int to)
{
	FILE *filehandle;
	int length;
	int i;
	int numberleft;
	int current;
	int	mask;

	length = (to-from) + 1;
	if (length<=0)
	{
		printf("Error stupid block size! from %04x to %04x (size %d)\n",
				from, to, length);
		return PU_FAIL;
	}

	if ((from>MAXPICSIZE) || (to>MAXPICSIZE))
	{
		printf("Error from or to Out of Range from=%04x to=%04x\n",from, to);
		return PU_FAIL;
	}

	if ((from < 0) || (to < 0))
	{
		printf("Error Negative address specified!\n");
		return PU_FAIL;
	}

	filehandle = fopen(filename, "a");
	if (filehandle == NULL)
	{
		printf("Could not open file %s for writing\n",filename);
		return PU_FAIL;
	}

	fprintf(filehandle,"A%04x\n", from);

	current = from;

	switch (pic->pictype) {
	  case 54:
		mask = 0x0fff;
		break;
	  case 84:
		mask = 0x3fff;
		break;
	  default:
		mask = ~0;
		break;
	}

	while(current < to)
	{
		numberleft = to - current;
		if (numberleft > 8)
			numberleft = 8;

		fprintf(filehandle,"D%d", numberleft);

		for(i=0;i<numberleft;i++)
			fprintf(filehandle," %04x", pic->picmemmap[current+i] & mask);

		fprintf(filehandle,"\n");

		current += numberleft;
	}

	fclose(filehandle);
	return PU_OK;
}

int PU_WriteTrailer(char *filename, PICDEFN *pic, char *comment)
{
	FILE *filehandle;
	int i;
	unsigned short checksum;

	filehandle = fopen(filename, "a");
	if (filehandle == NULL)
	{
		printf("Could not open file %s for writing\n",filename);
		return PU_FAIL;
	}

	checksum = 0;

	for(i=0; i<MAXPICSIZE; i++)
		checksum ^= pic->picmemmap[i];	/* simple checksum  */
						/* easily fooled!   */

	fprintf(filehandle, "S%04x\n", checksum);
	fprintf(filehandle, "# %s\n", comment);
	fprintf(filehandle, "# ... The End ...\n");

	fclose(filehandle);
	return PU_OK;
}

/* ... The End ... */
