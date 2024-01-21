#include <stdio.h>
#include <string.h>

main    (int argc, char **argv)
	{
	FILE* infile ;
	FILE* outfile ;
	int     count, c, d, CR, LF ;
	char    in_name[80], out_name[80] ;
	char    option[2] ;

	if(argc != 4)
		{
		printf("\nCR useage: CR option in_file_name out_file_name\n");
		printf("\nwhere 'option' is + to add CR and - to remove CR.\n");
		printf("and 'in_file_name' is the file name of the file to read.\n");
		printf("and 'out_file_name' is the file name of the modified file to create.\n");
		printf("\nthe output file will have carriage returns either added or removed") ;
		printf("\nfrom the line feeds at the end of lines.\n") ;
		exit();
		}

	/*  Input File  */

	strcpy(option,argv[1]);
	strcpy(in_name,argv[2]);
	strcpy(out_name,argv[3]);

	CR = 13 ;
	LF = 10 ;

	if((infile = fopen(in_name, "r")) == NULL)
		{
		printf("Can not open input file %s", in_name) ;
		return ;
		}
	if((outfile = fopen(out_name, "wb")) == NULL)
		{
		printf("Can not open output file %s", out_name) ;
		return ;
		}
	c = 0 ;
	d = CR ;
	while ((count = fread(&c, 1, 1, infile)) != 0)
		{
		if (c == LF)
			{
			if (strcmp(option, "+")==0)
				{
				count = fwrite(&d, 1, 1, outfile);
				}
			}
		if (c == CR)
			{
			if (strcmp(option, "-")==0)
				{
				count = fread(&c, 1, 1, infile);
				}
			}
		count = fwrite(&c, 1, 1, outfile) ;
		}
	count = fclose(infile) ; count = fclose(outfile) ;
	printf("\nThe file %s has been converted to the file %s\n", in_name, out_name) ;
	printf("\nCR is a Free product of Flexible Software.") ;
	printf("\n            Copyleft < 1994 by Flexible Software\n") ;
	printf("\nFor more information write to:\n") ;
	printf("\n        Flexible Software") ;
	printf("\n        11000 McCrackin Road") ;
	printf("\n        Tallahassee, FL  32308\n")  ;
	}
