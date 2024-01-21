#include "kiss.h"

int file2file (FILE *inf, FILE *outf)
{
    register int
	nzeros = 0,
	nholes = 0,
	ch;
    
    while (1)
    {
	ch = fgetc (inf);
	if (feof (inf))
	    break;

	if (! ch)
	    nzeros++;
	else
	{
	    if (nzeros)
	    {
		fseek (outf, nzeros, SEEK_CUR);
		nzeros = 0;
		nholes += (nzeros > HOLESIZE);
	    }
	    fputc (ch, outf);
	}
    }
    
    if (nzeros)
    {
	nholes += (nzeros > HOLESIZE);
	fseek (outf, nzeros, SEEK_CUR);
	fseek (outf, -1, SEEK_CUR);
	fputc (0, outf);
    }

    return (nholes);
}
