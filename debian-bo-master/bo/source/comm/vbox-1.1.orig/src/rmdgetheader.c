/*
 * $Id: rmdgetheader.c,v 1.1 1996/07/19 09:00:06 root Exp $
 *
 * $Log: rmdgetheader.c,v $
 * Revision 1.1  1996/07/19 09:00:06  root
 * o Init.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <netinet/in.h>

#include "voice.h"

/** Prototypes ***********************************************************/

static void Usage(void);

/** Variables ************************************************************/

static char *Basename;

static struct option Arguments[] =
{
	{ "help"    , no_argument, NULL, 'h' },
	{ NULL      , 0          , NULL,  0  }
};

/*************************************************************************
 ** The magic main...																	**
 *************************************************************************/

int main(int argc, char **argv)
{
	struct VoiceHeader Header;

	int	Opts;
	int	FD;
	short	Compression;

	Basename = argv[0];

	if (argc != 2) Usage();

	while ((Opts = getopt_long(argc, argv, "h", Arguments, (int *)0)) != EOF)
	{
		switch (Opts)
		{
			default:		Usage();
							break;
		}
	}

	if ((FD = open(argv[1], O_RDONLY)) != -1)
	{
		if (read(FD, &Header, sizeof(struct VoiceHeader)) == sizeof(struct VoiceHeader))
		{
			close(FD);

			if (strncmp(Header.Magic, VOICE_MAGIC, 4) == 0)
			{
				if ((strcmp(Header.Modem, VOICE_MODEM) == 0) || (strcmp(Header.Modem, VOICE_LISDN) == 0))
				{
					Compression = ntohs(Header.Compression);
	
					if ((Compression < MIN_COMPRESSION_NUM) || (Compression > MAX_COMPRESSION_NUM))
					{
						fprintf(stderr, "%s: unknown compressen mode found.\n", Basename);
					}
					else return((int)Compression);
				}
				else fprintf(stderr, "%s: unknown modem type found.\n", Basename);
			}
			else fprintf(stderr, "%s: unknown voice header found.\n", Basename);
		}
		else
		{
			fprintf(stderr, "%s: can't read voice header.\n", Basename);

			close(FD);
		}
	}
	else fprintf(stderr, "%s: can't open '%s'.\n", Basename, argv[1]);

	return(255);
}

/*************************************************************************
 ** Usage():																				**
 *************************************************************************/

static void Usage(void)
{
	fprintf(stderr, "\n");
	fprintf(stderr, "Usage: %s FILENAME\n", Basename);
	fprintf(stderr, "\n");
	fprintf(stderr, "The program returns the following codes:\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "   2   - For compression ADPCM-2\n");
	fprintf(stderr, "   3   - For compression ADPCM-3\n");
	fprintf(stderr, "   4   - For compression ADPCM-4\n");
	fprintf(stderr, "   5   - For compression ALAW\n");
	fprintf(stderr, "   6   - For compression ULAW\n");
	fprintf(stderr, "   255 - For unknown compression type or error.\n");
	fprintf(stderr, "\n");

	exit(255);
}
