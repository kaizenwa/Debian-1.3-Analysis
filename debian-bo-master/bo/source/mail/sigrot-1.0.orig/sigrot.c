/*
 * sigrot.c
 * Written by Christoher Morrone <cmorrone@udel.edu>
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

/**************************************************************************
 *  The following defines set the standard locations of sigrot's dir and 
 *  files relaive to the $HOME dir.  Edit as desired.
 *
 *  DEST is the file in each user's home dir that is read by mail, news, 
 *      etc. programs that sigrot will write to. (usually .signature)
 *  SOURCE_DIR is the dir in the user's home dir which will contain
 *      sigrot's SOURCE, NEXT, PREFIX and SUFFIX files.
 *  SOURCE is the archive file in the SOURCE_DIR that will contain the list
 *      of signatures.
 *  NEXT is a file in the SOURCE_DIR which contains an integer which tells
 *      sigrot which signature should be read from the archive file next.
 *  PREFIX is a file which contains standard signature information that 
 *      should appear at the top of every .signatur.
 *  SUFFIX is a file which contains standard signature information that
 *      should appear at the bottom of every .signatur.
 **************************************************************************/

#define DEST		".signature"
#define SOURCE_DIR	".sigrot"
#define	SOURCE		"sig_archive"
#define NEXT		"next"
#define PREFIX		"prefix"
#define SUFFIX		"suffix"

/******************************* STOP *************************************
 * It should not be necesarry to edit anything below this line.           
 **************************************************************************/

int exists (char *);
void write_next(int);
int get_next();
void find_next_entry(int *,FILE *);
void write_sig(FILE *);
void copy_file(char *,FILE *);

char Dest[1024], 
     Source_Dir[1024],
     Source[1024], 
     Next[1024], 
     Prefix[1024], 
     Suffix[1024],
     buffer[1024];

int main(int argc, char *argv[])
{
  FILE *inFile,*outFile;
  int next,test=0;

  /* Set the full path names */
  sprintf(Dest, "%s/%s", getenv("HOME"), DEST);
  sprintf(Source_Dir, "%s/%s", getenv("HOME"), SOURCE_DIR);
  sprintf(Source, "%s/%s", Source_Dir, SOURCE);
  sprintf(Next, "%s/%s", Source_Dir, NEXT);
  sprintf(Prefix, "%s/%s", Source_Dir, PREFIX);
  sprintf(Suffix, "%s/%s", Source_Dir, SUFFIX);

  /* Check for sigrot's dir.  Create if it doesn't exist */
  if ((exists(Source_Dir))==0) test=mkdir(Source_Dir,0700);
  if (test!=0) 
    printf("ERROR: Couldn't create dir \"%s\".\n", Source_Dir);

  /* If there are no command line parameters, proceed with normal 
     operation. */
  if (argc==1) { 
    next=get_next(); /* number of the next archive entry to read */
    inFile=fopen(Source,"r");
    if (inFile==NULL)
      fprintf(stderr,"ERROR: Couldn't open \"%s\".\n", Source);
    else {
      find_next_entry(&next,inFile); /* Move file pointer to the 
                                        beginning of the next entry */
      write_sig(inFile);
      fclose(inFile);
      write_next(next); /* Write the number of the entry to be read on the 
			   next call to sigrot */
    }
  }
  else {
  /* The following case statements handle command line parameters. */
    switch(argc) {
      case 2: if ((!strcmp(argv[1], "-r")) || 
                  (!strcmp(argv[1], "-on"))) {
                sprintf(buffer, "%s.bak", Source);
                outFile=fopen(Source,"w");
                copy_file(buffer, outFile);
                fclose(outFile);
                printf("Old signature archive restored.\n");
              }
	      if (!strcmp(argv[1], "-off")) {
                sprintf(buffer, "%s.bak", Source);
                outFile=fopen(buffer,"w");
                copy_file(Source, outFile);
                fclose(outFile);
	        outFile=fopen(Source,"w");
	        fclose(outFile);
                write_next(0);
                printf("Signatures off.\n");
 	      }
              break;
      case 3: if (!strcmp(argv[1], "-w")) {
                sprintf(buffer, "%s.bak", Source);
                outFile=fopen(buffer,"w");
                copy_file(Source, outFile);
                fclose(outFile);
                outFile=fopen(Source,"w");
                copy_file(argv[2], outFile);
                fclose(outFile);
		printf("\"%s\" copied over signature archive.\n", argv[2]);
		printf("Type \"sigrot -r\" to restore the previous archive.\n");
              }
              if (!strcmp(argv[1], "-a")) {
                sprintf(buffer, "%s.bak", Source);
                outFile=fopen(buffer,"w");
                copy_file(Source, outFile);
                fclose(outFile);
		outFile=fopen(Source,"a");
		fprintf(outFile, "\n");
		copy_file(argv[2], outFile);
		fclose(outFile);
                printf("\"%s\" appended to signature archive.\n", argv[2]);
                printf("Type \"sigrot -r\" to restore the previous archive.\n");
	      }
	      break;
      default: printf("Too many arguments.\n");
    }
  }
  return 0;
}

/*******************************************************************
 * exists
 *
 * Return non-zero if directory exists and path components up to at
 * are accessable, 0 if not.
 *******************************************************************/
int exists (char *directory) 
{
  struct stat buf;
  return (!stat (directory, &buf) && S_ISDIR(buf.st_mode));
}

/*******************************************************************
 * get_next
 *
 * Access file named in the "#define NEXT" line.
 * Read in interger and return it.
 * If file doesn't exist, return 1.
 *******************************************************************/
int get_next()
{
  FILE *inFile;
  int next;

  inFile=fopen(Next,"r");
  if (inFile==NULL)
    next=1;
  else
    fscanf(inFile,"%d",&next);
  fclose(inFile);
  return next;
}

/*******************************************************************
 * write_next
 *
 * Increments the value of the current archive entry, and writes it
 * to the file named in the "#define NEXT" line.
 *******************************************************************/
void write_next(int next) {
  FILE *outFile;

  outFile=fopen(Next,"w");
  if (outFile==NULL)
    fprintf(stderr,"ERROR: Couldn't open \"%s\".\n", Next);
  else
    next+=1;
    fprintf(outFile,"%d\n",next);
  fclose(outFile);
}

/*******************************************************************
 * find_next_entry
 *
 * Takes as arguments an integer, and a file already opened for 
 * input.  It then searches for occurances of a double \n, or the
 * end of the file.  If it finds the occurance "next" of a double \n,
 * it leaves the file pointer there (the beginning of the next 
 * archive entry) and exits.  If it hits the EOF before the "next"
 * occurance of a double \n is found, it returns 1. (So the first 
 * entry in the archive will be read.)
 *******************************************************************/
void find_next_entry(int *next, FILE *inFile)
{
  int localnext=*next;
  char previous,current;

  if (localnext!=1) {
    previous=getc(inFile);
    while(localnext>1) {
      current=getc(inFile);
      if ((current=='\n') && (previous=='\n')) localnext-=1;
      if (current==EOF) {
        fseek(inFile,0L,0);
        localnext=1;
        *next=1;
      }
      previous=current;
    }
  }
}

/*******************************************************************
 * write_sig
 * 
 * First opens the file DEST for output.
 * Then calls copy_file to copy the contents of PREFIX into DEST, if
 * PREFIX exists.
 * Next it copies from the SOURCE file and appends to DEST until
 * it finds a double \n or the EOF in SOURCE.
 * Finally, if SUFFIX exists, it calls copy_file to copy the contents of 
 * SUFFIX and append it to DEST.
 *******************************************************************/
void write_sig(FILE *inFile)
{
  FILE *outFile;
  int test=0;
  char previous,current;

  outFile=fopen(Dest,"w");
  if (outFile==NULL)
    fprintf(stderr,"ERROR: Couldn't open \"%s\".\n", Dest);
  else {
    copy_file(Prefix,outFile);
    previous=fgetc(inFile);
    if (previous==EOF) test=1;
    while(test!=1) {
      current=fgetc(inFile);
      if ((current==EOF) || ((current=='\n') && (previous=='\n')))
        test=1;
      fputc(previous, outFile);
      previous=current;
    }
    copy_file(Suffix,outFile);
    fclose(outFile);
  }
}

/*******************************************************************
 * copy_file
 *
 * Takes as arguments a string which contains the full pathname of a 
 * file, and a file already opened for output.
 * It then opens the file, named in the string, for input.
 * Then it copies the entire contents of the input file and appends 
 * it to the output file.
 *******************************************************************/
void copy_file (char *string,FILE *outFile)
{
  FILE *inFile;
  char tmp=0;

  inFile=fopen(string,"r");
  if (inFile!=NULL) {
    while(tmp!=EOF) {
      tmp=fgetc(inFile);
      if (tmp!=EOF) fputc(tmp,outFile);
    }
    fclose(inFile);
  }
}

/*************************  Thats all folks!  *************************/
