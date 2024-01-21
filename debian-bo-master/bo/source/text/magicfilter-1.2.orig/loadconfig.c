/*
 *  loadconfig.c
 *
 *  Copyright © 1995 H. Peter Anvin
 *
 *  Load the configuration file
 */

#include "magicfilter.h"

#define MAX_LINE 4096		/* Size of line buffer */

#define die(M) do { fprintf(stderr, "%s:%d: %s\n", file, lineno, (M)); \
		    fclose(f); return NULL; } while(0);

/*
 * char *getline(FILE *f, char *file, int *lineno)
 *
 * Get a logical line from file f; returning a pointer to the allocated
 * storage.  Advance the line pointer by # of lines.  file used for
 * error messages.
 */

static char *getline(FILE *f, char *file, int *line)
{
  char *buf, *p;
  int buflen, spc, ch, bslash;
  int lineno = *line;

  buf = p = malloc(BUFSIZ);
  if ( !buf ) die("Out of memory");
  buflen = spc = BUFSIZ;

  bslash = 0;

  while(1)
    {
      if ( !spc )		/* Risk for overrun? */
	{
	  spc = p-buf;
	  if ( !(buf = realloc(buf, buflen += BUFSIZ)) )
	    die("Out of memory");
	  p = buf+spc;
	  spc = BUFSIZ;
	}
      
      ch = getc(f);

      if ( ch == EOF )
	{
	  if ( p == buf )
	    {
	      free(buf);
	      return NULL;	/* EOF */
	    }
	  else
	    {
	      bslash = 0;
	      ch = '\n';	/* End of file -> end of line */
	    }
	}

      if ( ch == '\n' )
	{
	  lineno++;
	  
	  if ( bslash )
	    {
	      p--;		/* Delete backslash-newline combo */
	      spc++;
	      while ( isspace(ch = getc(f)) ); /* Ignore leading whitespace */
	      ungetc(ch, f);
	    }
	  else
	    {
	      *p = '\0';
	      *line = lineno;
	      return buf;
	    }
	}
      else
	{
	  bslash = (ch == '\\');
	  *(p++) = ch;
	  spc--;
	}
    }
}


/*
 * struct datatype *load_config(char *file)
 *
 * Load the configuration file, returning the pointer to the lead node.
 * In case of error, return NULL.
 *
 * The variable pointed to by in_block_size is set to the size needed
 * for the input buffer.
 */

struct datatype *load_config(char *file, int *in_block_size)
{
  FILE *f;
  struct datatype *head = NULL;	  /* Head pointer to linked list */
  struct datatype **next = &head; /* Chase pointer to store in order */
  struct datatype *new;
  char *line, *p;
  int lineno = 0;
  int max_line = 0;
  int bytes_needed = 0;
  int offset;
  int i;

  f = fopen(file, "r");
  if ( !f )
    {
      perror(file);
      return NULL;
    }

  while ( (line = getline(f, file, &lineno)) != NULL )
    {
      offset = getoffset(line, &p);
      
      if ( offset == MAG_ERR ) die("Syntax error");
      
      if ( offset != MAG_COMMENT )
	{
	  new = (struct datatype *) malloc(sizeof(struct datatype));
	  if ( !new ) die("Out of memory");
	  
	  if ( offset == MAG_DEFAULT )
	    {
	      new->offset = new->length = 0;
	      new->magic = new->mask = "";
	    }
	  else
	    {
	      i = getmagic(p, NULL, NULL, NULL); /* Get length of magic */
	      if ( i < 0 ) die("Syntax error");
	      new->offset = offset;
	      new->length = i;
	      new->magic = malloc(i);
	      new->mask = malloc(i);
	      if ( !(new->magic && new->mask) ) die("Out of memory");
	      if ( offset+i > bytes_needed )
		bytes_needed = offset+i;
#if DEBUG > 3
	      fprintf(stderr,"magic bytes: %d\n", i);
#endif
	      getmagic(p, &p, new->magic, new->mask);
	    }
	  
	  if ( (new->action = getaction(p, &p)) < 0 )
	    die("Invalid action");
	  
	  if ( (i = strlen(p)) > 0 )
	    {
	      if ( !(new->command = malloc(i+1)) )
		die("Out of memory");
	      strcpy(new->command, p);
	    }
	  else
	    new->command = "";
	  
	  /* Complete the linked list */
	  new->next = NULL;
	  *next = new;
	  next = &(new->next);
	  
#if DEBUG > 2
	  fprintf(stderr,"Added config entry: \n");
	  fprintf(stderr,"   offset =  %d\n", new->offset);
	  fprintf(stderr,"   length =  %d\n", new->length);
	  fprintf(stderr,"   action =  %d\n", new->action);
	  fprintf(stderr,"   command = %s\n", new->command);
	  fprintf(stderr,"   bytes =   ");
	  
	  for ( i = 0 ; i < new->length ; i++ )
	    {
	      if ( (new->mask)[i] )
		fprintf(stderr,"%03o ", (unsigned char)(new->magic)[i]);
	      else
		fprintf(stderr,"??? ");
	    }
	  fprintf(stderr,"\n");
#endif
	}
      free(line);
    }
  
  fclose(f);
  
  if ( in_block_size ) *in_block_size = bytes_needed;
  
  return head;
}
