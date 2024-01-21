/*
   If you do not have isprint(c)   (sequent) 
   Add the following line to video.c
*/
#define isprint(x) (x>=' ')


/*
  If you do not have strdup   (some DECs)
  Add the following to main.c
*/
#define NULL 0
char * strdup(x) 
char *x;
{
  char *new;
  new= (char *) malloc(strlen(x));
  if (new == NULL) {
    printf("Unable to allocate string!\n");
    exit(1);
  }
  strcpy(new, x);
  return new;
}
