#include "misc.h"

int NumAccts = 0;	/* Total number of accts 	*/
int NumValid = 0;	/* Total number of valid accts  */
int NumSCrypts = 0; 	/* Total compares		*/
int NumFCrypts = 0;	/* Total compare hits		*/
int NumHits = 0;	/* Number of accurate guesses   */
int NumPWs = 0;		/* Number of words in data file */

char *Ver = "v1.01, (c) 1995, The Crypt Keeper: tck@zorro.ucsd.edu - 5/9/95\n";

/**************************************************************************
  void PrintCracked (struct PWEntryT **PW, struct PWEntryT *first, 
      char *word, int flags)

  Input:
    PWEntryT *PW	-> Password entry to print
    PWEntryT *first     -> Password entry list start
    *word		-> The guessed word
    flags		-> Control output
**************************************************************************/

void PrintCracked (struct PWEntryT *PW, char *word, int flags)
{
  if (flags & FLAG_BEEP) fprintf(stderr, "%c", 7);
  if (NumHits==0) printf("\n");
  NumHits++;
  printf("%s:%s:%s:%s:%s:%s:%s\n",
      PW -> name,
      word,
      (PW -> UID == NULL) ? "\0" : PW -> UID,
      (PW -> GID == NULL) ? "\0" : PW -> GID,
      (PW -> gecos == NULL) ? "\0" : PW -> gecos,
      (PW -> dir == NULL) ? "\0" : PW -> dir,
      (PW -> shell == NULL) ? "\0" : PW -> shell);
  fflush(stdout);
}

/**************************************************************************
  void PrintStats(int starttime)

  Input:
    starttime		-> Used for calculating total time.  
  
**************************************************************************/

void PrintStats(int starttime)
{
  int difftime, endtime;
  endtime = time(NULL);

  difftime = endtime - starttime;
  if(difftime == 0) difftime = 1;

  if(NumAccts == 0) 
    fprintf(stderr, "\n\nInit Complete!\n\n");
  else
    fprintf(stderr,"\n\nCracking Complete!\n\n");

  fprintf(stderr,"Total time:      %d s\n", difftime);

  if(NumAccts == 0) {
    fprintf(stderr, "Throughput:      %g fcrypts/s\n\n", 
        (double) NumFCrypts / (double) difftime);
    fprintf(stderr, "Words:           %d words\n", NumPWs);
    fprintf(stderr, "WThroughput:     %g words/s\n", 
        (double) NumPWs / (double) difftime);
  } else {
    fprintf(stderr,"No. scrypts:     %d\n", NumSCrypts);
    fprintf(stderr,"Throughput:      %d c/s\n\n", NumSCrypts/difftime);

    fprintf(stderr,"No. Hits:        %d\n", NumHits);
    fprintf(stderr,"No. Accts:       %d\n", NumAccts);
    fprintf(stderr,"No. Valid:       %d\n", NumValid);  
    if (NumAccts) fprintf(stderr,"Valid/Accts:     %g\n",
        (double) NumValid / (double) (NumAccts));
    if (NumAccts) fprintf(stderr,"Crack Rate:      %g%%\n\n", 
        (double) NumHits / (double) NumValid*100);
    fprintf(stderr,"No. fcrypts:     %d\n", NumFCrypts);
    fprintf(stderr,"No. PWs:         %d\n", NumPWs);
    if (NumPWs!=0)
        fprintf(stderr, "Wordlist rating: %g\n", 
            (double) NumHits / (double) NumPWs * 100);
  }
  fprintf(stderr,"\n\n");
  fflush(stderr);
}
/**************************************************************************
  int GetSalt(int ch)
 
  Returns: 
    0 -> 63 if valid character
    -1      if invalid character

  Input:
    ch		-> Character to look up.
**************************************************************************/ 

int GetSalt(int ch)
{
  switch(ch) {
    case '/': return 0;
    case '.': return 1;
    case '0': case '1': case '2': case '3': case '4': case '5': case '6':
    case '7': case '8': case '9': return (ch-'0'+2);
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
    case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
    case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
    case 'V': case 'W': case 'X': case 'Y': case 'Z': return (ch-'A'+2+10);
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z': return (ch-'a'+2+10+26);
  }
  return -1;
}

/**************************************************************************
  int GetPWInfo(struct PWEntryT *PW, char *passwdfile, int flags)

  Returns:
    ptr to list
 
  Input:
    char *passwdfile 	-> Filename of file to read.
    int flags		-> Reading flags 
**************************************************************************/ 

struct PWEntryT *GetPWInfo(char *passwdfile, int flags)
{
  FILE *fp;
  char s[STRLEN];
  char *t, *sptr;
  struct PWEntryT *CurLine, *f;
  int i, plen, malf;
  
  fp = fopen (passwdfile, "r");

  if (fp == NULL) {
    fprintf (stderr, "\n\nCan't open passwdfile: [%s]\n", passwdfile);
    return NULL;
  }

  f = CurLine = (struct PWEntryT *) malloc (sizeof(struct PWEntryT));
  if (CurLine == NULL) {
    fprintf(stderr, "\nMemory allocation error allocating first node.\n");
    return NULL;
  }

  do {
    fgets (s, STRLEN, fp);
    if (feof(fp)) break; 
    NumAccts++; 
    malf=0;

/* 
   The below blob could probably afford to be a function or several
   function calls. 
*/

    sptr = (char *) s; 
    i = 0;
    t = strchr(s, ':');
    if (t != NULL) {
      while ((sptr != t) && (i < 8)) {
        CurLine -> name[i] = *sptr;
        sptr++;
        i++; 
      }
      CurLine -> name[i] = '\0';
    } else malf=1;

    sptr = t+1;
    i = 0;
    t = strchr(sptr, ':');
    if (t != NULL) {
      while ((sptr != t) && (i < 13)) {
        CurLine -> password[i] = *sptr; 
        sptr++;
        i++;
      }
      CurLine -> password[i] = '\0'; 
    } else malf = 1;

    plen = i;

    sptr = t+1;
    i = 0;
    t = strchr(sptr, ':');
    if (t != NULL) {
      CurLine -> UID = (char *) malloc (t - sptr + 1); 
      while (sptr != t) {
        CurLine -> UID[i++] = *sptr;
        sptr++;
      }
      CurLine -> UID[i] = '\0';
    } else malf = 1;

    sptr = t+1;
    i = 0;
    t = strchr(sptr, ':');
    if (t != NULL) {
      CurLine -> GID = (char *) malloc (t - sptr + 1); 
      while (sptr != t) {
        CurLine -> GID[i++] = *sptr;
        sptr++;
      }
      CurLine -> GID[i] = '\0';
    } else malf = 1;

    sptr = t+1;
    i = 0;
    t = strchr(sptr, ':');
    if (t != NULL) {
      CurLine -> gecos = (char *) malloc (t - sptr + 1); 
      while (sptr != t) {
        CurLine -> gecos[i++] = *sptr;
        sptr++;
      }
      CurLine -> gecos[i] = '\0';
    } else malf = 1;

    sptr = t+1;
    i = 0;
    t = strchr(sptr, ':');
    if (t != NULL) {
      CurLine -> dir = (char *) malloc (t - sptr + 1); 
      while (sptr != t) {
        CurLine -> dir[i++] = *sptr;
        sptr++;
      }
      CurLine -> dir[i] = '\0';
    } else malf = 1;

    sptr = t+1;
    i = 0;
    t = strchr(sptr, '\n');
    if (t != NULL) {
      CurLine -> shell = (char *) malloc (t - sptr + 1); 
      while (sptr != t) {
        CurLine -> shell[i++] = *sptr;
        sptr++;
      }
      CurLine -> shell[i] = '\0';
    } else malf = 1; 

    if ((plen == 13) && (strlen(CurLine -> name) > 0) && malf != 1) { 

      NumValid++;
      CurLine -> salt[0] = CurLine -> password[0];
      CurLine -> salt[1] = CurLine -> password[1];
      CurLine -> salt[2] = '\0';
      for(i=0; i<11; i++) CurLine -> passwd[i] = CurLine -> password[i+2];
      CurLine -> passwd[11] = '\0';
   
      CurLine -> hit =
          (GetSalt(CurLine -> passwd[0]) << 2) |
          (GetSalt(CurLine -> passwd[1]) & 3);

      CurLine -> next = (struct PWEntryT *) malloc (sizeof(struct PWEntryT));
      if (CurLine -> next == NULL) { 
        fprintf(stderr,"\nMemory allocation error allocating node.\n");
        return NULL;
      }
      CurLine = CurLine -> next;
      CurLine -> next = NULL;

    } else malf = 1;
    if(!(flags & FLAG_NOMSG) && malf)  
        fprintf(stderr, "Malformed entry, line %d.\n", NumAccts);
  } while (!feof(fp));

  if(!(flags & FLAG_NOMSG)) {
    fprintf(stderr, "\n--------------------------------------------------\n");
    fprintf(stderr, "\nTotal Entries: %d\n", NumAccts);
    fprintf(stderr, "Number Valid:  %d\n", NumValid);
    fprintf(stderr, "%% Valid        %g%%\n", (double) NumValid/NumAccts*100);
  }

  return f;
}
 
