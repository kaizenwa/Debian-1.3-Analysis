#include "misc.h"

void DeleteNode(struct PWEntryT *delme);
void PL(void);
void CheckPW(struct PWEntryT *pwptr, char *word, int flg);

struct PWEntryT *first;

void main(int argc, char **argv)
{
  FILE *wfile;
  time_t starttime;
  struct PWEntryT *pwptr;
  struct HT HTEntry;
  int i, x=0, y=0, flg=0;
  int baseptr;

  printf("\nQCrack %s", Ver);

  if(argc < 3) {
    fprintf(stderr, "Usage:\n");
    fprintf(stderr, "  QCrack [-bns] <passwdfile> <wordlist_from_qinit>\n");
    fprintf(stderr, "          -b: Beep at each crack\n");
    fprintf(stderr, "          -n: Don't test for login id as pw\n");
    fprintf(stderr, "          -s: Supress passwd parser messages\n");
    return;
  }

  if(argv[1][0] == '-') {
    flg |= 0x01;
    for(i=1; i<strlen(argv[1]); i++) {
      switch(argv[1][i]) {
        case 'n':
        case 'N':
          flg |= FLAG_TESTID;
          fprintf(stderr, "** Not login id testing\n");
          break;
        case 'b':
        case 'B':
          flg |= FLAG_BEEP;
          fprintf(stderr, "** Beeping enabled\n");
          break;
        case 's':
        case 'S': 
          flg |= FLAG_NOMSG;
          fprintf(stderr, "** Passwd parsing messages supressed\n");
          break;
      }
    }
    fprintf(stderr,"\n");
  }

  starttime = time(NULL);

  if(flg & 0x01) baseptr = 2;
  else baseptr = 1;
  first = GetPWInfo(argv[baseptr], flg);

  wfile = fopen(argv[baseptr+1], "rb");

  if (wfile == NULL) {
    fprintf(stderr,"wordlist file open error.\n");
    return;
  } 

  pwptr = first;

  if(pwptr != NULL) {
    while (pwptr -> next != NULL) {
      if (!(flg & FLAG_TESTID)) CheckPW(pwptr, pwptr -> name, flg);
      if (pwptr != NULL) pwptr = pwptr -> next;
    }
    pwptr = first;  

    while (1) {
      fread(&HTEntry, sizeof(struct HT), 1, wfile); 
      if(feof(wfile)) {
        PrintStats(starttime);
        exit(0);
      }
      NumPWs++;
      pwptr = first;
      while (pwptr -> next != NULL) {
        NumSCrypts++;
        x = GetSalt(pwptr -> salt[0]);
        y = GetSalt(pwptr -> salt[1]);
        x=(x*64)+y;
        if (x>0) {  
          if(pwptr -> hit == HTEntry.hasharray[x]) { 
            CheckPW(pwptr, HTEntry.currentword, flg);
          }
        } 
        if (pwptr != NULL) pwptr = pwptr -> next;
      }
    }
  }
}

/* We don't bother freeing the memory in here */
void DeleteNode(struct PWEntryT *delme)
{
  struct PWEntryT *t;

  if (first == delme) first = delme -> next; 
  else {
    t = first;

    while ((t != NULL) && (t -> next != delme)) t = t -> next;

    if (t -> next == delme ) t -> next = delme -> next;
  }
}

void PL(void)
{
  struct PWEntryT *t;
  t = first;

  while (t -> next != NULL) {
    printf("[%s]\n",t->name);
    fflush(stdout);
    t = t -> next;
  }
}

void CheckPW(struct PWEntryT *pwptr, char *word, int flg)
{
  char *hash;

  NumFCrypts++;
  hash = mycrypt(word, pwptr -> salt);
  hash+=2;
  if(strcmp(hash, pwptr -> passwd)==0) {
    PrintCracked(pwptr, word, flg); 
    DeleteNode(pwptr);
  }
}
