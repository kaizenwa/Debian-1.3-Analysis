#include<stdio.h>

main(argc, argv)
int argc;
char *argv[];
{
   char *last, line[100];
   int cur, limit;
   int intext, incomment;

   limit = 79;

   if (argc > 2) {
      fprintf(stderr, "Wrong number of arguments.\n\n");
      exit(1);
   }
   if (argc == 2) {
      limit = atoi(argv[1]);
      if (!limit) {
	 fprintf(stderr, "Invalid limit\n\n");
	 exit(2);
      }
   }
   last = 0;
   cur = 0;
   intext = 0;
   incomment = 0;


   do {
      line[cur] = getchar();

      /* This next line makes the program aggressively eat up white space  */
      /* found outside the text sections */

      if (!intext &&
	  (line[cur] == ' ' || line[cur] == '\n' || line[cur] == '\r'))
	 continue;

      /* These tests come first because they don't look at the current
       * character */
      /* They need to be checked before the flags (next section) are reset */

      /* line breaks at spaces within comments only */
      if (incomment && cur && line[cur - 1] == ' ')
	 last = &line[cur];

      /* line breaks at after ']' if followed by capital letter */
      if (!intext && cur > 2 && line[cur - 1] >= 'A' && line[cur - 1] <= 'Z' &&
	  line[cur - 2] == ']' && line[cur - 3] != '\\')
	 last = &line[cur - 1];

      /* These change the various flags */

      /* Check for start of comment 'C[' */
      if (cur && line[cur - 1] == 'C' && line[cur] == '[')
	 incomment = intext = 1;

      /* Check for start of non-comment bracketed section */
      if (line[cur] == '[' && (cur == 0 || line[cur - 1] != '\\'))
	 intext = 1;

      /* check for end of bracketed sections ']' */
      if (intext && line[cur] == ']' && (cur == 0 || line[cur - 1] != '\\'))
	 intext = incomment = 0;

      /* This test does use current character */

      /* line breaks at ';' outside of text */
      if (!intext && line[cur] == ';')
	 last = &line[cur];


      if (line[cur] == '\n') {
	 line[cur + 1] = 0;
	 fputs(line, stdout);
	 last = line;
	 cur = -1;
      }
      cur++;
      if (cur >= limit && last) {
	 char *loc, *end;
	 for (loc = line; loc < last; loc++)
	    putchar(*loc);
	 putchar('\n');
	 loc = line;
	 line[cur] = 0;
	 strcpy(line, last);
	 last = 0;
	 cur = strlen(line);
      }
   } while (!feof(stdin));
   line[cur] = 0;
   fputs(line, stdout);
}
