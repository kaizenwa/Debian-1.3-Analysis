#include <stdio.h>
#include <string.h>

#define MEMSET memset

/* lines per page */
#define MAX_LINES 120

#define MAX_LINE_LEN 1024

int EOF_Seen;
int Tabsize = 8;

#define BOLD_ATTR 0x1
#define ULINE_ATTR 0x2

/* This is specific to HP deskjet in Landscape mode */
int Device_Lines = 45;
int Device_Cols = 160;

/* Hp deskjet */
char **Dev_Init_String;

char *DeskJet_Init_String [] = 
{
   "\033&k2G",   		       /* \n -> \r\n */
   "\033&s1C",   		       /* wrap OFF */
   "\033&l1O",			       /* landscape */
   "\033&l0L",			       /* perf skip mode OFF */
   "\0339",			       /* no margins */
   "\033(s0P",			       /* fixed spacing */
   "\033(s16.67H",		       /* pitch */
   "\033(s12V",			       /* point size */
   "\033(s0S",			       /* upright style */
   "\033(s0B",			       /* normal stroke weight */
   "\033(s3T",			       /* courier typeface */
   NULL
};


typedef struct 
{
   unsigned char data[MAX_LINE_LEN];   /* buffer */
   unsigned char attr[MAX_LINE_LEN];   /* attributes of data */
   int len;			       /* current length */
}
Line_Type;

Line_Type The_Lines[MAX_LINES];

char *Program_Name;

static void exit_error (char *s)
{
   fprintf(stderr, "%s: %s\n", Program_Name, s);
   exit (-1);
}

static void usage (void)
{
   fprintf (stderr, "%s: usage (2 forms):\n\n\t%s -deskjet\n\t%s RxC\n\n", Program_Name, Program_Name, Program_Name);
   fprintf (stderr, "For example, use\n\n\t%s 60x80\n\nto print on paper with 60 rows and 80 columns.\n", 
	    Program_Name);
   exit (-1);
}


static int read_line (FILE *fp, Line_Type *line)
{
   unsigned char *d, *a;
   int ch;
   int len, dlen;
   
   line->len = 0;
   
   if (EOF_Seen) return -1;
   
   d = line->data;
   a = line->attr;
   
   MEMSET (d, 0, MAX_LINE_LEN);
   MEMSET (a, 0, MAX_LINE_LEN);
   
   while (((ch = getc(fp)) != EOF) && (ch != '\n'))
     {
	if (ch < ' ')
	  {
	     if (ch == '\t')
	       {
		  len = (int) (d - line->data);
		  dlen = Tabsize * (len / Tabsize) + Tabsize - len;
		  if (len + dlen > MAX_LINE_LEN)
		    {
		       exit_error ("Line too long for tab expansion.");
		    }
		  
		  while (dlen--)
		    {
		       *d++ = ' ';
		       a++;	       /* attributes not modified */
		    }
	       }
	     else if ((ch == '\b') && (d != line->data))
	       {
		  /* backspace */
		  if (d > line->data + line->len)
		    {
		       line->len = (int) (d - line->data);
		    }
		  d--; a--;
	       }
	     else if (ch == '\r')
	       {
		  if (d > line->data + line->len)
		    {
		       line->len = (int) (d - line->data);
		    }
		  d = line->data; 
		  a = line->data;
	       }
	     /* anything else gets stripped */
	  }
	else
	  {
	     if (*d)
	       {
		  if (*d == ch) *a |= BOLD_ATTR;
		  else if (*d == '_')
		    {
		       *a |= ULINE_ATTR;
		       *d = ch;
		    }
		  else if (ch == '_')
		    {
		       *a |= ULINE_ATTR;
		    }
	       }
	     else *d = ch;
	     
	     d++; a++;
	  }
     }
   
   if (d > line->data + line->len)
     {
	line->len = (int) (d - line->data);
     }
   
   if (ch == EOF) EOF_Seen = 1;
   return 0;
}

static int get_virtual_page (FILE *fp)
{
   int i;
   for (i = 0; i < Device_Lines; i++)
     {
	if (-1 == read_line (fp, The_Lines + i)) break;
     }
   return i;
}

static void newline (void)
{
   putc('\n', stdout);
}

static void newpage (void)
{
   putc ('\f', stdout);
}

static void page_setup (void)
{
}

static void init_printer (void)
{
   int i = 0;
   if (Dev_Init_String == NULL) return;
   while (Dev_Init_String[i] != NULL) fputs (Dev_Init_String[i++], stdout);
}

static void print_virtual_page (int len)
{
   int i;
   int jmax, jmin = 0;
   int again = 1;
   unsigned char *d, *dmax;
   
   while (again)
     {
	page_setup ();
	again = 0;
   
	for (i = 0; i < len; i++)
	  {
	     jmax = jmin + Device_Cols;
	     if (jmax > The_Lines[i].len) jmax = The_Lines[i].len;
	     else again = 1;
	     
	     d = The_Lines[i].data + jmin;
	     dmax = The_Lines[i].data + jmax;
	     while (d < dmax) 
	       {
		  putc (*d, stdout);
		  d++;
	       }
	     newline ();
	  }
	newpage ();
	jmin += Device_Cols;
     }
}

int main (int argc, char **argv)
{
   FILE *fp;
   int page_len;
   
   Program_Name = argv[0];
   if (argc <= 1) usage ();
   
   if (!strcmp (argv[1], "-deskjet")) 
     {
	Dev_Init_String = &DeskJet_Init_String[0];
     }
   else 
     {
	if (2 != sscanf (argv[1], "%dx%d", &Device_Lines, &Device_Cols))
	  {
	     usage ();
	  }
     }

   fp = stdin;
   
   init_printer ();
   while (EOF_Seen == 0) 
     {
	page_len = get_virtual_page(fp);
	print_virtual_page (page_len);
     }
   return 0;
}

   
   
