#include <a.out.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

#define CHKRFLAG 0x40

/* The name of the program */
char *progname;

void usage (void) __attribute__ ((noreturn));

void
usage (void)
{
  fprintf (stderr, "Usage: %s [-y -n --yes --no] files...\n", progname);
  exit (1);
}

void
main (int argc, char *argv[])
{
 int fd;
 struct exec header;
 int flag;
 int i;
 
 progname = argv[0];
 
 if (argc < 2)
   usage ();
 
 if (argv[1][0] == '-')
   {
     i = 2;	/* pos of the first file name */
     if (strcmp (argv[1], "-y") == 0 || strcmp (argv[1], "--yes") == 0)
       flag = 1;	/* Set the flag */
     else if (strcmp (argv[1], "-n") == 0 || strcmp (argv[1], "--no") == 0)
       flag = -1;	/* Reset */
     else usage ();
   }
 else
   {
     i = 1;
     flag = 0;	/* Display */
   }
 
 for (; i < argc; i++)
   {
     fd = open (argv[i], O_RDWR);
     if (fd == -1)
       {
         perror ("Can't open this file");
         exit( 2);
       }
     if (read (fd,&header, sizeof (header)) != sizeof (header))
       {
         fprintf (stderr, "Can't read the header of %s\n", argv[i]);
         exit (3);
       }
 
     if (N_BADMAG (header))
       {
         fprintf (stderr, "The file %s is not executable.\n", argv[i]);
         exit (4);
       }
 
       if (flag == 0)
         {
           printf ("%s: chkrflag is ", argv[i]);
           if (N_FLAGS(header) & CHKRFLAG)
             printf ("set\n");
           else
             printf ("not set\n");
         }
       else
         {
           if (flag == 1)
             N_SET_FLAGS(header, N_FLAGS(header) | CHKRFLAG);
           else
             N_SET_FLAGS(header, N_FLAGS(header) & ~CHKRFLAG);           
             
           if (lseek (fd,0,0) != 0L)
             {
               fprintf (stderr, "Can't lseek().\n");
               exit (5);
             }
   
           if (write (fd, &header, sizeof(header)) != sizeof(header))
             {
               fprintf (stderr, "Can't rewrite the header.\n");
               exit (6);
             }
         }
     close (fd);
   }
 exit (0);
}
