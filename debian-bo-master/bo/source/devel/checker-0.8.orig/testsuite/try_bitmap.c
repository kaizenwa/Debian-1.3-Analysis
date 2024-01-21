#include <stdio.h>

char *names[] = {"-- ", "RO ", "WO ", "RW "};

void
disp_buf (char *buf, int len)
{
 int i;
 for (i = 0; i < len; i++)
   printf ("%2d ", i);
 putchar ('\n');
 for (i = 0; i < len; i++, buf++)
   fputs(names[*buf & 0x03], stdout);
 putchar ('\n');
}
 
int
convert_name_to_rights (char *buf, int *offset)
{
 int i = 0;
 char c;
 *offset = 0;
 while (*buf == ' ')
   {
     (*offset)++;
     buf++;
   }
 while (*buf != '\n' && *buf)
   {
     switch (*buf)
       {
     case 'r':
     case 'R':
       c = 0x01;	/* CHKR_RO */
       break;
     case '-':
     case '0':
       c = 0x00;	/* CHKR_UN */
       break;
     case 'W':
     case 'w':
       c = 0x02;	/* CHKR_WO */
       break;
     case 'm':
     case 'M':
       c = 0x03;	/* CHKR_RW */
       break;
     default:
       c = 0;
       }
     *buf = c;
     buf++;
     i++;
   }
 return i;
}
     
main()
{
 char buf[24];
 int len;
 int offset;
 char *p = malloc (19);
 while (1)
   {
     __chkr_read_bitmap (p, buf, 24);
     disp_buf (buf, 24);
     printf ("Action> ");
     if (fgets (buf, 24, stdin) == NULL)
       exit (0);
     len = convert_name_to_rights (buf, &offset);
     __chkr_write_bitmap (p + offset, buf + offset, len);
   }
}
