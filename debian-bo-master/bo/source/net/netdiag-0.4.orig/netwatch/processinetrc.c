#include <stdio.h>
#include <string.h>

FILE *fp;
extern char configfile[];
extern char ethdevname[];
/* static char fname[] = "rc.inet1"; */
static char buf[256];
static char temp[200];

void
convint2char(unsigned int hold[], unsigned char cbuf[])
{
	int i;
	
	for (i=0;i<4;i++)
		cbuf[i] = (unsigned char)hold[i];

}

void 
processinetrc (unsigned char *netmask, unsigned char *local, int *ok)
{
  unsigned int hold[4];
  long *netint;
  int ethere=0;
  int netmaskglobal=0;
  int ipglobal=0;
  int i;
  char *tok;
  int ipnext, netmasknext;
  
  *ok = 0;
  fp = fopen (configfile, "r");
  if (fp == NULL)
    {
      return;
    }
  while (fgets (buf, 256, fp))
    {
      ethere = 0; 
      if (buf[0]=='#') continue;
      if (!strncmp (buf, "NETMASK=\"", 9))
	{
	  sscanf (buf, "%[^=]=\"%u.%u.%u.%u\"", temp, &hold[0],
		  &hold[1], &hold[2], &hold[3]);
	  convint2char(hold,netmask);
	  netmaskglobal = 1;
	}
      else if (!strncmp (buf, "NETMASK=", 8))
	{
	  sscanf (buf, "%[^=]=%u.%u.%u.%u", temp,  &hold[0],
		  &hold[1], &hold[2], &hold[3]);
	  convint2char(hold,netmask);
	  netmaskglobal = 1;
	}
      if (!strncmp (buf, "IPADDR=\"", 8))
	{
	  sscanf (buf, "%[^=]=\"%u.%u.%u.%u\"", temp, &hold[0],
		  &hold[1], &hold[2], &hold[3]); 
	  convint2char(hold,local);
	  ipglobal = 1;
	}
      else if (!strncmp (buf, "IPADDR=", 7))
	{
	  sscanf (buf, "%[^=]=%u.%u.%u.%u", temp,  &hold[0],
		  &hold[1], &hold[2], &hold[3]);
	  convint2char(hold,local);
           ipglobal = 1;		  
	}
      if (strstr (buf, ethdevname))
	{
	  *ok = 1;
	  ethere = 1;
	}
      if (strstr (buf, "ifconfig"))
        {
        if (ethere)
        {
        	/* PARSE the IFCONFIG line */
          ipnext = 0;
          netmasknext = 0;
          tok = strtok(buf," ");
          i = 1;
          do
          {
          	if (ipnext)
          	{
          		if (*tok != '$')  /* IPGLOBAL NOT USED */
          		{
          			if (*tok == '\"')
				{
				  sscanf (tok+1, "%u.%u.%u.%u", &hold[0],
					  &hold[1], &hold[2], &hold[3]);
				  convint2char(hold,local);
				}
          			else
          			{
				  sscanf (tok, "%u.%u.%u.%u", &hold[0],
					  &hold[1], &hold[2], &hold[3]);
				  convint2char(hold,local);
				 }
          		}	
          		ipnext = 0;
          	}
          	else if (netmasknext)
          	{
          		if (*tok != '$')  /* NETMASK GLOBAL NOT USED */
          		{
          			if (*tok == '\"')
          			{
				  sscanf (tok+1, "%u.%u.%u.%u", &hold[0],
					  &hold[1], &hold[2], &hold[3]);
				  convint2char(hold,netmask);
				}
          			else
          			{
				  sscanf (tok, "%u.%u.%u.%u", &hold[0],
					  &hold[1], &hold[2], &hold[3]);
				  convint2char(hold,netmask);
				}
          		}	
          		netmasknext = 0;
          	}
          	else if (!strcmp(tok,ethdevname))
          		ipnext = 1;
          	else if (!strcmp(tok,"netmask"))
          		netmasknext = 1;
          		
/*                printf("%d=%s$\n",i,tok);  */
          	i++;
          }
          while ((tok = strtok(NULL," "))!=NULL);
          
         }
       
       }
       
    }
  fclose (fp);
  return;
}
/*      TEST PROGRAM FOR ROUTINE  

   int main()
   {
   int ok;
   unsigned char mask[4];
   unsigned char loc[4];

   processinetrc(mask,loc,&ok);
   printf("NETMASK=%d.%d.%d.%d\n",mask[0],mask[1],mask[2],mask[3]);
   printf("IPADDR=%d.%d.%d.%d\n",loc[0],loc[1],loc[2],loc[3]);
   return;
   } 
   
   */
   
