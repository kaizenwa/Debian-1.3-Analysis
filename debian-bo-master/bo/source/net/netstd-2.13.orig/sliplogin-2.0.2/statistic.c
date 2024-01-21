#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <string.h>
#include <linux/if_ether.h>


/* Based on if_getstats from ifconfig.c */

int get_rx_packets(char *ifname, struct enet_statistics *stats)
{
  FILE *fp;
  int len;
  char buf[256],*bp;

  fp=fopen("/proc/net/dev","r");
  if (fp==NULL) return(-1);
  len=strlen(ifname);

  while(fgets(buf,255,fp))
  {
  	bp=buf;
  	while(*bp&&isspace(*bp)) bp++;
  	if(strncmp(bp,ifname,len)==0 && bp[len]==':')
  	{
 		bp=strchr(bp,':');
 		bp++;
        sscanf(bp,"%d %d %d %d %d %d %d %d %d %d %d",
            &stats->rx_packets,
            &stats->rx_errors,
            &stats->rx_dropped,
            &stats->rx_fifo_errors,
            &stats->rx_frame_errors,

            &stats->tx_packets,
            &stats->tx_errors,
            &stats->tx_dropped,
            &stats->tx_fifo_errors,
            &stats->collisions,

            &stats->tx_carrier_errors
        );
 		fclose(fp);
 		return(stats->rx_packets);
  	}
  }
  fclose(fp);
  return(-2);
}
