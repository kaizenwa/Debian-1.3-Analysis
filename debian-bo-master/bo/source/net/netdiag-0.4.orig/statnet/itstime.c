/*  itstime.c is part of Statnet */
/* Statnet is protected under the GNU Public License (GPL2). */
/* Author: Jeroen Baekelandt (jeroenb@igwe.vub.ac.be)       */
/* 27DEC95: Scot E. Wilcoxon (sewilco@fieldday.mn.org)      */

#include "stat.h"
#include "curs.h"
#include <ncurses.h>
#include <signal.h>
#include <unistd.h>

#define dX      2	/* Default X location: column to start at on left */
#define dY      1	/* Default Y location: column to start at on top  */
#define padH    1	/* Default padding to expand windows */
#define padW    2	/* Default padding to expand windows */
#define AH	(10+padH)	/* atwin Height */
#define AW	(22+padW)	/* atwin Width */
#define GH	(12+padH)	/* genwin Height */
#define GW	(27+padW)	/* genwin Width */
#define IH	(2+SN_NUM_IP_TYPES+padH)	/* ipwin Height */
#define IW	(22+padW)	/* ipwin Width */
#define PH	(2+SN_NUM_PROTOCOLS+padH)	/* protwin Height */
#define PW	(22+padW)	/* protwin Width */
#define SH	(3+SN_NUM_SAP_TYPES+padH)	/* sapwin Height */
#define SW	(22+padW)	/* sapwin Width */
#define TH	(1+SN_NUM_TCP_PORTS+padH)	/* tcpwin Height */
#define TW	(22+padW)	/* tcpwin Width */
#define UH	(1+SN_NUM_UDP_PORTS+padH)	/* udpwin Height */
#define UW	(22+padW)	/* udpwin Width */

static int AY;
static int AX;
static int GY;
static int GX;
static int IY;
static int IX;
static int PY;
static int PX;
static int SY;
static int SX;
static int TY;
static int TX;
static int UY;
static int UX;

/* Change the following to #if 1 to enable window debugging code */
#if 0
#define DEBUG_WINDOW(win_name,xx,yy) \
        { \
        mvwprintw (win_name,      0,      0, "+"); \
        mvwprintw (win_name, (yy-1),      0, "+"); \
        mvwprintw (win_name, (yy-1), (xx-1), "+"); \
        mvwprintw (win_name,      0, (xx-1), "+"); \
        }
#endif

void find_window_loc( int *X, int *Y, int width, int height );
void show_prot_labels();
void show_tcp_labels();
void show_sap_labels();
void show_ip_labels();
void show_udp_labels();

static WINDOW	*atwin = NULL;
static WINDOW	*genwin = NULL;
static WINDOW	*ipwin = NULL;
static WINDOW	*protwin = NULL;
static WINDOW	*sapwin = NULL;
static WINDOW	*tcpwin = NULL;
static WINDOW	*udpwin = NULL;
static gen_all, gen_eth, gen_plip, gen_slip, gen_ppp, gen_loop, gen_802 = 0;
int	free_Xtop, free_Xbottom, free_Y;

void
itstime (int errnum)
{
  extern struct registers regis;
  int len, noframes, true_noframes;
  long int nobytes;
  int	port_int;
  int	port_now;
  int	now_value;
  int	search_int;
  int	last_value;
  static short	update_tcp_labels, update_ip_labels, update_prot_labels,
		update_udp_labels, update_sap_labels;
  char eth[20];

  /* This code calculates Kilobytes Per Second by dividing by the time */
  /* period for which the timer was set.  This would be more accurate  */
  /* on a busy system if the actual time since set_null() was used,    */
  /* but more CPU time would be used.  The way it is coded now allows  */
  /* the compiler to perform part of the calculation at compile time.  */

  nobytes = regis.etherbytes + regis.plipbytes + regis.loopbytes +
			regis.slipbytes+ regis.pppbytes + regis.otherbytes;
  noframes = true_noframes = regis.ethercount + regis.plipcount + regis.loopcount +
			regis.slipcount + regis.pppcount + regis.othercount;
  if (noframes == 0)
  {
    noframes = 1;		/* dirty, i know, but it's easier and doesn't give errors*/
  }

  if( rewrite_labels )
  {  /* if rewrite_labels */

    clrscr();
    mvprintw (0, (COLS - 22) / 2, "STATISTICS OF NETWORKS");
    if( help_flag )
    {
      mvprintw (LINES-1, 26 , "keys: Quit/General/Ip/Sap/Protocols/Tcp/Udp/Appletalk");
      help_flag = 0;
    }

  /* Close any open subwindows before assigning new window locations */
  close_all_subwin();
  AX = AY = -1;
  GX = GY = -1;
  IX = IY = -1;
  PX = PY = -1;
  SX = SY = -1;
  TX = TY = -1;
  UX = UY = -1;


  /* Now assign locations for all requested windows */
  free_Xtop = dX;
  free_Xbottom = dX;
  free_Y = 0;

  /* The sequence of this paragraph affects the location of the windows */
  if ( regis.g )		find_window_loc( &GX, &GY, GW, GH );
  if ( regis.ip_option )	find_window_loc( &IX, &IY, IW, IH );
  if (regis.sap_option)		find_window_loc( &SX, &SY, SW, SH );
  if ( regis.prot_option )	find_window_loc( &PX, &PY, PW, PH );
  if ( regis.tcp_option )	find_window_loc( &TX, &TY, TW, TH );
  if (regis.udp_option)		find_window_loc( &UX, &UY, UW, UH );
  if (regis.at_option)		find_window_loc( &AX, &AY, AW, AH );

  if (regis.g && genwin == NULL && GX >= 0)
    genwin = subwin (stdscr, GH, GW, GY, GX);

  if (regis.prot_option && protwin == NULL && PX >= 0)
    protwin = subwin (stdscr, PH, PW, PY, PX);

  if (regis.ip_option && ipwin == NULL && IX >= 0)
    ipwin = subwin (stdscr, IH, IW, IY, IX);

  if (regis.tcp_option && tcpwin == NULL && TX >= 0)
    tcpwin = subwin (stdscr, TH, TW, TY, TX);

  if (regis.udp_option && udpwin == NULL && UX >= 0)
    udpwin = subwin (stdscr, UH, UW, UY, UX);

  if (regis.at_option && atwin == NULL && AX >= 0)
    atwin = subwin (stdscr, AH, AW, AY, AX);

  if (regis.sap_option && sapwin == NULL && SX >= 0)
    sapwin = subwin (stdscr, SH, SW, SY, SX);

  }  /* if rewrite_labels */

  if (regis.g && genwin != NULL )
  {  /* if general window was created */

      if( rewrite_labels && genwin != NULL ) {
        mvwprintw (genwin, 0, 0, "GENERAL  Frame:     /%-02d sec", SN_UPDATE_SECS);
        mvwprintw (genwin, 1, 0, "         KB/s Frame/s AvLen");
      }


      mvwprintw (genwin, 0, 15, "%5d", true_noframes);

      if (true_noframes == 0)
      {
        len = 0;
      }
      else
        len = nobytes / true_noframes;
      if( true_noframes || gen_all )
      {
        mvwprintw (genwin, 2, 0, "all  %8.2f   %5d %5d", 
                 nobytes / (1024.0*SN_UPDATE_SECS),	/* KB/sec */
                 true_noframes/SN_UPDATE_SECS,		/* frames/sec */
                 len);	/* frame length */
        /* Note that the maximum is 14,200 64 bit frames per second [WRL-TR-88.4] */
        gen_all = true_noframes;
      }

      if (regis.ethercount == 0)
        len = 0;
      else
        len = regis.etherbytes / regis.ethercount;
      if( regis.ethercount || gen_eth )
      {
        mvwprintw (genwin, 3, 0, "%3.3s  %8.2f   %5d %5d", 
                 ETH,
                 regis.etherbytes / (1024.0*SN_UPDATE_SECS),	/* KB/sec */
                 regis.ethercount/SN_UPDATE_SECS,		/* frames/sec */
                 len);	/* average frame len */
        gen_eth = regis.ethercount;
      }

      if (regis.plipcount == 0)
        len = 0;
      else
        len = regis.plipbytes / regis.plipcount;
      if( regis.plipcount || gen_plip )
      {
        mvwprintw (genwin, 4, 0, "PLIP %8.2f   %5d %5d", 
                 regis.plipbytes / (1024.0*SN_UPDATE_SECS),  /* KB/sec */
                 regis.plipcount/SN_UPDATE_SECS,		/* frames/sec */
                 len);	/* average frame len */
        gen_plip = regis.plipcount;
      }


      if (regis.slipcount == 0)
        len = 0;
      else
        len = regis.slipbytes / regis.slipcount;
      if( regis.slipcount || gen_slip )
      {
        mvwprintw (genwin, 5, 0, "SLIP %8.2f   %5d %5d", 
                 regis.slipbytes / (1024.0*SN_UPDATE_SECS),  /* KB/sec */
                 regis.slipcount/SN_UPDATE_SECS,		/* frames/sec */
                 len);	/* average frame len */
        gen_slip = regis.slipcount;
      }

      if (regis.pppcount == 0)
        len = 0;
      else
        len = regis.pppbytes / regis.pppcount;
      if( regis.pppcount || gen_ppp )
      {
        mvwprintw (genwin, 6, 0, "PPP  %8.2f   %5d %5d", 
                 regis.pppbytes / (1024.0*SN_UPDATE_SECS),  /* KB/sec */
                 regis.pppcount/SN_UPDATE_SECS,		/* frames/sec */
                 len);	/* average frame len */
        gen_ppp = regis.pppcount;
      }

      if (regis.loopcount == 0)
        len = 0;
      else
        len = regis.loopbytes / regis.loopcount;
      if( regis.loopcount || gen_loop )
      {
        mvwprintw (genwin, 7, 0, "loop %8.2f   %5d %5d", 
                 regis.loopbytes / (1024.0*SN_UPDATE_SECS),  /* KB/sec */
                 regis.loopcount/SN_UPDATE_SECS,		/* frames/sec */
                 len);	/* average frame len */
        gen_loop = regis.loopcount;
      }


      if( regis.new_ethernet_count || gen_802 )
      {
        mvwprintw (genwin, GH-4, 0, "802.2 packets:  %5d ", 
               regis.new_ethernet_count/SN_UPDATE_SECS);  /* IEEE 802.2 frames per second */
        gen_802 = regis.new_ethernet_count;
      }

      mvwprintw (genwin, GH-3, 1, "Ethernet Load %6.2f%%",
		regis.etherbytes * 100.0 / (1250000.0*SN_UPDATE_SECS));	/* percent load */
		/* The 1,250,000 number is 10Mbits divided by 8 bytes per bit */

      if( (stats_countdown--) < 1 )
      {  /* if time to check statistics */
        stats_countdown = (SN_STATS_SECS/SN_UPDATE_SECS)+1;
        last_stats = &stat_buf1;
        if_getstats( ETH, now_stats );	/* get present interface statistics */
        if( last_stats->rx_errors != now_stats->rx_errors ||
            last_stats->rx_dropped != now_stats->rx_dropped )
        {  /* if errors reported */
          mvwprintw (genwin, GH-2, 0, "%4d err/Hr(%2d%%)%4d drop/Hr", 
		(now_stats->rx_errors-last_stats->rx_errors)*((60*60)/SN_STATS_SECS),
		(int)((((float)now_stats->rx_errors-last_stats->rx_errors)/
			((float)((now_stats->rx_errors-last_stats->rx_errors)
			+(now_stats->rx_packets-last_stats->rx_packets))))*100.0),
		(now_stats->rx_errors-last_stats->rx_dropped)*((60*60)/SN_STATS_SECS) );

          /* swap last and now stats buffer pointers */
          temp_stats = last_stats;
          last_stats = now_stats;
          now_stats = temp_stats;
        }  /* if errors reported */
        else
        {  /* else clear the line */
          wmove (genwin, GH-2, 0); wclrtoeol(genwin);
        }  /* else clear the line */
      }  /* if time to check statistics */

      if( regis.errcount > 0 )
      {  /* if an error was found */
        mvwprintw (genwin, GH-1, 0, "Error: %d (%d)",regis.errcount,regis.errcode);
      }  /* if an error was found */

  } /* if general window was created */

  if (regis.prot_option && protwin != NULL )
  {  /* if protocol window to be shown */
    int	temp_int;

    if( rewrite_labels || update_prot_labels ) 
    {
      show_prot_labels();
      update_prot_labels = 0;
    }

    /* Rewrite label of last item in case a new one has appeared */
    if( regis.prot_types[SN_NUM_PROTOCOLS-1] >= 0)
    {  /* if last item should be shown */
      mvwprintw (protwin,  SN_NUM_PROTOCOLS, 0, "%*.*s",
			9,
			9,
			protocol_types[regis.prot_types[SN_NUM_PROTOCOLS-1]]);
    }  /* if last item should be shown */

    last_value = 0;
    for( port_int = 0; port_int < SN_NUM_PROTOCOLS; port_int++ )
    {  /* for all protocols to display */
      if( (port_now = regis.prot_types[port_int]) >= 0 )
      {  /* if a protocol to display is defined for this field */
        now_value = protocol_count[port_int];
        mvwprintw (protwin, 1+port_int, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);

        if( now_value > last_value )
        {  /* if should move this higher on the list */
          if( port_int > 0 && ( now_value > SN_LIST_SWAP || regis.prot_types[port_int-1] < 0 ) )
          {  /* if not already at the top of the list, swap */
            regis.prot_types[port_int] = regis.prot_types[port_int-1];
            regis.prot_types[port_int-1] = port_now;
            temp_int = protocol_count[port_int-1];
            protocol_count[port_int-1] = protocol_count[port_int];
            protocol_count[port_int] = temp_int;
            update_prot_labels = 1;
          }  /* if not already at the top of the list, swap */
        }  /* if should move this higher on the list */
        last_value = now_value;
      }  /* if a protocol to display is defined for this field */
      else last_value = 0;
    }  /* for all protocols to display */

    if( regis.unknown_frame_type > 0 )
    {  /* if an unknown frame type was encountered */
      mvwprintw (protwin, PH-1,  6, "%04X", regis.unknown_frame_type );
    }  /* if an unknown frame type was encountered */
    else
      mvwprintw (protwin, PH-1,  6, "    " );
    mvwprintw (protwin, PH-1, 10, "%4d %5.1f%%", regis.unknown_type, regis.unknown_type * 100.0 / noframes);
      
  }  /* if protocol window to be shown */

  if (regis.ip_option && ipwin != NULL)
  {  /* if IP window to be shown */
    if( rewrite_labels || update_ip_labels ) 
    {
      show_ip_labels();
      update_ip_labels = 0;
    }

    last_value = 0;
    for( port_int = 0; port_int < SN_NUM_IP_TYPES; port_int++ )
    {  /* for all IP protocols to display */
      if( (port_now = regis.IP_types[port_int]) >= 0 )
      {  /* if a IP protocol to display is defined for this field */
        now_value = ip_protocol_count[port_now];
        ip_protocol_count[port_now] = 0;	/* forget about current tally so we can search later */
        mvwprintw (ipwin, 2+port_int, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);

        if( now_value > last_value )
        {  /* if should move this higher on the list */
          if( port_int > 0 && ( now_value > SN_LIST_SWAP || regis.IP_types[port_int-1] < 0 ) )
          {  /* if not already at the top of the list, swap */
            regis.IP_types[port_int] = regis.IP_types[port_int-1];
            regis.IP_types[port_int-1] = port_now;
            update_ip_labels = 1;
          }  /* if not already at the top of the list, swap */
        }  /* if should move this higher on the list */
        last_value = now_value;
      }  /* if a IP protocol to display is defined for this field */
      else last_value = 0;
    }  /* for all IP protocols to display */

    if( last_value == 0 )
    {  /* if last item on the screen is zero, find something to replace it */

      for( search_int = 0; search_int <= SN_MAX_IP_PORT; search_int++ )
      {  /* for all IP protocols which might be displayed */
        if( now_value=ip_protocol_count[search_int] > 0)
        {  /* if another protocol with activity was found */
          for( port_int = 0; port_int < SN_NUM_IP_TYPES; port_int++ )
          {  /* for all IP protocols to display */
            if(regis.IP_types[port_int] < 0)
            {  /* if an IP protocol to display is not defined for this field */
              regis.IP_types[port_int] = search_int;
              ip_protocol_count[search_int] = 0;	/* forget about current tally so we can search later */
              if( !update_ip_labels ) 
              {
                show_ip_labels();
                mvwprintw (ipwin, 2+port_int, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);
              }
              break;
            }  /* if an IP protocol to display is not defined for this field */
          }  /* for all IP protocols to display */
          if( port_int >= SN_NUM_IP_TYPES )
          {  /* if no empty IP protocol slot was found */
            regis.IP_types[SN_NUM_IP_TYPES-1] = search_int;
            ip_protocol_count[search_int] = 0;	/* forget about current tally so we can search later */
            if( !update_ip_labels ) 
            {
              mvwprintw (ipwin, 2+SN_NUM_IP_TYPES, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);
              show_ip_labels();
            }
            break;
          }  /* if no empty IP protocol slot was found */
        }  /* if another protocol with activity was found */
      }  /* for all IP protocols which might be displayed */
    }  /* if last item on the screen is zero, find something to replace it */

  }  /* if IP window to be shown */

  if (regis.tcp_option && tcpwin != NULL)
  {  /* if TCP window to be shown */
    if( rewrite_labels || update_tcp_labels ) 
    {
      show_tcp_labels();
      update_tcp_labels = 0;
    }

    last_value = 0;
    for( port_int = 0; port_int < SN_NUM_TCP_PORTS; port_int++ )
    {  /* for all TCP ports to display */
      if( (port_now = regis.tcp_ports[port_int] ) >= 0 )
      {  /* if a TCP port to display is defined for this field */
        now_value = tcp_port_count[port_now];
        tcp_port_count[port_now] = 0;	/* forget about current tally so we can search later */
        mvwprintw (tcpwin, 1+port_int, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);

        if( now_value > last_value )
        {  /* if should move this higher on the list */
          if( port_int > 0 && ( now_value > SN_LIST_SWAP || regis.tcp_ports[port_int-1] < 0 ) )
          {  /* if not already at the top of the list, swap */
            regis.tcp_ports[port_int] = regis.tcp_ports[port_int-1];
            regis.tcp_ports[port_int-1] = port_now;
            update_tcp_labels = 1;
          }  /* if not already at the top of the list, swap */
        }  /* if should move this higher on the list */
        last_value = now_value;
      }  /* if a TCP port to display is defined for this field */
      else last_value = 0;
    }  /* for all TCP ports to display */

    if( last_value == 0 )
    {  /* if last item on the screen is zero, find something to replace it */

      for( search_int = 0; search_int <= SN_MAX_TCP_PORTS; search_int++ )
      {  /* for all TCP ports which might be displayed */

        if( now_value=tcp_port_count[search_int] )
        {  /* if another port with activity was found */
          for( port_int = 0; port_int < SN_NUM_TCP_PORTS; port_int++ )
          {  /* for all TCP protocols to display */
            if(regis.tcp_ports[port_int] < 0)
            {  /* if a TCP protocol to display is not defined for this field */
              regis.tcp_ports[port_int] = search_int;
              tcp_port_count[search_int] = 0;	/* forget about current tally so we can search later */
              if( !update_tcp_labels )
              {
                show_tcp_labels();
                mvwprintw (tcpwin, 1+port_int, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);
              }
              break;
            }  /* if a TCP protocol to display is not defined for this field */
          }  /* for all TCP protocols to display */
          if( port_int >= SN_NUM_TCP_PORTS )
          {  /* if no empty TCP protocol slot was found */
            regis.tcp_ports[SN_NUM_TCP_PORTS-1] = search_int;
            tcp_port_count[search_int] = 0;	/* forget about current tally so we can search later */
            if( !update_tcp_labels )
            {
              mvwprintw (tcpwin, SN_NUM_TCP_PORTS, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);
              show_tcp_labels();
            }
            break;
          }  /* if no empty TCP protocol slot was found */
        }  /* if another port with activity was found */
      }  /* for all TCP ports which might be displayed */
    }  /* if last item on the screen is zero, find something to replace it */
  }  /* if TCP window to be shown */

  if (regis.udp_option && udpwin != NULL)
  {  /* if UDP window to be shown */
    if( rewrite_labels || update_udp_labels ) 
    {
      show_udp_labels();
      update_udp_labels = 0;
    }

    last_value = 0;
    for( port_int = 0; port_int < SN_NUM_UDP_PORTS; port_int++ )
    {  /* for all UDP ports to display */
      if( ( port_now = regis.udp_ports[port_int] ) >= 0 )
      {  /* if a UDP port to display is defined for this field */
        now_value = udp_port_count[port_now];
        udp_port_count[port_now] = 0;	/* forget about current tally so we can search later */
        mvwprintw (udpwin, 1+port_int, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);

        if( now_value > last_value )
        {  /* if should move this higher on the list */
          if( port_int > 0 && ( now_value > SN_LIST_SWAP || regis.udp_ports[port_int-1] < 0 ) )
          {  /* if not already at the top of the list, swap */
            regis.udp_ports[port_int] = regis.udp_ports[port_int-1];
            regis.udp_ports[port_int-1] = port_now;
            update_udp_labels = 1;
          }  /* if not already at the top of the list, swap */
        }  /* if should move this higher on the list */
        last_value = now_value;
      }  /* if a UDP port to display is defined for this field */
      else last_value = 0;
    }  /* for all UDP ports to display */

    if( last_value == 0 )
    {  /* if last item on the screen is zero, find something to replace it */

      for( search_int = 0; search_int <= SN_MAX_UDP_PORTS; search_int++ )
      {  /* for all UDP ports which might be displayed */
        if( now_value=udp_port_count[search_int] )
        {  /* if another port with activity was found */
          for( port_int = 0; port_int < SN_NUM_UDP_PORTS; port_int++ )
          {  /* for all UDP protocols to display */
            if(regis.udp_ports[port_int] < 0)
            {  /* if a UDP protocol to display is not defined for this field */
              regis.udp_ports[port_int] = search_int;
              udp_port_count[search_int] = 0;	/* forget about current tally so we can search later */
              if( !update_udp_labels )
              {
                mvwprintw (udpwin, 1+port_int, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);
                show_udp_labels();
              }
              break;
            }  /* if a UDP protocol to display is not defined for this field */
          }  /* for all UDP protocols to display */
          if( port_int >= SN_NUM_UDP_PORTS )
          {  /* if no empty UDP protocol slot was found */
            regis.udp_ports[SN_NUM_UDP_PORTS-1] = search_int;
            udp_port_count[search_int] = 0;	/* forget about current tally so we can search later */
            if( !update_udp_labels )
            {
              mvwprintw (udpwin, SN_NUM_UDP_PORTS, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);
              show_udp_labels();
            }
            break;
          }  /* if no empty UDP protocol slot was found */
        }
      }  /* for all UDP ports which might be displayed */
    }  /* if last item on the screen is zero, find something to replace it */

  }  /* if UDP window to be shown */

  if (regis.at_option && atwin != NULL)
  {  /* if AppleTalk window to be shown */

    if( rewrite_labels ) {
        mvwprintw (atwin,  0, 0, "==== APPLETALK ====");
        mvwprintw (atwin,  2, 0, "AARP:");
        mvwprintw (atwin,  3, 0, "RTMPRD:");
        mvwprintw (atwin,  4, 0, "RTMPREQ:");
        mvwprintw (atwin,  5, 0, "NBP:");
        mvwprintw (atwin,  6, 0, "ATP:");
        mvwprintw (atwin,  7, 0, "AEP:");
        mvwprintw (atwin,  8, 0, "ZIP:");
        mvwprintw (atwin,  9, 0, "ADSP:");
    } /* if rewrite_labels */

    mvwprintw (atwin,  2, 9, "%5d %5.1f%%", regis.aarp, regis.aarp * 100.0 / noframes);
    mvwprintw (atwin,  3, 9, "%5d %5.1f%%", regis.rtmprd, regis.rtmprd * 100.0 / noframes);
    mvwprintw (atwin,  4, 9, "%5d %5.1f%%", regis.rtmpreq, regis.rtmpreq * 100.0 / noframes);
    mvwprintw (atwin,  5, 9, "%5d %5.1f%%", regis.nbp, regis.nbp * 100.0 / noframes);
    mvwprintw (atwin,  6, 9, "%5d %5.1f%%", regis.atp, regis.atp * 100.0 / noframes);
    mvwprintw (atwin,  7, 9, "%5d %5.1f%%", regis.aep, regis.aep * 100.0 / noframes);
    mvwprintw (atwin,  8, 9, "%5d %5.1f%%", regis.zip, regis.zip * 100.0 / noframes);
    mvwprintw (atwin,  9, 9, "%5d %5.1f%%", regis.adsp, regis.adsp * 100.0 / noframes);
      
  }  /* if AppleTalk window to be shown */

  if (regis.sap_option && sapwin != NULL)
  {  /* if SAP window to be shown */
    if( rewrite_labels || update_sap_labels ) 
    {
      show_sap_labels();
      update_sap_labels = 0;
    }
      
    last_value = 0;
    for( port_int = 0; port_int < SN_NUM_SAP_TYPES; port_int++ )
    {  /* for all SAP ports to display */
      if( ( port_now = regis.SAP_types[port_int] ) >= 0 )
      {  /* if a SAP port to display is defined for this field */
        now_value = sap_count[port_now];
        sap_count[port_now] = 0;	/* forget about current tally so we can search later */
        mvwprintw (sapwin, 2+port_int, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);

        mvwprintw (sapwin,  2+port_int, 0, "%*s",
			9,
			sap_port_types[port_now]);

        if( now_value > last_value )
        {  /* if should move this higher on the list */
          if( port_int > 0 && ( now_value > SN_LIST_SWAP || regis.SAP_types[port_int-1] < 0 ) )
          {  /* if not already at the top of the list, swap */
            regis.SAP_types[port_int] = regis.SAP_types[port_int-1];
            regis.SAP_types[port_int-1] = port_now;
            update_sap_labels = 1;
          }  /* if not already at the top of the list, swap */
        }  /* if should move this higher on the list */
        last_value = now_value;
      }  /* if a SAP port to display is defined for this field */
      else last_value = 0;
    }  /* for all SAP ports to display */

    if( last_value == 0 )
    {  /* if last item on the screen is zero, find something to replace it */

      for( search_int = 0; search_int <= SN_MAX_SAP; search_int++ )
      {  /* for all SAP ports which might be displayed */
        if( now_value=sap_count[search_int] )
        {  /* if another port with activity was found */
          for( port_int = 0; port_int < SN_NUM_SAP_TYPES; port_int++ )
          {  /* for all SAP protocols to display */
            if(regis.SAP_types[port_int] < 0)
            {  /* if a SAP protocol to display is not defined for this field */
              regis.SAP_types[port_int] = search_int;
              sap_count[search_int] = 0;	/* forget about current tally so we can search later */
              if( !update_sap_labels )
              {
                mvwprintw (sapwin, 2+port_int, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);
                show_sap_labels();
              }
              break;
            }  /* if a SAP protocol to display is not defined for this field */
          }  /* for all SAP protocols to display */
          if( port_int >= SN_NUM_SAP_TYPES )
          {  /* if no empty SAP protocol slot was found */
            regis.SAP_types[SN_NUM_SAP_TYPES-1] = search_int;
            sap_count[search_int] = 0;	/* forget about current tally so we can search later */
            if( !update_sap_labels )
            {
              mvwprintw (sapwin, 1+SN_NUM_SAP_TYPES, 9, "%5d %5.1f%%", now_value, now_value * 100.0 / noframes);
              show_sap_labels();

        mvwprintw (sapwin,  1+SN_NUM_SAP_TYPES, 0, "%*s",
			9,
			sap_port_types[search_int]);

            }
            break;
          }  /* if no empty SAP protocol slot was found */
        }
      }  /* for all SAP ports which might be displayed */
    }  /* if last item on the screen is zero, find something to replace it */

  }  /* if SAP window to be shown */

  rewrite_labels = 0;  /* labels have been written */

  set_null ();

  if( redraw_screen )
  {
    wredrawln(stdscr, 0, LINES);	/* redraw the entire screen */
    redraw_screen = 0;
  }

  touchwin (stdscr);	/* make screen ready to be refreshed */
  refresh ();		/* update the screen with the new info */

#if( defined(DEBUG_WINDOW) )
  if (atwin != NULL) DEBUG_WINDOW(atwin,AW,AH);
  if (genwin != NULL) DEBUG_WINDOW(genwin,GW,GH);
  if (ipwin != NULL) DEBUG_WINDOW(ipwin,IW,IH);
  if (protwin != NULL) DEBUG_WINDOW(protwin,PW,PH);
  if (sapwin != NULL) DEBUG_WINDOW(sapwin,SW,SH);
  if (tcpwin != NULL) DEBUG_WINDOW(tcpwin,TW,TH);
  if (udpwin != NULL) DEBUG_WINDOW(udpwin,UW,UH);
#endif

  if (signal (SIGALRM, itstime) == SIG_ERR)
    {
      perror ("Signal error: ");
      exit (5);
    }
  alarm (SN_UPDATE_SECS);
  return;
}

void show_tcp_labels()
{
  if( regis.tcp_option && tcpwin != NULL ) {
    int temp_int;

    mvwprintw (tcpwin, 0, 0, "==== TCP/IP PORTS ====");

    for( temp_int = 0; temp_int < SN_NUM_TCP_PORTS; temp_int++ ) {
      if( regis.tcp_ports[temp_int] >= 0)
      {  /* if something to show */
        mvwprintw (tcpwin,  1+temp_int, 0, "%*.*s",
			9,
			9,
			tcp_port_types[regis.tcp_ports[temp_int]]);
      }  /* if something to show */
      else
      {  /* else fill the line with spaces */
        mvwprintw (tcpwin,  1+temp_int, 0, "%*s", TW-1, " " );
      }  /* else fill the line with spaces */
    }
  }
}

void show_ip_labels()
{
  if( regis.ip_option && ipwin != NULL )
  {
    int temp_int;

    mvwprintw (ipwin, 0, 0, "==== IP PROTOCOLS ====");

    for( temp_int = 0; temp_int < SN_NUM_IP_TYPES; temp_int++ )
    {
      if( regis.IP_types[temp_int] >= 0)
        {  /* if something to show */
          mvwprintw (ipwin,  2+temp_int, 0, "%*s",
			8,
			ip_protocol_types[regis.IP_types[temp_int]]);
        }  /* if something to show */
        else
        {  /* else fill the line with spaces */
          mvwprintw (ipwin,  2+temp_int, 0, "%*s", TW-1, " " );
        }  /* else fill the line with spaces */
    }
  }

}

void show_udp_labels()
{
  if( regis.udp_option && udpwin != NULL ) {
    int temp_int;

    mvwprintw (udpwin, 0, 0, "==== UDP/IP PORTS ====");

    for( temp_int = 0; temp_int < SN_NUM_UDP_PORTS; temp_int++ )
    {
      if( regis.udp_ports[temp_int] >= 0)
      {  /* if something to show */
        mvwprintw (udpwin,  1+temp_int, 0, "%*.*s",
			9,
			9,
			udp_port_types[regis.udp_ports[temp_int]]);
      }  /* if something to show */
      else
      {  /* else fill the line with spaces */
        mvwprintw (udpwin,  1+temp_int, 0, "%*s", TW-1, " " );
      }  /* else fill the line with spaces */
    }
  }
}

void show_sap_labels()
{
  if( regis.sap_option && sapwin != NULL ) {
    int temp_int;

    mvwprintw (sapwin,  0, 0, "===== 802.2 SAP =====");
    /* mvwprintw (sapwin,  SH-1, 0, "(NetBIOS SAP:NetBEUI)"); */

    for( temp_int = 0; temp_int < SN_NUM_SAP_TYPES; temp_int++ )
    {
      if( regis.SAP_types[temp_int] >= 0)
      {  /* if something to show */
        mvwprintw (sapwin,  2+temp_int, 0, "%*.*s",
			9,
			9,
			sap_port_types[regis.SAP_types[temp_int]]);
      }  /* if something to show */
      else
      {  /* else fill the line with spaces */
        mvwprintw (sapwin,  2+temp_int, 0, "%*s", SW-1, " " );
      }  /* else fill the line with spaces */
    }
  }
}

void show_prot_labels()
{
  if( regis.prot_option && protwin != NULL) {
    int temp_int;

    mvwprintw (protwin,  0, 0, "===== PROTOCOLS =====");

    for( temp_int = 0; temp_int < SN_NUM_PROTOCOLS; temp_int++ )
    {
      if( regis.prot_types[temp_int] >= 0)
      {  /* if something to show */
        mvwprintw (protwin,  1+temp_int, 0, "%*.*s",
			9,
			9,
			protocol_types[regis.prot_types[temp_int]]);
      }  /* if something to show */
      else
      {  /* else fill the line with spaces */
        mvwprintw (protwin,  1+temp_int, 0, "%*s", PW-1, " " );
      }  /* else fill the line with spaces */
    }
    mvwprintw (protwin,  PH-1, 0, "Other:");
  }
}

void find_window_loc( int *X, int *Y, int width, int height )
{  /* Find location for a subwindow */
  if( free_Y == 0 )
  {  /* if starting at top, put this in upper left corner */
    if( width <= COLS-free_Xtop )
    {  /* if wide enough on the top */
      *X = free_Xtop;
      *Y = dY;
      free_Xtop += width;
      free_Y = height+dY;
    }  /* if wide enough on the top */
  }  /* if starting at top, put this in upper left corner */
  else
    if( width <= (COLS-free_Xbottom) )
    {  /* if wide enough on the bottom */
      if( LINES >= (free_Y + height) )
      {  /* if enough height on the bottom */
        *X = free_Xbottom;
        *Y = free_Y;
        free_Y += height;
      }  /* if enough height on the bottom */
      else
      {  /* else not enough height on the bottom */
        if( free_Xbottom > free_Xtop ) free_Xtop = free_Xbottom;
        free_Xbottom = free_Xtop;	/* line up the subwindows */
        free_Y = 0;  /* Ready to start from the top again */
        if( width <= COLS-free_Xtop )
        {  /* if should be enough room at the top, put it there else quit */
          find_window_loc( X, Y, width, height );
        }  /* if should be enough room at the top, put it there else quit */
      }  /* else not enough height on the bottom */
    }  /* if wide enough on the bottom */

  return;
}  /* Find location for a subwindow */

void close_all_subwin()
{
  if (atwin != NULL) 
  {
    delwin(atwin);
    atwin = NULL;
  }
  if (genwin != NULL) 
  {
    delwin(genwin);
    genwin = NULL;
  }
  if (ipwin != NULL) 
  {
    delwin(ipwin);
    ipwin = NULL;
  }
  if (protwin != NULL) 
  {
    delwin(protwin);
    protwin = NULL;
  }
  if (sapwin != NULL) 
  {
    delwin(sapwin);
    sapwin = NULL;
  }
  if (tcpwin != NULL) 
  {
    delwin(tcpwin);
    tcpwin = NULL;
  }
  if (udpwin != NULL) 
  {
    delwin(udpwin);
    udpwin = NULL;
  }
}
