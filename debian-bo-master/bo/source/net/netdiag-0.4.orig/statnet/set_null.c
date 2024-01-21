/*  set_null.c is part of Statnet */
/* Statnet is protected under the GNU Public License (GPL2). */
/* Author: Jeroen Baekelandt (jeroenb@igwe.vub.ac.be)       */


#include "stat.h"

void
set_null()
{
 extern struct registers regis;

 regis.ethercount = 0;
 regis.etherbytes = 0;
 regis.plipcount = 0;
 regis.plipbytes = 0;
 regis.slipcount = 0;
 regis.slipbytes = 0;
 regis.pppcount = 0;
 regis.pppbytes = 0;
 regis.loopcount = 0;
 regis.loopbytes = 0;
 regis.othercount = 0;
 regis.otherbytes = 0;
 regis.aarp = 0;
 regis.rtmprd = 0;
 regis.nbp = 0;
 regis.atp = 0;
 regis.aep = 0;
 regis.rtmpreq = 0;
 regis.zip = 0;
 regis.adsp = 0;
 regis.new_ethernet_count = 0;
 regis.unknown_type = 0;
 regis.unknown_frame_type = 0;
 regis.unknown_sap = 0;

 if( regis.ip_option ) memset( &ip_protocol_count, 0, sizeof(int)*SN_MAX_IP_PORT+1 );
 if( regis.prot_option ) memset( &protocol_count, 0, sizeof(int)*SN_NUM_PROTOCOLS+1 );
 if( regis.tcp_option ) memset( &tcp_port_count, 0, sizeof(int)*SN_MAX_TCP_PORTS+1 );
 if( regis.udp_option ) memset( &udp_port_count, 0, sizeof(int)*SN_MAX_UDP_PORTS+1 );
 if( regis.sap_option ) memset( &sap_count, 0, sizeof(int)*SN_MAX_SAP+1 );

}
