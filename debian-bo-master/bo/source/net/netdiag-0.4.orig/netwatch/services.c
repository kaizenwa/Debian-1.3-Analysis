/*  services.c is part of Statnet */
/* Statnet is protected under the GNU Public License (GPL2). */
/* Author: 15DEC95: Scot E. Wilcoxon (sewilco@fieldday.mn.org)     */

/* This routine initializes the names for port numbers. */
/* These are from RFC 1700, "Assigned Numbers", which are from hither and yon. */

#include "netwatch.h"

void 
services ()
{
  int temp_int;

#if( defined(SN_MAX_IP_PORT) )
  for (temp_int = 0; temp_int <= SN_MAX_IP_PORT; temp_int++)
    {				/* Put port number as default label for all ports */
      sprintf (ip_protocol_types[temp_int], "%d:", temp_int);
    }				/* Put port number as default label for all ports */

  strncpy (ip_protocol_types[1], "ICMP:", SN_PORT_TYPE_LEN);	/* Internet Control Message       [RFC792,JBP] */
  strncpy (ip_protocol_types[2], "IGMP:", SN_PORT_TYPE_LEN);	/* Internet Group Management     [RFC1112,JBP] */
  strncpy (ip_protocol_types[3], "GGP:", SN_PORT_TYPE_LEN);	/* Gateway-to-Gateway              [RFC823,MB] */
  strncpy (ip_protocol_types[4], "IP:", SN_PORT_TYPE_LEN);	/* IP in IP (encasulation)               [JBP] */
  strncpy (ip_protocol_types[5], "ST:", SN_PORT_TYPE_LEN);	/* Stream                 [RFC1190,IEN119,JWF] */
  strncpy (ip_protocol_types[6], "TCP:", SN_PORT_TYPE_LEN);	/* Transmission Control           [RFC793,JBP] */
  strncpy (ip_protocol_types[7], "UCL:", SN_PORT_TYPE_LEN);	/* UCL                                    [PK] */
  strncpy (ip_protocol_types[8], "EGP:", SN_PORT_TYPE_LEN);	/* Exterior Gateway Protocol     [RFC888,DLM1] */
  strncpy (ip_protocol_types[9], "IGP:", SN_PORT_TYPE_LEN);	/* any private interior gateway          [JBP] */
  strncpy (ip_protocol_types[10], "BBN-RCC-MON:", SN_PORT_TYPE_LEN);	/* BBN RCC Monitoring                    [SGC] */
  strncpy (ip_protocol_types[11], "NVP-II:", SN_PORT_TYPE_LEN);		/* Network Voice Protocol         [RFC741,SC3] */
  strncpy (ip_protocol_types[12], "PUP:", SN_PORT_TYPE_LEN);	/* PUP                             [PUP,XEROX] */
  strncpy (ip_protocol_types[13], "ARGUS:", SN_PORT_TYPE_LEN);	/* ARGUS                                [RWS4] */
  strncpy (ip_protocol_types[14], "EMCON:", SN_PORT_TYPE_LEN);	/* EMCON                                 [BN7] */
  strncpy (ip_protocol_types[15], "XNET:", SN_PORT_TYPE_LEN);	/* Cross Net Debugger            [IEN158,JFH2] */
  strncpy (ip_protocol_types[16], "CHAOS:", SN_PORT_TYPE_LEN);	/* Chaos                                 [NC3] */
  strncpy (ip_protocol_types[17], "UDP:", SN_PORT_TYPE_LEN);	/* User Datagram                  [RFC768,JBP] */
  strncpy (ip_protocol_types[18], "MUX:", SN_PORT_TYPE_LEN);	/* Multiplexing                    [IEN90,JBP] */
  strncpy (ip_protocol_types[19], "DCN-MEAS:", SN_PORT_TYPE_LEN);	/* DCN Measurement Subsystems           [DLM1] */
  strncpy (ip_protocol_types[20], "HMP:", SN_PORT_TYPE_LEN);	/* Host Monitoring                [RFC869,RH6] */
  strncpy (ip_protocol_types[21], "PRM:", SN_PORT_TYPE_LEN);	/* Packet Radio Measurement              [ZSU] */
  strncpy (ip_protocol_types[22], "XNS-IDP:", SN_PORT_TYPE_LEN);	/* XEROX NS IDP               [ETHERNET,XEROX] */
  strncpy (ip_protocol_types[23], "TRUNK-1:", SN_PORT_TYPE_LEN);	/* Trunk-1                              [BWB6] */
  strncpy (ip_protocol_types[24], "TRUNK-2:", SN_PORT_TYPE_LEN);	/* Trunk-2                              [BWB6] */
  strncpy (ip_protocol_types[25], "LEAF-1:", SN_PORT_TYPE_LEN);		/* Leaf-1                               [BWB6] */
  strncpy (ip_protocol_types[26], "LEAF-2:", SN_PORT_TYPE_LEN);		/* Leaf-2                               [BWB6] */
  strncpy (ip_protocol_types[27], "RDP:", SN_PORT_TYPE_LEN);	/* Reliable Data Protocol         [RFC908,RH6] */
  strncpy (ip_protocol_types[28], "IRTP:", SN_PORT_TYPE_LEN);	/* Internet Reliable Transaction  [RFC938,TXM] */
  strncpy (ip_protocol_types[29], "ISO-TP4:", SN_PORT_TYPE_LEN);	/* ISO Transport Protocol Class 4 [RFC905,RC77] */
  strncpy (ip_protocol_types[30], "NETBLT:", SN_PORT_TYPE_LEN);		/* Bulk Data Transfer Protocol    [RFC969,DDC1] */
  strncpy (ip_protocol_types[31], "MFE-NSP:", SN_PORT_TYPE_LEN);	/* MFE Network Services Protocol  [MFENET,BCH2] */
  strncpy (ip_protocol_types[32], "MERIT-INP:", SN_PORT_TYPE_LEN);	/* MERIT Internodal Protocol             [HWB] */
  strncpy (ip_protocol_types[33], "SEP:", SN_PORT_TYPE_LEN);	/* Sequential Exchange Protocol        [JC120] */
  strncpy (ip_protocol_types[34], "PC:", SN_PORT_TYPE_LEN);	/* Third Party Connect Protocol         [SAF3] */
  strncpy (ip_protocol_types[35], "IDPR:", SN_PORT_TYPE_LEN);	/* Inter-Domain Policy Routing Protocol [MXS1] */
  strncpy (ip_protocol_types[36], "XTP:", SN_PORT_TYPE_LEN);	/* XTP                                   [GXC] */
  strncpy (ip_protocol_types[37], "DDP:", SN_PORT_TYPE_LEN);	/* Datagram Delivery Protocol            [WXC] */
  strncpy (ip_protocol_types[38], "IDPR-CMTP:", SN_PORT_TYPE_LEN);	/* IDPR Control Message Transport Proto [MXS1] */
  strncpy (ip_protocol_types[39], "TP++:", SN_PORT_TYPE_LEN);	/* TP++ Transport Protocol               [DXF] */
  strncpy (ip_protocol_types[40], "IL:", SN_PORT_TYPE_LEN);	/* IL Transport Protocol                [DXP2] */
  strncpy (ip_protocol_types[41], "SIP:", SN_PORT_TYPE_LEN);	/* Simple Internet Protocol              [SXD] */
  strncpy (ip_protocol_types[42], "SDRP:", SN_PORT_TYPE_LEN);	/* Source Demand Routing Protocol       [DXE1] */
  strncpy (ip_protocol_types[43], "SIP-SR:", SN_PORT_TYPE_LEN);		/* SIP Source Route                      [SXD] */
  strncpy (ip_protocol_types[44], "SIP-FRAG:", SN_PORT_TYPE_LEN);	/* SIP Fragment                          [SXD] */
  strncpy (ip_protocol_types[45], "IDRP:", SN_PORT_TYPE_LEN);	/* Inter-Domain Routing Protocol   [Sue Hares] */
  strncpy (ip_protocol_types[46], "RSVP:", SN_PORT_TYPE_LEN);	/* Reservation Protocol           [Bob Braden] */
  strncpy (ip_protocol_types[47], "GRE:", SN_PORT_TYPE_LEN);	/* General Routing Encapsulation     [Tony Li] */
  strncpy (ip_protocol_types[48], "MHRP:", SN_PORT_TYPE_LEN);	/* Mobile Host Routing Protocol[David Johnson] */
  strncpy (ip_protocol_types[49], "BNA:", SN_PORT_TYPE_LEN);	/* BNA                          [Gary Salamon] */
  strncpy (ip_protocol_types[50], "SIPP-ESP:", SN_PORT_TYPE_LEN);	/* SIPP Encap Security Payload [Steve Deering] */
  strncpy (ip_protocol_types[51], "SIPP-AH:", SN_PORT_TYPE_LEN);	/* SIPP Authentication Header  [Steve Deering] */
  strncpy (ip_protocol_types[52], "I-NLSP:", SN_PORT_TYPE_LEN);		/* Integrated Net Layer Security  TUBA [GLENN] */
  strncpy (ip_protocol_types[53], "SWIPE:", SN_PORT_TYPE_LEN);	/* IP with Encryption                    [JI6] */
  strncpy (ip_protocol_types[54], "NHRP:", SN_PORT_TYPE_LEN);	/* NBMA Next Hop Resolution Protocol */
  strncpy (ip_protocol_types[61], "anyhost:", SN_PORT_TYPE_LEN);	/* any host internal protocol            [JBP] */
  strncpy (ip_protocol_types[62], "CFTP:", SN_PORT_TYPE_LEN);	/* CFTP                            [CFTP,HCF2] */
  strncpy (ip_protocol_types[63], "anylan:", SN_PORT_TYPE_LEN);		/* any local network                     [JBP] */
  strncpy (ip_protocol_types[64], "SAT-EXPAK:", SN_PORT_TYPE_LEN);	/* SATNET and Backroom EXPAK             [SHB] */
  strncpy (ip_protocol_types[65], "KRYPTOLAN:", SN_PORT_TYPE_LEN);	/* Kryptolan                            [PXL1] */
  strncpy (ip_protocol_types[66], "RVD:", SN_PORT_TYPE_LEN);	/* MIT Remote Virtual Disk Protocol      [MBG] */
  strncpy (ip_protocol_types[67], "IPPC:", SN_PORT_TYPE_LEN);	/* Internet Pluribus Packet Core         [SHB] */
  strncpy (ip_protocol_types[68], "dfs:", SN_PORT_TYPE_LEN);	/* any distributed file system           [JBP] */
  strncpy (ip_protocol_types[69], "SAT-MON:", SN_PORT_TYPE_LEN);	/* SATNET Monitoring                     [SHB] */
  strncpy (ip_protocol_types[70], "VISA:", SN_PORT_TYPE_LEN);	/* VISA Protocol                        [GXT1] */
  strncpy (ip_protocol_types[71], "IPCV:", SN_PORT_TYPE_LEN);	/* Internet Packet Core Utility          [SHB] */
  strncpy (ip_protocol_types[72], "CPNX:", SN_PORT_TYPE_LEN);	/* Computer Protocol Network Executive  [DXM2] */
  strncpy (ip_protocol_types[73], "CPHB:", SN_PORT_TYPE_LEN);	/* Computer Protocol Heart Beat         [DXM2] */
  strncpy (ip_protocol_types[74], "WSN:", SN_PORT_TYPE_LEN);	/* Wang Span Network                     [VXD] */
  strncpy (ip_protocol_types[75], "PVP:", SN_PORT_TYPE_LEN);	/* Packet Video Protocol                 [SC3] */
  strncpy (ip_protocol_types[76], "BR-SAT-MON:", SN_PORT_TYPE_LEN);	/* Backroom SATNET Monitoring            [SHB] */
  strncpy (ip_protocol_types[77], "SUN-ND:", SN_PORT_TYPE_LEN);		/* SUN ND PROTOCOL-Temporary             [WM3] */
  strncpy (ip_protocol_types[78], "WB-MON:", SN_PORT_TYPE_LEN);		/* WIDEBAND Monitoring                   [SHB] */
  strncpy (ip_protocol_types[79], "WB-EXPAK:", SN_PORT_TYPE_LEN);	/* WIDEBAND EXPAK                        [SHB] */
  strncpy (ip_protocol_types[80], "ISO-IP:", SN_PORT_TYPE_LEN);		/* ISO Internet Protocol                 [MTR] */
  strncpy (ip_protocol_types[81], "VMTP:", SN_PORT_TYPE_LEN);	/* VMTP                                 [DRC3] */
  strncpy (ip_protocol_types[82], "SECURE-VMTP:", SN_PORT_TYPE_LEN);	/* SECURE-VMTP                          [DRC3] */
  strncpy (ip_protocol_types[83], "VINES:", SN_PORT_TYPE_LEN);	/* VINES                                 [BXH] */
  strncpy (ip_protocol_types[84], "TTP:", SN_PORT_TYPE_LEN);	/* TTP                                   [JXS] */
  strncpy (ip_protocol_types[85], "NSFNET-IGP:", SN_PORT_TYPE_LEN);	/* NSFNET-IGP                            [HWB] */
  strncpy (ip_protocol_types[86], "DGP:", SN_PORT_TYPE_LEN);	/* Dissimilar Gateway Protocol     [DGP,ML109] */
  strncpy (ip_protocol_types[87], "TCF:", SN_PORT_TYPE_LEN);	/* TCF                                  [GAL5] */
  strncpy (ip_protocol_types[88], "IGRP:", SN_PORT_TYPE_LEN);	/* IGRP                            [CISCO,GXS] */
  strncpy (ip_protocol_types[89], "OSPFIGP:", SN_PORT_TYPE_LEN);	/* OSPFIGP                      [RFC1583,JTM4] */
  strncpy (ip_protocol_types[90], "Sprite-RPC:", SN_PORT_TYPE_LEN);	/* Sprite RPC Protocol            [SPRITE,BXW] */
  strncpy (ip_protocol_types[91], "LARP:", SN_PORT_TYPE_LEN);	/* Locus Address Resolution Protocol     [BXH] */
  strncpy (ip_protocol_types[92], "MTP:", SN_PORT_TYPE_LEN);	/* Multicast Transport Protocol          [SXA] */
  strncpy (ip_protocol_types[93], "AX.25:", SN_PORT_TYPE_LEN);	/* AX.25 Frames                         [BK29] */
  strncpy (ip_protocol_types[94], "IPIP:", SN_PORT_TYPE_LEN);	/* IP-within-IP Encapsulation Protocol   [JI6] */
  strncpy (ip_protocol_types[95], "MICP:", SN_PORT_TYPE_LEN);	/* Mobile Internetworking Control Pro.   [JI6] */
  strncpy (ip_protocol_types[96], "SCC-SP:", SN_PORT_TYPE_LEN);		/* Semaphore Communications Sec. Pro.    [HXH] */
  strncpy (ip_protocol_types[97], "ETHERIP:", SN_PORT_TYPE_LEN);	/* Ethernet-within-IP Encapsulation     [RXH1] */
  strncpy (ip_protocol_types[98], "ENCAP:", SN_PORT_TYPE_LEN);	/* Encapsulation Header         [RFC1241,RXB3] */
  strncpy (ip_protocol_types[99], "encrypt:", SN_PORT_TYPE_LEN);	/* any private encryption scheme         [JBP] */
  strncpy (ip_protocol_types[100], "GMTP:", SN_PORT_TYPE_LEN);	/* GMTP                                 [RXB5] */

#endif

#if( defined(SN_MAX_TCP_PORTS) )
  for (temp_int = 0; temp_int <= SN_MAX_TCP_PORTS; temp_int++)
    {				/* Put port number as default label for all ports */
      sprintf (tcp_port_types[temp_int], "%d:", temp_int);
    }				/* Put port number as default label for all ports */

#if( SN_MAX_TCP_PORTS > 6063 )
  strncpy (tcp_port_types[6000 - 6063], "x11:", SN_PORT_TYPE_LEN);
  for (temp_int = 6000; temp_int < 6064; temp_int++)
    {				/* Label the X11 Windows port range */
      sprintf (tcp_port_types[temp_int], "X11");
#if( defined(SN_MAX_UDP_PORTS) )
#if( SN_MAX_UDP_PORTS > 6063 )
      sprintf (udp_port_types[temp_int], "X11");
#endif
#endif
    }				/* Label the X11 Windows port range */
#endif

  strncpy (tcp_port_types[0], "fragment ", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1], "tcpmux:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2], "compressnet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[3], "compressnet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5], "rje:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7], "echo:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[9], "discard:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[11], "systat:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[13], "daytime:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[17], "qotd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[18], "msp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[19], "chargen:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[20], "ftp-data:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[21], "ftp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[23], "telnet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[25], "smtp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[27], "nsw-fe:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[29], "msg-icp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[31], "msg-auth:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[33], "dsp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[37], "time:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[38], "rap:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[39], "rlp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[41], "graphics:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[42], "nameserver:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[43], "nicname:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[44], "mpm-flags:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[45], "mpm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[46], "mpm-snd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[47], "ni-ftp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[48], "auditd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[49], "login:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[50], "re-mail-ck:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[51], "la-maint:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[52], "xns-time:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[53], "domain:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[54], "xns-ch:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[55], "isi-gl:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[56], "xns-auth:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[58], "xns-mail:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[61], "ni-mail:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[62], "acas:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[64], "covia:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[65], "tacacs-ds:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[66], "sql*net:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[67], "bootps:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[68], "bootpc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[69], "tftp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[70], "gopher:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[71], "netrjs-1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[72], "netrjs-2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[73], "netrjs-3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[74], "netrjs-4:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[76], "deos:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[78], "vettcp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[79], "finger:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[80], "www-http:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[81], "hosts2-ns:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[82], "xfer:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[83], "mit-ml-dev:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[84], "ctf:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[85], "mit-ml-dev:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[86], "mfcobol:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[88], "kerberos:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[89], "su-mit-tg:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[90], "dnsix:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[91], "mit-dov:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[92], "npp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[93], "dcp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[94], "objcall:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[95], "supdup:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[96], "dixie:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[97], "swift-rvf:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[98], "tacnews:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[99], "metagram:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[100], "newacct:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[101], "hostname:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[102], "iso-tsap:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[103], "gppitnp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[104], "acr-nema:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[105], "csnet-ns:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[106], "3com-tsmux:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[107], "rtelnet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[108], "snagas:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[109], "pop2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[110], "pop3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[111], "sunrpc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[112], "mcidas:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[113], "auth:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[114], "audionews:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[115], "sftp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[116], "ansanotify:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[117], "uucp-path:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[118], "sqlserv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[119], "nntp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[120], "cfdptkt:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[121], "erpc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[122], "smakynet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[123], "ntp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[124], "ansatrader:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[125], "locus-map:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[126], "unitary:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[127], "locus-con:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[128], "gss-xlicen:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[129], "pwdgen:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[130], "cisco-fna:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[131], "cisco-tna:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[132], "cisco-sys:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[133], "statsrv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[134], "ingres-net:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[135], "loc-srv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[136], "profile:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[137], "netbios-ns:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[138], "netbios-dgm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[139], "netbios-ssn:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[140], "emfis-data:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[141], "emfis-cntl:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[142], "bl-idm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[143], "imap2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[144], "news:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[145], "uaac:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[146], "iso-tp0:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[147], "iso-ip:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[148], "cronus:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[149], "aed-512:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[150], "sql-net:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[151], "hems:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[152], "bftp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[153], "sgmp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[154], "netsc-prod:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[155], "netsc-dev:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[156], "sqlsrv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[157], "knet-cmp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[158], "pcmail-srv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[159], "nss-routing:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[160], "sgmp-traps:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[161], "snmp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[162], "snmptrap:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[163], "cmip-man:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[164], "cmip-agent:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[165], "xns-courier:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[166], "s-net:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[167], "namp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[168], "rsvd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[169], "send:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[170], "print-srv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[171], "multiplex:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[172], "cl/1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[173], "xyplex-mux:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[174], "mailq:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[175], "vmnet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[176], "genrad-mux:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[177], "xdmcp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[178], "nextstep:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[179], "bgp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[180], "ris:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[181], "unify:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[182], "audit:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[183], "ocbinder:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[184], "ocserver:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[185], "remote-kis:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[186], "kis:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[187], "aci:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[188], "mumps:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[189], "qft:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[190], "gacp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[191], "prospero:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[192], "osu-nms:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[193], "srmp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[194], "irc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[195], "dn6-nlm-aud:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[196], "dn6-smm-red:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[197], "dls:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[198], "dls-mon:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[199], "smux:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[200], "src:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[201], "at-rtmp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[202], "at-nbp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[203], "at-3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[204], "at-echo:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[205], "at-5:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[206], "at-zis:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[207], "at-7:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[208], "at-8:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[209], "tam:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[210], "z39.50:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[211], "914c/g:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[212], "anet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[213], "ipx:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[214], "vmpwscs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[215], "softpc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[216], "atls:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[217], "dbase:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[218], "mpp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[219], "uarps:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[220], "imap3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[221], "fln-spx:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[222], "rsh-spx:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[223], "cdc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[243], "sur-meas:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[245], "link:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[246], "dsp3270:", SN_PORT_TYPE_LEN);
#if( SN_MAX_TCP_PORTS > 256 )
  strncpy (tcp_port_types[344], "pdap:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[345], "pawserv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[346], "zserv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[347], "fatserv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[348], "csi-sgwp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[371], "clearcase:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[372], "ulistserv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[373], "legent-1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[374], "legent-2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[375], "hassle:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[376], "nip:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[377], "tnETOS:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[378], "dsETOS:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[379], "is99c:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[380], "is99s:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[381], "hp-collector:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[382], "hp-managed-node:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[383], "hp-alarm-mgr:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[384], "arns:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[385], "ibm-app:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[386], "asa:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[387], "aurp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[388], "unidata-ldm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[389], "ldap:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[390], "uis:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[391], "synotics-relay:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[392], "synotics-broker:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[393], "dis:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[394], "embl-ndt:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[395], "netcp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[396], "netware-ip:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[397], "mptn:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[398], "kryptolan:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[400], "work-sol:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[401], "ups:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[402], "genie:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[403], "decap:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[404], "nced:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[405], "ncld:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[406], "imsp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[407], "timbuktu:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[408], "prm-sm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[409], "prm-nm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[410], "decladebug:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[411], "rmt:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[412], "synoptics-trap:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[413], "smsp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[414], "infoseek:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[415], "bnet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[416], "silverplatter:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[417], "onmux:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[418], "hyper-g:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[419], "ariel1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[420], "smpte:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[421], "ariel2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[422], "ariel3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[423], "opc-job-start:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[424], "opc-job-track:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[425], "icad-el:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[426], "smartsdp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[427], "svrloc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[428], "ocs_cmu:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[429], "ocs_amu:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[430], "utmpsd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[431], "utmpcd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[432], "iasd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[433], "nnsp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[434], "mobileip-agent:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[435], "mobilip-mn:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[436], "dna-cml:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[437], "comscm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[438], "dsfgw:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[439], "dasp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[440], "sgcp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[441], "decvms-sysmgt:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[442], "cvc_hostd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[443], "https:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[444], "snpp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[445], "microsoft-ds:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[446], "ddm-rdb:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[447], "ddm-dfm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[448], "ddm-byte:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[449], "as-servermap:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[450], "tserver:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[512], "exec:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[513], "login:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[514], "cmd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[515], "printer:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[517], "talk:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[518], "ntalk:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[519], "utime:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[520], "efs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[525], "timed:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[526], "tempo:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[530], "courier:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[531], "conference:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[532], "netnews:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[533], "netwall:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[539], "apertus-ldp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[540], "uucp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[541], "uucp-rlogin:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[543], "klogin:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[544], "kshell:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[550], "new-rwho:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[555], "dsf:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[556], "remotefs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[560], "rmonitor:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[561], "monitor:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[562], "chshell:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[564], "9pfs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[565], "whoami:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[570], "meter:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[571], "meter:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[600], "ipcserver:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[606], "urm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[607], "nqs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[608], "sift-uft:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[609], "npmp-trap:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[610], "npmp-local:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[611], "npmp-gui:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[634], "ginad:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[666], "doom:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[666], "mdqs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[704], "elcsd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[709], "entrustmanager:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[729], "netviewdm1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[730], "netviewdm2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[731], "netviewdm3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[741], "netgw:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[742], "netrcs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[744], "flexlm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[747], "fujitsu-dev:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[748], "ris-cm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[749], "kerberos-adm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[750], "rfile:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[751], "pump:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[752], "qrh:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[753], "rrh:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[754], "tell:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[758], "nlogin:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[759], "con:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[760], "ns:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[761], "rxe:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[762], "quotad:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[763], "cycleserv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[764], "omserv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[765], "webster:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[767], "phonebook:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[769], "vid:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[770], "cadlock:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[771], "rtip:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[772], "cycleserv2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[773], "submit:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[774], "rpasswd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[775], "entomb:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[776], "wpages:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[780], "wpgs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[786], "concert:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[800], "mdbs_daemon:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[801], "device:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[996], "xtreelic:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[997], "maitrd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[998], "busboy:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[999], "garcon:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[999], "puprouter:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1000], "cadlock:", SN_PORT_TYPE_LEN);
#if( SN_MAX_TCP_PORTS > 17007 )
  strncpy (tcp_port_types[1025], "blackjack:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1030], "iad1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1031], "iad2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1032], "iad3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1067], "instl_boots:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1068], "instl_bootc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1080], "socks:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1083], "ansoft-lm-1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1084], "ansoft-lm-2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1155], "nfa:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1222], "nerv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1248], "hermes:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1346], "alta-ana-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1347], "bbn-mmc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1348], "bbn-mmx:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1349], "sbook:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1350], "editbench:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1351], "equationbuilder:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1352], "lotusnote:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1353], "relief:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1354], "rightbrain:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1356], "cuillamartin:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1357], "pegboard:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1358], "connlcli:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1359], "ftsrv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1360], "mimer:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1361], "linx:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1362], "timeflies:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1363], "ndm-requester:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1364], "ndm-server:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1365], "adapt-sna:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1366], "netware-csp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1367], "dcs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1368], "screencast:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1369], "gv-us:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1370], "us-gv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1371], "fc-cli:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1372], "fc-ser:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1373], "chromagrafx:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1374], "molly:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1375], "bytex:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1376], "ibm-pps:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1377], "cichlid:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1378], "elan:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1379], "dbreporter:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1380], "telesis-licman:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1381], "apple-licman:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1382], "udt_os:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1383], "gwha:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1384], "os-licman:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1385], "atex_elmd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1386], "checksum:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1387], "cadsi-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1388], "objective-dbc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1389], "iclpv-dm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1390], "iclpv-sc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1391], "iclpv-sas:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1392], "iclpv-pm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1393], "iclpv-nls:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1394], "iclpv-nlc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1395], "iclpv-wsm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1396], "dvl-activemail:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1397], "audio-activmail:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1398], "video-activmail:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1399], "cadkey-licman:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1400], "cadkey-tablet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1401], "goldleaf-licman:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1402], "prm-sm-np:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1403], "prm-nm-np:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1404], "igi-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1405], "ibm-res:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1406], "netlabs-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1407], "dbsa-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1408], "sophia-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1409], "here-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1410], "hiq:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1411], "af:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1412], "innosys:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1413], "innosys-acl:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1414], "ibm-mqseries:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1415], "dbstar:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1416], "novell-lu6.2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1417], "timbuktu-srv1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1418], "timbuktu-srv2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1419], "timbuktu-srv3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1420], "timbuktu-srv4:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1421], "gandalf-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1422], "autodesk-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1423], "essbase:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1424], "hybrid:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1425], "zion-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1426], "sas-1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1427], "mloadd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1428], "informatik-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1429], "nms:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1430], "tpdu:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1431], "rgtp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1432], "blueberry-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1433], "ms-sql-s:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1434], "ms-sql-m:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1435], "ibm-cics:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1436], "sas-2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1437], "tabula:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1438], "eicon-server:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1439], "eicon-x25:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1440], "eicon-slp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1441], "cadis-1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1442], "cadis-2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1443], "ies-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1444], "marcam-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1445], "proxima-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1446], "ora-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1447], "apri-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1448], "oc-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1449], "peport:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1450], "dwf:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1451], "infoman:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1452], "gtegsc-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1453], "genie-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1454], "interhdl_elmd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1455], "esl-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1456], "dca:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1457], "valisys-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1458], "nrcabq-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1459], "proshare1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1460], "proshare2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1461], "ibm_wrless_lan:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1462], "world-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1463], "nucleus:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1464], "msl_lmd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1465], "pipes:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1466], "oceansoft-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1467], "csdmbase:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1468], "csdm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1469], "aal-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1470], "uaiact:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1471], "csdmbase:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1472], "csdm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1473], "openmath:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1474], "telefinder:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1475], "taligent-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1476], "clvm-cfg:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1477], "ms-sna-server:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1478], "ms-sna-base:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1479], "dberegister:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1480], "pacerforum:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1481], "airs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1482], "miteksys-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1483], "afs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1484], "confluent:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1485], "lansource:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1486], "nms_topo_serv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1487], "localinfosrvr:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1488], "docstor:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1489], "dmdocbroker:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1490], "insitu-conf:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1491], "anynetgateway:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1492], "stone-design-1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1493], "netmap_lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1494], "ica:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1495], "cvc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1496], "liberty-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1497], "rfx-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1498], "watcom-sql:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1499], "fhc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1500], "vlsi-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1501], "sas-3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1502], "shivadiscovery:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1503], "imtc-mcs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1504], "evb-elm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1505], "funkproxy:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1524], "ingreslock:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1525], "orasrv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1525], "prospero-np:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1526], "pdap-np:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1527], "tlisrv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1529], "coauthor:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1600], "issd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1650], "nkd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1651], "proshareaudio:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1652], "prosharevideo:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1653], "prosharedata:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1654], "prosharerequest:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1655], "prosharenotify:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1661], "netview-aix-1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1662], "netview-aix-2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1663], "netview-aix-3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1664], "netview-aix-4:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1665], "netview-aix-5:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1666], "netview-aix-6:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1986], "licensedaemon:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1987], "tr-rsrb-p1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1988], "tr-rsrb-p2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1989], "mshnet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1989], "tr-rsrb-p3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1990], "stun-p1:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1991], "stun-p2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1992], "ipsendmsg:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1992], "stun-p3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1993], "snmp-tcp-port:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1994], "stun-port:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1995], "perf-port:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1996], "tr-rsrb-port:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1997], "gdp-port:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1998], "x25-svc-port:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[1999], "tcp-id-port:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2000], "callbook:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2001], "dc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2002], "globe:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2004], "mailbox:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2005], "berknet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2006], "invokator:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2007], "dectalk:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2008], "conf:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2009], "news:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2010], "search:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2011], "raid-cc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2012], "ttyinfo:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2013], "raid-am:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2014], "troff:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2015], "cypress:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2016], "bootserver:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2017], "cypress-stat:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2018], "terminaldb:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2019], "whosockami:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2020], "xinupageserver:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2021], "servexec:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2022], "down:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2023], "xinuexpansion3:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2024], "xinuexpansion4:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2025], "ellpack:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2026], "scrabble:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2027], "shadowserver:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2028], "submitserver:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2030], "device2:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2032], "blackboard:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2033], "glogger:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2034], "scoremgr:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2035], "imsldoc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2038], "objectmanager:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2040], "lam:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2041], "interbase:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2042], "isis:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2043], "isis-bcast:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2044], "rimsl:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2045], "cdfunc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2046], "sdfunc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2047], "dls:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2048], "dls-monitor:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2049], "shilp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2065], "dlsrpn:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2067], "dlswpn:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2201], "ats:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2500], "rtsserv:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2501], "rtsclient:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2564], "hp-3000-telnet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[2784], "www-dev:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[3049], "NSWS:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[3264], "ccmail:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[3333], "dec-notes:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[3421], "bmap:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[3900], "udt_os:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[3984], "mapper-nodemgr:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[3985], "mapper-mapethd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[3986], "mapper-ws_ethd:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[4132], "nuts_dem:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[4133], "nuts_bootp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[4343], "unicall:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[4444], "krb524:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[4672], "rfa:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5000], "commplex-main:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5001], "commplex-link:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5002], "rfe:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5010], "telelpathstart:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5011], "telelpathattack:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5050], "mmcc:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5145], "rmonitor_secure:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5190], "aol:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5236], "padl2sim:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5300], "hacl-hb:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5301], "hacl-gs:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5302], "hacl-cfg:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5303], "hacl-probe:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5304], "hacl-local:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[5305], "hacl-test:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[6000], "x11:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[6111], "sub-process:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[6141], "meta-corp:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[6142], "aspentec-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[6143], "watershed-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[6144], "statsci1-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[6145], "statsci2-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[6146], "lonewolf-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[6147], "montage-lm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[6558], "xdsxdm:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7000], "afs3-fileserver:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7001], "afs3-callback:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7002], "afs3-prserver:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7003], "afs3-vlserver:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7004], "afs3-kaserver:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7005], "afs3-volser:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7006], "afs3-errors:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7007], "afs3-bos:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7008], "afs3-update:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7009], "afs3-rmtsys:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7010], "ups-onlinet:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7100], "font-service:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[7200], "fodms:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[9535], "man:", SN_PORT_TYPE_LEN);
  strncpy (tcp_port_types[17007], "isode-dua:", SN_PORT_TYPE_LEN);
#endif
#endif
#endif


#if( defined(SN_MAX_UDP_PORTS) )
  for (temp_int = 0; temp_int <= SN_MAX_UDP_PORTS; temp_int++)
    {				/* Put port number as default label for all ports */
      sprintf (udp_port_types[temp_int], "%d:", temp_int);
    }				/* Put port number as default label for all ports */


#if( SN_MAX_UDP_PORTS >= 1024 )
  /* UDP port names */
  strncpy (udp_port_types[0], "fragment ", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1], "tcpmux:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2], "compressnet:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[3], "compressnet:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5], "rje:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7], "echo:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[9], "discard:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[11], "systat:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[13], "daytime:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[17], "qotd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[18], "msp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[19], "chargen:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[20], "ftp-data:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[21], "ftp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[23], "telnet:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[25], "smtp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[27], "nsw-fe:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[29], "msg-icp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[31], "msg-auth:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[33], "dsp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[37], "time:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[38], "rap:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[39], "rlp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[41], "graphics:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[42], "nameserver:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[43], "nicname:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[44], "mpm-flags:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[45], "mpm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[46], "mpm-snd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[47], "ni-ftp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[48], "auditd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[49], "login:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[50], "re-mail-ck:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[51], "la-maint:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[52], "xns-time:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[53], "domain:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[54], "xns-ch:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[55], "isi-gl:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[56], "xns-auth:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[58], "xns-mail:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[61], "ni-mail:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[62], "acas:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[64], "covia:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[65], "tacacs-ds:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[66], "sql*net:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[67], "bootps:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[68], "bootpc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[69], "tftp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[70], "gopher:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[71], "netrjs-1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[72], "netrjs-2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[73], "netrjs-3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[74], "netrjs-4:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[76], "deos:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[78], "vettcp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[79], "finger:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[80], "www-http:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[81], "hosts2-ns:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[82], "xfer:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[83], "mit-ml-dev:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[84], "ctf:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[85], "mit-ml-dev:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[86], "mfcobol:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[88], "kerberos:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[89], "su-mit-tg:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[90], "dnsix:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[91], "mit-dov:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[92], "npp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[93], "dcp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[94], "objcall:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[95], "supdup:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[96], "dixie:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[97], "swift-rvf:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[98], "tacnews:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[99], "metagram:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[101], "hostname:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[102], "iso-tsap:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[103], "gppitnp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[104], "acr-nema:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[105], "csnet-ns:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[106], "3com-tsmux:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[107], "rtelnet:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[108], "snagas:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[109], "pop2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[110], "pop3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[111], "sunrpc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[112], "mcidas:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[113], "auth:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[114], "audionews:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[115], "sftp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[116], "ansanotify:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[117], "uucp-path:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[118], "sqlserv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[119], "nntp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[120], "cfdptkt:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[121], "erpc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[122], "smakynet:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[123], "ntp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[124], "ansatrader:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[125], "locus-map:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[126], "unitary:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[127], "locus-con:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[128], "gss-xlicen:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[129], "pwdgen:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[130], "cisco-fna:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[131], "cisco-tna:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[132], "cisco-sys:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[133], "statsrv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[134], "ingres-net:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[135], "loc-srv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[136], "profile:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[137], "netbios-ns:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[138], "netbios-dgm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[139], "netbios-ssn:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[140], "emfis-data:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[141], "emfis-cntl:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[142], "bl-idm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[143], "imap2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[144], "news:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[145], "uaac:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[146], "iso-tp0:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[147], "iso-ip:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[148], "cronus:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[149], "aed-512:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[150], "sql-net:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[151], "hems:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[152], "bftp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[153], "sgmp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[154], "netsc-prod:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[155], "netsc-dev:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[156], "sqlsrv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[157], "knet-cmp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[158], "pcmail-srv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[159], "nss-routing:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[160], "sgmp-traps:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[161], "snmp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[162], "snmptrap:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[163], "cmip-man:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[164], "smip-agent:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[165], "xns-courier:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[166], "s-net:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[167], "namp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[168], "rsvd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[169], "send:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[170], "print-srv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[171], "multiplex:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[172], "cl/1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[173], "xyplex-mux:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[174], "mailq:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[175], "vmnet:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[176], "genrad-mux:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[177], "xdmcp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[178], "NextStep:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[179], "bgp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[180], "ris:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[181], "unify:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[182], "audit:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[183], "ocbinder:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[184], "ocserver:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[185], "remote-kis:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[186], "kis:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[187], "aci:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[188], "mumps:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[189], "qft:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[190], "cacp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[191], "prospero:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[192], "osu-nms:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[193], "srmp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[194], "irc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[195], "dn6-nlm-aud:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[196], "dn6-smm-red:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[197], "dls:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[198], "dls-mon:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[199], "smux:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[200], "src:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[201], "at-rtmp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[202], "at-nbp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[203], "at-3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[204], "at-echo:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[205], "at-5:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[206], "at-zis:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[207], "at-7:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[208], "at-8:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[209], "tam:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[210], "z39.50:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[211], "914c/g:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[212], "anet:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[213], "ipx:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[214], "vmpwscs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[215], "softpc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[216], "atls:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[217], "dbase:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[218], "mpp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[219], "uarps:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[220], "imap3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[221], "fln-spx:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[222], "rsh-spx:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[223], "cdc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[243], "sur-meas:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[245], "link:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[246], "dsp3270:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[344], "pdap:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[345], "pawserv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[346], "zserv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[347], "fatserv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[348], "csi-sgwp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[371], "clearcase:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[372], "ulistserv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[373], "legent-1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[374], "legent-2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[375], "hassle:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[376], "nip:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[377], "tnETOS:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[378], "dsETOS:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[379], "is99c:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[380], "is99s:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[381], "hp-collector:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[382], "hp-managed-node:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[383], "hp-alarm-mgr:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[384], "arns:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[386], "asa:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[387], "aurp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[388], "unidata-ldm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[389], "ldap:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[390], "uis:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[391], "synotics-relay:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[392], "synotics-broker:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[393], "dis:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[394], "embl-ndt:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[395], "netcp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[396], "netware-ip:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[397], "mptn:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[398], "kryptolan:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[400], "work-sol:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[401], "ups:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[402], "genie:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[403], "decap:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[404], "nced:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[405], "ncld:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[406], "imsp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[407], "timbuktu:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[408], "prm-sm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[409], "prm-nm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[410], "decladebug:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[411], "rmt:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[412], "synoptics-trap:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[413], "smsp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[414], "infoseek:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[415], "bnet:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[416], "silverplatter:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[417], "onmux:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[418], "hyper-g:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[419], "ariel1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[420], "smpte:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[421], "ariel2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[422], "ariel3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[423], "opc-job-start:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[424], "opc-job-track:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[425], "icad-el:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[426], "smartsdp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[427], "svrloc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[428], "ocs_cmu:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[429], "ocs_amu:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[430], "utmpsd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[431], "utmpcd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[432], "iasd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[433], "nnsp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[434], "mobileip-agent:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[435], "mobilip-mn:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[436], "dna-cml:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[437], "comscm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[438], "dsfgw:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[439], "dasp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[440], "sgcp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[441], "decvms-sysmgt:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[442], "cvc_hostd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[443], "https:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[444], "snpp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[445], "microsoft-ds:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[446], "ddm-rdb:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[447], "ddm-dfm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[448], "ddm-byte:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[449], "as-servermap:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[450], "tserver:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[512], "biff:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[513], "who:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[514], "syslog:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[515], "printer:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[517], "talk:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[518], "ntalk:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[519], "utime:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[520], "router:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[525], "timed:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[526], "tempo:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[530], "courier:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[531], "conference:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[532], "netnews:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[533], "netwall:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[539], "apertus-ldp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[540], "uucp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[541], "uucp-rlogin:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[543], "klogin:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[544], "kshell:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[550], "new-rwho:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[555], "dsf:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[556], "remotefs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[560], "rmonitor:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[561], "monitor:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[562], "chshell:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[564], "9pfs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[565], "whoami:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[570], "meter:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[571], "meter:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[600], "ipcserver:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[606], "urm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[607], "nqs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[608], "sift-uft:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[609], "npmp-trap:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[610], "npmp-local:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[611], "npmp-gui:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[634], "ginad:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[666], "mdqs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[704], "elcsd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[709], "entrustmanager:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[729], "netviewdm1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[730], "netviewdm2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[731], "netviewdm3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[741], "netgw:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[742], "netrcs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[744], "flexlm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[747], "fujitsu-dev:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[748], "ris-cm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[749], "kerberos-adm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[750], "loadav:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[751], "pump:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[752], "qrh:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[753], "rrh:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[754], "tell:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[758], "nlogin:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[759], "con:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[760], "ns:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[761], "rxe:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[762], "quotad:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[763], "cycleserv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[764], "omserv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[765], "webster:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[767], "phonebook:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[769], "vid:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[770], "cadlock:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[771], "rtip:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[772], "cycleserv2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[773], "notify:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[774], "acmaint_dbd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[775], "acmaint_transd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[776], "wpages:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[780], "wpgs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[786], "concert:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[800], "mdbs_daemon:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[801], "device:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[996], "xtreelic:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[997], "maitrd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[998], "puparp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[999], "applix:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[999], "puprouter:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1000], "ock:", SN_PORT_TYPE_LEN);
#if( SN_MAX_UDP_PORTS > 17007 )
  strncpy (udp_port_types[1025], "blackjack:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1030], "iad1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1031], "iad2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1032], "iad3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1067], "instl_boots:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1068], "instl_bootc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1080], "socks:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1083], "ansoft-lm-1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1084], "ansoft-lm-2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1155], "nfa:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1222], "nerv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1248], "hermes:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1346], "alta-ana-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1347], "bbn-mmc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1348], "bbn-mmx:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1349], "sbook:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1350], "editbench:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1351], "equationbuilder:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1352], "lotusnote:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1353], "relief:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1354], "rightbrain:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1356], "cuillamartin:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1357], "pegboard:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1358], "connlcli:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1359], "ftsrv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1360], "mimer:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1361], "linx:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1362], "timeflies:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1363], "ndm-requester:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1364], "ndm-server:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1365], "adapt-sna:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1366], "netware-csp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1367], "dcs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1368], "screencast:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1369], "gv-us:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1370], "us-gv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1371], "fc-cli:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1372], "fc-ser:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1373], "chromagrafx:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1374], "molly:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1375], "bytex:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1376], "ibm-pps:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1377], "cichlid:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1378], "elan:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1379], "dbreporter:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1380], "telesis-licman:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1381], "apple-licman:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1382], "udt_os:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1383], "gwha:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1384], "os-licman:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1385], "atex_elmd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1386], "checksum:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1387], "cadsi-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1388], "objective-dbc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1389], "iclpv-dm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1390], "iclpv-sc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1391], "iclpv-sas:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1392], "iclpv-pm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1393], "iclpv-nls:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1394], "iclpv-nlc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1395], "iclpv-wsm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1396], "dvl-activemail:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1397], "audio-activmail:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1398], "video-activmail:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1399], "cadkey-licman:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1400], "cadkey-tablet:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1401], "goldleaf-licman:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1402], "prm-sm-np:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1403], "prm-nm-np:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1404], "igi-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1405], "ibm-res:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1406], "netlabs-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1407], "dbsa-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1408], "sophia-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1409], "here-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1410], "hiq:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1411], "af:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1412], "innosys:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1413], "innosys-acl:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1414], "ibm-mqseries:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1415], "dbstar:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1416], "novell-lu6.2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1418], "timbuktu-srv2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1419], "timbuktu-srv3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1420], "timbuktu-srv4:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1421], "gandalf-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1422], "autodesk-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1423], "essbase:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1424], "hybrid:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1425], "zion-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1426], "sas-1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1427], "mloadd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1428], "informatik-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1429], "nms:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1430], "tpdu:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1431], "rgtp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1432], "blueberry-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1433], "ms-sql-s:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1434], "ms-sql-m:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1435], "ibm-cics:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1436], "sas-2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1437], "tabula:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1438], "eicon-server:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1439], "eicon-x25:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1440], "eicon-slp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1441], "cadis-1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1442], "cadis-2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1443], "ies-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1444], "marcam-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1445], "proxima-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1446], "ora-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1447], "apri-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1448], "oc-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1449], "peport:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1450], "dwf:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1451], "infoman:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1452], "gtegsc-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1453], "genie-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1455], "esl-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1456], "dca:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1457], "valisys-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1458], "nrcabq-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1459], "proshare1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1460], "proshare2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1461], "ibm_wrless_lan:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1462], "world-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1463], "nucleus:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1464], "msl_lmd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1465], "pipes:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1466], "oceansoft-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1467], "csdmbase:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1468], "csdm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1469], "aal-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1470], "uaiact:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1471], "csdmbase:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1472], "csdm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1473], "openmath:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1474], "telefinder:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1475], "taligent-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1476], "clvm-cfg:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1477], "ms-sna-server:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1478], "ms-sna-base:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1479], "dberegister:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1480], "pacerforum:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1481], "airs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1482], "miteksys-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1483], "afs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1484], "confluent:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1485], "lansource:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1486], "nms_topo_serv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1487], "localinfosrvr:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1488], "docstor:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1489], "dmdocbroker:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1490], "insitu-conf:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1491], "anynetgateway:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1492], "stone-design-1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1493], "netmap_lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1494], "ica:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1495], "cvc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1496], "liberty-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1497], "rfx-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1498], "watcom-sql:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1499], "fhc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1500], "vlsi-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1501], "sas-3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1502], "shivadiscovery:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1503], "imtc-mcs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1504], "evb-elm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1505], "funkproxy:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1524], "ingreslock:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1525], "orasrv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1525], "prospero-np:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1526], "pdap-np:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1527], "tlisrv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1529], "coauthor:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1600], "issd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1650], "nkd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1651], "proshareaudio:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1652], "prosharevideo:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1653], "prosharedata:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1654], "prosharerequest:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1655], "prosharenotify:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1661], "netview-aix-1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1662], "netview-aix-2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1663], "netview-aix-3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1664], "netview-aix-4:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1665], "netview-aix-5:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1666], "netview-aix-6:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1986], "licensedaemon:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1987], "tr-rsrb-p1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1988], "tr-rsrb-p2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1989], "mshnet:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1989], "tr-rsrb-p3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1990], "stun-p1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1991], "stun-p2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1992], "ipsendmsg:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1992], "stun-p3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1993], "snmp-tcp-port:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1994], "stun-port:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1995], "perf-port:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1996], "tr-rsrb-port:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1997], "gdp-port:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1998], "x25-svc-port:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[1999], "tcp-id-port:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2000], "callbook:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2001], "wizard:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2002], "globe:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2004], "emce:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2005], "oracle:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2006], "raid-cc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2007], "raid-am:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2008], "terminaldb:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2009], "whosockami:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2010], "pipe_server:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2011], "servserv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2012], "raid-ac:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2013], "raid-cd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2014], "raid-sf:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2015], "raid-cs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2016], "bootserver:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2017], "bootclient:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2018], "rellpack:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2019], "about:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2020], "xinupageserver:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2021], "xinuexpansion1:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2022], "xinuexpansion2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2023], "xinuexpansion3:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2024], "xinuexpansion4:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2025], "xribs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2026], "scrabble:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2027], "shadowserver:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2028], "submitserver:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2030], "device2:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2032], "blackboard:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2033], "glogger:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2034], "scoremgr:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2035], "imsldoc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2038], "objectmanager:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2040], "lam:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2041], "interbase:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2042], "isis:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2043], "isis-bcast:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2044], "rimsl:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2045], "cdfunc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2046], "sdfunc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2047], "dls:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2048], "dls-monitor:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2049], "shilp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2065], "dlsrpn:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2067], "dlswpn:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2201], "ats:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2500], "rtsserv:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2501], "rtsclient:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[2784], "www-dev:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[3049], "NSWS:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[3264], "ccmail:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[3333], "dec-notes:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[3421], "bmap:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[3900], "udt_os:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[3984], "mapper-nodemgr:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[3985], "mapper-mapethd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[3986], "mapper-ws_ethd:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[4132], "nuts_dem:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[4133], "nuts_bootp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[4343], "unicall:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[4444], "krb524:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[4672], "rfa:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5000], "commplex-main:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5001], "commplex-link:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5002], "rfe:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5010], "telelpathstart:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5011], "telelpathattack:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5050], "mmcc:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5145], "rmonitor_secure:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5190], "aol:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5236], "padl2sim:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5300], "hacl-hb:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5301], "hacl-gs:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5302], "hacl-cfg:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5303], "hacl-probe:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5304], "hacl-local:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[5305], "hacl-test:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[6111], "sub-process:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[6000], "x11:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[6141], "meta-corp:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[6142], "aspentec-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[6143], "watershed-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[6144], "statsci1-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[6145], "statsci2-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[6146], "lonewolf-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[6147], "montage-lm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[6558], "xdsxdm:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7000], "afs3-fileserver:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7001], "afs3-callback:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7002], "afs3-prserver:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7003], "afs3-vlserver:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7004], "afs3-kaserver:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7005], "afs3-volser:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7006], "afs3-errors:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7007], "afs3-bos:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7008], "afs3-update:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7009], "afs3-rmtsys:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7010], "ups-onlinet:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7100], "font-service:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[7200], "fodms:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[9535], "man:", SN_PORT_TYPE_LEN);
  strncpy (udp_port_types[17007], "isode-dua:", SN_PORT_TYPE_LEN);
#endif
#endif
#endif

#if( defined(SN_MAX_SAP) )
#if( SN_MAX_SAP >= 256 )
  for (temp_int = 0; temp_int < SN_MAX_SAP + 1; temp_int++)
    {				/* Put number as default label for all */
      sprintf (sap_port_types[temp_int], "0x%02X:", temp_int);
    }				/* Put number as default label for all */

  /* The SAP numbers came from someplace on the Web, called */
  /* cisco-lsap-list.txt */
  /* ieee-lsap-list.txt */

  /* The low bit is a group address flag, and odd-numbered addresses */
  /* have been added to allow clearer labeling of what's seen. */

  /* IEEE - Administered LSAPs */
  strncpy (sap_port_types[0x00], "Null:", SN_PORT_TYPE_LEN);	/* Null LSAP */
  strncpy (sap_port_types[0x01], "Null grp:", SN_PORT_TYPE_LEN);
  strncpy (sap_port_types[0x02], "LLC Mgt:", SN_PORT_TYPE_LEN);		/* Individual LLC Sublayer Mgmt Function */
  strncpy (sap_port_types[0x03], "LLCgMgt:", SN_PORT_TYPE_LEN);		/* Group LLC Sublayer Mgmt Function */
  strncpy (sap_port_types[0x06], "IP:", SN_PORT_TYPE_LEN);	/* ARPANET Internet Protocol (IP) */
  strncpy (sap_port_types[0x07], "IP grp:", SN_PORT_TYPE_LEN);	/* ARPANET Internet Protocol (IP) */
  strncpy (sap_port_types[0x0E], "PROWAY Mgt:", SN_PORT_TYPE_LEN);	/* PROWAY (IEC955) Network Mgmt & Initialization */
  strncpy (sap_port_types[0x0F], "PROWAYgMgt:", SN_PORT_TYPE_LEN);	/* PROWAY (IEC955) Network Mgmt & Initialization */
  strncpy (sap_port_types[0x42], "SpanTree:", SN_PORT_TYPE_LEN);	/* IEEE 802.1 Bridge Spanning Tree Protocol */
  strncpy (sap_port_types[0x43], "SpanT grp:", SN_PORT_TYPE_LEN);	/* IEEE 802.1 Bridge Spanning Tree Protocol */
  strncpy (sap_port_types[0x4E], "ManMsg:", SN_PORT_TYPE_LEN);	/* EIA RS-511 Manufacturing Message Service */
  strncpy (sap_port_types[0x4F], "ManMsg grp:", SN_PORT_TYPE_LEN);	/* EIA RS-511 Manufacturing Message Service */
  strncpy (sap_port_types[0x7E], "X.25:", SN_PORT_TYPE_LEN);	/* ISO 8208 (X.25 over IEEE 802.2 Type 2 LLC) */
  strncpy (sap_port_types[0x7F], "X.25 grp:", SN_PORT_TYPE_LEN);	/* ISO 8208 (X.25 over IEEE 802.2 Type 2 LLC) */
  strncpy (sap_port_types[0x8E], "POWAY Stn:", SN_PORT_TYPE_LEN);	/* PROWAY (IEC955) Active Station List Maintenance */
  strncpy (sap_port_types[0x8F], "POWAYgStn:", SN_PORT_TYPE_LEN);	/* PROWAY (IEC955) Active Station List Maintenance */
  strncpy (sap_port_types[0xAA], "SNAP:", SN_PORT_TYPE_LEN);	/* Sub-Network Access Protocol (SNAP) */
  strncpy (sap_port_types[0xAB], "SNAP grp:", SN_PORT_TYPE_LEN);	/* Sub-Network Access Protocol (SNAP) */
  strncpy (sap_port_types[0xFE], "ISO Net:", SN_PORT_TYPE_LEN);		/* ISO Network Layer Protocol */
  strncpy (sap_port_types[0xFF], "Global:", SN_PORT_TYPE_LEN);	/* Global LSAP */

  /* Manufacturer-Implemented LSAPs */
  strncpy (sap_port_types[0x04], "SNA:", SN_PORT_TYPE_LEN);	/* IBM SNA Path Control (individual) */
  strncpy (sap_port_types[0x05], "SNA grp:", SN_PORT_TYPE_LEN);		/* IBM SNA Path Control (group) */
  strncpy (sap_port_types[0x18], "TI:", SN_PORT_TYPE_LEN);	/* Texas Instruments */
  strncpy (sap_port_types[0x19], "TI grp:", SN_PORT_TYPE_LEN);	/* Texas Instruments */
  strncpy (sap_port_types[0x80], "XNS:", SN_PORT_TYPE_LEN);	/* Xerox Network Systems (XNS) */
  strncpy (sap_port_types[0x81], "XNS grp:", SN_PORT_TYPE_LEN);		/* Xerox Network Systems (XNS) */
  strncpy (sap_port_types[0x86], "Nestar:", SN_PORT_TYPE_LEN);	/* Nestar */
  strncpy (sap_port_types[0x87], "Nestar grp:", SN_PORT_TYPE_LEN);	/* Nestar */
  strncpy (sap_port_types[0x98], "ARP:", SN_PORT_TYPE_LEN);	/* ARPANET Address Resolution Protocol (ARP) */
  strncpy (sap_port_types[0x99], "ARP grp:", SN_PORT_TYPE_LEN);		/* ARPANET Address Resolution Protocol (ARP) */
  strncpy (sap_port_types[0xBC], "VINES:", SN_PORT_TYPE_LEN);	/* Banyan VINES */
  strncpy (sap_port_types[0xBD], "VINES grp:", SN_PORT_TYPE_LEN);	/* Banyan VINES */
  strncpy (sap_port_types[0xE0], "NetWare:", SN_PORT_TYPE_LEN);		/* Novell Netware */
  strncpy (sap_port_types[0xE1], "NetW grp:", SN_PORT_TYPE_LEN);	/* Novell Netware */
  strncpy (sap_port_types[0xF0], "NetBIOS:", SN_PORT_TYPE_LEN);		/* IBM NetBIOS */
  strncpy (sap_port_types[0xF1], "NetB grp:", SN_PORT_TYPE_LEN);	/* IBM NetBIOS */
  strncpy (sap_port_types[0xF4], "LAN Mgt:", SN_PORT_TYPE_LEN);		/* IBM LAN Management (individual) */
  strncpy (sap_port_types[0xF5], "LANgMgt:", SN_PORT_TYPE_LEN);		/* IBM LAN Management (group) */
  strncpy (sap_port_types[0xF8], "RPL:", SN_PORT_TYPE_LEN);	/* IBM Remote Program Load (RPL) */
  strncpy (sap_port_types[0xF9], "RPL grp:", SN_PORT_TYPE_LEN);		/* IBM Remote Program Load (RPL) */
  strncpy (sap_port_types[0xFA], "UB:", SN_PORT_TYPE_LEN);	/* Ungermann-Bass */
  strncpy (sap_port_types[0xFB], "UB grp:", SN_PORT_TYPE_LEN);	/* Ungermann-Bass */

  /* further mysteries... */
  strncpy (sap_port_types[0x0C], "xSNA:", SN_PORT_TYPE_LEN);	/* SNA */
  strncpy (sap_port_types[0x0D], "xSNA grp:", SN_PORT_TYPE_LEN);	/* SNA */
  strncpy (sap_port_types[0x10], "xNetWare:", SN_PORT_TYPE_LEN);	/* Netware */
  strncpy (sap_port_types[0x11], "xNetW grp:", SN_PORT_TYPE_LEN);	/* Netware */
  strncpy (sap_port_types[0xFC], "xRPL:", SN_PORT_TYPE_LEN);	/* RPL */
  strncpy (sap_port_types[0xFD], "xRPL grp:", SN_PORT_TYPE_LEN);	/* RPL */
#endif
#endif

#if( defined(SN_MAX_PROTO_DESC) )
#if( SN_MAX_PROTO_DESC >= 200 )
  for (temp_int = 0; temp_int < SN_MAX_PROTO_DESC + 1; temp_int++)
    {				/* Put number as default label for all */
      protocol_num[temp_int] = 0;
      sprintf (protocol_types[temp_int], "%d", temp_int);
    }				/* Put number as default label for all */

  protocol_num[0] = 0x0000;
  strncpy (protocol_types[0], "IEEE802.3", SN_PORT_TYPE_LEN);
  protocol_num[1] = 0x0101;
  strncpy (protocol_types[1], "Experimental", SN_PORT_TYPE_LEN);
  protocol_num[2] = 0x0200;
  strncpy (protocol_types[2], "XEROX PUP", SN_PORT_TYPE_LEN);
  protocol_num[3] = 0x0201;
  strncpy (protocol_types[3], "PUP Addr Trans", SN_PORT_TYPE_LEN);
  protocol_num[4] = 0x0400;
  strncpy (protocol_types[4], "Nixdorf", SN_PORT_TYPE_LEN);
  protocol_num[5] = 0x0600;
  strncpy (protocol_types[5], "XEROX NS IDP", SN_PORT_TYPE_LEN);
  protocol_num[6] = 0x0660;
  strncpy (protocol_types[6], "DLOG", SN_PORT_TYPE_LEN);
  protocol_num[7] = 0x0661;
  strncpy (protocol_types[7], "DLOG", SN_PORT_TYPE_LEN);
  protocol_num[8] = 0x0800;
  strncpy (protocol_types[8], "Ethernet", SN_PORT_TYPE_LEN);
  protocol_num[9] = 0x0801;
  strncpy (protocol_types[9], "X.75", SN_PORT_TYPE_LEN);
  protocol_num[10] = 0x0802;
  strncpy (protocol_types[10], "NBS", SN_PORT_TYPE_LEN);
  protocol_num[11] = 0x0803;
  strncpy (protocol_types[11], "ECMA", SN_PORT_TYPE_LEN);
  protocol_num[12] = 0x0804;
  strncpy (protocol_types[12], "Chaosnet", SN_PORT_TYPE_LEN);
  protocol_num[13] = 0x0805;
  strncpy (protocol_types[13], "X.25", SN_PORT_TYPE_LEN);
  protocol_num[14] = 0x0806;
  strncpy (protocol_types[14], "ARP", SN_PORT_TYPE_LEN);
  protocol_num[15] = 0x0807;
  strncpy (protocol_types[15], "XNS Compat", SN_PORT_TYPE_LEN);
  protocol_num[16] = 0x081C;
  strncpy (protocol_types[16], "Symbolics", SN_PORT_TYPE_LEN);
  protocol_num[17] = 0x0888;
  strncpy (protocol_types[17], "Xyplex", SN_PORT_TYPE_LEN);
  protocol_num[18] = 0x0900;
  strncpy (protocol_types[18], "UB netdebug", SN_PORT_TYPE_LEN);
  protocol_num[19] = 0x0A00;
  strncpy (protocol_types[19], "IEEE802.3 PUP", SN_PORT_TYPE_LEN);
  protocol_num[20] = 0x0A01;
  strncpy (protocol_types[20], "PUP Addr Trans", SN_PORT_TYPE_LEN);
  protocol_num[21] = 0x0BAD;
  strncpy (protocol_types[21], "VINES", SN_PORT_TYPE_LEN);
  protocol_num[22] = 0x1000;
  strncpy (protocol_types[22], "Berk Trailer neg", SN_PORT_TYPE_LEN);
  protocol_num[23] = 0x1001;
  strncpy (protocol_types[23], "Berk Trailer encap/IP", SN_PORT_TYPE_LEN);
  protocol_num[24] = 0x1600;
  strncpy (protocol_types[24], "Valid Sys", SN_PORT_TYPE_LEN);
  protocol_num[25] = 0x4242;
  strncpy (protocol_types[25], "PCS Basic Block Protocol", SN_PORT_TYPE_LEN);
  protocol_num[26] = 0x5208;
  strncpy (protocol_types[26], "BBN Simnet", SN_PORT_TYPE_LEN);
  protocol_num[27] = 0x6000;
  strncpy (protocol_types[27], "DEC Unassigned", SN_PORT_TYPE_LEN);
  protocol_num[28] = 0x6001;
  strncpy (protocol_types[28], "DEC MOP Dump/Load", SN_PORT_TYPE_LEN);
  protocol_num[29] = 0x6002;
  strncpy (protocol_types[29], "DEC MOP Remote Console", SN_PORT_TYPE_LEN);
  protocol_num[30] = 0x6003;
  strncpy (protocol_types[30], "DEC DECNET Phase IV Route", SN_PORT_TYPE_LEN);
  protocol_num[31] = 0x6004;
  strncpy (protocol_types[31], "DEC LAT", SN_PORT_TYPE_LEN);
  protocol_num[32] = 0x6005;
  strncpy (protocol_types[32], "DEC Diag", SN_PORT_TYPE_LEN);
  protocol_num[33] = 0x6006;
  strncpy (protocol_types[33], "DEC Customer", SN_PORT_TYPE_LEN);
  protocol_num[34] = 0x6007;
  strncpy (protocol_types[34], "DEC LAVC, SCA", SN_PORT_TYPE_LEN);
  protocol_num[35] = 0x6008;
  strncpy (protocol_types[35], "DEC Unassigned", SN_PORT_TYPE_LEN);
  protocol_num[36] = 0x6010;
  strncpy (protocol_types[36], "3Com", SN_PORT_TYPE_LEN);
  protocol_num[37] = 0x7000;
  strncpy (protocol_types[37], "UB downld", SN_PORT_TYPE_LEN);
  protocol_num[38] = 0x7002;
  strncpy (protocol_types[38], "UB dia/loop", SN_PORT_TYPE_LEN);
  protocol_num[39] = 0x7020;
  strncpy (protocol_types[39], "LRT", SN_PORT_TYPE_LEN);
  protocol_num[40] = 0x7030;
  strncpy (protocol_types[40], "Proteon", SN_PORT_TYPE_LEN);
  protocol_num[41] = 0x7034;
  strncpy (protocol_types[41], "Cabletron", SN_PORT_TYPE_LEN);
  protocol_num[42] = 0x8003;
  strncpy (protocol_types[42], "Cronus VLN", SN_PORT_TYPE_LEN);
  protocol_num[43] = 0x8004;
  strncpy (protocol_types[43], "Cronus Direct", SN_PORT_TYPE_LEN);
  protocol_num[44] = 0x8005;
  strncpy (protocol_types[44], "HP Probe", SN_PORT_TYPE_LEN);
  protocol_num[45] = 0x8006;
  strncpy (protocol_types[45], "Nestar", SN_PORT_TYPE_LEN);
  protocol_num[46] = 0x8008;
  strncpy (protocol_types[46], "AT&T", SN_PORT_TYPE_LEN);
  protocol_num[47] = 0x8010;
  strncpy (protocol_types[47], "Excelan", SN_PORT_TYPE_LEN);
  protocol_num[48] = 0x8013;
  strncpy (protocol_types[48], "SGI diagnostics", SN_PORT_TYPE_LEN);
  protocol_num[49] = 0x8014;
  strncpy (protocol_types[49], "SGI net games", SN_PORT_TYPE_LEN);
  protocol_num[50] = 0x8015;
  strncpy (protocol_types[50], "SGI reserved", SN_PORT_TYPE_LEN);
  protocol_num[51] = 0x8016;
  strncpy (protocol_types[51], "SGI bounce server", SN_PORT_TYPE_LEN);
  protocol_num[52] = 0x8019;
  strncpy (protocol_types[52], "Apollo", SN_PORT_TYPE_LEN);
  protocol_num[53] = 0x802E;
  strncpy (protocol_types[53], "Tymshare", SN_PORT_TYPE_LEN);
  protocol_num[54] = 0x802F;
  strncpy (protocol_types[54], "Tigan", SN_PORT_TYPE_LEN);
  protocol_num[55] = 0x8035;
  strncpy (protocol_types[55], "RARP", SN_PORT_TYPE_LEN);
  protocol_num[56] = 0x8036;
  strncpy (protocol_types[56], "Aeonic Systems", SN_PORT_TYPE_LEN);
  protocol_num[57] = 0x8038;
  strncpy (protocol_types[57], "DEC LANBridge", SN_PORT_TYPE_LEN);
  protocol_num[58] = 0x8039;
  strncpy (protocol_types[58], "DEC Unassigned", SN_PORT_TYPE_LEN);
  protocol_num[59] = 0x803D;
  strncpy (protocol_types[59], "DEC Encryption", SN_PORT_TYPE_LEN);
  protocol_num[60] = 0x803E;
  strncpy (protocol_types[60], "DEC Unassigned", SN_PORT_TYPE_LEN);
  protocol_num[61] = 0x803F;
  strncpy (protocol_types[61], "DEC LAN Monitor", SN_PORT_TYPE_LEN);
  protocol_num[62] = 0x8040;
  strncpy (protocol_types[62], "DEC Unassigned", SN_PORT_TYPE_LEN);
  protocol_num[63] = 0x8044;
  strncpy (protocol_types[63], "Planning Research", SN_PORT_TYPE_LEN);
  protocol_num[64] = 0x8046;
  strncpy (protocol_types[64], "AT&T", SN_PORT_TYPE_LEN);
  protocol_num[65] = 0x8047;
  strncpy (protocol_types[65], "AT&T", SN_PORT_TYPE_LEN);
  protocol_num[66] = 0x8049;
  strncpy (protocol_types[66], "ExperData", SN_PORT_TYPE_LEN);
  protocol_num[67] = 0x805B;
  strncpy (protocol_types[67], "Stanford V Kernel exp.", SN_PORT_TYPE_LEN);
  protocol_num[68] = 0x805C;
  strncpy (protocol_types[68], "Stanford V Kernel prod.", SN_PORT_TYPE_LEN);
  protocol_num[69] = 0x805D;
  strncpy (protocol_types[69], "Evans & Sutherland", SN_PORT_TYPE_LEN);
  protocol_num[70] = 0x8060;
  strncpy (protocol_types[70], "Little Machines", SN_PORT_TYPE_LEN);
  protocol_num[71] = 0x8062;
  strncpy (protocol_types[71], "Counterpoint", SN_PORT_TYPE_LEN);
  protocol_num[72] = 0x8065;
  strncpy (protocol_types[72], "Univ. of Mass. @ Amherst", SN_PORT_TYPE_LEN);
  protocol_num[73] = 0x8066;
  strncpy (protocol_types[73], "Univ. of Mass. @ Amherst", SN_PORT_TYPE_LEN);
  protocol_num[74] = 0x8067;
  strncpy (protocol_types[74], "Veeco Integrated Auto.", SN_PORT_TYPE_LEN);
  protocol_num[75] = 0x8068;
  strncpy (protocol_types[75], "General Dynamics", SN_PORT_TYPE_LEN);
  protocol_num[76] = 0x8069;
  strncpy (protocol_types[76], "AT&T", SN_PORT_TYPE_LEN);
  protocol_num[77] = 0x806A;
  strncpy (protocol_types[77], "Autophon", SN_PORT_TYPE_LEN);
  protocol_num[78] = 0x806C;
  strncpy (protocol_types[78], "ComDesign", SN_PORT_TYPE_LEN);
  protocol_num[79] = 0x806D;
  strncpy (protocol_types[79], "Computgraphic", SN_PORT_TYPE_LEN);
  protocol_num[80] = 0x806E;
  strncpy (protocol_types[80], "Landmark Graphics", SN_PORT_TYPE_LEN);
  protocol_num[81] = 0x807A;
  strncpy (protocol_types[81], "Matra", SN_PORT_TYPE_LEN);
  protocol_num[82] = 0x807B;
  strncpy (protocol_types[82], "Dansk Data Elektronik", SN_PORT_TYPE_LEN);
  protocol_num[83] = 0x807C;
  strncpy (protocol_types[83], "Merit Internodal", SN_PORT_TYPE_LEN);
  protocol_num[84] = 0x807D;
  strncpy (protocol_types[84], "Vitalink Communications", SN_PORT_TYPE_LEN);
  protocol_num[85] = 0x8080;
  strncpy (protocol_types[85], "Vitalink TransLAN III", SN_PORT_TYPE_LEN);
  protocol_num[86] = 0x8081;
  strncpy (protocol_types[86], "Counterpoint", SN_PORT_TYPE_LEN);
  protocol_num[87] = 0x809B;
  strncpy (protocol_types[87], "Appletalk", SN_PORT_TYPE_LEN);
  protocol_num[88] = 0x809C;
  strncpy (protocol_types[88], "Datability", SN_PORT_TYPE_LEN);
  protocol_num[89] = 0x809F;
  strncpy (protocol_types[89], "Spider Systems", SN_PORT_TYPE_LEN);
  protocol_num[90] = 0x80A3;
  strncpy (protocol_types[90], "Nixdorf2", SN_PORT_TYPE_LEN);
  protocol_num[91] = 0x80A4;
  strncpy (protocol_types[91], "Siemens Gammasonics", SN_PORT_TYPE_LEN);
  protocol_num[92] = 0x80C0;
  strncpy (protocol_types[92], "DCA Data Exchange Cluster", SN_PORT_TYPE_LEN);
  protocol_num[93] = 0x80C4;
  strncpy (protocol_types[93], "Banyan", SN_PORT_TYPE_LEN);
  protocol_num[94] = 0x80C5;
  strncpy (protocol_types[94], "Banyan", SN_PORT_TYPE_LEN);
  protocol_num[95] = 0x80C6;
  strncpy (protocol_types[95], "Pacer Software", SN_PORT_TYPE_LEN);
  protocol_num[96] = 0x80C7;
  strncpy (protocol_types[96], "Applitek", SN_PORT_TYPE_LEN);
  protocol_num[97] = 0x80C8;
  strncpy (protocol_types[97], "Intergraph", SN_PORT_TYPE_LEN);
  protocol_num[98] = 0x80CD;
  strncpy (protocol_types[98], "Harris", SN_PORT_TYPE_LEN);
  protocol_num[99] = 0x80CF;
  strncpy (protocol_types[99], "Taylor Instrument", SN_PORT_TYPE_LEN);
  protocol_num[100] = 0x80D3;
  strncpy (protocol_types[100], "Rosemount Corporation", SN_PORT_TYPE_LEN);
  protocol_num[101] = 0x80D5;
  strncpy (protocol_types[101], "SNA/Ether", SN_PORT_TYPE_LEN);
  protocol_num[102] = 0x80DD;
  strncpy (protocol_types[102], "Varian Associates", SN_PORT_TYPE_LEN);
  protocol_num[103] = 0x80DE;
  strncpy (protocol_types[103], "Integrated Solutions TRFS", SN_PORT_TYPE_LEN);
  protocol_num[104] = 0x80E0;
  strncpy (protocol_types[104], "Allen-Bradley", SN_PORT_TYPE_LEN);
  protocol_num[105] = 0x80E4;
  strncpy (protocol_types[105], "Datability", SN_PORT_TYPE_LEN);
  protocol_num[106] = 0x80F2;
  strncpy (protocol_types[106], "Retix", SN_PORT_TYPE_LEN);
  protocol_num[107] = 0x80F3;
  strncpy (protocol_types[107], "AppleTalk AARP (Kinetics)", SN_PORT_TYPE_LEN);
  protocol_num[108] = 0x80F4;
  strncpy (protocol_types[108], "Kinetics", SN_PORT_TYPE_LEN);
  protocol_num[109] = 0x80F7;
  strncpy (protocol_types[109], "Apollo", SN_PORT_TYPE_LEN);
  protocol_num[110] = 0x80FF;
  strncpy (protocol_types[110], "Wellfleet", SN_PORT_TYPE_LEN);
  protocol_num[111] = 0x8107;
  strncpy (protocol_types[111], "Symbolics Private", SN_PORT_TYPE_LEN);
  protocol_num[112] = 0x8130;
  strncpy (protocol_types[112], "Hayes", SN_PORT_TYPE_LEN);
  protocol_num[113] = 0x8131;
  strncpy (protocol_types[113], "VG Laboratory Systems", SN_PORT_TYPE_LEN);
  protocol_num[114] = 0x8132;
  strncpy (protocol_types[114], "Bridge Communications", SN_PORT_TYPE_LEN);
  protocol_num[115] = 0x8137;
  strncpy (protocol_types[115], "Novell", SN_PORT_TYPE_LEN);
  protocol_num[116] = 0x8139;
  strncpy (protocol_types[116], "KTI", SN_PORT_TYPE_LEN);
  protocol_num[117] = 0x8148;
  strncpy (protocol_types[117], "Logicraft", SN_PORT_TYPE_LEN);
  protocol_num[118] = 0x8149;
  strncpy (protocol_types[118], "NCD", SN_PORT_TYPE_LEN);
  protocol_num[119] = 0x814A;
  strncpy (protocol_types[119], "Alpha Micro", SN_PORT_TYPE_LEN);
  protocol_num[120] = 0x814C;
  strncpy (protocol_types[120], "SNMP", SN_PORT_TYPE_LEN);
  protocol_num[121] = 0x814D;
  strncpy (protocol_types[121], "BIIN", SN_PORT_TYPE_LEN);
  protocol_num[122] = 0x814E;
  strncpy (protocol_types[122], "BIIN", SN_PORT_TYPE_LEN);
  protocol_num[123] = 0x814F;
  strncpy (protocol_types[123], "Technically Elite Concept", SN_PORT_TYPE_LEN);
  protocol_num[124] = 0x8150;
  strncpy (protocol_types[124], "Rational", SN_PORT_TYPE_LEN);
  protocol_num[125] = 0x8151;
  strncpy (protocol_types[125], "Qualcomm", SN_PORT_TYPE_LEN);
  protocol_num[126] = 0x815C;
  strncpy (protocol_types[126], "Computer Protocol Pty", SN_PORT_TYPE_LEN);
  protocol_num[127] = 0x8164;
  strncpy (protocol_types[127], "Charles River", SN_PORT_TYPE_LEN);
  protocol_num[128] = 0x817D;
  strncpy (protocol_types[128], "Protocol Engines", SN_PORT_TYPE_LEN);
  protocol_num[129] = 0x818D;
  strncpy (protocol_types[129], "Motorola", SN_PORT_TYPE_LEN);
  protocol_num[130] = 0x819A;
  strncpy (protocol_types[130], "Qualcomm", SN_PORT_TYPE_LEN);
  protocol_num[131] = 0x81A4;
  strncpy (protocol_types[131], "ARAI Bunkichi", SN_PORT_TYPE_LEN);
  protocol_num[132] = 0x81A5;
  strncpy (protocol_types[132], "RAD Network Devices", SN_PORT_TYPE_LEN);
  protocol_num[133] = 0x81B7;
  strncpy (protocol_types[133], "Xyplex", SN_PORT_TYPE_LEN);
  protocol_num[134] = 0x81CC;
  strncpy (protocol_types[134], "Apricot", SN_PORT_TYPE_LEN);
  protocol_num[135] = 0x81D6;
  strncpy (protocol_types[135], "Artisoft", SN_PORT_TYPE_LEN);
  protocol_num[136] = 0x81E6;
  strncpy (protocol_types[136], "Polygon", SN_PORT_TYPE_LEN);
  protocol_num[137] = 0x81F0;
  strncpy (protocol_types[137], "Comsat Labs", SN_PORT_TYPE_LEN);
  protocol_num[138] = 0x81F3;
  strncpy (protocol_types[138], "SAIC", SN_PORT_TYPE_LEN);
  protocol_num[139] = 0x81F6;
  strncpy (protocol_types[139], "VG Analytical", SN_PORT_TYPE_LEN);
  protocol_num[140] = 0x8203;
  strncpy (protocol_types[140], "Quantum Software", SN_PORT_TYPE_LEN);
  protocol_num[141] = 0x8221;
  strncpy (protocol_types[141], "Ascom Banking", SN_PORT_TYPE_LEN);
  protocol_num[142] = 0x823E;
  strncpy (protocol_types[142], "AdvEncrypSys", SN_PORT_TYPE_LEN);
  protocol_num[143] = 0x827F;
  strncpy (protocol_types[143], "Athena Programming", SN_PORT_TYPE_LEN);
  protocol_num[144] = 0x8263;
  strncpy (protocol_types[144], "Charles River Data System", SN_PORT_TYPE_LEN);
  protocol_num[145] = 0x829A;
  strncpy (protocol_types[145], "Inst Ind Info Tech", SN_PORT_TYPE_LEN);
  protocol_num[146] = 0x829C;
  strncpy (protocol_types[146], "Taurus Controls", SN_PORT_TYPE_LEN);
  protocol_num[147] = 0x82AC;
  strncpy (protocol_types[147], "Walker Richer & Quinn", SN_PORT_TYPE_LEN);
  protocol_num[148] = 0x8694;
  strncpy (protocol_types[148], "Idea Courier", SN_PORT_TYPE_LEN);
  protocol_num[149] = 0x869E;
  strncpy (protocol_types[149], "Computer Network Tech", SN_PORT_TYPE_LEN);
  protocol_num[150] = 0x86A3;
  strncpy (protocol_types[150], "Gateway Comm", SN_PORT_TYPE_LEN);
  protocol_num[151] = 0x86DB;
  strncpy (protocol_types[151], "SECTRA", SN_PORT_TYPE_LEN);
  protocol_num[152] = 0x86DE;
  strncpy (protocol_types[152], "Delta Controls", SN_PORT_TYPE_LEN);
  protocol_num[153] = 0x86DF;
  strncpy (protocol_types[153], "ATOMIC", SN_PORT_TYPE_LEN);
  protocol_num[154] = 0x86E0;
  strncpy (protocol_types[154], "Landis & Gyr Powers", SN_PORT_TYPE_LEN);
  protocol_num[155] = 0x8700;
  strncpy (protocol_types[155], "Motorola", SN_PORT_TYPE_LEN);
  protocol_num[156] = 0x8A96;
  strncpy (protocol_types[156], "Invisible Software", SN_PORT_TYPE_LEN);
  protocol_num[157] = 0x9000;
  strncpy (protocol_types[157], "Loopback", SN_PORT_TYPE_LEN);
  protocol_num[158] = 0x9001;
  strncpy (protocol_types[158], "3Com(Bridge) XNS Sys Mgmt", SN_PORT_TYPE_LEN);
  protocol_num[159] = 0x9002;
  strncpy (protocol_types[159], "3Com(Bridge) TCP-IP Sys", SN_PORT_TYPE_LEN);
  protocol_num[160] = 0x9003;
  strncpy (protocol_types[160], "3Com(Bridge) loop detect", SN_PORT_TYPE_LEN);
  protocol_num[161] = 0xFF00;
  strncpy (protocol_types[161], "BBN VITAL-LanBridge cache", SN_PORT_TYPE_LEN);

  protocol_num[SN_PROT_IEEE802_3] = SN_PROT_IEEE802_3;
  strncpy (protocol_types[SN_PROT_IEEE802_3], "IEEE802.3", SN_PORT_TYPE_LEN);
  protocol_num[SN_PROT_SLIP] = SN_PROT_SLIP;
  strncpy (protocol_types[SN_PROT_SLIP], "SLIP", SN_PORT_TYPE_LEN);
  protocol_num[SN_PROT_PPP] = SN_PROT_PPP;
  strncpy (protocol_types[SN_PROT_PPP], "PPP", SN_PORT_TYPE_LEN);
  protocol_num[SN_PROT_LOOP] = SN_PROT_LOOP;
  strncpy (protocol_types[SN_PROT_LOOP], "Loop int", SN_PORT_TYPE_LEN);
#endif
#endif

}
