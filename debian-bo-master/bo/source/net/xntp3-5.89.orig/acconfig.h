/* debugging code */
#undef DEBUG

/* MD5 authentication */
#undef MD5

/* DFS authentication (COCOM only) */
#undef DES

/* reference clock interface */
#undef REFCLOCK

/* ACTS modem service */
#undef ACTS

/* Austron 2200A/2201A GPS receiver */
#undef AS2201

/* Arbiter 1088A/B GPS receiver */
#undef ARBITER

/* PPS interface */
#undef ATOM

/* Datum/Bancomm bc635/VME interface */
#undef BANC

/* ELV/DCF7000 clock */
#undef CLOCK_DCF7000

/* HOPF 6021 clock */
#undef CLOCK_HOPF6021

/* Meinberg clocks */
#undef CLOCK_MEINBERG

/* DCF77 raw time code */
#undef CLOCK_RAWDCF

/* RCC 8000 clock */
#undef CLOCK_RCC8000

/* Schmid DCF77 clock */
#undef CLOCK_SCHMID

/* Trimble GPS receiver/TAIP protocol */
#undef CLOCK_TRIMTAIP

/* Trimble GPS receiver/TSIP protocol */
#undef CLOCK_TRIMTSIP

/* Diems Computime Radio Clock */
#undef CLOCK_COMPUTIME

/* Datum Programmable Time System */
#undef DATUM

/* TrueTime GPS receiver/VME interface */
#undef GPSVME

/* Heath GC-1000 WWV/WWVH receiver */
#undef HEATH

/* HP 58503A GPS receiver */
#undef HPGPS

/* Sun IRIG audio decoder */
#undef IRIG

/* Leitch CSD 5300 Master Clock System Driver */
#undef LEITCH

/* local clock reference */
#undef LOCAL_CLOCK

/* EES M201 MSF receiver */
#undef MSFEES

/* Magnavox MX4200 GPS receiver */
#undef MX4200

/* NMEA GPS receiver */
#undef NMEA

/* PARSE driver interface */
#undef PARSE

/* PARSE kernel PLL PPS support */
#undef PPS_SYNC

/* PCL 720 clock support */
#undef PPS720

/* PST/Traconex 1020 WWV/WWVH receiver */
#undef PST

/* PTB modem service */
#undef PTBACTS

/* KSI/Odetics TPRO/S GPS receiver/IRIG interface */
#undef TPRO

/* TRAK 8810 GPS receiver */
#undef TRAK

/* Kinemetrics/TrueTime receivers */
#undef TRUETIME

/* USNO modem service */
#undef USNO

/* Spectracom 8170/Netclock/2 WWVB receiver */
#undef WWVB

/* define if it's OK to declare char *sys_errlist[]; */
#undef CHAR_SYS_ERRLIST

/* define if it's OK to declare int syscall P((int, struct timeval *, struct timeval *)); */
#undef DECL_SYSCALL

/* define if we have syscall is buggy (Solaris 2.4) */
#undef SYSCALL_BUG

/* Do we need extra room for SO_RCVBUF? (HPUX <8) */
#undef NEED_RCVBUF_SLOP

/* Do we want the HPUX FindConfig()? */
#undef NEED_HPUX_FINDCONFIG

/* canonical system (cpu-vendor-os) string */
#undef STR_SYSTEM

/* define if [gs]ettimeofday() only takes 1 argument */
#undef SYSV_TIMEOFDAY

/* define if struct sockaddr has sa_len */
#undef HAVE_SA_LEN_IN_STRUCT_SOCKADDR

/* define if function prototypes are OK */
#undef HAVE_PROTOTYPES

/* define if setpgrp takes 0 arguments */
#undef HAVE_SETPGRP_0

/* hardwire a value for tick? */
#undef PRESET_TICK

/* hardwire a value for tickadj? */
#undef PRESET_TICKADJ

/* is adjtime() accurate? */
#undef ADJTIME_IS_ACCURATE

/* should we NOT read /dev/kmem? */
#undef NOKMEM

/* use UDP Wildcard Delivery? */
#undef UDP_WILDCARD_DELIVERY

/* always slew the clock? */
#undef SLEWALWAYS

/* step, then slew the clock? */
#undef STEP_SLEW

/* force ntpdate to step the clock if !defined(STEP_SLEW) ? */
#undef FORCE_NTPDATE_STEP

/* synch TODR hourly? */
#undef DOSYNCTODR

/* do we set process groups with -pid? */
#undef UDP_BACKWARDS_SETOWN

/* must we have a CTTY for fsetown? */
#undef USE_FSETOWNCTTY

/* can we use SIGIO for tcp and udp IO? */
#undef HAVE_SIGNALED_IO

/* can we use SIGPOLL for UDP? */
#undef USE_UDP_SIGPOLL

/* can we use SIGPOLL for tty IO? */
#undef USE_TTY_SIGPOLL

/* should we use clock_settime()? */
#undef USE_CLOCK_SETTIME

/* do we have the chu_clk line discipline/streams module? */
#undef CHUCLK

/* do we have the ppsclock streams module? */
#undef PPS

/* do we have the tty_clk line discipline/streams module? */
#undef TTYCLK

/* does the kernel support precision time discipline? */
#undef KERNEL_PLL

/* does the kernel support multicasting IP? */
#undef MCAST

/* do we have ntp_{adj,get}time in libc? */
#undef NTP_SYSCALLS_LIBC

/* do we have ntp_{adj,get}time in the kernel? */
#undef NTP_SYSCALLS_STD

/* do we have STREAMS/TLI? (Can we replace this with HAVE_SYS_STROPTS_H? */
#undef STREAMS_TLI

/* do we need an s_char typedef? */
#undef NEED_S_CHAR_TYPEDEF

/* include the GDT Surveying code? */
#undef GDT_SURVEYING

/* does SIOCGIFCONF return size in the buffer? */
#undef SIZE_RETURNED_IN_BUFFER

/* what is the name of TICK in the kernel? */
#undef K_TICK_NAME

/* Is K_TICK_NAME (nsec_per_tick, for example) in nanoseconds? */
#undef TICK_NANO

/* what is the name of TICKADJ in the kernel? */
#undef K_TICKADJ_NAME

/* Is K_TICKADJ_NAME (hrestime_adj, for example) in nanoseconds? */
#undef TICKADJ_NANO

/* what is (probably) the name of DOSYNCTODR in the kernel? */
#undef K_DOSYNCTODR_NAME

/* what is (probably) the name of NOPRINTF in the kernel? */
#undef K_NOPRINTF_NAME

/* do we need HPUX adjtime() library support? */
#undef NEED_HPUX_ADJTIME

/* Might nlist() values require an extra level of indirection (AIX)? */
#undef NLIST_EXTRA_INDIRECTION

/* Should we recommend a minimum value for tickadj? */
#undef MIN_REC_TICKADJ

/* Is there a problem using PARENB and IGNPAR (IRIX)? */
#undef NO_PARENB_IGNPAR

/* Should we not IGNPAR (Linux)? */
#undef RAWDCF_NO_IGNPAR

/* Does DTR power the DCF77 (Linux)? */
#undef RAWDCF_SETDTR

/* Does the compiler like "volatile"? */
#undef VOLATILE

/* Does qsort expect to work on "void *" stuff? */
#undef QSORT_USES_VOID_P

/* What is the fallback value for HZ? */
#undef DEFAULT_HZ

/***/

/* Which way should we declare... */

/* adjtime()? */
#undef DECL_ADJTIME_0

/* bzero()? */
#undef DECL_BZERO_0

/* ioctl()? */
#undef DECL_IOCTL_0

/* IPC? (bind, connect, recvfrom, sendto, setsockopt, socket) */
#undef DECL_IPC_0

/* memmove()? */
#undef DECL_MEMMOVE_0

/* mktemp()? */
#undef DECL_MKTEMP_0

/* rename()? */
#undef DECL_RENAME_0

/* select()? */
#undef DECL_SELECT_0

/* setitimer()? */
#undef DECL_SETITIMER_0

/* setpriority()? */
#undef DECL_SETPRIORITY_0
#undef DECL_SETPRIORITY_1

/* stdio stuff? */
#undef DECL_STDIO_0

/* strtol()? */
#undef DECL_STRTOL_0

/* syslog() stuff? */
#undef DECL_SYSLOG_0

/* time()? */
#undef DECL_TIME_0

/* [gs]ettimeofday()? */
#undef DECL_TIMEOFDAY_0

/* tolower()? */
#undef DECL_TOLOWER_0

