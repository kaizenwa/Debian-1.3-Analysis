#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>

#include "config.h"
#include "emu.h"
#include "video.h"
#include "mouse.h"
#include "serial.h"
#include "keymaps.h"
#include "memory.h"
#include "bios.h"
#include "kversion.h"

#include "dos2linux.h"
#include "priv.h"


/*
 * XXX - the mem size of 734 is much more dangerous than 704. 704 is the
 * bottom of 0xb0000 memory.  use that instead?
 */
#define MAX_MEM_SIZE    640


struct debug_flags d =
{  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
/* d  R  W  D  C  v  X  k  i  T  s  m  #  p  g  c  w  h  I  E  x  M  n  P  r  S */

static void     check_for_env_autoexec_or_config(void);
int     parse_debugflags(const char *s, unsigned char flag);
static void     usage(void);

/*
 * DANG_BEGIN_FUNCTION config_defaults
 * 
 * description: 
 * Set all values in the `config` structure to their default
 * value. These will be modified by the config parser.
 * 
 * DANG_END_FUNCTION
 * 
 */
static void
config_defaults(void)
{
    config.hdiskboot = 1;	/* default hard disk boot */
    config.mem_size = 640;
    config.ems_size = 0;
    config.ems_frame = 0xd000;
    config.xms_size = 0;
    config.max_umb = 0;
    config.dpmi = 0;
    config.secure = 1;  /* need to have it 'on', else user may trick it out
                           via -F option */
    config.mathco = 1;
    config.mouse_flag = 0;
    config.mapped_bios = 0;
    config.mapped_sbios = 0;
    config.vbios_file = NULL;
    config.vbios_copy = 0;
    config.vbios_seg = 0xc000;
    config.vbios_size = 0x10000;
    config.console = 0;
    config.console_keyb = 0;
    config.console_video = 0;
    config.kbd_tty = 0;
    config.fdisks = 0;
    config.hdisks = 0;
    config.bootdisk = 0;
    config.exitearly = 0;
    config.term_esc_char = 30;	       /* Ctrl-^ */
    /* config.term_method = METHOD_FAST; */
    config.term_color = 1;
    /* config.term_updatelines = 25; */
    config.term_updatefreq = 4;
    config.term_charset = CHARSET_LATIN;
    /* config.term_corner = 1; */
    config.X_updatelines = 25;
    config.X_updatefreq = 8;
    config.X_display = NULL;	/* NULL means use DISPLAY variable */
    config.X_title = "dosemu";
    config.X_icon_name = "dosemu";
    config.X_blinkrate = 8;
    config.X_sharecmap = 0;     /* Don't share colourmap in graphics modes */
    config.X_mitshm = 0;
    config.X_keycode = 0;
    config.X_font = "vga";
    config.usesX = 0;
    config.X = 0;
    config.hogthreshold = 10;	/* bad estimate of a good garrot value */
    config.chipset = PLAINVGA;
    config.cardtype = CARD_VGA;
    config.fullrestore = 0;
    config.graphics = 0;
    config.gfxmemsize = 256;
    config.vga = 0;		/* this flags BIOS graphics */
    config.dualmon = 0;
    config.force_vt_switch = 0;
    config.speaker = SPKR_EMULATED;

    /* The frequency in usec of the SIGALRM call (in signal.c) is
     * equal to this value / 6, and thus is currently 9158us = 100 Hz
     * The 6 (TIMER DIVISOR) is a constant of unknown origin
     * NOTE: if you set 'timer 18' in config.dist you can't get anything
     * better that 55555 (108Hz) because of integer math.
     * see timer_interrupt_init() in init.c
     */
    config.update = 54945;	/* should be = 1E6/config.freq */
    config.freq = 18;		/* rough frequency (real PC = 18.2065) */

    config.timers = 1;		/* deliver timer ints */
    config.keybint = 1;		/* keyboard interrupts */
 
    /* Lock file stuff */
    config.tty_lockdir = PATH_LOCKD;    /* The Lock directory  */
    config.tty_lockfile = NAME_LOCKF;   /* Lock file pretext ie LCK.. */
    config.tty_lockbinary = FALSE;      /* Binary lock files ? */

    config.num_ser = 0;
    config.num_lpt = 0;
    vm86s.cpu_type = CPU_386;
    config.fastfloppy = 1;

    config.emusys = (char *) NULL;
    config.emubat = (char *) NULL;
    config.emuini = (char *) NULL;
    tmpdir = strdup(tempnam("/tmp", "dosemu"));
    config.dosbanner = 1;
    config.allowvideoportaccess = 0;

    config.keyboard = KEYB_US;	/* What's the current keyboard  */
    config.key_map = key_map_us;/* pointer to the keyboard-maps */
    config.shift_map = shift_map_us;	/* Here the Shilt-map           */
    config.alt_map = alt_map_us;/* And the Alt-map              */
    config.num_table = num_table_dot;	/* Numeric keypad has a dot     */
    config.detach = 0;		/* Don't detach from current tty and open
				 * new VT. */
    config.debugout = NULL;	/* default to no debug output file */

    config.pre_stroke =NULL;	/* defualt no keyboard pre-strokes */

    config.sillyint = 0;
    config.must_spare_hardware_ram = 0;
    memset(config.hardware_pages, 0, sizeof(config.hardware_pages));

    mice->fd = -1;
    mice->add_to_io_select = 0;
    mice->type = 0;
    mice->flags = 0;
    mice->intdrv = 0;
    mice->cleardtr = 0;
    mice->baudRate = 0;
    mice->sampleRate = 0;
    mice->lastButtons = 0;
    mice->chordMiddle = 0;

    config.sb_base = 0x220;
    config.sb_dma = 1;
    config.sb_irq = 5;
#ifdef __NetBSD__
    config.sb_dsp = "/dev/sound";
    config.sb_mixer = "/dev/mixer";
#else
    config.sb_dsp = "/dev/dsp";
    config.sb_mixer = "/dev/mixer";
#endif /* !__NetBSD__ */
    config.mpu401_base = 0x330;
}

static void 
open_terminal_pipe(char *path)
{
    enter_priv_off();
    terminal_fd = DOS_SYSCALL(open(path, O_RDWR));
    leave_priv_setting();
    if (terminal_fd == -1) {
	terminal_pipe = 0;
	error("ERROR: open_terminal_pipe failed - cannot open %s!\n", path);
	return;
    } else
	terminal_pipe = 1;
}

static void 
open_Xkeyboard_pipe(char *path)
{
    keypipe = DOS_SYSCALL(open(path, O_RDWR));
    if (keypipe == -1) {
	keypipe = 0;
	error("ERROR: open_Xkeyboard_pipe failed - cannot open %s!\n", path);
	return;
    }
    return;
}

static void 
open_Xmouse_pipe(char *path)
{
    mousepipe = DOS_SYSCALL(open(path, O_RDWR));
    if (mousepipe == -1) {
	mousepipe = 0;
	error("ERROR: open_Xmouse_pipe failed - cannot open %s!\n", path);
	return;
    }
    return;
}

/*
 * DANG_BEGIN_FUNCTION config_init
 * 
 * description: 
 * This is called to parse the command-line arguments and config
 * files. 
 *
 * DANG_END_FUNCTION
 * 
 */
void 
config_init(int argc, char **argv)
{
    extern char *commandline_statements;
    int             c;
    char           *confname = NULL;

    config_defaults();

#ifdef X_SUPPORT
    /*
     * DANG_BEGIN_REMARK For simpler support of X, DOSEMU can be started
     * by a symbolic link called `xdos` which DOSEMU will use to switch
     * into X-mode. DANG_END_REMARK
     */
    {
	char           *p;
	p = strrchr(argv[0], '/');	/* parse the program name */
	p = p ? p + 1 : argv[0];

	if (strcmp(p, "xdos") == 0)
	    config.X = 1;	/* activate X mode if dosemu was */
	/* called as 'xdos'              */
    }
#endif

    opterr = 0;
    confname = CONFIG_FILE;
    while ((c = getopt(argc, argv, "ABCcF:I:kM:D:P:VNtsgx:Km234e:E:dXY:Z:o:O")) != EOF) {
	switch (c) {
	case 'F':
	    if (get_orig_uid()) {
		FILE *f;
		enter_priv_off();
		f=fopen(optarg, "r");
		leave_priv_setting();
		if (!f) {
		  fprintf(stderr, "Sorry, no access to configuration file %s\n", optarg);
		  exit(1);
		}
		fclose(f);
	    }
	    confname = optarg;	/* someone reassure me that this is *safe*? */
	    break;
	case 'I':
	    commandline_statements = optarg;
	    break;
	case 'd':
	    if (config.detach)
		break;
	    config.detach = (unsigned short) detach();
	    break;
	case 'D':
	    parse_debugflags(optarg, 1);
	    break;
	case 'O':
	    fprintf(stderr, "using stderr for debug-output\n");
	    dbg_fd = stderr;
	    break;
	case 'o':
	    config.debugout = strdup(optarg);
	    enter_priv_off();
	    dbg_fd = fopen(config.debugout, "w");
	    leave_priv_setting();
	    if (!dbg_fd) {
		fprintf(stderr, "can't open \"%s\" for writing\n", config.debugout);
		exit(1);
	    }
	    break;
	}
    }

#if defined(__NetBSD__) && defined(X_SUPPORT) && defined(X_GRAPHICS)
    { extern int selfmem_fd;
    /* do this before any set*id functions are called */
    selfmem_fd = open("/proc/curproc/mem", O_RDWR);
    }
#endif

    parse_config(confname);

    if (config.exitearly)
	leavedos(0);

#ifdef __NetBSD__
    optreset = 1;
    optind = 1;
#endif
#ifdef __linux__
    optind = 0;
#endif
    opterr = 0;
    while ((c = getopt(argc, argv, "ABCcF:I:kM:D:P:v:VNtT:sgx:Km2345e:dXY:Z:E:o:O")) != EOF) {
	switch (c) {
	case 'F':		/* previously parsed config file argument */
	case 'I':
	case 'd':
	case 'o':
	case 'O':
	    break;
	case 'A':
	    config.hdiskboot = 0;
	    break;
	case 'B':
	    config.hdiskboot = 2;
	    break;
	case 'C':
	    config.hdiskboot = 1;
	    break;
	case 'c':
	    config.console_video = 1;
	    break;
	case 'k':
	    config.console_keyb = 1;
	    break;
	case 'X':
#ifdef X_SUPPORT
	    config.X = 1;
#else
	    error("X support not compiled in\n");
#endif
	    break;
	case 'Y':
#ifdef X_SUPPORT
	    open_Xkeyboard_pipe(optarg);
	    config.cardtype = CARD_MDA;
	    config.mapped_bios = 0;
	    config.vbios_file = NULL;
	    config.vbios_copy = 0;
	    config.vbios_seg = 0xc000;
	    config.console_video = 0;
	    config.chipset = 0;
	    config.fullrestore = 0;
	    config.graphics = 0;
	    config.vga = 0;	/* this flags BIOS graphics */
	    config.usesX = 1;
	    config.console_keyb = 1;
#endif
	    break;
	case 'Z':
#ifdef X_SUPPORT
	    open_Xmouse_pipe(optarg);
	    config.usesX = 1;
#endif
	    break;
	case 'K':
	    warn("Keyboard interrupt enabled...this is still buggy!\n");
	    config.keybint = 1;
	    break;
	case 'M':{
		int             max_mem = config.vga ? 640 : MAX_MEM_SIZE;
		config.mem_size = atoi(optarg);
		if (config.mem_size > max_mem)
		    config.mem_size = max_mem;
		break;
	    }
	case 'D':
	    parse_debugflags(optarg, 1);
	    break;
	case 'P':
	    if (terminal_fd == -1) {
		open_terminal_pipe(optarg);
	    } else
		error("ERROR: terminal pipe already open\n");
	    break;
	case 'V':
	    g_printf("Configuring as VGA video card & mapped ROM\n");
	    config.vga = 1;
	    config.mapped_bios = 1;
	    if (config.mem_size > 640)
		config.mem_size = 640;
	    break;
	case 'v':
	    config.cardtype = atoi(optarg);
	    if (config.cardtype > 7)	/* keep it updated when adding a new card! */
		config.cardtype = 1;
	    g_printf("Configuring cardtype as %d\n", config.cardtype);
	    break;
	case 'N':
	    warn("DOS will not be started\n");
	    config.exitearly = 1;
	    break;
	case 'T':
	    g_printf("Using tmpdir=%s\n", optarg);
	    free(tmpdir);
	    tmpdir = strdup(optarg);
	    break;
	case 't':
	    g_printf("doing timer emulation\n");
	    config.timers = 1;
	    break;
	case 's':
	    g_printf("using new scrn size code\n");
	    sizes = 1;
	    break;
	case 'g':
	    g_printf("turning graphics option on\n");
	    config.graphics = 1;
	    break;

	case 'x':
	    config.xms_size = atoi(optarg);
	    x_printf("enabling %dK XMS memory\n", config.xms_size);
	    break;

	case 'e':
	    config.ems_size = atoi(optarg);
	    g_printf("enabling %dK EMS memory\n", config.ems_size);
	    break;

	case 'm':
	    g_printf("turning MOUSE support on\n");
	    config.mouse_flag = 1;
	    break;

	case '2':
	    g_printf("CPU set to 286\n");
	    vm86s.cpu_type = CPU_286;
	    break;

	case '3':
	    g_printf("CPU set to 386\n");
	    vm86s.cpu_type = CPU_386;
	    break;

	case '4':
	    g_printf("CPU set to 486\n");
	    vm86s.cpu_type = CPU_486;
	    break;

	case '5':
	    g_printf("CPU set to 586\n");
	    vm86s.cpu_type = CPU_586;
	    break;

	case 'E':
	    g_printf("DOS command given on command line\n");
	    misc_e6_store_command(optarg);
	    break;

	case '?':
	default:
	    fprintf(stderr, "unrecognized option: -%c\n\r", c);
	    usage();
	    fflush(stdout);
	    fflush(stderr);
	    _exit(1);
	}
    }
    if (config.X) {
	if (!config.X_keycode) {
	    extern void keyb_layout(int layout);
	    keyb_layout(-1);
	    c_printf("CONF: Forceing neutral Keyboard-layout, X-server will translate\n");
	}
	config.console_video = config.vga = config.graphics = 0;
    }
    check_for_env_autoexec_or_config();
    if (under_root_login)  c_printf("CONF: running exclusively as ROOT:");
    else {
#ifdef RUN_AS_ROOT
      c_printf("CONF: mostly running as ROOT:");
#else
      c_printf("CONF: mostly running as USER:");
#endif
    }
    c_printf(" uid=%d (cached %d) gid=%d (cached %d)\n",
        geteuid(), get_cur_euid(), getegid(), get_cur_egid());
}


static void 
check_for_env_autoexec_or_config(void)
{
    char           *cp;
    cp = getenv("AUTOEXEC");
    if (cp)
	config.emubat = cp;
    cp = getenv("CONFIG");
    if (cp)
	config.emusys = cp;


     /*
      * The below is already reported in the conf. It is in no way an error
      * so why the messages?
      */

#if 0
    if (config.emubat)
	fprintf(stderr, "autoexec extension = %s\n", config.emubat);
    if (config.emusys)
	fprintf(stderr, "config extension = %s\n", config.emusys);
#endif
}

/*
 * DANG_BEGIN_FUNCTION parse_debugflags
 * 
 * arguments: 
 * s - string of options.
 * 
 * description: 
 * This part is fairly flexible...you specify the debugging
 * flags you wish with -D string.  The string consists of the following
 * characters: +   turns the following options on (initial state) -
 * turns the following options off a   turns all the options on/off,
 * depending on whether +/- is set 0-9 sets debug levels (0 is off, 9 is
 * most verbose) #   where # is a letter from the valid option list (see
 * docs), turns that option off/on depending on the +/- state.
 * 
 * Any option letter can occur in any place.  Even meaningless combinations,
 * such as "01-a-1+0vk" will be parsed without error, so be careful. Some
 * options are set by default, some are clear. This is subject to my whim.
 * You can ensure which are set by explicitly specifying.
 * 
 * DANG_END_FUNCTION
 */
int parse_debugflags(const char *s, unsigned char flag)
{
    char            c;

#ifdef X_SUPPORT
    const char      allopts[] = "dRWDCvXkiTsm#pgcwhIExMnPrS";
#else
    const char      allopts[] = "dRWDCvkiTsm#pgcwhIExMnPrS";
#endif

    /*
     * if you add new classes of debug messages, make sure to add the
     * letter to the allopts string above so that "1" and "a" can work
     * correctly.
     */

    dbug_printf("debug flags: %s\n", s);
    while ((c = *(s++)))
	switch (c) {
	case '+':		/* begin options to turn on */
	    if (!flag)
		flag = 1;
	    break;
	case '-':		/* begin options to turn off */
	    flag = 0;
	    break;

	case 'd':		/* disk */
	    d.disk = flag;
	    break;
	case 'R':		/* disk READ */
	    d.read = flag;
	    break;
	case 'W':		/* disk WRITE */
	    d.write = flag;
	    break;
	case 'D':		/* DOS int 21h */
	    d.dos = flag;
	    break;
        case 'C':               /* CDROM */
	    d.cdrom = flag;
            break;
	case 'v':		/* video */
	    d.video = flag;
	    break;
#ifdef X_SUPPORT
	case 'X':
	    d.X = flag;
	    break;
#endif
	case 'k':		/* keyboard */
	    d.keyb = flag;
	    break;
	case 'i':		/* i/o instructions (in/out) */
	    d.io = flag;
	    break;
	case 'T':		/* i/o port tracing */
	    d.io_trace = flag;
	    break;
	case 's':		/* serial */
	    d.serial = flag;
	    break;
	case 'm':		/* mouse */
	    d.mouse = flag;
	    break;
	case '#':		/* default int */
	    d.defint =flag;
	    break;
	case 'p':		/* printer */
	    d.printer = flag;
	    break;
	case 'g':		/* general messages */
	    d.general = flag;
	    break;
	case 'c':		/* configuration */
	    d.config = flag;
	    break;
	case 'w':		/* warnings */
	    d.warning = flag;
	    break;
	case 'h':		/* hardware */
	    d.hardware = flag;
	    break;
	case 'I':		/* IPC */
	    d.IPC = flag;
	    break;
	case 'E':		/* EMS */
	    d.EMS = flag;
	    break;
	case 'x':		/* XMS */
	    d.xms = flag;
	    break;
	case 'M':		/* DPMI */
	    d.dpmi = flag;
	    break;
	case 'n':		/* IPX network */
	    d.network = flag;
	    break;
	case 'P':		/* Packet driver */
	    d.pd = flag;
	    break;
	case 'r':		/* PIC */
	    d.request = flag;
	    break;
	case 'S':		/* SOUND */
	    d.sound = flag;
	    break;
	case 'a':{		/* turn all on/off depending on flag */
		char           *newopts = (char *) malloc(strlen(allopts) + 2);

		newopts[0] = flag ? '+' : '-';
		newopts[1] = 0;
		strcat(newopts, allopts);
		parse_debugflags(newopts, flag);
		free(newopts);
		break;
	    }
	case '0'...'9':	/* set debug level, 0 is off, 9 is most
				 * verbose */
	    flag = c - '0';
	    break;
	default:
	    fprintf(stderr, "Unknown debug-msg mask: %c\n\r", c);
	    dbug_printf("Unknown debug-msg mask: %c\n", c);
	    return 1;
	}
  return 0;
}

static void
usage(void)
{
    fprintf(stdout, "dosemu 0.66\n");
    fprintf(stdout, "usage: dos [-ABCckbVNtsgxKm234e] [-D flags] [-M SIZE] [-P FILE] [ -F File ] 2> dosdbg\n");
    fprintf(stdout, "    -2,3,4,5 choose 286, 386, 486 or 586 CPU\n");
    fprintf(stdout, "    -A boot from first defined floppy disk (A)\n");
    fprintf(stdout, "    -B boot from second defined floppy disk (B) (#)\n");
    fprintf(stdout, "    -b map BIOS into emulator RAM (%%)\n");
    fprintf(stdout, "    -C boot from first defined hard disk (C)\n");
    fprintf(stdout, "    -c use PC console video (!%%)\n");
    fprintf(stdout, "    -d detach (?)\n");
#ifdef X_SUPPORT
    fprintf(stdout, "    -X run in X Window (#)\n");
/* seems no longer valid bo 18.7.95
    fprintf(stdout, "    -Y NAME use MDA direct and FIFO NAME for keyboard (only with x2dos!)\n");
    fprintf(stdout, "    -Z NAME use FIFO NAME for mouse (only with x2dos!)\n");
*/
    fprintf(stdout, "    -D set debug-msg mask to flags {+-}{0-9}{#CDEIMPRSWXcdghikmnprsvwx}\n");
#else				/* X_SUPPORT */
    fprintf(stdout, "    -D set debug-msg mask to flags {+-}{0-9}{#CDEIMPRSWcdghikmnprsvwx}\n");
#endif				/* X_SUPPORT */
    fprintf(stdout, "       #=defint  C=cdrom    D=dos    E=ems       I=ipc     M=dpmi\n");
    fprintf(stdout, "       P=packet  R=diskread S=sound  W=diskwrite c=config  d=disk\n");
    fprintf(stdout, "       g=general h=hardware i=i/o    k=keyb      m=mouse   n=ipxnet\n");
    fprintf(stdout, "       p=printer r=pic      s=serial v=video     w=warning x=xms\n");
    fprintf(stdout, "    -E STRING pass DOS command on command line\n");
    fprintf(stdout, "    -e SIZE enable SIZE K EMS RAM\n");
    fprintf(stdout, "    -F use config-file File\n");
    fprintf(stdout, "    -I insert config statements (on commandline)\n");
    fprintf(stdout, "    -g enable graphics modes (!%%#)\n");
    fprintf(stdout, "    -K Do int9 (!#)\n");
    fprintf(stdout, "    -k use PC console keyboard (!)\n");
    fprintf(stdout, "    -M set memory size to SIZE kilobytes (!)\n");
    fprintf(stdout, "    -m enable mouse support (!#)\n");
    fprintf(stdout, "    -N No boot of DOS\n");
    fprintf(stdout, "    -O write debugmessages to stderr\n");
    fprintf(stdout, "    -o FILE put debugmessages in file\n");
    fprintf(stdout, "    -P copy debugging output to FILE\n");
    fprintf(stdout, "    -T DIR set tmpdir\n");
    fprintf(stdout, "    -t try new timer code (#)\n");
    fprintf(stdout, "    -V use BIOS-VGA video modes (!#%%)\n");
    fprintf(stdout, "    -v NUM force video card type\n");
    fprintf(stdout, "    -x SIZE enable SIZE K XMS RAM\n");
    fprintf(stdout, "    (!) BE CAREFUL! READ THE DOCS FIRST!\n");
    fprintf(stdout, "    (%%) require dos be run as root (i.e. suid)\n");
    fprintf(stdout, "    (#) options do not fully work yet\n");
}
