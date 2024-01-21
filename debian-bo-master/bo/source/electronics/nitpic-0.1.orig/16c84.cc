/*
 * 16c84.cc -- contains all 16c84-specific simulator routines
 */
#include "picsim.hh"
#include <ctype.h>

info_reg	 pic16c84_info[]={
	{"STATUS",{"IRP","RP1","RP0","TO","PD","Z","DC","C"},0,0},
	{"OPTION",{"RBPU","INTE","RTS","RTE","PSA","PS2","PS1","PS0"},0,0},
	{"INTCON",{"GIE","EEIE","RTIE","INTE","RBIE","RTIF","INTF","RBIF"},0,0},
	{NULL,{NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},0,0}
};

pin_info	 pic16c84_pin[18] = {
	{ "RA2", 0, 0 },
	{ "RA3", 0, 0 },
	{ "RA4", 0, 0 },
	{ "MCLR", 0, 0 },
	{ "Vss", 2, 0 },
	{ "RB0", 0, 0 },
	{ "RB1", 0, 0 },
	{ "RB2", 0, 0 },
	{ "RB3", 0, 0 },
	{ "RB4", 0, 0 },
	{ "RB5", 0, 0 },
	{ "RB6", 0, 0 },
	{ "RB7", 0, 0 },
	{ "Vdd", 2, 0 },
	{ "OSC2", 2, 0 },
	{ "OSC1", 2, 0 },
	{ "RA0", 0, 0 },
	{ "RA1", 0, 0 }
} ;

pins_info	 pic16c84_pins = {
	18,
	pic16c84_pin
} ;

typedef struct {
	u_int	 mask, dont_care, discriminator;
	u_int	 literal;
	unsigned d : 1,
			 f : 1,
			 b : 1;
	char	*name;
	void	(*execute)( void );
} pic16c84_instr;

#define	C		0001			// Carry
#define	DC		0002			// Digit carry
#define	Z		0004			// Zero
#define	PD		0010			// Power Down
#define	TO		0020			// Time-out
#define	RP0		0040			// Page select bit 0
#define	RP1		0100			// Page select bit 1 (not used)
#define	IRP		0200			// Indirect addressing page select (not used)

static pic16c84_instr	 PIC16C84_INSTR[];
static int				 match_instr( u_int instr );
static void				 use_a_cycle( Boolean no_ints = False );
static void				 sleep_a_cycle( void );
static char				 get_f( u_int instr, int *p = NULL, int *r = NULL );
static char				 get_w( void );
static void				 set_f( int p, int r, char val, Boolean set_z );
static void				 set_w( char val, Boolean set_z );
static void				 set_status( int set, int clear );
static void				 init_pic16c84( void );
static void				 reset_pic16c84( void );
static char				*disasm_pic16c84( u_int instr );

static char				 status_hook( reg *status, Boolean write, char v = 0 );
static char				 port_hook( reg *port, Boolean write, char v = 0 );

/*
 * This is the entry point to the Pic16C84 simulator.  All calls from the
 * generic simulator come through here.
 */
SimAR
pic16c84( SimFunc func, SimAR arg )
{
	SimAR		 result;
	int			 i;
	static char	 buf[64];
	
	switch (func) {
	  case Init:
		init_pic16c84( );
		return result;

	  case Reset:
		reset_pic16c84( );
		return result;
		
	  case GetPage:
		result.i = (Pic.reg_file[0][3].value >> 4) & 3;
		return result;

	  case GetInfo:
		result.p = pic16c84_info;
		pic16c84_info[0].value = Pic.reg_file[0][3].value;
		pic16c84_info[1].value = Pic.reg_file[1][1].value;
		pic16c84_info[2].value = Pic.reg_file[0][11].value;
		return result;

	  case GetPins:
		result.p = &pic16c84_pins;
		pic16c84_pin[0].io = !(Pic.reg_file[1][5].value & 4);
		pic16c84_pin[0].hi = (Pic.reg_file[0][5].value & 4) != 0;
		pic16c84_pin[1].io = !(Pic.reg_file[1][5].value & 8);
		pic16c84_pin[1].hi = (Pic.reg_file[0][5].value & 8) != 0;
		pic16c84_pin[2].io = !(Pic.reg_file[1][5].value & 16);
		pic16c84_pin[2].hi = (Pic.reg_file[0][5].value & 16) != 0;
		pic16c84_pin[16].io = !(Pic.reg_file[1][5].value & 1);
		pic16c84_pin[16].hi = (Pic.reg_file[0][5].value & 1) != 0;
		pic16c84_pin[17].io = !(Pic.reg_file[1][5].value & 2);
		pic16c84_pin[17].hi = (Pic.reg_file[0][5].value & 2) != 0;
		for (i = 0; i < 8; i++) {
			pic16c84_pin[i + 5].io = !(Pic.reg_file[1][6].value & (1 << i));
			pic16c84_pin[i + 5].hi = (Pic.reg_file[0][6].value & (1 << i))!=0;
		}
		return result;

	  case Disassemble:
		result.p = buf;
		sprintf( buf, "%04X %s",
				arg.i, disasm_pic16c84( Pic.uinfo.picmemmap[arg.i] ) );
		return result;

	  case Step:
		if (!Pic.sleeping) {
			if ((i = match_instr( Pic.uinfo.picmemmap[Pic.pc] )) == -1) {
				fprintf( stderr,
						"executed invalid opcode 0x%04X at PC 0x%04X as NOP\n",
						Pic.uinfo.picmemmap[Pic.pc],
						Pic.pc );
				use_a_cycle( );
			} else {
				PIC16C84_INSTR[i].execute( );
				use_a_cycle( );
			}
		} else {
			sleep_a_cycle( );
		}
		return result;

	  default:
		exit( 1 );
	}
}

/*
 * PIC16C84 initialization
 */
static void
init_pic16c84( void )
{
	int		 i, j;
	
	Pic.isize = 0x400;
	Pic.reg_pages = 2;
	Pic.nstack = 8;

	for (i = 0; i < Pic.reg_pages; i++) {
		for (j = 0; j < 128; j++) {
			Pic.reg_file[i][j].redirect_page = i; /* redirect to self */ 
			Pic.reg_file[i][j].redirect_reg = j;
			Pic.reg_file[i][j].implemented = (j <= 0x2f) ? 0xff : 0;
			Pic.reg_file[i][j].modified = 0;
			Pic.reg_file[i][j].modified_last = 0;
			Pic.reg_file[i][j].value = 0;
			Pic.reg_file[i][j].hook = 0;

			switch (j) {
			  case 0:
				Pic.reg_file[i][j].name = "     W";
				Pic.reg_file[i][j].redirect_page = 0;
				break;
			  case 1:
				if (!i)
					Pic.reg_file[i][j].name = "  RTCC";
				else {
					Pic.reg_file[i][j].name = "OPTION";
					Pic.reg_file[i][j].value = 0xff;
				}
				break;
			  case 2:
				Pic.reg_file[i][j].name = "   PCL";
				Pic.reg_file[i][j].redirect_page = 0;
				break;
			  case 3:
				Pic.reg_file[i][j].name = "STATUS";
				Pic.reg_file[i][j].value = 0x18;
				Pic.reg_file[i][j].redirect_page = 0;
				Pic.reg_file[0][j].hook = status_hook;
				break;
			  case 4:
				Pic.reg_file[i][j].name = "   FSR";
				Pic.reg_file[i][j].redirect_page = 0;
				break;
			  case 5:
				Pic.reg_file[i][j].name = (!i) ? " PORTA" : " TRISA";
				Pic.reg_file[i][j].implemented = 0x1f;
				if (i) Pic.reg_file[i][j].value = 0x1f;
				Pic.reg_file[i][j].hook = port_hook;
				break;
			  case 6:
				Pic.reg_file[i][j].name = (!i) ? " PORTB" : " TRISB";
				if (i) Pic.reg_file[i][j].value = 0xff;
				Pic.reg_file[i][j].hook = port_hook;
				break;
			  case 7:
				Pic.reg_file[i][j].name = "------";
				Pic.reg_file[i][j].implemented = 0;
				break;
			  case 8:
				Pic.reg_file[i][j].name = (!i) ? "EEDATA" : "EECON1"; break;
			  case 9:
				Pic.reg_file[i][j].name = (!i) ? " EEADR" : "EECON2"; break;
			  case 10:
				Pic.reg_file[i][j].name = "PCLATH";
				Pic.reg_file[i][j].implemented = 0x1f;
				Pic.reg_file[i][j].redirect_page = 0;
				break;
			  case 11:
				Pic.reg_file[i][j].name = "INTCON"; break;
			  default:
				Pic.reg_file[i][j].name = NULL;
				if (i) Pic.reg_file[i][j].redirect_page = 0;
				break;
			}
		}
	}

	Pic.wdt_left = (u_long)(Pic.uinfo.clock * Pic.wdt_max / 4.0);
	Pic.prescaler = 128;
}

static void
reset_pic16c84( void )
{
	if (Pic.sleeping) {
		set_status( TO, IRP | RP1 | RP0 | PD );
		set_f( 0, 11, 0, False );	// INTCON = 0
	} else {
		set_f( 0, 11, get_f( 11, NULL, NULL ) & 1, False );	// INTCON = 0
		set_status( 0, IRP | RP1 | RP0 );
	}

	Pic.sleeping = 0;
	Pic.pc = 0;
	Pic.wdt_left = (u_long)(Pic.uinfo.clock * Pic.wdt_max / 4.0);
	Pic.prescaler = 128;

	set_f( 1, 1, ~0, False );	// OPTION = ~0
	set_f( 1, 5, ~0, False );	// Set TRIS-A & TRIS-B to 1's
	set_f( 1, 6, ~0, False );
	set_f( 0, 10, 0, False );	// PCLATH = 0
}

static char
status_hook( reg *status, Boolean write, char v )
{
	if (write) {
		Boolean switch_page = ((v ^ status->value) & RP0);
		status->value = v;
		if (switch_page) {
			String		 s = "next";
			Cardinal	 n = 1;
			reg_switch( NULL, NULL, &s, &n );
			instr_refresh( NULL, NULL, NULL, NULL );
		}
	}
	return status->value;
}

static char
port_hook( reg *port, Boolean write, char v )
{
	if (write) {
		Boolean twiddle_bits = (v ^ port->value);
		port->value = v & port->implemented;
		if (twiddle_bits) ic_update( );
	}
	return port->value;
}

/*
 * Common stuff that must be done every time the processor executes a
 * cycle:
 * Increment the Ticks counter
 * Increment the PC & set the PCL register
 * Decrement the WDT, check for timeout
 * Decrement the RTCC, check for interrupt
 *
 * Functions that are going to diddle the PC and just want to call this
 * to get the necessary extra cycle delay pass no_ints == True to avoid
 * getting a WDT timeout or interrupt in the middle of the instruction.
 */
static void
use_a_cycle( Boolean no_ints )
{
	Boolean	 wdt_tick = True;
	Boolean	 rtcc_tick = !(Pic.reg_file[1][1].value & 0x20);
	
	Pic.ticks += 4;

	if (--Pic.millisec_left <= 0) {
		Pic.millisec += 1;
		Pic.millisec_left = (int)(Pic.uinfo.clock / 4000.0);
	}

	Pic.pc += 1;
	if (Pic.pc >= Pic.isize) Pic.pc = 0;

	/*
	 * The prescaler will tick if it's assigned to the WDT or if
	 * the RTCC is running off the internal clock:
	 */
	if (Pic.reg_file[1][1].value & 0x28) {
		if (!no_ints || Pic.prescaler != 1) Pic.prescaler -= 1;
		if (Pic.prescaler > 0) {
			/*
			 * Prescaler hasn't timed out yet ... no tick on whatever
			 * it's connected to.
			 */
			if (Pic.reg_file[1][1].value & 8)
				wdt_tick = False;
			else
				rtcc_tick = False;
		} else {
			/*
			 * Prescaler has timed out ... tick on whatever it's
			 * connected to.
			 */
			if (Pic.reg_file[1][1].value & 8)
				wdt_tick = True;
			else
				rtcc_tick = True;
			/*
			 * Also, the prescaler gets reset.
			 */
			Pic.prescaler = (((Pic.reg_file[1][1].value & 8) ? 1 : 2) <<
							 (Pic.reg_file[1][1].value & 7));
		}
	}

	if (wdt_tick && (Pic.wdt_left || !no_ints)) {
		if (Pic.wdt_left) {
			Pic.wdt_left -= 1;
		} else {
			if (Pic.uinfo.wd_fuse) {
				Pic.pc = 0;		// WDT timeout!
				Pic.sleeping = 0;
				Pic.wdt_left = (u_long)(Pic.uinfo.clock * Pic.wdt_max / 4.0);
				Pic.prescaler = 128;

				set_f( 1, 1, ~0, False );	// OPTION = ~0
				set_f( 1, 5, ~0, False );	// Set TRIS-A & TRIS-B to 1's
				set_f( 1, 6, ~0, False );
				set_f( 0, 10, 0, False );	// PCLATH = 0

				set_status( PD, IRP | RP1 | RP0 | TO );

				info_refresh( NULL, NULL, NULL, NULL );

				if (Pic.running)
					sim_toggle_run( NULL, NULL, NULL, NULL );
			} else {
				Pic.wdt_left = (u_long)(Pic.uinfo.clock * Pic.wdt_max / 4.0);
				if (Pic.reg_file[1][1].value & 8) {
					Pic.prescaler = 1 << (Pic.reg_file[1][1].value & 7);
				}
			}
		}
	}

	if (rtcc_tick && (!no_ints || Pic.reg_file[0][1].value != 1)) {
		Pic.reg_file[0][1].value -= 1;	// RTCC decrements
		Pic.reg_file[0][1].modified = 1;
		// fixme: Check here for RTCC interrupt
	}

	set_f( 0, 2, Pic.pc & 0xff, False );
}

/*
 * Common stuff that must be done every time the processor sleeps a cycle:
 * Decrement the WDT & check for wakeup
 */
static void
sleep_a_cycle( void )
{
	if (--Pic.millisec_left <= 0) {
		Pic.millisec += 1;
		Pic.millisec_left = (int)(Pic.uinfo.clock / 4000.0);
	}

	if (Pic.reg_file[1][1].value & 8) {
		// Prescaler is assigned to WDT
		Pic.prescaler -= 1;
		if (Pic.prescaler > 0) return;
		Pic.prescaler = 1 << (Pic.reg_file[1][1].value & 7);
	}
	
	if (Pic.wdt_left) {
		Pic.wdt_left -= 1;
	} else {
		// WDT timeout during SLEEP
		Pic.sleeping = 0;
		Pic.wdt_left = (u_long)(Pic.uinfo.clock * Pic.wdt_max / 4.0);
		if (Pic.reg_file[1][1].value & 8) {
			Pic.prescaler = 1 << (Pic.reg_file[1][1].value & 7);
		}
		info_refresh( NULL, NULL, NULL, NULL );
		set_status( 0, PD | TO );
		Pic.pc += 1;
		set_f( 0, 2, Pic.pc & 0xff, False );
	}
}

static char
get_f( u_int instr, int *p, int *r )
{
	int	 a, b, c, d;

	a = (Pic.reg_file[0][3].value >> 5) & 1;
	b = instr & 0x7f;

	if (!b) {					// use contents of FSR
		c = (Pic.reg_file[0][4].value >> 7) & 1;
		d = Pic.reg_file[0][4].value;
	} else {
		c = Pic.reg_file[a][b].redirect_page;
		d = Pic.reg_file[a][b].redirect_reg;
	}

	if (p != NULL) *p = c;
	if (r != NULL) *r = d;

	if (d) {
		if (Pic.reg_file[c][d].hook) {
			return Pic.reg_file[c][d].hook( &Pic.reg_file[c][d], False );
		} else {
			return Pic.reg_file[c][d].value & 0xff;
		}
	} else {
		return 0;				// indirect access of F0 returns 0
	}

	return (!d) ? 0 : Pic.reg_file[c][d].value & 0xff;
}

static char
get_w( void )
{
	return Pic.reg_file[0][0].value;
}

/*
 * Construct a 13-bit value from the low 11 bits of the instr and bits <4:3>
 * of PCLATH.  This value is then truncated to fit within the address space.
 */
static int
get_addr( u_int instr )
{
	return (((((int)Pic.reg_file[0][10].value << 8) & 0x1800)
			 | instr & 0x7ff) - 1) & 0x3ff;
}

static void
set_f( int p, int r, char val, Boolean set_z )
{
	if (r) {
		if (Pic.reg_file[p][r].hook) {
			val = Pic.reg_file[p][r].hook( &Pic.reg_file[p][r], True, val );
		} else {
			val =
			Pic.reg_file[p][r].value = val & Pic.reg_file[p][r].implemented;
			Pic.reg_file[p][r].modified = 1;
		}
	}

	// fixme: execute any hooks on this register!

	if (set_z)
		set_status( (!val) ? Z : 0, (val) ? Z : 0 );
}

static void
set_w( char val, Boolean set_z )
{
	Pic.reg_file[0][0].value = val;
	Pic.reg_file[0][0].modified = 1;

	if (set_z)
		set_status( (!val) ? Z : 0, (val) ? Z : 0 );
}

static void
set_status( int set, int clear )
{
	Pic.reg_file[0][3].value |= set;
	Pic.reg_file[0][3].value &= ~clear;
	Pic.reg_file[0][3].modified = 1;
}

static char *
disasm_pic16c84( u_int instr )
{
	static char		 dscr[32];
	int				 i;
	pic16c84_instr	*tmpl;

	if ((i = match_instr( instr )) == -1) return "??????";

	tmpl = PIC16C84_INSTR + i;

	if (tmpl->f) {
		char	 reg[16];
		int		 a;
		int		 b;
		int		 p;
		int		 r;

		a = (Pic.reg_file[0][3].value >> 5) & 1;
		b = instr & 0x7f;
		p = Pic.reg_file[a][b].redirect_page;
		r = Pic.reg_file[a][b].redirect_reg;

		if (!p && !r) {
			strcpy( reg, "@FSR" );
		} else
		if (Pic.reg_file[p][r].name) {
			char *cp = Pic.reg_file[p][r].name;
			while (isspace(*cp)) ++cp;
			strcpy( reg, cp );
		} else
			sprintf( reg, "F%02X", r );
		
		if (tmpl->d)
			sprintf( dscr, "%s %s,%c",
					tmpl->name, reg, (instr & 0x80) ? 'F' : 'W' );
		else
		if (tmpl->b)
			sprintf( dscr, "%s %s,%d",
					tmpl->name, reg, (instr & 0x380) >> 7 );
		else
			sprintf( dscr, "%s %s", tmpl->name, reg );
	} else
		if (tmpl->literal)
			sprintf( dscr,
					(tmpl->literal & 0xff00) ? "%s 0x%04x" : "%s 0x%02x",
					tmpl->name, instr & tmpl->literal );
		else
			strcpy( dscr, tmpl->name );

	if (instr & tmpl->dont_care) {
		char *cp;

		for (cp = dscr; *cp; cp++)
			if (isalpha( *cp )) *cp = tolower( *cp );
	}

	return dscr;
}

/*
 * These routines simulate each individual instruction
 */
static void
exADDWF( void )
{
	int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int	 p, r;
	int	 w, f, sum;
	int	 c, dc;

	w = get_w( ) & 0xff;
	f = get_f( instr, &p, &r ) & 0xff;
	
	sum = w + f;

	if (instr & 0x80)
		set_f( p, r, sum, True );
	else
		set_w( sum, True );

	c = (sum & 0x100) ? C : 0;
	dc = (((w & 0x0f) + (f & 0x0f)) & 0x10) ? DC : 0;

	set_status( c | dc, (c^C) | (dc^DC) );
}
	 
static void
exANDWF( void )
{
	int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int	 p, r;
	int	 w, f, and;
	int	 d, dc;

	w = get_w( );
	f = get_f( instr, &p, &r );
	
	and = w & f;

	if (instr & 0x80)
		set_f( p, r, and, True );
	else
		set_w( and, True );
}

static void
exCLRF( void )
{
	int	 p, r;

	get_f( Pic.uinfo.picmemmap[ Pic.pc ], &p, &r );
	set_f( p, r, 0, True );
}

static void
exCLRW( void )
{
	set_w( 0, True );
}

static void
exCOMF( void )
{
	u_int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int		 p, r;
	char	 v;

	v = get_f( instr, &p, &r );
	if (instr & 0x80)
		set_f( p, r, ~v, True );
	else
		set_w( ~v, True );
}

static void
exDECF( void )
{
	u_int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int		 p, r;
	char	 v;

	v = get_f( instr, &p, &r );
	v -= 1;
	if (instr & 0x80)
		set_f( p, r, v, True );
	else
		set_w( v, True );
}

static void
exDECFSZ( void )
{
	int		 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int		 p, r;
	char	 v;

	v = get_f( instr, &p, &r );
	v -= 1;
	if (instr & 0x80)
		set_f( p, r, v, False );
	else
		set_w( v, False );

	if (v == 0) use_a_cycle( True );
}

static void
exINCF( void )
{
	u_int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int		 p, r;
	char	 v;

	v = get_f( instr, &p, &r );
	v += 1;
	if (instr & 0x80)
		set_f( p, r, v, True );
	else
		set_w( v, True );
}

static void
exINCFSZ( void )
{
	int		 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int		 p, r;
	char	 v;

	v = get_f( instr, &p, &r );
	v += 1;
	if (instr & 0x80)
		set_f( p, r, v, False );
	else
		set_w( v, False );

	if (v == 0) use_a_cycle( True );
}

static void
exIORWF( void )
{
	int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int	 p, r;
	int	 w, f, or;
	int	 c, dc;

	w = get_w( );
	f = get_f( instr, &p, &r );
	
	or = w | f;

	if (instr & 0x80)
		set_f( p, r, or, True );
	else
		set_w( or, True );
}

static void
exMOVF( void )
{
	u_int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int		 p, r;
	char	 v;

	v = get_f( instr, &p, &r );

	if (instr & 0x80)
		set_f( p, r, v, True );
	else
		set_w( v, True );
}

static void
exMOVWF( void )
{
	u_int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int		 p, r;

	get_f( instr, &p, &r );
	set_f( p, r, get_w(), False );
}

static void
exNOP( void )
{
}

static void
exRLF( void )
{
	u_int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int		 p, r;
	char	 v;
	int		 c;

	v = get_f( instr, &p, &r );

	c = (v & 0x80) ? C : 0;
	v = (v << 1) | ((Pic.reg_file[0][3].value & C) ? 1 : 0);

	if (instr & 0x80)
		set_f( p, r, v, False );
	else
		set_w( v, False );

	set_status( c, c ^ C );
}

static void
exRRF( void )
{
	u_int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int		 p, r;
	char	 v;
	int		 c;

	v = get_f( instr, &p, &r );

	c = (v & 1) ? C : 0;
	v = (v >> 1) | ((Pic.reg_file[0][3].value & C) ? 0x80 : 0);

	if (instr & 0x80)
		set_f( p, r, v, False );
	else
		set_w( v, False );

	set_status( c, c ^ C );
}

static void
exSUBWF( void )
{
	int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int	 p, r;
	int	 w, f, dif;
	int	 c, dc;

	w = get_w( ) & 0xff;
	f = get_f( instr, &p, &r ) & 0xff;
	
	dif = f - w;

	if (instr & 0x80)
		set_f( p, r, dif, True );
	else
		set_w( dif, True );

	c = (dif & 0x100) ? C : 0;
	dc = (((f & 0x0f) - (w & 0x0f)) & 0x10) ? DC : 0;

	set_status( c | dc, (c^C) | (dc^DC) );
}

static void
exSWAPF( void )
{
	u_int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int		 p, r;
	char	 v;

	v = get_f( instr, &p, &r );

	v = ((v >> 4) & 0x0f) | (v << 4);
	
	if (instr & 0x80)
		set_f( p, r, v, False );
	else
		set_w( v, False );
}

static void
exXORWF( void )
{
	int			 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int			 p, r;
	int			 w, f, xor;

	w = get_w( );
	f = get_f( instr, &p, &r );
	
	xor = w ^ f;

	if (instr & 0x80)
		set_f( p, r, xor, True );
	else
		set_w( xor, True );
}

static void
exBCF( void )
{
	u_int	 instr = Pic.uinfo.picmemmap[Pic.pc];
	int		 bit = (instr >> 7) & 7;
	int		 p, r;
	char	 v;

	v = get_f( instr, &p, &r ) & ~(1 << bit);
	
	set_f( p, r, v, False );
}

static void
exBSF( void )
{
	u_int	 instr = Pic.uinfo.picmemmap[Pic.pc];
	int		 bit = (instr >> 7) & 7;
	int		 p, r;
	char	 v;

	v = get_f( instr, &p, &r ) | (1 << bit);
	
	set_f( p, r, v, False );
}

static void
exBTFSC( void )
{
	u_int	 instr = Pic.uinfo.picmemmap[Pic.pc];
	int		 bit = (instr >> 7) & 7;

	if (!(get_f( instr ) & (1 << bit))) use_a_cycle( True );
}

static void
exBTFSS( void )
{
	u_int	 instr = Pic.uinfo.picmemmap[Pic.pc];
	int		 bit = (instr >> 7) & 7;

	if (get_f( instr ) & (1 << bit)) use_a_cycle( True );
}

static void
exADDLW( void )
{
	int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int	 w, l, sum;
	int	 c, dc;

	w = get_w( ) & 0xff;
	l = instr & 0xff;
	sum = w + l;

	set_w( sum, True );
	c = (sum & 0x100) ? C : 0;
	dc = (((w & 0x0f) + (l & 0x0f)) & 0x10) ? DC : 0;

	set_status( c | dc, (c^C) | (dc^DC) );
}

static void
exANDLW( void )
{
	int			 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int			 w, l, and;

	w = get_w( );
	l = instr & 0xff;
	and = w & l;

	set_w( and, True );
}

static void
exCALL( void )
{
	int			 instr = Pic.uinfo.picmemmap[ Pic.pc ];

	Pic.stack[Pic.sp] = Pic.pc;
	Pic.sp += 1;
	if (Pic.sp >= Pic.nstack) Pic.sp = 0;

	use_a_cycle( True );

	Pic.pc = get_addr( instr );
}

static void
exCLRWDT( void )
{
	Pic.wdt_left = (u_long)(Pic.uinfo.clock * Pic.wdt_max / 4.0);
	if (Pic.reg_file[1][1].value & 8) {
		Pic.prescaler = 1 << (Pic.reg_file[1][1].value & 7);
	}
	set_status( TO | PD, 0 );
	info_refresh( NULL, NULL, NULL, NULL );
}

static void
exGOTO( void )
{
	int			 instr = Pic.uinfo.picmemmap[ Pic.pc ];

	use_a_cycle( True );
	Pic.pc = get_addr( instr );
}

static void
exIORLW( void )
{
	int			 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int			 w, l, or;

	w = get_w( );
	l = instr & 0xff;
	or = w & l;

	set_w( or, True );
}

static void
exMOVLW( void )
{
	int			 instr = Pic.uinfo.picmemmap[ Pic.pc ];

	set_w( instr & 0xff, False );
}

static void
exRETFIE( void )
{
	int	 gie = get_f( 0x0b );

	set_f( 0, 0x0b, gie | 0x80, False );

	use_a_cycle( True );
	
	Pic.sp -= 1;
	if (Pic.sp < 0) Pic.sp = Pic.nstack - 1;
	Pic.pc = Pic.stack[Pic.sp];
}

static void
exRETLW( void )
{
	int			 instr = Pic.uinfo.picmemmap[ Pic.pc ];

	set_w( instr & 0xff, False );

	use_a_cycle( True );

	Pic.sp -= 1;
	if (Pic.sp < 0) Pic.sp = Pic.nstack - 1;
	Pic.pc = Pic.stack[Pic.sp];
}

static void
exRETURN( void )
{
	use_a_cycle( True );
	
	Pic.sp -= 1;
	if (Pic.sp < 0) Pic.sp = Pic.nstack - 1;
	Pic.pc = Pic.stack[Pic.sp];
}

static void
exSLEEP( void )
{
	exCLRWDT( );
	set_status( TO, PD );
	Pic.sleeping = 1;
	Pic.pc = Pic.pc - 1;
}

static void
exSUBLW( void )
{
	int	 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int	 w, l, dif;
	int	 c, dc;

	w = get_w( ) & 0xff;
	l = instr & 0xff;
	dif = l - w;

	set_w( dif, True );
	c = (dif & 0x100) ? C : 0;
	dc = (((l & 0x0f) - (w & 0x0f)) & 0x10) ? DC : 0;

	set_status( c | dc, (c^C) | (dc^DC) );
}

static void
exXORLW( void )
{
	int			 instr = Pic.uinfo.picmemmap[ Pic.pc ];
	int			 w, l, xor;

	w = get_w( );
	l = instr & 0xff;
	xor = w ^ l;

	set_w( xor, True );
}

static void
exOPTION( void )
{
	set_f( 1, 1, get_w(), False );
}

static void
exTRIS( void )
{
	int			 instr = Pic.uinfo.picmemmap[ Pic.pc ];

	switch (instr & 7) {
	  case 5:
	  case 6:
		set_f( 1, instr & 7, get_w(), False );
		break;
	  default:
		// fixme! what does this do?
		break;
	}
}

static pic16c84_instr
PIC16C84_INSTR[] = {
	{ 0x3f00, 0x0000, 0x0700, 0x0000, 1, 1, 0, "ADDWF ", exADDWF },
	{ 0x3f00, 0x0000, 0x0500, 0x0000, 1, 1, 0, "ANDWF ", exANDWF },
	{ 0x3f80, 0x0000, 0x0180, 0x0000, 0, 1, 0, "CLRF  ", exCLRF },
	{ 0x3f80, 0x007f, 0x0100, 0x0000, 0, 0, 0, "CLRW  ", exCLRW },
	{ 0x3f00, 0x0000, 0x0900, 0x0000, 1, 1, 0, "COMF  ", exCOMF },
	{ 0x3f00, 0x0000, 0x0300, 0x0000, 1, 1, 0, "DECF  ", exDECF },
	{ 0x3f00, 0x0000, 0x0b00, 0x0000, 1, 1, 0, "DECFSZ", exDECFSZ },
	{ 0x3f00, 0x0000, 0x0a00, 0x0000, 1, 1, 0, "INCF  ", exINCF },
	{ 0x3f00, 0x0000, 0x0f00, 0x0000, 1, 1, 0, "INCFSZ", exINCFSZ },
	{ 0x3f00, 0x0000, 0x0400, 0x0000, 1, 1, 0, "IORWF ", exIORWF },
	{ 0x3f00, 0x0000, 0x0800, 0x0000, 1, 1, 0, "MOVF  ", exMOVF },
	{ 0x3f80, 0x0000, 0x0080, 0x0000, 0, 1, 0, "MOVWF ", exMOVWF },
	{ 0x3f9f, 0x0060, 0x0000, 0x0000, 0, 0, 0, "NOP   ", exNOP },
	{ 0x3f00, 0x0000, 0x0d00, 0x0000, 1, 1, 0, "RLF   ", exRLF },
	{ 0x3f00, 0x0000, 0x0c00, 0x0000, 1, 1, 0, "RRF   ", exRRF },
	{ 0x3f00, 0x0000, 0x0200, 0x0000, 1, 1, 0, "SUBWF ", exSUBWF },
	{ 0x3f00, 0x0000, 0x0e00, 0x0000, 1, 1, 0, "SWAPF ", exSWAPF },
	{ 0x3f00, 0x0000, 0x0600, 0x0000, 1, 1, 0, "XORWF ", exXORWF },

	{ 0x3c00, 0x0000, 0x1000, 0x0000, 0, 1, 1, "BCF   ", exBCF },
	{ 0x3c00, 0x0000, 0x1400, 0x0000, 0, 1, 1, "BSF   ", exBSF },
	{ 0x3c00, 0x0000, 0x1800, 0x0000, 0, 1, 1, "BTFSC ", exBTFSC },
	{ 0x3c00, 0x0000, 0x1c00, 0x0000, 0, 1, 1, "BTFSS ", exBTFSS },

	{ 0x3e00, 0x0100, 0x3e00, 0x00ff, 0, 0, 0, "ADDLW ", exADDLW },
	{ 0x3F00, 0x0000, 0x3900, 0x00ff, 0, 0, 0, "ANDLW ", exANDLW },
	{ 0x3800, 0x0000, 0x2000, 0x07ff, 0, 0, 0, "CALL  ", exCALL },
	{ 0x3fff, 0x0000, 0x0064, 0x0000, 0, 0, 0, "CLRWDT", exCLRWDT },
	{ 0x3800, 0x0000, 0x2800, 0x07ff, 0, 0, 0, "GOTO  ", exGOTO },
	{ 0x3f00, 0x0000, 0x3800, 0x00ff, 0, 0, 0, "IORLW ", exIORLW },
	{ 0x3c00, 0x0300, 0x3000, 0x00ff, 0, 0, 0, "MOVLW ", exMOVLW },
	{ 0x3fff, 0x0000, 0x0009, 0x0000, 0, 0, 0, "RETFIE", exRETFIE },
	{ 0x3c00, 0x0300, 0x3400, 0x00ff, 0, 0, 0, "RETLW ", exRETLW },
	{ 0x3fff, 0x0000, 0x0008, 0x0000, 0, 0, 0, "RETURN", exRETURN },
	{ 0x3fff, 0x0000, 0x0063, 0x0000, 0, 0, 0, "SLEEP ", exSLEEP },
	{ 0x3e00, 0x0100, 0x3c00, 0x00ff, 0, 0, 0, "SUBLW ", exSUBLW },
	{ 0x3f00, 0x0000, 0x3a00, 0x00ff, 0, 0, 0, "XORLW ", exXORLW },

	{ 0x3fff, 0x0000, 0x0062, 0x0000, 0, 0, 0, "OPTION", exOPTION },
	{ 0x3ff8, 0x0000, 0x0060, 0x0007, 0, 0, 0, "TRIS  ", exTRIS }
} ;

static int
match_instr( u_int instr )
{
	const int		 ninstr = sizeof(PIC16C84_INSTR)/sizeof(PIC16C84_INSTR[0]);
	int				 i;
	pic16c84_instr	*tmpl;

	for (i = 0; i < ninstr; i++) {
		tmpl = PIC16C84_INSTR + i;
		if ((instr & ~tmpl->dont_care & tmpl->mask) == tmpl->discriminator)
			break;
	}

	return ((i >= ninstr) ? -1 : i);
}
