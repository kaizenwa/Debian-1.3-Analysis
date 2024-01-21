#include "includes.h"

#include "debug.h"

static int breakout = 0;
extern char breakout_string[256];
extern char *term_server;
extern int fudge_flow;
extern int byte_shift;

static int rewind_count=0;		/* This is so we can rescan data */
static un_char rewind_buffer[264];

/*
 * Handles the serial side of things.
 *
 * 4 main routines..
 * do_serial_in() is called when the serial port is ready for reading and
 *	the in packet buffer isn't full.
 * do_serial_out() is called when the serial port is available for writing and
 * 	there are packets waiting.
 *
 * Compression is done at this level. I opted for the computational more 
 * expensive method of trying to compress each packet, rewinding the dictionary
 * if it failed.
 *
 * this module sees 4 buffers.
 * serial_in[]	are characters read from modem.
 * serial_out[] are characters waiting to be sent to modem.
 *
 * Note that serial_in() won't be called if link_in() has any characters in it.
 */
/*----------------------------------------------------------------------*/
/* function prototypes for this module */
void do_ack(unsigned int);
void send_ack(unsigned int);
int check_match(int, int);
/* adjust_timeout changes the current timeout value based on Jacobson's */
/* modified version of RFC 793 */
void adjust_timeout(unsigned int rtt, int trans);

/*----------------------------------------------------------------------*/
/* Various ring buffers */
#ifndef titan
struct Buffer serial_in = {0,0,0,0,0}, serial_out = {0,0,0,0,0};
#else
struct Buffer serial_in, serial_out;
#endif
int inhabit_send = 0;

#define PUT_SERIAL(c) add_to_buffer(&serial_out, c)
#define GET_SERIAL() get_from_buffer(&serial_in)

/* Packet information */
struct Packet_out p_out[N_PACKETS];
int p_out_s, p_out_e;
int p_out_num, p_mismatch=0;

struct Packet_in p_in[N_PACKETS+1];
int p_in_e;
int p_in_num;

void serial_init() {
  int i;
	      
  p_out_s =
  p_out_e = 
  p_out_num =
		    
  p_in_e =
  p_in_num = 0;
  
  for (i = 0; i < N_PACKETS;++i) {
    p_out[i].type = -1;
    p_in[i].type = -1;
  }
  add_to_buffer(&serial_in, 0);
  get_from_buffer(&serial_in);
}

/*---------------------------------------------------------------------------*/
/* Takes a byte, and puts it in the serial buffer. */
/* If necessary, it emits escape characters until the byte is valid */
 /* for the serial line. It adds 33.  33 is a generator for */
 /* 0-255. As you can see, it is VERY expensive if there are a lot of */
 /* escaped characters. Hopefully this won't be the case. */

void put_serial(unsigned int a) {
  static int count=0; 
  static int f_c = 1;
  if (fudge_flow && !--f_c) {	/* If we need to generate */
				/* flow control characters, and */
				/* there has been enough */
				/* intervening characters, */
				/* then generate an XON. */
    PUT_SERIAL(17);		/* emit an XON */
    f_c = fudge_flow;		/* and reset the counter. */
  }

  a ^= byte_shift;		/* and shift it to try and avoid */
				/* characters that need escaping. */
  a &= out_mask;
  if (a == breakout_string[count]) {
    count++;
  }else {
    count=0;
  }

  DEBUG_PED(stderr, "%s:o%X\n", term_server, a); /* debugging.. */

  while (escapes[a] || !breakout_string[count+2]) 
		{		/* Ok. While it is a */
				/* character that needs escaping, we */
				/* emit escapes and try again. */
    PUT_SERIAL('^');		/* emit the escape character. */
    a = (a + 33);		/* and pick a new character.*/
				/* 33 is a generator for [0..255] */
    a &= out_mask;
    count=0;
  }
  PUT_SERIAL((un_char) a);		/* Now put the character out. */
}

/* collect stats on the distribution of input characters */
void do_histogram(int h)
{
  static int counter = 0;
  static long hist_gram[256];
  h &= 255;
  ++hist_gram[h];
  ++counter;

  return;
}
  
  

/* Ok. get a byte from the serial in buffer */
/* we handle character escapes here. */
int get_serial(void) {
  unsigned int a;		/* Our byte. */
  static int state = 0;
  
  while (serial_in.size) {
    if (state == 0) {		/* If we aren't in the middle of */
				/* handling and escape.. */
      SANITY(rewind_count < sizeof(rewind_buffer));
      if (rewind_count)
        a = rewind_buffer[--rewind_count];
      else {
        a = GET_SERIAL() & in_mask;		/* then get the next byte. */
        if (a == '^') {		/* Is it an escape?? */
  	  state++;		/* yes, go to escape handling. */
	  breakout=0;
	  continue;
        }
        if (ignores[a]) {		/* This char MUST be line noise */
          noise((int) a);		/* We were told the remote system will */
      	  continue;		/* never generate this character. */
        }
        DEBUG_PED(stderr, "%s:i%X\n", term_server,a); /* For debugging.. */
      }
      return ((int) a ^ byte_shift) & in_mask; /* Ok. Return it modulo the */
					 /* byte shift. As the */
					 /* byte_shift could be larger */
					 /* than the mask, mask it */
					 /* again. */
    }

    a = GET_SERIAL();		/* Ok. escape handling. Get the next */
				/* byte. */
    a &= in_mask;

    if ( a== '^') {		/* Ok! It is an escaped escape. Try again. */
      ++state;
      continue;
    }
    if (ignores[a]) continue;
				/* control junk. else it is line */
				/* noise. Either way, we don't want it. */
    a = (a - 33 * state) ^ byte_shift; /* Ok. Work out what the byte */
				/* should be.. */
    state = 0;			/* and return to normal character */
				/* processing. */
    a &= in_mask;
    DEBUG_PED(stderr, "%s:i%X\n", term_server, a); /* debug */
    return (int) a;		/* and return byte to caller. */
  }
  return -1;			/* Nothing left in buffer. Return EOF */
}

/*---------------------------------------------------------------------------*/
/* Main routines. */

/* This is very horrible. We find the packet that has been waiting for a 
 * ack the longest and send it..... This should be a list structure 
 * But that would be very messy to do. (Where do we add new packets?? )
 * This works fine for now. We will wait and see what sort of cpu time 
 * it uses..
 */
void do_serial_out(int force) {
  int i, j;
  unsigned int l;
				/* port is ready for writing. see */
				/* what's available. */ 

  if ( !p_out_num || serial_out.size || inhabit_send)
    return;			/* Hmm. How did this get called. */
  
				/* Check for timeouts.. */
				/* First we find the packet that has */
				/* been waiting longest. */
  l = p_out_s;
  for ( j = p_out_s, i = 0; i < N_PACKETS;++i, j = ((j+1)%N_PACKETS)) {
    if (p_out[j].type < 0) continue;
    if (p_out[j].timeout < p_out[l].timeout || p_out[l].type < 0)
      l = j;
  }
				/* Then we check to make sure that */
				/* this is longer than the minimum */
				/* packet timeout period and that we */
				/* actually found a packet. */
  if (p_out[l].type < 0)
    return;

  if (current_time < (p_out[l].timeout + packet_timeout))
    return;

				/* And then we send it. */
  if (fudge_flow && p_out[l].timeout)
    put_serial(19);

  put_serial((unsigned) 'A'+p_out[l].type); 
  put_serial((unsigned) p_out[l].len);
  put_serial(l);
  j = update_crc(update_crc(update_crc(0,(unsigned) ('A'+p_out[l].type)),
			    (unsigned) p_out[l].len), l);
  put_serial((unsigned int) j & 255);
  DEBUG_CHECK(stderr, "%s:header check == %x\n", term_server, j&255);
  
  for (j = 0; j < p_out[l].len;++ j)
    put_serial(p_out[l].data[j]);
  j = check_sum(p_out[l].data, j , out_mask);
  DEBUG_CHECK(stderr, "%s:p %d len %d checksum == %x\n", term_server, 
	      l, p_out[l].len , j);
  put_serial((unsigned int) j & 255);
  put_serial((unsigned int) (j >> 8) & 255);
  if (p_out[l].timeout) {
    WARNING(stderr, "\n%s:%s timed out %d at %ld trans %d\n", term_server, 
            remote ? "remote" : "local", l, 
	    current_time - p_out[l].timeout, p_out[l].trans);
    window_size = 1;  /* This allows for a complete recovery. */
  }
  p_out[l].timeout = current_time;
  if (auto_retrain) 
    adjust_timeout(current_time-p_out[l].q_time, p_out[l].trans);
  if (!p_out[l].trans++)
    *p_out[l].queue = (*p_out[l].queue > 0) ? (*p_out[l].queue - 1) : 0;
}

void do_serial_in() {
  int i, j;
  int check;
  static int breakout_last = 0;

  static unsigned int		/* various state flags */
    curr_p_stage = 0,
    curr_p_len = 0,
    curr_p_index = 0, 
    curr_p_type = 0,
    curr_p_num = 0;
  static un_char header[7];
  
  if (p_in_num > N_PACKETS/2 || !serial_in.size)
    return; /* packet window is full */
  
  while (serial_in.size) {  
    DEBUG_STATE(stderr, "%s:d_s_i state %d len %d ind %d type %d num %d\n", 
		term_server, curr_p_stage, curr_p_len, curr_p_index, curr_p_type,
		curr_p_num);  
    switch (curr_p_stage) {
    case -1:	/* This handles breakout strings */
      i = get_serial();
      if ( i < 0) break;

      if ( breakout && (i == (breakout_string[breakout]^byte_shift)) ) {
        breakout_last = breakout++;
        if ( !breakout_string[breakout] ) {
          int k;
	  do_shutdown = 1;
          for(k=0; breakout_string[k] ; ++k) noise(breakout_string[k]);
          noise('\r');
          noise('\n');
        }
        break;
      }
      rewind_buffer[rewind_count++] = i^byte_shift;
      SANITY(rewind_count < sizeof(rewind_buffer));
      while(breakout_last >= 0) {
        rewind_buffer[rewind_count++] =
          breakout_string[breakout_last--]^byte_shift;
        SANITY(rewind_count < sizeof(rewind_buffer));
      }
      breakout = curr_p_stage = 0;
      break;

    case 0:			/* waiting for packet header */
      i = get_serial();
      if ( i < 0) break;

      if ( !breakout_last && i == (breakout_string[breakout++]^byte_shift) ) {
        curr_p_stage = -1;
        break;
      }
      breakout_last = breakout = 0;

      if (i < 'A' || i > ('A'+MAX_TYPE)) {
        noise(i);
        break;
      }

				/* ok. for now we will assume that it */
				/* is a header. */ 
      curr_p_index = 0;
      header[curr_p_index++] = i;
      curr_p_stage = 1;
      break;
    case 1:			/* read header */
      i = get_serial();
      if (i < 0) 
	break;
      header[curr_p_index++] = i;
      if (curr_p_index < 4) 
	break;			/* more to read yet */
				/* Ok. We have the whole header. Check */
				/* the checksum */ 
      if (header[0] == 'E') {	/* Got a superack.  Need to do special error */
  				/* checking for it.  Not a standard packet */
        if (curr_p_index < 7)
          break;
        i = header[1] + header[2] + header[3] + header[4]; 
        if (i != header[5]) {
          DEBUG_CHECK(stderr, "Bad checksum on a superack packet\n");
          curr_p_stage = 0;
          break;
        }
        i = update_crc(update_crc(update_crc(update_crc(update_crc(update_crc(
            0, (unsigned int) header[0] & (unsigned int) in_mask),
               (unsigned int) header[1] & (unsigned int) in_mask),
               (unsigned int) header[2] & (unsigned int) in_mask),
               (unsigned int) header[3] & (unsigned int) in_mask),
               (unsigned int) header[4] & (unsigned int) in_mask),
               (unsigned int) header[5] & (unsigned int) in_mask);
        if (!check_match( i & in_mask, (int) header[6])) {
          DEBUG_CHECK(stderr, "%s:invalid check calc on super ack %x, read %x\n",
                      term_server, i, header[6]);
          curr_p_stage=0;
          break;
        }
        DEBUG_SER(stderr, "%s: got super ack:", term_server);
        for (i=0; i<N_PACKETS; i++) 
          if (header[1+(i / 8)] & (1 << (i % 8))) {
            DEBUG_SER(stderr, "%d ", i);
            do_ack((unsigned int) i);
          }
        DEBUG_SER(stderr, "\n");
        curr_p_stage=0;
        break;
      }
      i = update_crc(update_crc(update_crc(0,
		(unsigned int) header[0]& (unsigned int) in_mask),
		(unsigned int) header[1]& (unsigned int) in_mask),
		(unsigned int) header[2]& (unsigned int) in_mask); 
      if (!check_match( i & in_mask,(int) header[3])) {
	DEBUG_CHECK(stderr, "%s:invalid check calc %x, read %x\n", 
		    term_server, i, header[3]);
				/* checksum failed. treat first byte as noise */
				/* and start again with the next byte */
				/* Since this may just be a talk message or */
				/* such I make the warning optional. */
       
        if (header[2] < N_PACKETS && header[0] == 'D')
          NOTIFY(stderr,"%s:discarding possible ack %o %d %o",
            term_server, header[1], header[2], header[3]); 

        noise((int) header[0]);		/* This byte is noise, the rest is */
					/* Placed back in the rewind buffer */
        for (i=4;i > 1;) {
          rewind_buffer[rewind_count++]=header[--i]^byte_shift;
          SANITY(rewind_count < sizeof(rewind_buffer));
        }
	curr_p_stage = 0;
	break;
      }

				/* ok. checksum was good. see what */
				/* type packet is */ 
      if (header[0] == 'D') {
				/* it is an ack */
				/* we do a more stringent checksum here */
				/* cos bad acks are a real pain in the butt */
	DEBUG_SER(stderr, "%s:got ack %d.\n",term_server,header[1]);
	if (!check_match(((int) header[0] ^
			 (int) header[1]) & (int) in_mask,
			 (int) header[2] & (int) in_mask)) {
	  DEBUG_CHECK(stderr,
		      "%s:d_s_i: secondary. calc %x, read %x\n", 
		      term_server, header[0] ^ header[1], header[2]);
				/* heh heh. bad secondary checksum. */
				/* lose that sucker */ 
	  curr_p_stage = 0;
	  break;
	}
	do_ack(header[1]);
	curr_p_stage = 0;
	break;
      }
				/* see if we want the packet */
      if (!header[1] && remote_term_version >= 20000)
        curr_p_len = 2 + (1 << tok_byte_width_in);
      else	
        curr_p_len = header[1] + 2; /* get checksum as well. */

      i = (N_PACKETS + header[2] - (unsigned)p_in_e) % N_PACKETS;  
      if ( i + i >  N_PACKETS) {  
	DEBUG_SER(stderr, "%s:d_s_i: got old packet %d\n", term_server, i);
				/* an old packet. just lose it */
        WARNING(stderr,"old packet %d received.  try raising your %s timeout.\n",
          i, remote ? "local" : "remote");
	curr_p_num = N_PACKETS;
      }else {
        curr_p_num = header[2] % N_PACKETS;
        if (p_in[curr_p_num].type >= 0) {
  	  DEBUG_SER(stderr, "%s:got dup packet\n", term_server);
				/* Hmm. We think we already have it . */
				/* check it */ 
	  if (header[1] != p_in[curr_p_num].len) {
				/* We have a different length !! */
	    alert("Duplicate packet received with a different lengths\n");
				/* just ignore it for now. Will HAVE */
				/* to be fixed */ 
	  }else {
            WARNING(stderr,"duplicate packet received\n");
				/* discard following data and checksum */
          }
	  curr_p_num = N_PACKETS;
        }
      }
      curr_p_stage = 2;
      curr_p_index = 0;
      if (curr_p_num != N_PACKETS) stat_modem_recv += curr_p_len;
      break;
    case 2:			/* read data */
      i = get_serial();
      if (i < 0)
	break;
      p_in[curr_p_num].data[curr_p_index ++ ] = i;
      if (curr_p_index < curr_p_len) 
	break; /* more to read yet */
				/* all read. now test the checksum */
      j = (p_in[curr_p_num].data[(int) curr_p_len - 1] <<8) +
	p_in[curr_p_num].data[(int) curr_p_len -2] ;
      
      check = check_sum(p_in[curr_p_num].data, (int) curr_p_len-2, in_mask);
      
      if (!check_match(j, check) ) {
	DEBUG_CHECK(stderr, "%s:d_s_i: main calced = %x, read = %x\n", 
		    term_server, check, j);
	
				/* failed checksum . sigh. back to beginning */
	WARNING(stderr,"\nbad packet! garbage follows:\n");
        noise((int) header[0]);		/* This byte is noise, the rest is */
					/* Placed back in the rewind buffer */
        for (i=curr_p_len=0;i > 0;) {
          rewind_buffer[rewind_count++]=p_in[curr_p_num].data[--i]^byte_shift;
          SANITY(rewind_count < sizeof(rewind_buffer));
        }
        for (i=4;i > 1;) {
          rewind_buffer[rewind_count++]=header[--i]^byte_shift;
          SANITY(rewind_count < sizeof(rewind_buffer));
        }
	curr_p_stage = 0;
	break;
      }
      
				/* ok. got a complete packet. */
				/* whack it in list. This is */
				/* difficult. grin. */ 

      send_ack(header[2]);
      if (curr_p_num == N_PACKETS) {
        curr_p_stage = 0;
        break;
      }
      p_in[curr_p_num].type = header[0] - 'A';
      p_in[curr_p_num].len = curr_p_len - 2;
      
      ++p_in_num;
      DEBUG_LINK(stderr, "%s:Packet %d added to in Q\n", term_server, curr_p_num);
      curr_p_stage = 0;
      break;    
    case 3:			/* discard */
      if ((int) curr_p_len <= 0) {	/* sanity check */
	curr_p_stage  = 0;
	break;
      }
      if (get_serial() < 0)
	break;
      if (!--curr_p_len)
	curr_p_stage = 0;
      break;
    case 4:			/* This state is special. This is the */
				/* meta-state used by term to check */
				/* for characters that are getting */
				/* eaten, or to do other things that */
				/* aren't based on 
				packets. */
      i = get_serial();
      if (i < 0) break;

      if (meta_state(i))
	break;
				/* Return to normal packet handling. */
      inhabit_send = 0;
      curr_p_stage = 0;
      break;
    default:
      curr_p_stage = 0;
      break;
    }
  }
}

void do_ack(unsigned int num) {

  DEBUG_FP(stderr, "%s:got ack for packet %d\n", term_server, num);
  num %= N_PACKETS;
  if (p_out[num].type < 0) {
    DEBUG_SER(stderr, "%s:was dup.\n", term_server);
    return;
  }

  stat_modem_ack += p_out[num].len;

  p_out[num].type = -1;

  while (p_out[p_out_e].type < 0 && p_out_num > 0) {
    p_out_e = (p_out_e + 1 ) % N_PACKETS;
    --p_out_num;
  }

	/* In case of timeouts, we need to dequeue all the packets before */
	/* we continue. */
  if(window_size == 1 && ((p_out_e + N_PACKETS) - p_out_num) % N_PACKETS <= 1 ) 
    window_size = window_size_max;
}

void send_ack(unsigned int num) {
				/* yucky eh?? 4 bytes to transmit 1. */
				/* Oh well. I fix it in some other version */ 
				/* lets get it working first. ;) */
  DEBUG_SER(stderr, "%s:sending ack for %d\n", term_server, num);
  put_serial('D');
  put_serial(num);
  put_serial(num ^ 'D');
  put_serial( update_crc(update_crc(update_crc(0, 'D'),
				    num), num ^ 'D'));
}

int check_match(int check, int calc) {
  if (!seven_bit_in)
    return check == calc;
  
  return ((check & 127) == (calc & 127)) &&
    (((check >> 8) & 127) == ((calc>>8) & 127));
}

void adjust_timeout(unsigned int rtt, int trans)
{
  static int A=0;
  static int D=0;
  int Err;

  if (!A) 
    A = packet_timeout/2;
  if (!D)
    D = packet_timeout/8;
  Err = (int)rtt-A;
  A += Err/8;
  D += (abs(Err) - D)/4;
  packet_timeout = A + (4 * D);
  if (trans) 
    packet_timeout+=packet_timeout/2;
}
