#define NOTIFY \
    (write_noise < 2) ? 0 : fprintf

#define WARNING \
    (!(write_noise)) ? 0 : fprintf

#define DEBUG_STATE \
    (!(term_debug & 1)) ? 0 : fprintf

#define DEBUG_MAIN \
     (!(term_debug & 2)) ? 0 : fprintf

#define DEBUG_SER \
      (!(term_debug &8)) ? 0 :fprintf


#define DEBUG_CHECK \
       (!(term_debug & 16)) ? 0 : fprintf

#define DEBUG_PED \
	(!(term_debug & 32)) ? 0 : fprintf

#define DEBUG_FP (!(term_debug & 64)) ? 0 : fprintf

#define DEBUG_LINK \
	 (!(term_debug & 128)) ? 0 : fprintf

#define DEBUG_LL \
         (!(term_debug & 256)) ? 0 : fprintf

#define DEBUG_C \
         (!(term_debug & 512)) ? 0 : fprintf

#define DEBUG_SEV \
         (!(term_debug & 1024)) ? 0 : fprintf

#define DEBUG_UDP \
	(!(term_debug & 2048)) ? 0 : fprintf

