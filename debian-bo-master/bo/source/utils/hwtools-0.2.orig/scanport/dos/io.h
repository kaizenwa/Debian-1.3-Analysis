/* io.h -- pseudo linux like <asm/io.h> file */

/* For Turbo C v3 under dos */

#define inb inp
#define outb(v,p) outp((p),(v))
#define cli() asm cli
#define sti() asm sti
#define SLOW_DOWN_IO asm out 0x80,al
#define inb_p(p) inb((p)); SLOW_DOWN_IO
#define outb_p(v,p) outb((v),(p)); SLOW_DOWN_IO
int iopl(int level) { return (level & 0);}      /* To shut up compiler */
