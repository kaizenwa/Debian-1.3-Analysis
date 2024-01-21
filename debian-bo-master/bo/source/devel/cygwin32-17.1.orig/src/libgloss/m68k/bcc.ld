STARTUP(crt0.o)
OUTPUT_ARCH(m68k)
/* Uncomment this if you want srecords. This is needed for a.out
 * if you plan to use GDB.
OUTPUT_FORMAT(srec)
 */
SEARCH_DIR(.)
GROUP(-lc -lbcc -lgcc)
__DYNAMIC  =  0;

/*
 * Setup the memory map of the M68332BCC Business Card Computer.
 * stack grows down from high memory.
 *
 * The memory map look like this:
 * +--------------------+ <- low memory
 * | .text              |
 * |        _etext      |
 * |        ctor list   | the ctor and dtor lists are for
 * |        dtor list   | C++ support
 * +--------------------+
 * | .data              | initialized data goes here
 * |        _edata      |
 * +--------------------+
 * | .bss               |
 * |        __bss_start | start of bss, cleared by crt0
 * |        _end        | start of heap, used by sbrk()
 * +--------------------+
 * .                    .
 * .                    .
 * .                    .
 * |        __stack     | top of stack
 * +--------------------+
 */
MEMORY
{
  ram     : ORIGIN = 0x3000, LENGTH = 0xd000
}

/*
 * allocate the stack to be at the top of memory, since the stack
 * grows down
 */

PROVIDE (__stack = 0xd000 - 8);

/*
 * Initalize some symbols to be zero so we can reference them in the
 * crt0 without core dumping. These functions are all optional, but
 * we do this so we can have our crt0 always use them if they exist. 
 * This is so BSPs work better when using the crt0 installed with gcc.
 * We have to initalize them twice, so we cover a.out (which prepends
 * an underscore) and coff object file formats.
 */
PROVIDE (hardware_init_hook = 0);
PROVIDE (_hardware_init_hook = 0);
PROVIDE (software_init_hook = 0);
PROVIDE (_software_init_hook = 0);
/*
 * stick everything in ram (of course)
 */
SECTIONS
{
  .text :
  {
    *(.text)
     __CTOR_LIST__ = .;
    LONG((__CTOR_END__ - __CTOR_LIST__) / 4 - 2)
    *(.ctors)
    LONG(0)
    __CTOR_END__ = .;
    __DTOR_LIST__ = .;
    LONG((__DTOR_END__ - __DTOR_LIST__) / 4 - 2)
    *(.dtors)
     LONG(0)
    __DTOR_END__ = .;
    _etext = .;
    *(.lit)
    *(.shdata)
  } > ram
  .shbss SIZEOF(.text) + ADDR(.text) :
  {
    *(.shbss)
  } > ram
  .data :
  {
    *(.data)
    _edata = .;
  } > ram
  .bss SIZEOF(.data) + ADDR(.data) :
  {
    __bss_start = ALIGN(0x8);
    *(.bss)
    *(COMMON)
    _end = ALIGN(0x8);
    __end = ALIGN(0x8);
  } > ram
  .stab . (NOLOAD) :
  {
    [ .stab ]
  }
  .stabstr . (NOLOAD) :
  {
    [ .stabstr ]
  }
}
