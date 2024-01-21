#define NEED_MM    
#include <nlist.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include "machine.h"

struct nlist *symbols;
int symtabfilesize;
int nsymbols;
char *string_table;
int offset;
int fd;

/* Show all the symbol table.  */
void chkr_dump_symtab (void)
{
  int i;
  struct nlist *sp;

  /* From objdump.c (binutils 1.9) */
  printf ("%3s: %4s %5s %4s %8s\n",
	  "#", "type", "other", "desc", "val");
  for (i = 0, sp = symbols; i < nsymbols; i++, sp++)
    {
      printf ("%3d: %4x %5x %4x %8x %s\n",
	      i,
	      sp->n_type & 0xff,
	      sp->n_other & 0xff,
	      sp->n_desc & 0xffff,
	      sp->n_value,
	      sp->n_un.n_name);
    }
}

void
main (int argc, char *argv[])
{
  if (argc != 2)
    {
      printf("Usage: %s file\n", argv[0]);
      exit(1);
    }
    
 /* load into memory the symbol table */
 fd = open(argv[1], O_RDONLY);
 if (fd == -1)
   exit(1);
 offset = lseek(fd, 0, SEEK_END);
 if (offset < 2*sizeof(int))
   exit(1);
 lseek(fd, offset - sizeof(int), SEEK_SET);
 read(fd, &nsymbols, sizeof(int));
 lseek(fd, 0, SEEK_SET);
 symbols = (struct nlist*)(mmap((char*)MM_SYM, offset, PROT_READ,
             MAP_FIXED | MAP_FILE | MAP_PRIVATE, fd, 0));
 string_table = (char*)&symbols[nsymbols];
 if (symbols != (struct nlist*)MM_SYM)
   exit(1);
 close(fd);
 chkr_dump_symtab();
 return;
}
