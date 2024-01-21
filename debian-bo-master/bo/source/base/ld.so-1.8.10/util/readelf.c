/* adapted from Eric Youngdale's readelf program */

#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <linux/elf.h>

#define ELF32_R_SYM(x) ((x) >> 8)
#define ELF32_R_TYPE(x) ((x) & 0xff)

char *xstrdup(char *);

char *readsoname(FILE *infile)
{
  struct elfhdr *epnt;
  struct elf_phdr *ppnt;
  int i;
  char *header;
  unsigned int dynamic_addr = 0;
  unsigned int dynamic_size = 0;
  int strtab_val = 0;
  int soname_val = 0;
  int loadaddr = -1;
  struct dynamic *dpnt;
  struct stat st;
  char *res = NULL;

  if (fstat(fileno(infile), &st))
    return NULL;
  header = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fileno(infile), 0);
  if (header == (caddr_t)-1)
    return NULL;

  epnt = (struct elfhdr *)header;
  if ((int)(epnt+1) > (int)(header + st.st_size))
    goto skip;

  ppnt = (struct elf_phdr *)&header[epnt->e_phoff];
  if ((int)ppnt < (int)header ||
      (int)(ppnt+epnt->e_phnum) > (int)(header + st.st_size))
    goto skip;

  for(i = 0; i < epnt->e_phnum; i++)
  {
    if (loadaddr == -1 && ppnt->p_type == PT_LOAD) 
      loadaddr = (ppnt->p_vaddr & 0xfffff000) -
	(ppnt->p_offset & 0xfffff000);
    if(ppnt->p_type == 2)
    {
      dynamic_addr = ppnt->p_offset;
      dynamic_size = ppnt->p_filesz;
    };
    ppnt++;
  };
    
  dpnt = (struct dynamic *) &header[dynamic_addr];
  dynamic_size = dynamic_size / sizeof(struct dynamic);
  if ((int)dpnt < (int)header ||
      (int)(dpnt+dynamic_size) > (int)(header + st.st_size))
    goto skip;
  
  for (i = 0; i < dynamic_size; i++)
  {
    if (dpnt->d_tag == DT_STRTAB)
      strtab_val = dpnt->d_un.d_val;
    if (dpnt->d_tag == DT_SONAME)
      soname_val = dpnt->d_un.d_val;
    dpnt++;
  };

  if (!strtab_val || !soname_val)
    goto skip;
  if (soname_val + strtab_val - loadaddr < 0 ||
      soname_val + strtab_val - loadaddr >= st.st_size)
    goto skip;

  res = xstrdup((char *) (header - loadaddr + strtab_val + soname_val));

 skip:
  munmap(header, st.st_size);

  return res;
}

