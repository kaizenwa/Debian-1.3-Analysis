typedef unsigned long bfd_vma;

long reloc16_val = 0x132;
long instruction = 0x5e000000;

bfd_vma
bfd_coff_reloc16_get_value (void)
{
  return reloc16_val;
}  
   
long
bfd_get_32(void)
{
  return instruction;
}

void
bfd_put_32(long b)
{
  instruction = b;
}   
   
void
f()
{
  long v = bfd_coff_reloc16_get_value ();
  long o = bfd_get_32 ();
  v = (v & 0x00ffffff) | (o & 0xff000000);
  bfd_put_32 (v);
}

int main()
{
  f();
  if (instruction != 0x5e000132)
    abort ();
  else
    exit (0);
}
