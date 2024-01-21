#ifdef TEST1
main () {
   int j, i[3];

   i[3] = 0;
   j = i[3];
}

#else

main (int argc) {
   int i[3];

   func(i);
}

func (int *i) {

   int j;

   i += 4;
   *i = 0;
   j = *i;
}

#endif



