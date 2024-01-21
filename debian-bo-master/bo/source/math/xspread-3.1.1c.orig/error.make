Makefile:325: warning: overriding commands for target `gram.o'
Makefile:311: warning: ignoring old commands for target `gram.o'
cc -O    -I/usr/local/include    -DFUNCPROTO  -c gram.c
sed<gram.y >experres.h -f eres.sed;sed < gram.y > statres.h -f sres.sed
rm -f graphic_main.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    graphic_main.c
rm -f help.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    help.c
rm -f interp.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    interp.c
rm -f lex.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    lex.c
rm -f matrix.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    matrix.c
rm -f plot_XY.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    plot_XY.c
rm -f plot_bar.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    plot_bar.c
rm -f plot_line.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    plot_line.c
rm -f plot_pie.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    plot_pie.c
rm -f plot_stk_bar.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    plot_stk_bar.c
rm -f range.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    range.c
rm -f sc.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    sc.c

uopt: Warning: main: this procedure not optimized because it
      exceeds size threshold; to optimize this procedure, use -Olimit option
      with value >=  733.
rm -f screen.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    screen.c
rm -f scXstuff.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    scXstuff.c
rm -f utils.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    utils.c
rm -f vi.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    vi.c
rm -f vmtbl.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    vmtbl.c
rm -f version.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    version.c
rm -f xmalloc.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    xmalloc.c
rm -f sort.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    sort.c
rm -f search.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    search.c
rm -f color.o
cc -c -O    -I/usr/local/include    -DFUNCPROTO    color.c
rm -f xspread
cc -o xspread cmds.o crypt.o format.o gram.o graphic_main.o help.o interp.o lex.o matrix.o plot_XY.o plot_bar.o plot_line.o plot_pie.o plot_stk_bar.o range.o sc.o screen.o scXstuff.o utils.o vi.o vmtbl.o version.o xmalloc.o sort.o search.o color.o -O   -L/usr/local/lib   -lXext 			 -lX11 -lm 
ld:
Can't open: cmds.o (No such file or directory)
make: *** [xspread] Error 1
