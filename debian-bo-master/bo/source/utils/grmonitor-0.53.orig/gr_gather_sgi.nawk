#
# Awk script to simulate gr_gather - will work on most boxes with
# SVR4 style ps commands, and will work on others with modifications.
#
#  Run with something like:
#
#   nawk -f /home/michael/gr_gather_sgi.nawk | ./gr_monotor -stdin
#
#  or
#
#   (rsh bambam nawk -f /home/michael/gr_gather_sgi.nawk) | ./gr_monotor -stdin
#
#
BEGIN {

# IRIX 5.3 ps

  ps = "sleep 5; ps -elf"

  for (;;) {

    num = 0 ;

    ps | getline ;

    while (ps | getline) {
      line[++num] = $0 ; 
    }

    print "SGI Gather"    

    print num " 4 CPU Mem RSS Time";

    for (i = 1; i <= num; i++) {

      split(line[i],field) ;
      pid = field[4] ;
      user = field[3] ;
      split(field[10], mem, ":") ;
      msize = mem[1] / 1024 ;
      rss = mem[2] / 1024 ;
      cpu_x = substr(line[i], 76, 7) ;
      split(cpu_x, cpu, ":") ;

#     print line[i] ">" cpu[1] " " cpu[2]
      cpu_secs = cpu[1]*60.0 + cpu[2] ;
      
      print pid "\n" user "\n" cpu_secs / 60 " " msize " " rss " 1" ;

    }
    
    close( ps )
  }
}

