#!/bin/sh

# for l in \ 

/bin/ls $1 | \
gawk 'BEGIN {srand(systime())} {print int(rand()*10000) " \"" $0 "\""}' \
| sort \
| awk '{a=sprintf("%s %s %s %s %s %s %s",$2,$3,$4,$5,$6,$7,$8)
        system(sprintf("./splay %s",a))}'

# do ls $l; done

