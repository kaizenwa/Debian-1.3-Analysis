#!/bin/sh
set +u

files=$* # "time.h stdlib.h malloc.h stdio.h string.h unistd.h"

for i in $files
do
  name=`echo $i | sed 's/[.]h//g
s-/-_-g'`
  stub_name=stub-"$name".c
  aux_name="$name".aux

  if [ ! -f $aux_name ]; then
    echo Create $aux_name;
    echo "#include <$i>" > tmp.c;
    gcc $CFLAGS -S tmp.c -o /dev/null -aux-info $aux_name;
    rm tmp.c
  else
    echo $aux_name already built;
  fi
  if [ ! -f $stub_name ]; then
    echo Create $stub_name;
    echo '#include "available-stubs.h"' > $stub_name;
    echo >> $stub_name;
    cond_name=`echo $name | tr a-z A-Z`;
    cond_name=HAVE_"$cond_name"_H;
    echo "#ifdef $cond_name" >> $stub_name;
    echo "#include <$i>" >> $stub_name;
    echo '#include "checker_api.h"' >> $stub_name;
    echo >> $stub_name;
    ./mk-stubs -d < $aux_name >> $stub_name; 
    echo >> $stub_name;
    echo "#endif /* $cond_name */" >> $stub_name;
  else
    echo $stub_name already built;
  fi
done
