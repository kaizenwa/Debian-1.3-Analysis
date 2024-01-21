!
.IFDEF DEBUG
CFLAGS = $(CFLAGS)/name=as_is/noopt/debug
.ELSE
CFLAGS = $(CFLAGS)/name=as_is
.ENDIF

analog.exe : analog.obj,init.obj,init2.obj,hash.obj,utils.obj,formgen.obj,\
	alias.obj,sscanf.obj,output.obj,output2.obj,output3.obj,analog.opt
    link analog/option

analhea2.h : analhead.h
    set file analhea2.h/noexpire

analog.obj : analog.c,analhea2.h
    cc $(CFLAGS) analog.c

init.obj : init.c,analhea2.h
    cc $(CFLAGS) init.c

init2.obj : init2.c,analhea2.h
    cc $(CFLAGS) init2.c

utils.obj : utils.c,analhea2.h
    cc $(CFLAGS) utils.c

hash.obj : hash.c,analhea2.h
    cc $(CFLAGS) hash.c

formgen.obj : formgen.c,analhea2.h
    cc $(CFLAGS) formgen.c

alias.obj : alias.c,analhea2.h
    if f$getsyi("CPU") .GE. 128 then prefix_all = "/prefix=all"
    cc $(CFLAGS) alias.c 'prefix_all'

sscanf.obj : sscanf.c,analhea2.h
    cc $(CFLAGS) sscanf.c

output.obj : output.c,analhea2.h
    cc $(CFLAGS) output.c

output2.obj : output2.c,analhea2.h
    cc $(CFLAGS) output2.c

output3.obj : output3.c,analhea2.h
    cc $(CFLAGS) output3.c

