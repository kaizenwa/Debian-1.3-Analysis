CC=gcc
CFLAGS=-g
AS=as
AS86=as86
DD=dd
RM=rm
HD=/dev/hda
FD=/dev/fd0

# Rule for preprocessing asm files
%.s: %.S
	$(CC) $(INCLUDE) -E $< -o $@ || \
		( rm -f $@ ; exit 1 )

#%.o: %.s
#	$(AS86) -0 -a -o $@ $< || ( rm -f $@ ; exit 1 )

#%: %.o
#	$(LD86) -0 -s -o $@ $< || ( rm -f $@ ; exit 1 )

# To get from preprocessed asm files to .b files
%.b: %.s
	$(AS86) -0 -a -b $@ -l $*.out $< || ( rm -f $@ ; exit 1 )

# To extract the image
mbr: mbr.b
	$(DD) if=mbr.b of=mbr bs=1 skip=5 count=512

# The dependancies for this rule must be as written. It creates a hex
# assembler file containing the mbr.
mbrobj.s: bin2asm mbr
	$^ $@

ptable:
	$(DD) if=$(HD) bs=512 count=1

floppy: mbr ptable
	$(DD) if=mbr of=$(FD) bs=512 count=1
	$(DD) if=ptable of=$(FD) bs=2 skip=223 seek=223 count=32

install: mbr
	$(DD) if=mbr of=$(HD) bs=446 count=1
	$(DD) if=mbr of=$(HD) bs=1 count=2 skip=510 seek=510

clean:
	$(RM) -rf *.{o,b,s,out} mbr core mbrobj* bin2asm

.PHONY: clean floppy
