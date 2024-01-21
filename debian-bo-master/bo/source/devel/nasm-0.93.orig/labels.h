/* labels.h  header file for labels.c
 *
 * The Netwide Assembler is copyright (C) 1996 Simon Tatham and
 * Julian Hall. All rights reserved. The software is
 * redistributable under the licence given in the file "Licence"
 * distributed in the NASM archive.
 */

int lookup_label (char *label, long *segment, long *offset);
void define_label (char *label, long segment, long offset,
		   struct ofmt *ofmt, efunc error);
void define_common (char *label, long segment, long size,
		    struct ofmt *ofmt, efunc error);
void define_label_stub (char *label, efunc error);
void declare_as_global (char *label, efunc error);
int init_labels (void);
void cleanup_labels (void);
