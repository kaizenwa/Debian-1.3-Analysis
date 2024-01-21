#ifndef __EXT_H
#define __EXT_H

char *regex_command (char *filename, char *action, char *drops, int *move_dir);

/* Call it after the user has edited the mc.ext file, 
 * to flush the cached mc.ext file
 */
void flush_extension_file (void);
#endif
