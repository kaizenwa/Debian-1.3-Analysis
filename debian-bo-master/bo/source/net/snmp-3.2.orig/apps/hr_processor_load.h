/*
 * Linux specific code to maintain and compute the hrProcessorLoad
 * MIB value.
 *
 * Copyright (C) Patrick Weemeeuw, 1996.
 *
 */

#ifndef HR_PROCESSOR_LOAD_H

#define HR_PROCESSOR_LOAD_H

extern void init_HrProcessorLoad(); /* initialise data structures,
				      adds signal handler
				    */
extern int get_HrProcessorLoad(); /* return current load */

#endif
