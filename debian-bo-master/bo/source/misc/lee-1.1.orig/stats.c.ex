/* fworld.c
 *                         Copyright (1993)
 *
 *         Jeff Elman.  University of California, San Diego
 *          Rik Belew.  University of California, San Diego
 *      Stefano Nolfi.  Institute of Psychology, Rome.
 *    Filippo Menczer.  University of California, San Diego
 *        Greg Linden.  University of California, San Diego
 *
 *        This software may be redistributed without charge;
 *                 this notice should be preserved.
 */
 
#include "defs.h"


/*
 * called by generati() in populati.c;
 * we save on the results (.dat) file
 * whatever the experiment needs to monitor;
 * format: [tab datum tab datum ... tab datum] 
 */
save_dat(fp)

	FILE	*fp;
{
	/*
	 * EXAMPLE OF USEFUL STATISTICS:
	 * 1. Average energy level over population.
	 * 2. Average age over population.
	 * 3. Average fit = (#offsprings).
	 * 4. Accumulated energy "disappeared" at death.
	 */
	struct	indiv	*ap;
			int	flag = -1;

		float	en_aver;
		float	age_aver;
		float	fit_aver;

	/*
	 * initialization
	 */
	en_aver = 0.0;
	age_aver = 0.0;
	fit_aver = 0.0;

	/*
	 * go through population to collect complex stats
	 */
        while ((ap = getnext_indiv(flag))->next != (struct indiv *) -1)
        {
		en_aver += ap->energy;
		age_aver += (float)ap->lifecycle;
		fit_aver += (float)ap->fit;

                flag=0;
        }

	/*
	 * normalizations
	 */
	en_aver /= (float)pop_size;
	age_aver /= (float)pop_size;
	fit_aver /= (float)pop_size;

	/*
	 * the computed stats for this generation
	 * can finally be saved on the .dat file
	 */
	fprintf(fp, "\t%.2f", en_aver);
	fprintf(fp, "\t%.2f", age_aver);
	fprintf(fp, "\t%.2f", fit_aver);
	fprintf(fp, "\t%.2f", energy_reserve);

}

