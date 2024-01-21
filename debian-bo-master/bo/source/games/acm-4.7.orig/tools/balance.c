#include <math.h>
#include <stdio.h>
#include <Vlib.h>

struct balance_data {
	double	weight;	/* weight for this test */
	VPoint	rm;	/* rest main gear location (input) */
	VPoint	rn;	/* rest nose gear location (input) */
	double	cm, cn;	/* rest compression values of each strut */
	double	Gm, Gn; /* strut + tire lengths */
	double	Km, Kn;	/* string constants (output) */
	double  Gpz;	/* the old "grounding point" Z value */
	};

void
balance (s)
struct balance_data *s;
{

	double	theta, cosTheta, sinTheta;
	double	Fmz;

/*
 *  Determine the rest pitch angle of the aircraft body
 */

	theta = - atan2 (s->rn.z - s->rm.z, s->rn.x - s->rm.x);
	cosTheta = cos(theta);
	sinTheta = sin(theta);

	printf ("Theta = %f degrees (positive down)\n", theta * 180.0 / M_PI);

/*
 *  Determine correct rm/rn values
 */
	s->rn.z = s->rn.z - s->Gn - s->cn;
	s->rm.z = s->rm.z - s->Gm - s->cm;

/*
 *  Determine spring constants
 */

	Fmz = (s->weight * cosTheta * s->rn.x) / (s->rm.x - s->rn.x);

	s->Km = - Fmz * s->cm;
	s->Kn = (- Fmz - s->weight * cosTheta) * - s->cn;

/*
 *  Determine the initial grounding point
 */

	s->Gpz = s->rm.x * sinTheta + (s->rm.z + s->Gm + s->cm) * cosTheta;

}

main()
{

	struct balance_data s;
	double cnMax, cmMax;

/*
 *  Wheel contact locations for the aircraft fully loaded at rest.
 */

	VSetPoint (s.rn, 14.0, 0, 6.5);
	VSetPoint (s.rm, -1.0, 0, 6.5);

/*
 *  Gross weight
 */

	s.weight = 24326.0;

/*
 *  Maximum oleo extension lengths
 */

	cnMax = 1.5;
	cmMax = 1.5;

/*
 *  The length of the wheel and lower landing gear strut
 */
	s.Gm = 1.5;
	s.Gn = 1.5;

/*
 *  Rest oleo compression; must be less than cnMax or cmMax; usually about
 *  half the max value.
 */

	s.cm = 1.0;
	s.cn = 1.0;

	printf ("Input:\n");
	printf ("nose   contact = %lf  %lf  %lf\n", s.rn.x, s.rn.y, s.rn.z);
	printf ("main's contact = %lf  %lf  %lf\n", s.rm.x, s.rm.y, s.rm.z);
	printf ("Weight = %lf\n", s.weight);

	balance(&s);

	printf ("\nOutput:\n");
	printf ("rm = %lf,  %lf,  %lf\n", s.rm.x, s.rm.y, s.rm.z);
	printf ("rn = %lf,  %lf,  %lf\n", s.rn.x, s.rn.y, s.rn.z);
	printf ("Km = %lf\n", s.Km);
	printf ("Kn = %lf\n", s.Kn);
	printf ("Grounding point (z) = %lf\n", s.Gpz);
	printf ("\n\"inventory\" form:\n\n");
	printf ("\tRm\t\t{%lg,  %lg,  %lg}\n", s.rm.x, s.rm.y, s.rm.z);
	printf ("\tRn\t\t{%lg,  %lg,  %lg}\n", s.rn.x, s.rn.y, s.rn.z);
	printf ("\tKm\t\t%lg\n", s.Km);
	printf ("\tKn\t\t%lg\n", s.Kn);
	printf ("\tGm\t\t%lg\n", s.Gm);
	printf ("\tGn\t\t%lg\n", s.Gn);
	printf ("\tCmMax\t\t%lg\n", cmMax);
	printf ("\tCnMax\t\t%lg\n", cnMax);
	printf ("\tGroundingPoint\t{0.0, 0.0, %lg}\n", s.Gpz);

	exit (0);
}

