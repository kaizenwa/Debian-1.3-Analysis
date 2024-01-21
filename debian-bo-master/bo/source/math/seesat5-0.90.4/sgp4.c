/*
SGP4.C
by Paul S. Hirose, 1991 Oct 19
Orbit prediction model for the SEESAT satellite tracking program.

This file is in the public domain.

300 - 399
*/

/* library functions used in this file:
	atan2 cos sin sqrt
*/

#include "SEESAT.H"	/* the global header */

#if ECOC
extern double atan2(), cos(), sin(), sqrt();
#endif

/* If true, BSTAR will be estimated if a value is not provided in the element
set.  Estimation formula courtesy Ted Molczan. */
#define ESTB 1

/* NORAD velocity code is disabled because it's not used in SEESAT.  Can be
activated by setting VEL nonzero and defining an output struct for the
velocities as follows (struct tag "vector" is defined in SEESAT.H):
struct vector satdot;
*/

#define VEL 0

static double
a, ao, aodp, axn, aycof, ayn, aynl, a1, a3ovk2, beta, betal, betao,
betao2, capu, coef, coef1, cosepw, cosik, cosio, cosnok, cosu, cosuk,
cos2u, c1, c1sq, c2, c3, c4, c5, delm, delmo, delo, delomg, del1, d2,
d3, d4, e, ecose, eeta, elsq, eosq, epw, esine, eta, etasq, omega,
omgcof, omgadf, omgdot, perige, pinvsq, pl, psisq, qoms24, r, rdot,
rdotk, rfdot, rfdotk, rk, sinepw, sinik, sinio, sinmo, sinnok, sinu,
sinuk, sin2u, s4, tcube, temp, tfour, tempa, tempe, templ, temp1,
temp2, temp3, temp4, temp5, temp6, theta2, theta4, tsi, tsince, tsq,
t2cof, t3cof, t4cof, t5cof, u, uk, ux, uy, uz, vx, vy, vz, xhdot1,
xinck, xl, xlcof, xll, xlt, xmcof, xmdf, xmdot, xmp, xmx, xmy, xn,
xnodcf, xnoddf, xnode, xnodek, xnodot, xnodp, x1cof, x1mth2, x1m5th,
x3thm1, x7thm1;

static int i, isimp;

void
sgp4(tsince)
double tsince;
{
	DTEST((300, 1, &iflag));
	FTEST((301, 1, 3, &tsince));

	if (iflag) {
		/* this is first call of sgp4() in prediction run */

		/* Recover original mean motion (xnodp) and semimajor axis
		(aodp) from input elements. */

		a1 = POW(xke / xno, tothrd);
		cosio = cos(xincl);
		theta2 = cosio * cosio;
		x3thm1 = 3. * theta2 - 1.;
		eosq = eo * eo;
		betao2 = 1. - eosq;
		if  (betao2 < 0.0) {
            betao2 = FABS(betao2);
		}
		betao = sqrt(betao2);
		del1 = 1.5 * ck2 * x3thm1 / (a1 * a1 * betao * betao2);
		ao = a1 * (1. - del1 * (.5 * tothrd + del1 * (1. + 134.
		 / 81. * del1)));
		delo = 1.5 * ck2 * x3thm1 / (ao * ao * betao * betao2);
		xnodp = xno / (1. + delo);
		aodp = ao / (1. - delo);

		/* Initialization.  For perigee less than 220 km, the
		"isimp" flag is set and the equations are truncated to linear
		variation in sqrt(a) and quadratic variation in mean anomaly.
		Also, the c3, delta omega, and delta m terms are dropped. */

		if (aodp * (1. - eo) <  220. / xkmper + 1.)
			isimp = 1;
		else
			isimp = 0;

		s4 = s;
		qoms24 = qoms2t;
		perige = (aodp * (1. - eo) - 1.) * xkmper;

		/* For perigee below 156 km, the values of
		s and qoms2t are altered. */
		if (perige < 156.) {
			if (perige > 98.)
				s4 = perige - 78.;
			else
				s4 = 20.;
			qoms24 = POW((120. - s4) / xkmper, 4.);
			s4 = s4 / xkmper + 1.;
		}
		pinvsq = 1. / (aodp * aodp * betao2 * betao2);
		tsi = 1. / (aodp - s4);
		eta = aodp * eo * tsi;
		etasq = eta * eta;
		eeta = eo * eta;
		psisq = FABS(1. - etasq);
		coef = qoms24 * POW(tsi, 4.);
		coef1 = coef / POW(psisq, 3.5);
		c2 = coef1 * xnodp * (aodp * (1. + 1.5 * etasq + eeta *
		 (4. + etasq)) + .75 * ck2 * tsi / psisq * x3thm1 *
		 (8. + 3. * etasq * (8. + etasq)));
#if ESTB
		if (bstar == 0.)	/* field in element set was blank */
			bstar = tothrd * xndt2o / (xno * c2);
		ETEST((305, 1, 2, &bstar));
#endif
		c1 = bstar * c2;
		sinio = sin(xincl);
		a3ovk2 = -xj3 / ck2;
		c3 = coef * tsi * a3ovk2 * xnodp * sinio / eo;
		x1mth2 = 1. - theta2;
		c4 = 2. * xnodp * coef1 * aodp * betao2 * (eta
		 * (2. + .5 * etasq) + eo * (.5 + 2. * etasq) - 2. * ck2
		 * tsi / (aodp * psisq) * (-3. * x3thm1 * (1. - 2. * eeta
		 + etasq * (1.5 -.5 * eeta)) + .75 * x1mth2 * (2. * etasq
		 - eeta * (1. + etasq)) * cos(2. * omegao)));
		c5 = 2. * coef1 * aodp * betao2 * (1. + 2.75 * (etasq + eeta)
		 + eeta * etasq);
		theta4 = theta2 * theta2;
		temp1 = 3. * ck2 * pinvsq * xnodp;
		temp2 = temp1 * ck2 * pinvsq;
		temp3 = 1.25 * ck4 * pinvsq * pinvsq * xnodp;
		xmdot = xnodp + .5 * temp1 * betao * x3thm1 + .0625 * temp2
		 * betao * (13. - 78. * theta2 + 137. * theta4);
		x1m5th = 1. - 5. * theta2;
		omgdot = -.5 * temp1 * x1m5th + .0625 * temp2 * (7. - 114.
		 * theta2 + 395. * theta4) + temp3 * (3. - 36. * theta2
		 + 49. * theta4);
		xhdot1 = -temp1 * cosio;
		xnodot = xhdot1 + (.5 * temp2 * (4. - 19. * theta2) + 2.
		 * temp3 * (3. - 7. * theta2)) * cosio;
		omgcof = bstar * c3 * cos(omegao);
		xmcof = -tothrd * coef * bstar / eeta;
		xnodcf = 3.5 * betao2 * xhdot1 * c1;
		t2cof = 1.5 * c1;
		xlcof = .125 * a3ovk2 * sinio * (3. + 5. * cosio)
		 / (1. + cosio);
		aycof = .25 * a3ovk2 * sinio;
		delmo = POW(1. + eta * cos(xmo), 3.);
		sinmo = sin(xmo);
		x7thm1 = 7. * theta2 - 1.;
		if (!isimp) {
			c1sq = c1 * c1;
			d2 = 4. * aodp * tsi * c1sq;
			temp = d2 * tsi * c1 / 3.;
			d3 = (17. * aodp + s4) * temp;
			d4 = .5 * temp * aodp * tsi
			 * (221. * aodp + 31. * s4) * c1;
			t3cof = d2 + 2. * c1sq;
			t4cof = .25 * (3. * d3 + c1 * (12. * d2 + 10.
			 * c1sq));
			t5cof = .2 * (3. * d4 + 12. * c1 * d3 + 6. * d2 * d2
			 + 15. * c1sq * (2. * d2 + c1sq));
		} iflag = 0;
	}

	/* update for secular gravity and atmospheric drag */

	xmdf = xmo + xmdot * tsince;
	omgadf = omegao + omgdot * tsince;
	xnoddf = xnodeo + xnodot * tsince;
	omega = omgadf;
	xmp = xmdf;
	tsq = tsince * tsince;
	xnode = xnoddf + xnodcf * tsq;
	tempa = 1. - c1 * tsince;
	tempe = bstar * c4 * tsince;
	templ = t2cof * tsq;
	if (!isimp) {
		delomg = omgcof * tsince;
		delm = xmcof * (POW(1. + eta * cos(xmdf), 3.) - delmo);
		temp = delomg + delm;
		xmp = xmdf + temp;
		omega = omgadf - temp;
		tcube = tsq * tsince;
		tfour = tsince * tcube;
		tempa = tempa - d2 * tsq - d3 * tcube - d4 * tfour;
		tempe = tempe + bstar * c5 * (sin(xmp) - sinmo);
		templ = templ + t3cof * tcube + tfour * (t4cof + tsince
		 * t5cof);
	} a = aodp * tempa * tempa;
	e = eo - tempe;
	xl = xmp + omega + xnode + xnodp * templ;
	beta = (1.0 - e * e);
	if  (beta < 0.0) {
        beta = FABS(beta);
	}
	beta = sqrt(beta);
	xn = xke / POW(a, 1.5);

	/* long period periodics */

	axn = e * cos(omega);
	temp = 1. / (a * beta * beta);
	xll = temp * xlcof * axn;
	aynl = temp * aycof;
	xlt = xl + xll;
	ayn = e * sin(omega) + aynl;

	/* solve Kepler's equation */

	capu = fmod2p(xlt - xnode);

	FTEST((302, 3, 3, &xlt, &xnode, &capu));

	temp2 = capu;
	for (i = -10; i; ++i) {
		sinepw = sin(temp2);
		cosepw = cos(temp2);
		temp3 = axn * sinepw;
		temp4 = ayn * cosepw;
		temp5 = axn * cosepw;
		temp6 = ayn * sinepw;
		epw = (capu - temp4 + temp3 - temp2) / (1. - temp5 - temp6)
		 + temp2;
		if (FABS(epw - temp2) <= e6a)
			break;
		temp2 = epw;
	}
	DTEST((306, 1, &i));

	/* short period preliminary quantities */

	ecose = temp5 + temp6;
	esine = temp3 - temp4;
	elsq = axn * axn + ayn * ayn;
	temp = 1. - elsq;
	pl = a * temp;
	r = a * (1. - ecose);
	temp1 = 1. / r;
#if VEL
	rdot = xke * sqrt(a) * esine * temp1;
	rfdot = xke * sqrt(pl) * temp1;
#endif
	temp2 = a * temp1;
	if (temp < 0.0) {
       temp = FABS(temp);
	}
	betal = sqrt(temp);
	temp3 = 1. / (1. + betal);
	cosu = temp2 * (cosepw - axn + ayn * esine * temp3);
	sinu = temp2 * (sinepw - ayn - axn * esine * temp3);
	u = atan2(sinu, cosu);
	sin2u = 2. * sinu * cosu;
	cos2u = 2. * cosu * cosu - 1.;
	temp = 1. / pl;
	temp1 = ck2 * temp;
	temp2 = temp1 * temp;

	/* update for short periodics */

	rk = r * (1. - 1.5 * temp2 * betal * x3thm1) + .5 * temp1 * x1mth2
	 * cos2u;
	uk = u - .25 * temp2 * x7thm1 * sin2u;
	xnodek = xnode + 1.5 * temp2 * cosio * sin2u;
	xinck = xincl + 1.5 * temp2 * cosio * sinio * cos2u;
#if VEL
	rdotk = rdot - xn * temp1 * x1mth2 * sin2u;
	rfdotk = rfdot + xn * temp1 * (x1mth2 * cos2u + 1.5 * x3thm1);
#endif

	/* orientation vectors */

	sinuk = sin(uk);
	cosuk = cos(uk);
	sinik = sin(xinck);
	cosik = cos(xinck);
	sinnok = sin(xnodek);
	cosnok = cos(xnodek);
	xmx = -sinnok * cosik;
	xmy = cosnok * cosik;
	ux = xmx * sinuk + cosnok * cosuk;
	uy = xmy * sinuk + sinnok * cosuk;
	uz = sinik * sinuk;

#if VEL
	vx = xmx * cosuk - cosnok * sinuk;
	vy = xmy * cosuk - sinnok * sinuk;
	vz = sinik * cosuk;
#endif

	/* position */

	sat.x = rk * ux;
	sat.y = rk * uy;
	sat.z = rk * uz;

#if VEL
	satdot.x = rdotk * ux + rfdotk * vx;
	satdot.y = rdotk * uy + rfdotk * vy;
	satdot.z = rdotk * uz + rfdotk * vz;
#endif

	FTEST((303, 3, 6, &sat.x, &sat.y, &sat.z));
	FTEST((304, 3, 6, &satdot.x, &satdot.y, &satdot.z));
}
