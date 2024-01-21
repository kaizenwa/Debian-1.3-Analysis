/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

#ifdef RCSID
static char *RcsId[] = "@(#)1.46";
#endif

/* vshow.c,v 1.46 1996/05/28 18:53:25 koziol Exp */

/*****************************************************************************
*
* vshow.c
*
*   HDF Vset utility.
*
*   vshow:  dumps out vsets in a hdf file.
*
*   Usage:  vshow file [+|-[n]]
*       the '+' option indicates a full dump
*       the '-' option indicates a full dump with one record per line
*       'n' means only for the nth vdata.
*
*
******************************************************************************/
#if defined __MWERKS__
#include <console.h>
#endif

#include "vg.h"

static int  condensed;

int32       vsdumpfull
            (int32 vs);

int32 fmtbyte
            (char *x);
int32 fmtchar
            (char *x);

int32 fmtint
            (char *x);

int32 fmtfloat
            (char *x);

int32 fmtlong
            (char *x);

int32 fmtshort
            (char *x);

int32 fmtdouble
            (char *x);

int
main(int ac, char **av)
{
    int32       vg, vgt;
    int32       vgotag, vgoref;
    int32       vs;
    int32       vsotag, vsoref;
    HFILEID     f;
    int32       vgid = -1;
    int32       vsid = -1;
    int32       vsno = 0;
    int32       vstag;

    int32       i, t, nvg, n, ne, nv, interlace, vsize;
    int32      *lonevs;         /* array to store refs of all lone vdatas */
    int32       nlone;          /* total number of lone vdatas */

    char        fields[VSFIELDMAX*FIELDNAMELENMAX], vgname[VGNAMELENMAX];
    char        vsname[VSNAMELENMAX];
    char        vgclass[VGNAMELENMAX], vsclass[VSNAMELENMAX];
    const char *name;
    int32       fulldump = 0, start = 1;

#if defined __MWERKS__
    ac = ccommand(&av);
#endif

    if (ac == 3)
        if (av[2][0] == '-' || av[2][0] == '+')
          {
              sscanf(&(av[2][1]), "%ld", &vsno);
              if (vsno == 0)
                {
                    printf("FULL DUMP\n");
                }
              else
                {
                    printf("FULL DUMP on vs#%ld\n", (long) vsno);
                }
              fulldump = 1;
              if (av[2][0] == '+')
                  condensed = 1;
              else
                  condensed = 0;
              start = 2;
          }

    if (ac < 2)
      {
          printf("%s: dumps HDF vsets info from hdf file\n", av[0]);
          printf("usage: %s file [+|-[n]]\n", av[0]);
          printf("\t +  gives full dump of all vdatas.\n");
          printf("\t -  gives full dump of all vdatas one record per line.\n");
          printf("\t n  gives full dump of vdata with id n.\n");
          exit(0);
      }

    if ((f = Hopen(av[1], DFACC_READ, 0)) == FAIL)
        exit(0);

    Vstart(f);
    printf("\nFILE: %s\n", av[1]);

    nvg = 0;
    while ((vgid = Vgetid(f, vgid)) != -1)
      {
          vg = Vattach(f, vgid, "r");
          if (vg == FAIL)
            {
                printf("cannot open vg id=%d\n", (int) vgid);
            }
          Vinquire(vg, &n, vgname);
          vgotag = VQuerytag(vg);
          vgoref = VQueryref(vg);
          Vgetclass(vg, vgclass);
          if (HDstrlen(vgname) == 0)
              HDstrcat(vgname, "NoName");
          printf("\nvg:%d <%d/%d> (%s {%s}) has %d entries:\n",
           (int) nvg, (int) vgotag, (int) vgoref, vgname, vgclass, (int) n);

          for (t = 0; t < Vntagrefs(vg); t++)
            {
                Vgettagref(vg, t, &vstag, &vsid);

                /* ------ V D A T A ---------- */
                if (vstag == VSDESCTAG)
                  {
                      vs = VSattach(f, vsid, "r");

                      if (vs == FAIL)
                        {
                            printf("cannot open vs id=%d\n", (int) vsid);
                            continue;
                        }

                      VSinquire(vs, &nv, &interlace, fields, &vsize, vsname);
                      vsotag = VSQuerytag(vs);
                      vsoref = VSQueryref(vs);
                      if (HDstrlen(vsname) == 0)
                          HDstrcat(vsname, "NoName");
                      VSgetclass(vs, vsclass);
                      printf("  vs:%d <%d/%d> nv=%d i=%d fld [%s] vsize=%d (%s {%s})\n",
                             (int) t, (int) vsotag, (int) vsoref, (int) nv, (int) interlace, fields, (int) vsize, vsname, vsclass);

                      if (fulldump && vsno == 0)
                          vsdumpfull(vs);
                      else if (fulldump && vsno == vsoref)
                          vsdumpfull(vs);

                      VSdetach(vs);
                  }
                else if (vstag == DFTAG_VG)
                  {
                      /* ------ V G R O U P ----- */
                      vgt = Vattach(f, vsid, "r");

                      if (vgt == FAIL)
                        {
                            printf("cannot open vg id=%d\n", (int) vsid);
                            continue;
                        }

                      Vinquire(vgt, &ne, vgname);
                      if (HDstrlen(vgname) == 0)
                          HDstrcat(vgname, "NoName");
                      vgotag = VQuerytag(vgt);
                      vgoref = VQueryref(vgt);
                      Vgetclass(vgt, vgclass);
                      printf("  vg:%d <%d/%d> ne=%d (%s {%s})\n",
                             (int) t, (int) vgotag, (int) vgoref, (int) ne, vgname, vgclass);
                      Vdetach(vgt);
                  }
                else
                  {
                      name = HDgettagsname((uint16) vstag);
                      if (!name)
                          printf("  --:%d <%d/%d> %s\n", (int) t, (int) vstag, (int) vsid, "Unknown Tag");
                      else
                        {
                            printf("  --:%d <%d/%d> %s\n", (int) t, (int) vstag, (int) vsid, name);
                            HDfree(name);
                        } /* end else */
                  }
            }   /* while */

          Vdetach(vg);
          nvg++;

      }     /* while */

    if (nvg == 0)
      {
          printf("No vgroups in this file\n");
      }

    nlone = VSlone(f, NULL, 0);
    if (nlone > 0)
      {

          printf("Lone vdatas:\n");
          if (NULL == (lonevs = (int32 *) HDmalloc(sizeof(int) * nlone)))
            {
                printf("%s: File has %d lone vdatas but ", av[0], (int) nlone);
                printf("cannot alloc lonevs space. Quit.\n");
                exit(0);
            }

          VSlone(f, lonevs, nlone);
          for (i = 0; i < nlone; i++)
            {
                vsid = lonevs[i];
                if (FAIL == (vs = VSattach(f, lonevs[i], "r")))
                  {
                      printf("cannot open vs id=%d\n", (int) vsid);
                      continue;
                  }
                VSinquire(vs, &nv, &interlace, fields, &vsize, vsname);
                if (HDstrlen(vsname) == 0)
                    HDstrcat(vsname, "NoName");
                vsotag = VSQuerytag(vs);
                vsoref = VSQueryref(vs);
                VSgetclass(vs, vsclass);
                printf("L vs:%d <%d/%d> nv=%d i=%d fld [%s] vsize=%d (%s {%s})\n",
                       (int) vsid, (int) vsotag, (int) vsoref, (int) nv, (int) interlace, fields, (int) vsize, vsname, vsclass);
                if (fulldump && vsno == 0)
                    vsdumpfull(vs);
                else if (fulldump && vsno == vsoref)
                    vsdumpfull(vs);
                VSdetach(vs);
            }
          HDfree((VOIDP) lonevs);
      }

    Vend(f);
    Hclose(f);

    return (0);

}   /* main */

static int32 cn = 0;

/* ------------------------------------------------ */
/* printing functions used by vsdumpfull(). */
int32
fmtbyte(char *x)
{
    cn += printf("%02x ", *x);
    return (1);
}

int32
fmtchar(char *x)
{
    cn++;
    putchar(*x);
    return (1);
}

int32
fmtint(char *x)
{
    int         i = 0;
    HDmemcpy(&i, x, sizeof(int32));
    cn += printf("%d", i);
    return (1);
}

int32
fmtfloat(char *x)
{
    float       f = 0;
    HDmemcpy(&f, x, sizeof(float32));
    cn += printf("%f", f);
    return (1);
}

int32
fmtlong(char *x)
{
    long        l = 0;
    HDmemcpy(&l, x, sizeof(int32));
    cn += printf("%ld", l);
    return (1);
}

int32
fmtshort(char *x)
{
    short       s = 0;
    HDmemcpy(&s, x, sizeof(int16));
    cn += printf("%d", s);
    return (1);
}

int32
fmtdouble(char *x)
{
    double      d = 0;
    HDmemcpy(&d, x, sizeof(float64));
    cn += printf("%f", d);
    return (1);
}

#define BUFFER 1000000

/* ------------------------------------------------ */

int32
vsdumpfull(int32 vs)
{
    char        vsname[100], fields[VSFIELDMAX*FIELDNAMELENMAX];
    int32       j, i, t, interlace, nv, vsize;
    uint8      *bb, *b;
    DYN_VWRITELIST *w;
    int32       (*fmtfn[VSFIELDMAX]) (char *);
    int32       off[VSFIELDMAX];
    int32       order[VSFIELDMAX];

    int32       bufsize;        /* size of the buffer we are using */
    int32       chunk;          /* number of rows that will fit in the buffer */
    int32       done;           /* number of rows we have done */
    int32       count;          /* number of rows to do this time through the loop */

    int32       nf;             /* number of fields in this Vdata */

    VSinquire(vs, &nv, &interlace, fields, &vsize, vsname);

    if (nv * vsize > BUFFER)
      {
          bufsize = BUFFER;
          chunk = BUFFER / vsize;
      }
    else
      {
          bufsize = nv * vsize;
          chunk = nv;
      }

    done = 0;
    bb = (uint8 *) HDmalloc(bufsize);
    if (bb == NULL)
      {
          printf("vsdumpfull malloc error\n");
          return (0);
      }

    VSsetfields(vs, fields);

    w = vswritelist(vs);

    nf = w->n;
    for (i = 0; i < w->n; i++)
      {
          printf("%d: fld [%s], type=%d, order=%d\n", (int) i, w->name[i], w->type[i], w->order[i]);

          order[i] = w->order[i];
          off[i] = DFKNTsize(w->type[i] | DFNT_NATIVE);

          switch (w->type[i])
            {

                case DFNT_CHAR:
                case DFNT_UCHAR:
                    fmtfn[i] = fmtchar;
                    break;

                case DFNT_UINT8:
                case DFNT_INT8:
                    fmtfn[i] = fmtbyte;
                    break;

                case DFNT_UINT16:
                case DFNT_INT16:
                    fmtfn[i] = fmtshort;
                    break;

                case DFNT_UINT32:
                case DFNT_INT32:
                    fmtfn[i] = fmtlong;
                    break;

                case DFNT_FLOAT32:
                    fmtfn[i] = fmtfloat;
                    break;

                case DFNT_FLOAT64:
                    fmtfn[i] = fmtdouble;
                    break;

                default:
                    fprintf(stderr, "sorry, type [%d] not supported\n", (int) w->type[i]);
                    break;

            }
      }

    cn = 0;

    done = count = 0;
    while (done != nv)
      {

          /* figure out how many to read this time */
          if ((nv - done) > chunk)
              count = chunk;
          else
              count = nv - done;

          /* read and update bookkeeping */
          VSread(vs, bb, count, interlace);
          done += count;
          b = bb;

          /* print out the data */
          for (j = 0; j < count; j++)
            {
                for (i = 0; i < nf; i++)
                  {
                      for (t = 0; t < order[i]; t++)
                        {
                            (fmtfn[i]) ((char *)b);
                            b += off[i];
                            putchar(' ');
                            cn++;
                        }
                      putchar(' ');
                      cn++;
                  }

                /*
                 * if condensed == TRUE put as many as possible on one line else
                 *   put one record per line
                 */
                if (condensed)
                  {
                      if (cn > 65)
                        {
                            putchar('\n');
                            cn = 0;
                        }
                  }
                else
                  {
                      putchar('\n');
                  }
            }

      }

    /* ============================================ */

    HDfree((VOIDP) bb);
    printf("\n\n");

    return (1);

}   /* vsdumpfull */
/* ------------------------------------- */
