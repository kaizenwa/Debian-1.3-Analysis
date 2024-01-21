/*
   DRAT.C
   High level interface routines for Drat (interpreter independent).

   $Id: drat.c,v 1.1 1993/09/02 22:48:38 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include "drat.h"

/* ---------------------------------------------------------------------- */

void UpdateMesh(FullMesh *mesh, int *ireg)
{
  long i;
  long kmax= mesh->mesh.kmax;
  long lmax= mesh->mesh.lmax;
  long klmax= kmax*lmax;
  int *ir= mesh->mesh.ireg;
  int changed= 0;

  /* force consistency between klmax and kmax*lmax */
  mesh->mesh.klmax= klmax;

  if (!ir) {
    /* create the region array now */
    mesh->mesh.ireg= ir= (int *)Dmalloc(sizeof(int)*(klmax+kmax));
    for (i=0 ; i<kmax ; i++) ir[i]= 0;
    for ( ; i<klmax ; i++) {
      if (i%kmax) ir[i]= ireg? ireg[i] : 1;
      else ir[i]= 0;
    }
    for ( ; i<klmax+kmax ; i++) ir[i]= 0;
    changed= 1;

  } else if (ireg) {
    /* copy the ireg array, checking for any changes */
    for (i=kmax ; i<klmax ; i++) {
      if (i%kmax && ir[i]!=ireg[i]) {
	ir[i]= ireg[i];
	changed= 1;
      }
    }
  }

  /* check for symmetry change */
  if (mesh->boundary.zsym != mesh->mesh.zsym) {
    mesh->boundary.zsym= mesh->mesh.zsym;   /* force consistency */
    changed= 1;
  }
  if (!mesh->work) {
    /* initialize boundary portion of FullMesh */
    mesh->boundary.nk= mesh->boundary.nl= mesh->boundary.npoints= 0;
    mesh->boundary.zone= 0;
    mesh->boundary.side= 0;
    mesh->boundary.z= mesh->boundary.r= 0;
    /* initialize workspace */
    mesh->work= (int *)Dmalloc(sizeof(int)*2*(klmax+kmax));
    changed= 1;
  }

  if (!changed) MakeBoundaryZR(&mesh->boundary, 1, &mesh->mesh);
  else FindBoundaryPoints(&mesh->mesh, 0, 1, &mesh->boundary, mesh->work);
}

void DiscardMesh(FullMesh *mesh)
{
  int *ireg= mesh->mesh.ireg;
  int *work= mesh->work;
  mesh->mesh.z= mesh->mesh.r= 0;
  mesh->mesh.ireg= 0;
  mesh->work= 0;
  EraseBoundary(&mesh->boundary);
  Dfree(ireg);
  Dfree(work);
}

/* ---------------------------------------------------------------------- */

void TrackRay(FullMesh *mesh, Ray *ray, double *sLimits, RayPath *path)
{
  EntryPoint *entryList;

  if (mesh->mesh.zsym!=2) {
    entryList= FindEntryPoints(&mesh->boundary, ray);
    RayTrack(&mesh->mesh, entryList, path, sLimits);
    FreeEntryPoints(entryList);

  } else {
    RayTrackS(&mesh->mesh, ray, path, sLimits);
  }
}

/* ---------------------------------------------------------------------- */

static double *work= 0;
static long lwork= 0;

double *IntegWorkspace(long ncuts)
{
  if (lwork<3*(ncuts-1)) {	/* enough workspace? */
    long l= 3*(ncuts-1 + 100);
    IntegClear();
    work= (double *)Dmalloc(sizeof(double)*l);
    lwork= l;
  }
  return work;
}

void IntegClear(void)
{
  double *w= work;
  lwork= 0;
  work= 0;
  if (w) Dfree(w);
}

void IntegFlat(double *opac, double *source,
	       long kxlm, long ngroup, Ray *rays, long nrays,
	       FullMesh *mesh, double *slimits, double *result)
{
  RayPath path;
  double *work;
  long j;

  path.maxcuts= path.ncuts= 0;
  path.zone= path.pt1= path.pt2= 0;
  path.ds= 0;

  for (j=0 ; j<nrays ; j++) {
    /* first, track the current ray */
    TrackRay(mesh, rays, slimits, &path);
    rays++;
    slimits+= 2;

    /* then integrate the transport equation */
    work= IntegWorkspace(path.ncuts);
    FlatSource(opac, source, kxlm, ngroup, &path,
	       result, result+ngroup, work);
    result+= 2*ngroup;
  }

  IntegClear();
  EraseRayPath(&path);
}

void IntegLinear(double *opac, double *source,
		 long kxlm, long ngroup, Ray *rays, long nrays,
		 FullMesh *mesh, double *slimits, double *result)
{
  RayPath path;
  double *work;
  long j;

  path.maxcuts= path.ncuts= 0;
  path.zone= path.pt1= path.pt2= 0;
  path.ds= 0;

  for (j=0 ; j<nrays ; j++) {
    /* first, track the current ray */
    TrackRay(mesh, rays, slimits, &path);
    rays++;
    slimits+= 2;

    /* then integrate the transport equation */
    work= IntegWorkspace(path.ncuts);
    LinearSource(opac, source, kxlm, ngroup, &path,
		 result, result+ngroup, work);
    result+= 2*ngroup;
  }

  IntegClear();
  EraseRayPath(&path);
}

/* ---------------------------------------------------------------------- */

static long *edges= 0;
static void ClearEdges(void);

static void ClearEdges(void)
{
  if (edges) {
    long *e= edges;
    edges= 0;
    Dfree(e);
  }
}

void DoPtCenter(double *opac, double *source,
		long kxlm, long ngroup, FullMesh *mesh,
		long *nomilne, long nedges)
{
  Boundary trimmed, *boundary;
  long khold= mesh->khold;
  long lhold= mesh->lhold;
  long ne= (nedges>0? nedges : 0) + (khold>=0? 1 : 0) + (lhold>=0? 1 : 0);
  double *work;

  ClearEdges();
  if (ne) {
    long j, i= 0;
    edges= (long *)Dmalloc(sizeof(long)*4*ne);

    if (khold>=0) {
      edges[0]= edges[2]= khold;
      edges[1]= 0;
      edges[3]= mesh->mesh.lmax-1;
      i+= 4;
    }
    if (lhold>=0) {
      edges[i+1]= edges[i+3]= lhold;
      edges[i]= 0;
      edges[i+2]= mesh->mesh.kmax-1;
      i+=4;
    }
    for (j=0 ; j<4*nedges ; j+=4) {
      edges[i+j]= nomilne[j];
      edges[i+j+1]= nomilne[j+1];
      edges[i+j+2]= nomilne[j+2];
      edges[i+j+3]= nomilne[j+3];
    }

    TrimBoundary(&trimmed, &mesh->mesh, &mesh->boundary,
		 edges, ne, mesh->work);
    boundary= &trimmed;

  } else {
    boundary= &mesh->boundary;
  }

  /* misuse IntegWorkspace to get at least 4*(klmax+kmax+1) doubles */
  work= IntegWorkspace(1 + (4*(mesh->mesh.klmax+mesh->mesh.kmax+1)+3)/3);
  PtCenterSource(opac, source, kxlm, ngroup, &mesh->mesh, boundary, work);
  IntegClear();

  if (ne) EraseBoundary(&trimmed);
  ClearEdges();
}

/* ---------------------------------------------------------------------- */
