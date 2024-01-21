/* net.c
 *                         Copyright (1992)
 *
 *         Jeff Elman.  University of California, San Diego
 *          Rik Belew.  University of California, San Diego
 *      Stefano Nolfi.  Institute of Psychology, Rome.
 *    Filippo Menczer.  University of California, San Diego
 *
 *        This software may be redistributed without charge;
 *                 this notice should be preserved.
 */


#include "defs.h"



/*
 ***********************************************************************
 * build_geno_net()
 * build_pheno_net()
 * init_net()
 ***********************************************************************
 * Build a network to specification.
 * We are handed:
 *	- the number of layers,
 *	- the number of elements in each layer, and
 * 	- the bounds for random initialization of weights
 * We do:
 *	- create layer pointers and elements
 *	- initialize elements to 0
 *	- create inter-layer weights
 *	- initialize weights to some random value
 */

build_geno_net(ip)

	struct	indiv *ip;
{
	register int i;
	int	num;
	float	**wp;
	float	**bp;
	int	*ldp;

	/*
	 * allocate space for backup bias pointers
	 */
	ip->iibiasp = (float **) malloc(nlayers * sizeof(float *));
	if (ip->iibiasp == NIL_POINTER)
              {
		perror("ip->iibiasp malloc");
		exit(1);
	      }


	/*
	 * allocate space for backup biases
	 */
	for (i=0, bp=ip->iibiasp, ldp=layer_descp; i<nlayers; i++, bp++, ldp++)
              {
		*bp = (float *)malloc(*ldp * sizeof(float));
		if (*bp == NIL_POINTER)
                      {
			perror("bp malloc");
			exit(1);
		      }
	       }
	/*
	 * allocate space for backup weight pointers
	 */
	ip->iiweightp =(float **) malloc((nlayers-1) * sizeof(float *));
	if (ip->iiweightp == NIL_POINTER)
               {
		perror("ip->iiweightp malloc");
		exit(1);
	       }
	/*
	 * ...and for actual backup weights
	 * to each unit in layer below)
	 */
	for (i=0,wp=ip->iiweightp, ldp=(layer_descp+1); i<(nlayers-1); i++,wp++)
               {
		num = *(ldp + i) * *(ldp + i -1);
		*wp = (float *)malloc(num * sizeof(float));
		if (*wp == NIL_POINTER)
                      {
			perror("wp malloc");
			exit(1);
		      }
	        }
}








build_pheno_net(ip)

	struct	indiv *ip;
{
	register int i;
	int	num;
	float	**wp;
	float	**lp;
	float	**dp;
	float	**bp;
	int	*ldp;

	/*
	 * allocate space for layer pointers
	 */
	ip->layerp = (float **) malloc(nlayers * sizeof(float *));
	if (ip->layerp == NIL_POINTER)
              {
		perror("ip->layerp malloc");
		exit(1);
	      }
	/*
	 * allocate space for layers
	 */
	for (i=0, lp=ip->layerp, ldp=layer_descp; i<nlayers; i++, lp++, ldp++)
      {
		/*
		 * if our input contains a context copied in from hidden
		 * units, we will allocate space for the additional units
		 * (Note that the first layer may not be used; we often
		 * just point the ip->layerp toward where the input token is
		 * stored.).
		 */
		*lp = (float *)malloc(*ldp * sizeof(float));
		if (*lp == NIL_POINTER)
                       {
			perror("lp malloc");
			exit(1);
		       }
	      }
	/*
	 * allocate space for bias pointers
	 */
	ip->biasp = (float **) malloc(nlayers * sizeof(float *));
	if (ip->biasp == NIL_POINTER)
              {
		perror("ip->biasp malloc");
		exit(1);
	      }
	/*
	 * allocate space for biases
	 */
	for (i=0, bp=ip->biasp, ldp=layer_descp; i<nlayers; i++, bp++, ldp++)
              {
		*bp = (float *)malloc(*ldp * sizeof(float));
		if (*bp == NIL_POINTER)
                      {
			perror("bp malloc");
			exit(1);
		      }
	       }
	/*
	 * allocate space for weight pointers
	 */
	ip->weightp =(float **) malloc((nlayers-1) * sizeof(float *));
	if (ip->weightp == NIL_POINTER)
               {
		perror("ip->weightp malloc");
		exit(1);
	       }
	/*
	 * ...and for actual weights (each unit in a given layer connecting
	 * to each unit in layer below)
	 */
	for (i=0,wp=ip->weightp, ldp=(layer_descp+1); i<(nlayers-1); i++,wp++)
               {
		num = *(ldp + i) * *(ldp + i -1);
		*wp = (float *)malloc(num * sizeof(float));
		if (*wp == NIL_POINTER)
                      {
			perror("wp malloc");
			exit(1);
		      }
	        }
	/*
	 * allocate space for delta pointers
	 */
	ip->deltap = (float **) malloc(nlayers * sizeof(float *));
	if (ip->deltap == NIL_POINTER)
	    {
	     printf("ERROR : deltap malloc");
	     exit(1);
	    }
	/*
	 * allocate space for deltas
	 */
	for (i=0, dp=ip->deltap, ldp=layer_descp; i<nlayers; i++, dp++, ldp++)
	    {
	     *dp = (float *)malloc(*ldp * sizeof(float));
	     if (*dp == NIL_POINTER)
		{
		 printf("ERROR : dp malloc");
		 exit(1);
		}
	    }		

}

init_net(ip)

	struct	indiv *ip;
{
	register int i;
	register int j;
	int	num;
	float	**wp;
	float	**iiwp;
	float	**bp;
	float	**iibp;
	int	*ldp;
        float   *w;
        float   *iiw;
        float   *b;
        float   *iib;

	/*
	 * initialize biases
	 */
	for (i=0, bp=ip->biasp, iibp=ip->iibiasp, ldp=layer_descp; i<nlayers; i++, bp++
, ldp++,iibp++)
               {
		for (j=0, b= *bp, iib= *iibp; j < *ldp; j++, b++,iib++)
                      {
			*b = 0.0;
                        *iib = *b;
		       }
	        }
	/*
	 * initialize weights randomly
	 */
	for (i=0,wp=ip->weightp,iiwp=ip->iiweightp,ldp=(layer_descp+1); i<(nlayers-1);
i++, wp++,iiwp++)
               {
		num = *(ldp + i) * *(ldp + i - 1);
		for (j=0, w = *wp, iiw = *iiwp;  j < num ; j++, w++, iiw++)
                      {
			*w = rans(weight_limit);
                        *iiw = *w;
		      }
	        }

}

destroy_network(ip)

	struct	indiv *ip;
{
	register int i;
	float	**wp;
	float	**lp;
	float	**dp;
	float	**bp;

	

        /*
         * we free biases and pointers (geno and pheno)
         */
	for (i=0, bp=ip->biasp; i<nlayers; i++, bp++)
               {
		 if (*bp)
                    {
		       free((char *)*bp);
		    }
	       }
	if (ip->biasp)
               {
		  free((char *)ip->biasp);
	       }
	for (i=0, bp=ip->iibiasp; i<nlayers; i++, bp++)
               {
		 if (*bp)
                    {
		       free((char *)*bp);
		    }
	       }
	if (ip->iibiasp)
               {
		  free((char *)ip->iibiasp);
	       }
        /*
         * we free weights and pointers (geno and pheno)
         */
	 for (i=0, wp=ip->weightp; i<(nlayers-1); i++,wp++)
                {
		  if (*wp)
                       {
			 free((char *)*wp);
		       }
	        }
	 if (ip->weightp)
                {
		  free((char *)ip->weightp);
	        }
	 for (i=0, wp=ip->iiweightp; i<(nlayers-1); i++,wp++)
                {
		  if (*wp)
                       {
			 free((char *)*wp);
		       }
	        }
	 if (ip->iiweightp)
                {
		  free((char *)ip->iiweightp);
	        }
        /*
         * we free layers and pointers
         */
	 for (i=0, lp=ip->layerp; i<nlayers; i++, lp++)
                {
		   if (*lp)
                       {
			 free((char *)*lp);
		        }
	        }
	 if (ip->layerp)
		    free((char *)ip->layerp);
        /*
         * we free deltas and pointers
         */
	 for (i=0, dp=ip->deltap; i<nlayers; i++, dp++)
                {
		   if (*dp)
                       {
			 free((char *)*dp);
		        }
	        }
	 if (ip->deltap)
		    free((char *)ip->deltap);

	 ip->layerp = ip->deltap = 0;
	 ip->weightp = ip->biasp = 0;
	 ip->iiweightp = ip->iibiasp = 0;
}

/*
 ***********************************************************************
 * activate_net
 ***********************************************************************
 */
activate_net(ip)

       struct indiv *ip;

{
	register int i;
	float **wp;
	float **bp;
	float **lp;
	int    *ldp;

	wp = ip->weightp;
	lp = ip->layerp;
	bp = ip->biasp;
	ldp = layer_descp;

	for (i = 0; i < (nlayers - 1); i++)
	     {
	       act(*lp, *ldp, *(lp + 1), *(ldp + 1), *wp, *(bp + 1));
	       ldp++;
	       lp++;
	       wp++;
	       bp++;
	      }
	
}


/*
 * Allow units in one layer to activate the next one. Arguments:
 * sending layer pointer, sending layer size, receving layer pointer,
 * receiving layer size, weights pointer, receiving layer bias pointer
 */

act(b_actp, b_size, t_actp, t_size, t_weightp, t_biasp)
	float	*b_actp;
	int	b_size;
	float	*t_actp;
	int	t_size;
	register float *t_weightp;
	float	*t_biasp;
{
	register float *bap;
	register float *endp;
	register float net;
	register int t;

	/*
	 * Activate Propagation Up Network
	 */
	endp = b_actp + b_size;
	for (t = 0; t < t_size; t++)
               {
		net = 0.0;
		bap = b_actp;
		while (bap < endp)
                      {
			net += *t_weightp++ * *bap++;
		      }
		net += *t_biasp;
		if (net < -10.0)
			*t_actp = 0.0;
	        else if (net > 10.0)
			*t_actp = 1.0;
		else
			*t_actp = logistic(net);
		t_actp++;
		t_biasp++;
	      }
}




/*
 * load in memory the sigmoid table 
 * (only for positive values): it takes ~40K
 */

void
carica_sigmoide()
{
 int i;

 for (i=0;i<=NSIGMA;i++)
   {
    sigmoide[i] = (1.0 / (1.0 + exp(-((float)i / LOG_RES))));
   }
}



/*
 * (unused) standard logistic function: logistic() used instead
 */
float
clogistic(f)

  float f;

{
    return(1.0 / (1.0 + exp(-f)));
}

/*
 * logistic functions: uses values stored in sigmoid table
 */
float
logistic(neti)

   float neti;
{
   int i;

   i = (int)(neti * LOG_RES);
   if (i >=0)
      return(sigmoide[i]);
   else
      return(1.0 - sigmoide[-i]);
}


/*
 ***********************************************************************
 * update_net
 ***********************************************************************
 */


/*
 *  update weights
 */
update_net(ip,teachp,learnflag)

	struct indiv *ip;
	float  *teachp;
	boolean learnflag;
{
	float  **lp;
	float  **wp;
	float  **dp;
	float  **bp;
	int    *ldpin, *ldp;
	register int i;
	int     nl;


	 nl = nlayers - 1;
	 /*
	  * do layers in reverse; output layer is done differently
	  */
	  ldpin = layer_descp;
	  ldp = layer_descp + nl;
	  lp = ip->layerp + nl;
	  dp = ip->deltap + nl;
	  wp = ip->weightp + (nl - 1);
	  delta_out(*lp, *ldpin, *ldp, teachp, *dp);
	  if (!learnflag)
	     return;
	   /*
	    * do the rest for nl-1 (==nlayers-2);  we only have
	    * (nlayers-1) connections to change, and we've already done
	    * top set
	    */
	    for (i = 0; i < (nl - 1); i++)
	     {
	        delta(*ldp, *dp, *wp, *(lp - 1), *(ldp - 1), *(dp - 1));
		lp--;
		ldp--;
		dp--;
		wp--;
	      }
	    /*
	     * go back and update the weights and biases, given the deltas
	     * we just computed
	     */
	     ldp = layer_descp + nl;
	     lp = ip->layerp + nl;
	     wp = ip->weightp + (nl - 1);
	     dp = ip->deltap + nl;
	     bp = ip->biasp + nl;
	     for (i = 0; i < nl; i++)
	      {
	       upd_weight(*ldp, *dp, *wp, *bp, *(lp - 1), *(ldp - 1));
	       ldp--;
	       dp--;
	       wp--;
	       bp--;
	       lp--;
	      }
	
}




/*
 * update weights
 * for each weights layer it takes :
 * the size of the receiving layer, the delta of the receiving layer,
 * the weights layer, the bias layer
 * the sending units layer and the sending units layer size.
 */

upd_weight(t_size, t_deltap, t_weightp, t_biasp, b_actp, b_size)

	int	t_size;
	register float *t_deltap;
	register float *t_weightp;
	float	*t_biasp;
	float	*b_actp;
	int	b_size;
{
	register float *bap;
	register float *endp;
	register float r_rate = rate;
	register float dp_rate;
	register int t;

	endp = b_actp + b_size;
	for (t=0; t<t_size; t++)
	    {
	      bap = b_actp;
	      dp_rate = *t_deltap++ * r_rate;
	      while (bap < endp)
		{
		   *t_weightp++ += *bap++ * dp_rate;
		}
	      *t_biasp++ += dp_rate;
            }

}







/*
 * computes error and deltas of the output layer
 * it takes the output layer, the size of input/output layers,
 * the teaching input pointer and the delta of the last layer
 */
delta_out(outp, insize, outsize, tarp, outdeltap)
	register float *outp;
	register int insize;
	register int outsize;
	register float *tarp;
	register float *outdeltap;
{
	register int i;
	register float out;
	register float diff;

	global_error = 0;

/********************************************
 * RIK: when the bug correction is moved
 *      from here to update_net(), the
 *      following is the original code...
 ********************************************

	for (i=0; i<outsize; i++)
	    {
	     out = *outp;
	     diff = (*tarp - out);
	     if (*tarp <= -999999999.0)
			diff = 0.0;
	     *outdeltap = diff * out * (1. - out);
	     global_error += diff*diff;
	     outdeltap++;
	     outp++;
	     tarp++;
	    }

***********************************************
*	while the following is the 
*	temporary hack substituted for it...
*	note: outp is incremented in order to
*		align teaching pattern 
*		and prediction output units
***********************************************/

	for (i=insize; i<outsize; i++)
	{
		*outdeltap = 0.0;
		outdeltap++;
	     	outp++;
	}
	for (i=0; i<insize; i++)
	{
		out = *outp;
	     	diff = (*tarp - out);
	     	if (*tarp <= -999999999.0)
			diff = 0.0;
	     	*outdeltap = diff * out * (1.0 - out);
	     	global_error += diff*diff;
	     	outdeltap++;
	     	outp++;
	     	tarp++;
	}

/************************************************
 *    END OF HACK - Fil
 ************************************************/
}






/*
 * computes delta for all the layer but the output.
 * for each layer of weights it takes :
 *  the size of units of the receiving layer, deltas of the receiving layer,
 *  the layer weights, the units of the sending layer, the size of the
 *  sending layer and delta of the sending layer.
 */
delta(t_size, t_deltap, t_weightp, b_actp, b_size, b_deltap)
	register int t_size;
	register float *t_deltap;
	register float *t_weightp;
	register float *b_actp;
	register int b_size;
	float	*b_deltap;
{
	register float tmp;
	register float *tdp;
	register int t;
	register int b;

	for (b=0; b<b_size; b++)
	    {
	      tmp = 0;
	      for (t=0, tdp=t_deltap; t<t_size; t++, tdp++)
		  {
		    tmp += *tdp * *(t_weightp + (t*b_size) + b);
		  }
	      *b_deltap = (1.-*b_actp) * *b_actp * tmp;
	      b_deltap++;
	      b_actp++;
	   }
}







/*
 * print input and output activation values
 */


print_out(ap,cycle)

      struct indiv *ap;
      int cycle;

{

      int i;
      float **lp;
      float *l;

      lp = ap->layerp;
      printf("cycle: %d ",cycle);
      printf("in: ");
      for(i=0,l = *lp;i < *layer_descp;i++,l++)
        {
           printf("%.2f ",*l);
        }
      lp = ap->layerp;
      lp = (lp + (nlayers - 1));
      printf("ou: ");
      for(i=0,l = *lp;i < *(layer_descp+nlayers-1);i++,l++)
        {
           printf("%.2f ",*l);
        }
      printf("\n");

}


