#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>




/*simulation (direct method) of Markov jump processes 
 todo:
 t1: piecewise constant interpolation at C level
*/

SEXP sim_mjp(SEXP nmaxt, SEXP init,SEXP parms, SEXP timerange,  SEXP  jumpfunc, SEXP  ratesfunc, SEXP envir)
/* 
nmaxt simulationen
init Start
comp # Spezies +1
nreac # Reaktionen
ratesfunc Raten ortsabhängig
switchfunc Delta ortsabhängig
exp Exponentialverteilungen
sims Gleichverteilungen
out Ausgabe
*/ 
{ 	int i,ic,ir,jl,k=0,nreac,nreac1,nmax0=INTEGER(nmaxt)[0],nmax1,comp0=LENGTH(init),comp1;
	nmax1=nmax0+1;
	comp1=comp0+1;
	double s,t,sv[comp0],*rout,*rout1,*rinit,*rans,rtmin,rtmax;
	SEXP R_fcall, Rt,ans,nR,outs,out,vinit;
	rtmin=REAL(timerange)[0];
	rtmax=REAL(timerange)[1];
	PROTECT(Rt=allocVector(REALSXP,1));
	REAL(Rt)[1]=rtmin;
	PROTECT(R_fcall = lang4(ratesfunc,Rt,init,parms));   
  PROTECT(ans = eval(R_fcall, envir));  
nreac=LENGTH(ans);
nreac1=nreac+1;
double rv[nreac1];
UNPROTECT(2);
	GetRNGstate();
PROTECT(nR=allocVector(INTSXP,1));
PROTECT(ans=allocVector(REALSXP,comp0));
PROTECT(vinit=allocVector(REALSXP,comp0));
rinit=REAL(vinit);
PROTECT(outs=allocMatrix(REALSXP,nmax1,comp1));
rout=REAL(outs);
	for(ic=0;ic<comp0;ic++){
		rinit[ic]=REAL(init)[ic];};
	rout[0]=rtmin;
	for(ic=0;ic<comp0;ic++){
		rout[nmax1*(ic+1)]=rinit[ic];};
/*for(ir=0;ir<nreac0;ir++) for(ic=0;ic<*comp;ic++) printf("ir:%2d ic:%3d sv:%3e\n",ir, ic,switchvec[ic][ir]);
printf("hallo\n");
for(ir=0;ir<nreac0;ir++) for(ic=0;ic<2;ic++) printf("ir:%2d ic:%3d rt:%3d\n",ir, ic,reactab[ic][ir]);
printf("hallo\n");*/
	for(i=0;i<nmax0;i++){ /*printf("i:%2d nr: %3d\n",i,nreac0);*/
	  PROTECT(R_fcall = lang4(ratesfunc,Rt,vinit,parms));   
  PROTECT(ans = eval(R_fcall, envir));  
	s=0.;rans=REAL(ans);
	for(ir=0;ir<nreac;ir++) {s+=(rans[ir] >0) ? rans[ir] :0.;	
	rv[ir]=s;
	}
UNPROTECT(2);
/*printf("%2d %3e %3e %3e %3e\n",i,rinit[0],rinit[1],rv[0],rv[1]);*/
	if (s>0.)
        {rout[i+1]=rout[i]+exp_rand()/s;
	s*=unif_rand();
	t=0.;	
	for(ir=0;t<s;ir++) {t=rv[ir];};
	INTEGER(nR)[0]=ir;
	REAL(Rt)[0]=rout[i+1];
  PROTECT(R_fcall = lang5(jumpfunc,Rt,vinit,parms,nR));   
  PROTECT(ans = eval(R_fcall, envir));  
	rans=REAL(ans);
	for(ic=0;ic<comp0;ic++){rinit[ic]=rans[ic];
	rout[i+1+nmax1*(ic+1)]=rinit[ic];}
/*printf("%2d \n",ir);*/
UNPROTECT(2);
	} else 
	{k=2;
	break;
	};
	if(rout[i+1]>rtmax) {k=1;break;};
	};
/*printf("%5d %5d\n",i,k);*/
	if (k>=1) { 
	for(ic=1;ic<comp1;ic++){
	rout[i+1+nmax1*ic]=rout[i+nmax1*ic];};
	rout[i+1]=rtmax;
	nmax0=i+1;
	};
PROTECT(out=allocMatrix(REALSXP,nmax0+1,comp1));
rout1=REAL(out);
	for(jl=0;jl<nmax0+1;jl++){
	for(ic=0;ic<comp1;ic++){
		rout1[jl+(nmax0+1)*ic]=rout[jl+nmax1*ic];};};
UNPROTECT(6);
	PutRNGstate();
return(out);
}



