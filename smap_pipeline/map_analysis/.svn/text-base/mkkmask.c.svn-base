/*
** mymkkmask.c
** 
** Made by (alexandre amblard)
** Login   <amblard@ratiocination.ps.uci.edu>
** 
** Started on  Tue Jun 23 13:24:19 2009 alexandre amblard
** Last update Sun May 12 01:17:25 2002 Speed Blue
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include "idl_export.h"

//need to be 4 bytes integer
#define IDL_LONG int


#ifdef _OMP_
#include <omp.h>
#endif
#ifdef _MPI_
#include "mpi.h"
#endif



int main(int argc,char * argv[]) {

   
  IDL_LONG * R;
  IDL_LONG * XR; 
  IDL_LONG * YR;
  double * spec; 
  double * Mkk;
  IDL_LONG nk; 
  IDL_LONG nr; 
  IDL_LONG nspec;
  FILE * myfile;
  long i,j,k,l,nind1,nind2,ind1,ind2,ind;
#ifdef _MPI_
  long *beg,*temp1,*temp2;
  long long cnt,tot,tot2;
  int  numProc, myRank;
  double * Mkkout;
  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD, &numProc);
  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  /*  printf("num %i init done\n",myRank);*/
#endif

  /* It seems long with icc are 8 bytes, at least on 64bits system*/
  /* int are 4 and IDL_LONG are 4*/

  myfile = fopen(argv[1],"r");
  fread(&nk,sizeof(IDL_LONG),1,myfile);
  fread(&nr,sizeof(IDL_LONG),1,myfile);
  fread(&nspec,sizeof(IDL_LONG),1,myfile);

  R = (IDL_LONG*) malloc(sizeof(IDL_LONG)*nr);
  XR = (IDL_LONG*) malloc(sizeof(IDL_LONG)*nr);
  YR = (IDL_LONG*) malloc(sizeof(IDL_LONG)*nr);
  spec = (double*) malloc(sizeof(double)*nspec*nspec);
  Mkk = (double*) malloc(sizeof(double)*nk*nk);
  memset(Mkk,0.,nk*nk*sizeof(double));

#ifdef _MPI_
  /*  printf("num %i read some\n",myRank);*/
  if (myRank == 0) {
    Mkkout = (double*) malloc(sizeof(double)*nk*nk);
    memset(Mkkout,0.,nk*nk*sizeof(double));
  }
#endif
 

#ifdef _MPI_
  /*  printf("idl %i %i MPI %i %i %i\n",sizeof(IDL_LONG),sizeof(double),sizeof(MPI_BYTE),sizeof(MPI_FLOAT),MPI_INT);*/
  if (myRank ==0) {
#endif
  fread(R,sizeof(IDL_LONG),(int)nr,myfile);
  fread(XR,sizeof(IDL_LONG),(int)nr,myfile);
  fread(YR,sizeof(IDL_LONG),(int)nr,myfile);
  fread(spec,sizeof(double),(int)nspec*nspec,myfile);
#ifdef _MPI_
  }
  /*printf("num %i proc 0 read now pass\n");*/
  MPI_Bcast(R,nr,MPI_INT,0,MPI_COMM_WORLD);
  MPI_Bcast(XR,nr,MPI_INT,0,MPI_COMM_WORLD);
  MPI_Bcast(YR,nr,MPI_INT,0,MPI_COMM_WORLD);
  MPI_Bcast(spec,nspec*nspec,MPI_DOUBLE,0,MPI_COMM_WORLD);
  MPI_Barrier(MPI_COMM_WORLD);
  /*printf("num %i pass done\n",myRank);*/
#endif

  fclose(myfile);

  /*printf("%li %li %li\n",nr,nk,nspec);*/
#ifdef _MPI_
  /*printf("num %i start load\n",myRank);*/
  /* Compute the load to distribute*/
  temp1 = (long*) malloc(sizeof(long)*nk);
  temp2 = (long*) malloc(sizeof(long)*nk);
  beg = (long*) malloc(sizeof(long)*(numProc+1));
  for (i=0;i<nk;i++) 
    temp1[i]=R[i+1]-R[i];
  for (i=0;i<nk;i++)
    temp2[i]=temp1[i];
  for (i=nk-2;i>=0;i--)
    temp2[i]+=temp2[i+1];
  tot=0;
  for (i=0;i<nk;i++)
    tot+=temp2[i]*temp1[i];
  tot/=numProc;
  beg[0]=0;
  cnt=0;
  tot2=0;
  for (i=1;i<numProc;i++) {
    for (j=cnt;(j<nk) && (tot2<(tot*i));j++) tot2+=temp2[j]*temp1[j];
    if ((tot2-tot*i) > (tot*i-tot2+temp2[j-1]*temp1[j-1])) beg[i]=j-1; else beg[i]=j;
    cnt=j;
  }
  beg[numProc]=nk;
  /*for (i=0;i<numProc+1;i++)
    printf(" val of %i\n",beg[i]);*/

  /*printf("num %i start loop\n",myRank);*/
  for (i=beg[myRank];i<beg[myRank+1];i++) {
    /*    printf("value i %li value proc %i nbre proc %i\n",i,myRank,numProc);*/
#else
  for (i=0;i<nk;i++) {
#endif
    ind1= R[i];  
    nind1=R[i+1]-R[i];
    //printf("i %li nindi %li\n",i,nind1);
#ifdef _OMP_
#pragma omp parallel for private(j,ind2,nind2,l,k,ind) schedule(dynamic,1)
#endif
    for (j=i;j<nk;j++) { 
      ind2= R[j];
      nind2=R[j+1]-R[j];
      // printf("j %li nindj %li\n",j,nind2);
      for (l=0;l<(int)nind2;l++) 
	for (k=0;k<(int)nind1;k++) {
	  ind=abs(XR[ind2+l]-XR[ind1+k])+abs(YR[ind2+l]-YR[ind1+k])*nspec;
	  Mkk[i+nk*j]+=spec[ind];
	  //nele[i+(int)(*nk)*j]++;
	  //if ((i == 100) & (j ==200)) printf("%i\n",ind);
	  //	  if (( i == 100) & (j == 200)) printf("%lg\n",spec[ind]);
	}
    }}
  
#ifdef _MPI_
  /*printf("Job done proc %i\n",myRank);*/
  MPI_Barrier(MPI_COMM_WORLD);
  MPI_Reduce(Mkk, Mkkout, nk*nk, MPI_DOUBLE, MPI_SUM,0, MPI_COMM_WORLD);
#endif

#ifdef _MPI_
  /*printf("num %i reduce done now write \n",myRank);*/
  if (myRank == 0) {
    myfile= fopen(argv[2],"w");
    fwrite(Mkkout,sizeof(double),nk*nk,myfile);
    fclose(myfile);
    free(Mkkout);
    /*printf("end write\n");*/
  }
#else
  myfile= fopen(argv[2],"w");
  fwrite(Mkk,sizeof(double),nk*nk,myfile);
  fclose(myfile);
#endif

#ifdef _MPI_
  /*printf("num %i free up mem\n",myRank);*/
#endif
  free(R);
  free(XR);
  free(YR);
  free(spec);
  free(Mkk);
#ifdef _MPI_
  MPI_Finalize();
  /*printf("num %i end\n",myRank);*/
#endif
}
