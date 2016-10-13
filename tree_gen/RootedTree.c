/*==============================================================*/
/* program: RootedTree.c                                        */
/* purpose: generating all rooted trees                         */
/* input  : n -- number of nodes                                */
/*          mc -- max number of children of a node              */
/*          lb,ub -- lower and upper bound on height            */
/* output : listing of rooted trees in relex order              */
/* date   : September 1995, updated Sept 2000                   */
/* programmers: Gang Li & Frank Ruskey                          */
/* algorithm: From the paper: G. Li and F. Ruskey,  "The        */
/*    Advantages of Forward Thinking in Generating Rooted and   */
/*    Free Trees", 10th Annual ACM-SIAM Symposium on Discrete   */
/*    Algorithms (SODA), (1999) S939-940.  See the web page at  */
/*    http://www.theory.csc.UVic.CA/~fruskey/                   */
/*      Publications/RootedFreeTree.html                        */
/* more info: See                                               */
/*    http://www.theory.csc.UVic.CA/inf/RootedTrees.html   */
/*==============================================================*/

#include <stdio.h>

#define MaxSize 64  /* max size of the tree */

int
 n,                             /* number of nodes in a tree */
 par[MaxSize],                  /* parent position of i */
 L[MaxSize],                    /* level of node i      */
 num ,                          /* total number of trees */
 mc,                            /* max number of children */
 chi[MaxSize],                  /* number of children of a node */
 nextp[MaxSize],                /* next good pos to add nodes */
 rChi[MaxSize],                 /* the right most child of node i */
 lb,                            /* lower bound of the height */
 ub,                            /* upper bound of the height */
 outputP,outputL,               /* output patern          */
 omit;                          /* a flag to omit unwanted tree */
int
 out_format;                    /* input parameters */ 

void PrintIt() { 
  int i;
  num = num+1;  
  if (num == 1 && omit ) {omit=0; num--;} else {
     if (outputP) {
	for(i=1;i<=n;i++) printf( "%3d",par[i] );
     }
     if (outputL) {
	for(i=1;i<=n;i++)printf( "%3d",L[i] );
     }
     if (outputP || outputL) printf("\n");
  }
}

void Gen( int p, int s, int cL ) { 
  int np, temp;

  if (p > n) PrintIt();
  else {
    if (cL==0 && p <= ub+2 ) par[p] = L[p] = p-1; 
    else  
      if (par[p-cL] < s) { par[p] = par[s];  L[p] = L[s]; }
      else { par[p] = cL + par[p-cL]; L[p] = 1+L[par[p]]; } 
      
	++chi[par[p]]; 
    temp = rChi[par[p]];
    if (chi[par[p]] <= mc) {
       rChi[par[p]] = p;
       nextp[p] = chi[par[p]] < mc ? par[p] : nextp[par[p]];
       Gen( p+1, s, cL );               /* <==== recursive call */    
       rChi[par[p]] = temp;
    } 
	--chi[par[p]];
    
    nextp[p] = nextp[par[p]];
    np = nextp[p];
    while (np >= 1) {
       par[p] = np;  L[p] = 1 + L[np];
       ++chi[np]; 
       temp = rChi[np];  rChi[np] = p;
       if (chi[np] >= mc) nextp[p] = nextp[np];
       Gen( p+1, temp, p-temp );         /* <==== recursive call */
       --chi[np]; 
       rChi[par[p]] = temp;
       np = nextp[np];
       nextp[p] = np;
    }
  }
} 

void printhelp() {
   printf( "n is the number of nodes in the tree.\n" );
   printf( "MaxChild is the maximum number of children of a node.\n" );
   printf( "lb is a lower bound on the height.\n" );
   printf( "ub is an upper bound on the height.\n" );
   printf( "Output formats:\n  0 number of trees only\n" );
   printf( "  1 parent array \n   2 level array" ); 
   printf( "  3 parent and level array.\n" );
   printf( "Frank Ruskey, 1995, 2000\n" );
}


int main( int argc, char *argv[] ) {
  int i;

  /* first set all the parameters */
  printf( "Enter n (0 for help): " );  scanf( "%d", &n );
  if (n == 0) { printhelp();  return(0); }
  
  printf( "Enter format (0,1,2,3): " );  scanf( "%d", &out_format );
  printf( "Enter MaxChild, lb , ub: " );  scanf( "%d%d%d", &mc, &lb, &ub );
  if(out_format==1) outputP = 1;
  if(out_format==2) outputL = 1;
  if(out_format==3) {outputP = 1; outputL = 1;}
  num = 0;
  /* mc = n; */ 
  if (ub < n-1 && lb < ub) omit = 1;
  for( i=1; i<=n; i++) chi[i]=0;
  /* now start the program  */
  if (lb > n || n > MaxSize-1) printf("Error: Invalid parameter!\n"); else
  for (i=1; i<=lb+1; i++) {
     par[i] = L[i] = nextp[i] = i-1;
     rChi[i] = i+1;  chi[par[i]] = 1;
  }
  rChi[lb+1] = 0;
  if (lb  < ub) Gen(lb+2, 0,  0);
  if (lb == ub) Gen(lb+2,lb+1,1);
  if (lb > ub) {
     printf("lower bound must be <= upper bound!\n");
     return( 1 );
  }
  printf("Total = %4d\n",num); 
 
  return( 0 );
}


