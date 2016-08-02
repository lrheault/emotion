/* looper.c */

/*
Description: C program to create a valence-shifting variable until next punctuation mark.
Usage: To be called from an R wrapper.
@Author: L. Rheault
*/


#include <R.h>
#include <ctype.h>

void looper(double *x, char *y[], double *z, int *m){
	int i;
	for (i = 1; i < *m; i++){
		if (x[i-1]==1 && z[i]==z[i-1] && ispunct(y[i][0])==0){
			x[i]=1;
		}
	}
}

