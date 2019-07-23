/* C function for splitting an object into its columns ************************/

#include "col_split.h"

/**
 * @title Fast split(x, col(x))
 * @param x (N, d)-matrix
 * @return split(x, col(x))
 * @author Marius Hofert, Kurt Hornik
 */
SEXP col_split(SEXP x)
{
    /* Setup */
    int *dims = INTEGER(getAttrib(x, R_DimSymbol));
    int n = dims[0], d = dims[1];
    SEXP res = PROTECT(allocVector(VECSXP, d));
    SEXP ref;
    int i = 0, j, k; /* i runs globally, j runs over all cols, k runs over all rows */

    /* Distinguish int/real matrices */

    switch (TYPEOF(x)) {
    case INTSXP:
    	for(j = 0; j < d; j++) {
    		SET_VECTOR_ELT(res, j, allocVector(INTSXP, n));
    		int *e = INTEGER(VECTOR_ELT(res, j));
    		for(k = 0 ; k < n ; i++, k++) {
    			e[k] = INTEGER(x)[i];
    		}
    	}
    	break;
    case REALSXP:
    	for(j = 0; j < d; j++) {
    		SET_VECTOR_ELT(res, j, allocVector(REALSXP, n));
    		double *e = REAL(VECTOR_ELT(res, j));
    		for(k = 0 ; k < n ; i++, k++) {
    			e[k] = REAL(x)[i];
    		}
    	}
    	break;
    case LGLSXP:
    	for(j = 0; j < d; j++) {
    		SET_VECTOR_ELT(res, j, allocVector(LGLSXP, n));
    		int *e = LOGICAL(VECTOR_ELT(res, j));
    		for(k = 0 ; k < n ; i++, k++) {
    			e[k] = LOGICAL(x)[i];
    		}
    	}
    	break;
    case STRSXP:
    	for(j = 0; j < d; j++) {
		ref = allocVector(STRSXP, n); /* needs to be here (otherwise the last column is replicated) */
    		SET_VECTOR_ELT(res, j, ref);
    		ref = VECTOR_ELT(res, j);
    		for(k = 0 ; k < n ; i++, k++) {
    			SET_STRING_ELT(ref, k, STRING_ELT(x, i));
    		}
    	}
    	break;
    default: error("Wrong type of 'x': %s", CHAR(type2str_nowarn(TYPEOF(x))));
    }

    /* Return */
    UNPROTECT(1);
    return(res);
}
