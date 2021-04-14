#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _ChildRecordsR_convertor_long_cut_loop(SEXP, SEXP, SEXP, SEXP);
extern SEXP _ChildRecordsR_LENA_overlap_loop(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_ChildRecordsR_convertor_long_cut_loop", (DL_FUNC) &_ChildRecordsR_convertor_long_cut_loop, 4},
    {"_ChildRecordsR_LENA_overlap_loop",       (DL_FUNC) &_ChildRecordsR_LENA_overlap_loop,       3},
    {NULL, NULL, 0}
};

void R_init_ChildRecordsR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
