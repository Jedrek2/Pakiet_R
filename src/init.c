#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Deklaracje funkcji
SEXP euler(SEXP graf);
SEXP hammilton(SEXP graf);

static const R_CallMethodDef CallEntries[] = {
    {"euler", (DL_FUNC) &euler, 1},
    {"hammilton", (DL_FUNC) &hammilton, 1},
    {NULL, NULL, 0}
};

void R_init_Cykle(DllInfo *info) {
    R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}