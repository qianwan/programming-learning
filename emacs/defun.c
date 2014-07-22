#include "lisp_trim.h"

DEFUN("for-lisp", Ffor_lisp, Sfor_lisp, 0, MANY, 0, doc: /*what*/)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  nargs = 1;
  int i = args[0].i;
  fprintf(stderr, "%d\n", i);
  Lisp_Object a = {0};
  return a;
}

int main() {
  Lisp_Object b = {10};
  Ffor_lisp(0, &b);
}
