#include "lisp_trim.h"

extern Lisp_Object wrong_type_argument(Lisp_Object, Lisp_Object);

#define CHECK_TYPE(ok, Qxxxp, x) \
  do { if (!(ok)) wrong_type_argument(Qxxxp, (x)); } while(0)

#define CHECK_SYMBOL(x) CHECK_TYPE(SYMBOLP(x), Qconsp, x)
