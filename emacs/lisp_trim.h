#include <stddef.h>
#include <stdio.h>

#define USE_LSB_TAG 1

typedef long long int EMACS_INT;

typedef struct { EMACS_INT i; } Lisp_Object;

struct vectorlike_header
{
  ptrdiff_t size;
};

struct Lisp_Subr
{
  struct vectorlike_header header;
  union
  {
    Lisp_Object (*a0) (void);
    Lisp_Object (*a1) (Lisp_Object);
    Lisp_Object (*a2) (Lisp_Object, Lisp_Object);
    Lisp_Object (*a3) (Lisp_Object, Lisp_Object, Lisp_Object);
    Lisp_Object (*a4) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
    Lisp_Object (*a5) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
    Lisp_Object (*a6) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
    Lisp_Object (*a7) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
    Lisp_Object (*a8) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
    Lisp_Object (*aUNEVALLED) (Lisp_Object args);
    Lisp_Object (*aMANY) (ptrdiff_t, Lisp_Object *);
  } function;
  short min_args, max_args;
  const char *symbol_name;
  const char *intspec;
  const char *doc;
};

enum pvec_type
{
  PVEC_NORMAL_VECTOR,
  PVEC_FREE,
  PVEC_PROCESS,
  PVEC_FRAME,
  PVEC_WINDOW,
  PVEC_BOOL_VECTOR,
  PVEC_BUFFER,
  PVEC_HASH_TABLE,
  PVEC_TERMINAL,
  PVEC_WINDOW_CONFIGURATION,
  PVEC_SUBR,
  PVEC_OTHER,
  PVEC_COMPILED,
  PVEC_CHAR_TABLE,
  PVEC_SUB_CHAR_TABLE,
  PVEC_FONT
};

enum
  {
    BITS_PER_CHAR      = 1,
    BITS_PER_SHORT     = 1 * sizeof (short),
    BITS_PER_INT       = 1 * sizeof (int),
    BITS_PER_LONG      = 1 * sizeof (long int),
    BITS_PER_EMACS_INT = 1 * sizeof (EMACS_INT)
  };

enum Lisp_Bits
  {
    /* Number of bits in a Lisp_Object tag.  This can be used in #if,
       and for GDB's sake also as a regular symbol.  */
    GCTYPEBITS =
#define GCTYPEBITS 3
        GCTYPEBITS,

    /* 2**GCTYPEBITS.  This must also be a macro that expands to a
       literal integer constant, for MSVC.  */
    GCALIGNMENT =
#define GCALIGNMENT 8
        GCALIGNMENT,

    /* Number of bits in a Lisp_Object value, not counting the tag.  */
    VALBITS = BITS_PER_EMACS_INT - GCTYPEBITS,

    /* Number of bits in a Lisp fixnum tag.  */
    INTTYPEBITS = GCTYPEBITS - 1,

    /* Number of bits in a Lisp fixnum value, not counting the tag.  */
    FIXNUM_BITS = VALBITS + 1
  };

enum Lisp_Type
  {
    /* Integer.  XINT (obj) is the integer value.  */
    Lisp_Int0 = 0,
    Lisp_Int1 = USE_LSB_TAG ? 1 << INTTYPEBITS : 1,

    /* Symbol.  XSYMBOL (object) points to a struct Lisp_Symbol.  */
    Lisp_Symbol = 2,

    /* Miscellaneous.  XMISC (object) points to a union Lisp_Misc,
       whose first member indicates the subtype.  */
    Lisp_Misc = 3,

    /* String.  XSTRING (object) points to a struct Lisp_String.
       The length of the string, and its contents, are stored therein.  */
    Lisp_String = USE_LSB_TAG ? 1 : 1 << INTTYPEBITS,

    /* Vector of Lisp objects, or something resembling it.
       XVECTOR (object) points to a struct Lisp_Vector, which contains
       the size and contents.  The size field also contains the type
       information, if it's not a real vector object.  */
    Lisp_Vectorlike = 5,

    /* Cons.  XCONS (object) points to a struct Lisp_Cons.  */
    Lisp_Cons = 6,

    Lisp_Float = 7,
  };

enum More_Lisp_Bits
{
  PSEUDOVECTOR_SIZE_BITS = 12,
  PSEUDOVECTOR_SIZE_MASK = (1 << PSEUDOVECTOR_SIZE_BITS) - 1,
  PSEUDOVECTOR_REST_BITS = 12,
  PSEUDOVECTOR_REST_MASK = (((1 << PSEUDOVECTOR_REST_BITS) - 1)
                            << PSEUDOVECTOR_SIZE_BITS),
  PSEUDOVECTOR_AREA_BITS = PSEUDOVECTOR_SIZE_BITS + PSEUDOVECTOR_REST_BITS,
  PVEC_TYPE_MASK = 0x3f << PSEUDOVECTOR_AREA_BITS
};

#define GCALIGNMENT 8

#ifndef alignas
# define alignas(alignment) /* empty */
#endif

#define DEFUN_ARGS_MANY		(ptrdiff_t, Lisp_Object *)
#define DEFUN_ARGS_UNEVALLED	(Lisp_Object)
#define DEFUN_ARGS_0	(void)
#define DEFUN_ARGS_1	(Lisp_Object)
#define DEFUN_ARGS_2	(Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_3	(Lisp_Object, Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_4	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_5	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
                         Lisp_Object)
#define DEFUN_ARGS_6	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
                         Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_7	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
                         Lisp_Object, Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_8	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
                         Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object)

#define DEFUN(lname, fnname, sname, minargs, maxargs, intspec, doc)	\
   Lisp_Object fnname DEFUN_ARGS_ ## maxargs ;				\
   static struct Lisp_Subr alignas (GCALIGNMENT) sname =		\
     { PVEC_SUBR << PSEUDOVECTOR_SIZE_BITS,				\
      { .a ## maxargs = fnname },					\
       minargs, maxargs, lname, intspec, 0};				\
   Lisp_Object fnname

enum maxargs
{
  ZERO = 0,
  MANY = -2,
  UNEVALLED = -1
};

#define XLI(o) (o).i

enum lsb_bits
  {
    TYPEMASK = (1 << GCTYPEBITS) - 1,
    VALMASK = ~ TYPEMASK
  };

#define XTYPE(a) ((enum Lisp_Type) (XLI (a) & TYPEMASK))

#define SYMBOLP(x) (XTYPE ((x)) == Lisp_Symbol)

#define CHECK_TYPE(ok, Qxxxp, x) \
  do { if (!(ok)) wrong_type_argument(Qxxxp, (x)); } while(0)

#define CHECK_SYMBOL(x) CHECK_TYPE(SYMBOLP(x), Qconsp, x)

Lisp_Object LISP_MAKE_RVALUE(Lisp_Object o) { return o; }

#define eassert(X) ((void) (0 && (X)))

#define CONSP(x) (XTYPE((x)) == Lisp_Cons)

struct Lisp_Cons
{
  Lisp_Object car;
  union
  {
    Lisp_Object cdr;
    struct Lisp_Cons *chain;
  } u;
};

#define XUNTAG(a, type) (intptr_t (XLI(a) - (type)))

#define XCONS(a) (eassert (CONSP (a)),                          \
                  (struct Lisp_Cons *) XUNTAG (a, Lisp_Cons))

#define XCDR_AS_LVALUE(c) (XCONS (c)->u.cdr)

#define XCDR(c) LISP_MAKE_RVALUE (XCDR_AS_LVALUE(c))

enum Lisp_Fwd_Type
  {
    Lisp_Fwd_Int,		/* Fwd to a C `int' variable.  */
    Lisp_Fwd_Bool,		/* Fwd to a C boolean var.  */
    Lisp_Fwd_Obj,		/* Fwd to a C Lisp_Object variable.  */
    Lisp_Fwd_Buffer_Obj,	/* Fwd to a Lisp_Object field of buffers.  */
    Lisp_Fwd_Kboard_Obj,	/* Fwd to a Lisp_Object field of kboards.  */
  };

struct Lisp_Objfwd
{
  enum Lisp_Fwd_Type type;
  Lisp_Object *objvar;
};

extern void defvar_lisp(struct Lisp_Objfwd *, const char *, Lisp_Object *);

#define DEFVAR_LISP(lname, vname, doc)                  \
  do {                                                  \
    static struct Lisp_Objfwd o_fwd;                    \
    defvar_lisp (&o_fwd, lname, &globals.f_ ## vname);  \
  } while (0)

struct Lisp_Boolfwd
{
  enum Lisp_Fwd_Type type;
  Lisp_Object *boolvar;
};

extern void defvar_bool(struct Lisp_Boolfwd *, const char *, Lisp_Object *);

#define DEFVAR_BOOL(lname, vname, doc)                  \
  do {                                                  \
    static struct Lisp_Boolfwd b_fwd;                   \
    defvar_bool (&b_fwd, lname, &globals.f_ ## vname);  \
  } while (0)
