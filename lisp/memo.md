# Lisp Memo #

## 1 Syntax ##

### 1.1 S-expression ###

1. an atom, or
2. an expression of the form (x . y) where x and y are s-expressions.

Lisp Forms:
1. Function calls
2. Macros
3. Special operators

Macros
Macros provide hooks into compiler. Steps to compile and run:
1. Expand macros recursively until the code consists of only function call forms and special forms.
2. The macroless code is compiled into a FASL file.
3. Load FASL file

Bugs in Macros:
1. Multiple evaluation of the same form
2. Order of form evaluations
3. Symbol outside macros leaks into macros

Rules of Thumb when Writing a Macro
1. Unless there's a particular reason to do otherwise, include any subforms in the expansion in positions that will be evaluated in the same order as the subforms appear in the macro call.
2. Unless there's a particular reason to do otherwise, make sure subforms are evaluated only once by creating a variable in the expansion to hold the value of evaluating the argument form and then using that variable anywhere else the value is needed in the expansion.
3. Use GENSYM or MAKE-SYMBOL at macro expansion time to create variable names used in the expansion.

## Variable ##

### Lexical and Dynamic Variable ###
Global variables are dynamic. To define locally special variables, refer to DECLARE, SPECIAL and LOCALLY.

### DEFVAR and DEFPARAMETER ###
The difference between the two forms is that DEFPARAMETER always assigns the initial value to the named variable while DEFVAR does so only if the variable is undefined.

MAKUNBOUND is used to unbound a symbol.

(makunbound 'var)

### SETF ###
SETF is a macro.

(setf place value)

It checks the form on place. If that is a variable, SETF will be expaned into a call to the special operator SETQ.

### Generalized Assignment ###
For custom assignment, refer to DEFSETF and DEFINE-SET-EXPANDER.

### Modify Macros ###
Modify macros include INCF, DECF, PUSH, POP, PUSHNEW, ROTATEF and SHIFTF.

(let ((x '(1 2 3)))
    (incf (car x))
    (prin1 x))
;;; should return (2 2 3)

## Utils ##
This section shows how some useful utils work.

### MAPC, MAPCAR, MAPCAN, MAPL, MAPLIST, MAPCON ###
(MAPCAR) operates on successive elements of the lists
