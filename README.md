# List-accumulator
### _MRCO <angryspacefungus@gmail.com>_

This is a project to do ... something.

## License

GPL 3.0

## Overview

This project was inspired by the LREC and ALREC utilities in Paul Graham's "_On Lisp_". The book stated that the two helpers trended away from tail-recursive solutions, and that seemed like a suitable challenge.

LIST-ACCUMULATOR (LACC) is a tail-recursive equivalent to LREC
ANAPHORIC-LIST-ACCUMULATOR (ALACC) is a tail-recursive equivalent to ALREC which binds IT and ACC within it for reference. It includes several local macros embedded within it to function as a light DSL.

ACCUMULATE-FROM-LIST (ACCL) is a wrapper for ALACC.

## Exported Symbols and their aliases

### :accumulate-from-list :accl

#### Usage

The ACCL macro takes three arguments, a list, an initial value, and a function body. The function body is called on each object in the list. This function body may refer to two anaphoric local symbols IT and ACC. IT is the CAR of the list on each recursion. ACC is an accumulator which is initialized to the "initial value" argument. ACC is what the ACCL form will return.

First example:

```lisp
;;; An implementation of common-lisp function LENGTH.
* (defun mylength (lst)
     "Counts the objects in list LST."
     (accl lst 0 (incf ACC)))
    
* (mylength '(a b c))
-> 3
```

The function MYLENGTH passes the argument LST to ACCL. ACCL's internal anaphor ACC is initialized to zero. Finally, for each recursion, until the list is fully "eaten" (nil is reached at the end of the list), ACC is incremented. At the end, ACC is returned.

There is also one macrolet and many symbol macrolets which are available when calling ACCL. MYLENGTH could also be defined as:

```lisp
(defun mylength% (lst)
    (accl lst 0 increment))

(defun mylength%% (lst)
    (accl lst 0 inc))
```

For a list of all available local macros, see ANAPHORIC-LIST-ACCUMULATOR below. 

#### Examples

```lisp
(defun mylength (lst)
   (accl lst 0 increment))

* (mylength '(a b c))
-> 3
```

### :list-accumulator :lacc

#### Usage

LACC takes two arguments, a function (FN), and an optional initial value for the accumulator ACC which defaults to NIL. LACC returns recursive 

### :anaphoric-list-accumulator :alacc


## Msc. Exported Utilities.

### :named-lambda

### :genlambda :ulambda
