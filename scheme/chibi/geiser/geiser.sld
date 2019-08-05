; -*- geiser-scheme-implementation:chibi; -*-
(define-library (geiser)
  (export geiser:completions
          geiser:eval
          geiser:autodoc
          geiser:module-completions
          geiser:no-values
;	  geiser:symbol-location
	  geiser:module-location
          geiser:newline)
  (import (scheme small) (chibi modules) (chibi) (meta) (chibi ast) (chibi string) (srfi 1) (srfi 95) (chibi show))
  (include "geiser.scm"))
