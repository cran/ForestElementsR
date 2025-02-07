# ForestElementsR 2.1.0

* Implemented automated tests for stand_sums_static()
* Revised function stand_sums_static(); new version of v_gri() allowed to 
  simplify the code.
* Modified the important function v_gri() to make it more tolerant. In case of 
  missing height values, it does not stop anymore but returns NA and issues a 
  warning.

***
# ForestElementsR 2.0.1

Revised CRAN submission

We have addressed the issues raised by Konstanze Lauseker in her review of the 
initial submission:

* Added references to the description field of the `DESCRIPTION` file.  
* Removed the use of `:::` even in internal documentation.  
* Replaced `\dontrun{}` with `try()` for examples that intentionally generate 
  errors.  
* Substituted `options(warn = -1)` with `suppressWarnings()` in the automated 
  tests.  

***
# ForestElementsR 2.0.0

Initial CRAN submission.
