# ForestElementsR 2.2.0

* Implemented S3 function get_center() that retrieves the coordinates of a 
  meaningful center point for fe_stand childs that carry spatial information.
* The yield table related functions `ytable_age_slice`, `ytable_max_slice`,
  `site_index`, `ytable_lookup`, `si_to_mai_age`, `si_to_mai_max`, and
  `stocking_level` were made more safe. These functions are not designed for
  working with input variable vectors of length > 1. However, under certain
  circumstances such input would trigger a warning, but produce dubious results.
  Now, any attempt to provide input vectors with more than one element to these 
  functions results in an error.
* Edited all three vignettes.
  
***
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
