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

---
# ForestElementsR 2.0.0

Initial CRAN submission.
