% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fe_yield_table.R
\name{is_fe_yield_table}
\alias{is_fe_yield_table}
\title{Check if an Object is an \strong{fe_yield_table}}
\usage{
is_fe_yield_table(x)
}
\arguments{
\item{x}{An object}
}
\value{
\code{TRUE} if the object inherits from the \code{fe_yield_table}
class
}
\description{
Check if an Object is an \strong{fe_yield_table}
}
\examples{
  x <- "I want to be a yield table!"  # strange object
  is_fe_yield_table(x)                # sorry

  is_fe_yield_table(ytable_pine_wiedemann_moderate_1943_raw) # Nope
  is_fe_yield_table(fe_ytable_pine_wiedemann_moderate_1943)  # That's better

}
