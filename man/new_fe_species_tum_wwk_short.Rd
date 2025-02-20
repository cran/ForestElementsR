% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fe_species_tum_wwk_short.R
\name{new_fe_species_tum_wwk_short}
\alias{new_fe_species_tum_wwk_short}
\title{Constructor for the \strong{fe_species_tum_wwk_short} Class}
\usage{
new_fe_species_tum_wwk_short(x = character())
}
\arguments{
\item{x}{An appropriate \code{character} vector}
}
\value{
An object of class \code{fe_species_tum_wwk_short}
}
\description{
Should be used by expert users only who know exactly what they are doing.
Other users please take the function \code{\link{fe_species_tum_wwk_short}}
for creating an object of that class.
}
\examples{
# Constructing a fe_species_tum_wwk_short object from scratch
# Use fe_species_tum_wwk_short() if you are not absolutely sure
spec_ids <- new_fe_species_tum_wwk_short(
  as.character(c(4, 4, 3, 3, 5, 5, 5))
)

}
