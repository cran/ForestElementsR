% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fe_species_bavrn_state.R
\name{vec_ptype_abbr.fe_species_bavrn_state}
\alias{vec_ptype_abbr.fe_species_bavrn_state}
\title{Abbreviation for the \emph{fe_species_bavrn_state} Type}
\usage{
\method{vec_ptype_abbr}{fe_species_bavrn_state}(x, ...)
}
\arguments{
\item{x}{An object of type \code{fe_species_bavrn_state}}

\item{...}{Other parameters (not used)}
}
\value{
The abbreviation to be displayed for the species coding
(\code{character}) in tibbles and in \code{str()}
}
\description{
Provide an abbreviated name for the class \code{fe_species_bavrn_state} to
be displayed in tibbles and \code{str()}
}
\examples{
spec_ids <- fe_species_bavrn_state(as.character(c(10, 30, 60)))
vctrs::vec_ptype_abbr(spec_ids)
str(spec_ids)

}
