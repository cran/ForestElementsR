% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fe_species_master.R
\name{vec_ptype_abbr.fe_species_master}
\alias{vec_ptype_abbr.fe_species_master}
\title{Abbreviation for the \emph{fe_species_master} Type}
\usage{
\method{vec_ptype_abbr}{fe_species_master}(x, ...)
}
\arguments{
\item{x}{An object of type \code{fe_species_master}}

\item{...}{Other parameters (not used)}
}
\value{
The abbreviation to be displayed for the species coding
(\code{character}) in tibbles and in \code{str()}
}
\description{
Provide an abbreviated name for the class \code{fe_species_master} to
be displayed in tibbles and \code{str()}
}
\examples{
spec_ids <- fe_species_master(c("pinus_001", "quercus_002", "pinus_001"))
vctrs::vec_ptype_abbr(spec_ids)
str(spec_ids)

}
