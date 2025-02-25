% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/v_red_harvest_ubark.R
\name{v_red_harvest_ubark_core}
\alias{v_red_harvest_ubark_core}
\title{Core Function For the Volume Reduction With v_red_harvest_ubark}
\usage{
v_red_harvest_ubark_core(species_id, v_orig_m3, params)
}
\arguments{
\item{species_id}{Vector of species ids (see
\code{\link{v_red_harvest_ubark}}), but for this core function to work,
\code{species_id} must be provided as \code{character}. This will and
should be not checked inside this function, because if used as intended,
that has happened before calling it.}

\item{v_orig_m3}{Vector of wood volumes (m³) defined as standing over bark
(see documentation of \code{\link{v_red_harvest_ubark}}.)}

\item{params}{A data frame comprising species (group) wise reduction factors.
The calling function has to take care that \code{v_red_harvest_ubark_core}
is called with a \code{params} that fits to the coding used in
\code{species_id}.}
}
\value{
A vector of wood volumes defined as harvested under bark (see
\code{\link{v_red_harvest_ubark}})
}
\description{
See documentation of \code{\link{v_red_harvest_ubark}} for most information,
this function is the workhorse behind, and should never be called directly by
a user.
}
\details{
The function \code{v_red_harvest_ubark} is fully vectorized, inputs are
recyled according to the tibble rules
}
\keyword{internal}
