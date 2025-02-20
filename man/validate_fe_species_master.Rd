% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fe_species_master.R
\name{validate_fe_species_master}
\alias{validate_fe_species_master}
\title{Validate an \emph{fe_species_master} Object}
\usage{
validate_fe_species_master(x = character())
}
\arguments{
\item{x}{An object that is expected to be a correct
\code{fe_species_master} object}
}
\value{
Returns \code{x}, but this function is mainly called for its side
effect which is pointing out any violations of the
\code{fe_species_master} object specifications. In case of such
violations, the function will terminate with an error.
}
\description{
Regular users will not require this function. Expert users will want to use
it in combination with the constructor \code{\link{new_fe_species_master}}.
Regular users, please construct \code{fe_species_master} objects with
\code{\link{fe_species_master}}.
}
\examples{
# Passes validation
spec_ids <- c("pinus_001", "quercus_002", "pinus_001", "fagus_001")
spec_ids <- new_fe_species_master(spec_ids)
validate_fe_species_master(spec_ids)

# Validating the following spec_ids throws an error due to
# non-supported species codes
spec_ids <- c("pinus_001", "my_awesome_species_003", "wonder_tree_3012")
spec_ids <- new_fe_species_master(spec_ids)
try(
  validate_fe_species_master(spec_ids)
)

}
