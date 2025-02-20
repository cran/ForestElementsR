% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fe_ccircle_spatial.R
\name{validate_fe_ccircle_spatial}
\alias{validate_fe_ccircle_spatial}
\title{Validate an \strong{fe_ccircle_spatial} Object}
\usage{
validate_fe_ccircle_spatial(x, method = c("strict", "flexible"))
}
\arguments{
\item{x}{An object that is expected to be a correct \code{fe_ccircle_spatial}
object}

\item{method}{Character string that specifies whether tree_positions is
allowed to contain less tree_ids than trees (i.e. in this case, not all
trees have coordinates). Possible choices are "strict" (default) and
"flexible". If method == "flexible", a warning is issued if not all trees
have coordinates. If method == "strict", the validation terminates with an
error.}
}
\value{
Returns \code{x}, but this function is mainly called for its side
effect which is pointing out any violations of the \code{fe_ccircle_spatial}
object specifications. In case of such violations, the function will
terminate with an error.
}
\description{
Regular users will not require this function. Expert users will want to use
it in combination with the constructor \code{\link{new_fe_ccircle_spatial}}.
Regular users, please construct \code{fe_ccircle_spatial} objects with
\code{\link{fe_ccircle_spatial}}.
}
\examples{
  # Validate the example fe_ccircle_spatial object
  spruce_pine_ccircle_spatial |>
    validate_fe_ccircle_spatial(method = "flexible")

}
