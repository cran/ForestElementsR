% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stand_diameters_and_heights.R
\name{d_dom_weise}
\alias{d_dom_weise}
\title{Weise's Dominant Diameter}
\usage{
d_dom_weise(d, n_rep = 1)
}
\arguments{
\item{d}{vector of diameter values to calculate Weise's dominant diameter
of}

\item{n_rep}{vector of representation numbers (typically the number of trees
per ha corresponding to the diameter at the same position), will be used
as individual weights for each diameter. If n_rep has length 1, it will be
recycled to the length of d. Otherwise, if the length of n_rep does not
correspond to the length of d, the function will terminate with an error.}
}
\value{
The value of Weise's dominant diameter resulting from the input data
}
\description{
The dominant diameter after Weise is the quadratic mean diameter of the 20\%
biggest trees in a stand. In contrast to the dominant diameter
\code{\link{d_100}} it is well defined not only in monospecific stands, but
also in mixed stands.
}
\examples{
# A sample of trees from an angle count sample, where each
# tree represents a basal area of 4 m²/ha
d_cm <- c(12, 13, 25, 27, 28, 26, 26.1, 32, 35, 31, 42)
n_rep_ha <- 4 / ((d_cm / 100)^2 * pi / 4) # representation number of each tree
d_dom_weise(d_cm, n_rep_ha)
d_100(d_cm, n_rep_ha) # dominant diameter d100 for comparison
d_q(d_cm, n_rep_ha) # quadratic mean diameter for comparison

# if 20\% of the trees are 100 stems/ha, Weise's dominant diameter and
# d100 are equal
d_cm <- rnorm(n = 500, mean = 35, sd = 7)
d_dom_weise(d_cm, 1)
d_100(d_cm, 1)

# Weise's dominant diameter is greater than d100, if 20\% of the trees
# represent less than 100 trees/ha
d_cm <- rnorm(n = 200, mean = 35, sd = 7)
d_dom_weise(d_cm, 1)
d_100(d_cm, 1)

# Weise's dominant diameter is smaller than d100, if 20\% of the trees
# represent more than 100 trees/ha
d_cm <- rnorm(n = 800, mean = 35, sd = 7)
d_dom_weise(d_cm, 1)
d_100(d_cm, 1)

}
\seealso{
Other stand diameters: 
\code{\link{d_100}()},
\code{\link{d_q}()}
}
\concept{stand diameters}
