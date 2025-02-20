% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_yield_tables_for_species.R
\docType{data}
\name{yield_tables_for_species}
\alias{yield_tables_for_species}
\alias{ytables_bavrn_state_short_var_1}
\alias{ytables_tum_wwk_short_var_1}
\alias{ytables_bavrn_state_var_1}
\title{Yield Tables To Species Assignments}
\format{
An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 9 rows and 2 columns.

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 9 rows and 2 columns.

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 45 rows and 2 columns.
}
\usage{
ytables_bavrn_state_short_var_1

ytables_tum_wwk_short_var_1

ytables_bavrn_state_var_1
}
\description{
In order to facilitate the application of yield tables, we
provide data frames that link the names of implemented
\code{\link{fe_yield_table}} objects to species codings. Currently, there
are three such data frames:
\code{\link{fe_species_tum_wwk_short}},
\code{\link{fe_species_bavrn_state_short}},
\code{\link{fe_species_bavrn_state}}
Note, that different
yield table assignemnts for the same coding can be defined and coexist. In
future, such tables will be added also for less aggregated species codings.
}
\seealso{
Other yield table functions: 
\code{\link{fe_yield_table}()},
\code{\link{plot.fe_yield_table}()},
\code{\link{si_to_mai_age}()},
\code{\link{si_to_mai_max}()},
\code{\link{site_index}()},
\code{\link{stocking_level}()},
\code{\link{ytable_age_slice}()},
\code{\link{ytable_lookup}()},
\code{\link{ytable_max_slice}()}
}
\concept{yield table functions}
\keyword{datasets}
