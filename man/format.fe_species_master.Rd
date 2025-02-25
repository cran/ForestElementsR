% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fe_species_master.R
\name{format.fe_species_master}
\alias{format.fe_species_master}
\title{Formatted Output of an \strong{fe_species_master} Vector}
\usage{
\method{format}{fe_species_master}(x, spec_lang = options("fe_spec_lang")$fe_spec_lang, ...)
}
\arguments{
\item{x}{An object of type \code{fe_species_master}}

\item{spec_lang}{Choice of how species (group) names or id's are displayed.
Supported choices are "code" (displays the species codes as they are),
"eng" (English species names), "ger" (German species names), and "sci"
(scientific species names). The names and the codes refer to the species
coding given in the object's attribute \code{species_coding}. The default
is to request the choice with \code{options("fe_spec_lang")}. If this
option is not set, the choice "code" is used.}

\item{...}{Other parameters (not used)}
}
\value{
A \code{character} vector either displaying the original species
codes provided in \code{x}, or the species (group) names in the desired
language
}
\description{
Usually, this function is not required to be called explicitly. It Will
always be used automatically, when an object of type
\code{fe_species_master} is printed, be it alone, be it as part of
another object (e.g. a tibble)
}
\examples{
# Create an fe_species_master object
spec_ids <- fe_species_master(
  c("picea_001", "fagus_001", "quercus_002", "quercus_001")
)

# Display in default style, scientific names, English, and German names
format(spec_ids)
format(spec_ids, spec_lang = "sci")
format(spec_ids, spec_lang = "eng")
format(spec_ids, spec_lang = "ger")

# Usual application: Set option for species code output
# Any print of an fe_species object will use the last setting of the option
options(fe_spec_lang = "eng")
spec_ids

}
