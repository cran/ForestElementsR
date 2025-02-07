# license GPL-3
# This file is part of the R-package ForestElementsR.
#
# ForestElementsR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ForestElementsR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ForestElementsR.  If not, see <https://www.gnu.org/licenses/>.




# This is the implementation of the vctrs-based S3 class

# fe_species_tum_wwk_long

# The tum_wwk_long species coding is one of two codings in use at the Chair of
# Forest Growth and Yield Science. It defines a larger set of single species
# than the tum_wwk_short coding. In its original version, this coding contains
# several species groups, but most of these groups are ambiguous as they include
# species which also have a single coding. These groups were not included in
# this package.

# There are more than this species_id type

# Tradeoff: As I (P.B.) want to benefit from the typecasting infrastructure of
# vctrs,  I cannot implement a species_id superclass, there must be parallel S3
# classes which all inherit from character.

# https://vctrs.r-lib.org/articles/s3-vector.html



#' Constructor for the **fe_species_tum_wwk_long** Class
#'
#' Should be used by expert users only who know exactly what they are doing.
#' Other users please take the function \code{\link{fe_species_tum_wwk_long}}
#' for creating an object of that class.
#'
#' @param x An appropriate \code{character} vector
#'
#' @return An object of class \code{fe_species_tum_wwk_long}
#'
#' @export
#'
#' @examples
#' # Constructing a fe_species_tum_wwk_long object from scratch
#' # Use fe_species_tum_wwk_long() if you are not absolutely sure
#' spec_ids <- new_fe_species_tum_wwk_long(
#'   as.character(c(70, 61, 88, 88, 10, 971, 32))
#' )
#'
new_fe_species_tum_wwk_long <- function(x = character()) {
  new_fe_species(x, "fe_species_tum_wwk_long")
}



#' Check if an Object is a **fe_species_tum_wwk_long** species code vector
#'
#' @param x An object
#'
#' @return \code{TRUE} if the object inherits from the
#'   \code{fe_species_tum_wwk_long} class
#'
#' @export
#'
#' @examples
#' spec_ids <- new_fe_species_tum_wwk_long(
#'   as.character(c(70, 61, 88, 88, 10, 971, 32))
#' )
#' is_fe_species_tum_wwk_long(spec_ids)
#'
is_fe_species_tum_wwk_long <- function(x) {
  inherits(x, "fe_species_tum_wwk_long")
}



#' Formatted Output of an **fe_species_tum_wwk_long** Vector
#'
#' Usually, this function is not required to be called explicitly. It Will
#' always be used automatically, when an object of type
#' \code{fe_species_tum_wwk_long} is printed, be it alone, be it as part of
#' another object (e.g. a tibble)
#'
#' @param x An object of type \code{fe_species_tum_wwk_long}
#'
#' @param spec_lang Choice of how species (group) names or id's are displayed.
#'   Supported choices are "code" (displays the species codes as they are),
#'   "eng" (English species names), "ger" (German species names), and "sci"
#'   (scientific species names). The names and the codes refer to the species
#'   coding given in the object's attribute \code{species_coding}. The default
#'   is to request the choice with \code{options("fe_spec_lang")}. If this
#'   option is not set, the choice "code" is used.
#'
#' @param ... Other parameters (not used)
#'
#' @return A \code{character} vector either displaying the original species
#'   codes provided in \code{x}, or the species (group) names in the desired
#'   language
#'
#' @export
#'
#' @examples
#' # Create an fe_species_tum_wwk_long object
#' spec_ids <- fe_species_tum_wwk_long(
#'   as.character(c(70, 61, 88, 88, 10, 971, 32))
#' )
#'
#' # Display in default style, scientific names, English, and German names
#' format(spec_ids)
#' format(spec_ids, spec_lang = "sci")
#' format(spec_ids, spec_lang = "eng")
#' format(spec_ids, spec_lang = "ger")
#'
#' # Usual application: Set option for species code output
#' # Any print of an fe_species object will use the last setting of the option
#' options(fe_spec_lang = "sci")
#' spec_ids
#'
format.fe_species_tum_wwk_long <- function(x,
                                           spec_lang = options("fe_spec_lang")$fe_spec_lang,
                                           ...) {
  format_fe_species(x, spec_lang)
}



#' Summary of an **fe_species_tum_wwk_long** Vector
#'
#' Produces a summary for a fe_species_tum_wwk_long object in the same style as
#' R does for factors. Actually, after some conversions
#' \code{\link{summary.factor}} *is* called by this function. The species naming
#' in the summary depends on the parameter \code{spec_lang}.
#'
#' @param object Object of class \code{\link{fe_species_tum_wwk_long}}
#'
#' @param spec_lang Choice of how species (group) names or id's are displayed in
#'   the summary. Supported choices are "code" (displays the species codes as
#'   they are), "eng" (English species names), "ger" (German species names), and
#'   "sci" (scientific species names). The names and the codes refer to the
#'   species coding given in the object's attribute \code{species_coding}. The
#'   default is to request the choice with \code{options("fe_spec_lang")}. If
#'   this option is not set, the choice "code" is used.
#'
#' @param maxsum Same as parameter \code{maxsum} in \code{\link{summary.factor}}
#'
#' @param ... Other parameters (not used)
#'
#' @return A named vector in the same style as returned by
#'   \code{\link{summary.factor}}
#'
#' @export
#'
#' @examples
#' # Construct some species id vector
#' spec_ids <- c(
#'   rep(fe_species_tum_wwk_long(c("10", "60", "87", "811")), each = 15),
#'   NA, NA
#' )
#'
#' summary(spec_ids)
#' spec_ids |> summary()
#' spec_ids |> summary(spec_lang = "eng")
#'
#' # Usual application: Set option for species code output
#' # Any summary of an fe_species object will use the last setting of the
#' # option
#' options(fe_spec_lang = "sci")
#' spec_ids |> summary()
#'
summary.fe_species_tum_wwk_long <- function(object,
                                            spec_lang = options("fe_spec_lang")$fe_spec_lang,
                                            maxsum = 100L,
                                            ...) {
  summary_fe_species(object, spec_lang, maxsum)
}



#' Abbreviation for the *fe_species_tum_wwk_long* Type
#'
#' Provide an abbreviated name for the class \code{fe_species_tum_wwk_long} to
#' be displayed in tibbles and \code{str()}
#'
#' @param x An object of type \code{fe_species_tum_wwk_long}
#'
#' @param ... Other parameters (not used)
#'
#' @return The abbreviation to be displayed for the species coding
#'   (\code{character}) in tibbles and in \code{str()}
#'
#' @export
#'
#' @examples
#' spec_ids <- fe_species_tum_wwk_long(as.character(c(10, 50, 87, 813)))
#' vctrs::vec_ptype_abbr(spec_ids)
#' str(spec_ids)
#'
vec_ptype_abbr.fe_species_tum_wwk_long <- function(x, ...) {
  "tm_wwk_lng"
}



#' Validate an *fe_species_tum_wwk_long* Object
#'
#' Regular users will not require this function. Expert users will want to use
#' it in combination with the constructor
#' \code{\link{new_fe_species_tum_wwk_long}}. Regular users, please construct
#' \code{fe_species_tum_wwk_long} objects with
#' \code{\link{fe_species_tum_wwk_long}}.
#'
#' @param x An object that is expected to be a correct
#'   \code{fe_species_tum_wwk_long} object
#'
#' @return Returns \code{x}, but this function is mainly called for its side
#'   effect which is pointing out any violations of the
#'   \code{fe_species_tum_wwk_long} object specifications. In case of such
#'   violations, the function will terminate with an error.
#'
#' @export
#'
#' @examples
#' # Passes validation
#' spec_ids <- as.character(c(70, 61, 88, 88, 10, 971, 32))
#' spec_ids <- new_fe_species_tum_wwk_long(spec_ids)
#' validate_fe_species_tum_wwk_long(spec_ids)
#'
#' # Validating the following spec_ids throws an error due to
#' # non-supported species codes
#' spec_ids <- as.character(c(70, 61, 1221, 88, 88, 10, 971, 32, 4031))
#' spec_ids <- new_fe_species_tum_wwk_long(spec_ids)
#' try(
#'   validate_fe_species_tum_wwk_long(spec_ids)
#' )
#'
validate_fe_species_tum_wwk_long <- function(x = character()) {
  stopifnot(is_fe_species_tum_wwk_long(x)) # Check for class attribute
  validate_fe_species(x)
}



#' Construct a *fe_species_tum_wwk_long* Species Code Vector
#'
#' User interface for constructing a vector of species codes follwing the
#' *fe_species_tum_wwk_long* convention
#'
#' The *tum_wwk_long* species coding is one of two codings in use at the Chair
#' of Forest Growth and Yield Science (see \code{\link{species_codings}} for
#' more information). See the example section for how to look up the coding.
#'
#' @param x Input vector to become a vector of tree species codes by the
#'   definition *tum_wwk_long*. Any type of vector (typically \code{integer})
#'   which, after conversion with \code{\link{as.character}}, adheres to that
#'   definition is acceptable. If \code{x} is provided as a character vector,
#'   leading and trailing white spaces will be trimmed.
#'
#' @return If the user input allows to construct a well-defined
#'   \code{fe_species_ger_nfi_2012} object, this object will be returned. If
#'   not, the function will terminate with an error.
#'
#' @export
#'
#' @examples
#' # Libraries required for the following two examples
#' library(dplyr)
#' library(purrr)
#'
#' # Look up the tum_wwk_long species codes for all supported species
#' # the column species_id contains the tum_wwk_long codes
#' species_codings |>
#'   filter(species_coding == "tum_wwk_long") |>
#'   pluck(2, 1) |>
#'   arrange(as.numeric(species_id)) |> # just for the look of it
#'   print(n = Inf)
#'
#' # Display a summary table which shows the number of single species behind
#' # each tum_wwk_long species code
#' species_codings |>
#'   filter(species_coding == "tum_wwk_long") |>
#'   pluck(2, 1) |>
#'   group_by(name_eng, species_id) |> # display english names
#'   summarise(n = n()) |>
#'   arrange(as.numeric(species_id)) |> # just for the look of it
#'   print(n = Inf)
#'
#'
#' # Make an fe_species_tum_wwk_long vector from a vector of integer codes
#' spec_ids <- fe_species_tum_wwk_long(
#'   c(10, 10, 10, 20, 20, 20, 50, 50, 811, 811, 811, 891)
#' )
#'
fe_species_tum_wwk_long <- function(x = character()) {
  x <- trimws(as.character(x), which = "both")
  x <- new_fe_species_tum_wwk_long(x)
  validate_fe_species_tum_wwk_long(x)
}


# A fe_species_tum_wwk_long should be ordered by its integer values, even if
# ordering by the output of format would suggest another order

#' @export
vec_proxy_order.fe_species_tum_wwk_long <- function(x, ...) {
  as.integer(vctrs::vec_data(x))
}


# Casting into fe_species_tum_wwk_long

#' @export
vec_cast.fe_species_tum_wwk_long.integer <-
  function(x, to, ...) {
    x <- as.character(x)
    fe_species_tum_wwk_long(x)
  }

#' @export
vec_cast.fe_species_tum_wwk_long.double <-
  function(x, to, ...) {
    x <- as.character(x)
    fe_species_tum_wwk_long(x)
  }

#' @export
vec_cast.fe_species_tum_wwk_long.character <-
  function(x, to, ...) {
    fe_species_tum_wwk_long(x)
  }

#' @export
vec_cast.fe_species_tum_wwk_long.fe_species_tum_wwk_short <-
  function(x, to, ...) {
    x_trans <- spec_id_cast_do_it(x, "tum_wwk_short", "tum_wwk_long")
    fe_species_tum_wwk_long(x_trans)
  }

#' @export
vec_cast.fe_species_tum_wwk_long.fe_species_bavrn_state <-
  function(x, to, ...) {
    x_trans <- spec_id_cast_do_it(x, "bavrn_state", "tum_wwk_long")
    fe_species_tum_wwk_long(x_trans)
  }

#' @export
vec_cast.fe_species_tum_wwk_long.fe_species_bavrn_state_short <-
  function(x, to, ...) {
    x_trans <- spec_id_cast_do_it(x, "bavrn_state_short", "tum_wwk_long")
    fe_species_tum_wwk_long(x_trans)
  }

#' @export
vec_cast.fe_species_tum_wwk_long.fe_species_ger_nfi_2012 <-
  function(x, to, ...) {
    x_trans <- spec_id_cast_do_it(x, "ger_nfi_2012", "tum_wwk_long")
    fe_species_tum_wwk_long(x_trans)
  }

#' @export
vec_cast.fe_species_tum_wwk_long.fe_species_master <-
  function(x, to, ...) {
    x_trans <- spec_id_cast_do_it(x, "master", "tum_wwk_long")
    fe_species_tum_wwk_long(x_trans)
  }


#' Cast Appropriate Objects Into a **fe_species_tum_wwk_long** Species Class
#' Object
#'
#' If the cast is forward ambiguous, the function terminates with an error.
#' "Forward ambiguous" means that one code in the original object corresponds to
#' more than one codes in the goal coding. If the cast loses information, a
#' warning is raised, but the cast is performed. "Information loss" in this
#' context means that several codes from the orginal coding correspond to only
#' one code in the goal coding.
#'
#' Note that a cast where only one species id from
#' the original coding translates in a goal coding which represents a group of
#' species is NOT considered losing information (i.e. backward ambiguous),
#' because of the 1:1 match in the constellation of the specific cast.
#'
#' @param x The object to be cast, either a vector of types \code{integer},
#'   \code{double}, or \code{character} or an object of one of the supported
#'   **fe_species** classes
#'
#' @return If a meaningful cast is possible, an \code{fe_species_ger_nfi_2012}
#'   object is returned
#'
#' @export
#'
#' @examples
#' as_fe_species_tum_wwk_long(c(10L, 41L, 41L, 31L)) # integer
#' as_fe_species_tum_wwk_long(c(10, 41, 41, 31)) # double
#' as_fe_species_tum_wwk_long(c("10", "41", "41", "31")) # character
#'
#' # cast other fe_species classes
#' as_fe_species_tum_wwk_long(
#'   fe_species_tum_wwk_short(as.character(c(1, 1, 1, 3, 3, 5)))
#' )
#' as_fe_species_tum_wwk_long(
#'   fe_species_bavrn_state(as.character(c(20, 20, 10, 30, 30, 60)))
#' )
#'
#' # display the casting result in terms of scientific species names
#' as_fe_species_tum_wwk_long(c(10L, 41L, 41L, 31L)) |> format("sci")
#'
as_fe_species_tum_wwk_long <- function(x) {
  vec_cast(x, to = fe_species_tum_wwk_long())
}
