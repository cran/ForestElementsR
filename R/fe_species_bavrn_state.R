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

# fe_species_bavrn_state

# Which reflects the tree species coding used by the Bavarian State Forest
# Service

# There are more than this species_id type

# Tradeoff: As I (P.B.) want to benefit from the typecasting infrastructure of
# vctrs,  I cannot implement a species_id superclass, there must be parallel S3
# classes which all inherit from character.

# https://vctrs.r-lib.org/articles/s3-vector.html



#' Constructor for the **fe_species_bavrn_state** Class
#'
#' Should be used by expert users only who know exactly what they are doing.
#' Other users please take the function \code{\link{fe_species_bavrn_state}}
#' for creating an object of that class.
#'
#' @param x An appropriate \code{character} vector
#'
#' @return An object of class \code{fe_species_bavrn_state}
#'
#' @export
#'
#' @examples
#' # Constructing a fe_species_bavrn_state object from scratch
#' # Use fe_species_bavrn_state() if you are not absolutely sure
#' spec_ids <- new_fe_species_bavrn_state(
#'   as.character(c(40, 40, 30, 30, 60, 60, 60))
#' )
#'
new_fe_species_bavrn_state <- function(x = character()) {
  new_fe_species(x, "fe_species_bavrn_state")
}



#' Check if an Object is a **fe_species_bavrn_state** species code vector
#'
#' @param x An object
#'
#' @return \code{TRUE} if the object inherits from the
#'   \code{fe_species_bavrn_state} class
#'
#' @export
#'
#' @examples
#' spec_ids <- new_fe_species_bavrn_state(
#'   as.character(c(40, 40, 30, 30, 60, 60, 60))
#' )
#' is_fe_species_bavrn_state(spec_ids)
#'
is_fe_species_bavrn_state <- function(x) {
  inherits(x, "fe_species_bavrn_state")
}



#' Formatted Output of an **fe_species_bavrn_state** Vector
#'
#' Usually, this function is not required to be called explicitly. It Will
#' always be used automatically, when an object of type
#' \code{fe_species_bavrn_state} is printed, be it alone, be it as part of
#' another object (e.g. a tibble)
#'
#' @param x An object of type \code{fe_species_bavrn_state}
#'
#' @param spec_lang Choice of how species (group) names or id's are displayed.
#'   Supported choices are "code" (default, displays the species codes as they
#'   are), "eng" (English species names), "ger" (German species names), and "sci"
#'   (scientific species names). The names and the codes refer to the species
#'   coding given in the object's attribute \code{species_coding}. The default
#'   is to request the choice with \code{options("spec_lang")}. If this option
#'   is not set, the choice "code" is used.
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
#' # Create an fe_species_bavrn_state object
#' spec_ids <- fe_species_bavrn_state(
#'   as.character(c(10, 30, 20, 40, 50, 60, 87, 63, 73, 72, 86, 80))
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
format.fe_species_bavrn_state <- function(x,
                                          spec_lang = options("fe_spec_lang")$fe_spec_lang,
                                          ...) {
  format_fe_species(x, spec_lang)
}



#' Summary of an **fe_species_bavrn_state** Vector
#'
#' Produces a summary for a fe_species_bavrn_state object in the same style as R
#' does for factors. Actually, after some conversions
#' \code{\link{summary.factor}} *is* called by this function. The species naming
#' in the summary depends on the parameter \code{spec_lang}.
#'
#' @param object Object of class \code{\link{fe_species_bavrn_state}}
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
#'   rep(fe_species_bavrn_state(c("10", "20", "21", "89")),
#'     times = c(15, 31, 70, 12)
#'   ),
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
summary.fe_species_bavrn_state <- function(object,
                                           spec_lang = options("fe_spec_lang")$fe_spec_lang,
                                           maxsum = 100L,
                                           ...) {
  summary_fe_species(object, spec_lang, maxsum)
}



#' Abbreviation for the *fe_species_bavrn_state* Type
#'
#' Provide an abbreviated name for the class \code{fe_species_bavrn_state} to
#' be displayed in tibbles and \code{str()}
#'
#' @param x An object of type \code{fe_species_bavrn_state}
#'
#' @param ... Other parameters (not used)
#'
#' @return The abbreviation to be displayed for the species coding
#'   (\code{character}) in tibbles and in \code{str()}
#'
#' @export
#'
#' @examples
#' spec_ids <- fe_species_bavrn_state(as.character(c(10, 30, 60)))
#' vctrs::vec_ptype_abbr(spec_ids)
#' str(spec_ids)
#'
vec_ptype_abbr.fe_species_bavrn_state <- function(x, ...) {
  "bavrn_state"
}



#' Validate an *fe_species_bavrn_state* Object
#'
#' Regular users will not require this function. Expert users will want to use
#' it in combination with the constructor \code{\link{fe_species_bavrn_state}}.
#' Regular users, please construct \code{fe_species_bavrn_state} objects with
#' \code{\link{fe_species_bavrn_state}}.
#'
#' @param x An object that is expected to be a correct
#'   \code{fe_species_bavrn_state} object
#'
#' @return Returns \code{x}, but this function is mainly called for its side
#'   effect which is pointing out any violations of the
#'   \code{fe_species_bavrn_state} object specifications. In case of such
#'   violations, the function will terminate with an error.
#'
#' @export
#'
#' @examples
#' # Passes validation
#' spec_ids <- as.character(c(30, 30, 30, 10, 10, 10, 10, 20, 20, 80))
#' spec_ids <- new_fe_species_bavrn_state(spec_ids)
#' validate_fe_species_bavrn_state(spec_ids)
#'
#' # Validating the following spec_ids throws an error due to
#' # non-supported species codes
#' spec_ids <- as.character(c(30, 30, 8712, 10, 10, 10, 349, 20, 20, 80))
#' spec_ids <- new_fe_species_bavrn_state(spec_ids)
#' try(
#'   validate_fe_species_bavrn_state(spec_ids)
#' )
#'
validate_fe_species_bavrn_state <- function(x = character()) {
  stopifnot(is_fe_species_bavrn_state(x)) # Check for class attribute
  validate_fe_species(x)
}



#' Construct a *fe_species_bavrn_state* Species Code Vector
#'
#' User interface for constructing a vector of species codes follwing the
#' *fe_species_bavrn_state* convention
#'
#' The *bavrn_state* species coding is the species coding used by the Bavarian
#' State Forest Service. See the example section for how to look up the coding.
#'
#' @param x Input vector to become a vector of tree species codes by the
#'   definition *bavrn_state*. Any type of vector (typically
#'   \code{integer}) which, after conversion with \code{\link{as.character}},
#'   adheres to that definition is acceptable. If \code{x} is provided as a
#'   character vector, leading and trailing white spaces will be trimmed.
#'
#' @return If the user input allows to construct a well-defined
#'   \code{fe_species_bavrn_state} object, this object will be returned. If
#'   not, the function will terminate with an error.
#'
#' @export
#'
#' @examples
#' # Libraries required for the following two examples
#' library(dplyr)
#' library(purrr)
#'
#' # Look up the bavrn_state species codes for all supported species
#' # the column species_id contains the bavrn_state codes
#' species_codings |>
#'   filter(species_coding == "bavrn_state") |>
#'   pluck(2, 1) |>
#'   arrange(as.numeric(species_id)) |> # just for the look of it
#'   print(n = Inf)
#'
#' # Display a summary table which shows the number of single species behind
#' # each bavrn_state species code
#' species_codings |>
#'   filter(species_coding == "bavrn_state") |>
#'   pluck(2, 1) |>
#'   group_by(name_eng, species_id) |> # display english names
#'   summarise(n = n()) |>
#'   arrange(as.numeric(species_id)) |> # just for the look of it
#'   print(n = Inf)
#'
#'
#' # Make an fe_species_bavrn_state vector from a vector of integer codes
#' spec_ids <- fe_species_bavrn_state(
#'   c(10, 10, 10, 60, 60, 60, 60, 30, 30, 80, 86, 80)
#' )
#'
fe_species_bavrn_state <- function(x = character()) {
  x <- trimws(as.character(x), which = "both")
  x <- new_fe_species_bavrn_state(x)
  validate_fe_species_bavrn_state(x)
}


# A fe_species_bavrn_state should be ordered by its integer values, even if
# ordering by the output of format would suggest another order

#' @export
vec_proxy_order.fe_species_bavrn_state <- function(x, ...) {
  as.integer(vctrs::vec_data(x))
}


# Casting into fe_species_bavrn_state

#' @export
vec_cast.fe_species_bavrn_state.integer <-
  function(x, to, ...) {
    x <- as.character(x)
    fe_species_bavrn_state(x)
  }

#' @export
vec_cast.fe_species_bavrn_state.double <-
  function(x, to, ...) {
    x <- as.character(x)
    fe_species_bavrn_state(x)
  }

#' @export
vec_cast.fe_species_bavrn_state.character <-
  function(x, to, ...) {
    fe_species_bavrn_state(x)
  }

#' @export
vec_cast.fe_species_bavrn_state.fe_species_tum_wwk_short <-
  function(x, to, ...) {
    x_trans <- spec_id_cast_do_it(x, "tum_wwk_short", "bavrn_state")
    fe_species_bavrn_state(x_trans)
  }

#' @export
vec_cast.fe_species_bavrn_state.fe_species_ger_nfi_2012 <-
  function(x, to, ...) {
    x_trans <- spec_id_cast_do_it(x, "ger_nfi_2012", "bavrn_state")
    fe_species_bavrn_state(x_trans)
  }

#' @export
vec_cast.fe_species_bavrn_state.fe_species_master <-
  function(x, to, ...) {
    x_trans <- spec_id_cast_do_it(x, "master", "bavrn_state")
    fe_species_bavrn_state(x_trans)
  }

#' @export
vec_cast.fe_species_bavrn_state.fe_species_bavrn_state_short <-
  function(x, to, ...) {
    x_trans <- spec_id_cast_do_it(x, "bavrn_state_short", "bavrn_state")
    fe_species_bavrn_state(x_trans)
  }


#' Cast Appropriate Objects Into a **fe_stand_bavrn_state** Species Class
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
#' @return If a meaningful cast is possible, an
#'   \code{fe_species_bavrn_state} object is returned
#'
#' @export
#'
#' @examples
#' as_fe_species_bavrn_state(c(10L, 40L, 40L, 20L)) # integer
#' as_fe_species_bavrn_state(c(10, 40, 40, 20)) # double
#' as_fe_species_bavrn_state(c("10", "40", "40", "20")) # character
#'
#' # cast other fe_species classes
#' as_fe_species_bavrn_state(
#'   fe_species_tum_wwk_short(as.character(c(1, 1, 1, 3, 3, 5)))
#' )
#' as_fe_species_bavrn_state(
#'   fe_species_ger_nfi_2012(as.character(c(20, 20, 10, 30, 30, 100)))
#' )
#'
#' # display the casting result in terms of scientific species names
#' as_fe_species_bavrn_state(c(10L, 40L, 40L, 20L)) |> format("sci")
#'
as_fe_species_bavrn_state <- function(x) {
  vec_cast(x, to = fe_species_bavrn_state())
}
