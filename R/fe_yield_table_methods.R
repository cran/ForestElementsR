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




#' Take an Age Slice out of an fe_yield_table_object
#'
#' Age slices out of yield tables are typically required for finding out the
#' site index for a given age-height pair, or for extracting a yield table value
#' when age and site index are given.
#'
#' If the age provided by the user is not directly contained in the table,
#' linear interpolation and also extrapolation is used for obtaining the age
#' slice. Currently, this is only done inside the general age span covered by
#' the table (slot \code{$age_coverage} of the yield table object). For ages
#' outside this range, the slice is given for the nearest covered age extreme,
#' and a warning is issued.
#'
#' @param age The age (in years) for which the time slice has to be drawn
#'
#' @param variable Name of the yield table variable for which the slice is to
#'   be taken
#'
#' @param ytable An object of class \code{\link{fe_yield_table}}
#'
#' @return A (named) vector representing the vertical slice of the desired
#'   yield table variable. The names are the site indexes as defined in the
#'   yield table's element \code{$site_index} (in the same order) with the
#'   prefix "si_".
#'
#' @family yield table functions
#'
#' @export
#'
#' @examples
#'   # Get the yield table heights of the Wiedemann 1943 Scots pine table at age
#'   # 73
#'   ytable_age_slice(
#'     fe_ytable_pine_wiedemann_moderate_1943,
#'     age = 73,
#'     variable = "h_q_m"
#'   )
#'
#'
ytable_age_slice <- function(age, variable, ytable) {

  stopifnot(is_fe_yield_table(ytable))

  # Keep input constrained
  if (age < min(ytable$age_coverage)) {
    warning(
      paste0("Selected age below coverage of the yield table ",
             ytable$name_international, ". ",
             "Using minimum age of yield table instead."
      )
    )
    age <- min(ytable$age_coverage)
  }

  if (age > max(ytable$age_coverage)) {
    warning(
      paste0("Selected age above coverage of the yield table ",
             ytable$name_international, ". ",
             "Using maximum age of yield table instead."
      )
    )
    age <- max(ytable$age_coverage)
  }

  # Get matrix to takes values from
  mat <- ytable$values[[variable]]

  # Run over all columns of the matrix, get the values for the required age,
  # inter- or extrapolate if necessary. Result is a vector with each element
  # corresponding to one of the yield table's defined site indexes
  apply(mat, MARGIN = 2,
    FUN = function(X, age_coverage) {

      dist <- abs(age - age_coverage)
      dist <- dist[which(!is.na(X))]
      age_coverage <- age_coverage[which(!is.na(X))]

      X    <- X[which(!is.na(X))]

      i <- doBy::which.minn(rank(dist), n = 2) # neighbor index
      xx <- age_coverage[i]
      yy <- X[i]

      rslt <- (age - xx[1]) * (yy[2] - yy[1]) / (xx[2] - xx[1]) + yy[1]

      rslt
    },
    age_coverage = ytable$age_coverage
  )
}




#' Take a Max Slice out of an fe_yield_table_object
#'
#' Max slices out of yield tables are typically required for finding out the
#' mai max site index for a given height based standard site index.
#'
#' A max slice in the sense of this function means a vector that, for each of
#' the yield table's standard site indexes, contains the table's max value of
#' the variable of interest.
#'
#' @param variable Name of the yield table variable for which the slice is to
#'   be taken
#'
#' @param ytable Name of the yield table variable for which the slice is to
#'   be taken
#'
#' @return A (named) vector representing the max slice of the desired
#'   yield table variable. The names are the site indexes as defined in the
#'   yield table's element \code{$site_index} (in the same order) with the
#'   prefix "si_".
#'
#' @family yield table functions
#'
#' @export
#'
#' @examples
#'   ytable_max_slice("mai_m3_ha_yr", fe_ytable_beech_wiedemann_moderate_1931)
#'
ytable_max_slice <- function(variable, ytable) {

  stopifnot(is_fe_yield_table(ytable))

  if (!(variable %in% names(ytable$values))) {
    stop(
      paste0(
        "Variable ", variable, " is not contained in the yield table ",
        ytable$name_international, "."
      ),
      call. = FALSE
    )
  }

  # Get matrix to takes values from
  mat <- ytable$values[[variable]]
  # Get maximum values from each colum (site index)
  apply(mat, MARGIN = 2, FUN = max, na.rm = TRUE)
}




#' Find Site Indexes With a Yield Table
#'
#' @param age Age (years) of the stand to be site indexed
#'
#' @param size Size value, typically a height (m), of the stand to be site
#'   indexed. Must correspond to the parameter \code{si_variable} (see below)
#'
#' @param ytable A yield table, must be an \code{fe_yield_table} object
#'
#' @param si_variable Name of the stand size variable, typically a height (m),
#'   to be used for site indexing. Must correspond to the parameter \code{size}
#'   (see above). If  \code{si_variable} is not part of the yield table object's
#'   slot \code{$site_index_variable}, the function will terminate with an
#'   error.
#'
#' @return The site index resulting from \code{age} and \code{height}
#'
#' @family yield table functions
#'
#' @export
#'
#' @examples
#'   site_index(72, 19.7, fe_ytable_pine_wiedemann_moderate_1943, "h_q_m")
#'
site_index <- function(age, size, ytable, si_variable) {

  stopifnot(is_fe_yield_table(ytable))

  if (!(si_variable %in% ytable$site_index_variable)) {
    stop(
      paste0(
        "Variable ", si_variable, " is not a site index variable of the yield ",
        "table ", ytable$name_international, "."
      ),
      call. = FALSE
    )
  }

  yt_slice <- ytable_age_slice(age, si_variable, ytable)
  dist     <- abs(size - yt_slice)
  ytsi     <- ytable$site_indexes

  # two nearest neighbors - interpolate / extrapolate
  i  <- doBy::which.minn(rank(dist), n = 2) # neighbor index
  yy <- ytsi[i]
  xx <- yt_slice[i]

  rslt <- (size - xx[1]) * (yy[2] - yy[1]) / (xx[2] - xx[1]) + yy[1]

  unname(rslt)
}




#' Look Up Values From Yield Tables
#'
#' Provide yield table values for a given age and site index. If necessary,
#' values are linearly inter- and extrapolated.
#'
#' @param age Stand age (years)
#'
#' @param si Site index (according to the yield table of interest's
#'   (\code{ytable}) site index definition). If si is outside the yield table's
#'   coverage, an extrapolated value is returned, and a warning is raised.
#'
#' @param variable Name of the variable to be looked up. If the name is not
#'   one of the variable names available in the \code{\link{fe_yield_table}}
#'   object, the function will terminate with an error.
#'
#' @param ytable A yield table, must be an \code{fe_yield_table} object
#'
#' @return The requested yield table value
#'
#' @family yield table functions
#'
#' @export
#'
#' @examples
#'   age <- 72
#'   si  <- 3.2
#'
#'   ytable_lookup(
#'     age, si, "h_q_m", fe_ytable_spruce_gehrhardt_moderate_1921
#'   )
#'
#'   ytable_lookup(
#'     age, si, "v_m3_ha", fe_ytable_spruce_gehrhardt_moderate_1921
#'   )
#'
#'   ytable_lookup(
#'     age, si, "mai_m3_ha_yr", fe_ytable_spruce_gehrhardt_moderate_1921
#'   )
#'
#'
ytable_lookup <- function(age, si, variable, ytable) {

  stopifnot(is_fe_yield_table(ytable))

  if (!(variable %in% names(ytable$values))) {
    stop(
      paste0(
        "Variable ", variable, " is not contained in the yield table ",
        ytable$name_international, "."
      ),
      call. = FALSE
    )
  }

  if (si < min(ytable$site_indexes) | si > max(ytable$site_indexes)) {
    warning(
      paste0(
        "Site index ", si, " is outside the coverage of the yield table ",
        ytable$name_international, ". Return value is extrapolated."
      ),
      call. = FALSE
    )
  }

  yt_slice <- ytable_age_slice(age, variable, ytable)
  dist     <- abs(si - ytable$site_indexes)

  # two nearest neighbors - interpolate / extrapolate
  i  <- doBy::which.minn(rank(dist), n = 2) # neighbor index
  yy <- yt_slice[i]
  xx <- ytable$site_indexes[i]

  rslt <- (si - xx[1]) * (yy[2] - yy[1]) / (xx[2] - xx[1]) + yy[1]

  unname(rslt)
}




#' Convert a Standard Site Index Into an MAI(age) Site Index
#'
#' A useful way of site indexing is to give the site index of a stand in terms
#' of a mean annual increment (mai) at a given age, typically 100 years. This
#' function converts a standard site index into such an mai site index. See
#' \code{\link{si_to_mai_max}} for an alternative mai based site indexing
#' method.
#'
#' @param si Standard site index to be converted, must correspond to the site
#'  index nomenclature of the yield table to be used (param \code{ytable}, see
#'  below).
#'
#' @param mai_variable Character, name of the mai_variable to be used. Must be
#'  one if the mai variables listed in the \code{fe_yield_table} object
#'  provided with the parameter \code{ytable}.
#'
#' @param age The stand age (years) for which the mai site index is to be
#'  defined, typically 100 years.
#'
#' @param ytable An object of class \code{fe_yield_table}
#'
#' @return The requested mai value corresponding to the given standard
#'  site index at the given age
#'
#' @family yield table functions
#'
#' @export
#'
#' @examples
#'  age <- 100
#'  mai_var <- "mai_m3_ha_yr" # mai in volume over bark before harvest
#'
#'  si_to_mai_age(2.3, mai_var, age, fe_ytable_larch_schober_moderate_1946)
#'  si_to_mai_age(0.7, mai_var, age, fe_ytable_larch_schober_moderate_1946)
#'  si_to_mai_age(2.3, mai_var, age, fe_ytable_beech_wiedemann_moderate_1931)
#'  si_to_mai_age(0.7, mai_var, age, fe_ytable_beech_wiedemann_moderate_1931)
#'
#'  mai_var <- "red_mai_m3_ha_yr" # mai in vol. under bark minus harvest losses
#'  si_to_mai_age(2.3, mai_var, age, fe_ytable_larch_schober_moderate_1946)
#'  si_to_mai_age(0.7, mai_var, age, fe_ytable_larch_schober_moderate_1946)
#'  si_to_mai_age(2.3, mai_var, age, fe_ytable_beech_wiedemann_moderate_1931)
#'  si_to_mai_age(0.7, mai_var, age, fe_ytable_beech_wiedemann_moderate_1931)
#'
si_to_mai_age <- function(si, mai_variable, age, ytable) {

  stopifnot(is_fe_yield_table(ytable))

  if (!mai_variable %in% (ytable$mai_variable)) {
    stop(
      paste0(
        "Variable ", mai_variable, " is not an mai variable of the yield ",
        "table ", ytable$name_international, "."
      ),
      call. = FALSE
    )
  }

  # Keep input constrained
  if (age < min(ytable$age_coverage)) {
    warning(
      paste0("Selected age below coverage of the yield table ",
             ytable$name_international, ". ",
             "Using minimum age of yield table (",
             min(ytable$age_coverage),
             " yrs) instead."
      )
    )
    age <- min(ytable$age_coverage)
  }

  if (age > max(ytable$age_coverage)) {
    warning(
      paste0("Selected age above coverage of the yield table ",
             ytable$name_international, ". ",
             "Using maximum age of yield table (",
             max(ytable$age_coverage),
             " yrs) instead."
      )
    )
    age <- max(ytable$age_coverage)
  }

  mai_slice <- ytable_age_slice(age, mai_variable, ytable)
  dist      <- abs(si - ytable$site_indexes)

  # two nearest neighbors - interpolate / extrapolate
  i  <- doBy::which.minn(rank(dist), n = 2) # neighbor index
  yy <- mai_slice[i]
  xx <- ytable$site_indexes[i]

  rslt <- (si - xx[1]) * (yy[2] - yy[1]) / (xx[2] - xx[1]) + yy[1]

  unname(rslt)
}




#' Convert a Standard Site Index Into an maximum MAI Site Index
#'
#' A less common, but sometimes useful way of site indexing is to give the site
#  index of a stand in terms of the maximum mean annual increment (mai). This
#' function converts a standard site index into such an maximum mai site index.
#' Typically, the stand age where the maximum mai is obtained increases from
#' better to lesser site index classes. See \code{\link{si_to_mai_age}} for an
#' alternative mai based site indexing method.
#'
#' @param si Standard site index to be converted, must correspond to the site
#'  index nomenclature of the yield table to be used (param \code{ytable}, see
#'  below).
#'
#' @param mai_variable Character, name of the mai_variable to be used. Must be
#'  one if the mai variables listed in the \code{fe_yield_table} object
#'  provided with the parameter \code{ytable}.
#'
#' @param ytable An object of class \code{fe_yield_table}
#'
#' @return The requested maximum mai value corresponding to the given standard
#'  site index
#'
#' @family yield table functions
#'
#' @export
#'
#' @examples
#'  mai_var <- "mai_m3_ha_yr" # mai in volume over bark before harvest
#'
#'  si_to_mai_max(2.3, mai_var, fe_ytable_larch_schober_moderate_1946)
#'  si_to_mai_max(0.7, mai_var, fe_ytable_larch_schober_moderate_1946)
#'  si_to_mai_max(2.3, mai_var, fe_ytable_beech_wiedemann_moderate_1931)
#'  si_to_mai_max(0.7, mai_var, fe_ytable_beech_wiedemann_moderate_1931)
#'
#'  mai_var <- "red_mai_m3_ha_yr" # mai in vol. under bark minus harvest losses
#'  si_to_mai_max(2.3, mai_var, fe_ytable_larch_schober_moderate_1946)
#'  si_to_mai_max(0.7, mai_var, fe_ytable_larch_schober_moderate_1946)
#'  si_to_mai_max(2.3, mai_var, fe_ytable_beech_wiedemann_moderate_1931)
#'  si_to_mai_max(0.7, mai_var, fe_ytable_beech_wiedemann_moderate_1931)
#'
si_to_mai_max <- function(si, mai_variable, ytable) {

  stopifnot(is_fe_yield_table(ytable))

  if (!mai_variable %in% (ytable$mai_variable)) {
    stop(
      paste0(
        "Variable ", mai_variable, " is not an mai variable of the yield ",
        "table ", ytable$name_international, "."
      ),
      call. = FALSE
    )
  }

  mai_slice <- ytable_max_slice(mai_variable, ytable)
  dist      <- abs(si - ytable$site_indexes)

  # two nearest neighbors - interpolate / extrapolate
  i  <- doBy::which.minn(rank(dist), n = 2) # neighbor index
  yy <- mai_slice[i]
  xx <- ytable$site_indexes[i]

  rslt <- (si - xx[1]) * (yy[2] - yy[1]) / (xx[2] - xx[1]) + yy[1]

  unname(rslt)
}




#' Calculate the Stocking Level ("Bestockungsgrad") of a Stand
#'
#' The stocking level (German "Bestockungsgrad") is an important measure for
#' stand density in practice. It is the ratio of a stand's actual basal area
#' and its expected basal area due to a yield table.
#'
#' @param ba The stand's basal area in m²/ha
#'
#' @param age The stand's age in years
#'
#' @param si The stand's site index according to the yield table \code{yt}
#'
#' @param ytable The yield table to be used as reference. Must be an object
#'   of class \code{\link{fe_yield_table}}
#'
#' @return The stocking level of the stand based on the yield table of interest
#'
#' @family yield table functions
#'
#' @export
#'
#' @examples
#'   # Scots pine stand, 72 years old, site index 1.2, basal area 41.3 m²/ha
#'
#'   # 1. Reference: Yield table for pine by Wiedemann
#'   stocking_level(
#'     ba = 41.3, age = 72, si = 1.2,
#'     ytable = fe_ytable_pine_wiedemann_moderate_1943
#'   )
#'
#'   # 2. Reference: Yield table for pine by Wiedemann
#'   stocking_level(
#'     ba = 41.3, age = 72, si = 1.2,
#'     ytable = fe_ytable_pine_gehrhardt_moderate_1921
#'   )
#'
#'   # Norway spruce stand, 72 years old, site index 38, basal area 41.3 m²/ha
#'   # 1. Reference Yield Table by Assmann-Franz
#'   stocking_level(
#'     ba = 41.3, age = 72, si = 38,
#'     ytable = fe_ytable_spruce_assmann_franz_mean_yield_level_1963
#'   )
#'
#'   # 2. Reference Yield Table for spruce by Wiedemann, moderate thinning,
#'   # site index 1.0
#'   stocking_level(
#'     ba = 41.3, age = 72, si = 1.0,
#'     ytable = fe_ytable_spruce_wiedemann_moderate_1936_42
#'   )
#'
#'
stocking_level <- function(ba, age, si, ytable) {

  stopifnot(is_fe_yield_table(ytable))

  ba_yt <- ytable_lookup(age = age, si = si, variable = "ba_m2_ha", ytable)

  stock_level <- ba / ba_yt

  # Catch rare strange constellations we have observed with inventory data
  if (stock_level < 0) {
    warning("Stocking_level < 0 set to 0.")
    stock_level <- 0
  }

  stock_level
}









