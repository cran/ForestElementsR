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




#' Quadratic Mean Diameter
#'
#' Function for calculating the quadratic mean of a vector. The typical
#' application in forestry is to calculate the quadratic mean diameter.
#'
#' @param d vector of (stem diameter at breast height) values to calculate the
#'   quadratic mean of
#'
#' @param n_rep vector of representation numbers (typically the number of trees
#'   per ha corresponding to the diameter at the same position), will be used
#'   as individual weights for each diameter. If n_rep has length 1, it will be
#'   recycled to the length of d. Otherwise, if the length of n_rep does not
#'   correspond to the length of d, the function will terminate with an error.
#'
#' @return the quadratic mean of d
#'
#' @family stand diameters
#'
#' @export
#'
#' @examples
#' # Evaluate a sample of equally weighted tree diameters
#' d_cm <- c(12, 13, 25, 27, 28, 26, 26.1, 32, 35, 31, 42)
#' d_q(d_cm) # quadratic mean diameter
#' mean(d_cm) # the arithmetic mean is not the same!
#'
#' # Assume, the same sample comes from an angle count sample, where each
#' # tree represents a basal area of 4 m²/ha
#' n_rep_ha <- 4 / ((d_cm / 100)^2 * pi / 4) # representation number of each tree
#' d_q(d_cm, n_rep_ha)
#'
#'
#' # Typical application to a set of single tree data grouped by survey
#' # time and species
#' library(dplyr)
#' oldopt <- options(fe_spec_lang = "eng") # display colloquial species names
#' # extract the tree data to allow insights into the mechanics
#' trees <- mm_forest_1_fe_stand_spatial$trees |> filter(!removal)
#' trees |>
#'   group_by(species_id, time_yr) |>
#'   summarise(
#'     n_ha   = round(sum(n_rep_ha)),
#'     d_q_cm = d_q(dbh_cm, n_rep_ha)
#'   ) |>
#'   print(n = Inf)
#' options(oldopt) # set species name display to previous value
#'
d_q <- function(d, n_rep = 1) {
  if ((length(n_rep) > 1) & (length(n_rep) != length(d))) {
    stop(
      "Length of n_rep does not match length of d."
    )
  }

  if (length(n_rep) == 1) n_rep <- rep(n_rep, length(d))

  i <- n_rep != 0 # could be the case for special trees, e.g. outside border
  # trees in fe_stand_spatial objects
  d <- d[i]
  n_rep <- n_rep[i]

  sqrt(sum(d^2 * n_rep) / sum(n_rep))
}




#' Dominant Diameter d100
#'
#' The dominant diameter d100 was conceptuated by Ernst Assmann and Friedrich
#' Franz in order obtain a mean diameter value for those trees which usually
#' dominate a stand throughout its whole life.
#'
#' The d100 is defined as the quadratic mean diameter of the hundred thickest
#' trees per ha. If there are only 100 trees or less on one ha, the d100 is the
#' same as the quadratic mean diameter \code{\link{d_q}}. While the d100 is well
#' defined and useful in monospecific stands, it is less so in mixed stands.
#'
#' @param d vector of diameter values to calculate the dominant diameter d_100
#'   of
#'
#' @param n_rep_ha vector of representation numbers per ha for each diameter
#'   in d. Must have the same length as d or the length 1 (in which case it is
#'   recycled to the length of d). Otherwise, the function terminates with an
#'   error.
#'
#' @return The dominant diameter d100 value resulting from the input data
#'
#' @family stand diameters
#'
#' @export
#'
#' @examples
#' # A sample of trees from an angle count sample, where each
#' # tree represents a basal area of 4 m²/ha
#' d_cm <- c(12, 13, 25, 27, 28, 26, 26.1, 32, 35, 31, 42)
#' n_rep_ha <- 4 / ((d_cm / 100)^2 * pi / 4) # representation number of each tree
#' d_100(d_cm, n_rep_ha)
#' d_q(d_cm, n_rep_ha) # quadratic mean diameter for comparison
#'
#'
#' # Typical application to a set of single tree data grouped by survey
#' # time and species
#' # (note that everyone is applying d_100 mixed stands, but you should do it
#' # only, if you know exactly what you are doing)
#' library(dplyr)
#' oldopt <- options(fe_spec_lang = "eng") # display colloquial species names
#' # for d_100 in mixed stands, we require species shares
#' spec_shares <- species_shares(
#'   mm_forest_1_fe_stand_spatial,
#'   tree_filter = !removal, # include remaining trees only
#'   method = "ba_wd"
#' )
#' # extract the tree data to allow insights into the mechanics
#' trees <- mm_forest_1_fe_stand_spatial$trees |> filter(!removal)
#' # join with the shares
#' trees |>
#'   left_join(spec_shares) |>
#'   group_by(species_id, time_yr) |>
#'   summarise(
#'     n_ha     = round(sum(n_rep_ha)),
#'     d_q      = d_q(dbh_cm, n_rep_ha), # For comparison
#'     d_100_cm = d_100(dbh_cm, n_rep_ha / species_share)
#'   ) |>
#'   print(n = Inf)
#' options(oldopt) # set species name display to previous value
#'
d_100 <- function(d, n_rep_ha) {
  if ((length(n_rep_ha) > 1) & (length(n_rep_ha) != length(d))) {
    stop(
      "Length of n_rep does not match length of d."
    )
  }

  if (length(n_rep_ha) == 1) n_rep_ha <- rep(n_rep_ha, length(d))

  i <- n_rep_ha != 0 # could be the case for special trees, e.g. outside border
  # trees in fe_stand_spatial objects
  d <- d[i]
  n_rep_ha <- n_rep_ha[i]

  # sorting by tree size is essential
  i <- order(d, decreasing = TRUE)
  d <- d[i]
  n_rep_ha <- n_rep_ha[i]
  cum_rep_ha <- cumsum(n_rep_ha)

  # If the cumulative sum somewhere gets beyond 100, the n_rep_ha must be
  # corrected
  if (max(cum_rep_ha > 100)) {
    min_over <- min(cum_rep_ha[cum_rep_ha >= 100] - 100) # min_over can be 0
    # but not smaller
    cum_minus_min <- cum_rep_ha - min_over
    cum_minus_min <- cum_minus_min[cum_minus_min <= 100]
    n <- length(cum_minus_min)
    i <- 1:n
    d <- d[i]
    n_rep_ha <- n_rep_ha[i]
    n_rep_ha[n] <- n_rep_ha[n] - min_over
  }

  d_q(d, n_rep_ha)
}




#' Weise's Dominant Diameter
#'
#' The dominant diameter after Weise is the quadratic mean diameter of the 20%
#' biggest trees in a stand. In contrast to the dominant diameter
#' \code{\link{d_100}} it is well defined not only in monospecific stands, but
#' also in mixed stands.
#'
#' @param d vector of diameter values to calculate Weise's dominant diameter
#'   of
#'
#' @param n_rep vector of representation numbers (typically the number of trees
#'   per ha corresponding to the diameter at the same position), will be used
#'   as individual weights for each diameter. If n_rep has length 1, it will be
#'   recycled to the length of d. Otherwise, if the length of n_rep does not
#'   correspond to the length of d, the function will terminate with an error.
#'
#' @return The value of Weise's dominant diameter resulting from the input data
#'
#' @family stand diameters
#'
#' @export
#'
#' @examples
#' # A sample of trees from an angle count sample, where each
#' # tree represents a basal area of 4 m²/ha
#' d_cm <- c(12, 13, 25, 27, 28, 26, 26.1, 32, 35, 31, 42)
#' n_rep_ha <- 4 / ((d_cm / 100)^2 * pi / 4) # representation number of each tree
#' d_dom_weise(d_cm, n_rep_ha)
#' d_100(d_cm, n_rep_ha) # dominant diameter d100 for comparison
#' d_q(d_cm, n_rep_ha) # quadratic mean diameter for comparison
#'
#' # if 20% of the trees are 100 stems/ha, Weise's dominant diameter and
#' # d100 are equal
#' d_cm <- rnorm(n = 500, mean = 35, sd = 7)
#' d_dom_weise(d_cm, 1)
#' d_100(d_cm, 1)
#'
#' # Weise's dominant diameter is greater than d100, if 20% of the trees
#' # represent less than 100 trees/ha
#' d_cm <- rnorm(n = 200, mean = 35, sd = 7)
#' d_dom_weise(d_cm, 1)
#' d_100(d_cm, 1)
#'
#' # Weise's dominant diameter is smaller than d100, if 20% of the trees
#' # represent more than 100 trees/ha
#' d_cm <- rnorm(n = 800, mean = 35, sd = 7)
#' d_dom_weise(d_cm, 1)
#' d_100(d_cm, 1)
#'
d_dom_weise <- function(d, n_rep = 1) {
  if ((length(n_rep) > 1) & (length(n_rep) != length(d))) {
    stop(
      "Length of n_rep does not match length of d."
    )
  }

  if (length(n_rep) == 1) n_rep <- rep(n_rep, length(d))

  i <- n_rep != 0 # could be the case for special trees, e.g. outside border
  # trees in fe_stand_spatial objects
  d <- d[i]
  n_rep <- n_rep[i]

  # Only the biggest trees over n_cutoff are taken into account
  n_cutoff <- sum(n_rep) * 0.2

  # sorting by tree size is essential
  i <- order(d, decreasing = TRUE)
  d <- d[i]
  n_rep <- n_rep[i]
  cum_rep <- cumsum(n_rep)

  # Correct the n_rep vector to represent the 20% biggest trees
  # min_over can be 0 but not smaller
  min_over <- min(cum_rep[cum_rep >= n_cutoff] - n_cutoff)
  cum_minus_min <- cum_rep - min_over
  cum_minus_min <- cum_minus_min[cum_minus_min <= n_cutoff]
  n <- length(cum_minus_min)
  i <- 1:n
  d <- d[i]
  n_rep <- n_rep[i]
  n_rep[n] <- n_rep[n] - min_over

  d_q(d, n_rep)
}




#' Quadratic Mean Height
#'
#' The quadratic mean height is the stand height corresponding to the quadratic
#' mean diameter \code{\link{d_q}}. It is actually the basal area weighted
#' average height of a tree in the stand of interest. Insofar, its name is
#' somewhat misleading.
#'
#' @param h vector of tree height values to calculate the quadratic mean height
#'   from
#'
#' @param d vector of stem diameters at breast height corresponding to h, must
#'   therefore have the same length as h
#'
#' @param n_rep vector of representation numbers (typically the number of trees
#'   per ha corresponding to the diameter at the same position), will be used as
#'   individual weights for each height, together with the squared diameters d.
#'   If n_rep has length 1, it will be recycled to the length of d. Otherwise,
#'   if the length of n_rep does not correspond to the length of d, the function
#'   will terminate with an error.
#'
#' @param na_h.rm logical; if \code{TRUE}, records with \code{NA} heights will
#'   be excluded from \code{h}, \code{d}, and \code{n_rep}. If \code{FALSE},
#'   \code{NA} height values are kept and will lead to the function returning
#'   \code{NA}.
#'
#' @return the quadratic mean height value resulting from the input data
#'
#' @family stand heights
#'
#' @export
#'
#' @examples
#' # Evaluate a sample of equally weighted trees
#' d_cm <- c(12, 13, 25, 27, 28, 26, 26.1, 32, 35, 31, 42)
#' dq_cm <- d_q(d_cm)
#' h_m <- 0.9 * dq_cm * (d_cm / dq_cm)^0.8 # quick plausible height estim.
#' h_q(h_m, d_cm) # quadratic mean height
#' mean(h_m) # the arithmetic mean height is not the same!
#'
#' # Assume, the same sample are trees from an angle count sample, where each
#' # tree represents a basal area of 4 m²/ha
#' n_rep_ha <- 4 / ((d_cm / 100)^2 * pi / 4) # representation number of each tree
#' h_q(h_m, d_cm, n_rep_ha)
#' # Interestingly, the h_q obtained here is the same as the unweighted
#' # arithmetic mean height above. The reason: the n_rep_ha used here act as
#' # inverse basal area weights, which is exactly compensated by the basal
#' # area weights coming from d_cm.
#'
#'
#' # Typical application to a set of single tree data grouped by survey
#' # time and species
#' library(dplyr)
#' oldopt <- options(fe_spec_lang = "eng") # display colloquial species names
#' # extract the tree data to allow insights into the mechanics
#' trees <- mm_forest_1_fe_stand_spatial$trees |> filter(!removal)
#' trees |>
#'   group_by(species_id, time_yr) |>
#'   summarise(
#'     n_ha   = round(sum(n_rep_ha)),
#'     d_q_cm = d_q(dbh_cm, n_rep_ha),
#'     h_q_m  = h_q(height_m, dbh_cm, n_rep_ha)
#'   ) |>
#'   print(n = Inf)
#' options(oldopt) # set species name display to previous value
#'
h_q <- function(h, d, n_rep = 1, na_h.rm = FALSE) {
  if (length(h) != length(d)) {
    stop(
      "Lengths of h and d are different."
    )
  }

  if ((length(n_rep) > 1) & (length(n_rep) != length(d))) {
    stop(
      "Length of n_rep does not match length of h and d."
    )
  }

  if (length(n_rep) == 1) n_rep <- rep(n_rep, length(d))

  # Filtering on n_rep != 0 not necessary, because
  # If there is no other than 0 n_rep, there will be always an error in the
  # final calculation. In all other cases (at least one n_rep other than 0)
  # the final calculation will return a meaningful value (as long as the inputs
  # d and h are ok)

  # i <- n_rep != 0 # could be the case for special trees, e.g. outside border
  #                 # trees in fe_stand_spatial objects
  # h <- h[i]
  # d <- d[i]
  # n_rep <- n_rep[i]

  if (na_h.rm) {
    no_NA <- which(!is.na(h))

    h <- h[no_NA]
    d <- d[no_NA]
    n_rep <- n_rep[no_NA]
  }

  sum(h * (d / 10)^2 * n_rep) / sum((d / 10)^2 * n_rep) # d/10 for numerical
                                                        # reasons
}



#' Dominant Height h100
#'
#' The dominant height h100 was conceptuated by Ernst Assmann and Friedrich
#' Franz in order obtain a mean height value for those trees which usually
#' dominate a stand throughout its whole life.
#'
#' The h100 is defined as the quadratic mean height of the hundred thickest
#' trees per ha. If there are only 100 trees or less on one ha, the h100 is the
#' same as the quadratic mean height \code{\link{d_q}}. While the h100 is well
#' defined and useful in monospecific stands, it is less so in mixed stands.
#'
#' @param h vector of tree height values to calculate the dominant height
#'   h100 from
#'
#' @param d vector of stem diameters at breast height corresponding to h, must
#'   therefore have the same length as h
#'
#' @param n_rep_ha vector of representation numbers per ha for each diameter
#'   in d. Must have the same length as d or the length 1 (in which case it is
#'   recycled to the length of d). Otherwise, the function terminates with an
#'   error.
#'
#' @return the dominant height value h100 resulting from the input data
#'
#' @family stand heights
#'
#' @export
#'
#' @examples
#' # A sample of trees from an angle count sample , where each
#' # tree represents a basal area of 4 m²/ha
#' d_cm <- c(12, 13, 25, 27, 28, 26, 26.1, 32, 35, 31, 42)
#' n_rep_ha <- 4 / ((d_cm / 100)^2 * pi / 4) # representation number of each tree
#' dq_cm <- d_q(d_cm, n_rep_ha)
#' h_m <- 0.9 * dq_cm * (d_cm / dq_cm)^0.8 # quick plausible height estim.
#' h_100(h_m, d_cm, n_rep_ha)
#' h_q(h_m, d_cm) # quadratic mean height for comparison
#'
#'
#' # Typical application to a set of single tree data grouped by survey
#' # time and species
#' # (note that everyone is applying h_100 mixed stands, but you should do it
#' # only, if you know exactly what you are doing)
#' library(dplyr)
#' oldopt <- options(fe_spec_lang = "eng") # display colloquial species names
#' # for d_100 in mixed stands, we require species shares
#' spec_shares <- species_shares(
#'   mm_forest_1_fe_stand_spatial,
#'   tree_filter = !removal, # remaining trees only
#'   method = "ba_wd"
#' )
#' # extract the tree data to allow insights into the mechanics
#' trees <- mm_forest_1_fe_stand_spatial$trees |> filter(!removal)
#' # join with the shares
#' trees |>
#'   left_join(spec_shares) |>
#'   group_by(species_id, time_yr) |>
#'   summarise(
#'     n_ha     = round(sum(n_rep_ha)),
#'     d_q      = d_q(dbh_cm, n_rep_ha),
#'     d_100_cm = d_100(dbh_cm, n_rep_ha / species_share),
#'     h_q      = h_q(height_m, dbh_cm, n_rep_ha),
#'     h_100_m  = h_100(height_m, dbh_cm, n_rep_ha / species_share)
#'   ) |>
#'   print(n = Inf)
#' options(oldopt) # set species name display to previous value
#'
h_100 <- function(h, d, n_rep_ha) {
  if (length(h) != length(d)) {
    stop(
      "Lengths of h and d are different."
    )
  }

  if ((length(n_rep_ha) > 1) & (length(n_rep_ha) != length(d))) {
    stop(
      "Length of n_rep_ha does not match length of h and d."
    )
  }

  if (length(n_rep_ha) == 1) n_rep_ha <- rep(n_rep_ha, length(d))

  i <- n_rep_ha != 0 # could be the case for special trees, e.g. outside border
  # trees in fe_stand_spatial objects
  h <- h[i]
  d <- d[i]
  n_rep_ha <- n_rep_ha[i]

  # sorting by tree size is essential
  i <- order(d, decreasing = TRUE)
  h <- h[i]
  d <- d[i]
  n_rep_ha <- n_rep_ha[i]
  cum_rep_ha <- cumsum(n_rep_ha)

  # If the cumulative sum somewhere gets beyond 100, the n_rep_ha must be
  # corrected
  if (max(cum_rep_ha > 100)) {
    min_over <- min(cum_rep_ha[cum_rep_ha >= 100] - 100) # min_over can be 0
    # but not smaller
    cum_minus_min <- cum_rep_ha - min_over
    cum_minus_min <- cum_minus_min[cum_minus_min <= 100]
    n <- length(cum_minus_min)
    i <- 1:n
    h <- h[i]
    d <- d[i]
    n_rep_ha <- n_rep_ha[i]
    n_rep_ha[n] <- n_rep_ha[n] - min_over
  }

  h_q(h, d, n_rep_ha)
}




#' Weise's Dominant Height
#'
#' The dominant height after Weise is the quadratic mean height of the 20%
#' biggest trees in a stand. In contrast to the dominant height
#' \code{\link{h_100}} it is well defined not only in monospecific stands, but
#' also in mixed stands.
#'
#' @param h vector of tree height values to calculate Weise's dominant height
#'   from
#'
#' @param d vector of stem diameters at breast height corresponding to h, must
#'   therefore have the same length as h
#'
#' @param n_rep vector of representation numbers (typically the number of trees
#'   per ha corresponding to the diameter at the same position), will be used as
#'   individual weights for each height, together with the squared diameters d.
#'   If n_rep has length 1, it will be recycled to the length of d. Otherwise,
#'   if the length of n_rep does not correspond to the length of d, the function
#'   will terminate with an error.
#'
#' @return The value of Weise's dominant diameter resulting from the input data
#'
#' @family stand heights
#'
#' @export
#'
#' @examples
#' # A sample of trees from an angle count sample , where each
#' # tree represents a basal area of 4 m²/ha
#' d_cm <- c(12, 13, 25, 27, 28, 26, 26.1, 32, 35, 31, 42)
#' n_rep_ha <- 4 / ((d_cm / 100)^2 * pi / 4) # representation number of each tree
#' dq_cm <- d_q(d_cm, n_rep_ha)
#' h_m <- 0.9 * dq_cm * (d_cm / dq_cm)^0.8 # quick plausible height estim.
#' h_dom_weise(h_m, d_cm, n_rep_ha)
#' h_100(h_m, d_cm, n_rep_ha) # dominant height h100 for comparison
#' h_q(h_m, d_cm) # quadratic mean height for comparison
#'
#' # if 20% of the trees are 100 stems/ha, Weise's dominant diameter and
#' # d100 are equal
#' d_cm <- rnorm(n = 500, mean = 35, sd = 7)
#' dq_cm <- d_q(d_cm)
#' h_m <- 0.8 * dq_cm * (d_cm / dq_cm)^0.8 # quick plausible height estim.
#' h_dom_weise(h_m, d_cm, 1)
#' h_100(h_m, d_cm, 1)
#'
#' # Weise's dominant diameter is greater than d100, if 20% of the trees
#' # represent less than 100 trees/ha
#' d_cm <- rnorm(n = 200, mean = 35, sd = 7)
#' dq_cm <- d_q(d_cm)
#' h_m <- 0.8 * dq_cm * (d_cm / dq_cm)^0.8 # quick plausible height estim.
#' h_dom_weise(h_m, d_cm, 1)
#' h_100(h_m, d_cm, 1)
#'
#' # Weise's dominant diameter is smaller than d100, if 20% of the trees
#' # represent more than 100 trees/ha
#' d_cm <- rnorm(n = 800, mean = 35, sd = 7)
#' dq_cm <- d_q(d_cm)
#' h_m <- 0.8 * dq_cm * (d_cm / dq_cm)^0.8 # quick plausible height estim.
#' h_dom_weise(h_m, d_cm, 1)
#' h_100(h_m, d_cm, 1)
#'
h_dom_weise <- function(h, d, n_rep = 1) {
  if (length(h) != length(d)) {
    stop(
      "Lengths of h and d are different."
    )
  }

  if ((length(n_rep) > 1) & (length(n_rep) != length(d))) {
    stop(
      "Length of n_rep does not match length of h and d."
    )
  }

  if (length(n_rep) == 1) n_rep <- rep(n_rep, length(d))

  i <- n_rep != 0 # could be the case for special trees, e.g. outside border
  # trees in fe_stand_spatial objects
  d <- d[i]
  h <- h[i]
  n_rep <- n_rep[i]

  # Only the biggest trees over n_cutoff are taken into account
  n_cutoff <- sum(n_rep) * 0.2

  # sorting by tree size is essential
  i <- order(d, decreasing = TRUE)
  d <- d[i]
  h <- h[i]
  n_rep <- n_rep[i]
  cum_rep <- cumsum(n_rep)

  # Correct the n_rep vector to represent the 20% biggest trees
  # min_over can be 0 but not smaller
  min_over <- min(cum_rep[cum_rep >= n_cutoff] - n_cutoff)
  cum_minus_min <- cum_rep - min_over
  cum_minus_min <- cum_minus_min[cum_minus_min <= n_cutoff]
  n <- length(cum_minus_min)
  i <- 1:n
  d <- d[i]
  h <- h[i]
  n_rep <- n_rep[i]
  n_rep[n] <- n_rep[n] - min_over

  h_q(h, d, n_rep)
}
