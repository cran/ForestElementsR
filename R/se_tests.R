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




#' Convenient Information About the Precision of a Random Sample
#'
#' Given an arbitrary random sample, t-statistics are calculated in order to
#' obtain information about the precision of the sample mean or sum. A list of
#' useful statistics is returned, most importantly the standard error and the
#' confidence boundaries according to the confidence level provided by the user.
#'
#' @param x vector representing the sample to be evaluated
#'
#' @param ref the reference value for the satistic, whether it is calculated
#'   from the mean or the sum, default = 'mean'
#'
#' @param mu the hypothesized mean for the null hypothesis, standard = 0
#'
#' @param alternative a character string specifying the alternative hypothesis
#'  for the t-statistics: "two-sided", "greater" or "less". Default is
#'  "two-sided"
#'
#' @param conf.level confidence level of the interval, Default = 0.95
#'
#' @return A list containing various statistics such as standard error,
#'  t-statistic, degrees of freedom, p-value, and confidence interval and
#'  margin of error as a percentage of the mean or sum, respectively.
#'
#' @export
#'
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(100, mean = 5, sd = 2)
#'
#' # Test for sum
#' se_tests(
#'   data, mu = 0, ref = "sum", alternative = "two.sided", conf.level = 0.95
#' )
#' # Test for mean
#' se_tests(
#'   data, mu = 0, ref = "mean", alternative = "two.sided", conf.level = 0.95
#' )
#'
se_tests <- function(x, mu = 0,
                     ref = c("mean", "sum"),
                     alternative = c("two.sided", "greater", "less"),
                     conf.level = 0.95) {

  ref <- match.arg(ref)
  alternative <- match.arg(alternative)

  # Degrees of freedom
  df <- length(x) - 1

  if (ref == "mean") {
    # Calculate standard error of the mean
    se <- stats::sd(x) / sqrt(length(x))
    # Calculate t-statistic
    t_stat <- (mean(x) - mu) / se

    # t-test
    t_test <- stats::t.test(x,
      mu = mu,
      alternative = alternative,
      conf.level = conf.level
    )
    # confidence interval
    ci <- t_test$conf.int
    # t-statistic
    t_stat <- t_test$statistic[["t"]]
    # p-value
    p_value <- t_test$p.value
    # Calculate the margin of error as a percentage of the mean
    margin_of_error_percentage <- ((ci[2] - mean(x)) / mean(x)) * 100
    # Output a list with results
    result <- list(
      se = se,
      t_stat = t_stat,
      df = df,
      p_value = p_value,
      conf_interval = ci,
      margin_of_error_percentage = margin_of_error_percentage
    )
  } else if (ref == "sum") {
    # Calculate standard error of the sum
    se <- stats::sd(x) * sqrt(length(x))
    # Calculate t-statistic
    t_stat <- (sum(x) - mu) / se
    # t-test
    t_test <- stats::t.test(x,
      mu = mu,
      alternative = alternative,
      conf.level = conf.level,
      var.equal = TRUE # Assume equal variance for sum
    )
    # confidence interval
    ci <- t_test$conf.int * length(x) # correct ci for sum
    # t-statistic
    t_stat <- t_test$statistic[["t"]]
    # p-value
    p_value <- t_test$p.value
    # Calculate the margin of error as a percentage of the sum
    margin_of_error_percentage <- ((ci[2] - sum(x)) / sum(x)) * 100
    # Output a list with results
    result <- list(
      se = se,
      t_stat = t_stat,
      df = df,
      p_value = p_value,
      conf_interval = ci,
      margin_of_error_percentage = margin_of_error_percentage
    )
  }

  return(result)
}
