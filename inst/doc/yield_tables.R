## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Before we start, let us attach the package
library(ForestElementsR)

## -----------------------------------------------------------------------------
class(fe_ytable_pine_wiedemann_moderate_1943)

## -----------------------------------------------------------------------------
fe_ytable_pine_wiedemann_moderate_1943 |> summary()

## -----------------------------------------------------------------------------
fe_ytable_pine_wiedemann_moderate_1943[1:6] # Elements 1-6 contain metadata

## -----------------------------------------------------------------------------
fe_ytable_spruce_assmann_franz_mean_yield_level_1963$site_indexes

## -----------------------------------------------------------------------------
fe_ytable_spruce_assmann_franz_mean_yield_level_1963$site_index_variable

## -----------------------------------------------------------------------------
fe_ytable_pine_wiedemann_moderate_1943$values |> names()

## -----------------------------------------------------------------------------
fe_ytable_pine_wiedemann_moderate_1943$values$h_q_m

## -----------------------------------------------------------------------------
fe_ytable_pine_wiedemann_moderate_1943$values$pai_m3_ha_yr

## ----fig.width=4.9, fig.height=4.9, fig.align="center"------------------------
# Plot the quadratic mean height (site index fan)
fe_ytable_pine_wiedemann_moderate_1943 |> plot()

## ----fig.width=4.9, fig.height=4.9, fig.align="center"------------------------
# Plot the periodic annual increment
fe_ytable_pine_wiedemann_moderate_1943 |> plot(variable = "pai_m3_ha_yr")

## ----fig.width=4.9, fig.height=4.9, fig.align="center"------------------------
# Plot the tree number per ha and modify the plot's appearance
# 1 catch the plot
v_plot <- fe_ytable_pine_wiedemann_moderate_1943 |> plot(variable = "n_ha")

# 2 make adjustments as allowed by ggplot2, e.g.
v_plot + 
  ggplot2::theme_classic() +     # Appearance similar to R base graphics
  ggplot2::scale_y_log10() +     # Logarithmic scale for vertical axis
  ggplot2::ylab("Tree Number per hectare") # More explaining axis label

## -----------------------------------------------------------------------------
ytab <- fe_ytable_pine_wiedemann_moderate_1943 # store the yield table in a
                                               # variable with a shorter name
                                               # for convenience

# Assume a stand age of 73 years and a quadratic mean height of 19.7 m
si <- site_index(age = 72, size = 19.7, ytable = ytab, si_variable = "h_q_m")
si # The stand's relative site index according to the table
si |> round(digits = 1) # usually relative site indexes are rounded to one digit

# Same height, but twenty years younger
site_index(age = 52, size = 19.7, ytable = ytab, si_variable = "h_q_m") |>
  round(digits = 1)

# Same height, but thirty years older
site_index(age = 102, size = 19.7, ytable = ytab, si_variable = "h_q_m") |>
  round(digits = 1)

## -----------------------------------------------------------------------------
site_index(age = 171, size = 30.4, ytable = ytab, si_variable = "h_q_m")
site_index(age = 12, size = 9.2, ytable = ytab, si_variable = "h_q_m")

## -----------------------------------------------------------------------------
age <- c(35,   25,   120, 75,   42,    53)
h_q <- c(14.3, 9.1, 22.2, 13.6, 11.0,  21.7)

si_vec <- purrr::map2_dbl(
  .x = age, 
  .y = h_q, 
  .f = function(x, y, yt, si_v) site_index(x, y, yt, si_v), 
  yt = ytab, 
  si_v = "h_q_m"
) |>
  round(digits = 1)

si_vec

## -----------------------------------------------------------------------------
# Obtain the expected mean annual increment at age 100 for a stand with the
# relative site index 1.2
si_to_mai_age(
  si = 1.2, mai_variable = "mai_m3_ha_yr", age = 100, ytable = ytab
)

# Do the same for the site index vector we generated above
si_vec

purrr::map_dbl(
  .x = si_vec,
  .f = function(x, ma_v, age, yt) si_to_mai_age(x, ma_v, age, yt),
  ma_v = "mai_m3_ha_yr",
  age  = 100,
  yt   = ytab
)

## -----------------------------------------------------------------------------
# Obtain the expected maximum mean annual increment for a stand with the
# relative site index 1.2
si_to_mai_max(
  si = 1.2, mai_variable = "mai_m3_ha_yr", ytable = ytab
)

# Do the same for the site index vector we generated above
si_vec

purrr::map_dbl(
  .x = si_vec,
  .f = function(x, ma_v, yt) si_to_mai_max(x, ma_v, yt),
  ma_v = "mai_m3_ha_yr",
  yt   = ytab
)

## -----------------------------------------------------------------------------
# Obtain the expected stand height at age 100 for a stand with the relative
# site index 1.2 (this actually converts a relative into an absolute site index)
si_abs <- ytable_lookup(age = 100, si = 1.2, variable = "h_q_m", ytable = ytab)
si_abs
si_abs |> round(digits = 1)

# Let us do this again for the whole site index vector from above.
# Reasonably, we obtain warnings if we use site indexes beyond the range of the 
# yield table
si_abs_vec <- purrr::map_dbl(
  .x = si_vec,
  .f = function(x, age, var, yt) ytable_lookup(age, x, var, yt),
  age = 100,
  var = "h_q_m",
  yt   = ytab
)

si_abs_vec |> round(digits = 1)


## -----------------------------------------------------------------------------
# Get the periodic annual increment:
ytable_lookup(age = 100, si = 1.2, variable = "pai_m3_ha_yr", ytable = ytab)

# Try to go beyond the table's age coverage (raises warning)
ytable_lookup(age = 170, si = 1.2, variable = "pai_m3_ha_yr", ytable = ytab)

# ... standing volume
ytable_lookup(age = 73, si = 2.4, variable = "v_m3_ha", ytable = ytab)

# Use a site index above the table's coverage (raises warning)
ytable_lookup(age = 73, si = 0.4, variable = "v_m3_ha", ytable = ytab)

# ... basal area
ytable_lookup(age = 41, si = 3.4, variable = "ba_m2_ha", ytable = ytab)

# Use a site index below the table's coverage
ytable_lookup(age = 41, si = 6.2, variable = "ba_m2_ha", ytable = ytab)

